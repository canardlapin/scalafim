package scalafim.estimates.io

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path}
import java.nio.file.StandardOpenOption.READ
import java.util.zip.GZIPInputStream
import scalafim.estimates.*
import scalafim.image.SampleSpaces.*
import scalafim.image.io.{Nifti, NiftiHeader}

private[io] final case class NiftiInput(
    representation: NiftiRepresentation,
    dataHeader: NiftiHeader,
    validityHeader: NiftiHeader,
    values: FileChannel,
    validity: FileChannel
)

private[io] final class NiftiEstimateSource(
    store: LocalEstimateStore,
    val unit: EstimateUnit,
    val limits: ReadLimits,
    inputs: Vector[NiftiInput],
    private[io] val stagedFiles: Vector[Path],
    private[io] val stagedPayloadBytes: Long
) extends EstimateSource:
  private var closed = false

  private def scalar(channel: FileChannel, header: NiftiHeader, index: Long, buffer: ByteBuffer): Double =
    val size = header.bitpix / 8
    buffer.clear()
    buffer.limit(size)
    buffer.order(header.byteOrder)
    var position = header.voxOffset.toLong + index * size
    while buffer.hasRemaining do
      val read = channel.read(buffer, position)
      if read <= 0 then throw new java.io.EOFException("truncated NIfTI payload")
      position += read
    buffer.flip()
    val value = header.datatype match
      case 2 => (buffer.get() & 0xff).toDouble
      case 16 => buffer.getFloat().toDouble
      case 64 => buffer.getDouble()
      case _ => throw new IllegalArgumentException("unsupported scalar encoding")
    if header.slope == 0.0 then value else value * header.slope + header.intercept

  def read(product: ProductId, selection: EstimateSelection, values: Array[Double], validity: Array[Byte],
      cancelled: () => Boolean = () => false): Either[EstimateError, EstimateReadReceipt] = synchronized:
    if closed then Left(EstimateError.Closed)
    else EstimateReadValidation.check(unit, product, selection, values.length, validity.length, limits).flatMap: descriptor =>
      readCells(descriptor, selection.observations, selection.estimands.map(descriptor.targets.estimands.indexOf), selection.samples, values, validity, cancelled)
        .map(_ => EstimateReadReceipt(product, selection, selection.cells.toInt))

  override def readCovariance(product: ProductId, selection: CovarianceSelection, values: Array[Double], validity: Array[Byte],
      cancelled: () => Boolean = () => false): Either[EstimateError, CovarianceReadReceipt] = synchronized:
    if closed then Left(EstimateError.Closed)
    else CovarianceReadValidation.check(unit, product, selection, values.length, validity.length, limits).flatMap: descriptor =>
      readCells(descriptor, selection.observations, selection.pairs.map(CovarianceReadValidation.volume(descriptor, _)), selection.samples, values, validity, cancelled)
        .map(_ => CovarianceReadReceipt(product, selection, selection.cells.toInt))

  private def readCells(descriptor: ProductDescriptor, observations: Vector[ObservationId], volumes: Vector[Int], samples: Vector[Int],
      values: Array[Double], validity: Array[Byte], cancelled: () => Boolean): Either[EstimateError, Unit] = store.protect:
    val product = descriptor.id
    val diagonalVolumes = if descriptor.kind == ProductKind.Covariance then
      descriptor.targets.estimands.map(id => CovarianceReadValidation.volume(descriptor, EstimandPair(id, id))).toSet
    else Set.empty[Int]
    val scratch = ByteBuffer.allocate(8)
    var failure: Option[EstimateError] = None
    var offset = 0
    observations.foreach: observation =>
      val input = inputs.find(i => i.representation.product == product && i.representation.observation == observation).get
      volumes.foreach: volume =>
        samples.foreach: sample =>
          if failure.isEmpty then
            if cancelled() then failure = Some(EstimateError.Cancelled)
            else
              val position = volume.toLong * unit.domain.sampleCount + sample
              val rawCode = scalar(input.validity, input.validityHeader, position, scratch)
              Validity.fromCode(rawCode.toByte) match
                case Left(error) => failure = Some(error)
                case Right(status) =>
                  val value = scalar(input.values, input.dataHeader, position, scratch)
                  if unit.domain.contains(sample) == (status == Validity.OutsideSupport) then
                    failure = Some(EstimateError.Integrity("stored product validity disagrees with support"))
                  else if status == Validity.Valid && (!descriptor.kind.accepts(value) || (diagonalVolumes.contains(volume) && value < 0.0)) then
                    failure = Some(EstimateError.Integrity("stored valid value violates its numeric domain"))
                  else
                    values(offset) = value
                    validity(offset) = status.code
          offset += 1
    failure.toLeft(())

  def close(): Either[EstimateError, Unit] = synchronized:
    if closed then Right(())
    else
      closed = true
      var failure: Option[EstimateError] = None
      inputs.foreach: input =>
        Vector(input.values, input.validity).foreach: channel =>
          store.protect { channel.close(); Right(()) } match
            case Left(error) => failure = Some(error)
            case Right(_) => ()
      stagedFiles.foreach: path =>
        store.protect { Files.deleteIfExists(path); Right(()) } match
          case Left(error) => failure = Some(error)
          case Right(_) => ()
      failure.toLeft(())

private[io] object NiftiEstimateSource:
  private val maximumPairs = 32

  /** Gzip is an interchange encoding, staged to an owned seekable file under
    * a cumulative disk budget. The 64 KiB buffer is the only decompression
    * payload allocation; the source deletes every staged file on close/failure.
    */
  private def seekable(path: Path, remainingBytes: Long): Either[EstimateError, (Path, Long, Option[Path])] =
    if !path.toString.endsWith(".nii.gz") then Right((path, 0L, None))
    else
      val stage = Files.createTempFile("scalafim-estimate-", ".nii")
      val result = try
        val input = new GZIPInputStream(Files.newInputStream(path))
        try
          val output = Files.newOutputStream(stage)
          try
            val buffer = new Array[Byte](65536)
            var written = 0L
            var count = input.read(buffer)
            var overBudget = false
            while count >= 0 && !overBudget do
              if count.toLong > remainingBytes - written then overBudget = true
              else
                output.write(buffer, 0, count)
                written += count.toLong
                count = input.read(buffer)
            if overBudget then Left(EstimateError.Unsupported("gzip staging exceeds declared disk budget"))
            else Right((stage, written, Some(stage)))
          finally output.close()
        finally input.close()
      catch case error: java.io.IOException => Left(EstimateError.Io(Option(error.getMessage).getOrElse("gzip staging failed")))
      if result.isLeft then Files.deleteIfExists(stage)
      result

  private def determinant3(header: image4s.geometry.Affine[image4s.geometry.D3]): Double =
    val m = header.matrix
    m(0, 0) * (m(1, 1) * m(2, 2) - m(1, 2) * m(2, 1)) -
      m(0, 1) * (m(1, 0) * m(2, 2) - m(1, 2) * m(2, 0)) +
      m(0, 2) * (m(1, 0) * m(2, 1) - m(1, 1) * m(2, 0))

  private def maxCornerDisplacement(first: image4s.geometry.Affine[image4s.geometry.D3],
      second: image4s.geometry.Affine[image4s.geometry.D3], dimensions: Vector[Int]): Double =
    val corners = for x <- Vector(0, dimensions(0) - 1); y <- Vector(0, dimensions(1) - 1); z <- Vector(0, dimensions(2) - 1)
      yield Vector(x.toDouble, y.toDouble, z.toDouble, 1.0)
    val distances = corners.map: point =>
      val squared = (0 until 3).map: row =>
        val delta = (0 until 4).map(col => (first.matrix(row, col) - second.matrix(row, col)) * point(col)).sum
        delta * delta
      math.sqrt(squared.sum)
    distances.max

  private[io] def validateHeader(unit: EstimateUnit, product: ProductDescriptor, header: NiftiHeader, validity: Boolean,
      qformAlternativeFrame: Option[String] = None, storedDatatype: Option[NiftiStoredDatatype] = None): Either[EstimateError, Unit] =
    val expected = unit.domain.dimensions ++ Vector(product.targets.width.toInt)
    val dtype = if validity then 2 else storedDatatype.getOrElse(
      if product.precision == NumericPrecision.Float32 then NiftiStoredDatatype.Float32 else NiftiStoredDatatype.Float64).code
    if header.native.spatialUnit != image4s.nifti.NiftiSpatialUnit.Millimeter || header.native.storage != image4s.nifti.NiftiStorage.SingleFile then
      Left(EstimateError.Unsupported("local representation requires single-file NIfTI in millimetres"))
    else if header.dims != expected && !(product.targets.width == 1 && header.dims == unit.domain.dimensions) then
      Left(EstimateError.Integrity("NIfTI dimensions differ from declared logical axes"))
    else if header.native.temporalUnit != image4s.nifti.NiftiTemporalUnit.Unknown ||
        header.native.temporalOrigin.value != 0.0 || header.pixdim.lift(3).getOrElse(1.0) != 1.0 then
      Left(EstimateError.Integrity("estimate fourth axis is an ordered identity axis, not acquisition time"))
    else if header.datatype != dtype then Left(EstimateError.Integrity("NIfTI dtype differs from declared precision"))
    else if validity && (header.slope != 1.0 || header.intercept != 0.0) then Left(EstimateError.Integrity("validity must use unscaled uint8 codes"))
    else if header.sformCode != 1 || header.sform.isEmpty || unit.domain.worldFrame != "scanner" then
      Left(EstimateError.Unsupported("reader requires the qualified scanner-frame sform binding"))
    else
      val wanted = unit.domain.space.affineD3.toOption.get
      val actual = header.sform.get
      val displacement = maxCornerDisplacement(wanted, actual, unit.domain.dimensions)
      if !displacement.isFinite || displacement > 0.001 then Left(EstimateError.Integrity("manifest/header corner displacement exceeds 0.001 mm"))
      else if header.qformCode == 0 then
        if qformAlternativeFrame.nonEmpty then Left(EstimateError.Integrity("alternate qform frame declared without coded qform"))
        else Right(())
      else header.qform match
        case None => Left(EstimateError.Integrity("coded qform has no usable transform"))
        case Some(qform) =>
          val qdet = determinant3(qform)
          val sdet = determinant3(actual)
          if !qdet.isFinite || !sdet.isFinite || qdet == 0.0 || sdet == 0.0 then
            Left(EstimateError.Integrity("coded qform or sform is singular"))
          else if math.signum(qdet) != math.signum(sdet) then
            Left(EstimateError.Integrity("coded qform and selected sform disagree on handedness"))
          else
            val difference = maxCornerDisplacement(qform, actual, unit.domain.dimensions)
            if !difference.isFinite then Left(EstimateError.Integrity("coded qform/sform displacement is not finite"))
            else if difference <= 0.001 then
              if qformAlternativeFrame.nonEmpty then Left(EstimateError.Integrity("alternate qform frame declared for agreeing transforms"))
              else Right(())
            else
              val codedFrame = header.qformCode match
                case 2 => Some("aligned-anatomical")
                case 3 => Some("talairach")
                case 4 => Some("mni-152")
                case _ => None
              if codedFrame.nonEmpty && codedFrame == qformAlternativeFrame then Right(())
              else Left(EstimateError.Integrity("different qform requires a matching explicit alternate frame declaration"))

  def open(store: LocalEstimateStore, unit: EstimateUnit, representations: Vector[NiftiRepresentation], limits: ReadLimits): Either[EstimateError, EstimateSource] =
    val expected = unit.products.flatMap(p => p.observations.map(o => p.id -> o)).toSet
    val actual = representations.map(r => r.product -> r.observation)
    if actual.distinct.size != actual.size || actual.toSet != expected then
      Left(EstimateError.Integrity("representation inventory does not exactly cover product/observation axes"))
    else if representations.size > maximumPairs then
      Left(EstimateError.Unsupported(s"reader permits at most $maximumPairs product/observation pairs ($maximumPairs data and $maximumPairs validity handles)"))
    else
      var opened = Vector.empty[NiftiInput]
      var staged = Vector.empty[Path]
      var stagedBytes = 0L
      val result = store.protect:
        representations.foldLeft[Either[EstimateError, Unit]](Right(())): (previous, representation) =>
          previous.flatMap: _ =>
            val descriptor = unit.products.find(_.id == representation.product).get
            for
              _ <- store.objects.verify(store.verified(representation.values)).left.map(store.fromStore)
              _ <- store.objects.verify(store.verified(representation.validity)).left.map(store.fromStore)
              dataFile <- seekable(store.root.resolve(representation.values.path), limits.maximumStagingBytes - stagedBytes)
              _ = dataFile._3.foreach(path => staged :+= path)
              _ = stagedBytes += dataFile._2
              validityFile <- seekable(store.root.resolve(representation.validity.path), limits.maximumStagingBytes - stagedBytes)
              _ = validityFile._3.foreach(path => staged :+= path)
              _ = stagedBytes += validityFile._2
              dataHeader <- Nifti.readHeader(dataFile._1).left.map(e => EstimateError.Integrity(e.message))
              validityHeader <- Nifti.readHeader(validityFile._1).left.map(e => EstimateError.Integrity(e.message))
              _ <- if representation.precision == descriptor.precision && representation.volumeOrder == descriptor.targets.estimands &&
                       representation.pairOrder == descriptor.targets.pairs.map((a, b) => EstimandPair(a, b)) &&
                       dataHeader.slope == representation.slope && dataHeader.intercept == representation.intercept then Right(())
                   else Left(EstimateError.Integrity("physical encoding differs from declared precision, scaling or volume mapping"))
              _ <- validateHeader(unit, descriptor, dataHeader, false, representation.qformAlternativeFrame, representation.storedDatatype)
              _ <- validateHeader(unit, descriptor, validityHeader, true, representation.qformAlternativeFrame)
              _ <- if Files.size(dataFile._1) == dataHeader.voxOffset.toLong + unit.domain.sampleCount.toLong * descriptor.targets.width * (dataHeader.bitpix / 8) &&
                       Files.size(validityFile._1) == validityHeader.voxOffset.toLong + unit.domain.sampleCount.toLong * descriptor.targets.width then Right(())
                   else Left(EstimateError.Integrity("NIfTI file length differs from its declared logical payload"))
              _ <- store.protect:
                val data = FileChannel.open(dataFile._1, READ)
                try
                  val validity = FileChannel.open(validityFile._1, READ)
                  opened :+= NiftiInput(representation, dataHeader, validityHeader, data, validity)
                  Right(())
                catch
                  case error: Throwable => data.close(); throw error
            yield ()
        .map(_ => new NiftiEstimateSource(store, unit, limits, opened, staged, stagedBytes))
      result match
        case Left(error) =>
          opened.foreach(i => { i.values.close(); i.validity.close() })
          staged.foreach(Files.deleteIfExists(_))
          Left(error)
        case other => other
