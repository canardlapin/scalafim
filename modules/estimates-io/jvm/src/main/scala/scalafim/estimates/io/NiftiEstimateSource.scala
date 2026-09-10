package scalafim.estimates.io

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption.READ
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
    inputs: Vector[NiftiInput]
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
      failure.toLeft(())

private[io] object NiftiEstimateSource:
  private[io] def validateHeader(unit: EstimateUnit, product: ProductDescriptor, header: NiftiHeader, validity: Boolean): Either[EstimateError, Unit] =
    val expected = unit.domain.dimensions ++ Vector(product.targets.width.toInt)
    val dtype = if validity then 2 else if product.precision == NumericPrecision.Float32 then 16 else 64
    if header.native.spatialUnit != image4s.nifti.NiftiSpatialUnit.Millimeter || header.native.storage != image4s.nifti.NiftiStorage.SingleFile then
      Left(EstimateError.Unsupported("local representation requires single-file NIfTI in millimetres"))
    else if header.dims != expected then Left(EstimateError.Integrity("NIfTI dimensions differ from declared logical axes"))
    else if header.datatype != dtype then Left(EstimateError.Integrity("NIfTI dtype differs from declared precision"))
    else if validity && (header.slope != 1.0 || header.intercept != 0.0) then Left(EstimateError.Integrity("validity must use unscaled uint8 codes"))
    else if header.sformCode != 1 || header.sform.isEmpty || unit.domain.worldFrame != "scanner" then
      Left(EstimateError.Unsupported("reader requires the qualified scanner-frame sform binding"))
    else
      val wanted = unit.domain.space.affineD3.toOption.get
      val actual = header.sform.get
      val dims = unit.domain.dimensions
      val corners = for x <- Vector(0, dims(0) - 1); y <- Vector(0, dims(1) - 1); z <- Vector(0, dims(2) - 1) yield Vector(x.toDouble, y.toDouble, z.toDouble, 1.0)
      val displacement = corners.map: point =>
        math.sqrt((0 until 3).map: row =>
          val delta = (0 until 4).map(col => (wanted.matrix(row, col) - actual.matrix(row, col)) * point(col)).sum
          delta * delta
        .sum)
      if displacement.exists(x => !x.isFinite || x > 0.001) then Left(EstimateError.Integrity("manifest/header corner displacement exceeds 0.001 mm"))
      else Right(())

  def open(store: LocalEstimateStore, unit: EstimateUnit, representations: Vector[NiftiRepresentation], limits: ReadLimits): Either[EstimateError, EstimateSource] =
    val expected = unit.products.flatMap(p => p.observations.map(o => p.id -> o)).toSet
    val actual = representations.map(r => r.product -> r.observation)
    if actual.distinct.size != actual.size || actual.toSet != expected then
      Left(EstimateError.Integrity("representation inventory does not exactly cover product/observation axes"))
    else
      var opened = Vector.empty[NiftiInput]
      val result = store.protect:
        representations.foldLeft[Either[EstimateError, Unit]](Right(())): (previous, representation) =>
          previous.flatMap: _ =>
            val descriptor = unit.products.find(_.id == representation.product).get
            for
              _ <- store.objects.verify(store.verified(representation.values)).left.map(store.fromStore)
              _ <- store.objects.verify(store.verified(representation.validity)).left.map(store.fromStore)
              dataHeader <- Nifti.readHeader(store.root.resolve(representation.values.path)).left.map(e => EstimateError.Integrity(e.message))
              validityHeader <- Nifti.readHeader(store.root.resolve(representation.validity.path)).left.map(e => EstimateError.Integrity(e.message))
              _ <- if representation.precision == descriptor.precision && representation.volumeOrder == descriptor.targets.estimands &&
                       representation.pairOrder == descriptor.targets.pairs.map((a, b) => EstimandPair(a, b)) &&
                       dataHeader.slope == representation.slope && dataHeader.intercept == representation.intercept then Right(())
                   else Left(EstimateError.Integrity("physical encoding differs from declared precision, scaling or volume mapping"))
              _ <- validateHeader(unit, descriptor, dataHeader, false)
              _ <- validateHeader(unit, descriptor, validityHeader, true)
              _ <- if representation.values.bytes >= dataHeader.voxOffset.toLong + unit.domain.sampleCount.toLong * descriptor.targets.width * (dataHeader.bitpix / 8) &&
                       representation.validity.bytes >= validityHeader.voxOffset.toLong + unit.domain.sampleCount.toLong * descriptor.targets.width then Right(())
                   else Left(EstimateError.Integrity("NIfTI file is shorter than its declared logical payload"))
              _ <- store.protect:
                val data = FileChannel.open(store.root.resolve(representation.values.path), READ)
                try
                  val validity = FileChannel.open(store.root.resolve(representation.validity.path), READ)
                  opened :+= NiftiInput(representation, dataHeader, validityHeader, data, validity)
                  Right(())
                catch
                  case error: Throwable => data.close(); throw error
            yield ()
        .map(_ => new NiftiEstimateSource(store, unit, limits, opened))
      result match
        case Left(error) => opened.foreach(i => { i.values.close(); i.validity.close() }); Left(error)
        case other => other
