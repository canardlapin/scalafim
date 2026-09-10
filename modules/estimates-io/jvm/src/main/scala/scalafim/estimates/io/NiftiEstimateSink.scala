package scalafim.estimates.io

import java.io.RandomAccessFile
import java.nio.file.Path
import scala.util.control.NonFatal
import image4s.{Axis, AxisKind, NonSpatialAxes, SampleSpace}
import image4s.geometry.{D3, Frame}
import image4s.nifti.{NiftiCoordinateSystem, NiftiDatatype, NiftiScalarWriter, NiftiWriteOptions}
import scalafim.archive.io.StagedFile
import scalafim.estimates.*
import scalafim.image.SampleSpaces
import scalafim.image.io.Nifti

private[io] final class NiftiOutput(
    val product: ProductDescriptor,
    val observation: ObservationId,
    val valueStage: StagedFile,
    val validityStage: StagedFile,
    val values: NiftiScalarWriter[? <: Frame[D3], Path],
    val validity: NiftiScalarWriter[? <: Frame[D3], Path],
    val coverage: RandomAccessFile,
    var remaining: Long
):
  private def spans(samples: Vector[Int])(use: (Int, Int) => Unit): Unit =
    var first = 0
    while first < samples.size do
      var count = 1
      while first + count < samples.size && samples(first + count) == samples(first) + count do count += 1
      use(samples(first), count)
      first += count

  def fresh(volume: Int, samples: Vector[Int], sampleCount: Int): Boolean =
    val bytes = new Array[Byte](samples.size)
    var fresh = true
    spans(samples): (first, count) =>
      coverage.seek(volume.toLong * sampleCount + first)
      coverage.readFully(bytes, 0, count)
      var i = 0
      while i < count do
        if bytes(i) != 0 then fresh = false
        i += 1
    fresh

  def mark(volume: Int, samples: Vector[Int], domain: EstimateDomain): Unit =
    val bytes = Array.fill[Byte](samples.size)(1)
    spans(samples): (first, count) =>
      coverage.seek(volume.toLong * domain.sampleCount + first)
      coverage.write(bytes, 0, count)
    remaining -= samples.count(domain.contains)

  def close(): Either[EstimateError, Unit] =
    def safely(action: => Either[EstimateError, Unit]): Either[EstimateError, Unit] =
      try action
      catch case NonFatal(error) => Left(EstimateError.Io(Option(error.getMessage).getOrElse("output close failed")))
    // Evaluate every close even when an earlier resource reports failure.
    val a = safely(values.close().left.map(e => EstimateError.Io(e.message)))
    val b = safely(validity.close().left.map(e => EstimateError.Io(e.message)))
    val c = safely { coverage.close(); Right(()) }
    a.flatMap(_ => b).flatMap(_ => c)

private[io] final class NiftiEstimateSink(
    store: LocalEstimateStore,
    val unit: EstimateUnit,
    val maximumBlockCells: Int,
    outputs: Vector[NiftiOutput]
) extends EstimateSink:
  override val supportsCovariance = true
  private var closed = false
  private var receipt: Option[PinnedUnit] = None
  private var failure: Option[EstimateError] = None

  private def fail(error: EstimateError): Either[EstimateError, Nothing] =
    failure = Some(error)
    closed = true
    outputs.foreach(_.close())
    Left(error)

  def write(product: ProductId, selection: EstimateSelection, values: Array[Double], validity: Array[Byte]): Either[EstimateError, Unit] = synchronized:
    if closed then Left(EstimateError.Closed)
    else EstimateReadValidation.check(unit, product, selection, values.length, validity.length, ReadLimits(maximumBlockCells)).flatMap: descriptor =>
      writeCells(descriptor, selection.observations, selection.estimands.map(descriptor.targets.estimands.indexOf), selection.samples, values, validity)

  override def writeCovariance(product: ProductId, selection: CovarianceSelection, values: Array[Double], validity: Array[Byte]): Either[EstimateError, Unit] = synchronized:
    if closed then Left(EstimateError.Closed)
    else CovarianceReadValidation.check(unit, product, selection, values.length, validity.length, ReadLimits(maximumBlockCells)).flatMap: descriptor =>
      writeCells(descriptor, selection.observations, selection.pairs.map(CovarianceReadValidation.volume(descriptor, _)), selection.samples, values, validity)

  private def writeCells(descriptor: ProductDescriptor, observations: Vector[ObservationId], volumes: Vector[Int],
      samplesSelected: Vector[Int], values: Array[Double], validity: Array[Byte]): Either[EstimateError, Unit] =
    val product = descriptor.id
    val diagonalVolumes = if descriptor.kind == ProductKind.Covariance then
      descriptor.targets.estimands.map(id => CovarianceReadValidation.volume(descriptor, EstimandPair(id, id))).toSet
    else Set.empty[Int]
    val result = store.protect:
        // Validate every cell and duplicate before issuing any payload write.
        var invalid: Option[EstimateError] = None
        var offset = 0
        observations.foreach: observation =>
          val output = outputs.find(o => o.product.id == product && o.observation == observation).get
          volumes.foreach: volume =>
            if !output.fresh(volume, samplesSelected, unit.domain.sampleCount) then
              invalid = Some(EstimateError.Conflict("duplicate product cell delivery"))
            samplesSelected.foreach: sample =>
              val code = Validity.fromCode(validity(offset))
              code match
                case Left(error) => invalid = Some(error)
                case Right(status) =>
                  if unit.domain.contains(sample) == (status == Validity.OutsideSupport) then
                    invalid = Some(EstimateError.Invalid("product validity disagrees with requested support"))
                  else if status == Validity.Valid && (!descriptor.kind.accepts(values(offset)) || (diagonalVolumes.contains(volume) && values(offset) < 0.0)) then
                    invalid = Some(EstimateError.Invalid("valid product value violates its numeric domain"))
                  else if status == Validity.Valid && descriptor.precision == NumericPrecision.Float32 && values(offset).toFloat.toDouble != values(offset) then
                    invalid = Some(EstimateError.Invalid("Float64 to Float32 conversion requires an explicit derived product and conversion before delivery"))
              offset += 1
        invalid match
          case Some(error) => Left(error)
          case None =>
            offset = 0
            var writeFailure: Option[EstimateError] = None
            val samples = samplesSelected.map(_.toLong).toArray
            observations.foreach: observation =>
              val output = outputs.find(o => o.product.id == product && o.observation == observation).get
              volumes.foreach: volume =>
                if writeFailure.isEmpty then
                  val data = values.slice(offset, offset + samples.length)
                  val codes = validity.slice(offset, offset + samples.length).map(_.toDouble)
                  val written = for
                    _ <- output.values.writeSpatialBlock(Vector(volume), samples, data)
                    _ <- output.validity.writeSpatialBlock(Vector(volume), samples, codes)
                  yield ()
                  written match
                    case Left(error) => writeFailure = Some(EstimateError.Io(error.message))
                    case Right(_) => output.mark(volume, samplesSelected, unit.domain)
                offset += samples.length
            writeFailure.toLeft(())
    result match
      case Left(error: EstimateError.Io) => fail(error)
      case other => other

  def seal(): Either[EstimateError, PinnedUnit] = synchronized:
    receipt match
      case Some(ref) => Right(ref)
      case None if closed => failure.toLeft(()).flatMap(_ => Left(EstimateError.Closed))
      case None if outputs.exists(_.remaining != 0L) => Left(EstimateError.Invalid("cannot seal: requested product cells have not all been delivered"))
      case None =>
        closed = true
        val result = store.protect:
          val closing = outputs.foldLeft[Either[EstimateError, Unit]](Right(()))((previous, output) =>
            val result = output.close()
            previous.flatMap(_ => result))
          closing.flatMap: _ =>
            outputs.zipWithIndex.foldLeft[Either[EstimateError, Vector[NiftiRepresentation]]](Right(Vector.empty)):
              case (previous, (output, ordinal)) => previous.flatMap: refs =>
                val prefix = s"units/${unit.revision.value}/product-$ordinal"
                for
                  dataHeader <- Nifti.readHeader(output.valueStage.path).left.map(e => EstimateError.Integrity(e.message))
                  validityHeader <- Nifti.readHeader(output.validityStage.path).left.map(e => EstimateError.Integrity(e.message))
                  _ <- NiftiEstimateSource.validateHeader(unit, output.product, dataHeader, false)
                  _ <- NiftiEstimateSource.validateHeader(unit, output.product, validityHeader, true)
                  data <- store.objects.publishStaged(output.valueStage, s"${prefix}_values.nii").left.map(store.fromStore)
                  valid <- store.objects.publishStaged(output.validityStage, s"${prefix}_validity.nii").left.map(store.fromStore)
                yield refs :+ NiftiRepresentation(output.product.id, output.observation, store.reference(data), store.reference(valid),
                  output.product.precision, 1.0, 0.0, output.product.targets.estimands, "scanner-sform", output.product.targets.pairs.map((a, b) => EstimandPair(a, b)))
            .flatMap(refs => store.publishUnit(unit, refs))
        result match
          case Left(error) => failure = Some(error); Left(error)
          case Right(ref) => receipt = Some(ref); Right(ref)

  def abort(): Either[EstimateError, Unit] = synchronized:
    if closed then Right(())
    else
      closed = true
      store.protect:
        outputs.foldLeft[Either[EstimateError, Unit]](Right(()))((previous, output) =>
          val result = output.close()
          previous.flatMap(_ => result))

private[io] object NiftiEstimateSink:
  def open(store: LocalEstimateStore, unit: EstimateUnit, maximumBlockCells: Int): Either[EstimateError, EstimateSink] =
    if maximumBlockCells <= 0 then Left(EstimateError.Invalid("maximum block cells must be positive"))
    else if unit.products.map(_.observations.size).sum > 32 then
      Left(EstimateError.Unsupported("local writer permits at most 32 simultaneously open product/observation files"))
    else if unit.domain.worldFrame != "scanner" then
      Left(EstimateError.Unsupported("local writer currently requires an explicit scanner frame; other transform-code bindings are not yet qualified"))
    else
      var opened = Vector.empty[NiftiOutput]
      val result = store.protect:
        SampleSpaces.requireVolumeD3(unit.domain.space).left.map(e => EstimateError.Invalid(e.message)).flatMap: volume =>
          unit.products.foldLeft[Either[EstimateError, Unit]](Right(())): (previous, descriptor) =>
            previous.flatMap: _ =>
              descriptor.observations.foldLeft[Either[EstimateError, Unit]](Right(())): (prior, observation) =>
                prior.flatMap: _ =>
                  val dataType = if descriptor.precision == NumericPrecision.Float32 then NiftiDatatype.Float32 else NiftiDatatype.Float64
                  for
                    axis <- Axis.ordinal(if descriptor.kind == ProductKind.Covariance then "estimand-pair" else "estimand", AxisKind.Batch, descriptor.targets.width.toInt).left.map(e => EstimateError.Invalid(e.message))
                    axes <- NonSpatialAxes.from(Vector(axis)).left.map(e => EstimateError.Invalid(e.message))
                    options <- NiftiWriteOptions.create(datatype = dataType, slope = 1.0, intercept = 0.0, coordinateSystem = NiftiCoordinateSystem.ScannerAnatomical).left.map(e => EstimateError.Invalid(e.message))
                    validityOptions <- NiftiWriteOptions.create(datatype = NiftiDatatype.UInt8, slope = 1.0, intercept = 0.0, coordinateSystem = NiftiCoordinateSystem.ScannerAnatomical).left.map(e => EstimateError.Invalid(e.message))
                    dataStage <- store.objects.stage(".nii").left.map(store.fromStore)
                    validStage <- store.objects.stage(".nii").left.map(store.fromStore)
                    coverageStage <- store.objects.stage(".coverage").left.map(store.fromStore)
                    data <- Nifti.openScalarWriter(dataStage.path, SampleSpace.create(volume.grid, axes), options).left.map(e => EstimateError.Io(e.message))
                    valid <- Nifti.openScalarWriter(validStage.path, SampleSpace.create(volume.grid, axes), validityOptions).left.map: e =>
                      data.close()
                      EstimateError.Io(e.message)
                    _ <- store.protect:
                      val ledger = new RandomAccessFile(coverageStage.path.toFile, "rw")
                      val output = new NiftiOutput(descriptor, observation, dataStage, validStage, data, valid, ledger,
                        unit.domain.support.size.toLong * descriptor.targets.width)
                      opened :+= output
                      ledger.setLength(unit.domain.sampleCount.toLong * descriptor.targets.width)
                      var failure: Option[EstimateError] = None
                      var map = 0
                      while map < descriptor.targets.width && failure.isEmpty do
                        var first = 0
                        while first < unit.domain.sampleCount && failure.isEmpty do
                          val count = math.min(maximumBlockCells, unit.domain.sampleCount - first)
                          val codes = Array.tabulate(count)(i => if unit.domain.contains(first + i) then Validity.NotComputed.code.toDouble else Validity.OutsideSupport.code.toDouble)
                          valid.writeSpatialSpan(Vector(map), first.toLong, codes) match
                            case Left(error) => failure = Some(EstimateError.Io(error.message))
                            case Right(_) => ()
                          first += count
                        map += 1
                      failure.toLeft(())
                  yield ()
          .map(_ => new NiftiEstimateSink(store, unit, maximumBlockCells, opened))
      result match
        case Left(error) => opened.foreach(_.close()); Left(error)
        case other => other
