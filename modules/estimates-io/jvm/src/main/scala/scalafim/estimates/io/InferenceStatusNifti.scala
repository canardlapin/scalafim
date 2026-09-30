package scalafim.estimates.io

import java.io.RandomAccessFile
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.util.control.NonFatal
import image4s.{Axis, AxisKind, NonSpatialAxes, SampleSpace}
import image4s.geometry.{D3, Frame}
import image4s.nifti.{NiftiCoordinateSystem, NiftiDatatype, NiftiScalarWriter, NiftiWriteOptions}
import scalafim.archive.io.StagedFile
import scalafim.estimates.*
import scalafim.image.SampleSpaces
import scalafim.image.io.{Nifti, NiftiHeader}

private[io] final class InferenceStatusOutput(
    val stage: StagedFile,
    writer: NiftiScalarWriter[? <: Frame[D3], Path],
    ledger: RandomAccessFile,
    var remaining: Long
):
  private var closed = false
  def fresh(plane: Int, samples: Vector[Int], sampleCount: Int): Boolean =
    samples.forall: sample =>
      ledger.seek(plane.toLong * sampleCount + sample)
      ledger.readByte() == 0

  def write(unit: EstimateUnit, selection: InferenceStatusSelection, codes: Array[Byte]): Either[EstimateError, InferenceStatusReceipt] =
    val planes = unit.inferenceEvidence.get.planes
    var failure: Option[EstimateError] = None
    var offset = 0
    selection.planes.foreach: scope =>
      val plane = planes.indexOf(scope)
      if !fresh(plane, selection.samples, unit.domain.sampleCount) then
        failure = Some(EstimateError.Conflict("duplicate inference status delivery"))
      selection.samples.foreach: sample =>
        InferenceStatusCode.fromCode(codes(offset)) match
          case Left(error) => failure = Some(error)
          case Right(status) =>
            if unit.domain.contains(sample) == (status == InferenceStatusCode.OutsideSupport) then
              failure = Some(EstimateError.Invalid("inference status disagrees with support"))
        offset += 1
    failure match
      case Some(error) => Left(error)
      case None =>
        val samples = selection.samples.map(_.toLong).toArray
        offset = 0
        selection.planes.foreach: scope =>
          if failure.isEmpty then
            val plane = planes.indexOf(scope)
            val data = Array.tabulate(samples.length)(i => (codes(offset + i) & 0xff).toDouble)
            writer.writeSpatialBlock(Vector(plane), samples, data) match
              case Left(error) => failure = Some(EstimateError.Io(error.message))
              case Right(_) =>
                selection.samples.foreach: sample =>
                  ledger.seek(plane.toLong * unit.domain.sampleCount + sample)
                  ledger.writeByte(1)
                remaining -= selection.samples.count(unit.domain.contains)
          offset += samples.length
        failure.toLeft(InferenceStatusReceipt(selection, selection.cells.toInt))

  def close(): Either[EstimateError, Unit] =
    if closed then Right(())
    else
      closed = true
      InferenceStatusNifti.closeAll(Vector(
        () => writer.close().left.map(e => EstimateError.Io(e.message)),
        () => { ledger.close(); Right(()) }))

private[io] final class InferenceStatusInput(val channel: FileChannel, val header: NiftiHeader):
  def code(index: Long, scratch: ByteBuffer): Byte =
    scratch.clear()
    val read = channel.read(scratch, header.voxOffset.toLong + index)
    if read != 1 then throw new java.io.EOFException("truncated inference status payload")
    scratch.get(0)

  def read(unit: EstimateUnit, selection: InferenceStatusSelection, codes: Array[Byte],
      cancelled: () => Boolean): Either[EstimateError, InferenceStatusReceipt] =
    // The source validates the selection and maximum-cell cap before entering.
    val pending = new Array[Byte](selection.cells.toInt)
    val scratch = ByteBuffer.allocate(1)
    var failure: Option[EstimateError] = None
    var offset = 0
    selection.planes.foreach: scope =>
      val plane = unit.inferenceEvidence.get.planes.indexOf(scope)
      selection.samples.foreach: sample =>
        if failure.isEmpty then
          if cancelled() then failure = Some(EstimateError.Cancelled)
          else
            val raw = code(plane.toLong * unit.domain.sampleCount + sample, scratch)
            InferenceStatusNifti.checkCode(unit, sample, raw) match
              case Left(error) => failure = Some(error)
              case Right(_) => pending(offset) = raw
        offset += 1
    failure match
      case Some(error) => Left(error)
      case None =>
        System.arraycopy(pending, 0, codes, 0, pending.length)
        Right(InferenceStatusReceipt(selection, pending.length))

private[io] object InferenceStatusNifti:
  /** Evaluate every release even when a release throws or returns a failure. */
  private[io] def closeAll(actions: Vector[() => Either[EstimateError, Unit]]): Either[EstimateError, Unit] =
    var failure: Option[EstimateError] = None
    actions.foreach: action =>
      val result = try action()
        catch case NonFatal(error) => Left(EstimateError.Io(Option(error.getMessage).getOrElse("status close failed")))
      result.left.foreach(error => if failure.isEmpty then failure = Some(error))
    failure.toLeft(())

  private[io] def checkCode(unit: EstimateUnit, sample: Int, code: Byte): Either[EstimateError, Unit] =
    InferenceStatusCode.fromCode(code).left.map(e => EstimateError.Integrity(e.message)).flatMap: status =>
      if unit.domain.contains(sample) == (status == InferenceStatusCode.OutsideSupport) then
        Left(EstimateError.Integrity("inference status disagrees with support"))
      else Right(())

  def validateHeader(unit: EstimateUnit, header: NiftiHeader): Either[EstimateError, Unit] =
    NiftiEstimateSource.validateGeometry(unit, header, unit.inferenceEvidence.get.planes.size, 2, true)

  /** Caller records stages immediately; partial resources stay owned here. */
  def openOutput(store: LocalEstimateStore, unit: EstimateUnit, maximumCells: Int,
      stage: String => Either[EstimateError, StagedFile]): Either[EstimateError, InferenceStatusOutput] =
    var writer: Option[NiftiScalarWriter[? <: Frame[D3], Path]] = None
    var ledger: Option[RandomAccessFile] = None
    val result = store.protect:
      for
        cells <- InferenceStatusRepresentation.preflight(unit)
        volume <- SampleSpaces.requireVolumeD3(unit.domain.space).left.map(e => EstimateError.Invalid(e.message))
        axis <- Axis.ordinal("inference-status", AxisKind.Batch, unit.inferenceEvidence.get.planes.size).left.map(e => EstimateError.Invalid(e.message))
        axes <- NonSpatialAxes.from(Vector(axis)).left.map(e => EstimateError.Invalid(e.message))
        options <- NiftiWriteOptions.create(datatype = NiftiDatatype.UInt8, slope = 1.0, intercept = 0.0,
          coordinateSystem = NiftiCoordinateSystem.ScannerAnatomical).left.map(e => EstimateError.Invalid(e.message))
        payload <- stage(".nii")
        coverage <- stage(".coverage")
        opened <- Nifti.openScalarWriter(payload.path, SampleSpace.create(volume.grid, axes), options).left.map(e => EstimateError.Io(e.message))
        _ = writer = Some(opened)
        output <- store.protect:
          val book = new RandomAccessFile(coverage.path.toFile, "rw")
          ledger = Some(book)
          book.setLength(cells)
          var failure: Option[EstimateError] = None
          var plane = 0
          while plane < unit.inferenceEvidence.get.planes.size && failure.isEmpty do
            var first = 0
            while first < unit.domain.sampleCount && failure.isEmpty do
              val count = math.min(math.min(maximumCells, 65536), unit.domain.sampleCount - first)
              val codes = Array.tabulate(count)(i => if unit.domain.contains(first + i) then 1.0 else 0.0)
              opened.writeSpatialSpan(Vector(plane), first.toLong, codes) match
                case Left(error) => failure = Some(EstimateError.Io(error.message))
                case Right(_) => ()
              first += count
            plane += 1
          failure.toLeft(new InferenceStatusOutput(payload, opened, book,
            unit.domain.support.size.toLong * unit.inferenceEvidence.get.planes.size))
      yield output
    if result.isLeft then
      closeAll(writer.toVector.map(w => () => w.close().left.map(e => EstimateError.Io(e.message))) ++
        ledger.toVector.map(l => () => { l.close(); Right(()) }))
    result

  /** Verify all codes in bounded blocks before transferring channel ownership. */
  def openInput(store: LocalEstimateStore, unit: EstimateUnit, representation: InferenceStatusRepresentation,
      path: Path, maximumCells: Int): Either[EstimateError, InferenceStatusInput] =
    representation.validate(unit).flatMap(_ => openValidated(store, unit, path, maximumCells))

  private[io] def openValidated(store: LocalEstimateStore, unit: EstimateUnit,
      path: Path, maximumCells: Int): Either[EstimateError, InferenceStatusInput] = store.protect:
    for
      cells <- InferenceStatusRepresentation.preflight(unit)
      header <- Nifti.readHeader(path).left.map(e => EstimateError.Integrity(e.message))
      _ <- validateHeader(unit, header)
      _ <- if Files.size(path) == header.voxOffset.toLong + cells then Right(())
        else Left(EstimateError.Integrity("inference status length differs from declared payload"))
      input <- store.protect:
        val channel = FileChannel.open(path, StandardOpenOption.READ)
        var retained = false
        try
          val buffer = ByteBuffer.allocate(math.min(maximumCells.toLong, math.min(cells, 65536L)).toInt)
          var position = 0L
          var failure: Option[EstimateError] = None
          while position < cells && failure.isEmpty do
            buffer.clear()
            buffer.limit(math.min(buffer.capacity().toLong, cells - position).toInt)
            var filePosition = header.voxOffset.toLong + position
            while buffer.hasRemaining do
              val count = channel.read(buffer, filePosition)
              if count <= 0 then throw new java.io.EOFException("truncated inference status payload")
              filePosition += count
            buffer.flip()
            while buffer.hasRemaining && failure.isEmpty do
              checkCode(unit, (position % unit.domain.sampleCount).toInt, buffer.get()) match
                case Left(error) => failure = Some(error)
                case Right(_) => ()
              position += 1
          val result = failure.toLeft(new InferenceStatusInput(channel, header))
          retained = result.isRight
          result
        finally if !retained then channel.close()
    yield input
