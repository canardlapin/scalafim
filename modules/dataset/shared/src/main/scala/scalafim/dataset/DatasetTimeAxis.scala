package scalafim.dataset

import scalafim.fmri.hrf.design.SamplingFrame

opaque type RunOrdinal = Int

object RunOrdinal:
  def make(value: Int): Either[DatasetError, RunOrdinal] =
    if value < 0 then Left(DatasetError.InvalidTimeAxis(s"run ordinal must be non-negative; got $value"))
    else Right(value)

  def unsafe(value: Int): RunOrdinal =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (ordinal: RunOrdinal)
    inline def value: Int = ordinal

opaque type RunLocalTimepointIndex = Int

object RunLocalTimepointIndex:
  def make(value: Int): Either[DatasetError, RunLocalTimepointIndex] =
    if value < 0 then Left(DatasetError.InvalidTimeAxis(s"run-local timepoint must be non-negative; got $value"))
    else Right(value)

  def unsafe(value: Int): RunLocalTimepointIndex =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (index: RunLocalTimepointIndex)
    inline def value: Int = index

final case class DatasetTimeBlock private (
    ordinal: RunOrdinal,
    run: RunId,
    start: TimepointIndex,
    length: Int
):
  require(length > 0, "time block length must be positive")

  def runIndex: Int =
    ordinal.value

  def startValue: Int =
    start.value

  def endExclusive: Int =
    startValue + length

  def contains(timepoint: TimepointIndex): Boolean =
    val value = timepoint.value
    value >= startValue && value < endExclusive

  def localIndex(timepoint: TimepointIndex): Either[DatasetError, RunLocalTimepointIndex] =
    if contains(timepoint) then RunLocalTimepointIndex.make(timepoint.value - startValue)
    else Left(DatasetError.InvalidTimeAxis(s"timepoint ${timepoint.value} is not in run ${run.value}"))

  def globalTimepoints: Vector[TimepointIndex] =
    Vector.tabulate(length)(offset => TimepointIndex.unsafe(startValue + offset))

object DatasetTimeBlock:
  def make(
      ordinal: RunOrdinal,
      run: RunId,
      start: TimepointIndex,
      length: Int
  ): Either[DatasetError, DatasetTimeBlock] =
    if length <= 0 then Left(DatasetError.InvalidTimeAxis(s"run ${run.value} length must be positive; got $length"))
    else Right(new DatasetTimeBlock(ordinal, run, start, length))

final case class DatasetRunPartition private (
    ordinal: RunOrdinal,
    run: RunId,
    rowIndices: Vector[Int],
    timepointIndices: Vector[TimepointIndex],
    localIndices: Vector[RunLocalTimepointIndex]
):
  require(rowIndices.nonEmpty, "run partition must contain at least one selected row")
  require(rowIndices.length == timepointIndices.length, "run partition rows and timepoints must align")
  require(rowIndices.length == localIndices.length, "run partition rows and local timepoints must align")

  def runIndex: Int =
    ordinal.value

  def timepoints: Vector[Int] =
    timepointIndices.map(_.value)

  def localTimepoints: Vector[Int] =
    localIndices.map(_.value)

object DatasetRunPartition:
  private[dataset] def make(
      block: DatasetTimeBlock,
      rowIndices: Vector[Int],
      timepointIndices: Vector[TimepointIndex],
      localIndices: Vector[RunLocalTimepointIndex]
  ): Either[DatasetError, DatasetRunPartition] =
    if rowIndices.isEmpty then Left(DatasetError.InvalidTimeAxis(s"run ${block.run.value} partition must contain at least one selected row"))
    else if rowIndices.length != timepointIndices.length then Left(DatasetError.InvalidTimeAxis(s"run ${block.run.value} row/timepoint partition length mismatch"))
    else if rowIndices.length != localIndices.length then Left(DatasetError.InvalidTimeAxis(s"run ${block.run.value} row/local-time partition length mismatch"))
    else Right(new DatasetRunPartition(block.ordinal, block.run, rowIndices, timepointIndices, localIndices))

final class DatasetTimeAxis private (
    val blocks: Vector[DatasetTimeBlock],
    val timepoints: Int
):
  require(blocks.nonEmpty, "dataset time axis must contain at least one run")
  require(timepoints > 0, "dataset time axis must contain at least one timepoint")

  def nRuns: Int =
    blocks.length

  def runIds: Vector[RunId] =
    blocks.map(_.run)

  def blockLengths: Vector[Int] =
    blocks.map(_.length)

  def blockFor(timepoint: TimepointIndex): Either[DatasetError, DatasetTimeBlock] =
    val value = timepoint.value
    if value < 0 then Left(DatasetError.NegativeIndex(DatasetAxis.Timepoint, value))
    else if value >= timepoints then Left(DatasetError.IndexOutOfBounds(DatasetAxis.Timepoint, value, timepoints))
    else
      var i = 0
      while i < blocks.length do
        val block = blocks(i)
        if block.contains(timepoint) then return Right(block)
        i += 1
      Left(DatasetError.InvalidTimeAxis(s"timepoint $value does not belong to any run"))

  def localIndex(timepoint: TimepointIndex): Either[DatasetError, (RunId, RunLocalTimepointIndex)] =
    for
      block <- blockFor(timepoint)
      local <- block.localIndex(timepoint)
    yield (block.run, local)

  def partitions(timepoints: Vector[TimepointIndex]): Either[DatasetError, Vector[DatasetRunPartition]] =
    if timepoints.isEmpty then Left(DatasetError.EmptySelection(DatasetAxis.Timepoint))
    else
      validateSelectedTimepoints(timepoints).flatMap { _ =>
        val out = Vector.newBuilder[DatasetRunPartition]
        var blockIndex = 0
        var error = Option.empty[DatasetError]
        while blockIndex < blocks.length && error.isEmpty do
          val block = blocks(blockIndex)
          val rows = Vector.newBuilder[Int]
          val selectedTimepoints = Vector.newBuilder[TimepointIndex]
          val locals = Vector.newBuilder[RunLocalTimepointIndex]
          var selectedRow = 0
          while selectedRow < timepoints.length && error.isEmpty do
            val timepoint = timepoints(selectedRow)
            if block.contains(timepoint) then
              block.localIndex(timepoint) match
                case Left(err) => error = Some(err)
                case Right(local) =>
                  rows += selectedRow
                  selectedTimepoints += timepoint
                  locals += local
            selectedRow += 1

          val rowVector = rows.result()
          if rowVector.nonEmpty && error.isEmpty then
            DatasetRunPartition.make(block, rowVector, selectedTimepoints.result(), locals.result()) match
              case Left(err) => error = Some(err)
              case Right(partition) => out += partition
          blockIndex += 1
        error match
          case Some(err) => Left(err)
          case None => Right(out.result())
      }

  def partitionsForInts(values: Vector[Int]): Either[DatasetError, Vector[DatasetRunPartition]] =
    val typed = Vector.newBuilder[TimepointIndex]
    typed.sizeHint(values.length)
    var i = 0
    var error = Option.empty[DatasetError]
    while i < values.length && error.isEmpty do
      TimepointIndex.make(values(i)) match
        case Left(err) => error = Some(err)
        case Right(timepoint) => typed += timepoint
      i += 1
    error match
      case Some(err) => Left(err)
      case None => partitions(typed.result())

  private def validateSelectedTimepoints(values: Vector[TimepointIndex]): Either[DatasetError, Unit] =
    val seen = scala.collection.mutable.HashSet.empty[Int]
    var i = 0
    while i < values.length do
      val value = values(i).value
      if value < 0 then return Left(DatasetError.NegativeIndex(DatasetAxis.Timepoint, value))
      if value >= timepoints then return Left(DatasetError.IndexOutOfBounds(DatasetAxis.Timepoint, value, timepoints))
      if seen.contains(value) then return Left(DatasetError.DuplicateSelection(DatasetAxis.Timepoint, value))
      seen += value
      i += 1
    Right(())

object DatasetTimeAxis:
  def fromSamplingFrame(
      samplingFrame: SamplingFrame,
      runIds: Vector[RunId]
  ): Either[DatasetError, DatasetTimeAxis] =
    if samplingFrame.nBlocks <= 0 then Left(DatasetError.InvalidTimeAxis("sampling frame must contain at least one block"))
    else if runIds.length != samplingFrame.nBlocks then
      Left(DatasetError.InvalidTimeAxis(s"expected ${samplingFrame.nBlocks} run ids but got ${runIds.length}"))
    else if runIds.distinct.length != runIds.length then
      Left(DatasetError.InvalidTimeAxis("run ids must be unique within a dataset time axis"))
    else
      val blocks = Vector.newBuilder[DatasetTimeBlock]
      blocks.sizeHint(samplingFrame.nBlocks)
      var start = 0
      var run = 0
      while run < samplingFrame.nBlocks do
        val length = samplingFrame.blockLens(run)
        val block =
          for
            ordinal <- RunOrdinal.make(run)
            startIndex <- TimepointIndex.make(start)
            timeBlock <- DatasetTimeBlock.make(ordinal, runIds(run), startIndex, length)
          yield timeBlock
        block match
          case Left(error) => return Left(error)
          case Right(value) => blocks += value
        start += length
        run += 1
      Right(new DatasetTimeAxis(blocks.result(), start))

  def fromSamplingFrame(samplingFrame: SamplingFrame): Either[DatasetError, DatasetTimeAxis] =
    fromSamplingFrame(samplingFrame, defaultRunIds(samplingFrame.nBlocks))

  def unsafe(samplingFrame: SamplingFrame): DatasetTimeAxis =
    fromSamplingFrame(samplingFrame).fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafe(
      samplingFrame: SamplingFrame,
      runIds: Vector[RunId]
  ): DatasetTimeAxis =
    fromSamplingFrame(samplingFrame, runIds).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def defaultRunIds(nRuns: Int): Vector[RunId] =
    Vector.tabulate(nRuns)(index => RunId(s"run-${index + 1}"))
