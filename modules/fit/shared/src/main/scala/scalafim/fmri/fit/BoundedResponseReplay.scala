package scalafim.fmri.fit

import scalafim.dataset.{DataSelection, DatasetError, DatasetSeriesReader, FmriSeries}
import scalafim.fmri.model.{FitPlan, VolumeWeighting}

/** Repeated reductions require an immutable source revision. These compact
  * checks detect accidental legacy-reader changes; they are not an authenticity
  * or content-addressing protocol. The durable resolver owns source integrity.
  */
private[fit] final case class ResponseReplayChecks(
    timepoints: Vector[Int],
    blocks: Map[Vector[Int], Long]
):
  def verify(series: FmriSeries): Either[FitError, Unit] =
    if series.timepoints != timepoints || !blocks.get(series.voxelIndices).contains(ResponseReplayChecks.checksum(series)) then
      Left(FitError.PreparationReplayMismatch("response snapshot changed between spatial reduction passes"))
    else Right(())

private[fit] object ResponseReplayChecks:
  def checksum(series: FmriSeries): Long =
    var state = -3750763034362895579L
    var row = 0
    while row < series.data.rows do
      var col = 0
      while col < series.data.cols do
        state = (state ^ java.lang.Double.doubleToLongBits(series.data(row, col))) * 1099511628211L
        col += 1
      row += 1
    state

private[fit] final class BoundedResponseReplay private (
    val plan: FitPlan,
    val chunks: FitChunkPlan,
    val reader: DatasetSeriesReader,
    val responsePreparation: ResolvedResponsePreparation,
    val design: DesignMatrix,
    val partitions: Vector[RunPartition],
    val rawPartitions: Vector[RunPartition],
    val retainedVoxelIndices: Vector[Int],
    val checks: ResponseReplayChecks
):
  def input(series: FmriSeries): Either[FitError, FitBlockInput] =
    for
      _ <- checks.verify(series)
      input <- FitPlanExecutor.fitBlockInput(plan, series, rawPartitions, responsePreparation)
    yield input

  def foreachBlock(consume: FitBlockInput => Either[FitError, Unit]): Either[FitError, Unit] =
    val iterator = chunks.iterator
    while iterator.hasNext do
      val chunk = iterator.next()
      ChunkedFitExecutor.readChunk(reader, chunk).flatMap(input) match
        case Left(FitError.AllVoxelsExcluded(_)) => ()
        case Left(error) => return Left(FitError.ChunkFailed(chunk.ordinal.value, error))
        case Right(value) => consume(value) match
          case Left(error) => return Left(error)
          case Right(_) => ()
    Right(())

private[fit] object BoundedResponseReplay:
  def prepare(reader: DatasetSeriesReader, plan: FitPlan, chunks: FitChunkPlan): Either[FitError, BoundedResponseReplay] =
    val recorded = scala.collection.mutable.Map.empty[Vector[Int], Long]
    val recordingReader = new DatasetSeriesReader:
      def dataset = reader.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        reader.seriesEither(selection).flatMap { series =>
          val checksum = ResponseReplayChecks.checksum(series)
          recorded.get(series.voxelIndices) match
            case Some(previous) if previous != checksum =>
              Left(DatasetError.StorageFailure("response snapshot changed during preparation"))
            case _ =>
              recorded(series.voxelIndices) = checksum
              Right(series)
        }
    val resolved = plan.config.volumeWeighting match
      case VolumeWeighting.Estimated(_) =>
        FitPreparation.dvars(recordingReader, plan, chunks).map(_.responsePreparation)
      case _ => FitPreparation.responsePreparation(plan, chunks.timepoints)
    resolved.flatMap(preparation => complete(reader, recordingReader, plan, chunks, preparation, recorded))

  private def complete(
      reader: DatasetSeriesReader,
      recordingReader: DatasetSeriesReader,
      plan: FitPlan,
      chunks: FitChunkPlan,
      preparation: ResolvedResponsePreparation,
      recorded: scala.collection.mutable.Map[Vector[Int], Long]
  ): Either[FitError, BoundedResponseReplay] =
    val rawPartitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, chunks.timepoints)
    val retained = Vector.newBuilder[Int]
    val excluded = Vector.newBuilder[VoxelInferenceExclusion]
    var geometry = Option.empty[FitBlockInput]
    val iterator = chunks.iterator
    while iterator.hasNext do
      val chunk = iterator.next()
      val input = for
        series <- ChunkedFitExecutor.readChunk(recordingReader, chunk)
        value <- FitPlanExecutor.fitBlockInput(plan, series, rawPartitions, preparation)
      yield value
      input match
        case Left(FitError.AllVoxelsExcluded(values)) => excluded ++= values
        case Left(error) => return Left(FitError.ChunkFailed(chunk.ordinal.value, error))
        case Right(value) =>
          if geometry.isEmpty then geometry = Some(value)
          retained ++= value.voxelIndices
    geometry match
      case None => Left(FitError.AllVoxelsExcluded(excluded.result()))
      case Some(input) => Right(new BoundedResponseReplay(plan, chunks, reader, preparation,
        input.design, input.partitions, rawPartitions, retained.result(),
        ResponseReplayChecks(chunks.timepoints, recorded.toMap)))
