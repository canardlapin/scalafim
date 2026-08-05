package scalafim.dataset

import scalafim.image.{DMat, GridCompatibility, Mask, PrimitiveBuffers}

/** A scheduler-neutral, bounded read boundary for time-by-voxel response data.
  * Implementations may open local files, archives, or remote objects, but no
  * open resource is retained by this shared contract.
  */
trait ResponseBlockSource:
  def shape: DatasetShape
  def voxelDomain: VoxelDomain
  def metadata: DatasetMetadata
  lazy val acquisitionDomain: DatasetAcquisitionDomain =
    DatasetAcquisitionDomain
      .structuralCompatibility(shape, voxelDomain)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  final def readBlock(
      selection: DataSelection = DataSelection.All
  ): Either[DatasetError, FmriSeries] =
    selection.resolveEither(acquisitionDomain).flatMap(readResolved)

  protected[dataset] def readResolved(
      selection: ResolvedDataSelection
  ): Either[DatasetError, FmriSeries]

final case class RunResponseBlockSource(
    runId: RunId,
    source: ResponseBlockSource
)

final class CompositeResponseBlockSource private (
    val runs: Vector[RunResponseBlockSource],
    val shape: DatasetShape,
    val voxelDomain: VoxelDomain,
    val metadata: DatasetMetadata
) extends ResponseBlockSource:
  require(runs.nonEmpty, "composite response source must contain runs")
  require(runs.map(_.runId).distinct.length == runs.length, "composite response run ids must be unique")
  require(runs.map(_.source.shape.timepoints).sum == shape.timepoints, "composite run timepoints must match shape")

  def runIds: Vector[RunId] =
    runs.map(_.runId)

  def blockLengths: Vector[Int] =
    runs.map(_.source.shape.timepoints)

  protected[dataset] def readResolved(
      selection: ResolvedDataSelection
  ): Either[DatasetError, FmriSeries] =
    val assignments = assignTimepoints(selection.timepointIndices)
    val nRows = selection.nTimepoints
    val nCols = selection.nVoxels
    val values = PrimitiveBuffers.ofSize[Double](nRows * nCols)
    var failure = Option.empty[DatasetError]
    var runIndex = 0

    while runIndex < runs.length && failure.isEmpty do
      val requested = assignments(runIndex)
      if requested.nonEmpty then
        val localTimepoints = requested.map(_._2)
        val localSelection = DataSelection(
          time = TimepointSelection.Indices(localTimepoints),
          voxels = VoxelSelection.Indices(selection.voxelIndexValues)
        )
        runs(runIndex).source.readBlock(localSelection) match
          case Left(error) =>
            failure = Some(error)
          case Right(series) =>
            var localRow = 0
            while localRow < requested.length do
              val outputRow = requested(localRow)._1
              var column = 0
              while column < nCols do
                values(outputRow * nCols + column) = series.data(localRow, column)
                column += 1
              localRow += 1
      runIndex += 1

    failure match
      case Some(error) => Left(error)
      case None =>
        val data = matrixFromRowMajor(nRows, nCols, values)
        FmriSeries.make(
          data = data,
          voxelIndices = selection.voxelIndexValues,
          timepoints = selection.timepointIndices,
          shape = shape,
          metadata = metadata
        )

  private def assignTimepoints(
      requested: Vector[TimepointIndex]
  ): Vector[Vector[(Int, TimepointIndex)]] =
    val out = Array.fill(runs.length)(Vector.newBuilder[(Int, TimepointIndex)])
    val offsets = blockLengths.scanLeft(0)(_ + _)
    var outputRow = 0
    while outputRow < requested.length do
      val global = requested(outputRow).value
      var runIndex = 0
      while runIndex + 1 < offsets.length && global >= offsets(runIndex + 1) do
        runIndex += 1
      val local = TimepointIndex.unsafe(global - offsets(runIndex))
      out(runIndex) += outputRow -> local
      outputRow += 1
    out.iterator.map(_.result()).toVector

object CompositeResponseBlockSource:
  def make(
      runs: Vector[RunResponseBlockSource],
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): Either[DatasetError, CompositeResponseBlockSource] =
    if runs.isEmpty then Left(DatasetError.StorageFailure("composite response source requires at least one run"))
    else
      val duplicateIds = runs.map(_.runId.value).groupMapReduce(identity)(_ => 1)(_ + _).collect {
        case (id, count) if count > 1 => id
      }.toVector.sorted
      if duplicateIds.nonEmpty then
        Left(DatasetError.StorageFailure(s"composite response source has duplicate run ids: ${duplicateIds.mkString(", ")}"))
      else
        val first = runs.head.source
        val incompatibleSpace =
          runs.tail.find: run =>
            GridCompatibility
              .exact(run.source.shape.space, first.shape.space)
              .isLeft
        val incompatibleVoxels = runs.tail.find(_.source.voxelDomain.indices != first.voxelDomain.indices)
        incompatibleSpace match
          case Some(run) =>
            Left(DatasetError.ShapeMismatch(s"run '${run.runId.value}' has incompatible spatial geometry"))
          case None =>
            incompatibleVoxels match
              case Some(run) =>
                Left(DatasetError.ShapeMismatch(s"run '${run.runId.value}' has an incompatible voxel domain"))
              case None =>
                DatasetShape
                  .make(first.shape.space, runs.map(_.source.shape.timepoints).sum)
                  .map(shape => new CompositeResponseBlockSource(runs, shape, first.voxelDomain, metadata))

  def unsafe(
      runs: Vector[RunResponseBlockSource],
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): CompositeResponseBlockSource =
    make(runs, metadata).fold(error => throw new IllegalArgumentException(error.message), identity)

final class ResponseBlockDatasetBackend private (
    val id: DatasetId,
    val source: ResponseBlockSource,
    val mask: Mask.MaskVol,
    val metadata: DatasetMetadata,
    override val shape: DatasetShape,
    override val voxelDomain: VoxelDomain
) extends DatasetBackend:

  def readEither(selection: DataSelection = DataSelection.All): Either[DatasetError, FmriSeries] =
    selection.resolveEither(acquisitionDomain).flatMap(source.readResolved)

object ResponseBlockDatasetBackend:
  def make(
      id: DatasetId,
      source: ResponseBlockSource,
      mask: Mask.MaskVol,
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): Either[DatasetError, ResponseBlockDatasetBackend] =
    GridCompatibility
      .spatial(source.shape.space, mask.space)
      .left
      .map(error => DatasetError.ShapeMismatch(error.message))
      .map: _ =>
        new ResponseBlockDatasetBackend(
          id = id,
          source = source,
          mask = mask,
          metadata = metadata,
          shape = source.shape,
          voxelDomain = source.voxelDomain
        )

  def unsafe(
      id: DatasetId,
      source: ResponseBlockSource,
      mask: Mask.MaskVol,
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): ResponseBlockDatasetBackend =
    make(id, source, mask, metadata)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

private[dataset] def matrixFromRowMajor(
    rows: Int,
    cols: Int,
    values: Array[Double]
): DMat =
  require(rows > 0 && cols > 0, "response block matrix dimensions must be positive")
  require(values.length == rows * cols, "response block data must match matrix dimensions")
  DMat.fromRowMajorOwned(rows, cols, values)
