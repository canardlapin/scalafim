package scalafim.dataset

import image4s.geometry.Grid
import gale.linalg.DMat

final class FmriSeriesSegment private (
    val key: RunKey,
    val partition: DatasetRunPartition,
    val series: FmriSeries
)

object FmriSeriesSegment:
  def make(
      key: RunKey,
      partition: DatasetRunPartition,
      series: FmriSeries
  ): Either[DatasetError, FmriSeriesSegment] =
    if partition.run != key.run then
      Left(DatasetError.InvalidTimeAxis(
        s"segment key ${key.run.value} does not match partition run ${partition.run.value}"
      ))
    else if partition.timepointIndices != series.timepointIndices then
      Left(DatasetError.InvalidTimeAxis(
        s"segment ${key.label} partition timepoints do not match its series"
      ))
    else if partition.rowIndices != series.timepointIndices.indices.toVector then
      Left(DatasetError.InvalidTimeAxis(
        s"segment ${key.label} partition rows must cover its series in order"
      ))
    else Right(new FmriSeriesSegment(key, partition, series))

final class SegmentedFmriSeries private (
    val segments: Vector[FmriSeriesSegment]
):
  def size: Int =
    segments.length

  def groupBy[K](
      key: FmriSeriesSegment => K
  ): Map[K, SegmentedFmriSeries] =
    val grouped = scala.collection.mutable.LinkedHashMap.empty[K, Vector[FmriSeriesSegment]]
    var index = 0
    while index < segments.length do
      val segment = segments(index)
      val groupKey = key(segment)
      grouped.update(groupKey, grouped.getOrElse(groupKey, Vector.empty) :+ segment)
      index += 1
    grouped.iterator
      .map: (groupKey, values) =>
        groupKey -> SegmentedFmriSeries.unsafe(values)
      .toMap

  def blockConcatenate: Either[DatasetError, BlockConcatenatedFmriSeries] =
    BlockConcatenatedFmriSeries.fromSegments(segments)

  def reduceBy[K, A](
      key: FmriSeriesSegment => K
  )(
      reduce: SegmentedFmriSeries => Either[DatasetError, A]
  ): Either[DatasetError, Map[K, A]] =
    val groups = groupBy(key).iterator
    val reduced = scala.collection.mutable.LinkedHashMap.empty[K, A]
    var failure = Option.empty[DatasetError]
    while groups.hasNext && failure.isEmpty do
      val (groupKey, group) = groups.next()
      reduce(group) match
        case Left(error)  => failure = Some(error)
        case Right(value) => reduced.update(groupKey, value)
    failure match
      case Some(error) => Left(error)
      case None        => Right(reduced.toMap)

object SegmentedFmriSeries:
  def make(
      segments: Vector[FmriSeriesSegment]
  ): Either[DatasetError, SegmentedFmriSeries] =
    if segments.isEmpty then Left(DatasetError.EmptyDatasetIndex)
    else Right(new SegmentedFmriSeries(segments))

  private[dataset] def unsafe(
      segments: Vector[FmriSeriesSegment]
  ): SegmentedFmriSeries =
    make(segments).fold(error => throw new IllegalArgumentException(error.message), identity)

final class BlockSegmentBoundary private[dataset] (
    val key: RunKey,
    val rowStart: Int,
    val length: Int,
    val partition: DatasetRunPartition
):
  def rowEndExclusive: Int =
    rowStart + length

final class BlockConcatenatedFmriSeries private (
    val series: FmriSeries,
    val segments: Vector[FmriSeriesSegment],
    val boundaries: Vector[BlockSegmentBoundary]
)

object BlockConcatenatedFmriSeries:
  private[dataset] def fromSegments(
      segments: Vector[FmriSeriesSegment]
  ): Either[DatasetError, BlockConcatenatedFmriSeries] =
    if segments.isEmpty then Left(DatasetError.EmptyDatasetIndex)
    else
      for
        _ <- validateVoxelAxes(segments)
        totalRows <- totalRowCount(segments)
        shape <- DatasetShape.make(segments.head.series.shape.space, totalRows)
        data = DMat.tabulate(totalRows, segments.head.series.data.cols): (row, column) =>
          var segmentIndex = 0
          var localRow = row
          while localRow >= segments(segmentIndex).series.nTimepoints do
            localRow -= segments(segmentIndex).series.nTimepoints
            segmentIndex += 1
          segments(segmentIndex).series.data(localRow, column)
        series <- FmriSeries.make(
          data = data,
          voxelIndices = segments.head.series.voxelIndexValues,
          timepoints = Vector.tabulate(totalRows)(TimepointIndex.unsafe),
          shape = shape,
          metadata = commonMetadata(segments)
        )
      yield new BlockConcatenatedFmriSeries(
        series = series,
        segments = segments,
        boundaries = boundariesFor(segments)
      )

  private def validateVoxelAxes(
      segments: Vector[FmriSeriesSegment]
  ): Either[DatasetError, Unit] =
    val first = segments.head.series
    var index = 1
    var failure = Option.empty[DatasetError]
    while index < segments.length && failure.isEmpty do
      val current = segments(index).series
      Grid.exactCongruence(first.shape.grid, current.shape.grid) match
        case Left(error) =>
          failure = Some(DatasetError.Geometry(error))
        case Right(_) =>
          if current.voxelIndexValues != first.voxelIndexValues then
            failure = Some(DatasetError.ShapeMismatch(
              s"block concatenation requires identical ordered voxel selections; segment ${segments(index).key.label} differs"
            ))
      index += 1
    failure match
      case Some(error) => Left(error)
      case None        => Right(())

  private def totalRowCount(
      segments: Vector[FmriSeriesSegment]
  ): Either[DatasetError, Int] =
    val total = segments.foldLeft(0L)(_ + _.series.nTimepoints.toLong)
    if total <= Int.MaxValue.toLong then Right(total.toInt)
    else Left(DatasetError.ShapeMismatch(
      s"block-concatenated row count exceeds ${Int.MaxValue}: $total"
    ))

  private def commonMetadata(
      segments: Vector[FmriSeriesSegment]
  ): DatasetMetadata =
    val first = segments.head.series.metadata
    if segments.forall(_.series.metadata == first) then first
    else DatasetMetadata.Empty

  private def boundariesFor(
      segments: Vector[FmriSeriesSegment]
  ): Vector[BlockSegmentBoundary] =
    val boundaries = Vector.newBuilder[BlockSegmentBoundary]
    boundaries.sizeHint(segments.length)
    var rowStart = 0
    var index = 0
    while index < segments.length do
      val segment = segments(index)
      boundaries += new BlockSegmentBoundary(
        key = segment.key,
        rowStart = rowStart,
        length = segment.series.nTimepoints,
        partition = segment.partition
      )
      rowStart += segment.series.nTimepoints
      index += 1
    boundaries.result()
