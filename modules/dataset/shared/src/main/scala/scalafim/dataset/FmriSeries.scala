package scalafim.dataset

import gale.linalg.DMat

final class FmriSeries private (
    val data: DMat,
    val timepointIndices: Vector[TimepointIndex],
    val voxelIndexValues: Vector[VoxelIndex],
    val shape: DatasetShape,
    val metadata: DatasetMetadata
):
  def timepoints: Vector[Int] =
    timepointIndices.map(TimepointIndex.raw)

  def voxelIndices: Vector[Int] =
    voxelIndexValues.map(VoxelIndex.raw)

  def nTimepoints: Int = data.rows
  def nVoxels: Int = data.cols

object FmriSeries:
  def make(
      data: DMat,
      voxelIndices: Vector[VoxelIndex],
      timepoints: Vector[TimepointIndex],
      shape: DatasetShape,
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): Either[DatasetError, FmriSeries] =
    if data.rows != timepoints.length then
      Left(DatasetError.ShapeMismatch(s"series has ${data.rows} rows but ${timepoints.length} selected timepoints"))
    else if data.cols != voxelIndices.length then
      Left(DatasetError.ShapeMismatch(s"series has ${data.cols} columns but ${voxelIndices.length} selected voxels"))
    else
      validateTimepointBounds(timepoints, shape).flatMap { _ =>
        validateVoxelBounds(voxelIndices, shape).map { _ =>
          new FmriSeries(
            data = data,
            timepointIndices = timepoints,
            voxelIndexValues = voxelIndices,
            shape = shape,
            metadata = metadata
          )
        }
      }

  def fromIntIndices(
      data: DMat,
      voxelIndices: Vector[Int],
      timepoints: Vector[Int],
      shape: DatasetShape,
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): Either[DatasetError, FmriSeries] =
    for
      typedTimepoints <- traverseIndex(timepoints, TimepointIndex.make)
      typedVoxels <- traverseIndex(voxelIndices, VoxelIndex.make)
      series <- make(data, typedVoxels, typedTimepoints, shape, metadata)
    yield series

  private[dataset] def unsafe(
      data: DMat,
      voxelIndices: Vector[VoxelIndex],
      timepoints: Vector[TimepointIndex],
      shape: DatasetShape,
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): FmriSeries =
    make(data, voxelIndices, timepoints, shape, metadata)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

private def traverseIndex[A](
    values: Vector[Int],
    make: Int => Either[DatasetError, A]
): Either[DatasetError, Vector[A]] =
  val out = Vector.newBuilder[A]
  out.sizeHint(values.length)
  var i = 0
  var error = Option.empty[DatasetError]
  while i < values.length && error.isEmpty do
    make(values(i)) match
      case Left(err) => error = Some(err)
      case Right(value) => out += value
    i += 1

  error match
    case Some(err) => Left(err)
    case None => Right(out.result())

private def validateTimepointBounds(
    values: Vector[TimepointIndex],
    shape: DatasetShape
): Either[DatasetError, Unit] =
  validateBounds(DatasetAxis.Timepoint, values.map(TimepointIndex.raw), shape.timepoints)

private def validateVoxelBounds(
    values: Vector[VoxelIndex],
    shape: DatasetShape
): Either[DatasetError, Unit] =
  validateBounds(DatasetAxis.Voxel, values.map(VoxelIndex.raw), shape.spatialSize)

private def validateBounds(
    axis: DatasetAxis,
    values: Vector[Int],
    size: Int
): Either[DatasetError, Unit] =
  var i = 0
  var error = Option.empty[DatasetError]
  while i < values.length && error.isEmpty do
    val value = values(i)
    if value < 0 then error = Some(DatasetError.NegativeIndex(axis, value))
    else if value >= size then error = Some(DatasetError.IndexOutOfBounds(axis, value, size))
    i += 1

  error match
    case Some(err) => Left(err)
    case None => Right(())
