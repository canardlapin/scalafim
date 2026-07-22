package scalafim.dataset.zarr

import scala.util.control.NonFatal
import scalafim.archive.zarr.OpenedCanonicalBold
import scalafim.dataset.*
import scalafim.image.{Affine, DMat, NArrayUtil, NeuroSpace}
import scalafim.zarr.*

final class ZarrResponseBlockSource private (
    val opened: OpenedCanonicalBold,
    val shape: DatasetShape,
    val voxelDomain: VoxelDomain,
    val metadata: DatasetMetadata,
    val readLimits: ReadLimits
) extends ResponseBlockSource:
  protected[dataset] def readResolved(
      selection: ResolvedDataSelection
  ): Either[DatasetError, FmriSeries] =
    for
      points <- ZarrSelectionLowering.canonicalPoints(opened.canonical.array.shape, selection)
      raw <- opened.array.readPoints(points, readLimits)
        .left.map(error => DatasetError.StorageFailure(error.message))
      values <- calibrated(raw.block, opened.canonical.manifest.calibration)
      series <- FmriSeries.make(
        matrixFromRowMajor(selection.nTimepoints, selection.nVoxels, values),
        selection.voxelIndexValues,
        selection.timepointIndices,
        shape,
        metadata
      )
    yield series

  private def calibrated(
      block: PrimitiveBlock,
      calibration: scalafim.archive.zarr.ScalarCalibration
  ): Either[DatasetError, narr.NArray[Double]] =
    val values = NArrayUtil.ofSize[Double](block.elementCount)
    var index = 0
    while index < block.elementCount do
      val stored = block match
        case PrimitiveBlock.Bool(found) => if found(index) then 1.0 else 0.0
        case PrimitiveBlock.Int8(found) => found(index).toDouble
        case PrimitiveBlock.UInt8(found) => (found(index) & 0xff).toDouble
        case PrimitiveBlock.Int16(found) => found(index).toDouble
        case PrimitiveBlock.UInt16(found) => (found(index) & 0xffff).toDouble
        case PrimitiveBlock.Int32(found) => found(index).toDouble
        case PrimitiveBlock.UInt32(found) => java.lang.Integer.toUnsignedLong(found(index)).toDouble
        case PrimitiveBlock.Int64(found) => found(index).toDouble
        case PrimitiveBlock.UInt64(found) =>
          val value = found(index)
          if value >= 0L then value.toDouble
          else (value & Long.MaxValue).toDouble + 9223372036854775808.0
        case PrimitiveBlock.Float32(found) => found(index).toDouble
        case PrimitiveBlock.Float64(found) => found(index)
      values(index) = calibration(stored)
      index += 1
    Right(values)

object ZarrResponseBlockSource:
  def open(
      opened: OpenedCanonicalBold,
      voxelDomain: Option[VoxelDomain] = None,
      metadata: DatasetMetadata = DatasetMetadata.Empty,
      readLimits: ReadLimits = ReadLimits()
  ): Either[DatasetError, ZarrResponseBlockSource] =
    try
      val manifest = opened.canonical.manifest
      val affine = DMat.fromRows(Vector.tabulate(4): row =>
        Vector.tabulate(4)(column => manifest.geometry.voxelToWorld(row, column))
      )
      val dimensions = manifest.shape.toVector
      if dimensions.exists(_ > Int.MaxValue.toLong) then
        return Left(DatasetError.ShapeMismatch("canonical dimensions exceed the dataset Int boundary"))
      val spatialProduct = dimensions.drop(1).product
      if spatialProduct > Int.MaxValue.toLong then
        return Left(DatasetError.ShapeMismatch("canonical spatial size exceeds the dataset Int boundary"))
      val spatial = dimensions.drop(1).reverse.map(_.toInt)
      val space = NeuroSpace(
        spatial,
        spacing = Some(Affine.voxelSizes(affine)),
        origin = Some(Vector(affine(0, 3), affine(1, 3), affine(2, 3))),
        trans = Some(affine)
      )
      DatasetShape.make(space, manifest.shape.axis(0).toInt).flatMap: shape =>
        val domain = voxelDomain.getOrElse(VoxelDomain.fullUnsafe(shape))
        if domain.spatialSize != shape.spatialSize then
          Left(DatasetError.ShapeMismatch(
            s"voxel domain size ${domain.spatialSize} differs from canonical spatial size ${shape.spatialSize}"
          ))
        else Right(new ZarrResponseBlockSource(opened, shape, domain, metadata, readLimits))
    catch case NonFatal(error) =>
      Left(DatasetError.StorageFailure(s"invalid canonical Zarr geometry: ${error.getMessage}"))
