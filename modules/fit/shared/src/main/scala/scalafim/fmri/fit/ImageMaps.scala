package scalafim.fmri.fit

import scalafim.dataset.DatasetShape
import scalafim.image.{Axis, IndexLookupVol, Mask, NDArray, NeuroVec, NArrayUtil, SparseNeuroVec}
import spire.implicits.DoubleAlgebra

final case class FitImageMaps(
    names: Vector[String],
    values: SparseNeuroVec[Double]
):
  require(names.nonEmpty, "image map names must be non-empty")
  require(values.space.dims(3) == names.length, "image map names must match map volumes")

  def nMaps: Int = names.length
  def dense: NeuroVec[Double] = values.toDense
  def mapIndex(name: String): Option[Int] = names.indexOf(name) match
    case -1 => None
    case i  => Some(i)

object FitImageMaps:

  def fromRows(
      names: Vector[String],
      rowsByMap: Vector[Vector[Double]],
      shape: DatasetShape,
      voxelIndices: Vector[Int],
      label: String
  ): FitImageMaps =
    require(names.nonEmpty, "image map names must be non-empty")
    require(rowsByMap.length == names.length, "rowsByMap length must match names")
    require(voxelIndices.nonEmpty, "voxel indices must be non-empty")
    require(voxelIndices.distinct.length == voxelIndices.length, "voxel indices must be unique")
    require(voxelIndices.forall(i => i >= 0 && i < shape.spatialSize), "voxel index out of bounds for dataset shape")
    rowsByMap.foreach { row =>
      require(row.length == voxelIndices.length, "map rows must match voxel index count")
      require(row.forall(!_.isNaN), "image map values must not be NaN")
    }

    val sorted = voxelIndices.zipWithIndex.sortBy(_._1)
    val sortedIndices = sorted.map(_._1).toArray
    val nMaps = names.length
    val nVoxels = sorted.length
    val data = NArrayUtil.ofSize[Double](nMaps * nVoxels)

    var outPos = 0
    while outPos < nVoxels do
      val sourcePos = sorted(outPos)._2
      var map = 0
      while map < nMaps do
        data(map + outPos * nMaps) = rowsByMap(map)(sourcePos)
        map += 1
      outPos += 1

    val idx = NArrayUtil.fromArray(sortedIndices)
    val space = shape.space.spatialSpace.addDim(nMaps, Some(Axis.Time))
    val mask = Mask.fromIndices(shape.space.spatialSpace, idx, label = label)
    val lookup = IndexLookupVol(space, idx)
    FitImageMaps(
      names = names,
      values = SparseNeuroVec(
        data = NDArray(data, Vector(nMaps, nVoxels)),
        space = space,
        mask = mask,
        map = lookup,
        label = label
      )
    )

extension (result: DenseFmriFitResult)
  def coefficientMaps(shape: DatasetShape): FitImageMaps =
    FitImageMaps.fromRows(
      names = result.columnNames,
      rowsByMap = matrixRows(result.coefficients.value),
      shape = shape,
      voxelIndices = result.voxelIndices,
      label = "coefficients"
    )

  def standardErrorMaps(shape: DatasetShape): FitImageMaps =
    FitImageMaps.fromRows(
      names = result.columnNames,
      rowsByMap = matrixRows(result.standardErrors.value),
      shape = shape,
      voxelIndices = result.voxelIndices,
      label = "standard_errors"
    )

extension (result: TContrastResult)
  def statisticMap(shape: DatasetShape): FitImageMaps =
    FitImageMaps.fromRows(
      names = Vector(result.name),
      rowsByMap = Vector(result.statistics.toVector),
      shape = shape,
      voxelIndices = result.voxelIndices,
      label = s"t_${result.name}"
    )

  def maps(shape: DatasetShape): FitImageMaps =
    FitImageMaps.fromRows(
      names = Vector(s"${result.name}_estimate", s"${result.name}_standard_error", s"${result.name}_t"),
      rowsByMap = Vector(result.estimates.toVector, result.standardErrors.toVector, result.statistics.toVector),
      shape = shape,
      voxelIndices = result.voxelIndices,
      label = s"t_${result.name}"
    )

extension (result: FContrastResult)
  def statisticMap(shape: DatasetShape): FitImageMaps =
    FitImageMaps.fromRows(
      names = Vector(result.name),
      rowsByMap = Vector(result.statistics.toVector),
      shape = shape,
      voxelIndices = result.voxelIndices,
      label = s"f_${result.name}"
    )

  def maps(shape: DatasetShape): FitImageMaps =
    val estimateNames =
      Vector.tabulate(result.estimates.rows)(i => s"${result.name}_estimate_${i + 1}")
    FitImageMaps.fromRows(
      names = estimateNames :+ s"${result.name}_f",
      rowsByMap = matrixRows(result.estimates) :+ result.statistics.toVector,
      shape = shape,
      voxelIndices = result.voxelIndices,
      label = s"f_${result.name}"
    )

private def matrixRows(matrix: scalafim.linalg.DoubleMatrix): Vector[Vector[Double]] =
  Vector.tabulate(matrix.rows) { row =>
    Vector.tabulate(matrix.cols)(col => matrix(row, col))
  }
