package scalafim.image

import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.GeometryError

/** Neuroimaging calculations over image4s-owned spatial geometry.
  *
  * These extensions derive values from the provider affine. They do not retain
  * another matrix representation or reimplement inversion or composition.
  */
object NeuroAffineSyntax:
  extension (affine: Affine[D3])
    def neuroVoxelSizes: Vector[Double] =
      Vector.tabulate(3): column =>
        var row = 0
        var sum = 0.0
        while row < 3 do
          val value = affine.matrix(row, column)
          sum += value * value
          row += 1
        math.sqrt(sum)

    def neuroObliquity: Vector[Double] =
      val sizes = neuroVoxelSizes
      require(
        sizes.forall(_ != 0.0),
        "cannot compute obliquity for zero-length voxel axes"
      )
      Vector.tabulate(3): row =>
        var column = 0
        var best = 0.0
        while column < 3 do
          val cosine = math.abs(affine.matrix(row, column) / sizes(column))
          if cosine > best then best = cosine
          column += 1
        math.acos(math.max(-1.0, math.min(1.0, best)))

    def rescaledVoxelGeometry(
        shape: Vector[Int],
        voxelSizes: Vector[Double],
        newShape: Option[Vector[Int]] = None
    ): Either[GeometryError, Affine[D3]] =
      val outShape = newShape.getOrElse(shape)
      require(
        shape.length == 3 && voxelSizes.length == 3 && outShape.length == 3,
        "shape, voxelSizes, and newShape must contain three spatial values"
      )
      require(
        shape.forall(_ > 0) && outShape.forall(_ > 0),
        "shape and newShape must be positive"
      )
      require(
        voxelSizes.forall(value => value.isFinite && value > 0.0),
        "voxelSizes must be positive and finite"
      )

      val currentSizes = neuroVoxelSizes
      require(
        currentSizes.forall(_ != 0.0),
        "cannot rescale affine with a zero voxel size"
      )
      val centerIn = shape.map(value => math.floor((value - 1).toDouble / 2.0))
      val centerOut =
        outShape.map(value => math.floor((value - 1).toDouble / 2.0))
      val linear = Vector.tabulate(9): flat =>
        val row = flat / 3
        val column = flat % 3
        affine.matrix(row, column) *
          (voxelSizes(column) / currentSizes(column))
      val shiftedCenter = Vector.tabulate(3): row =>
        var column = 0
        var sum = 0.0
        while column < 3 do
          sum += linear(row * 3 + column) * centerOut(column)
          column += 1
        sum
      affine(centerIn).flatMap: centroid =>
        val translation =
          Vector.tabulate(3)(axis => centroid(axis) - shiftedCenter(axis))
        Affine.fromRowMajor[D3](
          Vector(
            linear(0), linear(1), linear(2), translation(0),
            linear(3), linear(4), linear(5), translation(1),
            linear(6), linear(7), linear(8), translation(2),
            0.0, 0.0, 0.0, 1.0
          )
        )
