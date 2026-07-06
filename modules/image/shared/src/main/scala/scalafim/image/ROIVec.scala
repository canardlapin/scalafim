package scalafim.image

import narr.NArray
import scala.reflect.ClassTag

final case class ROIVec[A](
  space: NeuroSpace,
  coords: ROICoords,
  data: NDArray[A]
):
  require(data.ndim == 2, "ROIVec data must be 2D (time x voxels)")
  require(data.shape(1) == coords.size, "data columns must match coords")
  require(space.ndim >= 3, "space must be at least 3D")

  def nVoxels: Int = coords.size
  def nTime: Int = data.shape(0)

  def seriesAt(voxel: Int)(using ClassTag[A]): NArray[A] =
    require(voxel >= 0 && voxel < nVoxels, "voxel index out of bounds")
    val out = NArray.ofSize[A](nTime)
    var t = 0
    while t < nTime do
      out(t) = data(t, voxel)
      t += 1
    out

  def map[B](f: A => B)(using ClassTag[B]): ROIVec[B] =
    ROIVec(space, coords, data.map(f))

object ROIVec:
  def apply[A](
    space: NeuroSpace,
    coords: Vector[Vector[Int]],
    data: NDArray[A]
  ): ROIVec[A] =
    ROIVec(space, ROICoords(coords), data)
