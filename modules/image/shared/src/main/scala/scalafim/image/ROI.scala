package scalafim.image

import narr.NArray

final case class ROICoords(coords: Vector[Vector[Int]]):
  require(coords.forall(_.length == 3), "coords must be Nx3")
  def size: Int = coords.length

  def linearIndices(space: NeuroSpace): NArray[Int] =
    val out = narr.NArray.ofSize[Int](coords.length)
    var i = 0
    while i < coords.length do
      out(i) = Indexing.gridToIndex3D(space.spatialDims, coords(i)(0), coords(i)(1), coords(i)(2))
      i += 1
    out

final case class ROIVol[A](
  space: NeuroSpace,
  coords: ROICoords,
  data: NArray[A]
):
  require(coords.size == data.length, "data length must match coords")
  require(space.ndim >= 3, "space must be at least 3D")

  def size: Int = coords.size

  def linearIndices: NArray[Int] =
    coords.linearIndices(space)

  def realCoords: Vector[Vector[Double]] =
    coords.coords.map(c => space.indexToCoord(c.map(_.toDouble)))

  def toSparse: SparseNeuroVol[A] =
    SparseNeuroVol(data, linearIndices, space)

object ROICoords:
  def apply(coords: NArray[Int]): ROICoords =
    require(coords.length % 3 == 0, "flat coords must be multiple of 3")
    val n = coords.length / 3
    val vec = Vector.tabulate(n) { i =>
      Vector(coords(3 * i), coords(3 * i + 1), coords(3 * i + 2))
    }
    ROICoords(vec)

object ROIVol:
  def apply[A](space: NeuroSpace, coords: Vector[Vector[Int]], data: NArray[A]): ROIVol[A] =
    ROIVol(space, ROICoords(coords), data)
