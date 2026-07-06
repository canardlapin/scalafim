package scalafim.image

import narr.NArray
import scala.reflect.ClassTag
import spire.algebra.Ring

final case class NeuroHyperVec[A](
  data: NDArray[A],              // shape: features x trials x voxels
  space: NeuroSpace,             // dims: x,y,z,trials,features
  mask: NeuroVol[Boolean],
  map: IndexLookupVol,
  label: String = ""
):
  require(space.ndim >= 5, "NeuroHyperVec space must be 5D")
  require(mask.space.spatialDims == space.spatialDims, "mask/space mismatch")

  val nTrials: Int = space.dims(3)
  val nFeatures: Int = space.dims(4)

  require(map.space.spatialDims == space.spatialDims, "map/space mismatch")
  require(
    data.shape == Vector(nFeatures, nTrials, map.cardinality),
    "data shape must be (features, trials, voxels)"
  )

  def series(linearSpatial: Int)(using ClassTag[A], Ring[A]): NDArray[A] =
    val pos = map.lookup(linearSpatial)
    val zero = summon[Ring[A]].zero
    val out = NArrayUtil.fillConst[A](nFeatures * nTrials, zero)
    if pos >= 0 then
      var f = 0
      while f < nFeatures do
        var t = 0
        while t < nTrials do
          out(f + t * nFeatures) = data(f, t, pos)
          t += 1
        f += 1
    NDArray(out, Vector(nFeatures, nTrials))

  def volume(trial: Int, feature: Int)(using ClassTag[A]): SparseNeuroVol[A] =
    require(trial >= 0 && trial < nTrials, "trial out of bounds")
    require(feature >= 0 && feature < nFeatures, "feature out of bounds")
    val vals = NArray.ofSize[A](map.cardinality)
    var p = 0
    while p < map.cardinality do
      vals(p) = data(feature, trial, p)
      p += 1
    SparseNeuroVol(vals, map.indices, space.spatialSpace, label)

object NeuroHyperVec:
  def fromDense[A](
    data: NDArray[A],
    space: NeuroSpace,
    mask: NeuroVol[Boolean],
    label: String = ""
  )(using ClassTag[A]): NeuroHyperVec[A] =
    val idx = Mask.indices(mask)
    val lookup = IndexLookupVol(space, idx)
    NeuroHyperVec(data, space, mask, lookup, label)

  def apply[A](
    data: NDArray[A],
    space: NeuroSpace,
    mask: NeuroVol[Boolean]
  )(using ClassTag[A]): NeuroHyperVec[A] =
    fromDense(data, space, mask)

  def apply[A](
    data: NDArray[A],
    space: NeuroSpace,
    mask: NeuroVol[Boolean],
    label: String
  )(using ClassTag[A]): NeuroHyperVec[A] =
    fromDense(data, space, mask, label)
