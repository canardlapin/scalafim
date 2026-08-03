package scalafim.image

import ravel.NDArray as RavelArray
import ravel.Rank
import ravel.Shape
import scala.reflect.ClassTag
import spire.algebra.Ring

final case class NeuroHyperVec[A](
  data: RavelArray[A, Rank[3]],  // shape: features x trials x voxels
  space: NeuroSpace,             // dims: x,y,z,trials,features
  mask: NeuroVol[Boolean],
  map: IndexLookupVol,
  label: String = ""
):
  require(space.ndim >= 5, "NeuroHyperVec space must be 5D")
  GridCompatibility.requireSpatial(space, mask.space)

  val nTrials: Int = space.dims(3)
  val nFeatures: Int = space.dims(4)

  GridCompatibility.requireSpatial(space, map.space)
  require(
    data.shape == Shape(nFeatures, nTrials, map.cardinality),
    "data shape must be (features, trials, voxels)"
  )

  def series(linearSpatial: Int)(using
      ClassTag[A],
      Ring[A]
  ): RavelArray[A, Rank[2]] =
    val pos = map.lookup(linearSpatial)
    val zero = summon[Ring[A]].zero
    given ravel.DType[A] = data.dtype
    RavelArray.tabulate[A](nFeatures, nTrials) { (feature, trial) =>
      if pos >= 0 then data(feature, trial, pos) else zero
    }

  def volume(trial: Int, feature: Int): SparseNeuroVol[A] =
    require(trial >= 0 && trial < nTrials, "trial out of bounds")
    require(feature >= 0 && feature < nFeatures, "feature out of bounds")
    given ravel.DType[A] = data.dtype
    val vals =
      RavelArray.tabulate[A](map.cardinality): p =>
        data(feature, trial, p)
    SparseNeuroVol(vals, map.indices, space.spatialSpace, label)

object NeuroHyperVec:
  def fromDense[A](
    data: RavelArray[A, Rank[3]],
    space: NeuroSpace,
    mask: NeuroVol[Boolean],
    label: String = ""
  )(using ClassTag[A]): NeuroHyperVec[A] =
    val idx = Mask.indices(mask)
    val lookup = IndexLookupVol(space, idx)
    NeuroHyperVec(data, space, mask, lookup, label)

  def apply[A](
    data: RavelArray[A, Rank[3]],
    space: NeuroSpace,
    mask: NeuroVol[Boolean]
  )(using ClassTag[A]): NeuroHyperVec[A] =
    fromDense(data, space, mask)

  def apply[A](
    data: RavelArray[A, Rank[3]],
    space: NeuroSpace,
    mask: NeuroVol[Boolean],
    label: String
  )(using ClassTag[A]): NeuroHyperVec[A] =
    fromDense(data, space, mask, label)
