package scalafim.image

import image4s.ImageError
import image4s.ImageMetadata
import image4s.Mask as MaskSemantics
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.locus.GridDomain
import image4s.locus.GridDomainError
import locus4s.Region
import locus4s.SpaceMismatch
import ravel.Array1
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import ravel.Shape

enum MaskRegionError:
  case Domain(error: GridDomainError)
  case RegionSpace(error: SpaceMismatch)
  case Image(error: ImageError)

  def message: String =
    this match
      case Domain(error) => error.message
      case RegionSpace(error) => error.message
      case Image(error) => error.message

object Mask:

  type MaskVol = SomeMaskVolume

  /** Convert a semantic dense mask into an exact voxel region. */
  def region[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      mask: SomeMaskVolume
  ): Either[MaskRegionError, Region[S]] =
    domain
      .spatialField(mask.sampled)
      .left
      .map(MaskRegionError.Domain.apply)
      .map: field =>
        Region.tabulate(domain.space)(index => field(index))

  /** Materialize a semantic dense mask from an exact voxel region. */
  def fromRegion[F <: Frame[D3], S, T](
      domain: GridDomain[F, D3, S],
      region: Region[T],
      metadata: ImageMetadata = ImageMetadata.empty
  ): Either[
    MaskRegionError,
    SomeMaskVolume
  ] =
    if !domain.space.sameRuntimeOwnerAs(region.space) then
      Left(
        MaskRegionError.RegionSpace(
          SpaceMismatch.between(domain.space, region.space)
        )
      )
    else
      val shape = domain.grid.shape
      val values =
        NDArray.build[Boolean, Rank[3]](
          Shape(shape(0), shape(1), shape(2))
        ): output =>
          var ordinal = 0
          while ordinal < domain.space.size do
            output.writeLinear(ordinal, false)
            ordinal += 1
          region.foreachIndex: index =>
            output.writeLinear(index.ordinal, true)
      val sampleSpace =
        SampleSpace.create(domain.grid, NonSpatialAxes.empty)
      NeuroVolume
        .mask(sampleSpace, values, metadata)
        .left
        .map(MaskRegionError.Image.apply)
        .map(SomeNeuroVolume.eraseSpace)

  def fromIndices(space: SomeSampleSpace, indices: Array1[Int], label: String = ""): MaskVol =
    val volumeSpace =
      VolumeSpace
        .fromSpatialPart(space)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    val shape = volumeSpace.shape
    val flags =
      NDArray.build[Boolean, Rank[3]](
        Shape(shape.x, shape.y, shape.z)
      ): output =>
        var position = 0
        while position < indices.size do
          val ordinal = indices(position)
          require(
            ordinal >= 0 && ordinal < volumeSpace.nVoxels,
            s"mask ordinal $ordinal is outside [0, ${volumeSpace.nVoxels})"
          )
          output.writeLinear(ordinal, true)
          position += 1
    SomeNeuroVolume.unsafeFromRavel(flags, volumeSpace.toSampleSpace, label)

  def fromIndices(space: SomeSampleSpace, indices: Array[Int], label: String): MaskVol =
    fromIndices(
      space,
      NDArray.fromSeq(Shape(indices.length), indices),
      label
    )

  def fromIndices(space: SomeSampleSpace, indices: Array[Int]): MaskVol =
    fromIndices(space, indices, "")

  def indices(mask: MaskVol): Array1[Int] =
    var count = 0
    var i = 0
    while i < mask.values.size do
      if mask.valueAtCanonicalOrdinal(i) then count += 1
      i += 1
    NDArray.build[Int, Rank[1]](Shape(count)): output =>
      var linear = 0
      var position = 0
      while linear < mask.values.size do
        if mask.valueAtCanonicalOrdinal(linear) then
          output.writeLinear(position, linear)
          position += 1
        linear += 1

  def all(space: SomeSampleSpace, label: String = ""): MaskVol =
    val shape = space.spatialShape
    SomeNeuroVolume.unsafeFromRavel[Boolean, MaskSemantics](
      NDArray.fill(Shape(shape.x, shape.y, shape.z), true),
      space.spatialSpace,
      label
    )

  def of[A, Sem](vol: SomeNeuroVolume[A, Sem]): MaskVol =
    all(vol.space, vol.label)

  @scala.annotation.targetName("ofNeuroSeries")
  def of[A, Sem](vec: SomeNeuroSeries[A, Sem]): MaskVol =
    all(vec.space.spatialSpace, vec.label)
