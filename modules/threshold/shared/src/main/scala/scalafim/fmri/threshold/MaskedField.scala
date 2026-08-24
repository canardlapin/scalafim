package scalafim.fmri.threshold

import scalafim.image.{
  GridCompatibility,
  Mask,
  PrimitiveBuffers,
  NeuroSpace,
  NeuroVol,
  VolumeDomain
}
import scalafim.locus.{
  DomainFactory,
  FiniteDomain,
  Injection,
  Region,
  Selection,
  TotalMap,
  mapping
}

sealed abstract class MaskedField private[threshold] (
    val space: NeuroSpace,
    val mask: NeuroVol[Boolean],
    private[threshold] val data: Array[Double],
    private[threshold] val volumeIndex: Array[Int],
    private[threshold] val x: Array[Int],
    private[threshold] val y: Array[Int],
    private[threshold] val z: Array[Int]
):
  require(data.length == volumeIndex.length, "masked field arrays must align")
  require(data.length == x.length && x.length == y.length && y.length == z.length, "coordinate arrays must align")

  type FullVoxel
  type ActiveVoxel

  val fullDomain: VolumeDomain[FullVoxel]
  val activeSpace: FiniteDomain[ActiveVoxel]
  val activeSelection: Selection[FullVoxel]
  val support: Region[FullVoxel]
  val activeToFull: Injection[ActiveVoxel, FullVoxel]

  def size: Int = data.length

  def value(index: Int): Double = data(index)

  def originalIndex(index: Int): Int = volumeIndex(index)

  def coord(index: Int): (Int, Int, Int) =
    (x(index), y(index), z(index))

  def valuesCopy: Array[Double] =
    data.clone

  def volumeIndicesCopy: Array[Int] =
    volumeIndex.clone

  def volumeIndices(maskSpaceIndices: Array[Int]): Either[ThresholdError, Array[Int]] =
    val out = new Array[Int](maskSpaceIndices.length)
    var i = 0
    while i < maskSpaceIndices.length do
      val idx = maskSpaceIndices(i)
      if idx < 0 || idx >= size then return Left(ThresholdError.IndexOutOfBounds(idx, size))
      val active = activeSpace.indexOption(idx).get
      out(i) = activeToFull.mapping(active).value
      i += 1
    Right(out)

  def maskFromMaskSpace(indices: Array[Int], label: String = ""): Either[ThresholdError, NeuroVol[Boolean]] =
    volumeIndices(indices).map { full =>
      Mask.fromIndices(space, PrimitiveBuffers.fromArray(full), label)
    }

object MaskedField:

  def fromVolume(stat: NeuroVol[Double], tail: Tail = Tail.Positive): Either[ThresholdError, MaskedField] =
    fromStatisticMap(StatisticMap.z(stat), tail.alternative)

  def fromStatisticMap(
    statistic: StatisticMap,
    alternative: ThresholdAlternative = ThresholdAlternative.Greater
  ): Either[ThresholdError, MaskedField] =
    val stat = statistic.volume
    val n = stat.space.spatialDims.product
    val flags = PrimitiveBuffers.fillConst[Boolean](n, false)
    var i = 0
    while i < n do
      flags(i) = stat.linear(i).isFinite
      i += 1
    fromStatisticMap(statistic, NeuroVol.fromLinear(flags, stat.space.spatialSpace, stat.label), alternative)

  def fromVolume(
    stat: NeuroVol[Double],
    mask: NeuroVol[Boolean],
    tail: Tail
  ): Either[ThresholdError, MaskedField] =
    fromStatisticMap(StatisticMap.z(stat), mask, tail.alternative)

  def fromStatisticMap(
    statistic: StatisticMap,
    mask: NeuroVol[Boolean],
    alternative: ThresholdAlternative
  ): Either[ThresholdError, MaskedField] =
    alternative.validate(statistic.orientation) match
      case Left(err) => return Left(err)
      case Right(()) => ()

    val stat = statistic.volume
    GridCompatibility.spatial(stat.space, mask.space) match
      case Left(error) =>
        return Left(
          ThresholdError.ShapeMismatch(
            "stat/mask space",
            stat.space.spatialSpace.toString,
            error.message
          )
        )
      case Right(_) =>
        ()

    val dims = stat.space.spatialDims
    val nx = dims(0)
    val ny = dims(1)
    val plane = nx * ny
    val n = dims.product

    val valueBuilder = Array.newBuilder[Double]
    val indexBuilder = Array.newBuilder[Int]
    val xBuilder = Array.newBuilder[Int]
    val yBuilder = Array.newBuilder[Int]
    val zBuilder = Array.newBuilder[Int]

    var lin = 0
    while lin < n do
      if mask.linear(lin) then
        val raw = stat.linear(lin)
        if !raw.isFinite then return Left(ThresholdError.NonFiniteData("stat volume inside mask"))
        if statistic.orientation == EvidenceOrientation.Unsigned && raw < 0.0 then
          return Left(ThresholdError.NegativeUnsignedEvidence(lin, raw))
        valueBuilder += alternative.applyTo(raw)
        indexBuilder += lin
        xBuilder += (lin % nx)
        yBuilder += ((lin / nx) % ny)
        zBuilder += (lin / plane)
      lin += 1

    val values = valueBuilder.result()
    if values.isEmpty then Left(ThresholdError.EmptyMask)
    else
      Right(
        make(
          stat.space.spatialSpace,
          mask,
          values,
          indexBuilder.result(),
          xBuilder.result(),
          yBuilder.result(),
          zBuilder.result()
        )
      )

  private def make(
      space: NeuroSpace,
      mask: NeuroVol[Boolean],
      data: Array[Double],
      volumeIndex: Array[Int],
      x: Array[Int],
      y: Array[Int],
      z: Array[Int]
  ): MaskedField =
    val packedFullDomain =
      VolumeDomain.structuralCompatibility(space.asVolumeSpace.toOption.get)
    type Full = packedFullDomain.S
    val full: VolumeDomain[Full] = packedFullDomain.value
    // A derived selection-position domain: it means nothing except relative to
    // `full`, and `injection` below is what relates the two. Ephemeral, so no
    // key proportional to the mask has to be built or retained.
    val activeDomain = DomainFactory.unsafeEphemeral("threshold-active", data.length)
    type Active = activeDomain.S
    val active: FiniteDomain[Active] = activeDomain.value
    val selected =
      Selection
        .fromOrdinals(full.finiteSpace, volumeIndex)
        .toOption
        .get
    val injection =
      Injection
        .validate(
          TotalMap
            .fromTargetOrdinals(
              active,
              full.finiteSpace,
              volumeIndex
            )
            .toOption
            .get
        )
        .toOption
        .get

    new MaskedField(space, mask, data, volumeIndex, x, y, z):
      type FullVoxel = Full
      type ActiveVoxel = Active
      val fullDomain: VolumeDomain[Full] = full
      val activeSpace: FiniteDomain[Active] = active
      val activeSelection: Selection[Full] = selected
      val support: Region[Full] = selected.region
      val activeToFull: Injection[Active, Full] = injection
