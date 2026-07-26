package scalafim.fmri.threshold

import scalafim.image.{
  GridCompatibility,
  Mask,
  NArrayUtil,
  NeuroSpace,
  NeuroVol,
  StructuralCompatibilityVoxel,
  VolumeDomain
}
import scalafim.locus.{
  FiniteSpace,
  Injection,
  Region,
  Selection,
  SpaceKey,
  TotalMap
}

sealed trait ThresholdActiveVoxel

final class MaskedField private[threshold] (
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

  def size: Int = data.length

  def value(index: Int): Double = data(index)

  def originalIndex(index: Int): Int = volumeIndex(index)

  def coord(index: Int): (Int, Int, Int) =
    (x(index), y(index), z(index))

  def valuesCopy: Array[Double] =
    data.clone

  def volumeIndicesCopy: Array[Int] =
    volumeIndex.clone

  lazy val fullDomain: VolumeDomain[StructuralCompatibilityVoxel] =
    VolumeDomain.structuralCompatibility(space.asVolumeSpace.toOption.get)

  lazy val activeSpace: FiniteSpace[ThresholdActiveVoxel] =
    FiniteSpace
      .make[ThresholdActiveVoxel](
        SpaceKey.unsafe(
          s"${fullDomain.finiteSpace.key.value}:threshold-active:${volumeIndex.mkString(",")}"
        ),
        size
      )
      .toOption
      .get

  lazy val activeSelection: Selection[StructuralCompatibilityVoxel] =
    Selection
      .fromOrdinals(fullDomain.finiteSpace, volumeIndex)
      .toOption
      .get

  lazy val support: Region[StructuralCompatibilityVoxel] =
    activeSelection.region

  lazy val activeToFull: Injection[ThresholdActiveVoxel, StructuralCompatibilityVoxel] =
    Injection
      .validate(
        TotalMap
          .fromTargetOrdinals(
            activeSpace,
            fullDomain.finiteSpace,
            volumeIndex
          )
          .toOption
          .get
      )
      .toOption
      .get

  def volumeIndices(maskSpaceIndices: Array[Int]): Either[ThresholdError, Array[Int]] =
    val out = new Array[Int](maskSpaceIndices.length)
    var i = 0
    while i < maskSpaceIndices.length do
      val idx = maskSpaceIndices(i)
      if idx < 0 || idx >= size then return Left(ThresholdError.IndexOutOfBounds(idx, size))
      val active = activeSpace.point(idx).get
      out(i) = activeToFull.mapping(active).ordinal
      i += 1
    Right(out)

  def maskFromMaskSpace(indices: Array[Int], label: String = ""): Either[ThresholdError, NeuroVol[Boolean]] =
    volumeIndices(indices).map { full =>
      Mask.fromIndices(space, NArrayUtil.fromArray(full), label)
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
    val flags = NArrayUtil.fillConst[Boolean](n, false)
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
        new MaskedField(
          stat.space.spatialSpace,
          mask,
          values,
          indexBuilder.result(),
          xBuilder.result(),
          yBuilder.result(),
          zBuilder.result()
        )
      )
