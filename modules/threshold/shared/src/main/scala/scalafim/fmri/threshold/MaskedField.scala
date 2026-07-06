package scalafim.fmri.threshold

import scalafim.image.{Mask, NArrayUtil, NeuroSpace, NeuroVol}

final case class MaskedField private[threshold] (
    space: NeuroSpace,
    mask: NeuroVol[Boolean],
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

  def volumeIndices(maskSpaceIndices: Array[Int]): Either[ThresholdError, Array[Int]] =
    val out = new Array[Int](maskSpaceIndices.length)
    var i = 0
    while i < maskSpaceIndices.length do
      val idx = maskSpaceIndices(i)
      if idx < 0 || idx >= size then return Left(ThresholdError.IndexOutOfBounds(idx, size))
      out(i) = volumeIndex(idx)
      i += 1
    Right(out)

  def maskFromMaskSpace(indices: Array[Int], label: String = ""): Either[ThresholdError, NeuroVol[Boolean]] =
    volumeIndices(indices).map { full =>
      Mask.fromIndices(space, NArrayUtil.fromArray(full), label)
    }

object MaskedField:

  def fromVolume(stat: NeuroVol[Double], tail: Tail = Tail.Positive): Either[ThresholdError, MaskedField] =
    val n = stat.space.spatialDims.product
    val flags = NArrayUtil.fillConst[Boolean](n, false)
    var i = 0
    while i < n do
      flags(i) = stat.linear(i).isFinite
      i += 1
    fromVolume(stat, NeuroVol.fromLinear(flags, stat.space.spatialSpace, stat.label), tail)

  def fromVolume(
    stat: NeuroVol[Double],
    mask: NeuroVol[Boolean],
    tail: Tail
  ): Either[ThresholdError, MaskedField] =
    if stat.space.spatialDims != mask.space.spatialDims then
      return Left(
        ThresholdError.ShapeMismatch(
          "stat/mask",
          stat.space.spatialDims.mkString("x"),
          mask.space.spatialDims.mkString("x")
        )
      )
    if stat.space.spacing != mask.space.spacing || stat.space.origin != mask.space.origin then
      return Left(ThresholdError.ShapeMismatch("stat/mask space", "same spacing and origin", "different spacing or origin"))

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
        valueBuilder += tail.applyTo(raw)
        indexBuilder += lin
        xBuilder += (lin % nx)
        yBuilder += ((lin / nx) % ny)
        zBuilder += (lin / plane)
      lin += 1

    val values = valueBuilder.result()
    if values.isEmpty then Left(ThresholdError.EmptyMask)
    else
      Right(
        MaskedField(
          stat.space.spatialSpace,
          mask,
          values,
          indexBuilder.result(),
          xBuilder.result(),
          yBuilder.result(),
          zBuilder.result()
        )
      )
