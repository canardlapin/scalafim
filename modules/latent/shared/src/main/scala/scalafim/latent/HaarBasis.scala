package scalafim.latent

import scalafim.linalg.DoubleMatrix

final class HaarSpec private (
    val timepoints: Int,
    val components: Int
):
  val levels: Int =
    HaarSpec.levelCount(timepoints)

  def copy(
      timepoints: Int = this.timepoints,
      components: Int = this.components
  ): Either[LatentError, HaarSpec] =
    HaarSpec(timepoints, components)

  def withComponents(value: Int): Either[LatentError, HaarSpec] =
    copy(components = value)

  override def equals(other: Any): Boolean =
    other match
      case that: HaarSpec =>
        timepoints == that.timepoints &&
          components == that.components
      case _ =>
        false

  override def hashCode(): Int =
    (timepoints, components).##

  override def toString: String =
    s"HaarSpec(timepoints=$timepoints, components=$components, levels=$levels)"

object HaarSpec:
  def apply(
      timepoints: Int,
      components: Int
  ): Either[LatentError, HaarSpec] =
    if timepoints <= 0 then Left(LatentError.NonPositiveDimension("Haar timepoints", timepoints))
    else if !isPowerOfTwo(timepoints) then Left(LatentError.NonPowerOfTwoDimension("Haar timepoints", timepoints))
    else if components <= 0 then Left(LatentError.NonPositiveDimension("Haar components", components))
    else if components > timepoints then Left(LatentError.DimensionMismatch("Haar components", timepoints, components))
    else Right(new HaarSpec(timepoints, components))

  def unsafe(
      timepoints: Int,
      components: Int
  ): HaarSpec =
    apply(timepoints, components).fold(error => throw new IllegalArgumentException(error.message), identity)

  def full(timepoints: Int): Either[LatentError, HaarSpec] =
    apply(timepoints, timepoints)

  private[latent] def isPowerOfTwo(value: Int): Boolean =
    value > 0 && (value & (value - 1)) == 0

  private[latent] def levelCount(timepoints: Int): Int =
    var value = timepoints
    var count = 0
    while value > 1 do
      value = value / 2
      count += 1
    count

object HaarBasis:
  def build(spec: HaarSpec): Either[LatentError, DoubleMatrix] =
    buildUnchecked(spec.timepoints, spec.components)

  def build(
      timepoints: Int,
      components: Int
  ): Either[LatentError, DoubleMatrix] =
    HaarSpec(timepoints, components).flatMap(build)

  def full(timepoints: Int): Either[LatentError, DoubleMatrix] =
    build(timepoints, timepoints)

  private def buildUnchecked(
      timepoints: Int,
      components: Int
  ): Either[LatentError, DoubleMatrix] =
    val out = new Array[Double](timepoints * components)
    val scaling = 1.0 / math.sqrt(timepoints.toDouble)

    var row = 0
    while row < timepoints do
      out(row * components) = scaling
      row += 1

    var column = 1
    var scale = 0
    val levels = HaarSpec.levelCount(timepoints)
    while scale < levels && column < components do
      val blocks = 1 << scale
      val blockSize = timepoints / blocks
      val halfBlock = blockSize / 2
      val amplitude = 1.0 / math.sqrt(blockSize.toDouble)
      var block = 0
      while block < blocks && column < components do
        val start = block * blockSize
        var local = 0
        while local < blockSize do
          row = start + local
          out(row * components + column) =
            if local < halfBlock then amplitude else -amplitude
          local += 1
        block += 1
        column += 1
      scale += 1

    Right(DoubleMatrix.unsafe(timepoints, components, out))
