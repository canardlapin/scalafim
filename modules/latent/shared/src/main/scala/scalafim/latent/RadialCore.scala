package scalafim.latent

enum RadialBasisError:
  case NonPositiveSigma(value: Double)
  case InvalidThreshold(value: Double)
  case NonPositiveRadiusFactor(value: Double)
  case NegativeLevelCount(label: String, value: Int)
  case NonPositiveTinyMaskLimit(value: Int)
  case NonPositiveTinySigmaScale(value: Double)
  case InvalidDistance(value: Double)
  case NonFiniteCoordinate(label: String, axis: String, value: Double)
  case InvalidActiveVoxelIndices(reason: String)
  case ActiveIndexCountMismatch(expected: Int, actual: Int)
  case MissingActiveIndices
  case InvalidMaskDimensions(reason: String)
  case ActiveIndexOutOfBounds(index: Int, size: Int)
  case DuplicateActiveIndex(index: Int)
  case InvalidSharedBasisArtifact(reason: String)
  case EmptyAtoms
  case EmptyActiveCoordinates

  def message: String =
    this match
      case NonPositiveSigma(value) =>
        s"radial basis sigma must be finite and positive; got $value"
      case InvalidThreshold(value) =>
        s"radial basis value threshold must be finite and non-negative; got $value"
      case NonPositiveRadiusFactor(value) =>
        s"radial basis radius factor must be finite and positive; got $value"
      case NegativeLevelCount(label, value) =>
        s"$label must be non-negative; got $value"
      case NonPositiveTinyMaskLimit(value) =>
        s"radial basis tiny-mask limit must be positive; got $value"
      case NonPositiveTinySigmaScale(value) =>
        s"radial basis tiny-mask sigma scale must be finite and positive; got $value"
      case InvalidDistance(value) =>
        s"radial basis distance must be finite and non-negative; got $value"
      case NonFiniteCoordinate(label, axis, value) =>
        s"$label $axis coordinate must be finite; got $value"
      case InvalidActiveVoxelIndices(reason) =>
        s"invalid radial basis active voxel indices: $reason"
      case ActiveIndexCountMismatch(expected, actual) =>
        s"radial basis active index count must match active coordinate count: expected $expected, got $actual"
      case MissingActiveIndices =>
        "radial basis active indices are required to build a shared-basis artifact"
      case InvalidMaskDimensions(reason) =>
        s"invalid radial basis mask dimensions: $reason"
      case ActiveIndexOutOfBounds(index, size) =>
        s"radial basis active index $index out of bounds for mask size $size"
      case DuplicateActiveIndex(index) =>
        s"radial basis active indices contain duplicate index $index"
      case InvalidSharedBasisArtifact(reason) =>
        s"invalid radial shared-basis artifact: $reason"
      case EmptyAtoms =>
        "radial basis atom set must be non-empty"
      case EmptyActiveCoordinates =>
        "radial basis active coordinates must be non-empty"

opaque type RadialSigmaMm = Double

object RadialSigmaMm:
  def apply(value: Double): Either[RadialBasisError, RadialSigmaMm] =
    if value > 0.0 && value.isFinite then Right(value)
    else Left(RadialBasisError.NonPositiveSigma(value))

  def unsafe(value: Double): RadialSigmaMm =
    apply(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (sigma: RadialSigmaMm)
    inline def value: Double = sigma
    def metadataValue: String = sigma.toString

opaque type RadialValueThreshold = Double

object RadialValueThreshold:
  val Zero: RadialValueThreshold = 0.0

  def apply(value: Double): Either[RadialBasisError, RadialValueThreshold] =
    if value >= 0.0 && value.isFinite then Right(value)
    else Left(RadialBasisError.InvalidThreshold(value))

  def unsafe(value: Double): RadialValueThreshold =
    apply(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (threshold: RadialValueThreshold)
    inline def value: Double = threshold
    def metadataValue: String = threshold.toString

opaque type RadialRadiusFactor = Double

object RadialRadiusFactor:
  def apply(value: Double): Either[RadialBasisError, RadialRadiusFactor] =
    if value > 0.0 && value.isFinite then Right(value)
    else Left(RadialBasisError.NonPositiveRadiusFactor(value))

  def unsafe(value: Double): RadialRadiusFactor =
    apply(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (factor: RadialRadiusFactor)
    inline def value: Double = factor
    def metadataValue: String = factor.toString

opaque type RadialLevelCount = Int

object RadialLevelCount:
  def apply(label: String, value: Int): Either[RadialBasisError, RadialLevelCount] =
    if value >= 0 then Right(value)
    else Left(RadialBasisError.NegativeLevelCount(label, value))

  def unsafe(label: String, value: Int): RadialLevelCount =
    apply(label, value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (levels: RadialLevelCount)
    inline def value: Int = levels
    def metadataValue: String = levels.toString

final case class WorldCoordinate3D private (x: Double, y: Double, z: Double):
  def squaredDistanceTo(that: WorldCoordinate3D): Double =
    val dx = x - that.x
    val dy = y - that.y
    val dz = z - that.z
    dx * dx + dy * dy + dz * dz

  def distanceTo(that: WorldCoordinate3D): Double =
    math.sqrt(squaredDistanceTo(that))

  def toVector: Vector[Double] =
    Vector(x, y, z)

object WorldCoordinate3D:
  def apply(x: Double, y: Double, z: Double): Either[RadialBasisError, WorldCoordinate3D] =
    firstNonFinite("world coordinate", x, y, z) match
      case Some(error) => Left(error)
      case None        => Right(new WorldCoordinate3D(x, y, z))

  def unsafe(x: Double, y: Double, z: Double): WorldCoordinate3D =
    apply(x, y, z).fold(error => throw IllegalArgumentException(error.message), identity)

  private def firstNonFinite(
      label: String,
      x: Double,
      y: Double,
      z: Double
  ): Option[RadialBasisError] =
    if !x.isFinite then Some(RadialBasisError.NonFiniteCoordinate(label, "x", x))
    else if !y.isFinite then Some(RadialBasisError.NonFiniteCoordinate(label, "y", y))
    else if !z.isFinite then Some(RadialBasisError.NonFiniteCoordinate(label, "z", z))
    else None

enum RadialKernel(val metadataValue: String):
  case Gaussian extends RadialKernel("gaussian")
  case WendlandC4 extends RadialKernel("wendland_c4")
  case WendlandC6 extends RadialKernel("wendland_c6")

  def value(distanceMm: Double, sigma: RadialSigmaMm): Either[RadialBasisError, Double] =
    if distanceMm < 0.0 || !distanceMm.isFinite then Left(RadialBasisError.InvalidDistance(distanceMm))
    else Right(valueUnchecked(distanceMm, sigma))

  private[latent] def valueUnchecked(distanceMm: Double, sigma: RadialSigmaMm): Double =
    this match
      case Gaussian =>
        val scaled = distanceMm / sigma.value
        math.exp(-0.5 * scaled * scaled)
      case WendlandC4 =>
        val r = distanceMm / sigma.value
        if r >= 1.0 then 0.0
        else
          val oneMinus = 1.0 - r
          math.pow(oneMinus, 6.0) * (35.0 * r * r + 18.0 * r + 3.0) / 3.0
      case WendlandC6 =>
        val r = distanceMm / sigma.value
        if r >= 1.0 then 0.0
        else
          val oneMinus = 1.0 - r
          math.pow(oneMinus, 8.0) * (32.0 * r * r * r + 25.0 * r * r + 8.0 * r + 1.0)

final case class RadialAtom(center: WorldCoordinate3D, sigma: RadialSigmaMm, level: Int = 0):
  require(level >= 0, "radial atom level must be non-negative")

final class RadialBasisSpec private (
    val kernel: RadialKernel,
    val sigma0: RadialSigmaMm,
    val levels: RadialLevelCount,
    val radiusFactor: RadialRadiusFactor,
    val extraFineLevels: RadialLevelCount,
    val seed: Long,
    val threshold: RadialValueThreshold,
    val tinyMaskIdentity: Boolean,
    val tinyMaskLimit: Int,
    val tinySigmaScale: Double,
    val normalizeAtoms: Boolean
):
  def maxLevel: Int =
    levels.value + extraFineLevels.value

  def metadata: Map[String, String] =
    Map(
      "radial.kernel" -> kernel.metadataValue,
      "radial.sigma0" -> sigma0.metadataValue,
      "radial.levels" -> levels.metadataValue,
      "radial.radius_factor" -> radiusFactor.metadataValue,
      "radial.extra_fine_levels" -> extraFineLevels.metadataValue,
      "radial.seed" -> seed.toString,
      "radial.threshold" -> threshold.metadataValue,
      "radial.tiny_mask_identity" -> tinyMaskIdentity.toString,
      "radial.tiny_mask_limit" -> tinyMaskLimit.toString,
      "radial.normalize_atoms" -> normalizeAtoms.toString
    )

  override def equals(other: Any): Boolean =
    other match
      case that: RadialBasisSpec =>
        kernel == that.kernel &&
          sigma0 == that.sigma0 &&
          levels == that.levels &&
          radiusFactor == that.radiusFactor &&
          extraFineLevels == that.extraFineLevels &&
          seed == that.seed &&
          threshold == that.threshold &&
          tinyMaskIdentity == that.tinyMaskIdentity &&
          tinyMaskLimit == that.tinyMaskLimit &&
          tinySigmaScale == that.tinySigmaScale &&
          normalizeAtoms == that.normalizeAtoms
      case _ =>
        false

  override def hashCode(): Int =
    (
      kernel,
      sigma0,
      levels,
      radiusFactor,
      extraFineLevels,
      seed,
      threshold,
      tinyMaskIdentity,
      tinyMaskLimit,
      tinySigmaScale,
      normalizeAtoms
    ).##

object RadialBasisSpec:
  def apply(
      kernel: RadialKernel = RadialKernel.Gaussian,
      sigma0: Double = 6.0,
      levels: Int = 3,
      radiusFactor: Double = 2.5,
      extraFineLevels: Int = 0,
      seed: Long = 1L,
      threshold: Double = 1e-8,
      tinyMaskIdentity: Boolean = true,
      tinyMaskLimit: Int = 64,
      tinySigmaScale: Double = 100.0,
      normalizeAtoms: Boolean = true
  ): Either[RadialBasisError, RadialBasisSpec] =
    for
      sigma <- RadialSigmaMm(sigma0)
      checkedLevels <- RadialLevelCount("radial basis levels", levels)
      factor <- RadialRadiusFactor(radiusFactor)
      checkedExtra <- RadialLevelCount("radial basis extra fine levels", extraFineLevels)
      checkedThreshold <- RadialValueThreshold(threshold)
      _ <-
        if tinyMaskLimit > 0 then Right(())
        else Left(RadialBasisError.NonPositiveTinyMaskLimit(tinyMaskLimit))
      _ <-
        if tinySigmaScale > 0.0 && tinySigmaScale.isFinite then Right(())
        else Left(RadialBasisError.NonPositiveTinySigmaScale(tinySigmaScale))
    yield
      new RadialBasisSpec(
        kernel = kernel,
        sigma0 = sigma,
        levels = checkedLevels,
        radiusFactor = factor,
        extraFineLevels = checkedExtra,
        seed = seed,
        threshold = checkedThreshold,
        tinyMaskIdentity = tinyMaskIdentity,
        tinyMaskLimit = tinyMaskLimit,
        tinySigmaScale = tinySigmaScale,
        normalizeAtoms = normalizeAtoms
      )

  def unsafe(
      kernel: RadialKernel = RadialKernel.Gaussian,
      sigma0: Double = 6.0,
      levels: Int = 3,
      radiusFactor: Double = 2.5,
      extraFineLevels: Int = 0,
      seed: Long = 1L,
      threshold: Double = 1e-8,
      tinyMaskIdentity: Boolean = true,
      tinyMaskLimit: Int = 64,
      tinySigmaScale: Double = 100.0,
      normalizeAtoms: Boolean = true
  ): RadialBasisSpec =
    apply(
      kernel = kernel,
      sigma0 = sigma0,
      levels = levels,
      radiusFactor = radiusFactor,
      extraFineLevels = extraFineLevels,
      seed = seed,
      threshold = threshold,
      tinyMaskIdentity = tinyMaskIdentity,
      tinyMaskLimit = tinyMaskLimit,
      tinySigmaScale = tinySigmaScale,
      normalizeAtoms = normalizeAtoms
    ).fold(error => throw IllegalArgumentException(error.message), identity)
