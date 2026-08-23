package scalafim.image

enum GridMismatch:
  case Image(expected: NeuroSpace, actual: NeuroSpace)
  case Volume(expected: VolumeSpace, actual: VolumeSpace)

  def message: String =
    this match
      case Image(expected, actual) =>
        s"image grid mismatch: expected $expected, got $actual"
      case Volume(expected, actual) =>
        s"volume grid mismatch: expected ${expected.toNeuroSpace}, got ${actual.toNeuroSpace}"

/** Explicit evidence that two independently owned grids have been checked as
  * coordinate-congruent. This is the only admission path for separately
  * decoded files whose live grid identities deliberately differ.
  */
final class CertifiedGridCongruence private[image] (
    private[scalafim] val expected: NeuroSpace,
    private[scalafim] val actual: NeuroSpace,
    val tolerance: Double,
    val includesNonSpatialAxes: Boolean
)

object GridCompatibility:
  def exact(expected: NeuroSpace, actual: NeuroSpace): Either[GridMismatch, Unit] =
    if sameCanonicalSpace(expected, actual, includeNonSpatial = true) then Right(())
    else Left(GridMismatch.Image(expected, actual))

  def volume(expected: VolumeSpace, actual: VolumeSpace): Either[GridMismatch, Unit] =
    if sameCanonicalSpace(
        expected.toNeuroSpace,
        actual.toNeuroSpace,
        includeNonSpatial = false
      )
    then Right(())
    else Left(GridMismatch.Volume(expected, actual))

  def spatial(expected: NeuroSpace, actual: NeuroSpace): Either[GridMismatch, Unit] =
    for
      expectedVolume <- VolumeSpace
        .fromSpatialPart(expected)
        .left
        .map(_ => GridMismatch.Image(expected, actual))
      actualVolume <- VolumeSpace
        .fromSpatialPart(actual)
        .left
        .map(_ => GridMismatch.Image(expected, actual))
      _ <- volume(expectedVolume, actualVolume)
    yield ()

  def certifyExactCongruence(
      expected: NeuroSpace,
      actual: NeuroSpace,
      tolerance: Double
  ): Either[GridMismatch, CertifiedGridCongruence] =
    certifyCongruence(
      expected,
      actual,
      tolerance,
      includeNonSpatial = true
    )

  def certifySpatialCongruence(
      expected: NeuroSpace,
      actual: NeuroSpace,
      tolerance: Double
  ): Either[GridMismatch, CertifiedGridCongruence] =
    certifyCongruence(
      expected,
      actual,
      tolerance,
      includeNonSpatial = false
    )

  def acceptCertifiedExact(
      certificate: CertifiedGridCongruence,
      expected: NeuroSpace,
      actual: NeuroSpace
  ): Either[GridMismatch, Unit] =
    if certificate.includesNonSpatialAxes &&
        sameCanonicalSpace(certificate.expected, expected, includeNonSpatial = true) &&
        sameCanonicalSpace(certificate.actual, actual, includeNonSpatial = true)
    then Right(())
    else Left(GridMismatch.Image(expected, actual))

  def acceptCertifiedSpatial(
      certificate: CertifiedGridCongruence,
      expected: NeuroSpace,
      actual: NeuroSpace
  ): Either[GridMismatch, Unit] =
    if sameCanonicalSpace(certificate.expected, expected, includeNonSpatial = true) &&
        sameCanonicalSpace(certificate.actual, actual, includeNonSpatial = true)
    then Right(())
    else Left(GridMismatch.Image(expected, actual))

  private[scalafim] def requireExact(expected: NeuroSpace, actual: NeuroSpace): Unit =
    exact(expected, actual).fold(error => throw new IllegalArgumentException(error.message), identity)

  private[scalafim] def requireVolume(expected: VolumeSpace, actual: VolumeSpace): Unit =
    volume(expected, actual).fold(error => throw new IllegalArgumentException(error.message), identity)

  private[scalafim] def requireSpatial(expected: NeuroSpace, actual: NeuroSpace): Unit =
    spatial(expected, actual).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def sameCanonicalSpace(
      expected: NeuroSpace,
      actual: NeuroSpace,
      includeNonSpatial: Boolean
  ): Boolean =
    val left = NeuroSpace.canonical(expected)
    val right = NeuroSpace.canonical(actual)
    val leftFrame = left.grid.frame
    val rightFrame = right.grid.frame
    val alignedFrames =
      leftFrame.sameRuntimeOwnerAs(rightFrame) ||
        leftFrame.samePersistentKeyAs(rightFrame)
    val sameGrid =
      left.spatialRank == right.spatialRank &&
        alignedFrames &&
        left.grid.shape == right.grid.shape &&
        left.grid.indexToFrame.rowMajor == right.grid.indexToFrame.rowMajor
    sameGrid &&
      (!includeNonSpatial ||
        left.nonSpatialAxes.records == right.nonSpatialAxes.records)

  private def certifyCongruence(
      expected: NeuroSpace,
      actual: NeuroSpace,
      tolerance: Double,
      includeNonSpatial: Boolean
  ): Either[GridMismatch, CertifiedGridCongruence] =
    val left = NeuroSpace.canonical(expected)
    val right = NeuroSpace.canonical(actual)
    val finiteTolerance = tolerance.isFinite && tolerance >= 0.0
    val sameFrameContract =
      left.grid.frame.spatialRank == right.grid.frame.spatialRank &&
        left.grid.frame.unit == right.grid.frame.unit &&
        left.grid.frame.convention == right.grid.frame.convention
    val sameGrid =
      left.spatialRank == right.spatialRank &&
        sameFrameContract &&
        left.grid.shape == right.grid.shape &&
        left.grid.indexToFrame.rowMajor
          .zip(right.grid.indexToFrame.rowMajor)
          .forall((a, b) => math.abs(a - b) <= tolerance)
    val sameAxes =
      !includeNonSpatial ||
        left.nonSpatialAxes.records == right.nonSpatialAxes.records
    if finiteTolerance && sameGrid && sameAxes then
      Right(
        new CertifiedGridCongruence(
          expected,
          actual,
          tolerance,
          includeNonSpatial
        )
      )
    else Left(GridMismatch.Image(expected, actual))
