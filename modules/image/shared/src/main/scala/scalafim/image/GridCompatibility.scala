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
