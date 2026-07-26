package scalafim.locus

enum SpaceError:
  case EmptyKey
  case NegativeSize(size: Int)

  def message: String =
    this match
      case EmptyKey =>
        "space key must be non-empty"
      case NegativeSize(size) =>
        s"space size must be non-negative, found $size"

final case class SpaceMismatch(
    expectedKey: SpaceKey,
    expectedSize: Int,
    actualKey: SpaceKey,
    actualSize: Int
):
  def message: String =
    s"space mismatch: expected ${expectedKey.value}[$expectedSize], " +
      s"found ${actualKey.value}[$actualSize]"

enum PointError:
  case OutOfBounds(pointOrdinal: Int, size: Int)

  def message: String =
    this match
      case OutOfBounds(pointOrdinal, size) =>
        s"point ordinal $pointOrdinal is outside [0, $size)"

enum RegionError:
  case OutOfBounds(position: Int, pointOrdinal: Int, size: Int)

  def message: String =
    this match
      case OutOfBounds(position, pointOrdinal, size) =>
        s"region ordinal at position $position is outside [0, $size): $pointOrdinal"

enum SelectionError:
  case OutOfBounds(position: Int, pointOrdinal: Int, size: Int)
  case DuplicateOrdinal(pointOrdinal: Int)

  def message: String =
    this match
      case OutOfBounds(position, pointOrdinal, size) =>
        s"selection ordinal at position $position is outside [0, $size): $pointOrdinal"
      case DuplicateOrdinal(pointOrdinal) =>
        s"selection contains duplicate ordinal $pointOrdinal"

enum TotalMapError:
  case WrongTargetCount(expected: Int, actual: Int)
  case TargetOutOfBounds(sourceOrdinal: Int, targetOrdinal: Int, targetSize: Int)

  def message: String =
    this match
      case WrongTargetCount(expected, actual) =>
        s"total map requires $expected targets, found $actual"
      case TargetOutOfBounds(sourceOrdinal, targetOrdinal, targetSize) =>
        s"target for source ordinal $sourceOrdinal is outside [0, $targetSize): $targetOrdinal"

enum MapEvidenceError:
  case DuplicateTarget(targetOrdinal: Int, firstSourceOrdinal: Int, secondSourceOrdinal: Int)
  case MissingTarget(targetOrdinal: Int)

  def message: String =
    this match
      case DuplicateTarget(target, first, second) =>
        s"target ordinal $target has source ordinals $first and $second"
      case MissingTarget(target) =>
        s"target ordinal $target has no source point"
