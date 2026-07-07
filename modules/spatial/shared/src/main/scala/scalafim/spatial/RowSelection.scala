package scalafim.spatial

enum RowSelection:
  case All
  case Rows(indices: Vector[Int])

  def roi: Option[Vector[Int]] =
    this match
      case RowSelection.All => None
      case RowSelection.Rows(indices) => Some(indices)

object RowSelection:
  def fromRoi(roi: Option[Vector[Int]]): Either[SpatialError, RowSelection] =
    roi match
      case None =>
        Right(RowSelection.All)
      case Some(indices) =>
        rows(indices)

  def rows(indices: Vector[Int]): Either[SpatialError, RowSelection] =
    validateRows(indices, Int.MaxValue).map(_ => RowSelection.Rows(indices))

  private[spatial] def unsafeFromRoi(roi: Option[Vector[Int]]): RowSelection =
    roi match
      case None => RowSelection.All
      case Some(indices) => RowSelection.Rows(indices)

  private[spatial] def validateRows(indices: Vector[Int], limit: Int): Either[SpatialError, Unit] =
    if indices.isEmpty then Left(SpatialError.EmptyRoi)
    else
      val seen = scala.collection.mutable.HashSet.empty[Int]
      var i = 0
      var error = Option.empty[SpatialError]
      while i < indices.length && error.isEmpty do
        val row = indices(i)
        if row < 0 || row >= limit then error = Some(SpatialError.InvalidRoiRow(row, limit))
        else if seen.contains(row) then error = Some(SpatialError.DuplicateRoiRow(row))
        else seen += row
        i += 1
      error match
        case Some(err) => Left(err)
        case None => Right(())

final case class TargetRows private (
  selection: RowSelection,
  indices: Vector[Int]
):
  require(indices.nonEmpty, "target row selection must be non-empty")

  def length: Int =
    indices.length

object TargetRows:
  def fromSelection(selection: RowSelection, targetRowCount: Int): Either[SpatialError, TargetRows] =
    if targetRowCount <= 0 then Left(SpatialError.NonPositiveDimension("target rows", targetRowCount))
    else
      selection match
        case RowSelection.All =>
          Right(new TargetRows(RowSelection.All, Vector.tabulate(targetRowCount)(identity)))
        case RowSelection.Rows(indices) =>
          RowSelection.validateRows(indices, targetRowCount).map(_ => new TargetRows(selection, indices))

  def all(targetRowCount: Int): Either[SpatialError, TargetRows] =
    fromSelection(RowSelection.All, targetRowCount)
