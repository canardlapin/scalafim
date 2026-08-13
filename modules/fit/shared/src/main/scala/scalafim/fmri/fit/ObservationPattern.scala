package scalafim.fmri.fit

opaque type ObservationPatternId = Int

object ObservationPatternId:
  def apply(value: Int): Either[FitError, ObservationPatternId] =
    if value >= 0 then Right(value)
    else Left(FitError.InvalidFitAxis("observation pattern id", s"value $value must be non-negative"))

  def unsafe(value: Int): ObservationPatternId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ObservationPatternId)
    inline def value: Int = id

/** Exact response geometry shared by one or more selected voxels.
  *
  * `rowPositions` index the outer selected response, while `timepoints` retain
  * the corresponding source acquisition indices. Keeping both axes explicit
  * prevents compacted missing rows from being mistaken for adjacent scans.
  */
final case class ObservationPattern private (
    id: ObservationPatternId,
    rowPositions: Vector[SelectedRowIndex],
    timepoints: SelectedTimepointIndices,
    voxelIndices: SelectedVoxelIndices
):
  def rows: Vector[Int] = rowPositions.map(_.value)
  def sourceTimepoints: Vector[Int] = timepoints.toVector
  def sourceVoxels: Vector[Int] = voxelIndices.toVector

object ObservationPattern:
  def make(
      id: Int,
      rowPositions: Vector[Int],
      timepoints: Vector[Int],
      voxelIndices: Vector[Int]
  ): Either[FitError, ObservationPattern] =
    if rowPositions.isEmpty then
      Left(FitError.InvalidFitAxis("observation pattern rows", "must be non-empty"))
    else if rowPositions.length != timepoints.length then
      Left(FitError.InvalidFitAxis(
        "observation pattern rows",
        s"${rowPositions.length} selected rows do not match ${timepoints.length} source timepoints"
      ))
    else if !strictlyIncreasing(rowPositions) then
      Left(FitError.InvalidFitAxis("observation pattern rows", "must be strictly increasing"))
    else if timepoints.distinct.length != timepoints.length then
      Left(FitError.InvalidFitAxis("observation pattern timepoints", "must be unique"))
    else
      for
        patternId <- ObservationPatternId(id)
        rows <- traverseRows(rowPositions)
        selectedTimepoints <- SelectedTimepointIndices.fromInts(timepoints)
        selectedVoxels <- SelectedVoxelIndices.fromInts(voxelIndices)
      yield new ObservationPattern(patternId, rows, selectedTimepoints, selectedVoxels)

  private[fit] def unsafe(
      id: Int,
      rowPositions: Vector[Int],
      timepoints: Vector[Int],
      voxelIndices: Vector[Int]
  ): ObservationPattern =
    make(id, rowPositions, timepoints, voxelIndices)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def traverseRows(values: Vector[Int]): Either[FitError, Vector[SelectedRowIndex]] =
    val out = Vector.newBuilder[SelectedRowIndex]
    var index = 0
    while index < values.length do
      SelectedRowIndex(values(index)) match
        case Left(error) => return Left(error)
        case Right(row)  => out += row
      index += 1
    Right(out.result())

  private def strictlyIncreasing(values: Vector[Int]): Boolean =
    var index = 1
    while index < values.length do
      if values(index) <= values(index - 1) then return false
      index += 1
    true
