package scalafim.locus

enum SearchlightError:
  case WrongSpace(error: SpaceMismatch)
  case NonEmptyOutsideCenters(pointOrdinal: Int)

  def message: String =
    this match
      case WrongSpace(error) => error.message
      case NonEmptyOutsideCenters(point) =>
        s"searchlight relation row $point is non-empty outside the center region"

enum CenteredSearchlightError:
  case MissingCenter(pointOrdinal: Int)

  def message: String =
    this match
      case MissingCenter(point) =>
        s"searchlight neighborhood at $point does not contain its center"

final class Searchlight[S] private (
    val centers: Region[S],
    val neighborhoods: Relation[S, S]
):
  def regionAt(center: Point[S]): Option[Region[S]] =
    Option.when(centers.contains(center))(neighborhoods.row(center))

object Searchlight:
  def make[S](
      centers: Region[S],
      neighborhoods: Relation[S, S]
  ): Either[SearchlightError, Searchlight[S]] =
    if !centers.space.sameIdentityAs(neighborhoods.from) then
      Left(SearchlightError.WrongSpace(mismatch(centers.space, neighborhoods.from)))
    else if !centers.space.sameIdentityAs(neighborhoods.to) then
      Left(SearchlightError.WrongSpace(mismatch(centers.space, neighborhoods.to)))
    else
      var ordinal = 0
      var invalid = -1
      while ordinal < centers.space.size && invalid < 0 do
        val point = centers.space.indexOption(ordinal).get
        if !centers.contains(point) && !neighborhoods.row(point).isEmpty then
          invalid = ordinal
        ordinal += 1
      if invalid >= 0 then Left(SearchlightError.NonEmptyOutsideCenters(invalid))
      else Right(new Searchlight(centers, neighborhoods))

final class CenteredSearchlight[S] private (
    val searchlight: Searchlight[S]
)

object CenteredSearchlight:
  def validate[S](
      searchlight: Searchlight[S]
  ): Either[CenteredSearchlightError, CenteredSearchlight[S]] =
    val centers = searchlight.centers.indicesInDomainOrder
    var missing = -1
    while centers.hasNext && missing < 0 do
      val center = centers.next()
      if !searchlight.neighborhoods.isRelated(center, center) then
        missing = center.ordinal
    if missing >= 0 then Left(CenteredSearchlightError.MissingCenter(missing))
    else Right(new CenteredSearchlight(searchlight))
