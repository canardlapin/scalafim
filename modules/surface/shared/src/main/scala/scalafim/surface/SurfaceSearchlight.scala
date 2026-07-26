package scalafim.surface

import scalafim.locus.{
  CenteredSearchlight,
  CenteredSearchlightError,
  IndexedField,
  Point,
  Region,
  Relation,
  Searchlight,
  SearchlightError,
  Section,
  SpaceMismatch
}

enum SurfaceSearchlightError:
  case InvalidRadius(value: Double)
  case TopologyMismatch(expected: String)
  case WrongSpace(error: SpaceMismatch)
  case InvalidSearchlight(error: SearchlightError)
  case NotCentered(error: CenteredSearchlightError)

  def message: String =
    this match
      case InvalidRadius(value) =>
        s"surface searchlight radius must be finite and non-negative, found $value"
      case TopologyMismatch(expected) =>
        s"surface searchlight topology does not match $expected"
      case WrongSpace(error) => error.message
      case InvalidSearchlight(error) => error.message
      case NotCentered(error) => error.message

object SurfaceSearchlight:
  def metricBalls[S](
      domain: SurfaceLocusDomain[S],
      topology: MeshTopology,
      radius: Double,
      centers: Region[S],
      metric: DistanceMetric = DistanceMetric.Geodesic,
      edgeWeights: Option[Seq[Double]] = None
  ): Either[SurfaceSearchlightError, CenteredSearchlight[S]] =
    if !radius.isFinite || radius < 0.0 then
      Left(SurfaceSearchlightError.InvalidRadius(radius))
    else if !domain.geometry.mesh.hasSameTopology(topology.mesh) then
      Left(SurfaceSearchlightError.TopologyMismatch(domain.meshDomain.display))
    else if !domain.finiteSpace.sameIdentityAs(centers.space) then
      Left:
        SurfaceSearchlightError.WrongSpace:
          SpaceMismatch(
            domain.finiteSpace.key,
            domain.finiteSpace.size,
            centers.space.key,
            centers.space.size
          )
    else
      val rows = Array.fill(domain.finiteSpace.size)(Array.emptyIntArray)
      val centerOrdinals = centers.ordinalsInDomainOrder
      if centerOrdinals.nonEmpty then
        val sourceVertices = centerOrdinals.toVector.map(VertexId.unsafe)
        val targets = Vector.tabulate(domain.finiteSpace.size)(VertexId.unsafe)
        val distances =
          SurfaceGeodesics.distanceMatrix(
            topology,
            sourceVertices,
            targets,
            metric,
            edgeWeights
          )
        var row = 0
        while row < distances.rows do
          val members = Array.newBuilder[Int]
          var col = 0
          while col < distances.cols do
            if distances(row, col) <= radius then
              members += distances.colVertices(col).index
            col += 1
          rows(centerOrdinals(row)) = members.result()
          row += 1

      val relation =
        Relation
          .fromOrdinalRows(domain.finiteSpace, domain.finiteSpace, rows)
          .toOption
          .get
      Searchlight
        .make(centers, relation)
        .left
        .map(SurfaceSearchlightError.InvalidSearchlight.apply)
        .flatMap: searchlight =>
          CenteredSearchlight
            .validate(searchlight)
            .left
            .map(SurfaceSearchlightError.NotCentered.apply)

  def metricBalls[S](
      domain: SurfaceLocusDomain[S],
      topology: MeshTopology,
      radius: Double,
      metric: DistanceMetric
  ): Either[SurfaceSearchlightError, CenteredSearchlight[S]] =
    metricBalls(
      domain,
      topology,
      radius,
      Region.whole(domain.finiteSpace),
      metric
    )

  def metricBalls[S](
      domain: SurfaceLocusDomain[S],
      topology: MeshTopology,
      radius: Double
  ): Either[SurfaceSearchlightError, CenteredSearchlight[S]] =
    metricBalls(domain, topology, radius, DistanceMetric.Geodesic)

  def sectionAt[S, A](
      searchlight: Searchlight[S],
      center: Point[S],
      field: IndexedField[S, A]
  ): Either[SpaceMismatch, Option[Section[S, A]]] =
    searchlight.regionAt(center) match
      case Some(region) => field.restrict(region).map(Some(_))
      case None => Right(None)
