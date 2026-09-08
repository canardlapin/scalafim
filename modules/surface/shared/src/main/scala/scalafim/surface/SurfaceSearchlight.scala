package scalafim.surface

import locus4s.{
  CenteredNeighborhoodSystem,
  Index,
  NeighborhoodSystemError,
  Relation,
  RelationError,
  Selection,
  SpaceMismatch
}
import locus4s.data.{Field, Section}
import scalafim.locus.mismatch

enum SurfaceSearchlightError:
  case InvalidRadius(value: Double)
  case TopologyMismatch(expected: String)
  case WrongSpace(error: SpaceMismatch)
  case InvalidMembership(error: RelationError)
  case InvalidNeighborhood(error: NeighborhoodSystemError)

  def message: String =
    this match
      case InvalidRadius(value) =>
        s"surface searchlight radius must be finite and non-negative, found $value"
      case TopologyMismatch(expected) =>
        s"surface searchlight topology does not match $expected"
      case WrongSpace(error) => error.message
      case InvalidMembership(error) => error.message
      case InvalidNeighborhood(error) => error.message

object SurfaceSearchlight:
  def metricBalls[S](
      domain: SurfaceLocusDomain[S],
      topology: MeshTopology,
      radius: Double,
      centers: Selection[S],
      metric: DistanceMetric = DistanceMetric.Geodesic,
      edgeWeights: Option[Seq[Double]] = None
  ): Either[
    SurfaceSearchlightError,
    CenteredNeighborhoodSystem[centers.I, S]
  ] =
    if !radius.isFinite || radius < 0.0 then
      Left(SurfaceSearchlightError.InvalidRadius(radius))
    else if !domain.geometry.mesh.hasSameTopology(topology.mesh) then
      Left(SurfaceSearchlightError.TopologyMismatch(domain.meshDomain.display))
    else if !domain.finiteSpace.sameRuntimeOwnerAs(centers.space) then
      Left:
        SurfaceSearchlightError.WrongSpace:
          mismatch(domain.finiteSpace, centers.space)
    else
      val rows = metricBallRows(domain, topology, radius, centers.ordinals, metric, edgeWeights)
      Relation
        .fromOrdinalRows(
          centers.positions,
          domain.finiteSpace,
          rows.iterator.map(_.iterator)
        )
        .left
        .map(SurfaceSearchlightError.InvalidMembership.apply)
        .flatMap: membership =>
          CenteredNeighborhoodSystem
            .fromSelection(centers, membership)
            .left
            .map(SurfaceSearchlightError.InvalidNeighborhood.apply)

  def metricBalls[S](
      domain: SurfaceLocusDomain[S],
      topology: MeshTopology,
      radius: Double,
      metric: DistanceMetric
  ): Either[SurfaceSearchlightError, CenteredNeighborhoodSystem[S, S]] =
    metricBalls(domain, topology, radius, metric, None)

  def metricBalls[S](
      domain: SurfaceLocusDomain[S],
      topology: MeshTopology,
      radius: Double
  ): Either[SurfaceSearchlightError, CenteredNeighborhoodSystem[S, S]] =
    metricBalls(domain, topology, radius, DistanceMetric.Geodesic)

  /** The field restricted to the neighborhood at a typed center. */
  def sectionAt[C, S, A](
      searchlight: CenteredNeighborhoodSystem[C, S],
      center: Index[C],
      field: Field[S, A]
  ): Section[S, A] =
    field.restrict(searchlight.neighborhood(center))

  private def metricBalls[S](
      domain: SurfaceLocusDomain[S],
      topology: MeshTopology,
      radius: Double,
      metric: DistanceMetric,
      edgeWeights: Option[Seq[Double]]
  ): Either[SurfaceSearchlightError, CenteredNeighborhoodSystem[S, S]] =
    if !radius.isFinite || radius < 0.0 then
      Left(SurfaceSearchlightError.InvalidRadius(radius))
    else if !domain.geometry.mesh.hasSameTopology(topology.mesh) then
      Left(SurfaceSearchlightError.TopologyMismatch(domain.meshDomain.display))
    else
      val centerOrdinals = Array.tabulate(domain.finiteSpace.size)(identity)
      val rows = metricBallRows(domain, topology, radius, centerOrdinals, metric, edgeWeights)
      Relation
        .fromOrdinalRows(
          domain.finiteSpace,
          domain.finiteSpace,
          rows.iterator.map(_.iterator)
        )
        .left
        .map(SurfaceSearchlightError.InvalidMembership.apply)
        .flatMap: membership =>
          CenteredNeighborhoodSystem
            .fromIdentityCenters(membership)
            .left
            .map(SurfaceSearchlightError.InvalidNeighborhood.apply)

  private def metricBallRows[S](
      domain: SurfaceLocusDomain[S],
      topology: MeshTopology,
      radius: Double,
      centerOrdinals: Array[Int],
      metric: DistanceMetric,
      edgeWeights: Option[Seq[Double]]
  ): Array[Array[Int]] =
    val rows = Array.ofDim[Array[Int]](centerOrdinals.length)
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
        var column = 0
        while column < distances.cols do
          if distances(row, column) <= radius then
            members += distances.colVertices(column).index
          column += 1
        rows(row) = members.result()
        row += 1
    rows
