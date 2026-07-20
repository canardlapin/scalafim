package scalafim.spatial

import scala.collection.mutable

import scalafim.graph.DirectedGraph
import scalafim.graph.EdgeCost
import scalafim.graph.Graph
import scalafim.graph.PathError
import scalafim.graph.VertexBasis
import scalafim.graph.shortestPath

final case class SpatialGraph private (
  domains: Map[DomainId, Domain],
  morphisms: Vector[Morphism]
):
  def add(domain: Domain): Either[SpatialError, SpatialGraph] =
    if domains.contains(domain.id) then Left(SpatialError.DuplicateDomain(domain.id))
    else Right(copy(domains = domains.updated(domain.id, domain)))

  def add(morphism: Morphism): Either[SpatialError, SpatialGraph] =
    if morphisms.exists(_.id == morphism.id) then Left(SpatialError.DuplicateMorphism(morphism.id))
    else if !domains.contains(morphism.source) then Left(SpatialError.MorphismDomainMissing(morphism.id, morphism.source))
    else if !domains.contains(morphism.target) then Left(SpatialError.MorphismDomainMissing(morphism.id, morphism.target))
    else
      val source = domains(morphism.source)
      val target = domains(morphism.target)
      Morphism.validateDomains(morphism, source, target).map(_ => copy(morphisms = morphisms :+ morphism))

  def domain(id: DomainId): Either[SpatialError, Domain] =
    domains.get(id).toRight(SpatialError.DomainNotFound(id))

  def path(
    source: DomainId,
    target: DomainId,
    policy: RoutingPolicy = RoutingPolicy.Shortest,
    allowInverses: Boolean = false
  ): Either[SpatialError, MorphismPath] =
    if !domains.contains(source) then return Left(SpatialError.DomainNotFound(source))
    if !domains.contains(target) then return Left(SpatialError.DomainNotFound(target))
    if source == target then return Right(MorphismPath.identity(source))

    given EdgeCost[Morphism] with
      def cost(edge: Morphism): Double = edge.cost

    routeGraph(policy, allowInverses).flatMap: graph =>
      graph.shortestPath(source, target) match
        case Right(result) =>
          val pathMorphisms = result.edges.map(_.value)
          MorphismPath.build(pathMorphisms, pathMorphisms.exists(_.isInverted))
        case Left(PathError.UnknownVertex(id)) => Left(SpatialError.DomainNotFound(id))
        case Left(PathError.NoPath(_, _))       => Left(SpatialError.NoPath(source, target))
        case Left(PathError.InvalidEdgeCost(_, _, value)) => Left(SpatialError.InvalidCost(value))

  private def routeGraph(
    policy: RoutingPolicy,
    allowInverses: Boolean
  ): Either[SpatialError, DirectedGraph[DomainId, Domain, Morphism]] =
    val basis =
      VertexBasis
        .from(domains.values.toVector.sortBy(_.id.value).map(domain => domain.id -> domain))
        .left
        .map(error => SpatialError.GraphAssemblyFailed(error.message))
    val routeMorphisms = collapseParallel(routeEdges(policy, allowInverses).filter(morphism => morphism.source != morphism.target))
    basis.flatMap: value =>
      Graph
        .directed(value, routeMorphisms.map(morphism => (morphism.source, morphism.target, morphism)))
        .left
        .map(errors => SpatialError.GraphAssemblyFailed(errors.message))

  private def routeEdges(policy: RoutingPolicy, allowInverses: Boolean): Vector[Morphism] =
    val forward =
      morphisms.filter(morphismAllowed(_, policy))
    if !allowInverses then forward
    else
      val inverse =
        morphisms.flatMap { morphism =>
          if morphismAllowed(morphism, policy) && morphism.inverse.isGeometric then
            morphism.reversed.toOption.flatMap { rev =>
              for
                source <- domains.get(rev.source)
                target <- domains.get(rev.target)
                if Morphism.validateDomains(rev, source, target).isRight
              yield rev
            }
          else None
        }
      forward ++ inverse

  private def collapseParallel(edges: Vector[Morphism]): Vector[Morphism] =
    edges
      .zipWithIndex
      .groupBy { case (morphism, _) => morphism.source -> morphism.target }
      .values
      .map(_.minBy { case (morphism, inputOrder) => (morphism.cost, inputOrder) }._1)
      .toVector

  private def morphismAllowed(morphism: Morphism, policy: RoutingPolicy): Boolean =
    policy match
      case RoutingPolicy.Shortest =>
        true
      case RoutingPolicy.Anatomical =>
        morphism.routeTag == RouteTag.Identity || morphism.routeTag == RouteTag.Anatomical
      case RoutingPolicy.Functional =>
        morphism.routeTag == RouteTag.Identity || morphism.routeTag == RouteTag.Functional

object SpatialGraph:
  val empty: SpatialGraph =
    SpatialGraph(Map.empty, Vector.empty)

  def build(domains: Vector[Domain], morphisms: Vector[Morphism] = Vector.empty): Either[SpatialError, SpatialGraph] =
    val domainMap = mutable.LinkedHashMap.empty[DomainId, Domain]
    var duplicateDomain: Option[DomainId] = None
    var i = 0
    while i < domains.length && duplicateDomain.isEmpty do
      val domain = domains(i)
      if domainMap.contains(domain.id) then duplicateDomain = Some(domain.id)
      else domainMap(domain.id) = domain
      i += 1

    duplicateDomain match
      case Some(id) =>
        Left(SpatialError.DuplicateDomain(id))
      case None =>
        val graph = SpatialGraph(domainMap.toMap, Vector.empty)
        morphisms.foldLeft[Either[SpatialError, SpatialGraph]](Right(graph)) { (acc, morphism) =>
          acc.flatMap(_.add(morphism))
        }
