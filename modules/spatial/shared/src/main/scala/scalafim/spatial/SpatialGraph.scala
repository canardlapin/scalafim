package scalafim.spatial

import scala.collection.mutable

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
    else Right(copy(morphisms = morphisms :+ morphism))

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

    val edges = routeEdges(policy, allowInverses)
    val adjacency = mutable.HashMap.empty[DomainId, Vector[RouteEdge]]
    edges.foreach { edge =>
      adjacency.update(edge.from, adjacency.getOrElse(edge.from, Vector.empty) :+ edge)
    }

    given Ordering[(Double, DomainId)] =
      Ordering.by[(Double, DomainId), Double](_._1).reverse

    val queue = mutable.PriorityQueue.empty[(Double, DomainId)]
    val distance = mutable.HashMap.empty[DomainId, Double]
    val previous = mutable.HashMap.empty[DomainId, (DomainId, Morphism, Boolean)]
    val visited = mutable.HashSet.empty[DomainId]

    distance(source) = 0.0
    queue.enqueue((0.0, source))

    while queue.nonEmpty do
      val (cost, node) = queue.dequeue()
      if !visited(node) then
        visited += node
        if node == target then
          val pathMorphisms = Vector.newBuilder[Morphism]
          var current = target
          var usedInverses = false
          while current != source do
            val (prior, morphism, inverted) = previous(current)
            pathMorphisms += morphism
            usedInverses = usedInverses || inverted
            current = prior
          return MorphismPath.build(pathMorphisms.result().reverse, usedInverses)

        adjacency.getOrElse(node, Vector.empty).foreach { edge =>
          if !visited(edge.to) then
            val candidate = cost + edge.morphism.cost
            if candidate < distance.getOrElse(edge.to, Double.PositiveInfinity) then
              distance(edge.to) = candidate
              previous(edge.to) = (node, edge.morphism, edge.inverted)
              queue.enqueue((candidate, edge.to))
        }

    Left(SpatialError.NoPath(source, target))

  private def routeEdges(policy: RoutingPolicy, allowInverses: Boolean): Vector[RouteEdge] =
    val forward =
      morphisms.filter(morphismAllowed(_, policy)).map { morphism =>
        RouteEdge(morphism.source, morphism.target, morphism, inverted = false)
      }
    if !allowInverses then forward
    else
      val inverse =
        morphisms.flatMap { morphism =>
          if morphismAllowed(morphism, policy) && morphism.inverse.isGeometric then
            morphism.reversed.toOption.map(rev => RouteEdge(rev.source, rev.target, rev, inverted = true))
          else None
        }
      forward ++ inverse

  private def morphismAllowed(morphism: Morphism, policy: RoutingPolicy): Boolean =
    policy match
      case RoutingPolicy.Shortest =>
        true
      case RoutingPolicy.Anatomical =>
        morphism.routeTag == RouteTag.Identity || morphism.routeTag == RouteTag.Anatomical
      case RoutingPolicy.Functional =>
        morphism.routeTag == RouteTag.Identity || morphism.routeTag == RouteTag.Functional

private final case class RouteEdge(from: DomainId, to: DomainId, morphism: Morphism, inverted: Boolean)

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
