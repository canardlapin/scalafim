package scalafim.spatial

import scala.collection.mutable

/** Weight of an inverse edge's quality deficit in routing: an inverse of `morphism` costs
  * `morphism.cost + penalty * (1 - inverse quality)`, as in neurofunctor's `find_path_with_inverses`.
  */
opaque type InversePenalty = Double

object InversePenalty:
  /** neurofunctor's default `penalty_factor`. */
  val default: InversePenalty = 1.0

  def apply(value: Double): Either[SpatialError, InversePenalty] =
    if value.isFinite && value >= 0.0 then Right(value)
    else Left(SpatialError.InvalidInversePenalty(value))

  extension (penalty: InversePenalty)
    inline def value: Double = penalty

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

  /** The cheapest route from `source` to `target`.
    *
    * Routing is forward-first, as in neurofunctor: forward edges admitted by `policy` are searched alone, and only
    * when they cannot reach `target` (and `allowInverses` holds) are geometric inverses added, each costing
    * `cost + inversePenalty * (1 - quality)`. A forward route is therefore never displaced by a cheaper inverse one.
    * `AdjointOnly` and `None` inverses never become routing edges.
    */
  def path(
    source: DomainId,
    target: DomainId,
    policy: RoutingPolicy = RoutingPolicy.Shortest,
    allowInverses: Boolean = false,
    inversePenalty: InversePenalty = InversePenalty.default
  ): Either[SpatialError, MorphismPath] =
    requireEndpoints(source, target).flatMap { _ =>
      if source == target then Right(MorphismPath.identity(source))
      else
        val forward = forwardEdges(policy)
        shortest(source, target, forward) match
          case Left(SpatialError.NoPath(_, _)) if allowInverses =>
            shortest(source, target, forward ++ inverseEdges(policy, inversePenalty))
          case other =>
            other
    }

  /** The route [[path]] would choose, as an inspectable step table (neurofunctor `inspect_path`). */
  def inspectPath(
    source: DomainId,
    target: DomainId,
    policy: RoutingPolicy = RoutingPolicy.Shortest,
    allowInverses: Boolean = false,
    inversePenalty: InversePenalty = InversePenalty.default
  ): Either[SpatialError, PathInspection] =
    path(source, target, policy, allowInverses, inversePenalty).map(PathInspection.of)

  /** Every simple route (no domain visited twice) from `source` to `target`, cheapest first, at most `maxPaths`.
    *
    * Parallel morphisms between the same domains are distinct routes. With `allowInverses`, geometric inverse edges
    * (priced as in [[path]]) are enumerated alongside forward ones rather than as a fallback, since the point is to
    * list alternatives, e.g. for a commutativity check. Ties are broken by length, then by morphism ids.
    */
  def allPaths(
    source: DomainId,
    target: DomainId,
    policy: RoutingPolicy = RoutingPolicy.Shortest,
    maxPaths: Int = 10,
    allowInverses: Boolean = false,
    inversePenalty: InversePenalty = InversePenalty.default
  ): Either[SpatialError, Vector[MorphismPath]] =
    if maxPaths <= 0 then Left(SpatialError.InvalidPathLimit(maxPaths))
    else
      requireEndpoints(source, target).flatMap { _ =>
        if source == target then Right(Vector(MorphismPath.identity(source)))
        else
          val edges =
            if allowInverses then forwardEdges(policy) ++ inverseEdges(policy, inversePenalty)
            else forwardEdges(policy)
          val adjacency = adjacencyOf(edges)
          val found = Vector.newBuilder[Vector[RouteEdge]]
          val visited = mutable.HashSet(source)

          def walk(node: DomainId, trail: List[RouteEdge]): Unit =
            adjacency.getOrElse(node, Vector.empty).foreach { edge =>
              if edge.to == target then found += (edge :: trail).reverse.toVector
              else if !visited(edge.to) then
                visited += edge.to
                walk(edge.to, edge :: trail)
                visited -= edge.to
            }

          walk(source, Nil)
          val ranked =
            found.result().sortBy { route =>
              (route.map(_.morphism.cost).sum, route.length, route.map(_.morphism.id.value).mkString("|"))
            }
          ranked.take(maxPaths).foldLeft[Either[SpatialError, Vector[MorphismPath]]](Right(Vector.empty)) {
            (acc, route) =>
              acc.flatMap(paths => MorphismPath.build(route.map(_.morphism), route.exists(_.inverted)).map(paths :+ _))
          }
      }

  private def requireEndpoints(source: DomainId, target: DomainId): Either[SpatialError, Unit] =
    if !domains.contains(source) then Left(SpatialError.DomainNotFound(source))
    else if !domains.contains(target) then Left(SpatialError.DomainNotFound(target))
    else Right(())

  private def shortest(source: DomainId, target: DomainId, edges: Vector[RouteEdge]): Either[SpatialError, MorphismPath] =
    val adjacency = adjacencyOf(edges)

    given Ordering[(Double, DomainId)] =
      Ordering.by[(Double, DomainId), Double](_._1).reverse

    val queue = mutable.PriorityQueue.empty[(Double, DomainId)]
    val distance = mutable.HashMap.empty[DomainId, Double]
    val previous = mutable.HashMap.empty[DomainId, (DomainId, RouteEdge)]
    val visited = mutable.HashSet.empty[DomainId]

    distance(source) = 0.0
    queue.enqueue((0.0, source))

    while queue.nonEmpty do
      val (cost, node) = queue.dequeue()
      if !visited(node) then
        visited += node
        if node == target then
          val pathEdges = Vector.newBuilder[RouteEdge]
          var current = target
          while current != source do
            val (prior, edge) = previous(current)
            pathEdges += edge
            current = prior
          val route = pathEdges.result().reverse
          return MorphismPath.build(route.map(_.morphism), route.exists(_.inverted))

        adjacency.getOrElse(node, Vector.empty).foreach { edge =>
          if !visited(edge.to) then
            val candidate = cost + edge.morphism.cost
            if candidate < distance.getOrElse(edge.to, Double.PositiveInfinity) then
              distance(edge.to) = candidate
              previous(edge.to) = (node, edge)
              queue.enqueue((candidate, edge.to))
        }

    Left(SpatialError.NoPath(source, target))

  private def adjacencyOf(edges: Vector[RouteEdge]): Map[DomainId, Vector[RouteEdge]] =
    edges.groupBy(_.from)

  private def forwardEdges(policy: RoutingPolicy): Vector[RouteEdge] =
    morphisms.filter(morphismAllowed(_, policy)).map { morphism =>
      RouteEdge(morphism.source, morphism.target, morphism, inverted = false)
    }

  private def inverseEdges(policy: RoutingPolicy, penalty: InversePenalty): Vector[RouteEdge] =
    morphisms.flatMap { morphism =>
      if morphismAllowed(morphism, policy) && morphism.inverse.isGeometric then
        morphism.reversed(penalty).toOption.flatMap { rev =>
          for
            source <- domains.get(rev.source)
            target <- domains.get(rev.target)
            if Morphism.validateDomains(rev, source, target).isRight
          yield RouteEdge(rev.source, rev.target, rev, inverted = true)
        }
      else None
    }

  private def morphismAllowed(morphism: Morphism, policy: RoutingPolicy): Boolean =
    policy match
      case RoutingPolicy.Shortest =>
        true
      case RoutingPolicy.Anatomical =>
        morphism.routeTag == RouteTag.Identity || morphism.routeTag == RouteTag.Anatomical
      case RoutingPolicy.Functional =>
        morphism.routeTag == RouteTag.Identity || morphism.routeTag == RouteTag.Functional

private final case class RouteEdge(from: DomainId, to: DomainId, morphism: Morphism, inverted: Boolean)

/** One step of an inspected route. `cost` is the routed cost, so an inverse step includes its quality penalty. */
final case class PathStep(
  index: Int,
  morphism: MorphismId,
  kind: MorphismKind,
  source: DomainId,
  target: DomainId,
  cost: Double,
  inverted: Boolean,
  inverseQuality: Double
)

/** A route as data: its steps, total cost, and whether any step runs a morphism backwards. */
final case class PathInspection(
  source: DomainId,
  target: DomainId,
  steps: Vector[PathStep],
  totalCost: Double,
  usedInverses: Boolean,
  pathQuality: Double
):
  def render: String =
    val header = s"Path from ${source.value} to ${target.value}"
    val body =
      steps.map { step =>
        val direction = if step.inverted then " (inverse)" else ""
        f"Step ${step.index + 1}: ${step.kind}$direction ${step.morphism.value}%n" +
          f"  Source: ${step.source.value}%n" +
          f"  Target: ${step.target.value}%n" +
          f"  Cost: ${step.cost}%.2f"
      }
    val footer =
      f"Total cost: $totalCost%.2f%nTotal steps: ${steps.length}%nUsed inverses: $usedInverses"
    (header +: body :+ footer).mkString(System.lineSeparator())

object PathInspection:
  def of(path: MorphismPath): PathInspection =
    PathInspection(
      source = path.source,
      target = path.target,
      steps = path.morphisms.zipWithIndex.map { (morphism, index) =>
        PathStep(
          index,
          morphism.id,
          morphism.kind,
          morphism.source,
          morphism.target,
          morphism.cost,
          morphism.isInverted,
          morphism.inverse.quality
        )
      },
      totalCost = path.cost,
      usedInverses = path.usedInverses,
      pathQuality = path.pathQuality
    )

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
