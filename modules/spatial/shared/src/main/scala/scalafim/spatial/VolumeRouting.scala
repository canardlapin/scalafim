package scalafim.spatial

import cats.Hash
import neuroim4s.spatial.{VolumeDomain, VolumeGraph, VolumeMapKind, VolumeReference, VolumeRegistration, VolumeRoute, VolumeRoutingError}

/** Mixed-domain integration seam. Anatomical volume policy and route search live in their providers. */
private[spatial] object VolumeRouting:
  private given Hash[DomainId] = Hash.fromUniversalHashCode

  def supports(graph: SpatialGraph, policy: RoutingPolicy): Boolean =
    policy != RoutingPolicy.Functional && graph.domains.values.forall(_.kind == DomainKind.Volume) &&
      graph.morphisms.forall(m => m.kind == MorphismKind.Identity || m.kind == MorphismKind.Affine3D || m.kind == MorphismKind.Warp3D)

  def path(graph: SpatialGraph, source: DomainId, target: DomainId, policy: RoutingPolicy, allowInverses: Boolean,
      penalty: InversePenalty): Either[SpatialError, MorphismPath] =
    for
      admitted <- admit(graph, policy)
      route <- admitted.path(source, target, allowInverses, penalty.value).left.map(error)
      path <- restore(graph, route, penalty)
    yield path

  def allPaths(graph: SpatialGraph, source: DomainId, target: DomainId, policy: RoutingPolicy, maxPaths: Int,
      allowInverses: Boolean, penalty: InversePenalty): Either[SpatialError, Vector[MorphismPath]] =
    for
      admitted <- admit(graph, policy)
      routes <- admitted.allPaths(source, target, maxPaths, allowInverses, penalty.value)(_.value).left.map(error)
      paths <- routes.foldLeft[Either[SpatialError, Vector[MorphismPath]]](Right(Vector.empty)):
        (acc, route) => acc.flatMap(paths => restore(graph, route, penalty).map(paths :+ _))
    yield paths

  private def admit(graph: SpatialGraph, policy: RoutingPolicy): Either[SpatialError, VolumeGraph[DomainId, MorphismId]] =
    val nodes = graph.domains.values.toVector.foldLeft[Either[SpatialError, Vector[VolumeDomain[DomainId]]]](Right(Vector.empty)):
      (acc, domain) => acc.flatMap(nodes => volume(domain).map(nodes :+ _))
    val edges = graph.morphisms.filter(m => m.kind != MorphismKind.Identity && (policy == RoutingPolicy.Shortest || m.routeTag != RouteTag.Functional)).foldLeft[Either[SpatialError, Vector[VolumeRegistration[DomainId, MorphismId]]]](Right(Vector.empty)):
      (acc, morphism) => acc.flatMap: edges =>
        val map = morphism.coordinateMap match
          case CoordinateMap.Geometric(binding) => Some(binding)
          case _ => None
        val inverse = if morphism.inverse.isGeometric then Some(morphism.inverse.quality) else None
        val kind = if morphism.kind == MorphismKind.Affine3D then VolumeMapKind.Affine3D else VolumeMapKind.Warp3D
        VolumeRegistration.build(morphism.id, morphism.source, morphism.target, kind, morphism.cost, inverse, map)
          .left.map(error).map(edges :+ _)
    for
      admittedNodes <- nodes
      admittedEdges <- edges
      result <- VolumeGraph.build(admittedNodes, admittedEdges).left.map(error)
    yield result

  private def volume(domain: Domain): Either[SpatialError, VolumeDomain[DomainId]] =
    val reference = domain.space match
      case SpaceRef.Volume(subject, session, _) => Right(VolumeReference.Native(subject, session))
      case SpaceRef.Template(name, _, TemplateKind.Volume) => Right(VolumeReference.Template(name))
      case _ => Left(SpatialError.NonVolumeDomain(domain.id))
    reference.flatMap: ref =>
      domain.geometry match
        case SamplingGeometry.Volume(grid) => Right(VolumeDomain.sampled(domain.id, ref, grid))
        case SamplingGeometry.Unsampled(DomainKind.Volume, frame) => Right(VolumeDomain.unsampled(domain.id, ref, frame))
        case _ => Left(SpatialError.NonVolumeDomain(domain.id))

  private def restore(graph: SpatialGraph, route: VolumeRoute[DomainId, MorphismId], penalty: InversePenalty): Either[SpatialError, MorphismPath] =
    if route.steps.isEmpty then Right(MorphismPath.identity(route.source))
    else
      route.steps.foldLeft[Either[SpatialError, Vector[Morphism]]](Right(Vector.empty)):
        (acc, step) => acc.flatMap: path =>
          val original = graph.morphisms.find(_.id == step.registration.id).get
          val directed = if step.inverted then original.reversed(penalty) else Right(original)
          directed.map(path :+ _)
      .flatMap(morphisms => MorphismPath.build(morphisms, route.usedInverses))

  private def error(cause: VolumeRoutingError[DomainId, MorphismId]): SpatialError = cause match
    case VolumeRoutingError.MissingDomain(id) => SpatialError.DomainNotFound(id)
    case VolumeRoutingError.DuplicateDomain(id) => SpatialError.DuplicateDomain(id)
    case VolumeRoutingError.DuplicateRegistration(id) => SpatialError.DuplicateMorphism(id)
    case VolumeRoutingError.InvalidCost(value) => SpatialError.InvalidCost(value)
    case VolumeRoutingError.InvalidQuality(value) => SpatialError.InvalidQuality("provided inverse", value)
    case VolumeRoutingError.InvalidPenalty(value) => SpatialError.InvalidInversePenalty(value)
    case VolumeRoutingError.InvalidLimit(value) => SpatialError.InvalidPathLimit(value)
    case VolumeRoutingError.NoPath(source, target) => SpatialError.NoPath(source, target)
    case VolumeRoutingError.MissingMap(id) => SpatialError.MissingCoordinateMap(id)
    case VolumeRoutingError.MapFailure(cause) => CoordinateMap.mapBindingError(cause)
    case VolumeRoutingError.FrameMismatch(cause) => SpatialError.ProviderMap(cause)
    case VolumeRoutingError.Topology(reason) => SpatialError.OperatorAssemblyFailed(reason)
