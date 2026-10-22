package scalafim.atlas

import neuroatlas4s.*

import image4s.geometry.{Affine as ProviderAffine, D3}
import neuroim4s.spaces.TemplateName
import neurosurf4s.routing.{
  CoordinateMap,
  Domain,
  DomainId,
  DomainKind,
  Inverse,
  Morphism,
  MorphismId,
  MorphismKind,
  MorphismPath,
  RouteTag,
  RoutingPolicy,
  SamplingGeometry,
  SpaceRef,
  SpatialError,
  SpatialGraph,
  TemplateKind
}
import neuroim4s.transform.{PushAvailability, WorldTransform}

/** Mixed volume/surface domains over the shared template frame catalog. */
object TemplateDomains:
  /** The routing-graph domain of a catalogued space: unsampled, in the space's world frame. */
  def domain(catalog: TemplateCatalog, space: AnySpaceId): Either[AtlasError, Domain] =
    val normalized = SpaceId.normalize(space)
    for
      world <- catalog.frame(normalized)
      kind = TemplateDomains.domainKind(normalized)
      geometry <- SamplingGeometry.unsampled(kind, world).left.map(error => AtlasError.Provider(TransformGraphFailure(error)))
      templateKind = if kind == DomainKind.Surface then TemplateKind.Surface else TemplateKind.Volume
      name <- TemplateName(normalized.value).left.map(_ => AtlasError.UnknownSpace(normalized))
      domain <- Domain
        .build(
          TemplateDomains.domainId(normalized),
          SpaceRef.Template(name, None, templateKind),
          geometry
        )
        .left
        .map(error => AtlasError.Provider(TransformGraphFailure(error)))
    yield domain

  def domainId(space: AnySpaceId): DomainId =
    DomainId(s"atlas-template:${SpaceId.normalize(space).value}")
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Surface templates are surface domains; volume and unknown spaces are volumetric world spaces. */
  def domainKind(space: AnySpaceId): DomainKind =
    SpaceId.kind(space) match
      case SpaceKindTag.Surface => DomainKind.Surface
      case SpaceKindTag.Volume | SpaceKindTag.Unknown => DomainKind.Volume

/** A transform manifest as a [[SpatialGraph]]: one unsampled domain per catalogued space, one morphism per step.
  *
  * Routing is `SpatialGraph.path` with inverse fallback, so the route is the cheapest forward one and inverses are used
  * only where no forward route exists. Edge cost ranks availability before confidence, as the manifest always has:
  * `1 + 10 * confidence rank + 100 * (planned ? 1 : 0)`.
  */
final case class SpaceTransformGraph private (
  catalog: TemplateCatalog,
  graph: SpatialGraph,
  private val stepsById: Map[MorphismId, TransformStep]
):
  def plan(from: AnySpaceId, to: AnySpaceId, dataKind: DataKind = DataKind.Parcel): Either[AtlasError, TransformPlan] =
    val fromNorm = SpaceId.normalize(from)
    val toNorm = SpaceId.normalize(to)
    if fromNorm == toNorm then
      val step =
        TransformStep(
          fromNorm,
          toNorm,
          TransformKind.Identity,
          TransformBackend.Identity,
          Confidence.Exact,
          reversible = true,
          dataFiles = Vector.empty,
          TransformStatus.Available,
          notes = Some("No transform required."),
          affine = Some(ProviderAffine.identity[D3])
        )
      val world = catalog.world(fromNorm)
      Right(TransformPlan.build(fromNorm, toNorm, Vector(step), MorphismPath.identity(TemplateDomains.domainId(fromNorm)), dataKind, world, world))
    else if !catalog.contains(fromNorm) || !catalog.contains(toNorm) then Left(AtlasError.NoTransformRoute(fromNorm, toNorm))
    else
      graph
        .path(TemplateDomains.domainId(fromNorm), TemplateDomains.domainId(toNorm), RoutingPolicy.Shortest, allowInverses = true)
        .left
        .map {
          case SpatialError.NoPath(_, _) => AtlasError.NoTransformRoute(fromNorm, toNorm)
          case other => AtlasError.Provider(TransformGraphFailure(other))
        }
        .flatMap { path =>
          path.morphisms
            .foldLeft[Either[AtlasError, Vector[TransformStep]]](Right(Vector.empty)) { (acc, morphism) =>
              acc.flatMap(steps => describe(morphism).map(steps :+ _))
            }
            .map(steps => TransformPlan.build(fromNorm, toNorm, steps, path, dataKind, catalog.world(fromNorm), catalog.world(toNorm)))
        }

  /** The manifest step a routed morphism runs; a step run through its inverse is described with swapped endpoints. */
  private def describe(morphism: Morphism): Either[AtlasError, TransformStep] =
    val forwardId =
      if morphism.isInverted then MorphismId(morphism.id.value.stripSuffix(":inverse"))
        .fold(error => throw new IllegalArgumentException(error.message), identity) else morphism.id
    stepsById
      .get(forwardId)
      .toRight(AtlasError.Provider(TransformGraphFailure(SpatialError.MissingCoordinateMap(morphism.id))))
      .map { step =>
        if !morphism.isInverted then step
        else
          step.copy(
            from = step.to,
            to = step.from,
            notes = Some(s"Inverse of ${step.from.value}->${step.to.value}" + step.notes.fold("")(note => s": $note")),
            affine = step.affine.map(_.inverse),
            asset = None
          )
      }

object SpaceTransformGraph:
  def build(
    registry: Vector[TransformStep],
    catalog: TemplateCatalog = TemplateCatalog.standard
  ): Either[AtlasError, SpaceTransformGraph] =
    val spaces = registry.flatMap(step => Vector(SpaceId.normalize(step.from), SpaceId.normalize(step.to))).distinct
    val fullCatalog = catalog.including(spaces)
    for
      domains <- traverse(spaces)(space => TemplateDomains.domain(fullCatalog, space))
      edges <- traverse(registry.zipWithIndex) { (step, index) => morphism(step, index, fullCatalog) }
      graph <- SpatialGraph.build(domains, edges.map(_._1)).left.map(error => AtlasError.Provider(TransformGraphFailure(error)))
    yield SpaceTransformGraph(fullCatalog, graph, edges.map((edge, step) => edge.id -> step).toMap)

  private def morphism(
    step: TransformStep,
    index: Int,
    catalog: TemplateCatalog
  ): Either[AtlasError, (Morphism, TransformStep)] =
    val from = SpaceId.normalize(step.from)
    val to = SpaceId.normalize(step.to)
    val normalized = step.copy(from = from, to = to)
    for
      source <- TemplateDomains.domain(catalog, from)
      target <- TemplateDomains.domain(catalog, to)
      coordinateMap <- coordinateMapOf(normalized, source, target)
      id <- MorphismId(s"atlas-step-$index:${from.value}->${to.value}")
        .left.map(error => AtlasError.Provider(TransformGraphFailure(error)))
      morphism <- Morphism
        .between(
          id = id,
          source = source,
          target = target,
          kind = morphismKind(normalized.kind),
          routeTag = RouteTag.Anatomical,
          cost = cost(normalized),
          inverse = inverseOf(normalized),
          coordinateMap = coordinateMap
        )
        .left
        .map(error => AtlasError.Provider(TransformGraphFailure(error)))
    yield (morphism, normalized)

  /** The step's pullback (target world to source world); non-executable steps carry no map. */
  private def coordinateMapOf(step: TransformStep, source: Domain, target: Domain): Either[AtlasError, CoordinateMap] =
    (step.affine, step.asset) match
      case (Some(forward), _) =>
        CoordinateMap.affine(source, target, forward.inverse).left.map(error => AtlasError.Provider(TransformGraphFailure(error)))
      case (None, Some(asset)) =>
        CoordinateMap.fromWorldTransform(asset.transform, asset.identity).left.map(error => AtlasError.Provider(TransformGraphFailure(error)))
      case (None, None) =>
        Right(CoordinateMap.Unspecified)

  /** A reversible step whose map runs backwards may be routed in reverse; otherwise its reverse must be listed. */
  private def inverseOf(step: TransformStep): Inverse =
    if !step.reversible then Inverse.None
    else
      (step.affine, step.asset.map(_.transform)) match
        case (Some(_), _) => Inverse.Exact("affine")
        case (None, Some(WorldTransform.Linear(_, _))) => Inverse.Exact("affine")
        case (None, Some(WorldTransform.Smooth(_, _))) => Inverse.Exact("analytic")
        case (None, Some(mapped: WorldTransform.Mapped[?, ?])) =>
          mapped.availability match
            case PushAvailability.FromAsset(_, _) => Inverse.Provided("inverse asset", 1.0)
            case PushAvailability.Composed(_)     => Inverse.Provided("composed forward maps", 1.0)
            case PushAvailability.Estimated(_)    => Inverse.Approximate("qualified numerical inverse", 0.9)
            case PushAvailability.Unavailable()   => Inverse.None
        case _ => Inverse.None

  private def morphismKind(kind: TransformKind): MorphismKind =
    kind match
      case TransformKind.Identity => MorphismKind.Identity
      case TransformKind.Affine => MorphismKind.Affine3D
      case TransformKind.NonlinearWarp => MorphismKind.Warp3D
      case TransformKind.SphereResample => MorphismKind.SurfaceToSurface
      case TransformKind.VolToSurf => MorphismKind.VolumeToSurface
      case TransformKind.SurfToVol => MorphismKind.SurfaceToVolume

  private def cost(step: TransformStep): Double =
    val statusRank =
      step.status match
        case TransformStatus.Available => 0
        case TransformStatus.Planned => 1
    (statusRank * 100 + TransformPlan.confidenceRank(step.confidence) * 10 + 1).toDouble

  private def traverse[A, B](values: Vector[A])(f: A => Either[AtlasError, B]): Either[AtlasError, Vector[B]] =
    values.foldLeft[Either[AtlasError, Vector[B]]](Right(Vector.empty)) { (acc, value) =>
      acc.flatMap(out => f(value).map(out :+ _))
    }
