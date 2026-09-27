package scalafim.atlas

import image4s.geometry.{Affine as ProviderAffine, D3, Frame}
import scalafim.image.world.{FrameCatalog, Spaces as WorldFrames, TemplateName, WorldSpace}
import scalafim.spatial.{
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
import scalafim.transform.WorldTransform

/** Stable world frames for the template spaces a transform manifest names.
  *
  * Provider maps check endpoint frames by runtime owner, so every edge and every attached [[TransformAsset]] must use
  * the frames from one catalog. Named templates reuse the frames in `scalafim.image.world.Spaces`; fsaverage5/6 share
  * fsaverage's world and fsLR_32k shares fsLR's, because they are samplings of one coordinate system.
  */
final class TemplateCatalog private (private val frames: Map[AnySpaceId, Frame[D3]]):
  def spaces: Vector[AnySpaceId] =
    frames.keys.toVector.sortBy(_.value)

  def contains(space: AnySpaceId): Boolean =
    frames.contains(SpaceId.normalize(space))

  def frame(space: AnySpaceId): Either[AtlasError, Frame[D3]] =
    val normalized = SpaceId.normalize(space)
    frames.get(normalized).toRight(AtlasError.UnknownSpace(normalized))

  /** The world space a catalogued space's coordinates live in: its catalog frame's world. Uncatalogued spaces are
    * [[AtlasError.UnknownSpace]], never a guessed template.
    */
  def world(space: AnySpaceId): Either[AtlasError, WorldSpace] =
    val normalized = SpaceId.normalize(space)
    frame(normalized).flatMap(f => FrameCatalog.worldOf(f).left.map(_ => AtlasError.UnknownSpace(normalized)))

  /** The routing-graph domain of a catalogued space: unsampled, in the space's world frame. */
  def domain(space: AnySpaceId): Either[AtlasError, Domain] =
    val normalized = SpaceId.normalize(space)
    for
      world <- frame(normalized)
      kind = TemplateCatalog.domainKind(normalized)
      geometry <- SamplingGeometry.unsampled(kind, world).left.map(AtlasError.TransformGraph.apply)
      templateKind = if kind == DomainKind.Surface then TemplateKind.Surface else TemplateKind.Volume
      domain <- Domain
        .build(
          TemplateCatalog.domainId(normalized),
          SpaceRef.Template(TemplateName.unsafe(normalized.value), None, templateKind),
          geometry
        )
        .left
        .map(AtlasError.TransformGraph.apply)
    yield domain

  /** This catalog plus fresh template frames for any of `extra` it does not already hold. */
  def including(extra: Iterable[AnySpaceId]): TemplateCatalog =
    val missing = extra.iterator.map(SpaceId.normalize).filterNot(frames.contains).toVector.distinct
    if missing.isEmpty then this
    else new TemplateCatalog(frames ++ missing.map(space => space -> TemplateCatalog.templateFrame(space.value)))

object TemplateCatalog:
  val standard: TemplateCatalog =
    new TemplateCatalog(
      Map(
        SpaceId.MNI152 -> templateFrame("MNI152"),
        SpaceId.MNI305 -> WorldFrames.MNI305,
        SpaceId.MNI152NLin6Asym -> WorldFrames.MNI152NLin6Asym,
        SpaceId.MNI152NLin2009cAsym -> WorldFrames.MNI152NLin2009cAsym,
        SpaceId.FsAverage -> WorldFrames.fsaverage,
        SpaceId.FsAverage5 -> WorldFrames.fsaverage,
        SpaceId.FsAverage6 -> WorldFrames.fsaverage,
        SpaceId.FsLR32k -> WorldFrames.fsLR
      )
    )

  def domainId(space: AnySpaceId): DomainId =
    DomainId.unsafe(s"atlas-template:${SpaceId.normalize(space).value}")

  /** Surface templates are surface domains; volume and unknown spaces are volumetric world spaces. */
  def domainKind(space: AnySpaceId): DomainKind =
    SpaceId.kind(space) match
      case SpaceKindTag.Surface => DomainKind.Surface
      case SpaceKindTag.Volume | SpaceKindTag.Unknown => DomainKind.Volume

  private def templateFrame(name: String): Frame[D3] =
    FrameCatalog.frame(WorldSpace.Template(TemplateName.unsafe(name)))

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
      Right(TransformPlan.build(fromNorm, toNorm, Vector(step), MorphismPath.identity(TemplateCatalog.domainId(fromNorm)), dataKind, world, world))
    else if !catalog.contains(fromNorm) || !catalog.contains(toNorm) then Left(AtlasError.NoTransformRoute(fromNorm, toNorm))
    else
      graph
        .path(TemplateCatalog.domainId(fromNorm), TemplateCatalog.domainId(toNorm), RoutingPolicy.Shortest, allowInverses = true)
        .left
        .map {
          case SpatialError.NoPath(_, _) => AtlasError.NoTransformRoute(fromNorm, toNorm)
          case other => AtlasError.TransformGraph(other)
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
      if morphism.isInverted then MorphismId.unsafe(morphism.id.value.stripSuffix(":inverse")) else morphism.id
    stepsById
      .get(forwardId)
      .toRight(AtlasError.TransformGraph(SpatialError.MissingCoordinateMap(morphism.id)))
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
      domains <- traverse(spaces)(fullCatalog.domain)
      edges <- traverse(registry.zipWithIndex) { (step, index) => morphism(step, index, fullCatalog) }
      graph <- SpatialGraph.build(domains, edges.map(_._1)).left.map(AtlasError.TransformGraph.apply)
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
      source <- catalog.domain(from)
      target <- catalog.domain(to)
      coordinateMap <- coordinateMapOf(normalized, source, target)
      morphism <- Morphism
        .between(
          id = MorphismId.unsafe(s"atlas-step-$index:${from.value}->${to.value}"),
          source = source,
          target = target,
          kind = morphismKind(normalized.kind),
          routeTag = RouteTag.Anatomical,
          cost = cost(normalized),
          inverse = inverseOf(normalized),
          coordinateMap = coordinateMap
        )
        .left
        .map(AtlasError.TransformGraph.apply)
    yield (morphism, normalized)

  /** The step's pullback (target world to source world); non-executable steps carry no map. */
  private def coordinateMapOf(step: TransformStep, source: Domain, target: Domain): Either[AtlasError, CoordinateMap] =
    (step.affine, step.asset) match
      case (Some(forward), _) =>
        CoordinateMap.affine(source, target, forward.inverse).left.map(AtlasError.TransformGraph.apply)
      case (None, Some(asset)) =>
        CoordinateMap.fromWorldTransform(asset.transform, asset.identity).left.map(AtlasError.TransformGraph.apply)
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
        case (None, Some(mapped: WorldTransform.Mapped[?, ?])) if mapped.push.nonEmpty =>
          Inverse.Provided("inverse asset", 1.0)
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
