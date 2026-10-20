package scalafim.spatial

import image4s.geometry.{Affine, D3, Frame}
import neuroim4s.image.{SpatialPoint, SpatialPullback, SpatialPullbacks}
import neuroim4s.spatial.{ProviderMapBinding, RegistrationMapError}
import scalafim.surface.{SurfaceGeometry, SurfaceSamplingPath, SurfaceVertexMapping, VolumeSurfaceSamplingPlan}
import neuroim4s.transform.WorldTransform
import reframe4s.core.SpatialMap

import scala.util.hashing.MurmurHash3

enum MorphismKind:
  case Identity, Affine3D, Warp3D, VolumeToSurface, SurfaceToVolume, SurfaceToSurface, Functional, Filter, Hybrid

object MorphismKind:
  def compatible(kind: MorphismKind, source: DomainKind, target: DomainKind): Boolean =
    kind match
      case MorphismKind.Identity =>
        source == target
      case MorphismKind.Affine3D | MorphismKind.Warp3D =>
        source == DomainKind.Volume && target == DomainKind.Volume
      case MorphismKind.VolumeToSurface =>
        source == DomainKind.Volume && target == DomainKind.Surface
      case MorphismKind.SurfaceToVolume =>
        source == DomainKind.Surface && target == DomainKind.Volume
      case MorphismKind.SurfaceToSurface =>
        source == DomainKind.Surface && target == DomainKind.Surface
      case MorphismKind.Functional =>
        true
      case MorphismKind.Filter =>
        source == target
      case MorphismKind.Hybrid =>
        source == DomainKind.Hybrid || target == DomainKind.Hybrid

enum RouteTag:
  case Identity, Anatomical, Functional

enum RoutingPolicy:
  case Shortest, Anatomical, Functional

enum CoordinateMap:
  case Identity
  case Geometric(binding: ProviderMapBinding)
  case VolumeSamples(plan: VolumeSurfaceSamplingPlan)
  case SurfaceVertices(mapping: SurfaceVertexMapping)
  case SurfaceResampling(binding: SurfaceResamplingBinding)
  case Unspecified

  @deprecated("Use transform(SpatialPoint); a bare Vector[Double] carries neither arity nor role.", "0.2.0")
  def transform(point: Vector[Double]): Either[SpatialError, Vector[Double]] =
    if point.length != 3 || point.exists(value => !value.isFinite) then
      Left(SpatialError.CoordinateTransformFailed("point must be finite 3D"))
    else transform(SpatialPoint(point(0), point(1), point(2))).map(_.toVector)

  def transform(point: SpatialPoint): Either[SpatialError, SpatialPoint] =
    this match
      case CoordinateMap.Identity =>
        Right(point)
      case CoordinateMap.Geometric(binding) =>
        SpatialPullbacks
          .transform(binding.pullback, point)
          .left
          .map(error => SpatialError.CoordinateTransformFailed(error.message))
      case CoordinateMap.VolumeSamples(_) =>
        Left(SpatialError.CoordinateTransformFailed("volume-to-surface sampling is not a point transform"))
      case CoordinateMap.SurfaceVertices(_) =>
        Left(SpatialError.CoordinateTransformFailed("surface vertex mapping is not a world-coordinate transform"))
      case CoordinateMap.SurfaceResampling(_) =>
        Left(SpatialError.CoordinateTransformFailed("surface resampling is not a world-coordinate transform"))
      case CoordinateMap.Unspecified =>
        Left(SpatialError.CoordinateTransformFailed("coordinate map is unspecified"))

  def inverted: Either[SpatialError, CoordinateMap] =
    this match
      case CoordinateMap.Identity =>
        Right(CoordinateMap.Identity)
      case CoordinateMap.Geometric(binding) =>
        binding.inverted.left.map(CoordinateMap.mapBindingError).map(CoordinateMap.Geometric.apply)
      case CoordinateMap.VolumeSamples(_) | CoordinateMap.SurfaceVertices(_) | CoordinateMap.SurfaceResampling(_) =>
        Left(SpatialError.CoordinateTransformFailed("discrete or sampling coordinate maps have no geometric inverse"))
      case CoordinateMap.Unspecified =>
        Left(SpatialError.CoordinateTransformFailed("coordinate map is unspecified"))

  def fingerprint: String =
    this match
      case CoordinateMap.Identity =>
        "identity-v1"
      case CoordinateMap.Geometric(binding) =>
        binding.fingerprint.value
      case CoordinateMap.VolumeSamples(plan) =>
        CoordinateMap.volumeSamplesFingerprint(plan)
      case CoordinateMap.SurfaceVertices(mapping) =>
        CoordinateMap.surfaceVerticesFingerprint(mapping)
      case CoordinateMap.SurfaceResampling(binding) =>
        CoordinateMap.surfaceResamplingFingerprint(binding)
      case CoordinateMap.Unspecified =>
        "unspecified-v1"

  private[spatial] def isProviderAffine: Boolean =
    this match
      case CoordinateMap.Geometric(binding) => binding.isAffine
      case _ => false

object CoordinateMap:
  def affine(
      source: Domain,
      target: Domain,
      operator: Affine[D3]
  ): Either[SpatialError, CoordinateMap] =
    for
      sourceFrame <- volumeFrame(source)
      targetFrame <- volumeFrame(target)
      binding <- affineBetween(sourceFrame, targetFrame, operator)
    yield CoordinateMap.Geometric(binding)

  private[spatial] def affine(
      source: neuroim4s.image.GridSpec[?],
      target: neuroim4s.image.GridSpec[?],
      operator: Affine[D3]
  ): Either[SpatialError, CoordinateMap] =
    affineBetween(source.grid.frame, target.grid.frame, operator).map(CoordinateMap.Geometric.apply)

  private[spatial] def affineBetween(outputFrame: Frame[D3], inputFrame: Frame[D3], operator: Affine[D3]): Either[SpatialError, ProviderMapBinding] =
    ProviderMapBinding.affineBetween(outputFrame, inputFrame, operator).left.map(mapBindingError)

  def dense(pullback: SpatialPullback[?, ?], inversePullback: Option[SpatialPullback[?, ?]] = None): Either[SpatialError, CoordinateMap] =
    ProviderMapBinding.dense(pullback, inversePullback).left.map(mapBindingError).map(CoordinateMap.Geometric.apply)

  def fromWorldTransform[S <: Frame[D3], T <: Frame[D3]](transform: WorldTransform[S, T], identity: String): Either[SpatialError, CoordinateMap] =
    ProviderMapBinding.fromWorldTransform(transform, identity).left.map(mapBindingError).map(CoordinateMap.Geometric.apply)

  private[spatial] def mapBindingError(error: RegistrationMapError): SpatialError = error match
    case RegistrationMapError.InvalidProviderCoordinateMap(reason) => SpatialError.InvalidProviderCoordinateMap(reason)
    case RegistrationMapError.CoordinateTransformFailed(reason) => SpatialError.CoordinateTransformFailed(reason)
    case RegistrationMapError.Geometry(cause) => SpatialError.Geometry(cause)

  /** Compose provider maps in the order they are applied to a point. */
  def compose(
    applicationOrder: Vector[CoordinateMap],
    inverseApplicationOrder: Option[Vector[CoordinateMap]] = None
  ): Either[SpatialError, CoordinateMap] =
    val components = geometricBindings(applicationOrder)
    val inverse = inverseApplicationOrder.map(geometricBindings)
    for
      checked <- components
      checkedInverse <- inverse.fold[Either[SpatialError, Option[Vector[ProviderMapBinding]]]](Right(None))(_.map(Some.apply))
      binding <- ProviderMapBinding.composed(checked, checkedInverse).left.map(mapBindingError)
    yield CoordinateMap.Geometric(binding)

  def attachInverse(
      forward: CoordinateMap,
      inverse: CoordinateMap
  ): Either[SpatialError, CoordinateMap] =
    (forward, inverse) match
      case (CoordinateMap.Geometric(primary), CoordinateMap.Geometric(reverse)) =>
        ProviderMapBinding
          .attachInverse(primary, reverse)
          .left.map(mapBindingError)
          .map(CoordinateMap.Geometric.apply)
      case _ =>
        Left(SpatialError.InvalidProviderCoordinateMap("forward and inverse must both be provider geometric maps"))

  def volumeSamples(plan: VolumeSurfaceSamplingPlan): CoordinateMap =
    CoordinateMap.VolumeSamples(plan)

  def surfaceVertices(mapping: SurfaceVertexMapping): CoordinateMap =
    CoordinateMap.SurfaceVertices(mapping)

  def surfaceResampling(binding: SurfaceResamplingBinding): CoordinateMap =
    CoordinateMap.SurfaceResampling(binding)

  private def geometricBindings(
      maps: Vector[CoordinateMap]
  ): Either[SpatialError, Vector[ProviderMapBinding]] =
    val out = Vector.newBuilder[ProviderMapBinding]
    var index = 0
    while index < maps.length do
      maps(index) match
        case CoordinateMap.Identity => ()
        case CoordinateMap.Geometric(binding) => out += binding
        case _ =>
          return Left(
            SpatialError.InvalidProviderCoordinateMap(
              s"component $index is not an identity or provider geometric map"
            )
          )
      index += 1
    val result = out.result()
    if result.isEmpty then Left(SpatialError.InvalidProviderCoordinateMap("at least one provider map is required"))
    else Right(result)

  private def volumeFrame(domain: Domain): Either[SpatialError, Frame[D3]] =
    domain.geometry match
      case SamplingGeometry.Volume(volumeGrid) =>
        val space = volumeGrid.space
        Right(space.grid.frame)
      case SamplingGeometry.Unsampled(DomainKind.Volume, frame) => Right(frame)
      case _ =>
        Left(
          SpatialError.InvalidProviderCoordinateMap(
            s"domain ${domain.id.value} is not volumetric"
          )
        )

  private def volumeSamplesFingerprint(plan: VolumeSurfaceSamplingPlan): String =
    var hash = MurmurHash3.stringHash(s"volume-samples-v1|${samplingPathFingerprint(plan.path)}|${plan.aggregation}")
    hash = hashSurfaceGeometry(hash, plan.surfaces.white)
    hash = hashSurfaceGeometry(hash, plan.surfaces.pial)
    s"volume-samples-v1:${java.lang.Integer.toHexString(MurmurHash3.finalizeHash(hash, 2))}"

  private def surfaceVerticesFingerprint(mapping: SurfaceVertexMapping): String =
    var hash = MurmurHash3.stringHash("surface-vertices-v1")
    hash = hashSurfaceGeometry(hash, mapping.sourceGeometry)
    hash = hashSurfaceGeometry(hash, mapping.targetGeometry)
    var i = 0
    while i < mapping.sourceForTarget.length do
      hash = MurmurHash3.mix(hash, mapping.sourceForTarget(i).index)
      i += 1
    s"surface-vertices-v1:${java.lang.Integer.toHexString(MurmurHash3.finalizeHash(hash, mapping.sourceForTarget.length))}"

  private def surfaceResamplingFingerprint(binding: SurfaceResamplingBinding): String =
    val raw = binding.plan.plan
    var hash = MurmurHash3.stringHash(s"surface-resampling-v1|${binding.plan.source}|${binding.plan.target}|${binding.normalization}|${binding.plan.registration}|${binding.plan.sourceSphereIdentity}|${binding.plan.targetSphereIdentity}|${raw.method}|${raw.referenceVertices}|${raw.movingVertices}")
    var i = 0
    while i < raw.vals.length do
      hash = MurmurHash3.mix(hash, raw.rows(i))
      hash = MurmurHash3.mix(hash, raw.cols(i))
      hash = MurmurHash3.mix(hash, raw.vals(i).hashCode)
      i += 1
    hash = hashSurfaceGeometry(hash, binding.sourceGeometry)
    hash = hashSurfaceGeometry(hash, binding.targetGeometry)
    s"surface-resampling-v1:${java.lang.Integer.toHexString(MurmurHash3.finalizeHash(hash, raw.vals.length))}"

  private def hashSurfaceGeometry(seed: Int, geometry: SurfaceGeometry): Int =
    var hash = MurmurHash3.mix(seed, geometry.hemisphere.hashCode)
    hash = MurmurHash3.mix(hash, geometry.kind.hashCode)
    var position = 0
    while position < geometry.mesh.coordinates.length do
      hash = MurmurHash3.mix(hash, geometry.mesh.coordinates(position).hashCode)
      position += 1
    var i = 0
    while i < geometry.mesh.faceIndices.length do
      hash = MurmurHash3.mix(hash, geometry.mesh.faceIndices(i))
      i += 1
    hashDoubles(hash, geometry.surfaceToWorld.rowMajor.toArray)

  private def samplingPathFingerprint(path: SurfaceSamplingPath): String =
    path match
      case SurfaceSamplingPath.White => "white"
      case SurfaceSamplingPath.Pial => "pial"
      case SurfaceSamplingPath.Midpoint => "midpoint"
      case SurfaceSamplingPath.FractionalThickness(fractions) =>
        s"fractions:${fractions.mkString(",")}"
      case SurfaceSamplingPath.NormalLine(offsets) =>
        s"normal:${offsets.mkString(",")}"

  private def hashDoubles(seed: Int, values: Array[Double]): Int =
    var hash = seed
    var i = 0
    while i < values.length do
      hash = MurmurHash3.mix(hash, values(i).hashCode)
      i += 1
    hash

enum Inverse:
  case Exact(method: String)
  case Provided(method: String, score: Double)
  case Approximate(method: String, score: Double)
  case AdjointOnly
  case None

  this match
    case Inverse.Exact(method) =>
      require(method.trim.nonEmpty, "inverse method must be non-empty")
    case Inverse.Provided(method, score) =>
      require(method.trim.nonEmpty, "inverse method must be non-empty")
      require(score.isFinite && score >= 0.0 && score <= 1.0, "inverse quality must be in [0, 1]")
    case Inverse.Approximate(method, score) =>
      require(method.trim.nonEmpty, "inverse method must be non-empty")
      require(score.isFinite && score >= 0.0 && score <= 1.0, "inverse quality must be in [0, 1]")
    case _ =>
      ()

  def isGeometric: Boolean =
    this match
      case Inverse.Exact(_) | Inverse.Provided(_, _) | Inverse.Approximate(_, _) => true
      case Inverse.AdjointOnly | Inverse.None => false

  def quality: Double =
    this match
      case Inverse.Exact(_) => 1.0
      case Inverse.Provided(_, score) => score
      case Inverse.Approximate(_, score) => score
      case Inverse.AdjointOnly | Inverse.None => 0.0

final case class Morphism private (
  id: MorphismId,
  source: DomainId,
  target: DomainId,
  kind: MorphismKind,
  routeTag: RouteTag,
  cost: Double,
  inverse: Inverse,
  coordinateMap: CoordinateMap,
  isInverted: Boolean,
  plugin: Option[MorphismPlugin]
):
  /** This morphism run backwards, as a routing edge costing `cost + penalty * (1 - inverse.quality)`. */
  def reversed: Either[SpatialError, Morphism] =
    reversed(InversePenalty.default)

  def reversed(inversePenalty: InversePenalty): Either[SpatialError, Morphism] =
    if plugin.nonEmpty || !inverse.isGeometric then Left(SpatialError.NonInvertibleMorphism(id))
    else
      val inverseId = MorphismId.unsafe(s"${id.value}:inverse")
      val penalty = inversePenalty.value * (1.0 - inverse.quality)
      val inverseMap =
        coordinateMap match
          case CoordinateMap.Unspecified => Right(CoordinateMap.Unspecified)
          case _ => coordinateMap.inverted.left.map(_ => SpatialError.NonInvertibleMorphism(id))
      inverseMap.flatMap { map =>
        Morphism.build(
          id = inverseId,
          source = target,
          target = source,
          kind = kind,
          routeTag = routeTag,
          cost = cost + penalty,
          inverse = inverse,
          coordinateMap = map,
          isInverted = true,
          plugin = None
        )
      }

object Morphism:
  def between(
    id: MorphismId,
    source: Domain,
    target: Domain,
    kind: MorphismKind,
    routeTag: RouteTag,
    cost: Double = 1.0,
    inverse: Inverse = Inverse.None,
    coordinateMap: CoordinateMap = CoordinateMap.Unspecified,
    isInverted: Boolean = false,
    plugin: Option[MorphismPlugin] = None
  ): Either[SpatialError, Morphism] =
    build(
      id = id,
      source = source.id,
      target = target.id,
      kind = kind,
      routeTag = routeTag,
      cost = cost,
      inverse = inverse,
      coordinateMap = coordinateMap,
      isInverted = isInverted,
      plugin = plugin
    ).flatMap(morphism => validateDomains(morphism, source, target).map(_ => morphism))

  def build(
    id: MorphismId,
    source: DomainId,
    target: DomainId,
    kind: MorphismKind,
    routeTag: RouteTag,
    cost: Double = 1.0,
    inverse: Inverse = Inverse.None,
    coordinateMap: CoordinateMap = CoordinateMap.Unspecified,
    isInverted: Boolean = false,
    plugin: Option[MorphismPlugin] = None
  ): Either[SpatialError, Morphism] =
    if !cost.isFinite || cost < 0.0 then Left(SpatialError.InvalidCost(cost))
    else if kind == MorphismKind.Identity && source != target then
      Left(SpatialError.IdentityMorphismDomainMismatch(source, target))
    else if coordinateMap != CoordinateMap.Unspecified && !mapMatchesKind(kind, coordinateMap) then
      Left(SpatialError.UnsupportedMorphismForCompilation(id, kind))
    else if plugin.exists(value => !MorphismPlugin.supports(kind, value)) then
      Left(SpatialError.InvalidMorphismPlugin(id, s"plugin payload is incompatible with $kind"))
    else if plugin.nonEmpty && coordinateMap != CoordinateMap.Unspecified then
      Left(SpatialError.InvalidMorphismPlugin(id, "value plugins cannot also carry a coordinate map"))
    else
      inverse match
        case Inverse.Provided(_, score) if !score.isFinite || score < 0.0 || score > 1.0 =>
          Left(SpatialError.InvalidQuality("provided inverse", score))
        case Inverse.Approximate(_, score) if !score.isFinite || score < 0.0 || score > 1.0 =>
          Left(SpatialError.InvalidQuality("approximate inverse", score))
        case _ =>
          Right(new Morphism(id, source, target, kind, routeTag, cost, inverse, coordinateMap, isInverted, plugin))

  def identity(domain: DomainId): Morphism =
    new Morphism(
      id = MorphismId.unsafe(s"identity:${domain.value}"),
      source = domain,
      target = domain,
      kind = MorphismKind.Identity,
      routeTag = RouteTag.Identity,
      cost = 0.0,
      inverse = Inverse.Exact("identity"),
      coordinateMap = CoordinateMap.Identity,
      isInverted = false,
      plugin = None
    )

  private def mapMatchesKind(kind: MorphismKind, coordinateMap: CoordinateMap): Boolean =
    (kind, coordinateMap) match
      case (MorphismKind.Identity, CoordinateMap.Identity) => true
      case (MorphismKind.Affine3D, CoordinateMap.Geometric(binding)) => binding.isAffine
      case (MorphismKind.Warp3D, CoordinateMap.Geometric(_)) => true
      case (MorphismKind.VolumeToSurface, CoordinateMap.VolumeSamples(_)) => true
      case (MorphismKind.SurfaceToSurface, CoordinateMap.SurfaceVertices(_)) => true
      case (MorphismKind.SurfaceToSurface, CoordinateMap.SurfaceResampling(_)) => true
      case (_, CoordinateMap.Unspecified) => true
      case _ => false

  private[spatial] def validateDomains(
    morphism: Morphism,
    source: Domain,
    target: Domain
  ): Either[SpatialError, Unit] =
    if morphism.source != source.id then Left(SpatialError.MorphismDomainMissing(morphism.id, morphism.source))
    else if morphism.target != target.id then Left(SpatialError.MorphismDomainMissing(morphism.id, morphism.target))
    else if !MorphismKind.compatible(morphism.kind, source.kind, target.kind) then
      Left(SpatialError.IncompatibleMorphismKind(morphism.kind, source.id, source.kind, target.id, target.kind))
    else
      val geometryValidation = morphism.coordinateMap match
        case CoordinateMap.Geometric(binding) =>
          validateProviderMapDomains(binding.pullback, source, target)
        case CoordinateMap.VolumeSamples(plan) =>
          target.geometry match
            case SamplingGeometry.Surface(geometry, _)
                if geometry == plan.surfaces.white => Right(())
            case _ => Left(SpatialError.SurfaceSamplingGeometryMismatch(morphism.id))
        case CoordinateMap.SurfaceVertices(mapping) =>
          (source.geometry, target.geometry) match
            case (
                  SamplingGeometry.Surface(sourceGeometry, _),
                  SamplingGeometry.Surface(targetGeometry, _)
                ) if sourceGeometry == mapping.sourceGeometry && targetGeometry == mapping.targetGeometry =>
              Right(())
            case _ => Left(SpatialError.SurfaceMappingGeometryMismatch(morphism.id))
        case CoordinateMap.SurfaceResampling(binding) =>
          binding.validateEndpoints
            .flatMap { _ =>
              (source.space, target.space, source.geometry, target.geometry) match
                case (
                      SpaceRef.Template(sourceName, _, TemplateKind.Surface),
                      SpaceRef.Template(targetName, _, TemplateKind.Surface),
                      SamplingGeometry.Surface(sourceGeometry, _),
                      SamplingGeometry.Surface(targetGeometry, _)
                    )
                    if sourceName.value == binding.plan.source.mesh.label &&
                      targetName.value == binding.plan.target.mesh.label &&
                      sourceGeometry == binding.sourceGeometry && targetGeometry == binding.targetGeometry => Right(())
                case _ => Left(SpatialError.SurfaceMappingGeometryMismatch(morphism.id))
            }
        case _ => Right(())
      geometryValidation.flatMap(_ => MorphismPlugin.validateDomains(morphism, source, target))

  private def validateProviderMapDomains(
      pullback: SpatialPullback[Frame[D3], Frame[D3]],
      source: Domain,
      target: Domain
  ): Either[SpatialError, Unit] =
    (worldFrame(source.geometry), worldFrame(target.geometry)) match
      case (Some(sourceFrame), Some(targetFrame)) =>
        for
          _ <- SpatialMap
            .validateSourceFrame(targetFrame, pullback.source)
            .left
            .map(SpatialError.ProviderMap.apply)
          _ <- SpatialMap
            .validateResultFrame(sourceFrame, pullback.target)
            .left
            .map(SpatialError.ProviderMap.apply)
        yield ()
      case _ =>
        Left(
          SpatialError.CoordinateTransformFailed(
            "provider spatial maps require volume source and target domains"
          )
        )

  /** The world frame a volumetric domain's coordinates live in: its grid's frame, or an unsampled volume's frame. */
  private def worldFrame(geometry: SamplingGeometry): Option[Frame[D3]] =
    geometry match
      case SamplingGeometry.Volume(volumeGrid) =>
        val space = volumeGrid.space
        Some(space.grid.frame)
      case SamplingGeometry.Unsampled(DomainKind.Volume, frame) => Some(frame)
      case _ => None

final case class MorphismPath private (
  source: DomainId,
  target: DomainId,
  morphisms: Vector[Morphism],
  usedInverses: Boolean
):
  def cost: Double =
    morphisms.map(_.cost).sum

  def pathQuality: Double =
    if morphisms.isEmpty then 1.0 else morphisms.map(_.inverse.quality).min

  def ids: Vector[MorphismId] =
    morphisms.map(_.id)

  /** The steps that run a morphism backwards through its geometric inverse. */
  def invertedSteps: Vector[MorphismId] =
    morphisms.filter(_.isInverted).map(_.id)

  def inspect: PathInspection =
    PathInspection.of(this)

  /** Evaluate the route's pullback: a point in the target's world to the corresponding point in the source's world.
    * Every non-identity step must carry a provider geometric map.
    */
  def pullback(point: SpatialPoint): Either[SpatialError, SpatialPoint] =
    morphisms.reverse.foldLeft[Either[SpatialError, SpatialPoint]](Right(point)) { (current, morphism) =>
      current.flatMap(p => MorphismPath.geometricMap(morphism).flatMap(_.transform(p)))
    }

  /** Evaluate the route forwards: a point in the source's world to the target's world. Every non-identity step must
    * carry a provider geometric map with an executable inverse (its forward direction).
    */
  def push(point: SpatialPoint): Either[SpatialError, SpatialPoint] =
    morphisms.foldLeft[Either[SpatialError, SpatialPoint]](Right(point)) { (current, morphism) =>
      current.flatMap { p =>
        MorphismPath
          .geometricMap(morphism)
          .flatMap(_.inverted.left.map(_ => SpatialError.NonInvertibleMorphism(morphism.id)))
          .flatMap(_.transform(p))
      }
    }

object MorphismPath:
  def build(morphisms: Vector[Morphism], usedInverses: Boolean = false): Either[SpatialError, MorphismPath] =
    if morphisms.isEmpty then Left(SpatialError.EmptyPath)
    else
      var i = 1
      while i < morphisms.length do
        val previous = morphisms(i - 1)
        val next = morphisms(i)
        if previous.target != next.source then return Left(SpatialError.DisconnectedPath(previous.target, next.source))
        i += 1
      Right(new MorphismPath(morphisms.head.source, morphisms.last.target, morphisms, usedInverses))

  def identity(domain: DomainId): MorphismPath =
    new MorphismPath(domain, domain, Vector(Morphism.identity(domain)), usedInverses = false)

  private def geometricMap(morphism: Morphism): Either[SpatialError, CoordinateMap] =
    morphism.coordinateMap match
      case CoordinateMap.Identity => Right(CoordinateMap.Identity)
      case geometric @ CoordinateMap.Geometric(_) => Right(geometric)
      case _ => Left(SpatialError.MissingCoordinateMap(morphism.id))

final case class ExecutableAffinePath private (
  path: MorphismPath,
  coordinateMap: CoordinateMap
):
  def source: DomainId =
    path.source

  def target: DomainId =
    path.target

  def ids: Vector[MorphismId] =
    path.ids

object ExecutableAffinePath:
  def from(path: MorphismPath): Either[SpatialError, ExecutableAffinePath] =
    pathCoordinateMap(path).map(map => new ExecutableAffinePath(path, map))

  private def pathCoordinateMap(path: MorphismPath): Either[SpatialError, CoordinateMap] =
    val nonIdentity = path.morphisms.filter(_.kind != MorphismKind.Identity)
    nonIdentity.find(morphism =>
      morphism.kind != MorphismKind.Affine3D ||
        !morphism.coordinateMap.isProviderAffine
    ) match
      case Some(morphism) =>
        Left(SpatialError.UnsupportedMorphismForCompilation(morphism.id, morphism.kind))
      case None if nonIdentity.isEmpty =>
        Right(CoordinateMap.Identity)
      case None =>
        CoordinateMap.compose(nonIdentity.reverse.map(_.coordinateMap))

final case class VolumeToSurfacePath private (path: MorphismPath):
  def source: DomainId =
    path.source

  def target: DomainId =
    path.target

  def ids: Vector[MorphismId] =
    path.ids

object VolumeToSurfacePath:
  def from(path: MorphismPath): Either[SpatialError, VolumeToSurfacePath] =
    val nonIdentity = path.morphisms.filter(_.kind != MorphismKind.Identity)
    if nonIdentity.length == 1 && nonIdentity.head.kind == MorphismKind.VolumeToSurface then
      Right(new VolumeToSurfacePath(path))
    else
      val offending = nonIdentity.headOption.getOrElse(path.morphisms.head)
      Left(SpatialError.UnsupportedMorphismForCompilation(offending.id, offending.kind))
