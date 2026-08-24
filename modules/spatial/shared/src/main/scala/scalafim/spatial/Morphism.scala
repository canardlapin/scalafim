package scalafim.spatial

import image4s.geometry.{Affine, D3, Frame}
import scalafim.image.{SpatialPoint, SpatialPullback, SpatialPullbacks}
import scalafim.surface.{SurfaceGeometry, SurfaceSamplingPath, SurfaceVertexMapping, VolumeSurfaceSamplingPlan}
import reframe4s.core.SpatialMap
import reframe4s.field.DenseMap

import scala.util.hashing.MurmurHash3

enum MorphismKind:
  case Identity, Affine3D, Warp3D, VolumeToSurface, SurfaceToSurface, Functional, Filter, Hybrid

object MorphismKind:
  def compatible(kind: MorphismKind, source: DomainKind, target: DomainKind): Boolean =
    kind match
      case MorphismKind.Identity =>
        source == target
      case MorphismKind.Affine3D | MorphismKind.Warp3D =>
        source == DomainKind.Volume && target == DomainKind.Volume
      case MorphismKind.VolumeToSurface =>
        source == DomainKind.Volume && target == DomainKind.Surface
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
  case Unspecified

  def transform(point: Vector[Double]): Either[SpatialError, Vector[Double]] =
    this match
      case _ if point.length != 3 || point.exists(value => !value.isFinite) =>
        Left(SpatialError.CoordinateTransformFailed("point must be finite 3D"))
      case CoordinateMap.Identity =>
        Right(point)
      case CoordinateMap.Geometric(binding) =>
        CoordinateMap.transformProvider(binding.pullback, point)
      case CoordinateMap.VolumeSamples(_) =>
        Left(SpatialError.CoordinateTransformFailed("volume-to-surface sampling is not a point transform"))
      case CoordinateMap.SurfaceVertices(_) =>
        Left(SpatialError.CoordinateTransformFailed("surface vertex mapping is not a world-coordinate transform"))
      case CoordinateMap.Unspecified =>
        Left(SpatialError.CoordinateTransformFailed("coordinate map is unspecified"))

  def transform(point: SpatialPoint): Either[SpatialError, SpatialPoint] =
    transform(point.toVector).flatMap { values =>
      SpatialPoint
        .fromVector(values, "transformed point")
        .left.map(err => SpatialError.CoordinateTransformFailed(err.message))
    }

  def inverted: Either[SpatialError, CoordinateMap] =
    this match
      case CoordinateMap.Identity =>
        Right(CoordinateMap.Identity)
      case CoordinateMap.Geometric(binding) =>
        binding.inverted.map(CoordinateMap.Geometric.apply)
      case CoordinateMap.VolumeSamples(_) | CoordinateMap.SurfaceVertices(_) =>
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
      case CoordinateMap.Unspecified =>
        "unspecified-v1"

  private[spatial] def isProviderAffine: Boolean =
    this match
      case CoordinateMap.Geometric(binding) => binding.isAffine
      case _ => false

opaque type CoordinateMapFingerprint = String

object CoordinateMapFingerprint:
  private[spatial] def unsafe(value: String): CoordinateMapFingerprint =
    value

  extension (fingerprint: CoordinateMapFingerprint)
    def value: String =
      fingerprint

/** Checked storage envelope for the one provider-owned geometric map algebra.
  *
  * This value carries routing metadata and an optional executable inverse. It
  * never evaluates or composes coordinates itself: `pullback` and every
  * composed value are reframe4s `SpatialMap`s.
  */
final case class ProviderMapBinding private (
  pullback: SpatialPullback,
  inversePullback: Option[SpatialPullback],
  fingerprint: CoordinateMapFingerprint,
  private[spatial] val inverseFingerprint: Option[CoordinateMapFingerprint],
  affineOperator: Option[Affine[D3]],
  containsDense: Boolean,
  componentCount: Int
):
  def isAffine: Boolean =
    affineOperator.nonEmpty

  def inverted: Either[SpatialError, ProviderMapBinding] =
    (inversePullback, inverseFingerprint) match
      case (Some(inverse), Some(reverseFingerprint)) =>
        Right(
          new ProviderMapBinding(
            inverse,
            Some(pullback),
            reverseFingerprint,
            Some(fingerprint),
            affineOperator.map(_.inverse),
            containsDense,
            componentCount
          )
        )
      case (None, None) =>
        Left(SpatialError.CoordinateTransformFailed("provider map has no executable geometric inverse"))
      case _ =>
        Left(SpatialError.InvalidProviderCoordinateMap("provider inverse map and fingerprint must be present together"))

object ProviderMapBinding:
  private[spatial] def attachInverse(
      primary: ProviderMapBinding,
      reverse: ProviderMapBinding
  ): Either[SpatialError, ProviderMapBinding] =
    validateInverse(primary.pullback, reverse.pullback).map: _ =>
      new ProviderMapBinding(
        primary.pullback,
        Some(reverse.pullback),
        primary.fingerprint,
        Some(reverse.fingerprint),
        primary.affineOperator,
        primary.containsDense,
        primary.componentCount
      )

  private[spatial] def affine(
    pullback: SpatialPullback,
    inversePullback: SpatialPullback,
    operator: Affine[D3]
  ): Either[SpatialError, ProviderMapBinding] =
    validateInverse(pullback, inversePullback).map: _ =>
      new ProviderMapBinding(
        pullback,
        Some(inversePullback),
        CoordinateMapFingerprint.unsafe(CoordinateMap.numericFingerprint("provider-affine-v1", operator.rowMajor.toArray)),
        Some(
          CoordinateMapFingerprint.unsafe(
            CoordinateMap.numericFingerprint("provider-affine-v1", operator.inverse.rowMajor.toArray)
          )
        ),
        Some(operator),
        containsDense = false,
        componentCount = 1
      )

  private[spatial] def dense(
    pullback: SpatialPullback,
    inversePullback: Option[SpatialPullback] = None
  ): Either[SpatialError, ProviderMapBinding] =
    val denseValidation =
      if DenseMap.isDense(pullback) && inversePullback.forall(DenseMap.isDense)
      then Right(())
      else
        Left(
          SpatialError.InvalidProviderCoordinateMap(
            "dense bindings require provider DenseMap values"
          )
        )
    for
      _ <- denseValidation
      _ <- inversePullback.fold[Either[SpatialError, Unit]](Right(()))(validateInverse(pullback, _))
      denseFingerprint <- DenseMap
        .fingerprint(pullback)
        .toRight(SpatialError.InvalidProviderCoordinateMap("provider dense map has no structural fingerprint"))
      inverseDenseFingerprint <- inversePullback match
        case Some(inverse) =>
          DenseMap
            .fingerprint(inverse)
            .map(value => Some(CoordinateMapFingerprint.unsafe(value)))
            .toRight(SpatialError.InvalidProviderCoordinateMap("provider inverse dense map has no structural fingerprint"))
        case None => Right(None)
    yield new ProviderMapBinding(
      pullback,
      inversePullback,
      CoordinateMapFingerprint.unsafe(denseFingerprint),
      inverseDenseFingerprint,
      affineOperator = None,
      containsDense = true,
      componentCount = 1
    )

  private[spatial] def composed(
      components: Vector[ProviderMapBinding],
      inverseComponents: Option[Vector[ProviderMapBinding]]
  ): Either[SpatialError, ProviderMapBinding] =
    for
      pullback <- composeProvider(components)
      inverse <- inverseComponents match
        case Some(explicit) => composeProvider(explicit).map(Some.apply)
        case None =>
          val derived = components.reverse.map(_.inversePullback)
          if derived.forall(_.nonEmpty) then
            composeProviderMaps(derived.flatten).map(Some.apply)
          else Right(None)
      _ <- inverse.fold[Either[SpatialError, Unit]](Right(()))(validateInverse(pullback, _))
      affine <- composeAffines(components)
      fingerprint = compositeFingerprint(components.map(_.fingerprint))
      inverseFingerprint = inverseComponents match
        case Some(explicit) => Some(compositeFingerprint(explicit.map(_.fingerprint)))
        case None =>
          val derived = components.reverse.map(_.inverseFingerprint)
          if derived.forall(_.nonEmpty) then Some(compositeFingerprint(derived.flatten))
          else None
    yield new ProviderMapBinding(
      pullback,
      inverse,
      fingerprint,
      inverseFingerprint,
      affine,
      components.exists(_.containsDense),
      components.map(_.componentCount).sum
    )

  private def composeProvider(
      components: Vector[ProviderMapBinding]
  ): Either[SpatialError, SpatialPullback] =
    if components.isEmpty then
      Left(SpatialError.InvalidProviderCoordinateMap("at least one provider map is required"))
    else composeProviderMaps(components.map(_.pullback))

  private def composeProviderMaps(
      components: Vector[SpatialPullback]
  ): Either[SpatialError, SpatialPullback] =
    var current = components.head
    var index = 1
    var error = Option.empty[SpatialError]
    while index < components.length && error.isEmpty do
      val next = components(index)
      SpatialMap.validateResultFrame(current.target, next.source) match
        case Left(cause) =>
          error = Some(SpatialError.InvalidProviderCoordinateMap(s"component ${index - 1} -> $index: ${cause.message}"))
        case Right(_) =>
          current = SpatialMap.compose(current, next)
      index += 1
    error.toLeft(current)

  private def composeAffines(
      components: Vector[ProviderMapBinding]
  ): Either[SpatialError, Option[Affine[D3]]] =
    if !components.forall(_.isAffine) then Right(None)
    else
      var current = components.head.affineOperator.get
      var index = 1
      var error = Option.empty[SpatialError]
      while index < components.length && error.isEmpty do
        current.andThen(components(index).affineOperator.get) match
          case Left(cause) => error = Some(SpatialError.Geometry(cause))
          case Right(value) => current = value
        index += 1
      error match
        case Some(cause) => Left(cause)
        case None => Right(Some(current))

  private[spatial] def validateInverse(
      pullback: SpatialPullback,
      inverse: SpatialPullback
  ): Either[SpatialError, Unit] =
    for
      _ <- SpatialMap
        .validateSourceFrame(inverse.source, pullback.target)
        .left
        .map(error => SpatialError.InvalidProviderCoordinateMap(error.message))
      _ <- SpatialMap
        .validateResultFrame(pullback.source, inverse.target)
        .left
        .map(error => SpatialError.InvalidProviderCoordinateMap(error.message))
    yield ()

  private def compositeFingerprint(
      components: Vector[CoordinateMapFingerprint]
  ): CoordinateMapFingerprint =
    val ordered = components.map(_.value).mkString("|")
    val hash = MurmurHash3.stringHash(s"provider-composite-v1|components=$ordered")
    CoordinateMapFingerprint.unsafe(s"provider-composite-v1:${java.lang.Integer.toHexString(hash)}")

object CoordinateMap:
  private[spatial] def transformProvider(
    map: SpatialPullback,
    coordinates: Vector[Double]
  ): Either[SpatialError, Vector[Double]] =
    SpatialPoint
      .fromVector(coordinates, "provider pullback input")
      .left
      .map(error => SpatialError.CoordinateTransformFailed(error.message))
      .flatMap(point =>
        SpatialPullbacks
          .transform(map, point)
          .left
          .map(error => SpatialError.CoordinateTransformFailed(error.message))
          .map(_.toVector)
      )

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
      source: scalafim.image.GridSpec,
      target: scalafim.image.GridSpec,
      operator: Affine[D3]
  ): Either[SpatialError, CoordinateMap] =
    affineBetween(source.providerFrame, target.providerFrame, operator).map(CoordinateMap.Geometric.apply)

  private[spatial] def affineBetween(
      outputFrame: Frame[D3],
      inputFrame: Frame[D3],
      operator: Affine[D3]
  ): Either[SpatialError, ProviderMapBinding] =
    val pullback = SpatialPullbacks.affineBetween(outputFrame, inputFrame, operator)
    val inverse = SpatialPullbacks.affineBetween(inputFrame, outputFrame, operator.inverse)
    ProviderMapBinding.affine(pullback, inverse, operator)

  def dense(
    pullback: SpatialPullback,
    inversePullback: Option[SpatialPullback] = None
  ): Either[SpatialError, CoordinateMap] =
    ProviderMapBinding.dense(pullback, inversePullback).map(CoordinateMap.Geometric.apply)

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
      binding <- ProviderMapBinding.composed(checked, checkedInverse)
    yield CoordinateMap.Geometric(binding)

  def attachInverse(
      forward: CoordinateMap,
      inverse: CoordinateMap
  ): Either[SpatialError, CoordinateMap] =
    (forward, inverse) match
      case (CoordinateMap.Geometric(primary), CoordinateMap.Geometric(reverse)) =>
        ProviderMapBinding
          .attachInverse(primary, reverse)
          .map(CoordinateMap.Geometric.apply)
      case _ =>
        Left(SpatialError.InvalidProviderCoordinateMap("forward and inverse must both be provider geometric maps"))

  def volumeSamples(plan: VolumeSurfaceSamplingPlan): CoordinateMap =
    CoordinateMap.VolumeSamples(plan)

  def surfaceVertices(mapping: SurfaceVertexMapping): CoordinateMap =
    CoordinateMap.SurfaceVertices(mapping)

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
      case SamplingGeometry.Volume(space, _) => Right(space.grid.frame)
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

  private def hashSurfaceGeometry(seed: Int, geometry: SurfaceGeometry): Int =
    var hash = MurmurHash3.mix(seed, geometry.hemisphere.hashCode)
    hash = MurmurHash3.mix(hash, geometry.kind.hashCode)
    hash = hashDoubles(hash, geometry.mesh.coordinates)
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

  private[spatial] def numericFingerprint(prefix: String, values: Array[Double]): String =
    val hash = hashDoubles(MurmurHash3.stringHash(prefix), values)
    s"$prefix:${java.lang.Integer.toHexString(MurmurHash3.finalizeHash(hash, values.length))}"

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
  def reversed: Either[SpatialError, Morphism] =
    if plugin.nonEmpty || !inverse.isGeometric then Left(SpatialError.NonInvertibleMorphism(id))
    else
      val inverseId = MorphismId.unsafe(s"${id.value}:inverse")
      val penalty = 1.0 - inverse.quality
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
        case _ => Right(())
      geometryValidation.flatMap(_ => MorphismPlugin.validateDomains(morphism, source, target))

  private def validateProviderMapDomains(
      pullback: SpatialPullback,
      source: Domain,
      target: Domain
  ): Either[SpatialError, Unit] =
    (source.geometry, target.geometry) match
      case (
            SamplingGeometry.Volume(sourceSpace, _),
            SamplingGeometry.Volume(targetSpace, _)
          ) =>
        for
          _ <- SpatialMap
            .validateSourceFrame(targetSpace.grid.frame, pullback.source)
            .left
            .map(SpatialError.ProviderMap.apply)
          _ <- SpatialMap
            .validateResultFrame(sourceSpace.grid.frame, pullback.target)
            .left
            .map(SpatialError.ProviderMap.apply)
        yield ()
      case _ =>
        Left(
          SpatialError.CoordinateTransformFailed(
            "provider spatial maps require volume source and target domains"
          )
        )

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
