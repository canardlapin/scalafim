package scalafim.spatial

import scalafim.image.{Affine, DMat, DenseFieldMorphism, SpatialPoint}
import scalafim.surface.{SurfaceGeometry, SurfaceSamplingPath, SurfaceVertexMapping, VolumeSurfaceSamplingPlan}
import ravel.NDArray as RavelArray
import ravel.Rank

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
  case Affine3D(matrix: DMat)
  case Dense3D(map: DenseCoordinateMap)
  case Composite3D(map: CompositeCoordinateMap)
  case VolumeSamples(plan: VolumeSurfaceSamplingPlan)
  case SurfaceVertices(mapping: SurfaceVertexMapping)
  case Unspecified

  this match
    case CoordinateMap.Affine3D(matrix) =>
      require(CoordinateMap.isFiniteAffine3D(matrix), "affine coordinate map must be a finite 4x4 matrix")
    case _ =>
      ()

  def transform(point: Vector[Double]): Either[SpatialError, Vector[Double]] =
    if point.length != 3 || point.exists(value => !value.isFinite) then
      Left(SpatialError.CoordinateTransformFailed("point must be finite 3D"))
    else
      this match
        case CoordinateMap.Identity =>
          Right(point)
        case CoordinateMap.Affine3D(matrix) =>
          Right(Affine.applyAffine(matrix, point))
        case CoordinateMap.Dense3D(map) =>
          Right(map.pullback.transform(point))
        case CoordinateMap.Composite3D(map) =>
          map.transform(point)
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
      case CoordinateMap.Affine3D(matrix) =>
        DMat.invert(matrix)
          .left.map(SpatialError.CoordinateTransformFailed.apply)
          .map(matrix => CoordinateMap.Affine3D(matrix))
      case CoordinateMap.Dense3D(map) =>
        map.inverted.map(CoordinateMap.Dense3D.apply)
      case CoordinateMap.Composite3D(map) =>
        map.inverted.map(CoordinateMap.Composite3D.apply)
      case CoordinateMap.VolumeSamples(_) | CoordinateMap.SurfaceVertices(_) =>
        Left(SpatialError.CoordinateTransformFailed("discrete or sampling coordinate maps have no geometric inverse"))
      case CoordinateMap.Unspecified =>
        Left(SpatialError.CoordinateTransformFailed("coordinate map is unspecified"))

  def fingerprint: String =
    this match
      case CoordinateMap.Identity =>
        "identity-v1"
      case CoordinateMap.Affine3D(matrix) =>
        CoordinateMap.numericFingerprint("affine-v1", matrix.data)
      case CoordinateMap.Dense3D(map) =>
        map.fingerprint.value
      case CoordinateMap.Composite3D(map) =>
        map.fingerprint.value
      case CoordinateMap.VolumeSamples(plan) =>
        CoordinateMap.volumeSamplesFingerprint(plan)
      case CoordinateMap.SurfaceVertices(mapping) =>
        CoordinateMap.surfaceVerticesFingerprint(mapping)
      case CoordinateMap.Unspecified =>
        "unspecified-v1"

enum DenseBoundaryPolicy:
  case PreserveQueryPoint

opaque type CoordinateMapFingerprint = String

object CoordinateMapFingerprint:
  private[spatial] def unsafe(value: String): CoordinateMapFingerprint =
    value

  extension (fingerprint: CoordinateMapFingerprint)
    def value: String =
      fingerprint

final case class DenseCoordinateMap private (
  pullback: DenseFieldMorphism,
  inversePullback: Option[DenseFieldMorphism],
  boundary: DenseBoundaryPolicy,
  fingerprint: CoordinateMapFingerprint
):
  def inverted: Either[SpatialError, DenseCoordinateMap] =
    inversePullback match
      case Some(inverse) =>
        DenseCoordinateMap.build(inverse, Some(pullback), boundary)
      case None =>
        Left(SpatialError.CoordinateTransformFailed("dense coordinate map has no supplied geometric inverse"))

object DenseCoordinateMap:
  def build(
    pullback: DenseFieldMorphism,
    inversePullback: Option[DenseFieldMorphism] = None,
    boundary: DenseBoundaryPolicy = DenseBoundaryPolicy.PreserveQueryPoint
  ): Either[SpatialError, DenseCoordinateMap] =
    inversePullback match
      case Some(inverse)
          if inverse.source.value != pullback.target.value ||
            inverse.target.value != pullback.source.value =>
        Left(
          SpatialError.InvalidDenseCoordinateMap(
            s"supplied inverse ${inverse.source.value}->${inverse.target.value} does not reverse ${pullback.source.value}->${pullback.target.value}"
          )
        )
      case _ =>
        Right(
          new DenseCoordinateMap(
            pullback,
            inversePullback,
            boundary,
            CoordinateMap.denseFingerprint(pullback, boundary)
          )
        )

final case class CompositeCoordinateMap private (
  components: Vector[CoordinateMap],
  inverseComponents: Option[Vector[CoordinateMap]],
  fingerprint: CoordinateMapFingerprint
):
  require(components.nonEmpty, "composite coordinate map must contain at least one component")

  def transform(point: Vector[Double]): Either[SpatialError, Vector[Double]] =
    var current = point
    var index = components.length - 1
    var error = Option.empty[SpatialError]
    while index >= 0 && error.isEmpty do
      components(index).transform(current) match
        case Left(err) => error = Some(err)
        case Right(next) => current = next
      index -= 1
    error.toLeft(current)

  def inverted: Either[SpatialError, CompositeCoordinateMap] =
    inverseComponents match
      case Some(explicit) =>
        CompositeCoordinateMap.build(explicit, Some(components))
      case None =>
        val out = Vector.newBuilder[CoordinateMap]
        var index = components.length - 1
        var error = Option.empty[SpatialError]
        while index >= 0 && error.isEmpty do
          components(index).inverted match
            case Left(err) => error = Some(err)
            case Right(inverse) => out += inverse
          index -= 1
        error match
          case Some(err) => Left(err)
          case None => CompositeCoordinateMap.build(out.result(), Some(components))

  def containsDense: Boolean =
    components.exists {
      case CoordinateMap.Dense3D(_) => true
      case _ => false
    }

object CompositeCoordinateMap:
  def build(
    components: Vector[CoordinateMap],
    inverseComponents: Option[Vector[CoordinateMap]] = None
  ): Either[SpatialError, CompositeCoordinateMap] =
    if components.isEmpty then
      Left(SpatialError.InvalidCompositeCoordinateMap("at least one component is required"))
    else
      invalidComponent(components) match
        case Some(index) =>
          Left(
            SpatialError.InvalidCompositeCoordinateMap(
              s"component $index is not an identity, affine, or dense 3D coordinate map"
            )
          )
        case None =>
          inverseComponents match
            case Some(inverse) if inverse.isEmpty =>
              Left(SpatialError.InvalidCompositeCoordinateMap("an explicit inverse cannot be empty"))
            case Some(inverse) =>
              invalidComponent(inverse) match
                case Some(index) =>
                  Left(
                    SpatialError.InvalidCompositeCoordinateMap(
                      s"inverse component $index is not an identity, affine, or dense 3D coordinate map"
                    )
                  )
                case None => Right(create(components, Some(inverse)))
            case None => Right(create(components, None))

  private def create(
    components: Vector[CoordinateMap],
    inverseComponents: Option[Vector[CoordinateMap]]
  ): CompositeCoordinateMap =
    new CompositeCoordinateMap(
      components,
      inverseComponents,
      CoordinateMap.compositeFingerprint(components, inverseComponents)
    )

  private def invalidComponent(components: Vector[CoordinateMap]): Option[Int] =
    components.indexWhere {
      case CoordinateMap.Identity | CoordinateMap.Affine3D(_) | CoordinateMap.Dense3D(_) => false
      case _ => true
    } match
      case -1 => None
      case index => Some(index)

object CoordinateMap:
  def affine3D(matrix: DMat): Either[SpatialError, CoordinateMap] =
    if isFiniteAffine3D(matrix) then Right(CoordinateMap.Affine3D(matrix))
    else Left(SpatialError.InvalidAffineCoordinateMap("affine"))

  def dense3D(
    pullback: DenseFieldMorphism,
    inversePullback: Option[DenseFieldMorphism] = None,
    boundary: DenseBoundaryPolicy = DenseBoundaryPolicy.PreserveQueryPoint
  ): Either[SpatialError, CoordinateMap] =
    DenseCoordinateMap.build(pullback, inversePullback, boundary).map(CoordinateMap.Dense3D.apply)

  def composite3D(
    components: Vector[CoordinateMap],
    inverseComponents: Option[Vector[CoordinateMap]] = None
  ): Either[SpatialError, CoordinateMap] =
    CompositeCoordinateMap.build(components, inverseComponents).map(CoordinateMap.Composite3D.apply)

  def volumeSamples(plan: VolumeSurfaceSamplingPlan): CoordinateMap =
    CoordinateMap.VolumeSamples(plan)

  def surfaceVertices(mapping: SurfaceVertexMapping): CoordinateMap =
    CoordinateMap.SurfaceVertices(mapping)

  private[spatial] def isFiniteAffine3D(matrix: DMat): Boolean =
    if matrix.rows != 4 || matrix.cols != 4 then false
    else
      var i = 0
      while i < matrix.data.length do
        if !matrix.data(i).isFinite then return false
        i += 1
      true

  private[spatial] def denseFingerprint(
    morphism: DenseFieldMorphism,
    boundary: DenseBoundaryPolicy
  ): CoordinateMapFingerprint =
    var hash = MurmurHash3.stringHash(
      s"dense-v1|${morphism.source.value}|${morphism.target.value}|${morphism.fieldKind}|${morphism.interpolation}|${morphism.grid.dims.mkString(",")}|$boundary"
    )
    hash = hashDoubles(hash, morphism.grid.affine.data)
    hash = hashDenseField(hash, morphism.field)
    val finalized =
      MurmurHash3.finalizeHash(
        hash,
        morphism.grid.affine.data.length + morphism.field.size
      )
    CoordinateMapFingerprint.unsafe(s"dense-v1:${java.lang.Integer.toHexString(finalized)}")

  private[spatial] def compositeFingerprint(
    components: Vector[CoordinateMap],
    inverseComponents: Option[Vector[CoordinateMap]]
  ): CoordinateMapFingerprint =
    val ordered = components.map(_.fingerprint).mkString("|")
    val inverse = inverseComponents.map(_.map(_.fingerprint).mkString("|")).getOrElse("none")
    val hash = MurmurHash3.stringHash(s"composite-v1|components=$ordered|inverse=$inverse")
    CoordinateMapFingerprint.unsafe(s"composite-v1:${java.lang.Integer.toHexString(hash)}")

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
    hashDoubles(hash, geometry.surfaceToWorld.data)

  private def samplingPathFingerprint(path: SurfaceSamplingPath): String =
    path match
      case SurfaceSamplingPath.White => "white"
      case SurfaceSamplingPath.Pial => "pial"
      case SurfaceSamplingPath.Midpoint => "midpoint"
      case SurfaceSamplingPath.FractionalThickness(fractions) =>
        s"fractions:${fractions.mkString(",")}"
      case SurfaceSamplingPath.NormalLine(offsets) =>
        s"normal:${offsets.mkString(",")}"

  private def numericFingerprint(prefix: String, values: Array[Double]): String =
    val hash = hashDoubles(MurmurHash3.stringHash(prefix), values)
    s"$prefix:${java.lang.Integer.toHexString(MurmurHash3.finalizeHash(hash, values.length))}"

  private def hashDoubles(seed: Int, values: Array[Double]): Int =
    var hash = seed
    var i = 0
    while i < values.length do
      hash = MurmurHash3.mix(hash, values(i).hashCode)
      i += 1
    hash

  private def hashDenseField(
      seed: Int,
      values: RavelArray[Double, Rank[4]]
  ): Int =
    var hash = seed
    values.foreachElement { value =>
      hash = MurmurHash3.mix(hash, value.hashCode)
    }
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
    else if !mapMatchesDomains(source, target, coordinateMap) then
      coordinateMap match
        case CoordinateMap.Dense3D(map) =>
          Left(
            SpatialError.CoordinateMapDomainMismatch(
              id,
              source,
              target,
              map.pullback.source.value,
              map.pullback.target.value
            )
          )
        case CoordinateMap.Composite3D(map) =>
          map.components.collectFirst {
            case CoordinateMap.Dense3D(dense)
                if dense.pullback.source.value != source.value || dense.pullback.target.value != target.value => dense
          } match
            case Some(dense) =>
              Left(
                SpatialError.CoordinateMapDomainMismatch(
                  id,
                  source,
                  target,
                  dense.pullback.source.value,
                  dense.pullback.target.value
                )
              )
            case None => Left(SpatialError.UnsupportedMorphismForCompilation(id, kind))
        case _ =>
          Left(SpatialError.UnsupportedMorphismForCompilation(id, kind))
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
      case (MorphismKind.Affine3D, CoordinateMap.Affine3D(_)) => true
      case (MorphismKind.Warp3D, CoordinateMap.Dense3D(_)) => true
      case (MorphismKind.Warp3D, CoordinateMap.Composite3D(_)) => true
      case (MorphismKind.Affine3D, CoordinateMap.Composite3D(map)) => !map.containsDense
      case (MorphismKind.VolumeToSurface, CoordinateMap.VolumeSamples(_)) => true
      case (MorphismKind.SurfaceToSurface, CoordinateMap.SurfaceVertices(_)) => true
      case (_, CoordinateMap.Unspecified) => true
      case _ => false

  private def mapMatchesDomains(
    source: DomainId,
    target: DomainId,
    coordinateMap: CoordinateMap
  ): Boolean =
    coordinateMap match
      case CoordinateMap.Dense3D(map) =>
        map.pullback.source.value == source.value && map.pullback.target.value == target.value
      case CoordinateMap.Composite3D(map) =>
        map.components.forall {
          case CoordinateMap.Dense3D(dense) =>
            dense.pullback.source.value == source.value && dense.pullback.target.value == target.value
          case _ => true
        }
      case _ => true

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
    var matrix = scalafim.image.DMat.eye(4)
    var sawAffine = false
    var i = path.morphisms.length - 1
    var error = Option.empty[SpatialError]
    while i >= 0 && error.isEmpty do
      val morphism = path.morphisms(i)
      morphism.coordinateMap match
        case CoordinateMap.Identity =>
          if morphism.kind != MorphismKind.Identity then
            error = Some(SpatialError.UnsupportedMorphismForCompilation(morphism.id, morphism.kind))
        case CoordinateMap.Affine3D(step) =>
          if morphism.kind != MorphismKind.Affine3D then
            error = Some(SpatialError.UnsupportedMorphismForCompilation(morphism.id, morphism.kind))
          else
            matrix = Affine.multiply(step, matrix)
            sawAffine = true
        case CoordinateMap.Dense3D(_) =>
          error = Some(SpatialError.UnsupportedMorphismForCompilation(morphism.id, morphism.kind))
        case CoordinateMap.Composite3D(_) =>
          error = Some(SpatialError.UnsupportedMorphismForCompilation(morphism.id, morphism.kind))
        case CoordinateMap.VolumeSamples(_) | CoordinateMap.SurfaceVertices(_) =>
          error = Some(SpatialError.UnsupportedMorphismForCompilation(morphism.id, morphism.kind))
        case CoordinateMap.Unspecified =>
          error = Some(SpatialError.MissingCoordinateMap(morphism.id))
      i -= 1

    error match
      case Some(err) => Left(err)
      case None =>
        if sawAffine then Right(CoordinateMap.Affine3D(matrix)) else Right(CoordinateMap.Identity)

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
