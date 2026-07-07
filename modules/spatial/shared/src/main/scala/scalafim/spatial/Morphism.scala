package scalafim.spatial

import scalafim.image.{Affine, DMat, SpatialPoint}

enum MorphismKind:
  case Identity, Affine3D, Warp3D, VolumeToSurface, SurfaceToSurface, Functional, Filter

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

enum RouteTag:
  case Identity, Anatomical, Functional

enum RoutingPolicy:
  case Shortest, Anatomical, Functional

enum CoordinateMap:
  case Identity
  case Affine3D(matrix: DMat)
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
      case CoordinateMap.Unspecified =>
        Left(SpatialError.CoordinateTransformFailed("coordinate map is unspecified"))

object CoordinateMap:
  def affine3D(matrix: DMat): Either[SpatialError, CoordinateMap] =
    if isFiniteAffine3D(matrix) then Right(CoordinateMap.Affine3D(matrix))
    else Left(SpatialError.InvalidAffineCoordinateMap("affine"))

  private[spatial] def isFiniteAffine3D(matrix: DMat): Boolean =
    if matrix.rows != 4 || matrix.cols != 4 then false
    else
      var i = 0
      while i < matrix.data.length do
        if !matrix.data(i).isFinite then return false
        i += 1
      true

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
  isInverted: Boolean
):
  def reversed: Either[SpatialError, Morphism] =
    if !inverse.isGeometric then Left(SpatialError.NonInvertibleMorphism(id))
    else
      val inverseId = MorphismId.unsafe(s"${id.value}:inverse")
      val penalty = 1.0 - inverse.quality
      val inverseMap =
        coordinateMap.inverted match
          case Right(map) => map
          case Left(_) => CoordinateMap.Unspecified
      Morphism.build(
        id = inverseId,
        source = target,
        target = source,
        kind = kind,
        routeTag = routeTag,
        cost = cost + penalty,
        inverse = inverse,
        coordinateMap = inverseMap,
        isInverted = true
      )

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
    isInverted: Boolean = false
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
      isInverted = isInverted
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
    isInverted: Boolean = false
  ): Either[SpatialError, Morphism] =
    if !cost.isFinite || cost < 0.0 then Left(SpatialError.InvalidCost(cost))
    else if kind == MorphismKind.Identity && source != target then
      Left(SpatialError.IdentityMorphismDomainMismatch(source, target))
    else if coordinateMap != CoordinateMap.Unspecified && !mapMatchesKind(kind, coordinateMap) then
      Left(SpatialError.UnsupportedMorphismForCompilation(id, kind))
    else
      inverse match
        case Inverse.Provided(_, score) if !score.isFinite || score < 0.0 || score > 1.0 =>
          Left(SpatialError.InvalidQuality("provided inverse", score))
        case Inverse.Approximate(_, score) if !score.isFinite || score < 0.0 || score > 1.0 =>
          Left(SpatialError.InvalidQuality("approximate inverse", score))
        case _ =>
          Right(new Morphism(id, source, target, kind, routeTag, cost, inverse, coordinateMap, isInverted))

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
      isInverted = false
    )

  private def mapMatchesKind(kind: MorphismKind, coordinateMap: CoordinateMap): Boolean =
    (kind, coordinateMap) match
      case (MorphismKind.Identity, CoordinateMap.Identity) => true
      case (MorphismKind.Affine3D, CoordinateMap.Affine3D(_)) => true
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
    else Right(())

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
