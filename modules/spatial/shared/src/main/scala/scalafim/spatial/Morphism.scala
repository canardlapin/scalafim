package scalafim.spatial

import scalafim.image.{Affine, DMat, SpatialPoint}

enum MorphismKind:
  case Identity, Affine3D, Warp3D, VolumeToSurface, SurfaceToSurface, Functional, Filter

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
    transform(point.toVector).map(point => SpatialPoint.unsafeFromVector(point, "transformed point"))

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
