package scalafim.spatial

import scalafim.image.{NeuroSpace, NeuroVol}
import scalafim.surface.{Hemisphere, SurfaceGeometry, SurfaceKind, SurfaceRoi}

enum TemplateKind:
  case Volume, Surface, Hybrid

enum SpaceRef:
  case Volume(subject: SubjectId, session: Option[SessionId], modality: Modality)
  case Surface(subject: SubjectId, hemisphere: Hemisphere, kind: SurfaceKind)
  case Template(name: TemplateName, resolution: Option[Resolution], kind: TemplateKind)
  case Latent(dim: Int, basis: Option[BasisName], support: Option[DomainId])

  this match
    case SpaceRef.Latent(dim, _, _) =>
      require(dim > 0, "latent dimension must be positive")
    case _ =>
      ()

object SpaceRef:
  def latent(
    dim: Int,
    basis: Option[BasisName] = None,
    support: Option[DomainId] = None
  ): Either[SpatialError, SpaceRef] =
    if dim <= 0 then Left(SpatialError.NonPositiveDimension("latent", dim))
    else Right(SpaceRef.Latent(dim, basis, support))

enum SamplingGeometry:
  case Volume(space: NeuroSpace, mask: Option[NeuroVol[Boolean]])
  case Surface(geometry: SurfaceGeometry, mask: Option[SurfaceRoi[Boolean]])
  case Hybrid(parts: Vector[DomainPart])

  this match
    case SamplingGeometry.Volume(space, Some(mask)) =>
      require(mask.space.spatialDims == space.spatialDims && mask.space.trans == space.trans, "volume mask geometry mismatch")
    case SamplingGeometry.Surface(geometry, Some(mask)) =>
      require(mask.geometry.vertexCount == geometry.vertexCount, "surface mask geometry mismatch")
    case SamplingGeometry.Hybrid(parts) =>
      require(parts.nonEmpty, "hybrid geometry must contain at least one part")
      require(parts.map(_.name.value).distinct.length == parts.length, "hybrid part names must be unique")
    case _ =>
      ()

  def nElements: Int =
    this match
      case SamplingGeometry.Volume(space, _) =>
        space.spatialDims.product
      case SamplingGeometry.Surface(geometry, _) =>
        geometry.vertexCount
      case SamplingGeometry.Hybrid(parts) =>
        parts.map(_.nElements).sum

object SamplingGeometry:
  def volume(space: NeuroSpace, mask: Option[NeuroVol[Boolean]] = None): Either[SpatialError, SamplingGeometry] =
    mask match
      case Some(m) if m.space.spatialDims != space.spatialDims || m.space.trans != space.trans =>
        Left(SpatialError.MaskSpaceMismatch("volume"))
      case _ =>
        Right(SamplingGeometry.Volume(space.spatialSpace, mask))

  def surface(
    geometry: SurfaceGeometry,
    mask: Option[SurfaceRoi[Boolean]] = None
  ): Either[SpatialError, SamplingGeometry] =
    mask match
      case Some(m) if m.geometry.vertexCount != geometry.vertexCount =>
        Left(SpatialError.MaskSpaceMismatch("surface"))
      case _ =>
        Right(SamplingGeometry.Surface(geometry, mask))

  def hybrid(parts: Vector[(PartName, Domain)]): Either[SpatialError, SamplingGeometry] =
    if parts.isEmpty then Left(SpatialError.EmptyHybrid)
    else
      val seen = scala.collection.mutable.HashSet.empty[String]
      val out = Vector.newBuilder[DomainPart]
      out.sizeHint(parts.length)
      var offset = 0
      var i = 0
      while i < parts.length do
        val (name, domain) = parts(i)
        if seen.contains(name.value) then return Left(SpatialError.DuplicatePartName(name))
        seen += name.value
        out += DomainPart.unsafe(name, domain, offset)
        offset += domain.nElements
        i += 1
      Right(SamplingGeometry.Hybrid(out.result()))

final case class Domain private (
  id: DomainId,
  space: SpaceRef,
  geometry: SamplingGeometry
):
  def nElements: Int =
    geometry.nElements

object Domain:
  def build(
    id: DomainId,
    space: SpaceRef,
    geometry: SamplingGeometry
  ): Either[SpatialError, Domain] =
    if geometry.nElements <= 0 then Left(SpatialError.NonPositiveDimension("domain elements", geometry.nElements))
    else Right(new Domain(id, space, geometry))

  private[scalafim] def unsafe(id: DomainId, space: SpaceRef, geometry: SamplingGeometry): Domain =
    new Domain(id, space, geometry)

final case class DomainPart private (
  name: PartName,
  domain: Domain,
  offset: Int
):
  def nElements: Int =
    domain.nElements

  def endExclusive: Int =
    offset + nElements

object DomainPart:
  def build(name: PartName, domain: Domain, offset: Int): Either[SpatialError, DomainPart] =
    if offset < 0 then Left(SpatialError.NegativeOffset(name, offset))
    else Right(new DomainPart(name, domain, offset))

  private[scalafim] def unsafe(name: PartName, domain: Domain, offset: Int): DomainPart =
    new DomainPart(name, domain, offset)
