package scalafim.spatial

import image4s.SampleSpace
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Grid
import scalafim.image.{SampleSpaces, SomeMaskVolume, SomeSampleSpace}
import scalafim.image.SampleSpaces.*
import scalafim.image.SomeNeuroVolume.*
import scalafim.locus.{
  DomainFactory,
  FiniteSpace,
  Region as LocusRegion,
  Selection as LocusSelection,
  SpaceKey,
  SpaceMismatch,
  mismatch
}
import scalafim.surface.{Hemisphere, SurfaceGeometry, SurfaceKind, SurfaceRoi}

enum DomainKind:
  case Volume, Surface, Hybrid, Latent

enum TemplateKind:
  case Volume, Surface, Hybrid

  def domainKind: DomainKind =
    this match
      case TemplateKind.Volume => DomainKind.Volume
      case TemplateKind.Surface => DomainKind.Surface
      case TemplateKind.Hybrid => DomainKind.Hybrid

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

  def domainKind: DomainKind =
    this match
      case SpaceRef.Volume(_, _, _) => DomainKind.Volume
      case SpaceRef.Surface(_, _, _) => DomainKind.Surface
      case SpaceRef.Template(_, _, kind) => kind.domainKind
      case SpaceRef.Latent(_, _, _) => DomainKind.Latent

object SpaceRef:
  def latent(
    dim: Int,
    basis: Option[BasisName] = None,
    support: Option[DomainId] = None
  ): Either[SpatialError, SpaceRef] =
    if dim <= 0 then Left(SpatialError.NonPositiveDimension("latent", dim))
    else Right(SpaceRef.Latent(dim, basis, support))

sealed trait SamplingGeometry:
  def kind: DomainKind =
    this match
      case _: SamplingGeometry.Volume => DomainKind.Volume
      case SamplingGeometry.Surface(_, _) => DomainKind.Surface
      case SamplingGeometry.Hybrid(_) => DomainKind.Hybrid
      case SamplingGeometry.Latent(_) => DomainKind.Latent

  def nElements: Int =
    this match
      case volume: SamplingGeometry.Volume =>
        volume.space.spatialDims.product
      case SamplingGeometry.Surface(geometry, _) =>
        geometry.vertexCount
      case SamplingGeometry.Hybrid(parts) =>
        parts.map(_.nElements).sum
      case SamplingGeometry.Latent(dim) =>
        dim

object SamplingGeometry:
  final class Volume private[SamplingGeometry] (
      val space: SampleSpace[? <: Frame[D3], D3],
      val mask: Option[SomeMaskVolume]
  ) extends SamplingGeometry:
    override def equals(other: Any): Boolean =
      other match
        case that: Volume =>
          space == that.space && mask == that.mask
        case _ =>
          false

    override def hashCode(): Int =
      31 * space.hashCode() + mask.hashCode()

    override def toString: String =
      s"Volume($space,$mask)"

  object Volume:
    private[SamplingGeometry] def admitted(
        space: SampleSpace[? <: Frame[D3], D3],
        mask: Option[SomeMaskVolume]
    ): Volume =
      new Volume(space, mask)

    def unapply(
        geometry: SamplingGeometry
    ): Option[(SampleSpace[? <: Frame[D3], D3], Option[SomeMaskVolume])] =
      geometry match
        case volume: Volume =>
          Some((volume.space, volume.mask))
        case _ =>
          None

  final case class Surface(
      geometry: SurfaceGeometry,
      mask: Option[SurfaceRoi[Boolean]]
  ) extends SamplingGeometry:
    mask.foreach: value =>
      require(
        geometry.mesh.hasSameTopology(value.geometry.mesh),
        "surface mask geometry mismatch"
      )

  final case class Hybrid(parts: Vector[DomainPart]) extends SamplingGeometry:
    require(parts.nonEmpty, "hybrid geometry must contain at least one part")
    require(parts.map(_.name.value).distinct.length == parts.length, "hybrid part names must be unique")

  final case class Latent(dim: Int) extends SamplingGeometry:
    require(dim > 0, "latent geometry dimension must be positive")

  def volume(space: SomeSampleSpace, mask: Option[SomeMaskVolume] = None): Either[SpatialError, SamplingGeometry] =
    for
      admitted <- SampleSpaces
        .requireSpatialD3(space)
        .left
        .map(SpatialError.SampleSpaceAdmission.apply)
      _ <- mask match
        case Some(value) =>
          Grid
            .exactCongruence(admitted.grid, value.grid)
            .left
            .map(SpatialError.Geometry.apply)
            .map(_ => ())
        case None =>
          Right(())
    yield SamplingGeometry.Volume.admitted(admitted, mask)

  def surface(
    geometry: SurfaceGeometry,
    mask: Option[SurfaceRoi[Boolean]] = None
  ): Either[SpatialError, SamplingGeometry] =
    mask match
      case Some(m) if !geometry.mesh.hasSameTopology(m.geometry.mesh) =>
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

  def latent(dim: Int): Either[SpatialError, SamplingGeometry] =
    if dim <= 0 then Left(SpatialError.NonPositiveDimension("latent geometry", dim))
    else Right(SamplingGeometry.Latent(dim))

final case class Domain private (
  id: DomainId,
  space: SpaceRef,
  geometry: SamplingGeometry
):
  def kind: DomainKind =
    space.domainKind

  def nElements: Int =
    geometry.nElements

  lazy val locus: DomainLocus =
    DomainLocus.make(id, nElements)

object Domain:
  def build(
    id: DomainId,
    space: SpaceRef,
    geometry: SamplingGeometry
  ): Either[SpatialError, Domain] =
    if geometry.nElements <= 0 then Left(SpatialError.NonPositiveDimension("domain elements", geometry.nElements))
    else validateKind(id, space, geometry).map(_ => new Domain(id, space, geometry))

  private[scalafim] def unsafe(id: DomainId, space: SpaceRef, geometry: SamplingGeometry): Domain =
    new Domain(id, space, geometry)

  private def validateKind(id: DomainId, space: SpaceRef, geometry: SamplingGeometry): Either[SpatialError, Unit] =
    if space.domainKind != geometry.kind then
      Left(SpatialError.DomainKindMismatch(id, space.domainKind, geometry.kind))
    else
      (space, geometry) match
        case (SpaceRef.Latent(dim, _, _), SamplingGeometry.Latent(geometryDim)) if dim != geometryDim =>
          Left(SpatialError.LatentDimensionMismatch(id, dim, geometryDim))
        case _ =>
          Right(())

trait DomainLocus:
  type S
  val domainId: DomainId
  val space: FiniteSpace[S]

  final def regionDemand(
      region: LocusRegion[S]
  ): Either[SpaceMismatch, FieldDemand] =
    if space.sameRuntimeOwnerAs(region.space) then
      Right(FieldDemand.roi(region.ordinalsInDomainOrder.toVector))
    else
      Left(mismatch(space, region.space))

  final def selectionDemand(
      selection: LocusSelection[S]
  ): Either[SpaceMismatch, FieldDemand] =
    if space.sameRuntimeOwnerAs(selection.space) then
      Right(FieldDemand.rows(selection.ordinals.toVector))
    else
      Left(mismatch(space, selection.space))

object DomainLocus:
  private[spatial] def make(
      requestedId: DomainId,
      size: Int
  ): DomainLocus =
    val resolution =
      DomainFactory.unsafeRestore(
        // `size` belongs in the key: a domain key must determine the domain it
        // names, and the registry now canonicalizes on it.
        SpaceKey.unsafe(s"scalafim:spatial:${requestedId.value}:$size"),
        size
      )
    new DomainLocus:
      type S = resolution.S
      val domainId: DomainId = requestedId
      val space: FiniteSpace[S] = resolution.space

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
