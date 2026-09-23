package scalafim.surface.reference

import scalafim.surface.*

enum CorticalMeshFamily:
  case FsLR, FsAverage

  def label: String =
    this match
      case FsLR => "fsLR"
      case FsAverage => "fsaverage"

/** A standard cortical mesh family at an exact density. The per-hemisphere
  * vertex count is part of the identity, but it is not correspondence evidence
  * on its own: admission also binds ordered topology and a medial-wall mask.
  */
final case class StandardCorticalMesh private (family: CorticalMeshFamily, density: String, verticesPerHemisphere: Int):
  def display: String = s"${family.label}-$density"

object StandardCorticalMesh:
  val FsLR32k: StandardCorticalMesh = StandardCorticalMesh(CorticalMeshFamily.FsLR, "32k", 32492)

  /** Declare another exact family/density, e.g. for a reduced test mesh. */
  def declare(family: CorticalMeshFamily, density: String, verticesPerHemisphere: Int): Either[ReferenceError, StandardCorticalMesh] =
    if density.trim.isEmpty || density.trim != density then Left(ReferenceError.InvalidCorticalMesh(s"invalid density '$density'"))
    else if verticesPerHemisphere <= 0 then Left(ReferenceError.InvalidCorticalMesh("vertices per hemisphere must be positive"))
    else Right(StandardCorticalMesh(family, density, verticesPerHemisphere))

/** Cortex/medial-wall partition of one ordered hemisphere mesh. */
final class MedialWallMask private (val domain: SurfaceMeshDomain, private val cortex: Array[Boolean]):
  val cortexCount: Int = cortex.count(identity)

  def medialWallCount: Int = cortex.length - cortexCount

  /** None when the vertex is outside this domain. */
  def cortexAt(vertex: VertexId): Option[Boolean] = cortex.lift(vertex.index)

  private[reference] def isCortex(vertex: VertexId): Boolean = cortex(vertex.index)

  override def equals(other: Any): Boolean =
    other match
      case that: MedialWallMask => domain == that.domain && java.util.Arrays.equals(cortex, that.cortex)
      case _ => false

  override def hashCode(): Int = 31 * domain.hashCode + java.util.Arrays.hashCode(cortex)

object MedialWallMask:
  /** `cortex(v)` is true for cortical vertices and false on the medial wall. */
  def fromCortexFlags(domain: SurfaceMeshDomain, cortex: IndexedSeq[Boolean]): Either[ReferenceError, MedialWallMask] =
    if cortex.length != domain.vertexCount then
      Left(ReferenceError.InvalidMedialWall(s"mask has ${cortex.length} vertices; domain has ${domain.vertexCount}"))
    else if !cortex.contains(true) then Left(ReferenceError.InvalidMedialWall("mask marks no cortical vertex"))
    else Right(MedialWallMask(domain, cortex.toArray))

/** An admitted hemisphere of a standard cortical mesh: exact family/density,
  * cortical hemisphere, ordered topology and medial wall. Every anatomical or
  * display geometry used with it must match the anchor topology face by face.
  */
final class CorticalMeshReference private (
  val mesh: StandardCorticalMesh,
  val domain: SurfaceMeshDomain,
  val medialWall: MedialWallMask,
  private val anchor: SurfaceGeometry
):
  def hemisphere: CorticalHemisphere = domain.hemisphere

  def vertexCount: Int = domain.vertexCount

  def display: String = s"${mesh.display}:${domain.display}"

  private[reference] def admits(geometry: SurfaceGeometry): Boolean =
    anchor.hasSameMeshDomain(geometry)

  /** Same mesh identity, ordered domain and medial wall. */
  def sameAs(other: CorticalMeshReference): Boolean =
    (this eq other) || (mesh == other.mesh && domain == other.domain && medialWall == other.medialWall &&
      admits(other.anchor))

object CorticalMeshReference:
  def make(mesh: StandardCorticalMesh, geometry: SurfaceGeometry, medialWall: MedialWallMask): Either[ReferenceError, CorticalMeshReference] =
    geometry.meshDomainEither.left.map(error => ReferenceError.InvalidCorticalMesh(error.message)).flatMap: domain =>
      if domain.vertexCount != mesh.verticesPerHemisphere then
        Left(ReferenceError.InvalidCorticalMesh(
          s"${mesh.display} has ${mesh.verticesPerHemisphere} vertices per hemisphere; geometry has ${domain.vertexCount}"))
      else if medialWall.domain != domain then
        Left(ReferenceError.InvalidMedialWall(s"mask domain ${medialWall.domain.display} does not match ${domain.display}"))
      else Right(CorticalMeshReference(mesh, domain, medialWall, geometry))

/** Anatomical geometry that locates vertices in a volume frame. Inflated,
  * very-inflated and spherical shapes can never appear here.
  */
enum AnatomicalGeometry:
  case Midthickness(surface: SurfaceGeometry)
  case WhitePial(pair: SurfaceGeometryPair)

  def label: String =
    this match
      case Midthickness(_) => "midthickness"
      case WhitePial(_) => "white+pial"

  private[reference] def geometries: Vector[SurfaceGeometry] =
    this match
      case Midthickness(surface) => Vector(surface)
      case WhitePial(pair) => Vector(pair.white, pair.pial)

/** Anatomical sampling geometry admitted against a cortical mesh reference,
  * together with the template frame its coordinates are expressed in. The frame
  * is a declaration carried from the asset's provenance, never inferred from a
  * file name or density.
  */
final case class SamplingAnatomy private (reference: CorticalMeshReference, frame: TemplateFrame, geometry: AnatomicalGeometry)

object SamplingAnatomy:
  def make(reference: CorticalMeshReference, frame: TemplateFrame, geometry: AnatomicalGeometry): Either[ReferenceError, SamplingAnatomy] =
    val kinds = geometry match
      case AnatomicalGeometry.Midthickness(surface) => Vector(surface.kind -> SurfaceKind.Midthickness)
      case AnatomicalGeometry.WhitePial(pair) => Vector(pair.white.kind -> SurfaceKind.White, pair.pial.kind -> SurfaceKind.Pial)
    kinds.collectFirst { case (actual, expected) if actual != expected => (actual, expected) } match
      case Some((actual, expected)) =>
        Left(ReferenceError.InvalidAnatomy(s"expected ${expected.label} geometry; got ${actual.label}"))
      case None =>
        if !geometry.geometries.forall(reference.admits) then
          Left(ReferenceError.InvalidAnatomy(s"geometry does not share the ordered mesh domain ${reference.display}"))
        else Right(SamplingAnatomy(reference, frame, geometry))

enum DisplayForm:
  case Anatomical(kind: SurfaceKind)
  case Inflated, VeryInflated, Sphere

  def label: String =
    this match
      case Anatomical(kind) => kind.label
      case Inflated => "inflated"
      case VeryInflated => "veryinflated"
      case Sphere => "sphere"

object DisplayForm:
  def fromKind(kind: SurfaceKind): Either[ReferenceError, DisplayForm] =
    kind match
      case SurfaceKind.White | SurfaceKind.Pial | SurfaceKind.Midthickness | SurfaceKind.SmoothWm => Right(Anatomical(kind))
      case SurfaceKind.Inflated => Right(Inflated)
      case SurfaceKind.Sphere => Right(Sphere)
      case SurfaceKind.Custom(value) =>
        value.trim.toLowerCase.replace("_", "").replace("-", "") match
          case "veryinflated" => Right(VeryInflated)
          case _ => Left(ReferenceError.InvalidDisplay(s"unrecognized display surface kind '$value'"))

/** Display-only geometry on the same ordered vertex domain as a cortical mesh
  * reference. Display coordinates never participate in volume sampling.
  */
final case class DisplaySurface private (reference: CorticalMeshReference, geometry: SurfaceGeometry, form: DisplayForm)

object DisplaySurface:
  def make(reference: CorticalMeshReference, geometry: SurfaceGeometry): Either[ReferenceError, DisplaySurface] =
    DisplayForm.fromKind(geometry.kind).flatMap: form =>
      if reference.admits(geometry) then Right(DisplaySurface(reference, geometry, form))
      else Left(ReferenceError.InvalidDisplay(s"${form.label} geometry does not share the ordered mesh domain ${reference.display}"))
