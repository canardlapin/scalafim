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

  /** Published per-hemisphere vertex counts of standard (family, density) meshes. */
  private val published: Map[(CorticalMeshFamily, String), Int] = Map(
    (CorticalMeshFamily.FsLR, "32k") -> 32492,
    (CorticalMeshFamily.FsLR, "164k") -> 163842,
    (CorticalMeshFamily.FsAverage, "3k") -> 2562,
    (CorticalMeshFamily.FsAverage, "10k") -> 10242,
    (CorticalMeshFamily.FsAverage, "41k") -> 40962,
    (CorticalMeshFamily.FsAverage, "164k") -> 163842
  )

  /** Declare another exact family/density, e.g. for a reduced test mesh. A
    * published (family, density) must carry its published vertex count, so a
    * mis-declared mesh cannot display as the standard one.
    */
  def declare(family: CorticalMeshFamily, density: String, verticesPerHemisphere: Int): Either[ReferenceError, StandardCorticalMesh] =
    if density.trim.isEmpty || density.trim != density then Left(ReferenceError.InvalidCorticalMesh(s"invalid density '$density'"))
    else if verticesPerHemisphere <= 0 then Left(ReferenceError.InvalidCorticalMesh("vertices per hemisphere must be positive"))
    else
      published.get((family, density)) match
        case Some(count) if count != verticesPerHemisphere =>
          Left(ReferenceError.InvalidCorticalMesh(
            s"${family.label}-$density has $count vertices per hemisphere; declared $verticesPerHemisphere"))
        case _ => Right(StandardCorticalMesh(family, density, verticesPerHemisphere))

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
  * very-inflated and spherical shapes can never appear here. Every surface is
  * a [[DeclaredSurface]], so its frame comes only from its declaration.
  */
enum AnatomicalGeometry:
  case Midthickness(surface: DeclaredSurface)
  case WhitePial(white: DeclaredSurface, pial: DeclaredSurface)

  def label: String =
    this match
      case Midthickness(_) => "midthickness"
      case WhitePial(_, _) => "white+pial"

  def surfaces: Vector[DeclaredSurface] =
    this match
      case Midthickness(surface) => Vector(surface)
      case WhitePial(white, pial) => Vector(white, pial)

/** Anatomical sampling geometry admitted against a cortical mesh reference.
  * Its frame is the frame of its surfaces' declarations, which must agree;
  * it is never supplied separately, inferred from a file name or density, or
  * read from a GIFTI space code.
  */
final case class SamplingAnatomy private (
  reference: CorticalMeshReference,
  frame: TemplateFrame,
  geometry: AnatomicalGeometry,
  private[reference] val located: SurfaceGeometryPair
):
  def declarations: Vector[FrameDeclaration] = geometry.surfaces.map(_.declaration)

object SamplingAnatomy:
  def make(reference: CorticalMeshReference, geometry: AnatomicalGeometry): Either[ReferenceError, SamplingAnatomy] =
    val kinds = geometry match
      case AnatomicalGeometry.Midthickness(surface) => Vector(surface.geometry.kind -> SurfaceKind.Midthickness)
      case AnatomicalGeometry.WhitePial(white, pial) =>
        Vector(white.geometry.kind -> SurfaceKind.White, pial.geometry.kind -> SurfaceKind.Pial)
    val frames = geometry.surfaces.map(_.frame).distinct
    kinds.collectFirst { case (actual, expected) if actual != expected => (actual, expected) } match
      case Some((actual, expected)) =>
        Left(ReferenceError.InvalidAnatomy(s"expected ${expected.label} geometry; got ${actual.label}"))
      case None =>
        if frames.size != 1 then
          Left(ReferenceError.InvalidAnatomy(s"surfaces declare different frames: ${frames.map(_.display).mkString(", ")}"))
        else if !geometry.surfaces.forall(surface => reference.admits(surface.geometry)) then
          Left(ReferenceError.InvalidAnatomy(s"geometry does not share the ordered mesh domain ${reference.display}"))
        else
          val pair = geometry match
            case AnatomicalGeometry.Midthickness(surface) =>
              SurfaceGeometryPair.fromEither(surface.geometry, surface.geometry)
            case AnatomicalGeometry.WhitePial(white, pial) =>
              SurfaceGeometryPair.fromEither(white.geometry, pial.geometry)
          pair.left.map(error => ReferenceError.InvalidAnatomy(error.message))
            .map(located => SamplingAnatomy(reference, frames.head, geometry, located))

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
