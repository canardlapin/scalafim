package scalafim.surface

/** The registration sphere a spherical mesh's coordinates are aligned to. Vertex data moves between two meshes only
  * through a sphere both are registered on.
  */
enum SphereRegistration derives CanEqual:
  /** FreeSurfer's fsaverage (icosahedron order 7) sphere. fsaverage5 and fsaverage6 are its nested vertex prefixes. */
  case FsAverage

  /** The HCP fs_LR sphere. */
  case FsLR

enum TemplateFamily derives CanEqual:
  case FsAverage, FsLR

/** A stock template mesh, with the per-hemisphere vertex and face counts of its TemplateFlow release. */
enum TemplateMesh(val family: TemplateFamily, val label: String, val density: String, val vertices: Int, val faces: Int)
    derives CanEqual:
  case FsAverage5 extends TemplateMesh(TemplateFamily.FsAverage, "fsaverage5", "10k", 10242, 20480)
  case FsAverage6 extends TemplateMesh(TemplateFamily.FsAverage, "fsaverage6", "41k", 40962, 81920)
  case FsAverage7 extends TemplateMesh(TemplateFamily.FsAverage, "fsaverage", "164k", 163842, 327680)
  case FsLR32k extends TemplateMesh(TemplateFamily.FsLR, "fsLR_32k", "32k", 32492, 64980)
  case FsLR59k extends TemplateMesh(TemplateFamily.FsLR, "fsLR_59k", "59k", 59292, 118580)
  case FsLR164k extends TemplateMesh(TemplateFamily.FsLR, "fsLR_164k", "164k", 163842, 327680)

  /** The sphere the mesh's own family is registered on. fsLR meshes are also published deformed onto fsaverage's. */
  def nativeRegistration: SphereRegistration =
    family match
      case TemplateFamily.FsAverage => SphereRegistration.FsAverage
      case TemplateFamily.FsLR      => SphereRegistration.FsLR

/** One hemisphere of a stock template mesh: the vertex domain that per-vertex template data is indexed by, whatever
  * geometry (sphere, midthickness, inflated) displays it.
  */
final case class TemplateSurface(mesh: TemplateMesh, hemisphere: CorticalHemisphere) derives CanEqual:
  def vertexCount: Int = mesh.vertices

  def display: String = s"${mesh.label}:${hemisphere.code}"

/** A template mesh placed on the registration sphere `R`, projected to radius 100 about the origin.
  *
  * `R` is the singleton type of a [[SphereRegistration]] case, so meshes on different registration spheres cannot be
  * paired at compile time; `identity` names the asset the coordinates came from (e.g. a file digest).
  */
final class TemplateSphere[R <: SphereRegistration] private (
    val surface: TemplateSurface,
    val registration: R,
    val sphere: TriangleMesh,
    val identity: String
):
  override def toString: String = s"TemplateSphere(${surface.display} on $registration, $identity)"

object TemplateSphere:
  /** The radius every template sphere is normalised to, as Workbench and neurotransform do. */
  val Radius: Double = 100.0

  /** Admit `mesh` as `surface` on `registration`: its counts must be the template's, it must be spherical about the
    * origin, and an fsaverage mesh can only be on the fsaverage sphere.
    */
  def admit[R <: SphereRegistration & Singleton](
      surface: TemplateSurface,
      registration: R,
      mesh: TriangleMesh,
      identity: String
  ): Either[SurfaceError, TemplateSphere[R]] =
    if identity.trim.isEmpty then Left(SurfaceError.InvalidGeometry("a template sphere needs a non-empty asset identity"))
    else if mesh.vertexCount != surface.mesh.vertices || mesh.faceCount != surface.mesh.faces then
      Left(
        SurfaceError.InvalidTopology(
          s"${surface.display} has ${surface.mesh.vertices} vertices and ${surface.mesh.faces} faces, got ${mesh.vertexCount} and ${mesh.faceCount}"
        )
      )
    else if surface.mesh.family == TemplateFamily.FsAverage && registration != SphereRegistration.FsAverage then
      Left(SurfaceError.InvalidGeometry(s"${surface.display} is registered on the fsaverage sphere, not $registration"))
    else SphereMesh.withRadius(mesh, Radius).map(projected => new TemplateSphere(surface, registration, projected, identity))

/** A sparse resampling operator from `source` vertex data to `target` vertices, planned on one registration sphere. */
final class TemplateResamplingPlan private[surface] (
    val source: TemplateSurface,
    val target: TemplateSurface,
    val registration: SphereRegistration,
    val plan: SurfaceResamplingPlan,
    val identity: String
):
  /** Source values to target values. `Element` (the default) interpolates: every target row sums to one. */
  def resample(
      values: Array[Double],
      normalization: SurfaceResampling.Normalization = SurfaceResampling.Normalization.Element
  ): Either[SurfaceError, Array[Double]] =
    SurfaceResampling.apply(plan, values, inverse = false, normalization)

  /** Target values back to source vertices through the transposed weights. `None` is the exact adjoint, `Sum` keeps
    * each target value's total.
    */
  def adjoint(
      values: Array[Double],
      normalization: SurfaceResampling.Normalization = SurfaceResampling.Normalization.None
  ): Either[SurfaceError, Array[Double]] =
    SurfaceResampling.apply(plan, values, inverse = true, normalization)

object TemplateResampling:
  /** Plan resampling from `source` onto `target`, both on the registration sphere `R` and of one hemisphere. */
  def plan[R <: SphereRegistration](
      source: TemplateSphere[R],
      target: TemplateSphere[R],
      method: SurfaceResampling.Method = SurfaceResampling.Method.Barycentric
  ): Either[SurfaceError, TemplateResamplingPlan] =
    // `R` rules out mixing spheres statically, unless a caller widens it to SphereRegistration; check the values too.
    if source.registration != target.registration then
      Left(SurfaceError.InvalidGeometry(s"${source.surface.display} is on ${source.registration}, ${target.surface.display} on ${target.registration}"))
    else if source.surface.hemisphere != target.surface.hemisphere then
      Left(SurfaceError.InvalidGeometry(s"cannot resample ${source.surface.display} onto ${target.surface.display}: hemispheres differ"))
    else
      SurfaceResampling
        .plan(reference = target.sphere, moving = source.sphere, method = method, spherical = true, radius = TemplateSphere.Radius)
        .map: plan =>
          TemplateResamplingPlan(
            source.surface,
            target.surface,
            source.registration,
            plan,
            s"$method on ${source.registration}: ${source.identity} -> ${target.identity}"
          )
