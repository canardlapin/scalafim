package scalafim.atlas

import scalafim.surface.{
  CorticalHemisphere,
  SphereRegistration,
  SurfaceGeometry,
  SurfaceResampling,
  TemplateMesh,
  TemplateResampling,
  TemplateResamplingPlan,
  TemplateSphere
}

/** Vertex-bearing geometry for a transform graph's template surface domains: one hemisphere's template spheres, all on
  * one registration sphere.
  *
  * A surface space with a sphere here becomes a sampled domain whose vertices are that sphere's, and a sphere-resampling
  * step between two such spaces carries a [[TemplateResamplingPlan]], so routes over it compile to vertex operators. A
  * space without a sphere (its asset was not loaded) keeps its unsampled domain, and steps touching it stay typed
  * non-executable edges. Only spaces a transform manifest names can be sampled: fsaverage (fsaverage7), fsaverage5,
  * fsaverage6 and fsLR_32k.
  */
sealed trait TemplateSurfaceSampling:
  /** The registration sphere every sampled space is placed on. */
  type R <: SphereRegistration

  def hemisphere: Option[CorticalHemisphere]

  def registration: Option[SphereRegistration]

  protected def spheres: Map[AnySpaceId, TemplateSphere[R]]

  /** The sampled spaces, in space-id order. */
  final def spaces: Vector[AnySpaceId] = spheres.keys.toVector.sortBy(_.value)

  final def isEmpty: Boolean = spheres.isEmpty

  /** The sphere geometry the domain of `space` is sampled on, if it has one. */
  final def geometry(space: AnySpaceId): Option[SurfaceGeometry] =
    spheres.get(SpaceId.normalize(space)).map(_.geometry)

  /** What determines `space`'s vertex coordinates (the sphere asset's identity), if it is sampled. */
  final def identity(space: AnySpaceId): Option[String] =
    spheres.get(SpaceId.normalize(space)).map(_.identity)

  /** Plan resampling `from` vertex data onto `to` vertices when both spaces are sampled; `None` otherwise. */
  private[atlas] final def plan(
      from: AnySpaceId,
      to: AnySpaceId,
      method: SurfaceResampling.Method
  ): Option[Either[AtlasError, TemplateResamplingPlan]] =
    for
      source <- spheres.get(SpaceId.normalize(from))
      target <- spheres.get(SpaceId.normalize(to))
    yield TemplateResampling.plan(source, target, method).left.map(AtlasError.TemplateSurface.apply)

object TemplateSurfaceSampling:
  /** No sampled spaces: every template surface domain stays unsampled. */
  val none: TemplateSurfaceSampling =
    new TemplateSurfaceSampling:
      type R = SphereRegistration
      val hemisphere: Option[CorticalHemisphere] = None
      val registration: Option[SphereRegistration] = None
      protected val spheres: Map[AnySpaceId, TemplateSphere[SphereRegistration]] = Map.empty

  /** The transform-manifest space a template mesh samples, if a manifest names one. */
  def space(mesh: TemplateMesh): Option[AnySpaceId] =
    mesh match
      case TemplateMesh.FsAverage7                      => Some(SpaceId.FsAverage)
      case TemplateMesh.FsAverage6                      => Some(SpaceId.FsAverage6)
      case TemplateMesh.FsAverage5                      => Some(SpaceId.FsAverage5)
      case TemplateMesh.FsLR32k                         => Some(SpaceId.FsLR32k)
      case TemplateMesh.FsLR59k | TemplateMesh.FsLR164k => None

  /** Sample the spaces of `spheres`: all of `hemisphere`, all on `registration`, at most one sphere per space. An empty
    * `spheres` (no assets loaded) leaves every domain unsampled.
    */
  def on[Reg <: SphereRegistration & Singleton](
      registration: Reg,
      hemisphere: CorticalHemisphere,
      spheres: Vector[TemplateSphere[Reg]]
  ): Either[AtlasError, TemplateSurfaceSampling] =
    val refusals =
      spheres.flatMap: sphere =>
        val display = sphere.surface.display
        Vector(
          Option.when(sphere.registration != registration)(s"$display is on ${sphere.registration}, not $registration"),
          Option.when(sphere.surface.hemisphere != hemisphere)(s"$display is not hemisphere ${hemisphere.code}"),
          Option.when(space(sphere.surface.mesh).isEmpty)(s"${sphere.surface.mesh.label} is not a transform-manifest space")
        ).flatten
    val keyed = spheres.flatMap(sphere => space(sphere.surface.mesh).map(_ -> sphere))
    val duplicates = keyed.groupBy(_._1).collect { case (id, entries) if entries.size > 1 => id.value }.toVector.sorted
    val problems = refusals ++ duplicates.map(id => s"more than one sphere samples $id")
    if problems.nonEmpty then Left(AtlasError.InvalidSurfaceSampling(problems.mkString("; ")))
    else
      val (side, sphere) = (hemisphere, registration)
      Right(
        new TemplateSurfaceSampling:
          type R = Reg
          val hemisphere: Option[CorticalHemisphere] = Some(side)
          val registration: Option[SphereRegistration] = Some(sphere)
          protected val spheres: Map[AnySpaceId, TemplateSphere[Reg]] = keyed.toMap
      )
