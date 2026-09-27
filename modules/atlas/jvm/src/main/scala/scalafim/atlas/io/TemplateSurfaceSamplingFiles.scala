package scalafim.atlas.io

import java.nio.file.Path

import scalafim.atlas.{AtlasError, TemplateSurfaceSampling}
import scalafim.surface.{CorticalHemisphere, SphereRegistration, TemplateMesh, TemplateSphere, TemplateSphereAssets, TemplateSurface}
import scalafim.surface.io.{TemplateFlowCache, TemplateSphereFiles}

/** Samples a transform graph's template surface domains from the TemplateFlow spheres in a local cache.
  *
  * fsaverage <-> fsLR data moves on the fsaverage sphere, so every manifest surface space (fsaverage, fsaverage5,
  * fsaverage6, fsLR_32k) is sampled on it: fsaverage's own spheres, and fsLR 32k deformed onto it (`space-fsaverage`).
  * A sphere in no cache leaves its space unsampled and is listed in [[Loaded.absent]]; a cached sphere whose bytes are
  * not the pinned asset is an error, never a silently unsampled space.
  */
object TemplateSurfaceSamplingFiles:
  /** The sampling built from whichever spheres were cached, and the TemplateFlow paths of those that were not. */
  final case class Loaded(sampling: TemplateSurfaceSampling, absent: Vector[String])

  /** The manifest spaces' meshes, in the order they are looked up. */
  val meshes: Vector[TemplateMesh] =
    TemplateMesh.values.toVector.filter(mesh => TemplateSurfaceSampling.space(mesh).nonEmpty)

  /** One hemisphere's manifest surface spaces on the fsaverage sphere, from `roots` ([[TemplateFlowCache.roots]]). */
  def onFsAverage(hemisphere: CorticalHemisphere, roots: Vector[Path] = TemplateFlowCache.roots): Either[AtlasError, Loaded] =
    val registration = SphereRegistration.FsAverage
    val surfaces = meshes.map(TemplateSurface(_, hemisphere))
    val unpublished = surfaces.filter(TemplateSphereAssets.relativePath(_, registration).isEmpty)
    val found =
      surfaces.flatMap: surface =>
        TemplateSphereAssets.relativePath(surface, registration).map(relative => relative -> TemplateFlowCache.locate(relative, roots).map(surface -> _))
    val absent = found.collect { case (relative, None) => relative }
    val loaded =
      found.collect { case (_, Some((surface, path))) => (surface, path) }.foldLeft[Either[AtlasError, Vector[TemplateSphere[SphereRegistration.FsAverage.type]]]](Right(Vector.empty)):
        case (acc, (surface, path)) =>
          acc.flatMap(spheres => TemplateSphereFiles.load(surface, SphereRegistration.FsAverage, path).left.map(AtlasError.TemplateSurface.apply).map(spheres :+ _))
    for
      _ <-
        if unpublished.isEmpty then Right(())
        else Left(AtlasError.InvalidSurfaceSampling(s"TemplateFlow publishes no fsaverage sphere for ${unpublished.map(_.display).mkString(", ")}"))
      spheres  <- loaded
      sampling <- TemplateSurfaceSampling.on(SphereRegistration.FsAverage, hemisphere, spheres)
    yield Loaded(sampling, absent)
