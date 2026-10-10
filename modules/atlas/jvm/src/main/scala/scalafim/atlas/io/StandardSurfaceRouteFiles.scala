package scalafim.atlas.io

import java.nio.file.Path

import scalafim.atlas.{Fslr32kFrom2009c, Fslr32kHemisphereInput, Fslr32kRoute, StandardRoutePolicy, StandardRouteRefusal,
  StandardSurfaceRoutes}
import scalafim.surface.SurfaceKind
import scalafim.surface.io.TemplateFlowCache
import scalafim.surface.reference.{DeclaredMedialWallReader, DeclaredPointMapReader, DeclaredSurfaceReader, ReferenceError}

/** Loads standard surface routes from local TemplateFlow caches. Nothing is downloaded: the locked files must already
  * be present (the point map as templateflow4s derived it), and every byte is digest-checked before it is decoded.
  */
object StandardSurfaceRouteFiles:
  /** The qualified MNI152NLin2009cAsym -> fsLR 32k route from the first cache under `roots` holding each locked file
    * ([[TemplateFlowCache.roots]] by default), under `policy` ([[StandardRoutePolicy.Frozen]] by default). Missing
    * files are all reported together; a file whose digest or content differs from the lock is refused.
    */
  def fsLR32kFrom2009c(
      policy: StandardRoutePolicy = StandardRoutePolicy.Frozen,
      roots: Vector[Path] = TemplateFlowCache.roots
  ): Either[StandardRouteRefusal, Fslr32kRoute] =
    val lock = Fslr32kFrom2009c
    val located = lock.requiredPaths.map(relative => relative -> TemplateFlowCache.locate(relative, roots))
    val missing = located.collect { case (relative, None) => relative }
    if missing.nonEmpty then Left(StandardRouteRefusal.AssetsMissing(missing, roots.map(_.toString)))
    else
      val path = located.collect { case (relative, Some(found)) => relative -> found }.toMap
      def hemisphere(h: Fslr32kFrom2009c.HemisphereLock): Either[StandardRouteRefusal, Fslr32kHemisphereInput] =
        for
          surface <- DeclaredSurfaceReader.read(path(h.midthickness.archivePath), h.declaration, h.hemisphere.tag,
              SurfaceKind.Midthickness)
            .left.map(StandardRouteRefusal.AssetRefused(h.midthickness.archivePath, _))
          domain <- surface.geometry.meshDomainEither
            .left.map(e => StandardRouteRefusal.AssetRefused(h.midthickness.archivePath,
              ReferenceError.InvalidCorticalMesh(e.message)))
          wall <- DeclaredMedialWallReader.read(path(h.medialWall.archivePath), domain, h.medialWall)
            .left.map(StandardRouteRefusal.AssetRefused(h.medialWall.archivePath, _))
        yield Fslr32kHemisphereInput(surface, wall)
      for
        left <- hemisphere(lock.hemispheres(0))
        right <- hemisphere(lock.hemispheres(1))
        pointMap <- DeclaredPointMapReader.read(path(s"${lock.pointMapDirectory}/manifest.json").getParent,
            lock.transform.sha256, lock.pointMapManifestSha256)
          .left.map(StandardRouteRefusal.AssetRefused(lock.pointMapDirectory, _))
        route <- StandardSurfaceRoutes.fsLR32kFrom2009c(pointMap, Vector(left, right), policy)
      yield route
