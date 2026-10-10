package scalafim.atlas.reference

import scalafim.atlas.{Fslr32kFrom2009c, Fslr32kRoute}
import scalafim.atlas.io.StandardSurfaceRouteFiles
import scalafim.surface.CorticalHemisphere
import scalafim.surface.reference.RealAssets

import java.nio.file.{Files, Path}

/** The qualified fsLR 32k route as consumers obtain it: through the public entry point, from the same TemplateFlow
  * home the real-asset gate checks (`$TEMPLATEFLOW_HOME` or `~/.cache/templateflow`), under the frozen policy.
  * Loaded once per JVM.
  */
object PublicFslrRoute:
  val roots: Vector[Path] = Vector(RealAssets.root)

  /** Every locked route input is present as a non-empty file. */
  def present: Boolean =
    Fslr32kFrom2009c.requiredPaths.forall: relative =>
      val path = RealAssets.root.resolve(relative)
      Files.isRegularFile(path) && Files.size(path) > 0L

  lazy val route: Fslr32kRoute =
    StandardSurfaceRouteFiles.fsLR32kFrom2009c(roots = roots).fold(r => throw new IllegalStateException(r.message), identity)

  /** The hemisphere label the oracle and the TemplateFlow file names use. */
  def label(hemisphere: CorticalHemisphere): String =
    hemisphere match
      case CorticalHemisphere.Left => "L"
      case CorticalHemisphere.Right => "R"
