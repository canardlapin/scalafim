package scalafim.surface.io

import java.nio.file.{Files, Path}

/** Read-only lookup of files in local TemplateFlow caches. Nothing is downloaded, and the zero-byte placeholders a
  * TemplateFlow skeleton leaves for unfetched files count as absent.
  */
object TemplateFlowCache:
  /** Searched in order: `$TEMPLATEFLOW_HOME`, `~/.cache/templateflow` (Linux default, templateflow4s) and
    * `~/Library/Caches/templateflow` (TemplateFlow's macOS default).
    */
  def roots: Vector[Path] =
    val home = Path.of(System.getProperty("user.home"))
    Option(System.getenv("TEMPLATEFLOW_HOME")).filter(_.trim.nonEmpty).map(Path.of(_)).toVector ++
      Vector(home.resolve(".cache").resolve("templateflow"), home.resolve("Library").resolve("Caches").resolve("templateflow"))

  /** The first non-empty regular file at `relativePath` (e.g. `tpl-fsLR/tpl-fsLR_hemi-L_den-32k_sphere.surf.gii`). */
  def locate(relativePath: String, searched: Vector[Path] = roots): Option[Path] =
    searched.iterator
      .map(_.resolve(relativePath))
      .find(path => Files.isRegularFile(path) && Files.size(path) > 0L)
