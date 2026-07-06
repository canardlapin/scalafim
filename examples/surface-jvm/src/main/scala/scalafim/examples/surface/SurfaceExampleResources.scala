package scalafim.examples.surface

import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.StandardCopyOption

object SurfaceExampleResources:
  def freeSurferAsciiPath(): Path =
    resourcePath("/surface/mini_lh_smoothwm.asc", "mini_lh_smoothwm", ".asc")

  def giftiPath(): Path =
    resourcePath("/surface/tetra_lh_midthickness.surf.gii", "tetra_lh_midthickness", ".surf.gii")

  private def resourcePath(name: String, prefix: String, suffix: String): Path =
    val input =
      Option(getClass.getResourceAsStream(name))
        .getOrElse(throw new IllegalStateException(s"missing bundled resource: $name"))
    val path = Files.createTempFile(prefix, suffix)
    try Files.copy(input, path, StandardCopyOption.REPLACE_EXISTING)
    finally input.close()
    path.toFile.deleteOnExit()
    path
