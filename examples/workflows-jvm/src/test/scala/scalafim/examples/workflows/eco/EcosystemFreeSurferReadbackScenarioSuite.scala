package scalafim.examples.workflows.eco

import java.nio.file.Files
import neurosurf4s.surface.{Hemisphere, SurfaceKind}
import neurosurf4s.surface.io.FreeSurferSurfaceReader

/** JVM-only: the provider has no Scala.js FreeSurfer geometry reader. */
class EcosystemFreeSurferReadbackScenarioSuite extends munit.FunSuite:
  test("FreeSurfer ASCII geometry feeds scalar display and restored raster readback"):
    val path = Files.createTempFile("eco-lh-pial-", ".asc")
    val ascii = """#!ascii version in FreeSurfer format (SUMA)
3 1
-1 -1 0 0
1 -1 0 0
0 2 0 0
2 0 1 0
"""
    try
      Files.writeString(path, ascii)
      val geometry = FreeSurferSurfaceReader.readAscii(path, Hemisphere.Left, SurfaceKind.Pial)
      Files.writeString(path, ascii.replace("2 0 1 0", "2 0 3 0"))
      val rejectsInvalidFace = FreeSurferSurfaceReader.readEither(path).isLeft
      val result = SurfaceReadbackScenario.run("freesurfer-geometry-jvm", geometry,
        "FreeSurfer ASCII coordinates; tkRAS convention is source metadata, not an admitted image frame", rejectsInvalidFace)
      assert(result.ciPass, result.render)
    finally Files.deleteIfExists(path)
