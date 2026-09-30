package scalafim.fmri.fit

import gale.linalg.DVec
import scalafim.fmri.model.VoxelwiseBootstrapContrast

class VoxelwiseReducedRankContrastSuite extends munit.FunSuite:
  test("contrast summaries preserve ordered definitions and reject invalid public values") {
    val forward = VoxelwiseBootstrapContrast.unsafe("difference", Vector(0 -> 1.0, 1 -> -1.0))
    val reversed = VoxelwiseBootstrapContrast.unsafe("difference", Vector(1 -> -1.0, 0 -> 1.0))
    assertNotEquals(forward, reversed)

    val summary = VoxelwiseReducedRankBootstrapContrast(
      forward,
      DVec.fromSeq(Vector(2.0, -3.0)),
      DVec.fromSeq(Vector(0.5, 0.75)),
      DVec.fromSeq(Vector(1.0, -4.0)),
      DVec.fromSeq(Vector(3.0, -2.0))
    )
    assertEquals(summary.definition, forward)
    assertEquals(summary.estimate.toSeq, Vector(2.0, -3.0))
    intercept[IllegalArgumentException] {
      VoxelwiseReducedRankBootstrapContrast(
        forward,
        DVec.fromSeq(Vector(1.0)),
        DVec.fromSeq(Vector(-0.1)),
        DVec.fromSeq(Vector(0.0)),
        DVec.fromSeq(Vector(2.0))
      )
    }
  }
