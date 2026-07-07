package scalafim.fmri.motion

import scala.io.Source
import scalafim.fmri.motion.fixtures.VolreggerFixtures

class VolreggerResourceSuite extends munit.FunSuite:

  test("generated volregger resource matches shared fixture mirror") {
    val source = Source.fromResource("motion/volregger_core.fixture")
    try
      val resource = source.mkString.replace("\r\n", "\n")
      assertEquals(resource.trim, VolreggerFixtures.coreFixtureText.trim)
    finally source.close()
  }
