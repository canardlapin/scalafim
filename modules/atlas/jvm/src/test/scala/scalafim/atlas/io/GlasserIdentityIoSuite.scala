package scalafim.atlas.io

import scalafim.atlas.*

class GlasserIdentityIoSuite extends munit.FunSuite:
  test("xcpEngine node names normalize to hemisphere-qualified surface keys"):
    val parsed = GlasserLoader.parseLabelsEither("Right_V1\nRight_p24\nLeft_V1\nLeft_p24\n")
      .fold(error => fail(error.message), identity)
    assertEquals(parsed.map(_.id.value), Vector(1, 2, 3, 4))
    assertEquals(parsed.map(_.fullLabel.value), Vector("R_V1_ROI", "R_p24_ROI", "L_V1_ROI", "L_p24_ROI"))
    assertEquals(parsed.map(_.hemisphere), Vector(Some(Hemisphere.Right), Some(Hemisphere.Right), Some(Hemisphere.Left), Some(Hemisphere.Left)))
    assertEquals(parsed.map(_.attributes.toMap("source_label")), Vector("Right_V1", "Right_p24", "Left_V1", "Left_p24"))

  test("complete Glasser names preserve exact area spelling and reject ambiguous names"):
    val parsed = GlasserLoader.parseLabels("R_9-46d_ROI\nL_7Pm_ROI\n")
    assertEquals(parsed.map(_.label.value), Vector("9-46d", "7Pm"))
    assertEquals(parsed.map(_.fullLabel.value), Vector("R_9-46d_ROI", "L_7Pm_ROI"))
    assert(GlasserLoader.parseLabelsEither("V1\n").isLeft)
    assert(GlasserLoader.parseLabelsEither("Left_\n").isLeft)
