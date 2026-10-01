package scalafim.surface.reference

import scalafim.surface.*
import PointMapFixtures.*

class DisplacementBridgeSuite extends munit.FunSuite:
  private val a = TemplateId.unsafe("MNI152NLin2009cAsym")
  private val b = TemplateId.unsafe("MNI152NLin6Asym")
  private val release = TemplateRelease.unsafe("templateflow@synthetic")
  private val dims = Vector(6, 6, 6)
  private val grid = affine(2, 0, 0, -5, 0, 2, 0, -5, 0, 0, 2, -5, 0, 0, 0, 1)
  private val pointMap = DeclaredPointMap.unsafeAssumeVerified(a, b, Some(release),
    PointMap.make(Vector(PointMapStage.DisplacementStage(field(dims, grid)(_ => Vector(1.25, -0.75, 0.5))))).toOption.get)

  test("a forward point-map bridge follows the declared map frames"):
    val bridge = FrameBridge.displacement(pointMap, PointMapUse.Forward).toOption.get
    assertEquals((bridge.from.template, bridge.to.template), (a, b))
    assert(bridge.display.contains("forward"))

  test("inverse bridge admission is refused until reframe4s supplies pointwise inversion"):
    val inverse = PointMapUse.Inverse(InversePolicy.make(1e-10, 50).toOption.get)
    assert(FrameBridge.displacement(pointMap, inverse).isLeft)
