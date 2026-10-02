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

  test("a pointwise inverse bridge runs output to input and is disclosed as approximate"):
    val bridge = FrameBridge.displacement(pointMap, PointMapUse.Inverse(InversePolicy.Default)).toOption.get
    assertEquals((bridge.from.template, bridge.to.template), (b, a))
    assertEquals(bridge.exactness, BridgeExactness.Approximate(NumericalBridgeMethod.PointwiseFixedPoint(InversePolicy.Default)))
    assert(bridge.display.contains("pointwise inverse (approximate"), bridge.display)
    assertEquals(FrameBridge.displacement(pointMap, PointMapUse.Forward).toOption.get.exactness, BridgeExactness.Exact)

  test("a pointwise inverse bridge is refused for a map the provider cannot invert pointwise"):
    val affineFirst = DeclaredPointMap.unsafeAssumeVerified(a, b, Some(release), PointMap.make(Vector(
      PointMapStage.AffineStage(translation(1, 0, 0)),
      PointMapStage.DisplacementStage(field(dims, grid)(_ => Vector(1.25, -0.75, 0.5))))).toOption.get)
    assert(FrameBridge.displacement(affineFirst, PointMapUse.Inverse(InversePolicy.Default))
      .left.exists(_.isInstanceOf[ReferenceError.PointwiseInverseUnsupported]))
