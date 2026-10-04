package scalafim.surface.reference

import scalafim.image.WorldPoint
import PointMapFixtures.*

class PointMapSuite extends munit.FunSuite:
  private val dims = Vector(6, 5, 4)
  private val grid = affine(2.0, 0.3, 0.0, -5.0, 0.0, 1.5, 0.2, -4.0, 0.1, 0.0, 2.5, -3.0, 0, 0, 0, 1)

  private def at(i: Double, j: Double, k: Double): WorldPoint =
    val w = grid(Vector(i, j, k)).toOption.get
    WorldPoint(w(0), w(1), w(2))

  private def mapOf(stages: PointMapStage*): PointMap = PointMap.make(stages.toVector).toOption.get

  private def mapped(outcome: PointMapOutcome): WorldPoint = outcome match
    case PointMapOutcome.Mapped(point) => point
    case other => fail(s"expected Mapped; got $other")

  private def assertPoint(actual: WorldPoint, expected: Vector[Double], tol: Double): Unit =
    actual.toVector.zip(expected).foreach: (a, e) =>
      assertEqualsDouble(a, e, tol)

  test("a constant RAS displacement translates points inside its support"):
    val c = Vector(0.7, -0.4, 1.1)
    val map = mapOf(PointMapStage.DisplacementStage(field(dims, grid)(_ => c)))
    val x = at(2.3, 1.7, 1.2)
    assertPoint(mapped(map.forward(x)), x.toVector.zip(c).map(_ + _), 1e-12)

  test("ITK half-voxel support preserves constant edge displacement at the original point"):
    val simple = affine(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)
    val values = new Array[Double](3 * 8)
    values(0) = 2.0
    values(1) = 6.0
    val field = DisplacementField.make(Vector(2, 2, 2), simple, values).toOption.get
    val out = new Array[Double](3)
    assert(field.displacementInto(-0.25, 0.0, 0.0, out))
    assertEqualsDouble(out(0), 2.0, 1e-12)
    val map = mapOf(PointMapStage.DisplacementStage(field))
    assertPoint(mapped(map.forward(WorldPoint(-0.25, 0.0, 0.0))), Vector(1.75, 0.0, 0.0), 1e-12)

  test("outside raw forwardInto keeps zero displacement while typed forward refuses support"):
    val map = mapOf(PointMapStage.DisplacementStage(field(dims, grid)(_ => Vector(1.0, 2.0, 3.0))))
    val far = WorldPoint(1000.0, 0.0, 0.0)
    val out = new Array[Double](3)
    assert(!map.forwardInto(far.x, far.y, far.z, out))
    assertEquals(out.toVector, far.toVector)
    assertEquals(map.forward(far), PointMapOutcome.OutsideSupport)

  test("an oblique singleton axis retains edge support without admitting the padded region"):
    val offset = Vector(0.7, -0.4, 1.1)
    val map = mapOf(PointMapStage.DisplacementStage(field(Vector(1, 2, 3), grid)(_ => offset)))
    for index <- Vector(Vector(0.0, 0.0, 0.0), Vector(0.25, 1.0, 2.0), Vector(-0.49, 1.49, 2.49)) do
      val point = at(index(0), index(1), index(2))
      assertPoint(mapped(map.forward(point)), point.toVector.zip(offset).map(_ + _), 1e-12)
    assertEquals(map.forward(at(-0.51, 0.5, 1.0)), PointMapOutcome.OutsideSupport)

  test("provider composition preserves declared first-to-last stage order"):
    val scale = affine(2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 1)
    val offset = Vector(0.4, -0.3, 0.2)
    val x = at(2.0, 2.0, 1.5)
    val map = mapOf(PointMapStage.DisplacementStage(field(dims, grid)(_ => offset)), PointMapStage.AffineStage(scale))
    assertPoint(mapped(map.forward(x)), x.toVector.zip(offset).map(_ + _).map(_ * 2.0), 1e-12)

  test("a pointwise inverse undoes a constant displacement then scale, with per-query evidence"):
    val scale = affine(2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 1)
    val offset = Vector(0.4, -0.3, 0.2)
    val map = mapOf(PointMapStage.DisplacementStage(field(dims, grid)(_ => offset)), PointMapStage.AffineStage(scale))
    val x = at(2.0, 2.0, 1.5)
    val y = mapped(map.forward(x))
    map.inverter(InversePolicy.Default).toOption.get.place(y) match
      case PointMapOutcome.Inverted(p, residual, iterations) =>
        assertPoint(p, x.toVector, 1e-8)
        assert(residual <= InversePolicy.Default.toleranceMm)
        assertEquals(iterations, 1, "after the affine is removed, a constant displacement needs exactly one update")
      case other => fail(s"expected an inverted point, got $other")

  test("an unconverged inverse is unsolved with its failure kind and never placed"):
    val map = mapOf(PointMapStage.DisplacementStage(field(dims, grid)(p => Vector(0.3 * math.sin(p(1)), 0.2, 0.0))))
    val y = mapped(map.forward(at(2.0, 2.0, 1.5)))
    val outcome = map.inverter(InversePolicy.make(1e-14, 1).toOption.get).toOption.get.place(y)
    assertEquals(outcome.placed, None)
    outcome match
      case PointMapOutcome.InverseUnsolved(InverseFailure.MaxIterations, 1, Some(_)) => ()
      case other => fail(s"expected an iteration-limited query, got $other")
    val outside = map.inverter(InversePolicy.Default).toOption.get.place(WorldPoint(500.0, 0.0, 0.0))
    assertEquals(outside.asInstanceOf[PointMapOutcome.InverseUnsolved].failure, InverseFailure.LeftSupport)

  test("pointwise inverses are refused for stage orders the provider cannot remove analytically"):
    val d = PointMapStage.DisplacementStage(field(dims, grid)(_ => Vector(0.1, 0.0, 0.0)))
    val a = PointMapStage.AffineStage(affine(1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1))
    for stages <- Vector(Vector(a, d), Vector(d, d), Vector(a)) do
      assert(PointMap.make(stages).toOption.get.inverter(InversePolicy.Default).isLeft, stages.toString)
    assert(mapOf(d, a, a).inverter(InversePolicy.Default).isRight)

  test("several trailing affines are removed in declared order (non-commuting scale then translation)"):
    val scale = affine(2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 1)
    val shift = affine(1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)
    val map = mapOf(PointMapStage.DisplacementStage(field(dims, grid)(p => Vector(0.2 * math.sin(p(2)), 0.1, -0.05))),
      PointMapStage.AffineStage(scale), PointMapStage.AffineStage(shift))
    val x = at(2.0, 2.0, 1.5)
    val y = mapped(map.forward(x))
    // Reversing the composition (shift then scale) would place x off by 0.5 mm along x.
    map.inverter(InversePolicy.Default).toOption.get.place(y) match
      case PointMapOutcome.Inverted(p, _, _) => assertPoint(p, x.toVector, 1e-8)
      case other => fail(s"expected an inverted point, got $other")

  test("fields, stages and policies validate their inputs"):
    assert(DisplacementField.make(Vector(2, 2), grid, new Array[Double](12)).isLeft)
    assert(DisplacementField.make(dims, grid, new Array[Double](7)).isLeft)
    val bad = new Array[Double](3 * dims.product)
    bad(5) = Double.NaN
    assert(DisplacementField.make(dims, grid, bad).isLeft)
    assert(PointMap.make(Vector.empty).isLeft)
    assert(InversePolicy.make(-1.0, 10).isLeft)
    assert(InversePolicy.make(0.0, 10).isLeft)
    assert(InversePolicy.make(1e-6, 0).isLeft)
    assert(InversePolicy.make(1e-6, 10, divergenceRatio = 1.0).isLeft)
