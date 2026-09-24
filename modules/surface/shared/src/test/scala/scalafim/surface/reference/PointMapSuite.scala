package scalafim.surface.reference

import scalafim.image.WorldPoint
import PointMapFixtures.*

/** Analytic point maps with closed-form forward and inverse maps, and ITK's
  * displacement-field border rule, checked against construction.
  */
class PointMapSuite extends munit.FunSuite:
  private val dims = Vector(6, 5, 4)
  // Oblique, anisotropic, shifted grid.
  private val grid = affine(2.0, 0.3, 0.0, -5.0, 0.0, 1.5, 0.2, -4.0, 0.1, 0.0, 2.5, -3.0, 0, 0, 0, 1)
  private val strict = InversePolicy.make(1e-12, 100).toOption.get

  /** World point at a continuous voxel index. */
  private def at(i: Double, j: Double, k: Double): WorldPoint =
    val w = grid(Vector(i, j, k)).toOption.get
    WorldPoint(w(0), w(1), w(2))

  private def mapOf(stages: PointMapStage*): PointMap = PointMap.make(stages.toVector).toOption.get

  private def assertPoint(actual: WorldPoint, expected: Vector[Double], tol: Double, clue: String = ""): Unit =
    for (a, e) <- actual.toVector.zip(expected) do assertEqualsDouble(a, e, tol, clue)

  private def mapped(outcome: PointMapOutcome): WorldPoint =
    outcome match
      case PointMapOutcome.Mapped(p) => p
      case other => fail(s"expected Mapped; got $other")

  private def converged(outcome: PointMapOutcome): PointMapOutcome.Converged =
    outcome match
      case c: PointMapOutcome.Converged => c
      case other => fail(s"expected Converged; got $other")

  private def mul(m: Vector[Vector[Double]], v: Vector[Double]): Vector[Double] =
    Vector.tabulate(3)(r => (0 until 3).map(c => m(r)(c) * v(c)).sum)

  private def add(a: Vector[Double], b: Vector[Double]) = Vector.tabulate(3)(i => a(i) + b(i))
  private def sub(a: Vector[Double], b: Vector[Double]) = Vector.tabulate(3)(i => a(i) - b(i))

  test("a constant field translates inside its support and inverts in one update"):
    val c = Vector(0.7, -0.4, 1.1)
    val map = mapOf(PointMapStage.DisplacementStage(field(dims, grid)(_ => c)))
    val x = at(2.3, 1.7, 1.2)
    assertPoint(mapped(map.forward(x)), add(x.toVector, c), 1e-12)
    val y = converged(map.inverse(x, strict))
    assertPoint(y.point, sub(x.toVector, c), 1e-12)
    assertEquals(y.iterations, 1)

  test("a field linear in position is reproduced exactly and inverts to the closed form"):
    val a = Vector(Vector(0.04, -0.01, 0.02), Vector(0.015, -0.03, 0.01), Vector(-0.02, 0.005, 0.05))
    val b = Vector(0.3, -0.2, 0.4)
    val map = mapOf(PointMapStage.DisplacementStage(field(dims, grid)(p => add(mul(a, p), b))))
    val y = at(2.4, 2.1, 1.6)
    val x = add(add(y.toVector, mul(a, y.toVector)), b)
    assertPoint(mapped(map.forward(y)), x, 1e-12)
    val solved = converged(map.inverse(WorldPoint(x(0), x(1), x(2)), strict))
    assertPoint(solved.point, y.toVector, 1e-10)
    assert(solved.residualMm <= 1e-12)

  test("a small rotation about an interior centre inverts to the transposed rotation"):
    val theta = math.toRadians(3.0)
    val r = Vector(Vector(math.cos(theta), -math.sin(theta), 0.0), Vector(math.sin(theta), math.cos(theta), 0.0),
      Vector(0.0, 0.0, 1.0))
    val c = at(2.5, 2.0, 1.5).toVector
    val map = mapOf(PointMapStage.DisplacementStage(field(dims, grid)(p => sub(mul(r, sub(p, c)), sub(p, c)))))
    val x = at(2.2, 2.4, 1.3).toVector
    val rt = Vector.tabulate(3, 3)((i, j) => r(j)(i))
    val expected = add(c, mul(rt, sub(x, c)))
    val solved = converged(map.inverse(WorldPoint(x(0), x(1), x(2)), strict))
    assertPoint(solved.point, expected, 1e-10)
    assertPoint(mapped(map.forward(solved.point)), x, 1e-10)

  test("ITK border: -0.5 inclusive, n - 0.5 exclusive, clamped half-voxel border, zero beyond"):
    // ci = (x + 3) / 2 on every axis; component x of voxel (i, j, k) is 10 i + j + 0.1 k.
    val simple = affine(2, 0, 0, -3, 0, 2, 0, -3, 0, 0, 2, -3, 0, 0, 0, 1)
    val d = Vector(4, 3, 3)
    val values = new Array[Double](3 * 36)
    for k <- 0 until 3; j <- 0 until 3; i <- 0 until 4 do values(i + 4 * (j + 3 * k)) = 10.0 * i + j + 0.1 * k
    val f = DisplacementField.make(d, simple, values).toOption.get
    val out = new Array[Double](3)
    def dx(x: Double): Option[Double] = Option.when(f.displacementInto(x, -1.0, -1.0, out))(out(0))
    assertEquals(dx(-4.0), Some(1.1), "ci = -0.5 is inside and takes the edge value")
    assertEquals(dx(-4.0 - 1e-9), None, "just below -0.5 is outside")
    assertEquals(out.toVector, Vector(0.0, 0.0, 0.0), "outside the support the displacement is zero")
    assertEquals(dx(4.0), None, "ci = n - 0.5 is outside")
    assertEquals(dx(4.0 - 1e-9), Some(31.1), "just below n - 0.5 takes the clamped edge value")
    assertEquals(dx(3.5), Some(31.1), "ci = n - 0.75 is in the clamped half-voxel border")
    assertEqualsDouble(dx(-0.5).get, 0.75 * 11.1 + 0.25 * 21.1, 1e-12, "interior is trilinear")
    assertEquals(dx(Double.NaN), None, "nonfinite points are never inside")

  test("stage order is applied first-to-last and is distinguishable"):
    val scale = affine(2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 1)
    // A uniform scale commutes with a homogeneous linear field, so the field has an offset.
    val a = Vector(Vector(0.03, 0.0, 0.0), Vector(0.0, 0.02, 0.0), Vector(0.0, 0.0, -0.01))
    val offset = Vector(0.4, -0.3, 0.2)
    val lin = field(dims, grid)(p => add(mul(a, p), offset))
    val x = at(2.0, 2.0, 1.5).toVector
    val displacedFirst = mapOf(PointMapStage.DisplacementStage(lin), PointMapStage.AffineStage(scale))
    val affineFirst = mapOf(PointMapStage.AffineStage(scale), PointMapStage.DisplacementStage(lin))
    val px = WorldPoint(x(0), x(1), x(2))
    assertPoint(mapped(displacedFirst.forward(px)), add(add(x, mul(a, x)), offset).map(_ * 2.0), 1e-12)
    val doubled = x.map(_ * 2.0)
    val out = new Array[Double](3)
    val inside = affineFirst.forwardInto(x(0), x(1), x(2), out, new Array[Double](3))
    assert(lin.supports(doubled(0), doubled(1), doubled(2)), "fixture keeps the scaled point inside the field")
    val expected = add(add(doubled, mul(a, doubled)), offset)
    assertPoint(WorldPoint(out(0), out(1), out(2)), expected, 1e-12)
    assertNotEquals(out.toVector, mapped(displacedFirst.forward(px)).toVector)
    assertEquals(inside, lin.supports(doubled(0), doubled(1), doubled(2)))

  test("inverse reports NonConvergent within a small budget and OutsideSupport beyond the field"):
    val a = Vector(Vector(0.05, 0.0, 0.0), Vector(0.0, 0.05, 0.0), Vector(0.0, 0.0, 0.05))
    val map = mapOf(PointMapStage.DisplacementStage(field(dims, grid)(p => mul(a, sub(p, at(2.5, 2.0, 1.5).toVector)))))
    val x = at(3.1, 2.8, 1.9)
    map.inverse(x, InversePolicy.make(1e-12, 1).toOption.get) match
      case PointMapOutcome.NonConvergent(point, residual, iterations) =>
        assertEquals(iterations, 1)
        assert(residual > 1e-12)
        assert(point.placedLike(x), "last iterate is reported but not admitted")
      case other => fail(s"expected NonConvergent; got $other")
    assertEquals(map.inverse(x, InversePolicy.make(1e-12, 1).toOption.get).placed, None)
    val far = WorldPoint(1000.0, 0.0, 0.0)
    assertEquals(map.inverse(far, strict), PointMapOutcome.OutsideSupport)
    assertEquals(map.forward(far), PointMapOutcome.OutsideSupport)
    val itk = new Array[Double](3)
    assert(!map.forwardInto(far.x, far.y, far.z, itk, new Array[Double](3)))
    assertEquals(itk.toVector, far.toVector, "ITK evaluation outside the field uses zero displacement")

  test("overflow to a nonfinite coordinate is its own outcome, not OutsideSupport"):
    val huge = mapOf(PointMapStage.AffineStage(affine(1e10, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)))
    assertEquals(huge.forward(WorldPoint(1e300, 0.0, 0.0)), PointMapOutcome.NonFinite)
    assertEquals(huge.inverse(WorldPoint(1e300, 0.0, 0.0), strict), PointMapOutcome.NonFinite)
    assertEquals(huge.inverse(WorldPoint(1e300, 0.0, 0.0), strict).placed, None)

  test("fields, stages and policies validate their inputs"):
    assert(DisplacementField.make(Vector(2, 2), grid, new Array[Double](12)).isLeft)
    assert(DisplacementField.make(dims, grid, new Array[Double](7)).isLeft)
    val bad = new Array[Double](3 * dims.product)
    bad(5) = Double.NaN
    assert(DisplacementField.make(dims, grid, bad).isLeft)
    assert(PointMap.make(Vector.empty).isLeft)
    assert(InversePolicy.make(-1.0, 10).isLeft)
    assert(InversePolicy.make(1e-6, 0).isLeft)
    val copied = new Array[Double](3 * dims.product)
    val f = DisplacementField.make(dims, grid, copied).toOption.get
    copied(0) = 99.0
    val out = new Array[Double](3)
    val c0 = centre(grid, 0, 0, 0)
    f.displacementInto(c0(0), c0(1), c0(2), out)
    assertEquals(out(0), 0.0, "fields own a copy of their values")

  extension (p: WorldPoint)
    private def placedLike(other: WorldPoint): Boolean = p.toVector.zip(other.toVector).exists((a, b) => a != b)
