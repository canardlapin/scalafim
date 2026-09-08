package scalafim.surface.view.javafx

import intaglio.Rgba32

class NativeSubpixelCoverageSuite extends munit.FunSuite:
  import NativeSubpixelCoverage.*
  private val gray = Rgba32.unsafe(85, 85, 85)
  // The vertical edge lies just to the right of pixel center (0.5, 0.5).
  // Rounding to 1/256 moves it onto the center; all depths are constant.
  private val triangle = Vector(Point(0.5005, 0, 0.2), Point(0.5005, 1, 0.2), Point(2, 0.5, 0.2))
  private def check(points: Vector[Point] = triangle, referenceDepth: Double = 0.7,
      referenceFace: Int = 1, actual: Rgba32 = gray): Option[Evidence] =
    certify(points, 0.5, 0.5, referenceDepth, referenceFace, 2, gray, actual)

  test("only a nearer, newly covered triangle with matching native color is classified"):
    val evidence = check().get
    assertEqualsDouble(evidence.maximumVertexShift, 0.0005, 1e-14)
    assertEqualsDouble(evidence.nativeDepth, 0.2, 1e-14)
    assertEquals(evidence.maximumColorError, 0)
    assert(check(referenceDepth = 0.1).isEmpty)
    assert(check(referenceFace = 2).isEmpty)
    assert(check(actual = Rgba32.unsafe(119, 119, 119)).isEmpty)

  test("ordinary interiors, distant edges, degeneracy and nonfinite data remain unclassified"):
    assert(check(triangle.map(p => p.copy(x = p.x - 0.01))).isEmpty)
    assert(check(triangle.map(p => p.copy(x = p.x + 0.01))).isEmpty)
    assert(check(Vector.fill(3)(triangle.head)).isEmpty)
    assert(check(triangle.updated(0, triangle.head.copy(x = Double.NaN))).isEmpty)
    assert(check(referenceDepth = Double.NaN).isEmpty)
    assert(check(triangle.map(_.copy(depth = -0.1))).isEmpty)

  test("pixel interiors require the entire guarded square, not just its center"):
    val enclosing = Vector(Point(-0.1, -0.1, 0.2), Point(2.5, -0.1, 0.2), Point(-0.1, 2.5, 0.2))
    assert(containsPixel(enclosing, 0, 0))
    assert(containsPixel(enclosing.reverse, 0, 0))
    assert(!containsPixel(triangle, 0, 0))
    assert(!containsPixel(Vector(Point(0, 0, 0.2), Point(2, 0, 0.2), Point(0, 2, 0.2)), 0, 0))
    assert(!containsPixel(Vector.fill(3)(enclosing.head), 0, 0))
    assert(!containsPixel(enclosing.updated(0, enclosing.head.copy(x = Double.NaN)), 0, 0))

  test("nearest-boundary classification requires unchanged face, different owners and newly covered exact color"):
    def boundary(points: Vector[Point] = triangle, face: Int = 2, vertex: Int = 8,
        actual: Rgba32 = gray): Option[Evidence] =
      certifyNearestBoundary(points, 0.5, 0.5, 0.2, 2, face, 7, vertex, gray, actual)
    assert(boundary().nonEmpty)
    assert(boundary(face = 3).isEmpty)
    assert(boundary(vertex = 7).isEmpty)
    assert(boundary(vertex = -1).isEmpty)
    assert(boundary(triangle.map(p => p.copy(x = p.x - 0.01))).isEmpty)
    assert(boundary(triangle.map(p => p.copy(x = p.x + 0.01))).isEmpty)
    assert(boundary(actual = Rgba32.unsafe(119, 119, 119)).isEmpty)
    assert(boundary(triangle.map(_.copy(depth = 0.7))).isEmpty)

  test("observed ellipsoid nearest boundary has subpixel coverage with the original face preserved"):
    val observed = Vector(Point(490.169161961459, 194.2682771129081, 0.9999114292697657),
      Point(486.492595447623, 192.4809052287497, 0.9999117480769653),
      Point(486.6134820924735, 192.78861928761629, 0.9999115942549504))
    val blue = Rgba32.unsafe(38, 139, 210)
    val evidence = certifyNearestBoundary(observed, 486.5, 192.5, 0.9999117385130308,
      51311, 51311, 26011, 25756, blue, blue).get
    assertEqualsDouble(evidence.maximumVertexShift, 0.0012541370919052497, 1e-12)
    assertEqualsDouble(evidence.nativeDepth, 0.999911738369976, 1e-12)
    assertEquals(evidence.maximumColorError, 0)
    assert(certify(observed, 486.5, 192.5, 0.9999117385130308, 51311, 51311, blue, blue).isEmpty)
