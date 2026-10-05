package scalafim.phrfcmp.run

class AlphaGridSuite extends munit.FunSuite:

  test("the pilot grid is the nine powers of two 2^-6 .. 2^2, strictly increasing"):
    val g = AlphaGrid.Pilot
    assertEquals(g.size, 9)
    assertEquals(g.alphas, (-6 to 2).toVector.map(e => math.pow(2.0, e.toDouble)))
    assertEquals(g.alpha(0), 0.015625)
    assertEquals(g.alpha(8), 4.0)
    assert(g.alphas.zip(g.alphas.tail).forall((a, b) => a < b))

  test("lambda is exactly 1 / alpha (powers of two are exact), and the typed alpha carries the same value"):
    val g = AlphaGrid.Pilot
    (0 until g.size).foreach { k =>
      assertEquals(g.lambda(k), 1.0 / g.alpha(k))
      assertEquals(g.lambda(k) * g.alpha(k), 1.0)
      assertEquals(g.typed(k).lambda, g.lambda(k))
    }
    assertEquals(g.lambdas.head, 64.0)
    assertEquals(g.lambdas.last, 0.25)

  test("grids must be non-empty, positive, finite and strictly increasing"):
    assert(AlphaGrid.from(Vector.empty).isLeft)
    assert(AlphaGrid.from(Vector(0.0, 1.0)).isLeft, "alpha 0 is not available (ML refuses it)")
    assert(AlphaGrid.from(Vector(-1.0, 1.0)).isLeft)
    assert(AlphaGrid.from(Vector(1.0, Double.NaN)).isLeft)
    assert(AlphaGrid.from(Vector(1.0, 1.0)).isLeft)
    assert(AlphaGrid.from(Vector(2.0, 1.0)).isLeft)
    assertEquals(AlphaGrid.from(Vector(0.5, 1.0)).map(_.size), Right(2))
    assertEquals(AlphaGrid.from((-6 to 2).toVector.map(e => math.pow(2.0, e.toDouble))), Right(AlphaGrid.Pilot))
