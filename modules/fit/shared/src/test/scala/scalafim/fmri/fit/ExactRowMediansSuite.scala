package scalafim.fmri.fit

import gale.linalg.Matrix

class ExactRowMediansSuite extends munit.FunSuite:
  private val rows = Vector(
    Vector(0.0, 1.0, 2.0, 100.0, 101.0, 102.0, 103.0),
    Vector(Double.MinPositiveValue, -0.0, 0.0, 1e-200, 1e200, Double.MaxValue, Double.PositiveInfinity),
    Vector(1.0, 1.0, 1.0, 2.0, 2.0, Double.NaN, Double.NaN)
  )

  for population <- Vector(1, 2, 6, 7); width <- Vector(1, 3, 7) do
    test(s"exact population median, size=$population width=$width") {
      var passes = 0
      val actual = ExactRowMedians(rows.length, population, consume =>
        passes += 1
        (0 until population).grouped(width).foreach { columns =>
          consume(Matrix.tabulate(rows.length, columns.length)((row, col) => rows(row)(columns(col))))
        }
        Right(())
      ).fold(error => fail(error.message), identity)
      assertEquals(passes, 8)
      rows.indices.foreach { row =>
        val expected = Robust.median(rows(row).take(population))
        if expected.isNaN then assert(actual(row).isNaN)
        else assertEquals(java.lang.Double.doubleToLongBits(actual(row)), java.lang.Double.doubleToLongBits(expected))
      }
    }

  test("a median of chunk medians is not a population median") {
    val values = Vector(0.0, 1.0, 2.0, 100.0, 101.0, 102.0, 103.0)
    val expected = 100.0
    val chunkMedian = Robust.median(values.grouped(3).map(Robust.median).toVector)
    assertEqualsDouble(chunkMedian, 101.0, 0.0)
    assertEqualsDouble(Robust.median(values), expected, 0.0)
  }

  for emitted <- Vector(1, 3, 6) do
    test(s"replay population mismatch is refused even if middle rank exists: $emitted") {
      val result = ExactRowMedians(1, 5, consume =>
        consume(Matrix.tabulate(1, emitted)((_, _) => 1.0))
        Right(())
      )
      assert(result.left.toOption.exists(_.isInstanceOf[FitError.PreparationReplayMismatch]))
    }
