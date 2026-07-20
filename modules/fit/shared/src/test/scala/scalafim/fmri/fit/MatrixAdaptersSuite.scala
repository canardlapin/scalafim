package scalafim.fmri.fit

import scalafim.linalg.DoubleMatrix

class MatrixAdaptersSuite extends munit.FunSuite:
  test("temporary Gale carrier bridge preserves values without mutable aliasing") {
    val source = DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0), Vector(3.0, 4.0)))
    val gale = MatrixAdapters.toGaleMatrix(source)

    source.dataArray(0) = 99.0
    assertEqualsDouble(gale(0, 0), 1.0, 0.0)

    val roundTrip = MatrixAdapters.fromGaleMatrix(gale)
    roundTrip.dataArray(1) = -7.0
    assertEqualsDouble(gale(0, 1), 2.0, 0.0)
    assertEquals((roundTrip.rows, roundTrip.cols), (2, 2))
    assertEqualsDouble(roundTrip(1, 0), 3.0, 0.0)
    assertEqualsDouble(roundTrip(1, 1), 4.0, 0.0)
  }
