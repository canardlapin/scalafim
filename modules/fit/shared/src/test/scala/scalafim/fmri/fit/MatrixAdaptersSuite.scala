package scalafim.fmri.fit

import scalafim.fmri.fit.GaleTestSyntax.*

import gale.linalg.Matrix

class MatrixAdaptersSuite extends munit.FunSuite:
  test("Gale column binding preserves logical row-major values"):
    val left = Matrix.dense(2, 1, Seq(1.0, 3.0))
    val right = Matrix.dense(2, 1, Seq(2.0, 4.0))
    val bound = MatrixAdapters.bindColumns(left, right)

    assertEquals((bound.rows, bound.cols), (2, 2))
    assertEqualsDouble(bound(0, 0), 1.0, 0.0)
    assertEqualsDouble(bound(0, 1), 2.0, 0.0)
    assertEqualsDouble(bound(1, 0), 3.0, 0.0)
    assertEqualsDouble(bound(1, 1), 4.0, 0.0)
