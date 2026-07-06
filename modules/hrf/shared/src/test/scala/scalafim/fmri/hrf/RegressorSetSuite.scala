package scalafim.fmri.hrf

import scalafim.fmri.hrf.regressor.*

class RegressorSetSuite extends munit.FunSuite:

  private val box: Hrf =
    Hrf.scalar("box", span = 1.0.s)(t => if t.value >= 0.0 && t.value <= 1.0 then 1.0 else 0.0)

  test("RegressorSet constructs and evaluates") {
    val ons = Seq(0.0, 1.0, 2.0, 3.0)
    val fac = Seq("a", "a", "b", "b")
    val rs = RegressorSet(ons, fac, hrf = box)
    val grid = Seq(0.0, 1.0, 2.0, 3.0, 4.0)
    val mat = rs.evaluate(grid, precision = 1.0, method = Regressor.EvalMethod.Conv)
    assertEquals(mat.rows, grid.length)
    assertEquals(mat.cols, 2)
  }
