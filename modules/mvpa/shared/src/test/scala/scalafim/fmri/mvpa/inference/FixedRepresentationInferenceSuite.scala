package scalafim.fmri.mvpa.inference

import munit.FunSuite
import scalafim.fmri.mvpa.analysis.CalibrationBindings
import gale.linalg.DMat

/** Portable production-kernel fixtures. These are not Monte Carlo qualification. */
class FixedRepresentationInferenceSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def walsh(row: Int, mask: Int): Double = if Integer.bitCount(row & mask) % 2 == 0 then 1.0 else -1.0

  test("serialized independent rank coordinates retain the actual nuisance and candidate scope"):
    val x = DMat.tabulate(16, 3)((i, j) => walsh(i, Vector(1, 2, 4)(j)))
    val y = DMat.tabulate(16, 2)((i, j) =>
      if j == 0 then .8 * walsh(i, 1) + .6 * walsh(i, 8)
      else .3 * walsh(i, 2) + math.sqrt(.91) * walsh(i, 3))
    val z = DMat.tabulate(16, 2)((i, j) => if j == 0 then 1.0 else walsh(i, 12))
    val result = right(CalibrationBindings.rank(x, y, z, 39251L, 39, "independent-rank-oracle"))
    assertEqualsDouble(result.candidateArithmetic.correlations(0), .8, 1e-10)
    assertEqualsDouble(result.candidateArithmetic.correlations(1), .3, 1e-10)
    assertEquals(result.residualRows, 14)
    assert(result.admittedDetectableRank.isLeft)
    assert(result.candidateArithmetic.receipts.forall(_.consumed.value == 39))
