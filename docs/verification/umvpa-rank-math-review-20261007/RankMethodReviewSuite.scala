package scalafim.fmri.mvpa.inference

import gale.backend.Backend.given
import gale.linalg.DMat
import multivar.inference.StepwiseCanonicalRank
import resample4s.kernel.Permutation

/** Review-only reproduction of a current limitation, outside shipping test roots.
  * Temporarily copy into test sources; never a qualification or desired API law. */
class RankMethodReviewSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(e => fail(e.toString), identity)

  test("current coefficient completion changes exact permutation probabilities under an invertible shear"):
    val directory = CalibrationPlatform.environment("SCALAFIM_RANK_MATH_REVIEW_DIRECTORY")
    assume(directory.isDefined, "no mathematical review fixture requested")
    val rows = CalibrationPlatform.readText(directory.get + "/affine-input.tsv")
      .linesIterator.drop(1).filter(_.nonEmpty).map(_.split("\t").map(_.toDouble).toVector).toVector
    assertEquals(rows.size, 6)
    val x = DMat.tabulate(6, 2)((i, j) => rows(i)(j))
    val y = DMat.tabulate(6, 3)((i, j) => rows(i)(j + 2))
    val shear = DMat.tabulate(3, 3)((i, j) => if i == j then 1.0 else if i == 0 && j == 2 then 2.0 else 0.0)
    val base = right(StepwiseCanonicalRank.from(x, y))
    val changed = right(StepwiseCanonicalRank.from(x, y * shear))
    base.correlations.zip(changed.correlations).foreach((a, b) => assertEqualsDouble(a, b, 1e-12))
    base.observedWilks.zip(changed.observedWilks).foreach((a, b) => assertEqualsDouble(a, b, 1e-12))
    val expected = CalibrationPlatform.readText(directory.get + "/affine-actions.tsv")
      .linesIterator.drop(1).filter(_.startsWith("coefficient-euclidean\t"))
      .map(_.split("\t").drop(2).map(_.toDouble).toVector).toVector
    val actions = Vector.range(0, 6).permutations.toVector
    assertEquals(actions.size, 720)
    assertEquals(expected.size, actions.size)
    val baseCounts = Array(0, 0)
    val changedCounts = Array(0, 0)
    var largestDifference = 0.0
    actions.zipWithIndex.foreach: (indices, index) =>
      val action = right(Permutation.from(IArray.from(indices)))
      val a = right(base.nullStatistics(action))
      val b = right(changed.nullStatistics(action))
      Vector(a(0), b(0), a(1), b(1)).zip(expected(index)).foreach: (actual, oracle) =>
        assertEqualsDouble(actual, oracle, 1e-10)
      largestDifference = math.max(largestDifference, math.abs(a(1) - b(1)))
      (0 until 2).foreach: k =>
        if a(k) >= base.observedWilks(k) - 1e-12 then baseCounts(k) += 1
        if b(k) >= changed.observedWilks(k) - 1e-12 then changedCounts(k) += 1
    assertEquals(baseCounts.toVector, Vector(460, 642))
    assertEquals(changedCounts.toVector, Vector(460, 672))
    assert(largestDifference > 2.8)
    CalibrationPlatform.appendText(directory.get + "/production-checks.jsonl",
      "{\"purpose\":\"mathematical-counterexample\",\"group_actions\":720,\"random_draws\":0," +
        "\"base_counts\":[460,642],\"sheared_counts\":[460,672],\"maximum_tail_statistic_change\":" + largestDifference + "}\n")
