package scalafim.fmri.mvpa.inference

import gale.linalg.DMat
import multivar.inference.{CanonicalResidualBasis, StepwiseCanonicalRank}
import resample4s.kernel.Permutation

/** Algebra-only replay of already exposed inputs, never additional pilot evidence. */
class RankDiagnosticReplaySuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(e => fail(e.toString), identity)

  test("opt-in diagnostic exports actual residual and stepwise kernels without Monte Carlo"):
    val list = CalibrationPlatform.environment("SCALAFIM_RANK_DIAGNOSTIC_CASE_LIST")
    assume(list.isDefined, "no exposed-case replay requested")
    val output = CalibrationPlatform.environment("SCALAFIM_RANK_DIAGNOSTIC_OUTPUT")
      .getOrElse(fail("diagnostic output path required"))
    val paths = CalibrationPlatform.readText(list.get).linesIterator.filter(_.nonEmpty).toVector
    assertEquals(paths.size, 16)
    paths.foreach: path =>
      val input = right(CalibrationProtocolSupport.readCase(CalibrationPlatform.readText(path)))
      assertEquals(input.phase, "pilot")
      assertEquals(input.ordinal, 0)
      assert(input.scenario.startsWith("rank.R3."))
      val nuisance = DMat.tabulate(input.x.rows, input.nuisance.cols + 1): (i, j) =>
        if j == 0 then 1.0 else input.nuisance(i, j - 1)
      val basis = right(CanonicalResidualBasis.from(nuisance))
      val problem = right(StepwiseCanonicalRank.from(right(basis.project(input.x)), right(basis.project(input.y))))
      val rows = basis.matrix.cols
      val actions = Vector(
        Vector.range(0, rows).reverse,
        Vector.tabulate(rows)(i => (i + 1) % rows),
        Vector.tabulate(rows)(i => if i == 0 then 1 else if i == 1 then 0 else i)
      )
      val nulls = actions.map(action => right(problem.nullStatistics(right(Permutation.from(IArray.from(action))))))
      val coefficients = Vector.tabulate(basis.matrix.rows * basis.matrix.cols): index =>
        basis.matrix(index / basis.matrix.cols, index % basis.matrix.cols)
      def vector(values: Vector[Double]): String = values.mkString("[", ",", "]")
      val record = "{\"purpose\":\"exposed-input-algebra-only\",\"new_random_draws\":0,\"scenario_id\":" +
        CalibrationProtocolSupport.jsonString(input.scenario) + ",\"ordinal\":0,\"rows\":" + input.x.rows +
        ",\"residual_rows\":" + rows + ",\"basis\":" + vector(coefficients) +
        ",\"roots\":" + vector(problem.correlations) + ",\"wilks\":" + vector(problem.observedWilks) +
        ",\"actions\":[" + actions.map(_.mkString("[", ",", "]")).mkString(",") +
        "],\"null_statistics\":[" + nulls.map(vector).mkString(",") + "]}\n"
      CalibrationPlatform.appendText(output, record)
