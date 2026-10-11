package scalafim.fmri.laws

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.ar.TimeSegments
import scalafim.fmri.fit.*
import ujson.{Arr, Num, Obj, Str}

/** Deterministic investigation of already-spent study inputs, never a new confirmation run. The original study and its
  * records remain immutable.
  */
class FglsDiagnosisSuite extends munit.FunSuite:
  private val study = FglsVoxelwiseStudy
  private val retainedReplicates = Vector(0, 1, 1024, 2047)

  private def retainedSeed(group: Int, replicate: Int): Int =
    ((1499711011L + 22L * 10000019L + (group + 1L) * 1000003L + replicate.toLong * 7919L) %
      2147483646L + 1L).toInt

  private def rows(matrix: DMat): Arr =
    Arr.from((0 until matrix.rows).map(i => Arr.from((0 until matrix.cols).map(j => Num(matrix(i, j))))))

  private def known(cell: FglsStudyCell, data: DMat, voxel: Int = 0): VoxelArFglsFit =
    VoxelwiseArFgls
      .fit(
        cell.base.design,
        data,
        TimeSegments.fromRunLengths(cell.base.runLengths),
        cell.base.censorRows,
        VoxelwiseArSpec(
          2,
          VoxelArCoefficients.Known(cell.base.runLengths.indices.map(cell.truth.phi(voxel, _)).toVector),
          CensorContinuity.ContinuousMissing,
          CovarianceUncertainty.Conditional
        )
      )
      .fold(error => fail(error.message), identity)
      .voxels
      .head

  test("retained affine seeds concentrate one Box-Muller angle across every long-cell replicate"):
    // Two uniforms per Gaussian; 448 innovations in run 0 then burn-in and
    // time 140 in run 1. This records a diagnosed legacy-generator failure,
    // not a requirement for a future generator to reproduce it.
    var total = 0.0
    var positive = 0
    var replicate = 0
    while replicate < 2048 do
      val rng = new GlsStudyRng(retainedSeed(1, replicate))
      var innovation = 0.0
      var step = 0
      while step < 845 do
        innovation = rng.gaussian()
        step += 1
      total += innovation
      if innovation > 0.0 then positive += 1
      replicate += 1
    assertEquals(positive, 2048)
    assertEqualsDouble(total / 2048.0, 0.982888492484109, 1e-12)

  test("known continuous GLS matches the independent dense R oracle on forty retained inputs"):
    FglsKnownOracleFixtures.values.foreach: expected =>
      val cell = study.cells.find(_.id == expected.cell).get
      val response = study.response(cell, study.noise(cell, retainedSeed(cell.group, expected.replicate)))
      val fitted = known(cell, Matrix.tabulate(response.rows, 1)((i, _) => response(i, expected.voxel)), expected.voxel)
      fitted.coefficients.zip(expected.coefficients).foreach((a, b) => assertEqualsDouble(a, b, 1e-10))
      assertEqualsDouble(fitted.residualVariance, expected.residualVariance, 1e-10)
      assertEqualsDouble(fitted.covariance(0, 0), expected.variance, 1e-10)
      assertEquals(fitted.residualDf, expected.df)
      val contrast = Matrix.tabulate(1, fitted.coefficients.length)((_, j) => if j == 0 then 1.0 else 0.0)
      val tested =
        fitted.test(contrast, Vector(cell.base.coefficient(0, expected.voxel))).fold(e => fail(e.message), identity)
      assertEqualsDouble(
        FglsDistribution.fUpper(tested.statistic, 1.0, tested.denominatorDf),
        expected.probability,
        1e-10
      )

  study.cells.foreach: cell =>
    test(s"known continuous GLS preserves planted truth and ignores censored values: ${cell.id}"):
      val errors = study.noise(cell, retainedSeed(cell.group, 0))
      val signal = study.response(cell, Matrix.zeros(cell.base.rows, 4))
      def response(sign: Double, replaceCensored: Boolean = false): DMat =
        Matrix.tabulate(cell.base.rows, 1): (row, _) =>
          signal(row, 0) + sign * errors(row, 0) +
            (if replaceCensored && cell.base.censorRows.contains(row) then 10000.0 + row else 0.0)
      val positive = known(cell, response(1.0))
      val negative = known(cell, response(-1.0))
      val replaced = known(cell, response(1.0, true))
      positive.coefficients.indices.foreach: col =>
        assertEqualsDouble(
          (positive.coefficients(col) + negative.coefficients(col)) / 2.0,
          cell.base.coefficient(col, 0),
          1e-10
        )
        assertEqualsDouble(replaced.coefficients(col), positive.coefficients(col), 1e-8)
      assertEquals(positive.residualDf, cell.base.rows - cell.base.design.cols - cell.base.censorRows.length)
      assertEqualsDouble(replaced.residualVariance, positive.residualVariance, 1e-8)

  if LawEnvironment.get("SCALAFIM_FGLS_DIAGNOSIS_EXPORT").contains("true") then
    test("export fixed retained inputs and complete design for the independent R diagnosis"):
      study.cells.foreach: cell =>
        val fixtures = retainedReplicates.map: replicate =>
          val seed = retainedSeed(cell.group, replicate)
          val errors = study.noise(cell, seed)
          Obj(
            "replicate" -> Num(replicate),
            "seed" -> Num(seed),
            "noise" -> rows(Matrix.tabulate(errors.rows, 2)((i, j) => errors(i, j)))
          )
        val packet = Obj(
          "cell" -> Str(cell.id),
          "group" -> Num(cell.group),
          "run_lengths" -> Arr.from(cell.base.runLengths),
          "censored_rows_zero_based" -> Arr.from(cell.base.censorRows),
          "design" -> rows(cell.base.design),
          "beta" -> rows(Matrix.tabulate(cell.base.design.cols, 2)((i, j) => cell.base.coefficient(i, j))),
          "phi_by_voxel_run" -> Arr.from(
            (0 until 2).map(v => Arr.from(cell.base.runLengths.indices.map(r => Arr.from(cell.truth.phi(v, r)))))
          ),
          "fixtures" -> Arr.from(fixtures)
        )
        GlsStudyTrace.emit("FGLS_DIAGNOSIS_INPUT " + ujson.write(packet))
