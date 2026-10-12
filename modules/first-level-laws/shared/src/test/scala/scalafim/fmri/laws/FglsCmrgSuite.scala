package scalafim.fmri.laws

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.ar.TimeSegments
import scalafim.fmri.fit.*
import scala.concurrent.duration.*

class FglsCmrgSuite extends munit.FunSuite:
  override val munitTimeout: Duration = 30.minutes

  private def response(cell: FglsStudyCell, noise: Array[Double], offset: Int): DMat =
    FglsVoxelwiseStudy.response(cell, Matrix.tabulate(cell.base.rows, 4)((i, j) => noise(offset + j * cell.base.rows + i)))

  private def oracle(cell: FglsStudyCell, data: DMat, expected: ujson.Value): Unit =
    for voxel <- 0 until 4 do
      val fitted = VoxelwiseArFgls.fit(cell.base.design,
        Matrix.tabulate(data.rows, 1)((i, _) => data(i, voxel)), TimeSegments.fromRunLengths(cell.base.runLengths),
        cell.base.censorRows, VoxelwiseArSpec(2,
          VoxelArCoefficients.Known(cell.base.runLengths.indices.map(cell.truth.phi(voxel, _)).toVector),
          CensorContinuity.ContinuousMissing, CovarianceUncertainty.Conditional)).fold(e => fail(e.message), identity).voxels.head
      val want = expected(voxel)
      fitted.coefficients.zip(want("coefficients").arr).foreach((a, b) => assertEqualsDouble(a, b.num, 1e-8))
      assertEqualsDouble(fitted.residualVariance, want("residual_variance").num, 1e-8)
      assertEqualsDouble(fitted.covariance(0, 0), want("variance").num, 1e-8)
      assertEquals(fitted.residualDf, want("df").num.toInt)

  for example <- FglsCmrgFixtures.values do
    test(s"external CMRG known-control oracle: ${example.cell} dataset ${example.replicate}"):
      val cell = FglsVoxelwiseStudy.cells.find(_.id == example.cell).get
      val data = response(cell, example.noise.split(',').map(_.toDouble), 0)
      oracle(cell, data, ujson.read(example.expected))
      if cell.base.censorRows.nonEmpty then
        val changed = Matrix.tabulate(data.rows, data.cols)((i, j) => data(i, j) +
          (if cell.base.censorRows.contains(i) then 10000.0 + i else 0.0))
        oracle(cell, changed, ujson.read(example.expected))

  test("opt-in full external CMRG control packet preserves all five cells and every dataset"):
    val directory = LawEnvironment.get("SCALAFIM_FGLS_CMRG_CONTROL")
    assume(directory.isDefined, "external control qualification not requested")
    val root = directory.get
    val receipt = ujson.read(FglsExternalInput.text(root + "/receipt.json"))
    assertEquals(receipt("phase").str, "control")
    assertEquals(receipt("replicates").num.toInt, 2048)
    assert(!receipt("scientific_admission").bool)
    assertEquals(receipt("inputs").arr.map(_("cell").str).toSet, FglsVoxelwiseStudy.cells.map(_.id).toSet)
    for cell <- FglsVoxelwiseStudy.cells do
      val noise = FglsExternalInput.doubles(root + "/" + cell.id + ".f64le")
      assertEquals(noise.length, 2048 * 4 * cell.base.rows)
      val expected = ujson.read(FglsExternalInput.text(root + "/" + cell.id + ".oracle.json"))
      for d <- 0 until 2048 do
        val data = response(cell, noise, d * 4 * cell.base.rows)
        val result = FglsVoxelwiseStudy.measureCandidate(cell, data, FglsEngine.KnownContinuous, d)
          .fold(fail(_), identity)
        assertEqualsDouble(result.nullEstimate, expected(0)("coefficients")(d)(0).num, 1e-8)
        assertEqualsDouble(result.signalEstimate, expected(1)("coefficients")(d)(0).num, 1e-8)
        assertEqualsDouble(result.nullVariance, expected(0)("variance")(d).num, 1e-8)
        assertEqualsDouble(result.signalVariance, expected(1)("variance")(d).num, 1e-8)
        assertEqualsDouble(result.tPValue, expected(0)("probability")(d).num, 1e-8)
        assertEqualsDouble(result.fPValue, expected(0)("f_probability")(d).num, 1e-8)
        assertEqualsDouble(result.coverPValue, expected(1)("probability")(d).num, 1e-8)
        assertEquals(result.tRejected, expected(0)("probability")(d).num < 0.05)
        assertEquals(result.fRejected, expected(0)("f_probability")(d).num < 0.05)
        assertEquals(result.covered, expected(1)("probability")(d).num >= 0.05)
        // The established analyzer's fixed profile supplies N=2048 only. This
        // external packet remains control qualification, never confirmation.
        val record = result.json(cell, FglsStudyProfile.Confirmation)
        record("profile") = ujson.Str("control")
        record("role") = ujson.Str("independent-cmrg-known-control-qualification")
        GlsStudyTrace.emit("FGLS_STUDY_TRIAL " + ujson.write(record))
