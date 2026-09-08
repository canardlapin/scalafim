package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.design.event.ConvolvedTerm
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

class SignedOnsetDesignSuite extends munit.FunSuite:
  test("identical local pre-acquisition schedules retain identical first and later run responses") {
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(-1.0, -1.0)),
      "condition" -> Column.Strings(Vector("A", "A")))
    for hrf <- Vector(Hrfs.SPMG1, Hrfs.fir(nBasis = 2, span = 4.s)) do
      val model = EventModelBuilder.build("onset ~ hrf(condition)", data,
        SamplingFrame(blockLens = Seq(8, 8), tr = Seq(1.0)), blockIds = Vector(0, 1),
        defaultHrf = hrf, precision = 0.25.s)
      for row <- 0 until 8; column <- 0 until model.designMatrix.cols do
        assertEqualsDouble(model.designMatrix(row, column), model.designMatrix(row + 8, column), 1e-10)
      assert(model.designMatrix.data.exists(_ > 0.0))
      val term = model.terms.head._2.asInstanceOf[ConvolvedTerm].term
      assertEquals(term.onsets.map(_.value), Vector(-1.0, -1.0))
      val varied = term.convolvePerEvent(Vector(hrf, hrf), model.samplingFrame, precision = 0.25.s)
      for row <- 0 until 8; column <- 0 until varied.data.cols do
        assertEqualsDouble(varied.data(row, column), varied.data(row + 8, column), 1e-10)
      assert(varied.data.data.exists(_ > 0.0))
  }
