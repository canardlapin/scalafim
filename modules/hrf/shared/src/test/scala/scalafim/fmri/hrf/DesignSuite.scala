package scalafim.fmri.hrf

import scalafim.fmri.hrf.design.{Design, SamplingFrame}

class DesignSuite extends munit.FunSuite:

  private val box: Hrf =
    Hrf.scalar("box", span = 1.0.s)(t => if t.value >= 0.0 && t.value <= 1.0 then 1.0 else 0.0)

  test("regressorDesign produces expected design dims") {
    val ons = Seq(0.0, 1.0, 0.0, 2.0)
    val fac = Seq("a", "b", "a", "b")
    val blk = Seq(0, 0, 1, 1)
    val sf = SamplingFrame(blockLens = Seq(3, 3), tr = Seq(1.0))
    val dmat = Design.regressorDesign(ons, fac, blk, sf, hrf = box, precision = 1.0)
    assertEquals(dmat.rows, sf.samples().length)
    assertEquals(dmat.cols, 2)
  }
