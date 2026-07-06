package scalafim.fmri.design

import scalafim.fmri.design.event.ConditionBasisList
import scalafim.fmri.design.event.Event
import scalafim.fmri.design.event.EventTerm
import scalafim.fmri.hrf.Hrfs
import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

class ConditionBasisListSuite extends munit.FunSuite:

  private def assertAllClose(actual: Array[Double], expected: Array[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      val a = actual(i)
      val e = expected(i)
      assert(clue(math.abs(a - e)) <= tol, clues(s"index=$i a=$a e=$e"))
      i += 1

  private def sliceCondition(conv: Mat, cond: Int, nConds: Int, nbasis: Int): Mat =
    val totalRows = conv.rows
    val totalCols = conv.cols
    val out = new Array[Double](totalRows * nbasis)
    var r = 0
    while r < totalRows do
      var b = 0
      while b < nbasis do
        val inCol = b * nConds + cond
        out(r * nbasis + b) = conv.data(r * totalCols + inCol)
        b += 1
      r += 1
    Mat.unsafe(totalRows, nbasis, out)

  test("condition_basis_list splits matrix by condition") {
    val eventData = Vector("A", "B", "A")
    val onsets = Vector(0.0, 30.0, 60.0).map(Seconds(_))
    val sf = SamplingFrame(blockLens = Seq(90), tr = Seq(1.0))

    val term = EventTerm(
      events = Vector(Event.factor(eventData, "cond")),
      onsets = onsets,
      blockIds = Vector(0, 0, 0)
    )

    val res = ConditionBasisList.list(term, Hrfs.SPMG2, sf)
    assertEquals(res.keySet.toVector, Vector("cond.A", "cond.B"))
    assertEquals(res.size, 2)

    val nb = Hrfs.SPMG2.nbasis
    res.values.foreach { m =>
      assertEquals(m.rows, 90)
      assertEquals(m.cols, nb)
    }
  }

  test("condition_basis_list matrix output matches convolve") {
    val eventData = Vector("A", "B", "A")
    val onsets = Vector(0.0, 30.0, 60.0).map(Seconds(_))
    val sf = SamplingFrame(blockLens = Seq(90), tr = Seq(1.0))

    val term = EventTerm(
      events = Vector(Event.factor(eventData, "cond")),
      onsets = onsets,
      blockIds = Vector(0, 0, 0)
    )

    val m1 = ConditionBasisList.matrix(term, Hrfs.SPMG2, sf)
    val m2 = term.convolve(Hrfs.SPMG2, sf).data
    assertAllClose(m1.data, m2.data, tol = 1e-12)
  }

  test("condition_basis_list handles multi-basis FIR HRF") {
    val eventData = Vector("A", "B", "A")
    val onsets = Vector(0.0, 30.0, 60.0).map(Seconds(_))
    val sf = SamplingFrame(blockLens = Seq(90), tr = Seq(1.0))

    val term = EventTerm(
      events = Vector(Event.factor(eventData, "cond")),
      onsets = onsets,
      blockIds = Vector(0, 0, 0)
    )

    val fir = Hrfs.fir()
    val res = ConditionBasisList.list(term, fir, sf)

    assertEquals(res.keySet.toVector, Vector("cond.A", "cond.B"))
    val nb = fir.nbasis
    res.values.foreach(m => assertEquals(m.cols, nb))

    val conv = term.convolve(fir, sf)
    val nConds = conv.term.designMatrix(dropEmpty = true).conditionTags.length

    val expectedA = sliceCondition(conv.data, cond = 0, nConds = nConds, nbasis = nb)
    val expectedB = sliceCondition(conv.data, cond = 1, nConds = nConds, nbasis = nb)

    assertAllClose(res("cond.A").data, expectedA.data, tol = 1e-12)
    assertAllClose(res("cond.B").data, expectedB.data, tol = 1e-12)
  }
