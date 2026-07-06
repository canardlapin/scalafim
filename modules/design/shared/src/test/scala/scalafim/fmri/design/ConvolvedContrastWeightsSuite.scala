package scalafim.fmri.design

import scalafim.fmri.design.contrast.*
import scalafim.fmri.design.event.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

class ConvolvedContrastWeightsSuite extends munit.FunSuite:

  private def assertAllClose(actual: Array[Double], expected: Array[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      val a = actual(i)
      val e = expected(i)
      assert(clue(math.abs(a - e)) <= tol, clues(s"index=$i a=$a e=$e"))
      i += 1

  private def row(mat: Mat, r: Int): Array[Double] =
    mat.data.slice(r * mat.cols, (r + 1) * mat.cols)

  test("column contrast matches regex patterns") {
    val mat = Mat.unsafe(3, 2, Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0))
    val onsets = Vector(1.0, 2.0, 3.0).map(Seconds(_))
    val sf = SamplingFrame(blockLens = Seq(10), tr = Seq(1.0))

    val term = EventTerm(
      events = Vector(Event.matrix(mat, name = "m")),
      onsets = onsets,
      blockIds = Vector(0, 0, 0),
      termTag = Some("term")
    )

    val conv = term.convolve(Hrfs.SPMG3, sf)
    assertEquals(
      conv.columnNames,
      Vector(
        "term_f01_b01",
        "term_f02_b01",
        "term_f01_b02",
        "term_f02_b02",
        "term_f01_b03",
        "term_f02_b03"
      )
    )

    val cw = ConvolvedContrastWeights.column(
      term = conv,
      name = "f01_vs_f02_b01",
      patternA = "f01_b01$".r,
      patternB = Some("f02_b01$".r)
    )

    assertEquals(cw.condNames, conv.columnNames)
    assertEquals(cw.weights.rows, conv.columnNames.length)
    assertEquals(cw.weights.cols, 1)

    val idxA = cw.condNames.indexOf("term_f01_b01")
    val idxB = cw.condNames.indexOf("term_f02_b01")
    assert(idxA >= 0 && idxB >= 0)

    assertEquals(cw.weights.data(idxA), 1.0)
    assertEquals(cw.weights.data(idxB), -1.0)
    assertEquals(cw.weights.data.count(_ != 0.0), 2)
    assertEquals(cw.selectedCondNames, Vector("term_f01_b01", "term_f02_b01"))
  }

  test("oneway produces Helmert weights and aligns to convolved columns") {
    val conds = Vector("A", "B", "C", "A", "B", "C")
    val onsets = (1 to conds.length).map(_.toDouble).toVector.map(Seconds(_))
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0))

    val term = EventTerm(
      events = Vector(Event.factor(conds, "condition")),
      onsets = onsets,
      blockIds = Vector.fill(conds.length)(0),
      termTag = Some("t")
    )

    val conv = term.convolve(Hrfs.SPMG3, sf)
    val cw = ConvolvedContrastWeights.oneway(
      term = conv,
      name = "cond",
      factor = "condition",
      basis = Some(Seq(1))
    )

    assertEquals(cw.condNames, conv.columnNames)
    assertEquals(cw.contrastNames, Vector("cond_1", "cond_2"))
    assertEquals(cw.weights.rows, conv.columnNames.length)
    assertEquals(cw.weights.cols, 2)
    assertEquals(cw.selectedCondNames, Vector("t_condition.A_b01", "t_condition.B_b01", "t_condition.C_b01"))

    val idxA = cw.condNames.indexOf("t_condition.A_b01")
    val idxB = cw.condNames.indexOf("t_condition.B_b01")
    val idxC = cw.condNames.indexOf("t_condition.C_b01")
    assert(idxA >= 0 && idxB >= 0 && idxC >= 0)

    assertAllClose(row(cw.weights, idxA), Array(-1.0, -1.0), tol = 1e-12)
    assertAllClose(row(cw.weights, idxB), Array(1.0, -1.0), tol = 1e-12)
    assertAllClose(row(cw.weights, idxC), Array(0.0, 2.0), tol = 1e-12)

    var r = 0
    while r < cw.weights.rows do
      if !cw.condNames(r).endsWith("_b01") then assertAllClose(row(cw.weights, r), Array(0.0, 0.0), tol = 1e-12)
      r += 1
  }

  test("interaction 2x2 matches expected +/- pattern") {
    val f1 = Vector("A", "B", "A", "B")
    val f2 = Vector("X", "X", "Y", "Y")
    val onsets = (1 to f1.length).map(_.toDouble).toVector.map(Seconds(_))
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0))

    val term = EventTerm(
      events = Vector(Event.factor(f1, "f1"), Event.factor(f2, "f2")),
      onsets = onsets,
      blockIds = Vector.fill(f1.length)(0),
      termTag = Some("t")
    )

    val conv = term.convolve(Hrfs.SPMG1, sf)
    assertEquals(
      conv.columnNames,
      Vector(
        "t_f1.A_f2.X",
        "t_f1.B_f2.X",
        "t_f1.A_f2.Y",
        "t_f1.B_f2.Y"
      )
    )

    val cw = ConvolvedContrastWeights.interaction(
      term = conv,
      name = "int",
      factor1 = "f1",
      factor2 = "f2"
    )

    assertEquals(cw.condNames, conv.columnNames)
    assertEquals(cw.weights.rows, conv.columnNames.length)
    assertEquals(cw.weights.cols, 1)
    assertEquals(cw.selectedCondNames, conv.columnNames)

    assertAllClose(cw.weights.data, Array(1.0, -1.0, -1.0, 1.0), tol = 1e-12)
  }
