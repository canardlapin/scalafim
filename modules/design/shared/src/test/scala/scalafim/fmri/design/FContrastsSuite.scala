package scalafim.fmri.design

import scalafim.fmri.design.contrast.*
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.*
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

class FContrastsSuite extends munit.FunSuite:

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

  test("FContrasts.forConvolvedTerm: single factor uses contr.sum") {
    val conds = Vector("A", "B", "C", "A", "B", "C")
    val onsets = (1 to conds.length).map(_.toDouble).toVector.map(Seconds(_))
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0))

    val term = EventTerm(
      events = Vector(Event.factor(conds, "condition")),
      onsets = onsets,
      blockIds = Vector.fill(conds.length)(0),
      termTag = Some("t")
    )

    val conv = term.convolve(Hrfs.SPMG1, sf)
    val fs = FContrasts.forConvolvedTerm(conv)
    assertEquals(fs.keySet, Set("condition"))

    val cw = fs("condition")
    assertEquals(cw.condNames, conv.columnNames)
    assertEquals(cw.weights.rows, conv.columnNames.length)
    assertEquals(cw.weights.cols, 2)

    val expected = Array(
      1.0, 0.0,
      0.0, 1.0,
      -1.0, -1.0
    )
    assertAllClose(cw.weights.data, expected, tol = 1e-12)
  }

  test("FContrasts.forConvolvedTerm: 3x2 ordering matches event cell order") {
    val f1 = Vector("A", "B", "C", "A", "B", "C")
    val f2 = Vector("X", "X", "X", "Y", "Y", "Y")
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
        "t_f1.C_f2.X",
        "t_f1.A_f2.Y",
        "t_f1.B_f2.Y",
        "t_f1.C_f2.Y"
      )
    )

    val fs = FContrasts.forConvolvedTerm(conv)
    assertEquals(fs.keySet, Set("f1", "f2", "f1:f2"))

    val f1w = fs("f1").weights
    assertEquals(f1w.rows, 6)
    assertEquals(f1w.cols, 2)
    assertAllClose(row(f1w, 0), Array(1.0, 0.0), tol = 1e-12)
    assertAllClose(row(f1w, 1), Array(0.0, 1.0), tol = 1e-12)
    assertAllClose(row(f1w, 2), Array(-1.0, -1.0), tol = 1e-12)
    assertAllClose(row(f1w, 3), Array(1.0, 0.0), tol = 1e-12)
    assertAllClose(row(f1w, 4), Array(0.0, 1.0), tol = 1e-12)
    assertAllClose(row(f1w, 5), Array(-1.0, -1.0), tol = 1e-12)

    val f2w = fs("f2").weights
    assertEquals(f2w.rows, 6)
    assertEquals(f2w.cols, 1)
    assertAllClose(f2w.data, Array(1.0, 1.0, 1.0, -1.0, -1.0, -1.0), tol = 1e-12)

    val intw = fs("f1:f2").weights
    assertEquals(intw.rows, 6)
    assertEquals(intw.cols, 2)
    assertAllClose(row(intw, 0), Array(1.0, 0.0), tol = 1e-12)
    assertAllClose(row(intw, 1), Array(0.0, 1.0), tol = 1e-12)
    assertAllClose(row(intw, 2), Array(-1.0, -1.0), tol = 1e-12)
    assertAllClose(row(intw, 3), Array(-1.0, 0.0), tol = 1e-12)
    assertAllClose(row(intw, 4), Array(0.0, -1.0), tol = 1e-12)
    assertAllClose(row(intw, 5), Array(1.0, 1.0), tol = 1e-12)
  }

  test("FContrasts.forConvolvedTerm: replicates weights across basis") {
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
    val fs = FContrasts.forConvolvedTerm(conv)
    val cw = fs("condition")

    val idxA1 = conv.columnNames.indexOf("t_condition.A_b01")
    val idxB2 = conv.columnNames.indexOf("t_condition.B_b02")
    val idxC3 = conv.columnNames.indexOf("t_condition.C_b03")
    assert(idxA1 >= 0 && idxB2 >= 0 && idxC3 >= 0)

    assertAllClose(row(cw.weights, idxA1), Array(1.0, 0.0), tol = 1e-12)
    assertAllClose(row(cw.weights, idxB2), Array(0.0, 1.0), tol = 1e-12)
    assertAllClose(row(cw.weights, idxC3), Array(-1.0, -1.0), tol = 1e-12)
  }

  test("EventModel.validateAllContrasts validates attached + F-contrasts") {
    val sf = SamplingFrame(blockLens = Seq(40), tr = Seq(1.0))

    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 10.0, 20.0, 30.0, 35.0, 38.0)),
      "cond" -> Column.Strings(Vector("A", "B", "C", "A", "B", "C"))
    )

    val cset =
      ContrastSpec.ContrastSet(
        ContrastSpec.Pair(
          name = "A_vs_B",
          A = cell => cell("cond") == "A",
          B = cell => cell("cond") == "B"
        )
      )

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(cond, id = task, contrasts = myset)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0, 0, 0, 0),
      contrastSets = Map("myset" -> cset)
    )

    import ContrastRegistry.*

    val res = model.validateAllContrasts()
    assertEquals(res.map(_.name), Vector("task#A_vs_B", "task#cond#1", "task#cond#2"))
    val fRows = res.filter(_.name.startsWith("task#cond#"))
    assertEquals(fRows.map(_.contrastType).distinct, Vector(Validate.ContrastType.F))
    assert(fRows.forall(_.fullRank.contains(true)))
  }
