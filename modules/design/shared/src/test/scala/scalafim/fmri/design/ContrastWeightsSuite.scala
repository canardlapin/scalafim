package scalafim.fmri.design

import scalafim.fmri.design.basis.ParametricBasis
import scalafim.fmri.design.contrast.*
import scalafim.fmri.design.event.*
import scalafim.fmri.hrf.Seconds

class ContrastWeightsSuite extends munit.FunSuite:

  private def assertAllClose(actual: Array[Double], expected: Array[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      val a = actual(i)
      val e = expected(i)
      assert(clue(math.abs(a - e)) <= tol, clues(s"index=$i a=$a e=$e"))
      i += 1

  test("pair contrast respects where clause") {
    val category = Vector("face", "scene", "face", "scene")
    val attention = Vector("attend", "attend", "ignored", "ignored")
    val onsets = Vector(1.0, 2.0, 3.0, 4.0).map(Seconds(_))

    val term = EventTerm(
      events = Vector(
        Event.factor(category, "category"),
        Event.factor(attention, "attention")
      ),
      onsets = onsets
    )

    val cw = ContrastWeights.pair(
      term = term,
      name = "face_vs_scene_attend",
      A = _.apply("category") == "face",
      B = _.apply("category") == "scene",
      where = _.apply("attention") == "attend"
    )

    assertEquals(cw.condNames, term.conditions)
    assertEquals(cw.weights.rows, 4)
    assertEquals(cw.weights.cols, 1)

    assertAllClose(cw.weights.data, Array(1.0, -1.0, 0.0, 0.0), tol = 1e-12)
  }

  test("unit contrast allows where subsetting") {
    val category = Vector("face", "scene", "face", "scene")
    val attention = Vector("attend", "attend", "ignored", "ignored")
    val onsets = Vector(1.0, 2.0, 3.0, 4.0).map(Seconds(_))

    val term = EventTerm(
      events = Vector(
        Event.factor(category, "category"),
        Event.factor(attention, "attention")
      ),
      onsets = onsets
    )

    val cw = ContrastWeights.unit(
      term = term,
      name = "face_only_attend",
      where = cell => cell("attention") == "attend" && cell("category") == "face"
    )

    assertEquals(cw.condNames, term.conditions)
    assertAllClose(cw.weights.data, Array(1.0, 0.0, 0.0, 0.0), tol = 1e-12)
  }

  test("poly contrast respects where clause") {
    val repnum = Vector("1", "2", "3", "4", "1", "2", "3", "4")
    val grp = Vector("A", "A", "A", "A", "B", "B", "B", "B")
    val onsets = (1 to 8).map(_.toDouble).toVector.map(Seconds(_))

    val term = EventTerm(
      events = Vector(
        Event.factor(repnum, "repnum"),
        Event.factor(grp, "grp")
      ),
      onsets = onsets
    )

    val cw = ContrastWeights.poly(
      term = term,
      name = "polyrep",
      degree = 1,
      value = cell => cell("repnum").toDouble,
      where = _.apply("grp") == "A"
    )

    val expectedPoly = ParametricBasis.Poly.fit(Vector(1.0, 2.0, 3.0, 4.0), degree = 1, argName = "x").y.data
    val expected = expectedPoly ++ Array.fill(4)(0.0)

    assertEquals(cw.condNames, term.conditions)
    assertEquals(cw.weights.rows, 8)
    assertEquals(cw.weights.cols, 1)
    assertAllClose(cw.weights.data, expected, tol = 1e-12)
  }

  test("pair contrast basis filtering keeps only selected bases") {
    val conditions = Vector("A", "B", "C", "A", "B", "C")
    val onsets = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0).map(Seconds(_))

    val term = EventTerm(
      events = Vector(Event.factor(conditions, "condition")),
      onsets = onsets
    )

    val cw = ContrastWeights.pair(
      term = term,
      name = "A_vs_B_b1",
      A = _.apply("condition") == "A",
      B = _.apply("condition") == "B",
      nbasis = 5,
      basis = Some(Seq(1))
    )

    assertEquals(cw.selectedCondNames, Vector("condition.A_b01", "condition.B_b01", "condition.C_b01"))
    assertEquals(cw.condNames.length, 15)
    assertEquals(cw.weights.rows, 15)
    assertEquals(cw.weights.cols, 1)

    val idxA = cw.condNames.indexOf("condition.A_b01")
    val idxB = cw.condNames.indexOf("condition.B_b01")
    assert(idxA >= 0 && idxB >= 0)
    assertEquals(cw.weights.data(idxA), 1.0)
    assertEquals(cw.weights.data(idxB), -1.0)

    // All other basis rows should be zero.
    val nonZero = cw.weights.data.count(_ != 0.0)
    assertEquals(nonZero, 2)
  }

  test("basis weights reject negative and non-finite values") {
    val conditions = Vector("A", "B", "A", "B")
    val onsets = Vector(1.0, 2.0, 3.0, 4.0).map(Seconds(_))
    val term = EventTerm(
      events = Vector(Event.factor(conditions, "condition")),
      onsets = onsets
    )

    intercept[IllegalArgumentException] {
      ContrastWeights.pair(
        term = term,
        name = "bad_negative",
        A = _.apply("condition") == "A",
        B = _.apply("condition") == "B",
        nbasis = 2,
        basis = Some(Seq(1, 2)),
        basisWeights = Some(Seq(0.5, -0.5))
      )
    }

    intercept[IllegalArgumentException] {
      ContrastWeights.pair(
        term = term,
        name = "bad_nan",
        A = _.apply("condition") == "A",
        B = _.apply("condition") == "B",
        nbasis = 2,
        basis = Some(Seq(1, 2)),
        basisWeights = Some(Seq(0.5, Double.NaN))
      )
    }
  }
