package scalafim.fmri.design

import scalafim.fmri.design.contrast.*
import scalafim.fmri.design.event.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

class ContrastSpecSuite extends munit.FunSuite:

  private def assertAllClose(actual: Array[Double], expected: Array[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      val a = actual(i)
      val e = expected(i)
      assert(clue(math.abs(a - e)) <= tol, clues(s"index=$i a=$a e=$e"))
      i += 1

  test("ContrastSpec.ContrastSet evaluates against a ConvolvedTerm") {
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
    val set = ContrastSpec.pairwiseContrasts(levels = Vector("A", "B", "C"), facName = "condition")
    val weights = set.weights(conv)

    assertEquals(weights.keys.toVector, Vector("con_A_B", "con_A_C", "con_B_C"))
    assertAllClose(weights("con_A_B").weights.data, Array(1.0, -1.0, 0.0), tol = 1e-12)
    assertAllClose(weights("con_A_C").weights.data, Array(1.0, 0.0, -1.0), tol = 1e-12)
    assertAllClose(weights("con_B_C").weights.data, Array(0.0, 1.0, -1.0), tol = 1e-12)
  }

  test("ContrastSpec.Mask builds weights from logical masks") {
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
    val spec = ContrastSpec.Mask(
      name = "A_vs_B",
      A = Vector(true, false, false),
      B = Some(Vector(false, true, false))
    )
    val weights = spec.weights(conv)

    assertEquals(weights.condNames, conv.columnNames)
    assertAllClose(weights.weights.data, Array(1.0, -1.0, 0.0), tol = 1e-12)
  }

  test("ContrastSpec difference subtracts two contrast specs") {
    val category = Vector("face", "scene", "face", "scene")
    val attention = Vector("attend", "attend", "ignored", "ignored")
    val onsets = Vector(1.0, 2.0, 3.0, 4.0).map(Seconds(_))
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0))

    val term = EventTerm(
      events = Vector(Event.factor(category, "category"), Event.factor(attention, "attention")),
      onsets = onsets,
      blockIds = Vector.fill(onsets.length)(0),
      termTag = Some("t")
    )

    val conv = term.convolve(Hrfs.SPMG1, sf)
    val attend = ContrastSpec.Pair(
      name = "face_scene#attend",
      A = cell => cell("category") == "face",
      B = cell => cell("category") == "scene",
      where = cell => cell("attention") == "attend"
    )
    val ignored = ContrastSpec.Pair(
      name = "face_scene#ignored",
      A = cell => cell("category") == "face",
      B = cell => cell("category") == "scene",
      where = cell => cell("attention") == "ignored"
    )

    val diff = attend - ignored
    val weights = diff.weights(conv)
    assertEquals(weights.name, "face_scene#attend:face_scene#ignored")
    assertAllClose(weights.weights.data, Array(1.0, -1.0, -1.0, 1.0), tol = 1e-12)
  }

  test("ContrastSpec set generators build specs without a term") {
    val cs = ContrastSpec.slidingWindowContrasts(
      levels = Vector("A", "B", "C", "D", "E"),
      facName = "condition",
      windowSize = 2,
      namePrefix = "win"
    )

    assertEquals(cs.contrasts.map(_.name), Vector("win_A-B_vs_C-D", "win_B-C_vs_D-E"))
  }

  test("translateLegacyPattern matches R helper examples") {
    assertEquals(ContrastSpec.translateLegacyPattern("condition[A]"), "condition.A")
    assertEquals(ContrastSpec.translateLegacyPattern("term:basis[2]"), "term_b2")
    assertEquals(ContrastSpec.translateLegacyPattern("term:basis[2]$"), "term_b2$")
    assertEquals(ContrastSpec.translateLegacyPattern("a:b:c"), "a_b_c")
    assertEquals(ContrastSpec.translateLegacyPattern("a::b:c"), "a::b_c")
  }
