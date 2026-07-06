package scalafim.fmri.design

import scalafim.fmri.design.contrast.*
import scalafim.fmri.design.event.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

class ContrastSetGeneratorsSuite extends munit.FunSuite:

  private def assertAllClose(actual: Array[Double], expected: Array[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      val a = actual(i)
      val e = expected(i)
      assert(clue(math.abs(a - e)) <= tol, clues(s"index=$i a=$a e=$e"))
      i += 1

  test("pairwiseContrasts generates all pairs in order") {
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
    assertEquals(conv.columnNames, Vector("t_condition.A", "t_condition.B", "t_condition.C"))

    val cs = ConvolvedContrastWeights.pairwiseContrasts(
      term = conv,
      levels = Vector("A", "B", "C"),
      facName = "condition"
    )

    assertEquals(cs.map(_.name), Vector("con_A_B", "con_A_C", "con_B_C"))
    assertAllClose(cs(0).weights.data, Array(1.0, -1.0, 0.0), tol = 1e-12)
    assertAllClose(cs(1).weights.data, Array(1.0, 0.0, -1.0), tol = 1e-12)
    assertAllClose(cs(2).weights.data, Array(0.0, 1.0, -1.0), tol = 1e-12)
  }

  test("oneAgainstAllContrasts weights 1 vs mean(other)") {
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

    val cs = ConvolvedContrastWeights.oneAgainstAllContrasts(
      term = conv,
      levels = Vector("A", "B", "C"),
      facName = "condition"
    )

    assertEquals(cs.map(_.name), Vector("con_A_vs_other", "con_B_vs_other", "con_C_vs_other"))
    assertAllClose(cs(0).weights.data, Array(1.0, -0.5, -0.5), tol = 1e-12)
    assertAllClose(cs(1).weights.data, Array(-0.5, 1.0, -0.5), tol = 1e-12)
    assertAllClose(cs(2).weights.data, Array(-0.5, -0.5, 1.0), tol = 1e-12)
  }

  test("slidingWindowContrasts generates disjoint adjacent windows") {
    val conds = Vector("A", "B", "C", "D", "E")
    val onsets = (1 to conds.length).map(_.toDouble).toVector.map(Seconds(_))
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0))

    val term = EventTerm(
      events = Vector(Event.factor(conds, "condition")),
      onsets = onsets,
      blockIds = Vector.fill(conds.length)(0),
      termTag = Some("t")
    )

    val conv = term.convolve(Hrfs.SPMG1, sf)
    assertEquals(conv.columnNames, Vector("t_condition.A", "t_condition.B", "t_condition.C", "t_condition.D", "t_condition.E"))

    val cs = ConvolvedContrastWeights.slidingWindowContrasts(
      term = conv,
      levels = Vector("A", "B", "C", "D", "E"),
      facName = "condition",
      windowSize = 2,
      namePrefix = "win"
    )

    assertEquals(cs.map(_.name), Vector("win_A-B_vs_C-D", "win_B-C_vs_D-E"))
    assertAllClose(cs(0).weights.data, Array(0.5, 0.5, -0.5, -0.5, 0.0), tol = 1e-12)
    assertAllClose(cs(1).weights.data, Array(0.0, 0.5, 0.5, -0.5, -0.5), tol = 1e-12)
  }
