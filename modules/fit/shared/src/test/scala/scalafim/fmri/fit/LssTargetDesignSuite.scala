package scalafim.fmri.fit

import gale.linalg.DMat
import scalafim.fmri.design.ColumnId

class LssTargetDesignSuite extends munit.FunSuite:
  private def mat(rows: Vector[Vector[Double]]): DMat = GaleTestMatrix.fromRows(rows)
  private def id(value: String): ColumnId = ColumnId.unsafe(value)

  test("selected target retains identities, row provenance, fixed columns, and sums term peers"):
    val trials = LssTrialDesign.unsafe(mat(Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9))), Vector("a", "b", "c"))
    val fixed = LssFixedDesign.unsafe(mat(Vector(Vector(10), Vector(11), Vector(12))), Vector("motion"))
    val term = LssTermDesign.make(trials, fixed, Vector(LssTrialIdentity("a", id("trial_a")), LssTrialIdentity("b", id("trial_b")), LssTrialIdentity("c", id("trial_c"))), Vector(20, 22, 25)).toOption.getOrElse(fail("term"))
    val selected = LssTargetDesign.select(term, "b").toOption.getOrElse(fail("target"))
    assertEquals(selected.target.sourceColumnId, id("trial_b"))
    assertEquals(selected.rowProvenance, Vector(20, 22, 25))
    assertEquals(selected.sourceColumnIds, Vector(id("trial_a"), id("trial_b"), id("trial_c")))
    val expected = mat(Vector(Vector(2, 4, 10), Vector(5, 10, 11), Vector(8, 16, 12)))
    var row = 0
    while row < expected.rows do
      var col = 0
      while col < expected.cols do
        assertEqualsDouble(selected.matrix(row, col), expected(row, col), 0.0)
        col += 1
      row += 1

  test("target selection fails explicitly for absent or duplicate identities"):
    val trials = LssTrialDesign.unsafe(mat(Vector(Vector(1, 2), Vector(3, 4))), Vector("a", "b"))
    val fixed = LssFixedDesign.empty(2)
    assert(LssTermDesign.make(trials, fixed, Vector(LssTrialIdentity("a", id("x")), LssTrialIdentity("a", id("y"))), Vector(0, 1)).isLeft)
    val term = LssTermDesign.make(trials, fixed, Vector(LssTrialIdentity("a", id("x")), LssTrialIdentity("b", id("y"))), Vector(0, 1)).toOption.getOrElse(fail("term"))
    assertEquals(LssTargetDesign.select(term, "missing").left.toOption, Some(LssTargetDesignError.MissingTarget("missing", Vector("a", "b"))))
