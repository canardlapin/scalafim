package scalafim.fmri.design

import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.contrast.{ContrastRegistry, ContrastSpec}
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.fixtures.RParityFixtures
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

class RParityCorpusSuite extends munit.FunSuite:

  private def assertAllClose(actual: Array[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      assert(math.abs(actual(i) - expected(i)) <= tol, clues(s"i=$i actual=${actual(i)} expected=${expected(i)}"))
      i += 1

  private def assertMatrixClose(actual: Mat, expected: RParityFixtures.MatrixFixture, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    assertAllClose(actual.data, expected.values, tol)

  test("R parity corpus: event interaction design, metadata, and contrasts") {
    val fixture = RParityFixtures.eventInteraction
    val sf = SamplingFrame(blockLens = Seq(12), tr = Seq(1.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "block" -> Column.Ints(Vector(1, 1, 1)),
      "cond" -> Column.Strings(Vector("A", "B", "A")),
      "x" -> Column.Doubles(Vector(1.0, 2.0, 3.0))
    )
    val cset = ContrastSpec.ContrastSet(
      ContrastSpec.Column(
        name = "diff",
        patternA = "cond\\.A".r,
        patternB = Some("cond\\.B".r)
      )
    )

    val model = EventModelBuilder.buildWithBlockFormula(
      formula = "onset ~ hrf(cond, x, name = task, contrasts = myset)",
      data = events,
      samplingFrame = sf,
      block = "~block",
      contrastSets = Map("myset" -> cset)
    )

    import ContrastRegistry.*

    assertEquals(model.termKeys, fixture.termKeys)
    assertEquals(model.columnNames, fixture.matrix.columnNames)
    assertMatrixClose(model.designMatrix, fixture.matrix, tol = 1e-6)
    assertEquals(model.colIndices("task"), fixture.rColIndices1Based("task").map(_ - 1))
    assertEquals(model.termSpans.map(_._2), fixture.rTermSpans1Based)

    val attached = model.contrastWeights
    val attachedKey = fixture.attachedContrastKeys.head.replaceFirst("\\.", "#")
    assertEquals(attached.keySet, Set(attachedKey))
    assertAllClose(attached(attachedKey).weights.data, fixture.attachedContrastWeights, tol = 1e-12)

    val fWeights = model.fContrastWeights()
    assertEquals(fWeights.keySet, fixture.fContrastKeys.toSet)
    assertAllClose(fWeights(fixture.fContrastKeys.head).weights.data, fixture.fContrastWeights, tol = 1e-12)
  }

  test("R parity corpus: constant runwise baseline matrix") {
    val fixture = RParityFixtures.baselineConstantRunwise
    val sf = SamplingFrame(blockLens = Seq(3, 2), tr = Seq(1.0))
    val model = BaselineModel.build(
      samplingFrame = sf,
      basis = BaselineBasis.Constant,
      intercept = Intercept.Runwise
    )

    assertEquals(model.termKeys, fixture.termKeys)
    assertEquals(model.columnNames, fixture.matrix.columnNames)
    assertMatrixClose(model.designMatrix, fixture.matrix, tol = 1e-12)
  }
