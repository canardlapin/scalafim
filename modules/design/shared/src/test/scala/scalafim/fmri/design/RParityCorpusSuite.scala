package scalafim.fmri.design

import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept, NuisanceCheck}
import scalafim.fmri.design.contrast.{ContrastRegistry, ContrastSpec}
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.fixtures.RParityFixtures
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.design.hrf.HrfGenerators
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

  private def conditionMajorToBasisMajor(
      fixture: RParityFixtures.MatrixFixture,
      nConditions: Int,
      nbasis: Int
  ): RParityFixtures.MatrixFixture =
    assertEquals(fixture.cols, nConditions * nbasis)
    val out = Vector.newBuilder[Double]
    var r = 0
    while r < fixture.rows do
      var basis = 0
      while basis < nbasis do
        var cond = 0
        while cond < nConditions do
          val sourceCol = cond * nbasis + basis
          out += fixture.values(r * fixture.cols + sourceCol)
          cond += 1
        basis += 1
      r += 1
    fixture.copy(values = out.result())

  private def assertContrastClose(actual: Mat, expected: RParityFixtures.ContrastFixture, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    assertAllClose(actual.data, expected.values, tol)

  private def assertEventFixture(
      model: scalafim.fmri.design.event.EventModel,
      fixture: RParityFixtures.EventFixture,
      tol: Double,
      compareColumnNames: Boolean = true
  ): Unit =
    assertEquals(model.termKeys, fixture.termKeys)
    if compareColumnNames then
      assertEquals(model.columnNames, fixture.matrix.columnNames)
    assertMatrixClose(model.designMatrix, fixture.matrix, tol)
    fixture.rColIndices1Based.foreach { case (key, cols1Based) =>
      assertEquals(model.colIndices(key), cols1Based.map(_ - 1))
    }
    assertEquals(model.termSpans.map(_._2), fixture.rTermSpans1Based)

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

    assertEventFixture(model, fixture, tol = 1e-6)

    val attached = model.contrastWeights
    assertEquals(attached.keySet, fixture.attachedContrasts.map(_.key).toSet)
    val attachedFixture = fixture.attachedContrasts.head
    assertContrastClose(attached(attachedFixture.key).weights, attachedFixture, tol = 1e-12)

    val fWeights = model.fContrastWeights()
    assertEquals(fWeights.keySet, fixture.fContrasts.map(_.key).toSet)
    val fFixture = fixture.fContrasts.head
    assertContrastClose(fWeights(fFixture.key).weights, fFixture, tol = 1e-12)
  }

  test("R parity corpus: SPMG3 multi-basis HRF design") {
    val fixture = RParityFixtures.eventSpmg3
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "block" -> Column.Ints(Vector(1, 1, 1)),
      "cond" -> Column.Strings(Vector("A", "A", "A"))
    )

    val model = EventModelBuilder.buildWithBlockFormula(
      formula = "onset ~ hrf(cond, basis = \"spmg3\", id = task)",
      data = events,
      samplingFrame = sf,
      block = "~block"
    )

    assertEventFixture(model, fixture, tol = 1e-6)
  }

  test("R parity corpus: two-condition SPMG3 uses scalafim basis-major value ordering") {
    val fixture = RParityFixtures.eventSpmg3TwoCondition
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0, 13.0)),
      "block" -> Column.Ints(Vector(1, 1, 1, 1)),
      "cond" -> Column.Strings(Vector("A", "B", "A", "B"))
    )

    val model = EventModelBuilder.buildWithBlockFormula(
      formula = "onset ~ hrf(cond, basis = \"spmg3\", id = task)",
      data = events,
      samplingFrame = sf,
      block = "~block"
    )

    // R fmridesign currently emits two-condition multi-basis values in condition-major order,
    // while the column names are basis-major. scalafim keeps matrix values aligned to the names.
    val basisMajorFixture = fixture.copy(matrix = conditionMajorToBasisMajor(fixture.matrix, nConditions = 2, nbasis = 3))
    assertEventFixture(model, basisMajorFixture, tol = 1e-6)
  }

  test("R parity corpus: subset expression drops empty condition columns") {
    val fixture = RParityFixtures.eventSubset
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 3.0, 5.0, 7.0)),
      "block" -> Column.Ints(Vector(1, 1, 1, 1)),
      "cond" -> Column.Strings(Vector("A", "B", "A", "B")),
      "keep" -> Column.Bools(Vector(true, false, true, false))
    )

    val model = EventModelBuilder.buildWithBlockFormula(
      formula = "onset ~ hrf(cond, subset = keep, id = task)",
      data = events,
      samplingFrame = sf,
      block = "~block"
    )

    assertEventFixture(model, fixture, tol = 1e-6)
  }

  test("R parity corpus: trialwise add_sum matrix") {
    val fixture = RParityFixtures.eventTrialwiseAddSum
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "block" -> Column.Ints(Vector(1, 1, 1))
    )

    val model = EventModelBuilder.buildWithBlockFormula(
      formula = "onset ~ trialwise(add_sum = TRUE)",
      data = events,
      samplingFrame = sf,
      block = "~block"
    )

    assertEventFixture(model, fixture, tol = 1e-6, compareColumnNames = false)
    assertEquals(model.columnNames.last, fixture.matrix.columnNames.last)
  }

  test("R parity corpus: hrf_fun weighted per-event HRF design") {
    val fixture = RParityFixtures.eventWeightedHrfFun
    val sf = SamplingFrame(blockLens = Seq(12), tr = Seq(1.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 4.0)),
      "block" -> Column.Ints(Vector(1, 1)),
      "cond" -> Column.Strings(Vector("A", "B")),
      "sub_times" -> Column.DoubleLists(Vector(Vector(0.0, 1.0), Vector(0.0, 2.0))),
      "sub_weights" -> Column.DoubleLists(Vector(Vector(0.5, 0.5), Vector(0.25, 0.75)))
    )

    val model = EventModelBuilder.buildWithBlockFormula(
      formula = "onset ~ hrf(cond, hrf_fun = weighted, id = weighted)",
      data = events,
      samplingFrame = sf,
      block = "~block",
      hrfFuns = Map("weighted" -> HrfGenerators.weighted(relative = true))
    )

    assertEventFixture(model, fixture, tol = 1e-12)
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

  test("R parity corpus: nuisance drop baseline matrix") {
    val fixture = RParityFixtures.baselineNuisanceDrop
    val sf = SamplingFrame(blockLens = Seq(4, 3), tr = Seq(1.0))
    val nuisance1 = Mat.unsafe(
      rows = 4,
      cols = 3,
      data = Array(
        0.0, 0.0, 1.0,
        1.0, 1.0, 0.0,
        2.0, 2.0, 1.0,
        3.0, 3.0, 0.0
      )
    )
    val nuisance2 = Mat.unsafe(
      rows = 3,
      cols = 2,
      data = Array(
        1.0, 0.0,
        2.0, 0.0,
        3.0, 0.0
      )
    )

    val model = BaselineModel.build(
      samplingFrame = sf,
      basis = BaselineBasis.Constant,
      intercept = Intercept.Runwise,
      nuisanceList = Some(Seq(nuisance1, nuisance2)),
      nuisanceCheck = NuisanceCheck.Drop,
      nuisanceNames = Some(Seq(Seq("dvars", "dvars_dup", "motion"), Seq("motion_x", "zero_col")))
    )

    assertEquals(model.termKeys, fixture.termKeys)
    assertEquals(model.columnNames, fixture.matrix.columnNames)
    assertMatrixClose(model.designMatrix, fixture.matrix, tol = 1e-12)
  }
