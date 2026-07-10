package scalafim.fmri.design.scenarios

import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.{ConvolvedTerm, EventModel}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.design.linalg.QrDecomposition
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

class MixedTrMultirunScenarioSuite extends munit.FunSuite:
  private val ScenarioId = "design.mixed-tr-multirun.v1"
  private val Formula = "onset ~ hrf(cond)"
  private val Tol = ScenarioTolerance.mixed(1e-9, 1e-9)

  test("mixed-TR multi-run design preserves run-local sampling, baselines, and condition columns") {
    val result = runScenario()

    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val fixture = MixedTrFixture()
    val model = fixture.concatenatedModel
    val term = convolved(model)
    val baseline = fixture.baseline
    val stitched = fixture.stitchedRunwiseDesign
    val uniform = fixture.uniformTrCanaryModel
    val fullDesign = model.designMatrix ++ baseline.designMatrix

    val observations =
      Vector(
        ScenarioHarness.fact(
          "term keys",
          model.termKeys == Vector("cond"),
          s"actual=${model.termKeys.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "condition columns",
          model.columnNames == Vector("cond_cond.A", "cond_cond.B"),
          s"actual=${model.columnNames.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "term block ids",
          term.term.blockIds0 == Vector(0, 0, 1, 1),
          s"actual=${term.term.blockIds0.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "baseline columns",
          baseline.columnNames == Vector("base_constant1_block_1", "base_constant1_block_2"),
          s"actual=${baseline.columnNames.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "condition A has signal in both runs",
          fixture.hasSignalInRun(model.designMatrix, col = 0, run = 0) &&
            fixture.hasSignalInRun(model.designMatrix, col = 0, run = 1),
          s"nonzeroRows=${fixture.nonZeroRows(model.designMatrix, col = 0).mkString(",")}"
        ),
        ScenarioHarness.fact(
          "condition B has signal in both runs",
          fixture.hasSignalInRun(model.designMatrix, col = 1, run = 0) &&
            fixture.hasSignalInRun(model.designMatrix, col = 1, run = 1),
          s"nonzeroRows=${fixture.nonZeroRows(model.designMatrix, col = 1).mkString(",")}"
        ),
        ScenarioHarness.fact(
          "uniform-TR canary differs in second run",
          fixture.maxAbsDiffRows(model.designMatrix, uniform.designMatrix, fixture.rowsForRun(1)) > 1e-3,
          s"maxDiff=${fixture.maxAbsDiffRows(model.designMatrix, uniform.designMatrix, fixture.rowsForRun(1))}"
        ),
        ScenarioHarness.fact(
          "baseline rank",
          rank(baseline.designMatrix) == baseline.designMatrix.cols,
          s"rank=${rank(baseline.designMatrix)} cols=${baseline.designMatrix.cols}"
        ),
        ScenarioHarness.fact(
          "full design rank",
          rank(fullDesign) == fullDesign.cols,
          s"rank=${rank(fullDesign)} cols=${fullDesign.cols}"
        )
      ) ++
        ScenarioHarness.values("TRs", fixture.samplingFrame.tr.map(_.value), Vector(2.0, 0.8), Tol) ++
        ScenarioHarness.values("run 1 local samples", fixture.localSamples(0), fixture.expectedLocalSamples(0), Tol) ++
        ScenarioHarness.values("run 2 local samples", fixture.localSamples(1), fixture.expectedLocalSamples(1), Tol) ++
        ScenarioHarness.values("run 2 global samples", fixture.globalSamples(1), fixture.expectedGlobalSamples(1), Tol) ++
        ScenarioHarness.matrix("concatenated design equals stitched per-run oracle", model.designMatrix, stitched, Tol) ++
        ScenarioHarness.matrix("runwise baseline", baseline.designMatrix, fixture.expectedRunwiseBaseline, Tol) ++
        Vector(
          ScenarioHarness.finite("event design finite", model.designMatrix.data),
          ScenarioHarness.finite("baseline design finite", baseline.designMatrix.data)
        )

    ScenarioHarness.result(ScenarioId, observations)

  private def convolved(model: EventModel): ConvolvedTerm =
    model.terms.headOption.map(_._2) match
      case Some(term: ConvolvedTerm) => term
      case Some(other)               => fail(s"expected ConvolvedTerm, found $other")
      case None                      => fail("expected one convolved term")

  private def rank(mat: Mat): Int =
    QrDecomposition.decompose(
      mat.data.clone(),
      rows = mat.rows,
      cols = mat.cols,
      pivoting = true,
      tol = BaselineModel.DefaultNuisanceTol
    ).rank

  private final case class MixedTrFixture():
    val samplingFrame: SamplingFrame =
      SamplingFrame(
        blockLens = Seq(8, 12),
        tr = Seq(2.0, 0.8),
        startTime = Seq(0.0, 0.0),
        precision = 0.1
      )

    private val runLabels = Vector("run-1", "run-1", "run-2", "run-2")
    private val onsets = Vector(0.0, 4.0, 0.0, 2.4)
    private val conditions = Vector("A", "B", "A", "B")

    def concatenatedModel: EventModel =
      EventModelBuilder.buildWithBlockFormula(
        formula = Formula,
        data = allEvents,
        samplingFrame = samplingFrame,
        block = "~run",
        precision = samplingFrame.precision
      )

    def uniformTrCanaryModel: EventModel =
      EventModelBuilder.buildWithBlockFormula(
        formula = Formula,
        data = allEvents,
        samplingFrame = SamplingFrame(
          blockLens = samplingFrame.blockLens,
          tr = Seq(2.0, 2.0),
          startTime = samplingFrame.startTime.map(_.value),
          precision = samplingFrame.precision.value
        ),
        block = "~run",
        precision = samplingFrame.precision
      )

    def baseline: BaselineModel =
      BaselineModel.build(
        samplingFrame = samplingFrame,
        basis = BaselineBasis.Constant,
        intercept = Intercept.Runwise
      )

    def stitchedRunwiseDesign: Mat =
      stitchRows(singleRunModel(0).designMatrix, singleRunModel(1).designMatrix)

    def expectedRunwiseBaseline: Mat =
      Mat.fromRows(
        Vector.fill(samplingFrame.blockLens(0))(Vector(1.0, 0.0)) ++
          Vector.fill(samplingFrame.blockLens(1))(Vector(0.0, 1.0))
      )

    def localSamples(run: Int): Vector[Double] =
      samplingFrame.samples(blocks = Seq(run), global = false).map(_.value)

    def globalSamples(run: Int): Vector[Double] =
      samplingFrame.samples(blocks = Seq(run), global = true).map(_.value)

    def expectedLocalSamples(run: Int): Vector[Double] =
      Vector.tabulate(samplingFrame.blockLens(run))(i => i.toDouble * samplingFrame.tr(run).value)

    def expectedGlobalSamples(run: Int): Vector[Double] =
      val offset =
        samplingFrame.blockLens.zip(samplingFrame.tr).take(run).map { case (len, tr) =>
          len.toDouble * tr.value
        }.sum
      expectedLocalSamples(run).map(_ + offset)

    def rowsForRun(run: Int): Range =
      val start = samplingFrame.blockLens.take(run).sum
      start until (start + samplingFrame.blockLens(run))

    def hasSignalInRun(matrix: Mat, col: Int, run: Int): Boolean =
      rowsForRun(run).exists(row => math.abs(matrix(row, col)) > 1e-12)

    def nonZeroRows(matrix: Mat, col: Int): Vector[Int] =
      (0 until matrix.rows).filter(row => math.abs(matrix(row, col)) > 1e-12).toVector

    def maxAbsDiffRows(left: Mat, right: Mat, rows: Range): Double =
      require(left.cols == right.cols, "matrix column counts must match")
      require(left.rows == right.rows, "matrix row counts must match")
      var max = 0.0
      rows.foreach { row =>
        var col = 0
        while col < left.cols do
          max = math.max(max, math.abs(left(row, col) - right(row, col)))
          col += 1
      }
      max

    private def allEvents: DataTable =
      DataTable.fromColumns(
        "onset" -> Column.Doubles(onsets),
        "cond" -> Column.Strings(conditions),
        "run" -> Column.Strings(runLabels)
      )

    private def singleRunModel(run: Int): EventModel =
      val label = s"run-${run + 1}"
      val rows = runLabels.indices.filter(i => runLabels(i) == label).toVector
      val data =
        DataTable.fromColumns(
          "onset" -> Column.Doubles(rows.map(onsets)),
          "cond" -> Column.Strings(rows.map(conditions))
        )
      EventModelBuilder.build(
        formula = Formula,
        data = data,
        samplingFrame = SamplingFrame(
          blockLens = Seq(samplingFrame.blockLens(run)),
          tr = Seq(samplingFrame.tr(run).value),
          startTime = Seq(samplingFrame.startTime(run).value),
          precision = samplingFrame.precision.value
        ),
        blockIds = Vector.fill(rows.length)(0),
        precision = samplingFrame.precision
      )

    private def stitchRows(first: Mat, second: Mat): Mat =
      require(first.cols == second.cols, "stitched matrices must have the same columns")
      Mat.unsafe(first.rows + second.rows, first.cols, first.data ++ second.data)
