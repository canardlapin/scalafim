package scalafim.fmri.mvpa.pattern

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import gale.optim.{FirstOrderConfig, FirstOrderTolerance}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import munit.FunSuite
import scalafim.fmri.mvpa.{AxisRef, EvidenceSource, MultiResponse, Observations}
import scalafim.fmri.mvpa.analysis.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

/** Independent contracts for the training-only pilot/covariance/final adapter. */
class TwoStagePatternFitSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)

  private def axis(name: String, role: SpaceRole, n: Int): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, role, Vector.tabulate(n)(index => s"$name-$index"), "two-stage", "one", "raw"))

  private def source(name: String): EvidenceSource =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))

  private def value(name: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(name))
  private def strict = right(FirstOrderConfig.from(100000, right(FirstOrderTolerance.from(1e-10, 1e-10))))
  private def structured = right(StructuredPatternConfig(right(SupportPenalty(0.0, 0.0)), maximumOuterIterations = 250, inner = strict,
    stationarityTolerance = 1e-7, objectiveTolerance = 1e-9, maximumWorkspaceCells = 10000000L, maximumOperatorColumns = 100000L))
  private def covariance(rank: Int = 1) = right(ResidualCovarianceFitPolicy(rank, maximumIterations = 5000, tolerance = 1e-9,
    diagonalMomentTolerance = 1e-7, centeringTolerance = 1e-10, convergence = ConvergencePolicy.Refuse))
  private def policy(maximumResidualCells: Long = 10000000L, rank: Int = 1) = TwoStagePatternFitPolicy(structured, covariance(rank), maximumResidualCells)

  private final class Counted(matrix: Option[DMat]) extends DoubleLinearOperator:
    val rows = matrix.fold(8)(_.rows)
    val cols = matrix.fold(3)(_.cols)
    var forwardColumns = 0L
    var adjointColumns = 0L
    def applyTo(input: DVec, output: MutableDVec): Unit =
      forwardColumns += 1L
      matrix.fold(throw new IllegalStateException("poison source read"))(_.applyTo(input, output))
    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      adjointColumns += 1L
      matrix.fold(throw new IllegalStateException("poison source read"))(_.transposeApplyTo(input, output))

  /** Centered Walsh residual columns are orthogonal to centered y and have
    * nonzero diagonal covariance; the requested factor-analysis noise rank is one.
    */
  private final class Fixture(noiseScale: Double = 0.4, operator: Option[Counted] = None):
    val samples: AxisRef[String] = axis("sample", SpaceRole.Samples, 8)
    val neural: AxisRef[String] = axis("neural", SpaceRole.Observed, 3)
    val target: AxisRef[String] = axis("target", SpaceRole.Observed, 1)
    val components: AxisRef[String] = axis("component", SpaceRole.Observed, 1)
    private val y = Vector(1.0, 1.0, 1.0, 1.0, -1.0, -1.0, -1.0, -1.0)
    private val z1 = Vector(1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0)
    private val z2 = Vector(1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0)
    private val z3 = Vector(1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0, 1.0)
    val responseMatrix = DMat.dense(8, 1, y)
    val observationMatrix = DMat.dense(8, 3, (0 until 8).flatMap: row =>
      Vector(2.0 * y(row) + noiseScale * z1(row), -1.5 * y(row) + 0.3 * z2(row), y(row) + 0.2 * z3(row)))
    val observations = operator match
      case Some(counted) => right(Observations.fromOperator(samples, neural, counted, value("x"), source("x")))
      case None => right(Observations.fromDense(samples, neural, observationMatrix, value("x"), source("x")))
    val responses = right(MultiResponse.fromDense(samples, target, responseMatrix, value("y"), source("y")))
    val support = right(SupportGraph(neural.descriptor, Vector.empty, SupportTopology.Declared("isolated"), "one"))
    private val unit = right(AxisValues(target, Vector(1.0)))
    val geometry = right(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit)))
    val binding = right(TrainingBinding(samples.descriptor, "two-stage", "sha256:two-stage"))
    def fit(replay: PatternReplay = PatternReplay.Repeatable("two-stage"), fitPolicy: TwoStagePatternFitPolicy = policy(), resourcePolicy: TwoStagePatternResources = TwoStagePatternResources()) =
      TwoStagePatternFit.fit(samples, neural, target, components)(observations, responses, support, geometry,
        CenteringPolicy.CenteredBeforeFit("x-centered", "y-centered"), binding, Vector("two-stage"), replay, fitPolicy, resourcePolicy)

  private def assertMatrix(actual: DMat, expected: DMat): Unit =
    assertEquals((actual.rows, actual.cols), (expected.rows, expected.cols))
    for row <- 0 until actual.rows; column <- 0 until actual.cols do assertEqualsDouble(actual(row, column), expected(row, column), 1e-12)

  test("training residuals stream one neural column and equal independently calculated X minus Y C A transpose"):
    val samples = axis("residual-samples", SpaceRole.Samples, 4)
    val neural = axis("residual-neural", SpaceRole.Observed, 3)
    val target = axis("residual-target", SpaceRole.Observed, 2)
    val x = DMat.dense(4, 3, Vector(3.0, 5.0, 7.0, 11.0, 13.0, 17.0, 19.0, 23.0, 29.0, 31.0, 37.0, 41.0))
    val counted = new Counted(Some(x))
    val observations = right(Observations.fromOperator(samples, neural, counted, value("residual-x"), source("residual-x")))
    val responses = right(MultiResponse.fromDense(samples, target, DMat.dense(4, 2, Vector(2.0, 1.0, 3.0, -1.0, 5.0, 2.0, 7.0, -2.0)), value("residual-y"), source("residual-y")))
    val c = DMat.dense(2, 2, Vector(1.0, 2.0, -1.0, 0.5))
    val a = DMat.dense(3, 2, Vector(1.0, 2.0, -3.0, 1.0, 0.5, -2.0))
    val yc = responses.targets(c).fold(error => fail(error.toString), identity)
    val expected = DMat.tabulate(4, 3)((row, column) => x(row, column) - (0 until 2).map(k => yc(row, k) * a(column, k)).sum)
    assertMatrix(right(TwoStagePatternFit.trainingResiduals(observations, responses, a, c)), expected)
    assertEquals((counted.forwardColumns, counted.adjointColumns), (3L, 0L))

  test("two-stage fit returns converged pilot covariance and final artifacts with bounded real work"):
    val result = right(new Fixture().fit())
    assertEquals(result.pilot.stopping, StructuredPatternStopping.Converged)
    assert(result.pilot.artifact.nonEmpty)
    assert(result.covarianceFit.receipt.converged)
    assertEquals(result.finalFit.stopping, StructuredPatternStopping.Converged)
    assert(result.finalFit.artifact.nonEmpty)
    assertEquals(result.work, TwoStagePatternFitWork(2, 1, 1, 24L, 24L, 8L))
    assert(result.residualReceipt.nonEmpty)

  test("budget replay axis and noise-rank preflights reject poison observations before reads"):
    val poisonBudget = new Counted(None)
    assert(new Fixture(operator = Some(poisonBudget)).fit(fitPolicy = policy(maximumResidualCells = 0L)).isLeft)
    assertEquals((poisonBudget.forwardColumns, poisonBudget.adjointColumns), (0L, 0L))
    val poisonReplay = new Counted(None)
    assert(new Fixture(operator = Some(poisonReplay)).fit(replay = PatternReplay.SinglePass).isLeft)
    assertEquals((poisonReplay.forwardColumns, poisonReplay.adjointColumns), (0L, 0L))
    val poisonRank = new Counted(None)
    assert(new Fixture(operator = Some(poisonRank)).fit(fitPolicy = policy(rank = 2)).isLeft)
    assertEquals((poisonRank.forwardColumns, poisonRank.adjointColumns), (0L, 0L))
    val fixture = new Fixture()
    val foreignNeural = axis("foreign-neural", SpaceRole.Observed, 3)
    val poisonAxis = new Counted(None)
    val foreign = right(Observations.fromOperator(fixture.samples, foreignNeural, poisonAxis, value("foreign-x"), source("foreign-x")))
    val forged = foreign.asInstanceOf[Observations[fixture.samples.Id, fixture.neural.Id]]
    assert(TwoStagePatternFit.fit(fixture.samples, fixture.neural, fixture.target, fixture.components)(forged, fixture.responses, fixture.support,
      fixture.geometry, CenteringPolicy.CenteredBeforeFit("x-centered", "y-centered"), fixture.binding, Vector("two-stage"), PatternReplay.Repeatable("two-stage"), policy()).isLeft)
    assertEquals((poisonAxis.forwardColumns, poisonAxis.adjointColumns), (0L, 0L))

  test("training perturbation changes the framed content residual receipt"):
    val baseline = right(new Fixture(0.4).fit())
    val perturbed = right(new Fixture(0.6).fit())
    assertNotEquals(baseline.residualReceipt, perturbed.residualReceipt)

  test("mandatory whole numeric admission refuses unknown providers before any fitting or reads"):
    val poison = new Counted(None)
    val fixture = new Fixture(operator = Some(poison))
    val resources = TwoStagePatternResources(budget = ResourceBudget(ResourceLimit.WholeNumeric(Long.MaxValue), MaterializationPolicy.ForbidSourceCopy))
    assert(fixture.fit(resourcePolicy = resources).left.toOption.exists:
      case TwoStagePatternFitError.Resource(ObservationProductError.Resource(_: ResourceError.UnknownStrictCost)) => true
      case _ => false)
    assertEquals((poison.forwardColumns, poison.adjointColumns), (0L, 0L))

  test("owned and direct two-stage routes preserve the fitted object and report actual distinct reads"):
    val matrix = new Fixture().observationMatrix
    val directSource = new Counted(Some(matrix))
    val denseSource = new Counted(Some(matrix))
    val fixture = new Fixture(operator = Some(directSource))
    val direct = right(fixture.fit())
    val dense = right(new Fixture(operator = Some(denseSource)).fit(resourcePolicy = TwoStagePatternResources(
      budget = ResourceBudget(ResourceLimit.OwnedNumeric(Long.MaxValue), MaterializationPolicy.AllowSourceCopy(192)),
      route = ObservationProductRoute.OwnedDenseCopy)))
    assertMatrix(dense.finalFit.factors.neuralByComponent, direct.finalFit.factors.neuralByComponent)
    assertMatrix(dense.finalFit.factors.targetByComponent, direct.finalFit.factors.targetByComponent)
    assertMatrix(dense.covarianceFit.covariance.loadingsMatrix, direct.covarianceFit.covariance.loadingsMatrix)
    dense.covarianceFit.covariance.diagonalValues.zip(direct.covarianceFit.covariance.diagonalValues).foreach((actual, expected) => assertEqualsDouble(actual, expected, 1e-12))
    assertEquals(dense.residualReceipt, direct.residualReceipt)
    assertEquals(dense.resources.binding, direct.resources.binding)
    val work = direct.resources.observationWork
    assertEquals(work.forwardColumns, direct.pilot.work.forwardColumns + direct.finalFit.work.forwardColumns + 3)
    assertEquals(work.adjointColumns, direct.pilot.work.adjointColumns + direct.finalFit.work.adjointColumns)
    assertEquals(work.returnedCells, work.forwardColumns * 8 + work.adjointColumns * 3)
    assertEquals(work.fitCalls, 3L)
    assertEquals((directSource.forwardColumns, directSource.adjointColumns), (work.forwardColumns, work.adjointColumns))
    assertEquals((denseSource.forwardColumns, denseSource.adjointColumns), (3L, 0L))
    assertEquals(dense.resources.observationWork.fitCalls, 3L)
    assertEquals(dense.resources.observationWork.materializationForwardColumns, 3L)
    assertEquals(dense.resources.observationWork.copiedCells, 24L)
    assertEquals(dense.resources.observationWork.materializationReturnedCells, 24L)
    assert(direct.resources.admission.unknownCosts.nonEmpty)
    assertEquals(direct.resources.admission.wholeNumericBytes, None)
    assert(direct.resources.structuredWorkspaceCellsUpperBound >= direct.pilot.work.plannedWorkspaceCells)
    assert(direct.resources.structuredWorkspaceCellsUpperBound >= direct.finalFit.work.plannedWorkspaceCells)
    assertEquals(direct.resources.retainedFactorCells, 14L)

  test("full provider declarations admit the same fit without changing scientific population or metric"):
    val sourceX = ResourceBound.Known(192, "synthetic 8-by-3 binary64 matrix")
    val sourceY = ResourceBound.Known(64, "synthetic 8-by-1 binary64 matrix")
    val xScratch = ResourceBound.Known(512, "fixture declared provider scratch")
    val yScratch = ResourceBound.Known(128, "fixture declared response scratch")
    val backend = ResourceBound.Known(4096, "fixture declared numerical backend scratch")
    val resources = TwoStagePatternResources(
      budget = ResourceBudget(ResourceLimit.WholeNumeric(Long.MaxValue), MaterializationPolicy.ForbidSourceCopy),
      observations = ObservationProviderCosts(sourceX, xScratch), responseSource = sourceY,
      responseScratch = yScratch, numericalScratch = backend)
    val result = right(new Fixture().fit(resourcePolicy = resources))
    assert(result.resources.admission.wholeNumericBytes.nonEmpty)
    assertEquals(result.resources.admission.unknownCosts, Vector.empty)
    // Independent sum: source 192+64, provider 512+128+4096,
    // enclosing vector-width application buffers (8+3)*1*8=88.
    assertEquals(result.resources.admission.wholeNumericBytes.get - result.resources.admission.ownedNumericBytes, 5080L)
    assertEquals(result.work.structuredFits, 2)
    assertEquals(result.work.covarianceFitCalls, 1)

  test("dense route reuses complete structured admission before acquisition or poison reads"):
    val poison = new Counted(None)
    val fixture = new Fixture(operator = Some(poison))
    var acquired = 0
    val reader = new ObservationProductResource:
      def acquire() =
        acquired += 1
        Right(())
      def close() = Right(())
    val tooFewColumns = right(StructuredPatternConfig(right(SupportPenalty(0.0, 0.0)), maximumOperatorColumns = 0L))
    val resources = TwoStagePatternResources(
      budget = ResourceBudget(ResourceLimit.OwnedNumeric(Long.MaxValue), MaterializationPolicy.AllowSourceCopy(192)),
      route = ObservationProductRoute.OwnedDenseCopy, resource = reader)
    assert(fixture.fit(fitPolicy = policy().copy(structured = tooFewColumns), resourcePolicy = resources).left.toOption.exists:
      case TwoStagePatternFitError.Structured("pilot admission", _: StructuredPatternError.Budget) => true
      case _ => false)
    assertEquals(acquired, 0)
    assertEquals((poison.forwardColumns, poison.adjointColumns), (0L, 0L))

  test("final high-noise-rank precision shape overflow refuses using only metadata"):
    // Independent shape arithmetic: X=800040000 and q*q=1600000000 fit,
    // but final augmented precision is 60000*40000=2400000000 cells.
    assert(BigInt(20001) * 40000 <= Int.MaxValue)
    assert(BigInt(40000) * 40000 <= Int.MaxValue)
    assert(BigInt(60000) * 20000 <= Int.MaxValue)
    assert(BigInt(60000) * 40000 > Int.MaxValue)
    assertEquals(TwoStagePatternFit.admitAllocationShapes(20001, 40000, 40000, 40000, 20000),
      Left(TwoStagePatternFitError.Admission("numeric allocation shape exceeds Int capacity")))
    assertEquals(TwoStagePatternFit.admitAllocationShapes(8, 3, 1, 1, 1), Right(()))
