package scalafim.fmri.mvpa.pattern

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import gale.optim.{FirstOrderConfig, FirstOrderTolerance}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import munit.FunSuite
import resample4s.core.{IndexSpace, Injection}
import scalafim.fmri.mvpa.{AxisRef, EvidenceSource, MultiResponse, Observations, ReindexingLeg}
import scalafim.response.{Provenance, ProvenanceId, SourceId}

/** Independent qualification boundary for whole-brain pattern artifacts.
  * It covers this adapter's declared finite, centered, repeatable inputs only.
  */
class WholeBrainPatternQualificationSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, role: SpaceRole, n: Int) =
    right(AxisRef.fromStableKeys(name, role, Vector.tabulate(n)(i => s"$name-$i"), "qualification", "one", "raw"))
  private def value(name: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(name))
  private def source(name: String): EvidenceSource =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))
  private def strict = right(FirstOrderConfig.from(100000, right(FirstOrderTolerance.from(1e-10, 1e-10))))
  private def structured(maximumTargetDimension: Int = 64, workspace: Long = 10000000L, columns: Long = 100000L) =
    right(StructuredPatternConfig(right(SupportPenalty(0.0, 0.0)), maximumOuterIterations = 250, inner = strict,
      stationarityTolerance = 1e-7, objectiveTolerance = 1e-9, maximumWorkspaceCells = workspace,
      maximumOperatorColumns = columns, maximumTargetDimension = maximumTargetDimension))
  private def covariance(rank: Int = 1, iterations: Int = 5000, tolerance: Double = 1e-9) =
    right(ResidualCovarianceFitPolicy(rank, maximumIterations = iterations, tolerance = tolerance,
      diagonalMomentTolerance = 1e-7, centeringTolerance = 1e-10, convergence = ConvergencePolicy.Refuse))

  private final class Counted(val matrix: DMat) extends DoubleLinearOperator:
    val rows = matrix.rows
    val cols = matrix.cols
    var forwardColumns = 0L
    var adjointColumns = 0L
    def applyTo(input: DVec, output: MutableDVec): Unit =
      forwardColumns += 1L
      matrix.applyTo(input, output)
    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      adjointColumns += 1L
      matrix.transposeApplyTo(input, output)

  private final class Poison(val rows: Int, val cols: Int) extends DoubleLinearOperator:
    var reads = 0
    def applyTo(input: DVec, output: MutableDVec): Unit =
      reads += 1
      throw new IllegalStateException("metadata admission must not read evidence")
    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      reads += 1
      throw new IllegalStateException("metadata admission must not read evidence")

  private final class FitFixture(categorical: Boolean = false):
    val samples = axis("sample", SpaceRole.Samples, 8)
    val neural = axis("neural", SpaceRole.Observed, 3)
    val target = axis("target", SpaceRole.Observed, 1)
    val components = axis("component", SpaceRole.Observed, 1)
    val y = Vector(1.0, 1.0, 1.0, 1.0, -1.0, -1.0, -1.0, -1.0)
    private val z1 = Vector(1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0)
    private val z2 = Vector(1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0)
    private val z3 = Vector(1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0, 1.0)
    val operator = new Counted(DMat.dense(8, 3, (0 until 8).flatMap: row =>
      Vector(2.0 * y(row) + .4 * z1(row), -1.5 * y(row) + .3 * z2(row), y(row) + .2 * z3(row))))
    val observations = right(Observations.fromOperator(samples, neural, operator, value("x"), source("x")))
    val responses = right(MultiResponse.fromDense(samples, target, DMat.dense(8, 1, y), value("y"), source("y")))
    val support = right(SupportGraph(neural.descriptor, Vector.empty, SupportTopology.Declared("isolated voxels"), "one"))
    private val unit = right(AxisValues(target, Vector(1.0)))
    val geometry =
      if categorical then
        val conditions = axis("class", SpaceRole.Observed, 2)
        right(TargetGeometry.categorical(conditions, target, DMat.dense(2, 1, Vector(1.0, -1.0)), right(AxisValues(conditions, Vector(.5, .5)))))
      else right(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit)))
    val binding = right(TrainingBinding(samples.descriptor, "qualification", "sha256:qualification"))
    def fit = TwoStagePatternFit.fit(samples, neural, target, components)(observations, responses, support, geometry,
      CenteringPolicy.CenteredBeforeFit("x-centered", "y-centered"), binding, Vector("qualification"),
      PatternReplay.Repeatable("qualification"), TwoStagePatternFitPolicy(structured(), covariance(), 10000000L), TwoStagePatternResources())
    def select(ordinals: Int*) = right(ReindexingLeg.bind(neural, right(Injection.from(IArray.from(ordinals), right(IndexSpace.of(neural.size))))))

  test("a two-stage artifact supplies independently checked Gaussian, encoding, categorical, and ROI heads without another provider lifecycle"):
    def densePsi(covariance: ResidualCovariance[String]): DMat =
      val diagonal = covariance.diagonalValues
      val loadings = covariance.loadingsMatrix
      DMat.tabulate(3, 3): (row, column) =>
        (if row == column then diagonal(row) else 0.0) + (0 until covariance.rank).map(component => loadings(row, component) * loadings(column, component)).sum
    def inverse3(matrix: DMat): DMat =
      val d = matrix(0, 0) * (matrix(1, 1) * matrix(2, 2) - matrix(1, 2) * matrix(2, 1)) -
        matrix(0, 1) * (matrix(1, 0) * matrix(2, 2) - matrix(1, 2) * matrix(2, 0)) +
        matrix(0, 2) * (matrix(1, 0) * matrix(2, 1) - matrix(1, 1) * matrix(2, 0))
      DMat.tabulate(3, 3): (i, j) =>
        val rows = (0 until 3).filter(_ != j); val columns = (0 until 3).filter(_ != i)
        val minor = matrix(rows(0), columns(0)) * matrix(rows(1), columns(1)) - matrix(rows(0), columns(1)) * matrix(rows(1), columns(0))
        (if (i + j) % 2 == 0 then minor else -minor) / d
    def inverse2(a: Double, b: Double, d: Double) =
      val determinant = a * d - b * b
      ((d / determinant, -b / determinant), (-b / determinant, a / determinant))
    val continuous = new FitFixture
    val fit = right(continuous.fit)
    val artifact = fit.finalFit.artifact.getOrElse(fail("converged final fit must retain its artifact"))
    val prior = right(TargetPriorCovariance(continuous.target, DMat.eye(1), value("continuous-prior"), "unit target covariance"))
    val prediction = right(PatternPrediction.fromArtifact(continuous.neural, continuous.target, continuous.components, artifact, fit.covarianceFit.covariance, Some(prior)))
    val before = (continuous.operator.forwardColumns, continuous.operator.adjointColumns)
    val xValues = Vector(1.0, -.5, .25)
    val x = right(AxisValues(continuous.neural, xValues))
    val factors = fit.finalFit.factors
    val f = Vector.tabulate(3)(row => factors.neuralByComponent(row, 0) * factors.targetByComponent(0, 0))
    val psi = densePsi(fit.covarianceFit.covariance)
    val posteriorCovariance = DMat.tabulate(3, 3)((row, column) => psi(row, column) + f(row) * f(column))
    val inverse = inverse3(posteriorCovariance)
    val denseDecode = (0 until 3).map(row => f(row) * (0 until 3).map(column => inverse(row, column) * xValues(column)).sum).sum
    assertEqualsDouble(right(prediction.decode(x)).values.values.head, denseDecode, 1e-8)
    val encoded = right(prediction.encode(right(AxisValues(continuous.target, Vector(.75))))).values
    for row <- 0 until 3 do assertEqualsDouble(encoded(row), .75 * f(row), 1e-10)
    val local = right(LocalPatternPrediction.derive(prediction, continuous.select(0, 2), LocalPredictionPreparation.AlreadyPreparedLocalCoordinates("held-out ROI coordinates")))
    val localInput = Vector(1.0, .25)
    val localPsi00 = psi(0, 0); val localPsi01 = psi(0, 2); val localPsi11 = psi(2, 2)
    val localH = inverse2(localPsi00 + f(0) * f(0), localPsi01 + f(0) * f(2), localPsi11 + f(2) * f(2))
    val localDecode = f(0) * (localH._1._1 * localInput(0) + localH._1._2 * localInput(1)) + f(2) * (localH._2._1 * localInput(0) + localH._2._2 * localInput(1))
    assertEqualsDouble(right(local.decode(right(AxisValues(local.localAxis, localInput)))).values.values.head, localDecode, 1e-8)
    assertEquals((continuous.operator.forwardColumns, continuous.operator.adjointColumns), before)
    assertEquals(fit.work.structuredFits, 2)
    assertEquals(fit.work.covarianceFitCalls, 1)

    val categorical = new FitFixture(categorical = true)
    val categoricalFit = right(categorical.fit)
    val categoryArtifact = categoricalFit.finalFit.artifact.getOrElse(fail("categorical fit must retain its artifact"))
    val classifier = right(PatternPrediction.fromArtifact(categorical.neural, categorical.target, categorical.components, categoryArtifact, categoricalFit.covarianceFit.covariance))
    val categoryBefore = (categorical.operator.forwardColumns, categorical.operator.adjointColumns)
    val categoryX = Vector(1.0, -.5, .25)
    val categoryFactors = categoricalFit.finalFit.factors
    val categoryPrecision = inverse3(densePsi(categoricalFit.covarianceFit.covariance))
    val precisionX = Vector.tabulate(3)(row => (0 until 3).map(column => categoryPrecision(row, column) * categoryX(column)).sum)
    val raw = (0 until 3).map(row => categoryFactors.neuralByComponent(row, 0) * precisionX(row)).sum
    val gram = (for row <- 0 until 3; column <- 0 until 3 yield categoryFactors.neuralByComponent(row, 0) * categoryPrecision(row, column) * categoryFactors.neuralByComponent(column, 0)).sum
    val means = Vector(categoryFactors.targetByComponent(0, 0), -categoryFactors.targetByComponent(0, 0))
    val expectedScores = means.map(mean => math.log(.5) + mean * raw - .5 * mean * mean * gram)
    val normalizer = expectedScores.map(score => math.exp(score - expectedScores.max)).sum
    val result = right(classifier.classify(right(AxisValues(categorical.neural, categoryX))))
    assertEquals(result.keys, Vector("class-0", "class-1"))
    result.logScores.zip(expectedScores).foreach((actual, expected) => assertEqualsDouble(actual, expected, 1e-8))
    result.probabilities.zip(expectedScores).foreach: (actual, score) =>
      assertEqualsDouble(actual, math.exp(score - expectedScores.max) / normalizer, 1e-8)
    assertEquals((categorical.operator.forwardColumns, categorical.operator.adjointColumns), categoryBefore)

  test("three-dimensional diagonal-plus-rank-one precision matches a dense cofactor oracle and target units scale outputs"):
    val neural = axis("oracle-neural", SpaceRole.Observed, 3)
    val covariance = right(ResidualCovariance.fromFactors(neural, Vector(2.0, 3.0, 4.0), DMat.dense(3, 1, Vector(1.0, 2.0, -1.0))))
    val input = DMat.dense(3, 1, Vector(3.0, -2.0, 5.0))
    val actual = right(covariance.applyPrecision(input))
    val psi = Array(Array(3.0, 2.0, -1.0), Array(2.0, 7.0, -2.0), Array(-1.0, -2.0, 5.0))
    val determinant = psi(0)(0) * (psi(1)(1) * psi(2)(2) - psi(1)(2) * psi(2)(1)) - psi(0)(1) * (psi(1)(0) * psi(2)(2) - psi(1)(2) * psi(2)(0)) + psi(0)(2) * (psi(1)(0) * psi(2)(1) - psi(1)(1) * psi(2)(0))
    val inverse = Array.tabulate(3, 3): (i, j) =>
      val rows = (0 until 3).filter(_ != j); val cols = (0 until 3).filter(_ != i)
      val minor = psi(rows(0))(cols(0)) * psi(rows(1))(cols(1)) - psi(rows(0))(cols(1)) * psi(rows(1))(cols(0))
      (if (i + j) % 2 == 0 then minor else -minor) / determinant
    for i <- 0 until 3 do
      val expected = (0 until 3).map(j => inverse(i)(j) * input(j, 0)).sum
      assertEqualsDouble(actual(i, 0), expected, 1e-12)

    def scalar(scale: Double) =
      val target = axis(s"scaled-target-$scale", SpaceRole.Observed, 1)
      val component = axis(s"scaled-component-$scale", SpaceRole.Observed, 1)
      val samples = axis(s"scaled-samples-$scale", SpaceRole.Samples, 2)
      val unit = right(AxisValues(target, Vector(1.0)))
      val geometry = right(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit)))
      val factors = right(PatternFactors(neural, target, component, DMat.dense(3, 1, Vector(1.0 / scale, 0.0, 0.0)), DMat.dense(1, 1, Vector(1.0)), GaugeEvidence.PendingNumericalCheck))
      val artifact = right(PatternArtifact(factors, geometry, CenteringPolicy.CenteredBeforeFit("x", "y"), DegenerateTargetPolicy.Refuse,
        ResidualCovarianceCapability.DiagonalPlusLowRank(neural.descriptor, 1), right(TrainingBinding(samples.descriptor, "oracle", "units")), Vector("oracle"), right(PatternFitDiagnostics(Vector(0.0), "oracle", Vector.empty))))
      right(PatternPrediction.fromArtifact(neural, target, component, artifact, covariance, Some(right(TargetPriorCovariance(target, DMat.dense(1, 1, Vector(scale * scale)), value(s"prior-$scale"), "scaled")))))
    val baselineModel = scalar(1.0)
    val scaledModel = scalar(5.0)
    val targetUnitInput = right(AxisValues(neural, Vector(2.0, 0.0, 0.0)))
    val baseline = right(baselineModel.decode(targetUnitInput)).values.values.head
    val scaled = right(scaledModel.decode(targetUnitInput)).values.values.head
    assertEqualsDouble(scaled, 5.0 * baseline, 1e-12)
    val baselineEncoding = right(baselineModel.encode(right(AxisValues(baselineModel.factors.targetAxis, Vector(.75))))).values
    val scaledEncoding = right(scaledModel.encode(right(AxisValues(scaledModel.factors.targetAxis, Vector(3.75))))).values
    baselineEncoding.zip(scaledEncoding).foreach((baselineValue, scaledValue) => assertEqualsDouble(scaledValue, baselineValue, 1e-12))

  test("checkerboard support is consumed by the support optimizer with its literal weighted TV law"):
    val voxels = axis("checker", SpaceRole.Observed, 4)
    val graph = right(SupportGraph(voxels.descriptor, Vector(SupportEdge(0, 1, 1.0), SupportEdge(0, 2, 2.0), SupportEdge(1, 3, 3.0), SupportEdge(2, 3, 4.0)), SupportTopology.Declared("2x2 checkerboard face neighbours"), "edge weight"))
    val disconnected = right(SupportGraph(voxels.descriptor, Vector.empty, SupportTopology.Declared("no adjacency"), "edge weight"))
    val reference = DMat.dense(4, 1, Vector(1.0, 0.0, 0.0, 1.0))
    val envelope = Vector(1.0, 0.0, 0.0, 1.0)
    val literalTv = graph.edges.map(edge => edge.weight * math.abs(envelope(edge.left) - envelope(edge.right))).sum
    assertEqualsDouble(literalTv, 10.0, 0.0)
    val penalty = right(SupportPenalty(0.0, 1.0))
    val checked = right(SpatialSupport.update(voxels, reference, envelope, graph, penalty, 1000000L, strict))
    val isolated = right(SpatialSupport.update(voxels, reference, envelope, disconnected, penalty, 1000000L, strict))
    assertEquals(checked.work.edgeCells, 4L)
    assert(checked.converged)
    checked.envelope.foreach(value => assertEqualsDouble(value, 2.0 / 3.0, 2e-7))
    Vector(2.0 / 3.0, 0.0, 0.0, 2.0 / 3.0).zipWithIndex.foreach: (expected, row) =>
      assertEqualsDouble(checked.loadings(row, 0), expected, 2e-7)
    assertEqualsDouble(checked.objective, 2.0 / 3.0, 2e-7)
    assertEquals(isolated.work.edgeCells, 0L)
    assert(isolated.converged)
    isolated.envelope.zip(envelope).foreach((actual, expected) => assertEqualsDouble(actual, expected, 1e-10))
    for row <- 0 until 4 do assertEqualsDouble(isolated.loadings(row, 0), reference(row, 0), 1e-10)
    assertEqualsDouble(isolated.objective, 0.0, 1e-10)

  test("rank, conditioning, and covariance convergence boundaries return typed outcomes"):
    val samples = axis("boundary-samples", SpaceRole.Samples, 200)
    val neural = axis("boundary-neural", SpaceRole.Observed, 3)
    val raw = DMat.tabulate(200, 3): (row, column) =>
      math.sin(.17 * (row + 1) * (column + 2)) + .3 * math.cos(.11 * (row + 3) * (column + 1))
    val means = Vector.tabulate(3)(column => (0 until 200).map(row => raw(row, column)).sum / 200.0)
    val residuals = DMat.tabulate(200, 3)((row, column) => raw(row, column) - means(column))
    val binding = right(TrainingBinding(samples.descriptor, "boundary", "rank"))
    assertEquals(ResidualCovariance.fit(neural, residuals, "rank", binding, covariance(rank = 3)).left.toOption, Some(ResidualCovarianceError.InvalidRank(3, 1)))
    val forced = ResidualCovariance.fit(neural, residuals, "forced", binding, covariance(rank = 1, iterations = 1, tolerance = 1e-30))
    forced match
      case Left(ResidualCovarianceError.NotConverged(1, _, _)) => ()
      case other => fail(s"expected refused one-iteration covariance fit, got $other")

    val f = new FitFixture
    val admitted = StructuredPatternOptimizer.admit(f.samples, f.neural, f.target, f.components)(f.observations, f.responses,
      right(ResidualCovariance.fromFactors(f.neural, Vector(1.0, 1.0, 1.0), DMat.zeros(3, 1))), f.support, f.geometry,
      CenteringPolicy.CenteredBeforeFit("x", "y"), structured(), f.binding, Vector("boundary"), PatternReplay.Repeatable("boundary"))
    assert(admitted.isRight, "well-conditioned one-column target is admitted")
    val conditionSamples = axis("condition-samples", SpaceRole.Samples, 4)
    val conditionNeural = axis("condition-neural", SpaceRole.Observed, 2)
    val conditionTarget = axis("condition-target", SpaceRole.Observed, 2)
    val conditionComponents = axis("condition-components", SpaceRole.Observed, 2)
    val nearDependent = DMat.dense(4, 2, Vector(1.0, 1.0 + 1e-14, 1.0, 1.0 - 1e-14, -1.0, -1.0 + 1e-14, -1.0, -1.0 - 1e-14))
    val conditionX = right(Observations.fromDense(conditionSamples, conditionNeural, nearDependent, value("condition-x"), source("condition-x")))
    val conditionY = right(MultiResponse.fromDense(conditionSamples, conditionTarget, nearDependent, value("condition-y"), source("condition-y")))
    val conditionUnit = right(AxisValues(conditionTarget, Vector(1.0, 1.0)))
    val conditionGeometry = right(TargetGeometry.continuous(conditionTarget, conditionUnit, conditionUnit, Vector("all" -> conditionUnit)))
    val conditionCovariance = right(ResidualCovariance.fromFactors(conditionNeural, Vector(1.0, 1.0), DMat.zeros(2, 1)))
    val conditionGraph = right(SupportGraph(conditionNeural.descriptor, Vector.empty, SupportTopology.Declared("condition fixture"), "one"))
    val conditionBinding = right(TrainingBinding(conditionSamples.descriptor, "condition", "near-dependent"))
    StructuredPatternOptimizer.fit(conditionSamples, conditionNeural, conditionTarget, conditionComponents)(conditionX, conditionY,
      conditionCovariance, conditionGraph, conditionGeometry, CenteringPolicy.CenteredBeforeFit("x", "y"), structured(),
      conditionBinding, Vector("condition"), PatternReplay.Repeatable("condition")) match
      case Left(StructuredPatternError.Invalid("rank-deficient target coordinates")) => ()
      case other => fail(s"expected typed near-dependent target refusal, got $other")
    val zero = right(MultiResponse.fromDense(f.samples, f.target, DMat.zeros(8, 1), value("zero"), source("zero")))
    StructuredPatternOptimizer.fit(f.samples, f.neural, f.target, f.components)(f.observations, zero,
      right(ResidualCovariance.fromFactors(f.neural, Vector(1.0, 1.0, 1.0), DMat.zeros(3, 1))), f.support, f.geometry,
      CenteringPolicy.CenteredBeforeFit("x", "y"), structured(), f.binding, Vector("boundary"), PatternReplay.Repeatable("boundary")) match
      case Left(StructuredPatternError.Invalid("rank-deficient target coordinates")) => ()
      case other => fail(s"expected explicit null-target rank refusal, got $other")

  test("metadata admission proves p squared and p by q are not required while q squared remains a planned bound"):
    def admit(p: Int, q: Int) =
      val samples = axis(s"large-samples-$p-$q", SpaceRole.Samples, 2)
      val neural = axis(s"large-neural-$p-$q", SpaceRole.Observed, p)
      val target = axis(s"large-target-$p-$q", SpaceRole.Observed, q)
      val component = axis(s"large-component-$p-$q", SpaceRole.Observed, 1)
      val x = new Poison(2, p); val y = new Poison(2, q)
      val observations = right(Observations.fromOperator(samples, neural, x, value(s"x-$p-$q"), source(s"x-$p-$q")))
      val responses = right(MultiResponse.fromOperator(samples, target, y, value(s"y-$p-$q"), source(s"y-$p-$q")))
      val covariance = right(ResidualCovariance.fromFactors(neural, Vector.fill(p)(1.0), DMat.zeros(p, 1)))
      val graph = right(SupportGraph(neural.descriptor, Vector.empty, SupportTopology.Declared("metadata only"), "one"))
      val weights = right(AxisValues(target, Vector.fill(q)(1.0)))
      val geometry = right(TargetGeometry.continuous(target, weights, weights, Vector("all" -> weights)))
      val result = StructuredPatternOptimizer.admit(samples, neural, target, component)(observations, responses, covariance, graph, geometry,
        CenteringPolicy.CenteredBeforeFit("x", "y"), structured(q, Long.MaxValue, Long.MaxValue), right(TrainingBinding(samples.descriptor, "metadata", "large")), Vector("metadata"), PatternReplay.Repeatable("metadata"))
      (result, x, y, right(covariance.precisionWork(1)))
    val square = admit(50000, 1)
    assert(square._1.isRight, "p squared is not an adapter allocation")
    assertEquals((square._2.reads, square._3.reads), (0, 0))
    assertEquals((square._4.largestDenseRows, square._4.largestDenseColumns), (50001, 1))
    val rectangular = admit(100000, 40000)
    assert(rectangular._1.isRight, "p by q is not an adapter allocation when q squared remains representable")
    assertEquals((rectangular._2.reads, rectangular._3.reads), (0, 0))
    assert(right(rectangular._1).workspaceCells > 0L)
    assertEquals((rectangular._4.largestDenseRows, rectangular._4.largestDenseColumns), (100001, 1))
    val qSquared = admit(1, 50000)
    assert(qSquared._1.isLeft, "q squared is a retained target-Gram bound")
    assertEquals((qSquared._2.reads, qSquared._3.reads), (0, 0))
