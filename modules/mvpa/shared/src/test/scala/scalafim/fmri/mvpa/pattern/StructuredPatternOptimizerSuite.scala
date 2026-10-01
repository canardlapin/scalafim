package scalafim.fmri.mvpa.pattern

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import gale.optim.{FirstOrderConfig, FirstOrderTolerance}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import munit.FunSuite
import scalafim.fmri.mvpa.{AxisRef, EvidenceSource, MultiResponse, Observations}
import scalafim.response.{Provenance, ProvenanceId, SourceId}

/** Numerical contracts for the fixed-Psi alternating adapter.
  *
  * The small fixtures deliberately compute their expected values from dense
  * arithmetic rather than by reusing the fitter's callbacks.
  */
class StructuredPatternOptimizerSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)

  private def axis(name: String, role: SpaceRole, n: Int) =
    right(AxisRef.fromStableKeys(name, role, Vector.tabulate(n)(i => s"$name-$i"), "fixture", "one", "raw"))

  private def source(name: String): EvidenceSource =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))

  private def value(name: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(name))

  private def strict = right(FirstOrderConfig.from(100000, right(FirstOrderTolerance.from(1e-10, 1e-10))))

  private def config(penalty: SupportPenalty, outer: Int = 100, workspace: Long = 10000000L,
      columns: Long = 100000L) =
    right(StructuredPatternConfig(penalty, maximumOuterIterations = outer, inner = strict,
      stationarityTolerance = 1e-7, objectiveTolerance = 1e-9,
      maximumWorkspaceCells = workspace, maximumOperatorColumns = columns))

  private final class Counted(matrix: Option[DMat]) extends DoubleLinearOperator:
    val rows = matrix.fold(2)(_.rows)
    val cols = matrix.fold(2)(_.cols)
    var forwardColumns = 0L
    var adjointColumns = 0L
    def applyTo(input: DVec, output: MutableDVec): Unit =
      forwardColumns += 1
      matrix.fold(throw new IllegalStateException("poison read"))(_.applyTo(input, output))
    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      adjointColumns += 1
      matrix.fold(throw new IllegalStateException("poison read"))(_.transposeApplyTo(input, output))

  private final class Poison(val rows: Int, val cols: Int) extends DoubleLinearOperator:
    var reads = 0
    def applyTo(input: DVec, output: MutableDVec): Unit =
      reads += 1
      throw new IllegalStateException("preflight must not read this source")
    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      reads += 1
      throw new IllegalStateException("preflight must not read this source")

  private final class TwoByTwo(operator: Option[Counted] = None):
    val samples = axis("sample", SpaceRole.Samples, 2)
    val neural = axis("neural", SpaceRole.Observed, 2)
    val target = axis("target", SpaceRole.Observed, 1)
    val components = axis("component", SpaceRole.Observed, 1)
    val x = DMat.dense(2, 2, Vector(-3.0, 4.0, 3.0, -4.0))
    val y = DMat.dense(2, 1, Vector(-1.0, 1.0))
    val observations = operator match
      case Some(counted) => right(Observations.fromOperator(samples, neural, counted, value("x-op"), source("x")))
      case None => right(Observations.fromDense(samples, neural, x, value("x-dense"), source("x")))
    val targets = right(MultiResponse.fromDense(samples, target, y, value("y"), source("y")))
    val covariance = right(ResidualCovariance.fromFactors(neural, Vector(2.0, 1.0), DMat.dense(2, 1, Vector(1.0, 1.0))))
    val graph = right(SupportGraph(neural.descriptor, Vector(SupportEdge(0, 1, 1.0)), SupportTopology.Declared("pair"), "one"))
    val unit = right(AxisValues(target, Vector(1.0)))
    val geometry = right(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit)))
    val binding = right(TrainingBinding(samples.descriptor, "fixture", "sha256:fixture"))
    def fit(c: StructuredPatternConfig, replay: PatternReplay = PatternReplay.Repeatable("fixture"),
        support: SupportGraph = graph) =
      StructuredPatternOptimizer.fit(samples, neural, target, components)(observations, targets, covariance, support,
        geometry, CenteringPolicy.CenteredBeforeFit("x-centered", "y-centered"), c, binding, Vector("fixture"), replay)

  /** Y has two centered orthogonal Hadamard columns.  X = Y diag(3, 2), so
    * the independent unpenalized rank-two product is diag(3, 2).
    */
  private final class Hadamard:
    val samples = axis("h-sample", SpaceRole.Samples, 4)
    val neural = axis("h-neural", SpaceRole.Observed, 2)
    val target = axis("h-target", SpaceRole.Observed, 2)
    val oneComponent = right(AxisRef.fromStableKeys("h-component", SpaceRole.Observed, Vector("c0"), "fixture", "one", "raw"))
    val components = right(AxisRef.fromStableKeys("h-component", SpaceRole.Observed, Vector("c0", "c1"), "fixture", "one", "raw"))
    val y = DMat.dense(4, 2, Vector(1.0, 1.0, 1.0, -1.0, -1.0, 1.0, -1.0, -1.0))
    val x = DMat.dense(4, 2, Vector(3.0, 2.0, 3.0, -2.0, -3.0, 2.0, -3.0, -2.0))
    val observations = right(Observations.fromDense(samples, neural, x, value("hadamard-x"), source("hadamard-x")))
    val targets = right(MultiResponse.fromDense(samples, target, y, value("hadamard-y"), source("hadamard-y")))
    val covariance = right(ResidualCovariance.fromFactors(neural, Vector(1.0, 1.0), DMat.zeros(2, 1)))
    val graph = right(SupportGraph(neural.descriptor, Vector.empty, SupportTopology.Declared("isolated"), "one"))
    val unit = right(AxisValues(target, Vector(1.0, 1.0)))
    val geometry = right(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit)))
    val binding = right(TrainingBinding(samples.descriptor, "fixture", "sha256:hadamard"))
    def factors(c: DMat, a: DMat = DMat.dense(2, 1, Vector(0.0, 2.0))) =
      right(PatternFactors(neural, target, oneComponent, a, c, GaugeEvidence.PendingNumericalCheck))
    def fit(c: StructuredPatternConfig, rank: Int = 2, initial: Option[PatternFactors[String, String, ?]] = None) =
      val componentAxis = if rank == 1 then oneComponent else components
      StructuredPatternOptimizer.fit(samples, neural, target, componentAxis)(observations, targets, covariance, graph,
        geometry, CenteringPolicy.CenteredBeforeFit("x-centered", "y-centered"), c, binding, Vector("fixture"),
        PatternReplay.Repeatable("fixture"), initial = initial)

  private def close(actual: Double, expected: Double, tolerance: Double = 1e-7): Unit =
    assertEqualsDouble(actual, expected, tolerance)

  test("coupled fixed-Psi two-row oracle recovers the dense product, envelope, and full objective"):
    val f = new TwoByTwo
    val result = right(f.fit(config(right(SupportPenalty(0.2, 0.1)))))
    val sign = if result.factors.targetByComponent(0, 0) >= 0.0 then 1.0 else -1.0
    close(result.factors.neuralByComponent(0, 0) * sign, 3.0, 2e-5)
    close(result.factors.neuralByComponent(1, 0) * sign, -3.5, 2e-5)
    close(result.envelope(0), 3.0, 2e-5)
    close(result.envelope(1), 3.5, 2e-5)
    // Independent constant: tr(X Psi^-1 X^T)/(2n) = 9.
    // The fitter reports J - constant, while the direct full objective is .075 + 1.3 + .05.
    close(result.iterations.last.shiftedObjective + 9.0, 1.425, 2e-5)
    assert(result.iterations.last.aStationarity.isFinite, "finite final A residual")
    assert(result.iterations.last.cStationarity.isFinite, "finite final C residual")
    assertEquals(result.stopping, StructuredPatternStopping.Converged)
    assert(result.artifact.nonEmpty, "completed stationary fit carries its artifact")
    assert(result.iterations.last.aStationarity < 1e-6, "same-point A stationarity")
    assert(result.iterations.last.dualStationarity < 1e-6, "same-point TV stationarity")
    assert(result.iterations.last.cStationarity < 1e-6, "same-point C stationarity")

  test("C block uses empirical target covariance rather than normalizing the cross product"):
    val p = new StructuredPatternOptimizer.TargetProblem(
      DMat.dense(2, 2, Vector(1.0, 0.0, 0.0, 4.0)), DMat.dense(1, 1, Vector(1.0)),
      DMat.dense(2, 1, Vector(1.2, 4.0)), config(right(SupportPenalty(0.0, 0.0))))
    val c = DMat.dense(2, 1, Vector(0.6, 0.8))
    close(right(p.smooth.value(c)), -2.46, 1e-12)
    val gradient = right(p.smooth.gradient(c))
    close(gradient(0, 0), -0.6, 1e-12)
    close(gradient(1, 0), -0.8, 1e-12)
    val solved = right(p.solve(DMat.dense(2, 1, Vector(1.0, 0.0))))
    close(solved.primal(0, 0), 0.6, 2e-6)
    close(solved.primal(1, 0), 0.8, 2e-6)

  test("C-block residual projects a zero trial point onto Stiefel instead of refusing its rank"):
    val halfStep = right(FirstOrderConfig.from(100, right(FirstOrderTolerance.from(1e-10, 1e-10)), stepSafety = .5))
    val c = right(StructuredPatternConfig(right(SupportPenalty(0.0, 0.0)), inner = halfStep))
    // Lipschitz=2, so step=.5/2=.25.  At e1, grad=4e1 and the trial point is zero.
    // A projection set must still return a nearest orthonormal representative.
    val p = new StructuredPatternOptimizer.TargetProblem(DMat.eye(2), DMat.dense(1, 1, Vector(1.0)),
      DMat.dense(2, 1, Vector(-3.0, 0.0)), c)
    assert(right(p.residual(DMat.dense(2, 1, Vector(1.0, 0.0)))).isFinite, "zero trial polar projection")

  test("C block gradient agrees with central differences of its independent dense objective"):
    val s = DMat.dense(2, 2, Vector(2.0, .25, .25, 1.5))
    val g = DMat.dense(2, 2, Vector(1.2, .1, .1, .8))
    val d = DMat.dense(2, 2, Vector(.3, -.4, 1.1, .2))
    val p = new StructuredPatternOptimizer.TargetProblem(s, g, d, config(right(SupportPenalty(0.0, 0.0))))
    val at = DMat.dense(2, 2, Vector(.2, -.6, .7, .4))
    val analytic = right(p.smooth.gradient(at))
    def objective(m: DMat): Double =
      var total = 0.0
      for i <- 0 until 2; j <- 0 until 2; k <- 0 until 2; l <- 0 until 2 do
        total += .5 * m(i, j) * s(i, k) * m(k, l) * g(l, j)
      for i <- 0 until 2; j <- 0 until 2 do total -= m(i, j) * d(i, j)
      total
    val h = 1e-6
    for i <- 0 until 2; j <- 0 until 2 do
      val plus = DMat.tabulate(2, 2)((r, c) => at(r, c) + (if r == i && c == j then h else 0.0))
      val minus = DMat.tabulate(2, 2)((r, c) => at(r, c) - (if r == i && c == j then h else 0.0))
      close(analytic(i, j), (objective(plus) - objective(minus)) / (2.0 * h), 2e-6)

  test("loading gradient and all penalty values match independent four-index arithmetic"):
    val f = new TwoByTwo
    val h = DMat.dense(2, 2, Vector(2.0, .3, .3, 1.0))
    val b = DMat.dense(2, 2, Vector(.5, -.2, 1.0, .4))
    val penalty = right(SupportPenalty(.2, .15, .4))
    val cfg = right(StructuredPatternConfig(penalty, ridge = .3))
    val problem = new StructuredPatternOptimizer.LoadingProblem(h, b, 1.0, f.graph, cfg,
      at => f.covariance.applyPrecision(at).left.map(StructuredPatternError.Covariance.apply))
    val at = DMat.dense(6, 1, Vector(.7, -.4, 1.3, -.2, .9, 1.5))
    val inverse = Vector(Vector(.4, -.2), Vector(-.2, .6))
    def smooth(m: DMat): Double =
      var out = 0.0
      for i <- 0 until 2; j <- 0 until 2; k <- 0 until 2; l <- 0 until 2 do
        out += .5 * m(i * 3 + k, 0) * inverse(i)(j) * m(j * 3 + l, 0) * h(l, k)
      for i <- 0 until 2; j <- 0 until 2; k <- 0 until 2 do
        out -= m(i * 3 + k, 0) * inverse(i)(j) * b(j, k)
      for i <- 0 until 2; k <- 0 until 2 do out += .15 * math.pow(m(i * 3 + k, 0), 2)
      for k <- 0 until 2 do out += .2 * math.pow(m(k, 0) - m(3 + k, 0), 2)
      out
    close(right(problem.smooth.value(at)), smooth(at), 1e-12)
    val analytic = right(problem.smooth.gradient(at))
    val delta = 1e-6
    for index <- 0 until 6 do
      val plus = DMat.tabulate(6, 1)((row, _) => at(row, 0) + (if row == index then delta else 0.0))
      val minus = DMat.tabulate(6, 1)((row, _) => at(row, 0) - (if row == index then delta else 0.0))
      close(analytic(index, 0), (smooth(plus) - smooth(minus)) / (2 * delta), 1e-8)
    close(right(problem.objective(DMat.dense(2, 2, Vector(.7, -.4, -.2, .9)), Vector(1.3, 1.5))),
      smooth(at) + .2 * (1.3 + 1.5) + .15 * .2, 1e-12)

  test("rank-start extension preserves the old mean even when cold columns duplicate it"):
    val old = DMat.dense(2, 1, Vector(0.0, 1.0))
    val expanded = right(StructuredPatternOptimizer.appendComplement(old, DMat.eye(2)))
    close(expanded(0, 0), 0.0, 1e-12)
    close(expanded(1, 0), 1.0, 1e-12)
    val a = DMat.dense(2, 1, Vector(0.0, 2.0))
    val extendedA = DMat.dense(2, 2, Vector(0.0, 0.0, 2.0, 0.0))
    val before = a * old.t
    val after = extendedA * expanded.t
    for row <- 0 until 2; col <- 0 until 2 do close(after(row, col), before(row, col), 1e-12)

  test("rank expansion preserves a prefix-compatible warm component and appends its orthogonal complement"):
    val f = new Hadamard
    val old = f.factors(DMat.dense(2, 1, Vector(0.0, 1.0)))
    val result = right(f.fit(config(right(SupportPenalty(0.0, 0.0))), initial = Some(old)))
    assertEquals(result.work.initialization, "rank-expanded warm start")
    assert(result.iterations.last.orthogonalityViolation <= 1e-6, "expanded target basis is orthonormal")
    // Sign/rotation free product, checked against the dense independently known map.
    val product = result.factors.neuralByComponent * result.factors.targetByComponent.t
    close(product(0, 0), 3.0, 2e-4)
    close(product(0, 1), 0.0, 2e-4)
    close(product(1, 0), 0.0, 2e-4)
    close(product(1, 1), 2.0, 2e-4)

  test("cold and full-rank warm starts follow the same penalized fixture contract"):
    val f = new Hadamard
    val penalty = config(right(SupportPenalty(.15, 0.0)))
    val cold = right(f.fit(penalty))
    val warm = right(f.fit(penalty, initial = Some(right(PatternFactors(f.neural, f.target, f.components,
      DMat.dense(2, 2, Vector(3.0, 0.0, 0.0, 2.0)), DMat.eye(2), GaugeEvidence.PendingNumericalCheck)))))
    assertEquals(cold.work.initialization, "supervised streamed right Gram")
    assertEquals(warm.work.initialization, "provided warm start")
    assert(cold.iterations.last.orthogonalityViolation <= 1e-6, "cold target basis")
    assert(warm.iterations.last.orthogonalityViolation <= 1e-6, "warm target basis")
    assert(cold.envelope.forall(_.isFinite), "cold envelope")
    assert(warm.envelope.forall(_.isFinite), "warm envelope")
    val nextPenalty = config(right(SupportPenalty(.3, 0.0)))
    val warmPath = right(f.fit(nextPenalty, initial = Some(warm.factors)))
    val coldPath = right(f.fit(nextPenalty))
    Vector((cold, 2.85, 1.85), (warm, 2.85, 1.85), (warmPath, 2.7, 1.7), (coldPath, 2.7, 1.7)).foreach:
      case (result, first, second) =>
        val product = result.factors.neuralByComponent * result.factors.targetByComponent.t
        close(product(0, 0), first, 2e-5)
        close(product(1, 1), second, 2e-5)
        close(product(0, 1), 0.0, 2e-5)
        close(product(1, 0), 0.0, 2e-5)
        assertEquals(result.stopping, StructuredPatternStopping.Converged)

  test("several valid rank-one starts record provided initialization without a global-optimum assertion"):
    val f = new Hadamard
    val starts = Vector(
      f.factors(DMat.dense(2, 1, Vector(1.0, 0.0)), DMat.dense(2, 1, Vector(1.0, 0.0))),
      f.factors(DMat.dense(2, 1, Vector(0.0, 1.0))),
      f.factors(DMat.dense(2, 1, Vector(1.0 / math.sqrt(2.0), 1.0 / math.sqrt(2.0))))
    )
    val results = starts.map(start => right(f.fit(config(right(SupportPenalty(.1, 0.0))), rank = 1, initial = Some(start))))
    results.foreach: result =>
      assertEquals(result.work.initialization, "provided warm start")
      assertEquals(result.stopping, StructuredPatternStopping.Converged)
      assert(result.artifact.nonEmpty, "each stationary multistart result has a completed artifact")
      assert(result.iterations.last.orthogonalityViolation <= 1e-6, "rank-one target basis")
      assert(result.iterations.last.shiftedObjective.isFinite, "finite nonconvex stationary objective")
    val objectives = results.map(_.iterations.last.shiftedObjective)
    assert(objectives.max - objectives.min > 1.0, "distinct stationary starts expose nonconvex sensitivity")
    close(objectives(0), -4.205, 2e-5)
    close(objectives(1), -1.805, 2e-5)

  test("preflight refusals do not read a one-shot source and metadata mismatch is also read-free"):
    val counted = new Counted(None)
    val f = new TwoByTwo(Some(counted))
    f.fit(config(right(SupportPenalty(.2, .1)), workspace = 0L)) match
      case Left(StructuredPatternError.Budget("workspace cells", _, 0L)) => ()
      case other => fail(s"expected workspace preflight refusal, got $other")
    assertEquals((counted.forwardColumns, counted.adjointColumns), (0L, 0L))
    f.fit(config(right(SupportPenalty(.2, .1))), PatternReplay.SinglePass) match
      case Left(StructuredPatternError.Invalid(_)) => ()
      case other => fail(s"expected replay refusal, got $other")
    assertEquals((counted.forwardColumns, counted.adjointColumns), (0L, 0L))
    val foreign = axis("foreign", SpaceRole.Observed, 2)
    val foreignGraph = right(SupportGraph(foreign.descriptor, Vector.empty, SupportTopology.Declared("foreign"), "one"))
    f.fit(config(right(SupportPenalty(.2, .1))), support = foreignGraph) match
      case Left(StructuredPatternError.AxisMismatch("neural coordinates")) => ()
      case other => fail(s"expected neural metadata refusal, got $other")
    assertEquals((counted.forwardColumns, counted.adjointColumns), (0L, 0L))

  test("a q squared overflow declaration is refused before either operator is read"):
    val n = axis("overflow-sample", SpaceRole.Samples, 2)
    val neural = axis("overflow-neural", SpaceRole.Observed, 1)
    val target = axis("overflow-target", SpaceRole.Observed, 50000)
    val component = axis("overflow-component", SpaceRole.Observed, 1)
    val x = new Poison(2, 1)
    val y = new Poison(2, 50000)
    val observations = right(Observations.fromOperator(n, neural, x, value("overflow-x"), source("overflow-x")))
    val targets = right(MultiResponse.fromOperator(n, target, y, value("overflow-y"), source("overflow-y")))
    val covariance = right(ResidualCovariance.fromFactors(neural, Vector(1.0), DMat.zeros(1, 1)))
    val graph = right(SupportGraph(neural.descriptor, Vector.empty, SupportTopology.Declared("isolated"), "one"))
    val weights = right(AxisValues(target, Vector.fill(50000)(1.0)))
    val geometry = right(TargetGeometry.continuous(target, weights, weights, Vector("all" -> weights)))
    val unlimited = right(StructuredPatternConfig(right(SupportPenalty(0.0, 0.0)), maximumTargetDimension = 50000,
      maximumWorkspaceCells = Long.MaxValue, maximumOperatorColumns = Long.MaxValue))
    assert(StructuredPatternOptimizer.fit(n, neural, target, component)(observations, targets, covariance, graph,
      geometry, CenteringPolicy.CenteredBeforeFit("x", "y"), unlimited,
      right(TrainingBinding(n.descriptor, "fixture", "sha256:overflow")), Vector("fixture"), PatternReplay.Repeatable("fixture")).isLeft,
      "q squared overflow preflight")
    assertEquals(x.reads, 0)
    assertEquals(y.reads, 0)

  test("operator and dense fixtures agree, and work counts requested widths rather than a p-wide identity"):
    val dense = new TwoByTwo
    val counted = new Counted(Some(dense.x))
    val operator = new TwoByTwo(Some(counted))
    val c = config(right(SupportPenalty(.2, .1)))
    val a = right(dense.fit(c))
    val b = right(operator.fit(c))
    close(math.abs(a.factors.neuralByComponent(0, 0)), math.abs(b.factors.neuralByComponent(0, 0)), 2e-5)
    assertEquals(b.work.forwardColumns, counted.forwardColumns)
    assertEquals(b.work.adjointColumns, counted.adjointColumns)
    assertEquals(b.work.targetColumns, 1L)
    assert(b.work.forwardColumns > 0L, "forward source width")
    assert(b.work.adjointColumns > 0L, "adjoint source width")
    assert(b.work.forwardColumns + b.work.adjointColumns <= b.work.plannedOperatorColumns, "counted source width budget")

  test("outer iteration exhaustion produces no artifact and records both final block residuals"):
    val f = new TwoByTwo
    val exhausted = right(f.fit(config(right(SupportPenalty(.2, .1)), outer = 1)))
    assertEquals(exhausted.stopping, StructuredPatternStopping.IterationLimit)
    assertEquals(exhausted.artifact, None)
    assertEquals(exhausted.iterations.size, 1)
    assert(exhausted.iterations.last.aStationarity.isFinite, "final A residual")
    assert(exhausted.iterations.last.cStationarity.isFinite, "final C residual")

  test("structured config rejects invalid numerical and resource declarations"):
    assert(StructuredPatternConfig(right(SupportPenalty(0, 0)), ridge = -1.0).isLeft, "negative ridge")
    assert(StructuredPatternConfig(right(SupportPenalty(0, 0)), maximumOuterIterations = 0).isLeft, "zero outer iterations")
    assert(StructuredPatternConfig(right(SupportPenalty(0, 0)), stationarityTolerance = Double.NaN).isLeft, "NaN tolerance")
    assert(StructuredPatternConfig(right(SupportPenalty(0, 0)), maximumTargetDimension = 0).isLeft, "zero target bound")
