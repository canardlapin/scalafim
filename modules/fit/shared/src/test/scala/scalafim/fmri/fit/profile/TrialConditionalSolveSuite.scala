package scalafim.fmri.fit.profile

import gale.linalg.DMat
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.fmri.design.hrf.{ExpandedTrialDesign, HrfKernelBasis, KernelBasisSpec, TrialMembership}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{Cascade34Family, GaussianFamily, ParametricHrfFamily, ShapePoint}

/** Dense original `(a, gamma)` equations are assembled from time-row design
  * columns and a membership projector, independently of packed Gram assembly.
  * The oracle deliberately shares the compiled basis coefficients with the
  * production model: this proves the prepared-basis solve, not HRF-family or
  * basis-approximation accuracy.
  */
class TrialConditionalSolveSuite extends munit.FunSuite:
  private val step = PositiveSeconds(0.2).fold(error => fail(error.message), identity)
  private lazy val gaussian = HrfKernelBasis.compile(
    KernelBasisSpec(GaussianFamily.Default, step, Vector(26, 21), tolerance = 1e-4, maxRank = 40))
    .fold(error => fail(error.message), identity)
  private lazy val cascade = HrfKernelBasis.compile(
    KernelBasisSpec(Cascade34Family.Default, step, Vector(9, 7, 5), tolerance = 1e-4, maxRank = 40))
    .fold(error => fail(error.message), identity)

  private final case class Fixture(
      expanded: ExpandedTrialDesign,
      nuisance: Option[DMat],
      rawResponse: Array[Double],
      whitening: Option[WhiteningPlan],
      lambda: Double):
    def rows: Int = expanded.rows
    def trials: Int = expanded.trials
    def nuisanceColumns: Int = nuisance.fold(0)(_.cols)

  private def fixture(
      basis: HrfKernelBasis,
      lambda: Double = 2.0,
      nuisanceColumns: Int = 2,
      ar: Boolean = false,
      coincident: Boolean = false): Fixture =
    val rows = if basis.family.dimension == 3 then 48 else 80
    val run = rows / 2
    val membership = TrialMembership.make(Vector(0, 1, 1, 1, 2, 2), 3)
      .fold(error => fail(error.message), identity)
    val onsets =
      if coincident then Vector(2.5, 8.0, 8.0, 17.0, 4.0, 14.0)
      else Vector(2.5, 8.0, 15.0, 21.0, 4.0, 14.0)
    val expanded = ExpandedTrialDesign.lower(
      onsets.map(Seconds(_)), Vector(0, 0, 0, 0, 1, 1),
      Vector.fill(6)(Seconds(0.0)), membership,
      SamplingFrame(blockLens = Seq(run, run), tr = Seq(1.0, 1.0)),
      basis, Seconds(0.2)).fold(error => fail(error.message), identity)
    val nuisance = if nuisanceColumns == 0 then None else Some(DMat.tabulate(rows, nuisanceColumns): (t, j) =>
      val local = if t < run then t else t - run
      if j == 0 then 1.0 else (local - (run - 1) * 0.5) / run)
    val center = centerPoint(basis.family)
    val x = designAt(expanded, center)
    val signed = Array(0.7, -0.4, 0.35, 0.9, -0.65, 0.2)
    val response = Array.tabulate(rows): t =>
      var value = 0.03 * math.sin(0.31 * t) - 0.02 * math.cos(0.17 * t)
      var i = 0
      while i < expanded.trials do
        value += x(t * expanded.trials + i) * signed(i)
        i += 1
      nuisance.foreach: matrix =>
        value += 0.42 * matrix(t, 0)
        if nuisanceColumns > 1 then value -= 0.18 * matrix(t, 1)
      value
    val whitening =
      if ar then Some(WhiteningPlan.global(ArmaCoefficients.ar(0.31),
        Vector(TimeSegment(0, run, 0), TimeSegment(run, rows, 1))))
      else None
    Fixture(expanded, nuisance, response, whitening, lambda)

  private def centerPoint(family: ParametricHrfFamily): ShapePoint =
    ShapePoint.unsafe(Vector.tabulate(family.dimension): axis =>
      0.5 * (family.chart.lower(axis) + family.chart.upper(axis)))

  private def reference(fx: Fixture): (TrialBandedPreparation, TrialBandedObjective, TrialBandedResponse, Int) =
    val prep = TrialBandedPreparation.prepare(fx.expanded, fx.whitening, fx.nuisance, fx.lambda)
      .fold(error => fail(error.message), identity)
    val grid = NodeGrid(fx.expanded.basis.family.chart, Vector.fill(fx.expanded.basis.family.dimension)(3))
    val objective = prep.objective(grid).fold(error => fail(error.message), identity)
    val whitened = prep.whitenResponses(1, fx.rawResponse).fold(error => fail(error.message), identity)
    val encoded = prep.encodeWhitened(whitened).fold(error => fail(error.message), identity)
    val indices = Array.fill(grid.dimension)(1)
    (prep, objective, encoded, grid.indexOf(indices))

  private def designAt(expanded: ExpandedTrialDesign, point: ShapePoint): Array[Double] =
    val basis = expanded.basis
    val coefficients = new Array[Double](basis.rank)
    basis.coefficientsInto(point, new Array[Double](basis.fineCount), coefficients)
    val rows = expanded.rows
    val n = expanded.trials
    val m = basis.rank
    val source = expanded.term.data.data
    Array.tabulate(rows * n): index =>
      val row = index / n
      val trial = index % n
      var value = 0.0
      var p = 0
      while p < m do
        value += source(row * n * m + p * n + trial) * coefficients(p)
        p += 1
      value

  private def whiten(fx: Fixture, columns: Int, data: Array[Double]): Array[Double] =
    fx.whitening match
      case None => java.util.Arrays.copyOf(data, data.length)
      case Some(plan) =>
        val out = new Array[Double](data.length)
        // Explicit two-run AR(1) recurrence, reset at each run boundary.
        plan.segments.foreach: segment =>
          val phi = plan.coefficientsFor(segment).phi.head
          var col = 0
          while col < columns do
            out(segment.start * columns + col) = data(segment.start * columns + col) *
              (if plan.exactFirstAr1 then math.sqrt(1.0 - phi * phi) else 1.0)
            var row = segment.start + 1
            while row < segment.endExclusive do
              out(row * columns + col) = data(row * columns + col) - phi * data((row - 1) * columns + col)
              row += 1
            col += 1
        out

  private def denseNormal(fx: Fixture, point: ShapePoint): DMat =
    val rows = fx.rows
    val n = fx.trials
    val f = fx.nuisanceColumns
    val x = whiten(fx, n, designAt(fx.expanded, point))
    val nuisanceRaw = new Array[Double](rows * f)
    fx.nuisance.foreach(_.copyRowMajorTo(nuisanceRaw))
    val nuisance = whiten(fx, f, nuisanceRaw)
    val membership = fx.expanded.membership
    DMat.tabulate(n + f, n + f): (i, j) =>
      var sum = 0.0
      var t = 0
      while t < rows do
        val xi = if i < n then x(t * n + i) else nuisance(t * f + i - n)
        val xj = if j < n then x(t * n + j) else nuisance(t * f + j - n)
        sum += xi * xj
        t += 1
      if i < n && j < n then
        val same = membership.conditionOfTrial(i) == membership.conditionOfTrial(j)
        sum += fx.lambda * ((if i == j then 1.0 else 0.0) -
          (if same then 1.0 / membership.trialsOf(membership.conditionOfTrial(i)).length else 0.0))
      sum

  private def denseRhs(fx: Fixture, point: ShapePoint): Array[Double] =
    val rows = fx.rows
    val n = fx.trials
    val f = fx.nuisanceColumns
    val x = whiten(fx, n, designAt(fx.expanded, point))
    val nuisanceRaw = new Array[Double](rows * f)
    fx.nuisance.foreach(_.copyRowMajorTo(nuisanceRaw))
    val nuisance = whiten(fx, f, nuisanceRaw)
    val response = whiten(fx, 1, fx.rawResponse)
    Array.tabulate(n + f): j =>
      var sum = 0.0
      var t = 0
      while t < rows do
        val column = if j < n then x(t * n + j) else nuisance(t * f + j - n)
        sum += column * response(t)
        t += 1
      sum

  private def denseSolve(normal: DMat, rhs: Array[Double]): Array[Double] =
    val column = DMat.tabulate(rhs.length, 1)((i, _) => rhs(i))
    val result = normal.cholesky.fold(throw _, identity).solve(column).fold(throw _, identity)
    Array.tabulate(rhs.length)(i => result(i, 0))

  private def conditionInf(normal: DMat): Double =
    val size = normal.rows
    val inverseRowSums = Array.fill(size)(0.0)
    var normalNorm = 0.0
    var row = 0
    while row < size do
      var rowSum = 0.0
      var col = 0
      while col < size do
        rowSum += math.abs(normal(row, col))
        col += 1
      normalNorm = math.max(normalNorm, rowSum)
      row += 1
    var col = 0
    while col < size do
      val unit = Array.fill(size)(0.0)
      unit(col) = 1.0
      val inverseColumn = denseSolve(normal, unit)
      row = 0
      while row < size do
        inverseRowSums(row) += math.abs(inverseColumn(row))
        row += 1
      col += 1
    normalNorm * inverseRowSums.max

  private def norm(values: Array[Double]): Double =
    math.sqrt(values.map(x => x * x).sum)

  private def difference(left: Array[Double], right: Array[Double]): Double =
    norm(Array.tabulate(left.length)(i => left(i) - right(i)))

  private def coefficients(result: TrialConditionalResult): Array[Double] =
    (result.trialAmplitudes ++ result.nuisanceCoefficients).toArray

  test("arbitrary reference RHS uses the original projector normal inverse"):
    for lambda <- Vector(1e-6, 2.0, 1e4); nuisance <- Vector(0, 2); coincident <- Vector(false, true) do
      val fx = fixture(gaussian, lambda, nuisance, coincident = coincident)
      val (_, objective, _, node) = reference(fx)
      val rhs = Array.tabulate(fx.trials + nuisance)(i => 0.3 * math.sin(0.8 * (i + 1)) + (if i % 2 == 0 then 0.1 else -0.2))
      val actual = new Array[Double](rhs.length)
      objective.solveConditionalReference(node, rhs, actual).fold(error => fail(error.message), identity)
      val normal = denseNormal(fx, objective.grid.point(node))
      val expected = denseSolve(normal, rhs)
      val relative = difference(actual, expected) / math.max(1.0, norm(expected))
      // At tiny lambda, coincident/near-coincident trial columns are only
      // weakly regularized on the within-condition contrast subspace. Report
      // the dense oracle's infinity-norm condition estimate with the wider
      // forward-error tolerance; the ordinary fixtures remain tighter.
      if lambda == 1e-6 then
        println(s"reference normal conditionInf=${conditionInf(normal)} relative=$relative nuisance=$nuisance coincident=$coincident")
      assert(relative < (if lambda == 1e-6 then 1e-6 else 1e-8),
        s"arbitrary RHS mismatch lambda=$lambda nuisance=$nuisance coincident=$coincident: $relative")
      val work = objective.work.snapshot
      assertEquals(work.attempted.conditionalInverseAttempts, 1L)
      assertEquals(work.attempted.conditionalInverseFailures, 0L)
      assertEquals(work.attempted.conditionalReadoutAttempts, 0L)
      assertEquals(work.attempted.conditionalCorrectionAttempts, 0L)
      assertEquals(work.attempted.solveAttempts, 1L)
      assertEquals(work.bandedSolveCalls, 1L)
      val rejected = objective.solveConditionalReference(node, Array(1.0), actual)
      assertEquals(rejected, Left(TrialBandedError.ReadoutRhs(rhs.length, 1)))
      assertEquals(objective.solveConditionalReference(node, rhs, Array(0.0)),
        Left(TrialBandedError.ReadoutOutput(rhs.length, 1)))
      val nonfinite = rhs.clone()
      nonfinite(2) = Double.NaN
      assert(objective.solveConditionalReference(node, nonfinite, actual).left.toOption.exists:
        case TrialBandedError.NonFiniteReadoutRhs(2, _) => true
        case _ => false)
      val refusedWork = objective.work.snapshot
      assertEquals(refusedWork.attempted.conditionalInverseAttempts, 4L)
      assertEquals(refusedWork.attempted.conditionalInverseFailures, 3L)
      assertEquals(refusedWork.attempted.solveAttempts, 1L)

  test("zero displacement is the exact prepared-node control"):
    val fx = fixture(gaussian, lambda = 2.0, nuisanceColumns = 2, ar = true)
    val (_, objective, encoded, node) = reference(fx)
    val point = objective.grid.point(node)
    val result = new TrialConditionalSolve(objective.newWorker())
      .solve(encoded, node, point.coordinates).fold(error => fail(error.message), identity)
    val expected = denseSolve(denseNormal(fx, point), denseRhs(fx, point))
    assert(difference(coefficients(result), expected) / math.max(1.0, norm(expected)) < 1e-8)
    assert(result.preparedBasisResidualNorm < 1e-8)
    assertEquals(result.work.referenceInverseAttempts, 3L)
    val work = result.work
    assertEquals(work.residualCorrections, 1)

  test("corrected actual-shape coefficients and prepared-basis residual match dense original equations"):
    for (basis, nuisance, ar, coincident, lambda) <- Vector(
      (gaussian, 2, false, false, 2.0),
      (gaussian, 0, true, true, 1e-6),
      (cascade, 2, true, false, 2.0),
      (cascade, 0, false, true, 1e4)) do
      val fx = fixture(basis, lambda, nuisance, ar, coincident)
      val (prep, objective, encoded, node) = reference(fx)
      val referencePoint = objective.grid.point(node)
      val delta = if basis.family.dimension == 3 then Vector(0.01, -0.011, 0.008) else Vector(0.055, -0.024)
      val actual = referencePoint.coordinates.zip(delta).map((x, dx) => x + dx)
      val solver = new TrialConditionalSolve(objective.newWorker())
      val result = solver.solve(encoded, node, actual).fold(error => fail(error.message), identity)
      val dense = denseSolve(denseNormal(fx, ShapePoint.unsafe(actual)), denseRhs(fx, ShapePoint.unsafe(actual)))
      val relative = difference(coefficients(result), dense) / math.max(1.0, norm(dense))
      // The small-lambda/coincident fixture has conditionInf about 2.9e6.
      // Unlike the reference inverse check, this also includes the local
      // cubic truncation at a fixed off-node displacement (observed ~1.1e-5).
      if lambda == 1e-6 then
        println(s"off-node normal conditionInf=${conditionInf(denseNormal(fx, ShapePoint.unsafe(actual)))} relative=$relative")
      assert(relative < (if lambda == 1e-6 then 5e-5 else 2e-5),
        s"off-node mismatch ${basis.family.name}, lambda=$lambda, nuisance=$nuisance: $relative")
      var condition = 0
      while condition < prep.conditions do
        val trials = prep.membership.trialsOf(condition)
        val arithmetic = trials.map(i => result.trialAmplitudes(i)).sum / trials.length
        assertEqualsDouble(result.conditionMeans(condition), arithmetic, 1e-12)
        condition += 1
      val normal = denseNormal(fx, ShapePoint.unsafe(actual))
      val rhs = denseRhs(fx, ShapePoint.unsafe(actual))
      val fitted = coefficients(result)
      val residual = Array.tabulate(rhs.length): i =>
        var value = rhs(i)
        var j = 0
        while j < rhs.length do
          value -= normal(i, j) * fitted(j)
          j += 1
        value
      assertEqualsDouble(result.preparedBasisResidualNorm, norm(residual), 1e-7)
      assertEquals(result.work.referenceInverseAttempts, 3L)
      assertEquals(result.work.referenceInverseFailures, 0L)
      assertEquals(result.work.bandedSolveAttempts, 3L)
      assertEquals(result.work.bandedSolveFailures, 0L)
      assertEquals(result.work.bandedRightHandSideAttempts, 3L)
      assertEquals(result.work.factorAttempts, 0L)
      assertEquals(result.work.continuousFactors, 0L)
      assertEquals(result.work.exactReadoutFactorAttempts, 0L)
      assertEquals(result.work.residualCorrections, 1)
      val snapshot = solver.worker.work.snapshot
      assertEquals(snapshot.attempted.conditionalReadoutAttempts, 1L)
      assertEquals(snapshot.attempted.conditionalReadoutFailures, 0L)
      assertEquals(snapshot.attempted.conditionalCorrectionAttempts, 1L)
      assertEquals(snapshot.attempted.conditionalCorrectionFailures, 0L)
      assertEquals(snapshot.attempted.readoutAttempts, 0L)
      assertEquals(snapshot.attempted.readoutFailures, 0L)
      assertEquals(snapshot.amplitudeCorrections, 0L)
      assertEquals(snapshot.continuousFactors, 0L)
      assertEquals(objective.setupReceipt, objective.newWorker().setupReceipt)

  test("one residual correction has local cubic error before floating-point floor"):
    val fx = fixture(gaussian, lambda = 2.0)
    val (_, objective, encoded, node) = reference(fx)
    val ref = objective.grid.point(node).coordinates
    val direction = Vector(0.3, -0.14)
    val solver = new TrialConditionalSolve(objective.newWorker())
    val scales = Vector(1.0, 0.5, 0.25, 0.125)
    val observations = scales.map: scale =>
      val actual = ref.zip(direction).map((x, dx) => x + scale * dx)
      val result = solver.solve(encoded, node, actual).fold(error => fail(error.message), identity)
      val expected = denseSolve(denseNormal(fx, ShapePoint.unsafe(actual)), denseRhs(fx, ShapePoint.unsafe(actual)))
      (difference(coefficients(result), expected),
        difference(solver.lastFirstOrderPredictor.toArray, expected))
    val errors = observations.map(_._1)
    val omittedCorrectionErrors = observations.map(_._2)
    val ratios = errors.zip(errors.tail).map((larger, smaller) => larger / smaller)
    val omittedRatios = omittedCorrectionErrors.zip(omittedCorrectionErrors.tail)
      .map((larger, smaller) => larger / smaller)
    println(s"conditional cubic errors=$errors ratios=$ratios; omitted correction errors=$omittedCorrectionErrors ratios=$omittedRatios")
    assert(ratios.forall(_ > 6.0), s"cubic ratio must separate quadratic: $errors, $ratios")
    assert(omittedCorrectionErrors.head > errors.head * 4.0)
    assert(omittedRatios.forall(_ < 6.0), s"omitting the correction must remain quadratic: $omittedRatios")
    val snapshot = solver.worker.work.snapshot
    assertEquals(snapshot.attempted.conditionalInverseAttempts, 12L)
    assertEquals(snapshot.attempted.conditionalReadoutAttempts, 4L)
    assertEquals(snapshot.attempted.conditionalCorrectionAttempts, 4L)
    assertEquals(snapshot.attempted.readoutAttempts, 0L)
    assertEquals(snapshot.amplitudeCorrections, 0L)

    // An independently differenced first-order predictor exposes both an
    // omitted correction and a reversed shape direction at this displacement.
    val epsilon = 1e-4
    def denseAt(scale: Double): Array[Double] =
      val point = ShapePoint.unsafe(ref.zip(direction).map((x, dx) => x + scale * dx))
      denseSolve(denseNormal(fx, point), denseRhs(fx, point))
    val base = denseAt(0.0)
    val plus = denseAt(epsilon)
    val minus = denseAt(-epsilon)
    val firstOrder = Array.tabulate(base.length)(i => base(i) + (plus(i) - minus(i)) / (2.0 * epsilon))
    val reversed = Array.tabulate(base.length)(i => base(i) - (plus(i) - minus(i)) / (2.0 * epsilon))
    val exact = denseAt(1.0)
    assert(difference(firstOrder, exact) > errors(0) * 4.0)
    assert(difference(reversed, exact) > errors(0) * 10.0)

  test("invalid shape, node, owner and certificate refuse before inverse work"):
    val fx = fixture(gaussian)
    val (prep, objective, encoded, node) = reference(fx)
    val worker = objective.newWorker()
    val solver = new TrialConditionalSolve(worker)
    val center = objective.grid.point(node).coordinates
    assertEquals(solver.solve(encoded, -1, center),
      Left(TrialConditionalError.InvalidReferenceNode(-1, objective.grid.count)))
    assert(solver.solve(encoded, node, Vector(Double.NaN, center(1))).left.toOption.exists:
      case TrialConditionalError.InvalidShape(_) => true
      case _ => false)
    assertEquals(solver.solve(encoded, node, center, TrialConditionalEvidence.CertifiedOriginalEquations),
      Left(TrialConditionalError.CertificateUnavailable))
    val foreign = TrialBandedPreparation.prepare(fx.expanded, None, fx.nuisance, fx.lambda)
      .fold(error => fail(error.message), identity).encodeWhitened(fx.rawResponse)
      .fold(error => fail(error.message), identity)
    assertEquals(solver.solve(foreign, node, center), Left(TrialConditionalError.ForeignResponse))
    assertEquals(worker.work.snapshot.attempted.conditionalInverseAttempts, 0L)
    assertEquals(worker.work.snapshot.attempted.conditionalReadoutAttempts, 4L)
    assertEquals(worker.work.snapshot.attempted.conditionalReadoutFailures, 4L)
    assertEquals(worker.work.snapshot.attempted.conditionalCorrectionAttempts, 0L)
    assertEquals(worker.work.snapshot.attempted.conditionalCorrectionFailures, 0L)
    assertEquals(worker.work.snapshot.attempted.readoutAttempts, 0L)
    assertEquals(worker.work.snapshot.attempted.readoutFailures, 0L)
    assertEquals(worker.work.snapshot.amplitudeCorrections, 0L)
    assert(prep ne foreign.owner)

  test("same-worker ML exact payload recovers dense nuisance and measured residual without a second solve"):
    for kernel <- Vector(gaussian, cascade); nuisance <- Vector(0, 2) do
      val fx = fixture(kernel, nuisanceColumns = nuisance, ar = true)
      val (prep, objective, encoded, node) = reference(fx)
      val backend = TrialBandedMlBackend.make(objective).result.fold(error => fail(error.message), identity)
      val epoch = backend.pointAt(whiten(fx, 1, fx.rawResponse)).result.fold(error => fail(error.message), identity)
      val at = objective.grid.point(node).coordinates.zipWithIndex.map((x, i) => x + 0.07 * objective.grid.step(i))
      val jet = backend.jetAt(at).result.fold(error => fail(error.message), identity)
      val before = backend.legacyWork
      val mlBefore = backend.workerWorkSnapshot
      val payload = backend.exactReadoutPayload(at, epoch).fold(error => fail(error.message), identity)
      val normal = denseNormal(fx, ShapePoint.unsafe(at))
      val rhs = denseRhs(fx, ShapePoint.unsafe(at))
      val expected = denseSolve(normal, rhs)
      val actual = (payload.raw.trialAmplitudes ++ payload.summary.nuisanceCoefficients).toArray
      actual.zip(expected).foreach((a, b) => assertEqualsDouble(a, b, 1e-8))
      val residual = Array.tabulate(rhs.length)(i => rhs(i) - actual.indices.map(j => normal(i, j) * actual(j)).sum)
      assertEqualsDouble(payload.summary.preparedBasisResidualNorm, norm(residual), 1e-10)
      assertEqualsDouble(payload.raw.penalizedEnergy, jet.raw.jet.energy, 1e-9)
      payload.raw.conditionMeans.zip(jet.raw.jet.amplitudes).foreach((a, b) => assertEqualsDouble(a, b, 1e-8))
      payload.summary.conditionMeans.indices.foreach: c =>
        val members = prep.membership.trialsOf(c)
        assertEqualsDouble(payload.summary.conditionMeans(c), members.map(actual(_)).sum / members.length, 1e-10)
      assertEquals(backend.workerWorkSnapshot, mlBefore)
      assertEquals(backend.legacyWork.voxels, before.voxels)
      assertEquals(backend.legacyWork.trialBasisScores, before.trialBasisScores)
      assertEquals(payload.numerical.attempted.readoutAttempts, 1L)
      assertEquals(payload.numerical.attempted.exactReadoutFactorAttempts, 1L)
      assertEquals(payload.numerical.exactReadoutFactors, 1L)
      assertEquals(payload.numerical.attempted.conditionalReadoutAttempts, 0L)
      assertEquals(payload.measurement, TrialResidualMeasurementWork(1, 0, 1))
      assertEquals(payload.summary.work.factorAttempts, 0L)
      assertEquals(payload.summary.work.bandedSolveAttempts, 0L)
      assertEquals(payload.summary.work.bandedRightHandSideAttempts, 0L)
      assertEquals(payload.summary.work.normalActionApplications, 1L)
      assertEquals(payload.coefficientScratchValues, fx.trials + nuisance)
      assert(payload.measurementScratchValues > 10 * (fx.trials + nuisance))
      val completed = backend.legacyWork
      assert(backend.exactReadoutPayload(at, epoch).toOption.get eq payload)
      assertEquals(backend.exactReadout(at).toOption.get, payload.raw)
      assertEquals(backend.legacyWork, completed)
      assertEquals(backend.measurementWork, TrialResidualMeasurementWork(1, 0, 1))
      // The measurement helper alone cannot alter any objective work fields.
      val solver = new TrialConditionalSolve(objective)
      val snapshot = objective.work.snapshot
      val measured = solver.measureExactSolved(encoded, at, actual).toOption.get
      assertEquals(objective.work.snapshot, snapshot)
      assertEqualsDouble(measured.preparedBasisResidualNorm, norm(residual), 1e-10)

  test("measurement rejects invalid inputs and nonfinite normal actions without returning prior nuisance scratch"):
    val fx = fixture(gaussian, ar = true)
    val (_, objective, encoded, node) = reference(fx)
    val at = objective.grid.point(node).coordinates
    val solved = denseSolve(denseNormal(fx, ShapePoint.unsafe(at)), denseRhs(fx, ShapePoint.unsafe(at)))
    val solver = new TrialConditionalSolve(objective)
    assert(solver.measureExactSolved(encoded, at, solved).isRight)
    val before = objective.work.snapshot
    assert(solver.measureExactSolved(encoded, at, solved.take(1)).isLeft)
    assert(solver.measureExactSolved(encoded, at, solved.updated(0, Double.NaN)).isLeft)
    val (_, _, foreign, _) = reference(fx)
    assertEquals(solver.measureExactSolved(foreign, at, solved), Left(TrialConditionalError.ForeignResponse))
    val gram = objective.preparation.gramBlocksData
    val saved = gram.clone()
    try
      java.util.Arrays.fill(gram, Double.NaN)
      assert(solver.measureExactSolved(encoded, at, solved).left.toOption.exists:
        case TrialConditionalError.NonFiniteAssembly("normal residual") => true
        case _ => false)
    finally Array.copy(saved, 0, gram, 0, gram.length)
    assertEquals(objective.work.snapshot, before)
    assertEquals(solver.measurementWork, TrialResidualMeasurementWork(5, 4, 2))
