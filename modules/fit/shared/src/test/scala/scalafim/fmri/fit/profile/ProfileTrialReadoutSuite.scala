package scalafim.fmri.fit.profile

import gale.linalg.DMat
import scalafim.fmri.design.{ColumnId, ConditionId, ScanIndex, TrialId}
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.fmri.design.hrf.{ExpandedTrialDesign, HrfKernelBasis, KernelBasisSpec, TrialMembership}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{Cascade34Family, GaussianFamily, NormalizationRule, ParametricHrfFamily, ShapePoint}

class ProfileTrialReadoutSuite extends munit.FunSuite:
  // Independent original-time readout checks took 69 s in the full Java 17 gate.
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")
  private lazy val basis = HrfKernelBasis.compile(KernelBasisSpec(
    GaussianFamily.Default,
    PositiveSeconds(0.2).fold(error => fail(error.message), identity),
    Vector(26, 21), tolerance = 1e-4, maxRank = 40))
    .fold(error => fail(error.message), identity)
  private lazy val cascade = HrfKernelBasis.compile(KernelBasisSpec(
    Cascade34Family.Default,
    PositiveSeconds(0.2).fold(error => fail(error.message), identity),
    Vector(9, 7, 5), tolerance = 1e-4, maxRank = 40))
    .fold(error => fail(error.message), identity)

  private final case class Setup(
      expanded: ExpandedTrialDesign,
      preparation: TrialBandedPreparation,
      objective: TrialBandedObjective,
      axis: ProfileTrialAxis,
      referenceNode: Int,
      nuisance: Option[DMat],
      rawResponse: Array[Double],
      whitening: Option[WhiteningPlan]):
    def rows: Int = preparation.rows

  private def setup(
      nuisanceColumns: Int = 2,
      ar: Boolean = false,
      lambda: Double = 2.0,
      coincident: Boolean = false,
      kernel: HrfKernelBasis = basis): Setup =
    val rows = if kernel.family.dimension == 3 then 48 else 80
    val run = rows / 2
    val membership = TrialMembership.make(Vector(0, 1, 1, 1, 2, 2), 3)
      .fold(error => fail(error.message), identity)
    val onsets =
      if coincident then Vector(2.5, 8.0, 8.0, 17.0, 4.0, 14.0)
      else Vector(2.5, 8.0, 15.0, 21.0, 4.0, 14.0)
    val expanded = ExpandedTrialDesign.lower(
      onsets.map(Seconds(_)),
      Vector(0, 0, 0, 0, 1, 1), Vector.fill(6)(Seconds(0.0)), membership,
      SamplingFrame(blockLens = Seq(run, run), tr = Seq(1.0, 1.0)), kernel, Seconds(0.2))
      .fold(error => fail(error.message), identity)
    val nuisance =
      if nuisanceColumns == 0 then None
      else Some(DMat.tabulate(expanded.rows, nuisanceColumns): (row, col) =>
        if col == 0 then 1.0 else (row % run - (run - 1) * 0.5) / run)
    val center = centerPoint(kernel.family)
    val x = designAt(expanded, center)
    val signed = Array(0.7, -0.4, 0.35, 0.9, -0.65, 0.2)
    val response = Array.tabulate(rows): row =>
      var value = 0.03 * math.sin(0.31 * row) - 0.02 * math.cos(0.17 * row)
      var trial = 0
      while trial < expanded.trials do
        value += x(row * expanded.trials + trial) * signed(trial)
        trial += 1
      nuisance.foreach: matrix =>
        value += 0.42 * matrix(row, 0)
        if nuisanceColumns > 1 then value -= 0.18 * matrix(row, 1)
      value
    val whitening =
      if ar then Some(WhiteningPlan.byRun(
        Vector(ArmaCoefficients.ar(0.31), ArmaCoefficients.ar(-0.22)),
        Vector(TimeSegment(0, run, 0), TimeSegment(run, rows, 1))))
      else None
    val preparation = TrialBandedPreparation.prepare(expanded, whitening, nuisance, lambda)
      .fold(error => fail(error.message), identity)
    val grid = NodeGrid(kernel.family.chart, Vector.fill(kernel.family.dimension)(3))
    val objective = preparation.objective(grid).fold(error => fail(error.message), identity)
    val axis = ProfileTrialAxis.make(
      expanded, preparation,
      Vector.tabulate(6)(i => TrialId.unsafe(s"trial-${i + 1}")),
      Vector("singleton", "unequal", "pair").map(ConditionId.unsafe),
      Vector("singleton", "unequal", "unequal", "unequal", "pair", "pair").map(ConditionId.unsafe),
      Vector.tabulate(nuisanceColumns)(i => ColumnId.unsafe(s"physical-nuisance-${i + 1}")),
      Vector.tabulate(expanded.rows)(i => ScanIndex.unsafeOneBased(i + 101)))
      .fold(error => fail(error.message), identity)
    Setup(expanded, preparation, objective, axis,
      grid.indexOf(Array.fill(kernel.family.dimension)(1)), nuisance, response, whitening)

  private def centerPoint(family: ParametricHrfFamily): ShapePoint =
    ShapePoint.unsafe(Vector.tabulate(family.dimension): axis =>
      0.5 * (family.chart.lower(axis) + family.chart.upper(axis)))

  /** The dense oracle uses original time-row columns, not packed trial Grams. */
  private def designAt(expanded: ExpandedTrialDesign, point: ShapePoint): Array[Double] =
    val coefficients = new Array[Double](expanded.rank)
    expanded.basis.coefficientsInto(point, new Array[Double](expanded.basis.fineCount), coefficients)
    val source = expanded.term.data.data
    val n = expanded.trials
    val m = expanded.rank
    Array.tabulate(expanded.rows * n): index =>
      val row = index / n
      val trial = index % n
      var value = 0.0
      var p = 0
      while p < m do
        value += source(row * n * m + p * n + trial) * coefficients(p)
        p += 1
      value

  /** Independently apply two-run AR(1), including each nonunit first scale. */
  private def whiten(s: Setup, columns: Int, data: Array[Double]): Array[Double] =
    s.whitening match
      case None => data.clone()
      case Some(plan) =>
        val out = new Array[Double](data.length)
        plan.segments.foreach: segment =>
          val phi = plan.coefficientsFor(segment).phi.head
          val scale = if plan.exactFirstAr1 then math.sqrt(1.0 - phi * phi) else 1.0
          var col = 0
          while col < columns do
            out(segment.start * columns + col) = scale * data(segment.start * columns + col)
            var row = segment.start + 1
            while row < segment.endExclusive do
              out(row * columns + col) = data(row * columns + col) - phi * data((row - 1) * columns + col)
              row += 1
            col += 1
        out

  private def nuisanceData(s: Setup): Array[Double] =
    val out = new Array[Double](s.rows * s.preparation.nuisanceColumns)
    s.nuisance.foreach(_.copyRowMajorTo(out))
    out

  private def denseNormal(s: Setup, point: ShapePoint): DMat =
    val n = s.preparation.trials
    val f = s.preparation.nuisanceColumns
    val x = whiten(s, n, designAt(s.expanded, point))
    val nuisance = whiten(s, f, nuisanceData(s))
    val membership = s.preparation.membership
    DMat.tabulate(n + f, n + f): (i, j) =>
      var value = 0.0
      var row = 0
      while row < s.rows do
        val xi = if i < n then x(row * n + i) else nuisance(row * f + i - n)
        val xj = if j < n then x(row * n + j) else nuisance(row * f + j - n)
        value += xi * xj
        row += 1
      if i < n && j < n then
        val same = membership.conditionOfTrial(i) == membership.conditionOfTrial(j)
        value += s.preparation.lambda * ((if i == j then 1.0 else 0.0) -
          (if same then 1.0 / membership.trialsOf(membership.conditionOfTrial(i)).length else 0.0))
      value

  private def denseRhs(s: Setup, point: ShapePoint, originalResponse: Array[Double]): Array[Double] =
    denseRhsWhitened(s, point, whiten(s, 1, originalResponse))

  private def denseRhsWhitened(s: Setup, point: ShapePoint, whitenedResponse: Array[Double]): Array[Double] =
    val n = s.preparation.trials
    val f = s.preparation.nuisanceColumns
    val x = whiten(s, n, designAt(s.expanded, point))
    val nuisance = whiten(s, f, nuisanceData(s))
    Array.tabulate(n + f): col =>
      var value = 0.0
      var row = 0
      while row < s.rows do
        value += (if col < n then x(row * n + col) else nuisance(row * f + col - n)) * whitenedResponse(row)
        row += 1
      value

  private def denseSolve(normal: DMat, rhs: Array[Double]): Array[Double] =
    val column = DMat.tabulate(rhs.length, 1)((i, _) => rhs(i))
    val solved = normal.cholesky.fold(throw _, identity).solve(column).fold(throw _, identity)
    Array.tabulate(rhs.length)(i => solved(i, 0))

  private def norm(values: Array[Double]): Double =
    math.sqrt(values.map(x => x * x).sum)

  private def difference(left: Array[Double], right: Array[Double]): Double =
    norm(Array.tabulate(left.length)(i => left(i) - right(i)))

  /** Independent dense correction: original time-row equations and a central
    * shape difference supply G1/B1, without production packed-gram jets.
    */
  private def denseCorrectedWhitened(
      s: Setup,
      reference: Vector[Double],
      actual: Vector[Double],
      whitenedResponse: Array[Double]
  ): Array[Double] =
    val delta = actual.zip(reference).map((x, r) => x - r)
    val epsilon = 1e-4
    val plus = ShapePoint.unsafe(reference.zip(delta).map((r, dx) => r + epsilon * dx))
    val minus = ShapePoint.unsafe(reference.zip(delta).map((r, dx) => r - epsilon * dx))
    val refPoint = ShapePoint.unsafe(reference)
    val actualPoint = ShapePoint.unsafe(actual)
    val g0 = denseNormal(s, refPoint)
    val gt = denseNormal(s, actualPoint)
    val gp = denseNormal(s, plus)
    val gm = denseNormal(s, minus)
    val b0 = denseRhsWhitened(s, refPoint, whitenedResponse)
    val bt = denseRhsWhitened(s, actualPoint, whitenedResponse)
    val bp = denseRhsWhitened(s, plus, whitenedResponse)
    val bm = denseRhsWhitened(s, minus, whitenedResponse)
    val size = b0.length
    def derivativeAction(vector: Array[Double]): Array[Double] =
      Array.tabulate(size): row =>
        var value = 0.0
        var col = 0
        while col < size do
          value += ((gp(row, col) - gm(row, col)) / (2.0 * epsilon)) * vector(col)
          col += 1
        value
    val base = denseSolve(g0, b0)
    val g1Base = derivativeAction(base)
    val direction = denseSolve(g0, Array.tabulate(size)(i =>
      (bp(i) - bm(i)) / (2.0 * epsilon) - g1Base(i)))
    val predictor = Array.tabulate(size)(i => base(i) + direction(i))
    val correctionRhs = Array.tabulate(size): row =>
      var value = bt(row)
      var col = 0
      while col < size do
        value -= gt(row, col) * predictor(col)
        col += 1
      value
    val correction = denseSolve(g0, correctionRhs)
    Array.tabulate(size)(i => predictor(i) + correction(i))

  test("physical axes bind source, ordered trial and condition ids, nuisance ids and selected rows"):
    val s = setup()
    val axis = s.axis
    assert(axis.source eq s.expanded)
    assert(axis.preparation eq s.preparation)
    assertEquals(axis.conditionForTrial(1), axis.conditionIds(1))
    assertEquals(s.expanded.canonicalToInput.length, axis.trialIds.length)
    assert(ProfileTrialAxis.make(s.expanded, s.preparation,
      axis.trialIds.reverse, axis.conditionIds, axis.conditionForTrial,
      axis.nuisanceColumnIds, axis.selectedResponseRows).isRight)
    val duplicated = axis.trialIds.updated(1, axis.trialIds.head)
    assertEquals(ProfileTrialAxis.make(s.expanded, s.preparation, duplicated,
      axis.conditionIds, axis.conditionForTrial, axis.nuisanceColumnIds, axis.selectedResponseRows),
      Left(ProfileTrialAxisError.DuplicateTrialId))
    val wrongMembership = axis.conditionForTrial.updated(1, axis.conditionIds.head)
    assertEquals(ProfileTrialAxis.make(s.expanded, s.preparation, axis.trialIds,
      axis.conditionIds, wrongMembership, axis.nuisanceColumnIds, axis.selectedResponseRows),
      Left(ProfileTrialAxisError.MembershipMismatch(1)))
    assertEquals(ProfileTrialAxis.make(s.expanded, s.preparation, axis.trialIds,
      axis.conditionIds, axis.conditionForTrial, Vector.empty, axis.selectedResponseRows),
      Left(ProfileTrialAxisError.NuisanceCount(2, 0)))
    val rowsReversed = axis.selectedResponseRows.reverse
    assertEquals(ProfileTrialResponse.make(axis, rowsReversed, ProfileTrialResponseDomain.Whitened,
      Array.fill(s.rows)(0.0)), Left(ProfileTrialReadoutError.ResponseRows))

  test("trial queries are signed and bound to the exact physical axis"):
    val s = setup(nuisanceColumns = 0)
    val q = ProfileTrialSignedQuery.make("signed", s.axis,
      Vector(1.0, -1.0, 0.0, 0.0, 0.0, 0.0), 1e-8)
      .fold(error => fail(error.message), identity)
    assertEquals(q.weights.sum, 0.0)
    assert(ProfileTrialSignedQuery.make("", s.axis, q.weights, 1e-8).isLeft)
    assert(ProfileTrialSignedQuery.make("bad", s.axis, Vector(Double.NaN) ++ q.weights.tail, 1e-8).isLeft)
    assert(ProfileTrialSignedQuery.make("bad", s.axis, q.weights.take(5), 1e-8).isLeft)
    assert(ProfileTrialSignedQuery.make("bad", s.axis, q.weights, 0.0).isLeft)
    val request = OutputRequest.TrialQueries(Vector(q), NormalizationRule.Density)
    assert(request.validateForTrial(s.axis).isRight)
    assert(request.validateFor(s.axis.conditionIds.length).isLeft)
    val duplicate = OutputRequest.TrialQueries(Vector(q, q), NormalizationRule.Density)
    assertEquals(duplicate.validateForTrial(s.axis), Left(OutputError.DuplicateLabel("signed")))
    val reversedAxis = ProfileTrialAxis.make(s.expanded, s.preparation,
      s.axis.trialIds.reverse, s.axis.conditionIds, s.axis.conditionForTrial,
      s.axis.nuisanceColumnIds, s.axis.selectedResponseRows)
      .fold(error => fail(error.message), identity)
    assertEquals(request.validateForTrial(reversedAxis), Left(OutputError.ForeignTrialAxis("signed")))

  test("freeze binds actual coordinates and refuses unavailable original certificate before work"):
    val s = setup()
    val actual = s.objective.grid.point(s.referenceNode).coordinates.zip(Vector(0.05, -0.02)).map((x, dx) => x + dx)
    val frozen = ProfileTrialReadout.freeze(s.objective, s.axis, actual, s.referenceNode,
      NormalizationRule.Density, ProfileTrialReadoutMode.CorrectedReference)
      .fold(error => fail(error.message), identity)
    assertEquals(frozen.actualCoordinates, actual)
    assertEquals(frozen.nativeLambda, 2.0)
    assertEqualsDouble(frozen.equivalentNormalizedLambda,
      frozen.normalizationScale * frozen.normalizationScale * frozen.nativeLambda, 1e-12)
    assertEquals(ProfileTrialReadout.freeze(s.objective, s.axis, actual, s.referenceNode,
      NormalizationRule.Density, ProfileTrialReadoutMode.CorrectedReference,
      ProfileTrialEvidenceRequest.CertifiedOriginalEquations),
      Left(ProfileTrialReadoutError.CertificateUnavailable))
    assertEquals(frozen.newWorker().workSnapshot.attempted.conditionalInverseAttempts, 0L)
    assert(ProfileTrialReadout.freeze(s.objective, s.axis, actual, -1,
      NormalizationRule.Density, ProfileTrialReadoutMode.CorrectedReference).isLeft)
    assertEquals(ProfileTrialReadout.freeze(s.objective, s.axis, actual, s.referenceNode,
      NormalizationRule.PositiveComponentArea, ProfileTrialReadoutMode.CorrectedReference),
      Left(ProfileTrialReadoutError.UnsupportedNormalization(NormalizationRule.PositiveComponentArea)))

  test("corrected off-node trial and signed query outputs match dense original equations"):
    for (kernel, nuisance, ar, coincident, lambda) <- Vector(
      (basis, 2, false, false, 2.0),
      (basis, 0, true, true, 1e-6),
      (cascade, 2, true, false, 1e4)) do
      val s = setup(nuisance, ar, lambda, coincident, kernel)
      val reference = s.objective.grid.point(s.referenceNode).coordinates
      val delta = if kernel.family.dimension == 3 then Vector(0.01, -0.011, 0.008) else Vector(0.055, -0.024)
      val actual = reference.zip(delta).map((x, dx) => x + dx)
      val point = ShapePoint.unsafe(actual)
      val dense = denseSolve(denseNormal(s, point), denseRhs(s, point, s.rawResponse))
      val rule = if kernel.family.dimension == 3 then NormalizationRule.PositiveComponentArea else NormalizationRule.Density
      val frozen = ProfileTrialReadout.freeze(s.objective, s.axis, actual, s.referenceNode,
        rule, ProfileTrialReadoutMode.CorrectedReference)
        .fold(error => fail(error.message), identity)
      val response = ProfileTrialResponse.make(s.axis, s.axis.selectedResponseRows,
        ProfileTrialResponseDomain.Original, s.rawResponse)
        .fold(error => fail(error.message), identity)
      val full = frozen.newWorker().evaluate(response, OutputRequest.TrialAmplitudes(rule))
        .fold(error => fail(error.message), identity)
      val n = s.preparation.trials
      val expected = Array.tabulate(n)(i => dense(i) / frozen.normalizationScale)
      val observed = full.trialAmplitudes.getOrElse(fail("amplitude output absent")).toArray
      val relative = difference(observed, expected) / math.max(1.0, norm(expected))
      val tolerance = if lambda == 1e-6 then 5e-5 else 2e-5
      assert(relative < tolerance, s"${kernel.family.name} lambda=$lambda relative=$relative")
      var condition = 0
      while condition < s.preparation.conditions do
        val trials = s.preparation.membership.trialsOf(condition)
        val arithmetic = trials.map(i => observed(i)).sum / trials.length
        assertEqualsDouble(full.conditionMeans(condition), arithmetic, 1e-12)
        condition += 1
      val nuisanceExpected = dense.drop(n)
      assert(difference(full.nuisanceCoefficients.toArray, nuisanceExpected) <
        (if lambda == 1e-6 then 5e-5 else 2e-5))
      assertEquals(full.axis.selectedResponseRows, s.axis.selectedResponseRows)
      assertEquals(full.actualCoordinates, actual)
      assertEquals(full.work.referenceInverseAttempts, 3L)
      assertEquals(full.work.residualCorrections, 1)
      assertEquals(full.work.normalActionApplications, 3L)
      assertEquals(full.work.exactReadoutFactorAttempts, 0L)
      assertEquals(full.work.retainedTrialAmplitudeValues, n)
      val weights = Vector(dense(1), -dense(0), 0.0, 0.0, 0.0, 0.0)
      val query = ProfileTrialSignedQuery.make("near-cancel", s.axis, weights, 1e-4)
        .fold(error => fail(error.message), identity)
      val queryWorker = frozen.newWorker()
      val queryOnly = queryWorker.evaluate(response,
        OutputRequest.TrialQueries(Vector(query), rule))
        .fold(error => fail(error.message), identity)
      assertEquals(queryOnly.trialAmplitudes, None)
      assertEquals(queryOnly.work.retainedTrialAmplitudeValues, 0)
      assertEquals(queryOnly.work.wrapperCoefficientBufferValues, n + s.preparation.nuisanceColumns)
      assertEquals(queryOnly.work.alwaysRetainedAdjointRowValues, s.rows)
      assertEquals(queryOnly.work.retainedAdjointOutputValues, 0)
      assertEquals(queryOnly.work.responseRowsEncoded, s.rows)
      assertEquals(queryOnly.work.whiteningForwardRowsVisited, if ar then s.rows else 0)
      assertEquals(queryWorker.workSnapshot.trialBasisScores,
        n.toLong * s.preparation.basisRank)
      assertEquals(queryOnly.work.referenceInverseAttempts, 3L)
      assertEquals(queryOnly.work.normalActionApplications, 3L)
      val expectedQuery = weights.indices.map(i => weights(i) * expected(i)).sum
      assert(math.abs(queryOnly.queries.head.value - expectedQuery) < 1e-4)
      assert(math.abs(expectedQuery) < 1e-12)
      assertEqualsDouble(queryOnly.queries.head.value,
        weights.indices.map(i => weights(i) * observed(i)).sum, 1e-10)
      assert(queryOnly.evidence.preparedBasisNormalResidualNorm.isFinite)
      val conditionQuery = SignedQuery.make("condition-signed", Vector(1.0, -1.0, 0.0), 1e-4)
        .fold(error => fail(error.message), identity)
      val conditions = frozen.newWorker().evaluate(response,
        OutputRequest.ConditionQueries(Vector(conditionQuery), rule))
        .fold(error => fail(error.message), identity)
      assertEquals(conditions.trialAmplitudes, None)
      assertEquals(conditions.work.retainedTrialAmplitudeValues, 0)
      assertEqualsDouble(conditions.queries.head.value,
        full.conditionMeans(0) - full.conditionMeans(1), 1e-10)

  test("scoped retained storage is invariant across query evaluation, transpose and reuse"):
    for nuisanceColumns <- Vector(0, 2) do
      val s = setup(nuisanceColumns = nuisanceColumns, ar = true)
      val actual = s.objective.grid.point(s.referenceNode).coordinates
        .zip(Vector(0.035, -0.02)).map((x, dx) => x + dx)
      val frozen = ProfileTrialReadout.freeze(s.objective, s.axis, actual, s.referenceNode,
        NormalizationRule.Density, ProfileTrialReadoutMode.CorrectedReference)
        .fold(error => fail(error.message), identity)
      val query = ProfileTrialSignedQuery.make("reuse-signed", s.axis,
        Vector(0.5, -0.3, 0.0, 0.2, -0.1, 0.7), 1e-6)
        .fold(error => fail(error.message), identity)
      val response = ProfileTrialResponse.make(s.axis, s.axis.selectedResponseRows,
        ProfileTrialResponseDomain.Original, s.rawResponse)
        .fold(error => fail(error.message), identity)
      val request = OutputRequest.TrialQueries(Vector(query), NormalizationRule.Density)
      val worker = frozen.newWorker()
      val first = worker.evaluate(response, request).fold(error => fail(error.message), identity)
      val adjoint = worker.transposeOriginal(query).fold(error => fail(error.message), identity)
      val second = worker.evaluate(response, request).fold(error => fail(error.message), identity)
      for result <- Vector(first, second) do
        assertEquals(result.trialAmplitudes, None)
        assertEquals(result.queries.length, 1)
        assertEquals(result.work.retainedTrialAmplitudeValues, 0)
        assertEquals(result.work.retainedAdjointOutputValues, 0)
        assertEquals(result.work.wrapperCoefficientBufferValues, s.preparation.trials + nuisanceColumns)
        assertEquals(result.work.alwaysRetainedAdjointRowValues, s.rows)
      assertEqualsDouble(first.queries.head.value, second.queries.head.value, 1e-12)
      assertEquals(adjoint.work.wrapperCoefficientBufferValues, s.preparation.trials + nuisanceColumns)
      assertEquals(adjoint.work.alwaysRetainedAdjointRowValues, s.rows)
      assertEquals(adjoint.work.retainedAdjointOutputValues, adjoint.values.length)
      assertEquals(adjoint.values.length, s.rows)
      assertEquals(adjoint.work.retainedTrialAmplitudeValues, 0)
      assertEqualsDouble(first.queries.head.value,
        adjoint.values.indices.map(i => adjoint.values(i) * s.rawResponse(i)).sum, 1e-8)
      val sourceValues = s.expanded.term.data.data.length.toLong
      assertEquals(sourceValues, s.rows.toLong * s.preparation.trials * s.expanded.rank)
      assertEquals(s.preparation.retainedSourceDesignDataValues, sourceValues)
      assertEquals(s.preparation.retainedSourceDesignDataBytes, 8L * sourceValues)
      assert(s.preparation.source eq s.expanded)
      assert(frozen.bank.newWorker().preparation.source eq s.expanded)
      // Existing kernel estimates stay separate from the retained caller source.
      assertEquals(s.objective.estimatedSharedBytes,
        s.preparation.receipt.estimatedBytes + s.objective.grid.count.toLong * s.objective.estimatedReferenceBytes)
      assertEquals(s.objective.estimatedEngineBytes,
        s.objective.estimatedSharedBytes + s.objective.estimatedWorkerBytes)

  test("explicit exact-shape mode charges one factor and matches dense original coefficients"):
    val s = setup()
    val ref = s.objective.grid.point(s.referenceNode).coordinates
    val actual = ref.zip(Vector(0.07, -0.03)).map((x, dx) => x + dx)
    val frozen = ProfileTrialReadout.freeze(s.objective, s.axis, actual, s.referenceNode,
      NormalizationRule.Density, ProfileTrialReadoutMode.ExactShape)
      .fold(error => fail(error.message), identity)
    val response = ProfileTrialResponse.make(s.axis, s.axis.selectedResponseRows,
      ProfileTrialResponseDomain.Original, s.rawResponse)
      .fold(error => fail(error.message), identity)
    val result = frozen.newWorker().evaluate(response, OutputRequest.TrialAmplitudes(NormalizationRule.Density))
      .fold(error => fail(error.message), identity)
    val dense = denseSolve(denseNormal(s, ShapePoint.unsafe(actual)),
      denseRhs(s, ShapePoint.unsafe(actual), s.rawResponse))
    val amplitudes = result.trialAmplitudes.getOrElse(fail("amplitudes absent"))
    assert(difference(amplitudes.toArray,
      Array.tabulate(s.preparation.trials)(i => dense(i) / frozen.normalizationScale)) < 1e-8)
    assert(difference(result.nuisanceCoefficients.toArray, dense.drop(s.preparation.trials)) < 1e-8)
    assertEquals(result.work.exactReadoutFactorAttempts, 1L)
    assertEquals(result.work.referenceInverseAttempts, 1L)
    assertEquals(result.work.residualCorrections, 0)
    assertEquals(result.work.normalActionApplications, 1L)
    val query = ProfileTrialSignedQuery.make("exact-signed", s.axis,
      Vector(0.5, -0.3, 0.0, 0.2, -0.1, 0.7), 1e-6)
      .fold(error => fail(error.message), identity)
    val queryValue = frozen.newWorker().evaluate(response,
      OutputRequest.TrialQueries(Vector(query), NormalizationRule.Density))
      .fold(error => fail(error.message), identity).queries.head.value
    val transpose = frozen.newWorker().transposeWhitened(query)
      .fold(error => fail(error.message), identity)
    val whitened = whiten(s, 1, s.rawResponse)
    assertEqualsDouble(queryValue,
      transpose.values.indices.map(i => transpose.values(i) * whitened(i)).sum, 1e-9)
    assertEquals(transpose.work.exactReadoutFactorAttempts, 1L)
    assertEquals(transpose.work.referenceInverseAttempts, 1L)
    assertEquals(transpose.work.normalActionApplications, 0L)

  test("frozen corrected operator is linear and has a whitened-response adjoint"):
    val s = setup(ar = true)
    val ref = s.objective.grid.point(s.referenceNode).coordinates
    val actual = ref.zip(Vector(0.05, -0.025)).map((x, dx) => x + dx)
    val frozen = ProfileTrialReadout.freeze(s.objective, s.axis, actual, s.referenceNode,
      NormalizationRule.Density, ProfileTrialReadoutMode.CorrectedReference)
      .fold(error => fail(error.message), identity)
    val q = ProfileTrialSignedQuery.make("signed", s.axis,
      Vector(0.8, -0.7, 0.2, -0.4, 0.3, -0.1), 1e-5)
      .fold(error => fail(error.message), identity)
    val request = OutputRequest.TrialQueries(Vector(q), NormalizationRule.Density)
    val first = whiten(s, 1, s.rawResponse)
    val second = Array.tabulate(s.rows)(row => 0.06 * math.cos(0.27 * row))
    def response(values: Array[Double]): ProfileTrialResponse =
      ProfileTrialResponse.make(s.axis, s.axis.selectedResponseRows,
        ProfileTrialResponseDomain.Whitened, values)
        .fold(error => fail(error.message), identity)
    def evaluate(values: Array[Double]): Double =
      frozen.newWorker().evaluate(response(values), request)
        .fold(error => fail(error.message), identity).queries.head.value
    val forward = evaluate(first)
    val other = evaluate(second)
    val combined = evaluate(Array.tabulate(s.rows)(i => first(i) + second(i)))
    assertEqualsDouble(combined, forward + other, 1e-9)
    val transpose = frozen.newWorker().transposeWhitened(q)
      .fold(error => fail(error.message), identity)
    val dot = transpose.values.indices.map(i => transpose.values(i) * first(i)).sum
    assertEqualsDouble(forward, dot, 1e-8)
    for row <- Vector(0, 1, s.rows / 2 - 1, s.rows / 2, s.rows - 1) do
      val basisResponse = Array.fill(s.rows)(0.0)
      basisResponse(row) = 1.0
      val dense = denseCorrectedWhitened(s, ref, actual, basisResponse)
      val expected = q.weights.indices.map(i => q.weights(i) * dense(i)).sum / frozen.normalizationScale
      assertEqualsDouble(transpose.values(row), expected, 2e-7)
    assertEquals(transpose.selectedRows, s.axis.selectedResponseRows)
    assertEquals(transpose.work.referenceInverseAttempts, 3L)
    assertEquals(transpose.work.normalActionApplications, 2L)
    assertEquals(transpose.work.retainedTrialAmplitudeValues, 0)
    assertEquals(transpose.work.wrapperCoefficientBufferValues,
      s.preparation.trials + s.preparation.nuisanceColumns)
    assertEquals(transpose.work.alwaysRetainedAdjointRowValues, s.rows)
    assertEquals(transpose.work.retainedAdjointOutputValues, s.rows)
    val originalTranspose = frozen.newWorker().transposeOriginal(q)
      .fold(error => fail(error.message), identity)
    assertEquals(originalTranspose.work.whiteningTransposeRowsVisited, s.rows)
    val originalDot = originalTranspose.values.indices.map(i => originalTranspose.values(i) * s.rawResponse(i)).sum
    assertEqualsDouble(forward, originalDot, 1e-8)
    s.whitening.foreach: plan =>
      plan.segments.foreach: segment =>
        val phi = plan.coefficientsFor(segment).phi.head
        val scale = math.sqrt(1.0 - phi * phi)
        var row = segment.start
        while row < segment.endExclusive do
          val diagonal = (if row == segment.start then scale else 1.0) * transpose.values(row)
          val below = if row + 1 < segment.endExclusive then -phi * transpose.values(row + 1) else 0.0
          assertEqualsDouble(originalTranspose.values(row), diagonal + below, 1e-10)
          row += 1

  test("IID original and whitened adjoints coincide; equal-length foreign response axes refuse"):
    val s = setup(ar = false, nuisanceColumns = 0)
    val actual = s.objective.grid.point(s.referenceNode).coordinates
      .zip(Vector(0.025, -0.015)).map((x, dx) => x + dx)
    val frozen = ProfileTrialReadout.freeze(s.objective, s.axis, actual, s.referenceNode,
      NormalizationRule.Unnormalised, ProfileTrialReadoutMode.CorrectedReference)
      .fold(error => fail(error.message), identity)
    val q = ProfileTrialSignedQuery.make("trial", s.axis,
      Vector(1.0, 0.0, -0.5, 0.0, 0.2, 0.0), 1e-8)
      .fold(error => fail(error.message), identity)
    val whitenedTranspose = frozen.newWorker().transposeWhitened(q)
      .fold(error => fail(error.message), identity)
    val originalTranspose = frozen.newWorker().transposeOriginal(q)
      .fold(error => fail(error.message), identity)
    assertEquals(originalTranspose.values, whitenedTranspose.values)
    val response = ProfileTrialResponse.make(s.axis, s.axis.selectedResponseRows,
      ProfileTrialResponseDomain.Original, s.rawResponse)
      .fold(error => fail(error.message), identity)
    val forward = frozen.newWorker().evaluate(response,
      OutputRequest.TrialQueries(Vector(q), NormalizationRule.Unnormalised))
      .fold(error => fail(error.message), identity).queries.head.value
    assertEqualsDouble(forward,
      originalTranspose.values.indices.map(i => originalTranspose.values(i) * s.rawResponse(i)).sum, 1e-8)
    val reversedAxis = ProfileTrialAxis.make(s.expanded, s.preparation,
      s.axis.trialIds.reverse, s.axis.conditionIds, s.axis.conditionForTrial,
      s.axis.nuisanceColumnIds, s.axis.selectedResponseRows)
      .fold(error => fail(error.message), identity)
    val foreign = ProfileTrialResponse.make(reversedAxis, reversedAxis.selectedResponseRows,
      ProfileTrialResponseDomain.Original, s.rawResponse)
      .fold(error => fail(error.message), identity)
    assertEquals(frozen.newWorker().evaluate(foreign,
      OutputRequest.TrialQueries(Vector(q), NormalizationRule.Unnormalised)),
      Left(ProfileTrialReadoutError.ForeignAxis))
