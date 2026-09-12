package scalafim.fmri.laws.profile

import gale.linalg.DMat
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan, WhiteningTransform}
import scalafim.fmri.design.hrf.{ExpandedTrialDesign, HrfKernelBasis, KernelBasisSpec, TrialMembership}
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, ShapePoint}

/** Independent augmented-system and dense-covariance laws for PHRF-07. */
class TrialBandedSuite extends munit.FunSuite:

  private val family = GaussianFamily.Default
  private val step = PositiveSeconds(0.2).fold(error => fail(error.message), identity)
  private lazy val basis = HrfKernelBasis
    .compile(KernelBasisSpec(family, step, Vector(26, 21), tolerance = 1e-4, maxRank = 40))
    .fold(error => fail(error.message), identity)

  private final case class Fixture(
      expanded: ExpandedTrialDesign,
      nuisance: DMat,
      response: Array[Double],
      point: ShapePoint,
      lambda: Double,
      whitening: Option[WhiteningPlan] = None)

  private def fixture(lambda: Double = 2.5, coincident: Boolean = false): Fixture =
    val rows = 120
    val frame = SamplingFrame(blockLens = Seq(60, 60), tr = Seq(1.0, 1.0))
    val onsets =
      if coincident then Vector(3.0, 3.0, 12.0, 18.0, 18.0, 31.0).map(Seconds(_))
      else Vector(3.0, 10.0, 18.0, 5.0, 14.0, 31.0).map(Seconds(_))
    val blocks = Vector(0, 0, 0, 1, 1, 1)
    val membership = TrialMembership.make(Vector(0, 1, 1, 1, 2, 2), 3).fold(error => fail(error.message), identity)
    val expanded = ExpandedTrialDesign
      .lower(onsets, blocks, Vector.fill(onsets.length)(Seconds(0.0)), membership, frame, basis, Seconds(0.2))
      .fold(error => fail(error.message), identity)
    val nuisance = DMat.tabulate(rows, 2): (t, j) =>
      val local = if t < 60 then t else t - 60
      if j == 0 then 1.0 else (local - 29.5) / 29.5
    val point = ShapePoint.unsafe(Vector(5.3, math.log(1.55)))
    val x = designAt(expanded, point)
    val conditionMeans = Array(-1.2, 0.7, 1.5)
    val deviations = Array(0.0, -0.3, 0.1, 0.2, -0.4, 0.4)
    val response = new Array[Double](rows)
    var t = 0
    while t < rows do
      var value = 0.8 * nuisance(t, 0) - 0.35 * nuisance(t, 1)
      var trial = 0
      while trial < expanded.trials do
        val amplitude = conditionMeans(membership.conditionOfTrial(trial)) + deviations(trial)
        value += x(t * expanded.trials + trial) * amplitude
        trial += 1
      value += 0.03 * math.sin(0.37 * t) - 0.02 * math.cos(0.11 * t)
      response(t) = value
      t += 1
    Fixture(expanded, nuisance, response, point, lambda)

  test("packed cross-basis blocks reconstruct the dense Gram and preserve run boundaries"):
    val fx = fixture()
    val prep = TrialBandedPreparation.prepare(fx.expanded, None, Some(fx.nuisance), fx.lambda).fold(error => fail(error.message), identity)
    assertEquals(prep.gramBlockCount, basis.rank * (basis.rank + 1) / 2)
    assert(prep.bandwidth < fx.expanded.trials - 1, s"unexpected full bandwidth ${prep.bandwidth}")
    val data = fx.expanded.term.data.data
    val n = fx.expanded.trials
    val m = fx.expanded.rank
    val rows = fx.expanded.rows
    var q = 0
    while q < m do
      var p = 0
      while p <= q do
        val block = prep.gramBlock(p, q)
        var i = 0
        while i < n do
          var j = 0
          while j <= i do
            var expected = 0.0
            var t = 0
            while t < rows do
              val row = t * n * m
              if p == q then expected += data(row + p * n + i) * data(row + p * n + j)
              else
                expected += data(row + p * n + i) * data(row + q * n + j) +
                  data(row + q * n + i) * data(row + p * n + j)
              t += 1
            if i - j <= prep.bandwidth then assertEqualsDouble(block(i, i - j), expected, 2e-11)
            else assertEqualsDouble(expected, 0.0, 2e-12)
            if i >= 3 && j < 3 then assertEqualsDouble(expected, 0.0, 2e-12, s"cross-run pair ($i,$j)")
            j += 1
          i += 1
        p += 1
      q += 1

  test("banded ridge-and-release energy, means, jets and ML determinant match independent dense equations"):
    val fx = fixture()
    val prep = TrialBandedPreparation.prepare(fx.expanded, None, Some(fx.nuisance), fx.lambda).fold(error => fail(error.message), identity)
    val grid = NodeGrid(family.chart, Vector(3, 3))
    val objective = prep.objective(grid).fold(error => fail(error.message), identity)
    val encoded = prep.encodeWhitened(fx.response).fold(error => fail(error.message), identity)
    objective.pointAt(encoded)
    val out = new ProfileJetBuffer(family.dimension, fx.expanded.membership.conditionCount)
    assert(objective.jetAt(fx.point.coordinates.toArray, out))
    val direct = directFit(fx, fx.point)
    assertEqualsDouble(out.energy, direct._1, 2e-8)
    direct._2.indices.foreach(i => assertEqualsDouble(out.amplitudes(i), direct._2(i), 2e-8, s"condition $i"))

    val h = 2e-4
    val x0 = fx.point.coordinates.toArray
    var p = 0
    while p < family.dimension do
      val plus = x0.clone()
      val minus = x0.clone()
      plus(p) += h
      minus(p) -= h
      val gradient = (directFit(fx, ShapePoint.unsafe(plus.toVector))._1 - directFit(fx, ShapePoint.unsafe(minus.toVector))._1) / (2.0 * h)
      assertEqualsDouble(out.gradient(p), gradient, 2e-5 * math.max(1.0, math.abs(gradient)), s"gradient $p")
      val diagonal = (directFit(fx, ShapePoint.unsafe(plus.toVector))._1 - 2.0 * direct._1 + directFit(fx, ShapePoint.unsafe(minus.toVector))._1) / (h * h)
      assertEqualsDouble(out.hessian(p * family.dimension + p), diagonal, 2e-3 * math.max(1.0, math.abs(diagonal)), s"hessian $p,$p")
      var q = p + 1
      while q < family.dimension do
        val pp = x0.clone()
        val pm = x0.clone()
        val mp = x0.clone()
        val mm = x0.clone()
        pp(p) += h; pp(q) += h
        pm(p) += h; pm(q) -= h
        mp(p) -= h; mp(q) += h
        mm(p) -= h; mm(q) -= h
        val mixed = (directFit(fx, ShapePoint.unsafe(pp.toVector))._1 - directFit(fx, ShapePoint.unsafe(pm.toVector))._1 -
          directFit(fx, ShapePoint.unsafe(mp.toVector))._1 + directFit(fx, ShapePoint.unsafe(mm.toVector))._1) / (4.0 * h * h)
        assertEqualsDouble(out.hessian(p * family.dimension + q), mixed, 3e-3 * math.max(1.0, math.abs(mixed)), s"hessian $p,$q")
        q += 1
      p += 1

    val denseLogDet = constrainedCovarianceLogDet(fx, fx.point)
    val bandedLogDet = objective.logDetAt(x0).fold(error => fail(error.message), identity)
    assertEqualsDouble(bandedLogDet, denseLogDet, 3e-9)

  test("shared AR whitening matches independently whitened augmented equations across run resets"):
    val raw = fixture()
    val plan = WhiteningPlan.global(
      ArmaCoefficients.ar(0.37),
      Vector(TimeSegment(0, 60, 0), TimeSegment(60, 120, 1)),
      exactFirstAr1 = true
    )
    val fx = raw.copy(whitening = Some(plan))
    val prep = TrialBandedPreparation.prepare(fx.expanded, Some(plan), Some(fx.nuisance), fx.lambda)
      .fold(error => fail(error.message), identity)
    val whitenedResponse = prep.whitenResponses(1, fx.response).fold(error => fail(error.message), identity)
    val objective = prep.objective(NodeGrid(family.chart, Vector(3, 3))).fold(error => fail(error.message), identity)
    objective.pointAt(prep.encodeWhitened(whitenedResponse).fold(error => fail(error.message), identity))
    val out = new ProfileJetBuffer(family.dimension, fx.expanded.membership.conditionCount)
    assert(objective.jetAt(fx.point.coordinates.toArray, out))
    val direct = directFit(fx, fx.point)
    assertEqualsDouble(out.energy, direct._1, 3e-8)
    direct._2.indices.foreach(i => assertEqualsDouble(out.amplitudes(i), direct._2(i), 3e-8, s"condition $i"))

  test("unequal singleton and coincident conditions remain exact across lambda extremes and trial permutations"):
    for lambda <- Vector(1e-6, 1e4) do
      val fx = fixture(lambda, coincident = true)
      val prep = TrialBandedPreparation.prepare(fx.expanded, None, Some(fx.nuisance), lambda).fold(error => fail(error.message), identity)
      val objective = prep.objective(NodeGrid(family.chart, Vector(2, 2))).fold(error => fail(error.message), identity)
      objective.pointAt(prep.encodeWhitened(fx.response).fold(error => fail(error.message), identity))
      val out = new ProfileJetBuffer(family.dimension, 3)
      val actual = objective.energyAt(fx.point.coordinates.toArray, out)
      val expected = directFit(fx, fx.point)
      val tol = if lambda < 1e-3 then 2e-5 else 2e-8
      assertEqualsDouble(actual, expected._1, tol * math.max(1.0, math.abs(expected._1)), s"lambda=$lambda")
      expected._2.indices.foreach(i => assertEqualsDouble(out.amplitudes(i), expected._2(i), 2e-5 * math.max(1.0, math.abs(expected._2(i))), s"lambda=$lambda condition=$i"))

    val original = fixture()
    val permutation = Vector(2, 0, 5, 1, 4, 3)
    val permMembership = TrialMembership
      .make(permutation.map(original.expanded.membership.conditionOfTrial), 3)
      .fold(error => fail(error.message), identity)
    val permuted = ExpandedTrialDesign
      .lower(
        permutation.map(i => Vector(3.0, 10.0, 18.0, 5.0, 14.0, 31.0)(i)).map(Seconds(_)),
        Vector.fill(6)(0),
        Vector.fill(6)(Seconds(0.0)),
        permMembership,
        SamplingFrame(blockLens = Seq(120), tr = Seq(1.0)),
        basis,
        Seconds(0.2)
      )
      .fold(error => fail(error.message), identity)
    val permFx = original.copy(expanded = permuted)
    val permPrep = TrialBandedPreparation.prepare(permuted, None, Some(original.nuisance), original.lambda).fold(error => fail(error.message), identity)
    val permObjective = permPrep.objective(NodeGrid(family.chart, Vector(2, 2))).fold(error => fail(error.message), identity)
    permObjective.pointAt(permPrep.encodeWhitened(original.response).fold(error => fail(error.message), identity))
    val permOut = new ProfileJetBuffer(family.dimension, 3)
    assert(permObjective.jetAt(original.point.coordinates.toArray, permOut))
    val permDirect = directFit(permFx, original.point)
    assertEqualsDouble(permOut.energy, permDirect._1, 2e-8)
    permDirect._2.indices.foreach(i => assertEqualsDouble(permOut.amplitudes(i), permDirect._2(i), 2e-8))

  test("rank aliases are refused and exact per-voxel readout factors are explicit and counted"):
    val fx = fixture()
    val x = designAt(fx.expanded, ShapePoint.unsafe(Vector(
      0.5 * (family.chart.lower(0) + family.chart.upper(0)),
      0.5 * (family.chart.lower(1) + family.chart.upper(1))
    )))
    val alias = DMat.tabulate(fx.expanded.rows, 1)((t, _) =>
      var sum = 0.0
      var trial = 0
      while trial < fx.expanded.trials do
        if fx.expanded.membership.conditionOfTrial(trial) == 0 then sum += x(t * fx.expanded.trials + trial)
        trial += 1
      sum
    )
    val aliasPrep = TrialBandedPreparation.prepare(fx.expanded, None, Some(alias), fx.lambda).fold(error => fail(error.message), identity)
    assert(aliasPrep.objective(NodeGrid(family.chart, Vector(3, 3))).isLeft)
    assert(TrialBandedPreparation.prepare(fx.expanded, None, Some(fx.nuisance), 0.0).isLeft)

    val prep = TrialBandedPreparation.prepare(fx.expanded, None, Some(fx.nuisance), fx.lambda).fold(error => fail(error.message), identity)
    val objective = prep.objective(NodeGrid(family.chart, Vector(2, 2))).fold(error => fail(error.message), identity)
    objective.pointAt(prep.encodeWhitened(fx.response).fold(error => fail(error.message), identity))
    val rhs = Array.tabulate(fx.expanded.trials)(i => math.sin(0.7 * i) - 0.2 * i)
    val solved = objective
      .solveReadoutSystem(TrialReadoutFactorMode.ExactShape(fx.point.coordinates), rhs)
      .fold(error => fail(error.message), identity)
    val dense = denseTrialSystem(fx, fx.point).cholesky.fold(throw _, identity)
    val rhsMatrix = DMat.tabulate(rhs.length, 1)((i, _) => rhs(i))
    val expected = dense.solve(rhsMatrix).fold(throw _, identity)
    solved.indices.foreach(i => assertEqualsDouble(solved(i), expected(i, 0), 2e-9))
    assertEquals(objective.work.snapshot.exactReadoutFactors, 1L)
    val readout = objective.readout(TrialReadoutFactorMode.ExactShape(fx.point.coordinates)).fold(error => fail(error.message), identity)
    val direct = directFit(fx, fx.point)
    assertEqualsDouble(readout.penalizedEnergy, direct._1, 2e-8)
    direct._2.indices.foreach(i => assertEqualsDouble(readout.conditionMeans(i), direct._2(i), 2e-8))
    direct._3.indices.foreach(i => assertEqualsDouble(readout.trialAmplitudes(i), direct._3(i), 2e-8))
    assertEquals(objective.work.snapshot.exactReadoutFactors, 2L)
    objective.solveReadoutSystem(TrialReadoutFactorMode.PreparedNode(0), rhs).fold(error => fail(error.message), identity)
    assertEquals(objective.work.snapshot.exactReadoutFactors, 2L)
    var node = 0
    while node < objective.grid.count do
      objective.scoreNode(node)
      node += 1
    val jet = new ProfileJetBuffer(family.dimension, 3)
    assert(objective.jetAtNode(0, jet))
    assert(objective.jetAtNode(1, jet))
    val trialOut = new Array[Double](fx.expanded.trials)
    objective.readoutInto(TrialReadoutFactorMode.PreparedNode(0), trialOut).fold(error => fail(error.message), identity)
    val work = objective.work.snapshot
    assertEquals(work.bankValueEvaluations, objective.grid.count.toLong)
    assertEquals(work.jetEvaluations, 2L)
    assertEquals(work.amplitudeCorrections, 2L)
    assertEquals(work.exactReadoutFactors, 2L)

  private def designAt(expanded: ExpandedTrialDesign, point: ShapePoint): Array[Double] =
    val coefficients = new Array[Double](expanded.rank)
    expanded.basis.coefficientsInto(point, new Array[Double](expanded.basis.fineCount), coefficients)
    val out = new Array[Double](expanded.rows * expanded.trials)
    val source = expanded.term.data.data
    var t = 0
    while t < expanded.rows do
      var trial = 0
      while trial < expanded.trials do
        var sum = 0.0
        var p = 0
        while p < expanded.rank do
          sum += source(t * expanded.columns + p * expanded.trials + trial) * coefficients(p)
          p += 1
        out(t * expanded.trials + trial) = sum
        trial += 1
      t += 1
    out

  private def denseTrialSystem(fx: Fixture, point: ShapePoint): DMat =
    val x = designAt(fx.expanded, point)
    val n = fx.expanded.trials
    DMat.tabulate(n, n): (i, j) =>
      var sum = if i == j then fx.lambda else 0.0
      var t = 0
      while t < fx.expanded.rows do
        sum += x(t * n + i) * x(t * n + j)
        t += 1
      sum

  /** Independent direct normal equations for
    * `min ||y-F gamma-X a||^2 + lambda ||a-M beta||^2`.
    */
  private def directFit(fx: Fixture, point: ShapePoint): (Double, Vector[Double], Vector[Double]) =
    val rows = fx.expanded.rows
    val n = fx.expanded.trials
    val f = fx.nuisance.cols
    val c = fx.expanded.membership.conditionCount
    val x = whitenColumns(fx.whitening, rows, n, designAt(fx.expanded, point))
    val nuisanceValues = new Array[Double](rows * f)
    fx.nuisance.copyRowMajorTo(nuisanceValues)
    val nuisance = whitenColumns(fx.whitening, rows, f, nuisanceValues)
    val response = whitenColumns(fx.whitening, rows, 1, fx.response)
    val size = n + f + c
    val normal = DMat.tabulate(size, size): (i, j) =>
      if i < n && j < n then
        var sum = if i == j then fx.lambda else 0.0
        var t = 0
        while t < rows do
          sum += x(t * n + i) * x(t * n + j)
          t += 1
        sum
      else if i < n && j < n + f then
        var sum = 0.0
        var t = 0
        while t < rows do
          sum += x(t * n + i) * nuisance(t * f + j - n)
          t += 1
        sum
      else if j < n && i < n + f then
        var sum = 0.0
        var t = 0
        while t < rows do
          sum += nuisance(t * f + i - n) * x(t * n + j)
          t += 1
        sum
      else if i < n && j >= n + f then
        if fx.expanded.membership.conditionOfTrial(i) == j - n - f then -fx.lambda else 0.0
      else if j < n && i >= n + f then
        if fx.expanded.membership.conditionOfTrial(j) == i - n - f then -fx.lambda else 0.0
      else if i < n + f && j < n + f then
        var sum = 0.0
        var t = 0
        while t < rows do
          sum += nuisance(t * f + i - n) * nuisance(t * f + j - n)
          t += 1
        sum
      else if i >= n + f && j >= n + f then
        if i == j then fx.lambda * fx.expanded.membership.trialsOf(i - n - f).length else 0.0
      else 0.0
    val rhs = DMat.tabulate(size, 1): (i, _) =>
      if i < n then
        var sum = 0.0
        var t = 0
        while t < rows do
          sum += x(t * n + i) * response(t)
          t += 1
        sum
      else if i < n + f then
        var sum = 0.0
        var t = 0
        while t < rows do
          sum += nuisance(t * f + i - n) * response(t)
          t += 1
        sum
      else 0.0
    val coefficients = normal.cholesky.fold(throw _, identity).solve(rhs).fold(throw _, identity)
    var energy = 0.0
    var t = 0
    while t < rows do
      var fitted = 0.0
      var trial = 0
      while trial < n do
        fitted += x(t * n + trial) * coefficients(trial, 0)
        trial += 1
      var nuisanceCol = 0
      while nuisanceCol < f do
        fitted += nuisance(t * f + nuisanceCol) * coefficients(n + nuisanceCol, 0)
        nuisanceCol += 1
      val residual = response(t) - fitted
      energy += residual * residual
      t += 1
    var trial = 0
    while trial < n do
      val beta = coefficients(n + f + fx.expanded.membership.conditionOfTrial(trial), 0)
      val deviation = coefficients(trial, 0) - beta
      energy += fx.lambda * deviation * deviation
      trial += 1
    (
      energy,
      Vector.tabulate(c)(condition => coefficients(n + f + condition, 0)),
      Vector.tabulate(n)(trial => coefficients(trial, 0))
    )

  private def whitenColumns(plan: Option[WhiteningPlan], rows: Int, cols: Int, values: Array[Double]): Array[Double] =
    plan match
      case None => java.util.Arrays.copyOf(values, values.length)
      case Some(value) =>
        val matrix = DMat.tabulate(rows, cols)((i, j) => values(i * cols + j))
        val out = new Array[Double](values.length)
        WhiteningTransform.matrix(value, matrix).fold(error => fail(error.toString), identity).copyRowMajorTo(out)
        out

  private def constrainedCovarianceLogDet(fx: Fixture, point: ShapePoint): Double =
    val x = designAt(fx.expanded, point)
    val rows = fx.expanded.rows
    val n = fx.expanded.trials
    val membership = fx.expanded.membership
    val covariance = DMat.tabulate(rows, rows): (i, j) =>
      var sum = if i == j then 1.0 else 0.0
      var a = 0
      while a < n do
        val condition = membership.conditionOfTrial(a)
        var b = 0
        while b < n do
          if membership.conditionOfTrial(b) == condition then
            val count = membership.trialsOf(condition).length.toDouble
            val p = (if a == b then 1.0 else 0.0) - 1.0 / count
            sum += x(i * n + a) * p * x(j * n + b) / fx.lambda
          b += 1
        a += 1
      sum
    val factor = covariance.cholesky.fold(throw _, identity)
    2.0 * (0 until rows).map(i => math.log(factor.lower(i, i))).sum
