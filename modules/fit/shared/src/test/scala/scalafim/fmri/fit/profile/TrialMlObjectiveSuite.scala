package scalafim.fmri.fit.profile

import gale.linalg.{CholeskyOptions, DMat}
import scalafim.fmri.hrf.family.ShapeChart

class TrialMlObjectiveSuite extends munit.FunSuite:
  private def get[A](value: Either[CriterionJet.Error, A]): A = value.fold(e => fail(e.message), identity)
  private def success[A](value: Either[TrialMlFailure, A]): A = value.fold(e => fail(e.message), identity)

  /** Independent analytic scalar functions; no native TrialBanded/Gale factor
    * identity, Gaussian/Cascade derivatives or scientific admission is claimed.
    */
  private final class Analytic(val dimension: Int = 1, val mode: String = "quadratic") extends CoherentTrialMlBackend:
    val grid = if dimension == 1 then NodeGrid(ShapeChart(("x", -1.0, 1.0)), Vector(if mode == "fallback" || mode == "reject" then 3 else 5))
      else NodeGrid(ShapeChart(("x", -1.0, 1.0), ("y", -1.0, 1.0)), Vector(5, 5))
    val amplitudeCount = 1
    val owner = get(CriterionOwner.checked(1.7))
    val setupReceipt = TrialMlWork(referenceAttempts = grid.count)
    private var total = TrialMlWork()
    private var sequence = 0L
    private var epoch = get(CriterionEpoch.checked(owner, 1L))
    private var shift = 0.0
    var pointCalls = 0
    var valueNodes = 0
    var fullNodes = 0
    var values = 0
    var fulls = 0
    var refuseFull = false
    var refuseValues = false
    var staleEpoch = false
    var wrongOwner = false
    var wrongCoordinates = false
    var inconsistent = false
    var lastFullCoordinates = Vector.empty[Double]
    def workerWorkSnapshot: TrialMlWork = total
    private def attempt[A](result: Either[TrialMlFailure, A]): TrialMlAttempt[A] =
      val work = TrialMlWork(referenceAttempts = 1L, failures = if result.isLeft then 1L else 0L)
      total = total + work
      TrialMlAttempt(result, work)
    def pointAt(response: Array[Double]): TrialMlAttempt[CriterionEpoch] =
      pointCalls += 1
      sequence += 1L
      shift = response.headOption.getOrElse(0.0)
      epoch = get(CriterionEpoch.checked(owner, sequence))
      attempt(Right(epoch))
    def e(x: Vector[Double]): Double = math.pow(x(0) + 0.4 - shift, 2) +
      (if dimension == 2 then 2.0 * math.pow(x(1) + 0.2, 2) else 0.0)
    def j(x: Vector[Double]): Double = mode match
      case "fallback" => 5.0 + math.pow(x(0) * x(0) - 0.36, 2) + 0.1 * x(0)
      case "reject" => 5.0 + math.exp(10.0 * (x(0) - 0.2)) - 10.0 * (x(0) - 0.2)
      case _ => e(x) + 2.0 * (math.pow(x(0) - 0.6, 2) + (if dimension == 2 then 0.5 * math.pow(x(1) - 0.8, 2) else 0.0))
    def raw(x: Vector[Double]): ProfileJet = ProfileJet(e(x),
      if dimension == 1 then Vector(2.0 * (x(0) + 0.4 - shift)) else Vector(2.0 * (x(0) + 0.4 - shift), 4.0 * (x(1) + 0.2)),
      if dimension == 1 then Vector(2.0) else Vector(2.0, 0.0, 0.0, 4.0),
      Vector(3.0 + shift + x(0) - (if dimension == 2 then 2.0 * x(1) else 0.0)), CurvatureStatus.PositiveDefinite)
    private def jGradient(x: Vector[Double]): Vector[Double] = mode match
      case "fallback" => Vector(4.0 * x(0) * (x(0) * x(0) - 0.36) + 0.1)
      case "reject" => Vector(10.0 * math.exp(10.0 * (x(0) - 0.2)) - 10.0)
      case _ => raw(x).gradient.updated(0, raw(x).gradient(0) + 4.0 * (x(0) - 0.6))
        .zipWithIndex.map((g, i) => if i == 1 then g + 2.0 * (x(1) - 0.8) else g)
    private def jHessian(x: Vector[Double]): Vector[Double] = mode match
      case "fallback" => Vector(12.0 * x(0) * x(0) - 1.44)
      case "reject" => Vector(100.0 * math.exp(10.0 * (x(0) - 0.2)))
      case _ => if dimension == 1 then Vector(6.0) else Vector(6.0, 0.0, 0.0, 6.0)
    private def ref(x: Vector[Double], order: CriterionDerivativeOrder): CriterionReference =
      val boundOwner = if wrongOwner then get(CriterionOwner.checked(1.7)) else owner
      get(CriterionReference.checked(boundOwner, if wrongCoordinates then x.updated(0, x(0) + 0.1) else x, order))
    private def boundEpoch(reference: CriterionReference): CriterionEpoch =
      if wrongOwner then get(CriterionEpoch.checked(reference.owner, sequence))
      else if staleEpoch then get(CriterionEpoch.checked(owner, sequence)) else epoch
    private def full(x: Vector[Double]): TrialMlAttempt[CoherentCriterionJet] =
      lastFullCoordinates = x
      if refuseFull then attempt(Left(TrialMlFailure.Backend("full jet refused")))
      else
        val r = ref(x, CriterionDerivativeOrder.Full)
        val original = raw(x)
        val p = if inconsistent then original.copy(energy = original.energy + 1.0) else original
        val energy = get(RawEnergyJet.checked(r, boundEpoch(r), p, 1))
        val det = get(DeterminantJet.checked(r, (j(x) - e(x)) / 2.0,
          jGradient(x).zip(original.gradient).map((a, b) => (a - b) / 2.0),
          jHessian(x).zip(original.hessian).map((a, b) => (a - b) / 2.0)))
        attempt(Right(get(CoherentCriterionJet.checked(energy, det))))
    private def value(x: Vector[Double]): TrialMlAttempt[CoherentCriterionValue] =
      if refuseValues then attempt(Left(TrialMlFailure.Backend("value refused")))
      else
        val r = ref(x, CriterionDerivativeOrder.Value)
        val energy = get(RawEnergyValue.checked(r, boundEpoch(r), e(x), raw(x).amplitudes, 1))
        val det = get(DeterminantValue.checked(r, (j(x) - e(x)) / 2.0))
        attempt(Right(get(CoherentCriterionValue.checked(energy, det))))
    def valueAtNode(node: Int): TrialMlAttempt[CoherentCriterionValue] =
      valueNodes += 1
      value(grid.point(node).coordinates)
    def jetAtNode(node: Int): TrialMlAttempt[CoherentCriterionJet] =
      fullNodes += 1
      full(grid.point(node).coordinates)
    def valueAt(x: Vector[Double]): TrialMlAttempt[CoherentCriterionValue] =
      values += 1
      value(x)
    def jetAt(x: Vector[Double]): TrialMlAttempt[CoherentCriterionJet] =
      fulls += 1
      full(x)

  private def decoder(b: Analytic, budget: DecodeBudget, prior: Option[ShapePrior] = None): TrialMlDecoder =
    success(TrialMlDecoder.checked(Some(b), 2.0, budget, prior))
  private def evidence(result: TrialMlDecodeResult): CriterionJet = result.terminalEvidence match
    case TerminalEvidence.Available(jet) => jet
    case TerminalEvidence.Unavailable => fail("terminal jet unavailable")
  private val fullBudget = DecodeBudget(coarseStride = 1, maxNewtonSteps = 2, maxJets = 4)
  private val energyBudget = DecodeBudget(coarseStride = 1, maxNewtonSteps = 2, maxJets = 2)

  test("all four entry points supply coherent J, including off-node values"):
    val backend = new Analytic(2)
    val objective = new TrialMlObjective(backend, 2.0, 4)
    success(objective.pointAt(Array(0.0)))
    val out = new ProfileJetBuffer(2, 1)
    val node = 13
    val x = backend.grid.point(node).coordinates
    assertEqualsDouble(objective.scoreNode(node), backend.j(x), 1e-14)
    assert(objective.jetAtNode(node, out))
    assertEqualsDouble(out.energy, backend.j(x), 1e-14)
    assertEquals(out.hessian.toVector, Vector(6.0, 0.0, 0.0, 6.0))
    val off = Vector(0.23, -0.37)
    assert(objective.jetAt(off.toArray, out))
    assertEqualsDouble(out.energy, backend.j(off), 1e-14)
    val fullValue = out.energy
    assertEqualsDouble(objective.energyAt(off.toArray, out), fullValue, 1e-14)
    assert(out.gradient.forall(_.isNaN) && out.hessian.forall(_.isNaN))
    assertEquals(out.amplitudes.toVector, backend.raw(off).amplitudes)
    assertEquals(objective.retainedJetCount, 2)

  test("unchanged decoder chooses J node rankings and the independent 2D off-node optimum"):
    val b = new Analytic(2)
    val counters = new DecoderCounters
    val attempt = decoder(b, fullBudget).decode(Array(0.0), counters)
    val result = success(attempt.result)
    assertEquals(result.decoded.status, DecodeStatus.Accepted)
    assertEqualsDouble(result.decoded.coordinates(0), 4.0 / 15.0, 1e-13)
    assertEqualsDouble(result.decoded.coordinates(1), 2.0 / 15.0, 1e-13)
    assertEquals(result.decoded.node, 13) // x=.5,y=0; E ranks x=-.5,y=0 instead
    for n <- 0 until b.grid.count do assertEqualsDouble(result.nodeEnergies(n), b.j(b.grid.point(n).coordinates), 1e-13)
    val coords = b.grid.point(13).coordinates
    val competitors = (0 until b.grid.count).filter { n =>
      val q = b.grid.point(n).coordinates
      math.abs(q(0) - coords(0)) > 0.5 || math.abs(q(1) - coords(1)) > 0.5
    }
    val gap = competitors.map(n => b.j(b.grid.point(n).coordinates)).min - b.j(coords)
    assertEqualsDouble(result.decoded.ambiguityGap, gap, 1e-13)
    assertEquals(counters.jets, 2L)
    assertEquals(counters.exactEvaluations, 0L)
    assertEquals(b.fullNodes, 1)
    assertEquals(b.fulls, 1)
    assertEquals(result.retainedJetCount, 2)
    assertEquals(attempt.work.referenceAttempts, 28L) // point + 25 nodes + 2 jets
    assertEquals(attempt.work.solveAttempts, 0L)
    assertEquals(result.setupReceipt.referenceAttempts, 25L)

  test("energy-only candidate uses J and the reserved terminal jet without extra evaluations"):
    val b = new Analytic()
    val counters = new DecoderCounters
    val result = success(decoder(b, energyBudget).decode(Array(0.0), counters).result)
    assertEqualsDouble(result.decoded.coordinates.head, 4.0 / 15.0, 1e-13)
    assertEquals(result.decoded.status, DecodeStatus.Accepted)
    assertEquals(counters.exactEvaluations, 1L)
    assertEquals(counters.terminalVerifications, 1L)
    assertEquals(counters.jets, 2L)
    assertEquals(b.values, 1)
    assertEquals(b.fulls, 1)
    assertEquals(result.retainedJetCount, 2)
    assertEqualsDouble(evidence(result).raw.jet.energy, math.pow(4.0 / 15.0 + 0.4, 2), 1e-13)

  test("analytic indefinite HJ with positive HE takes parabolic fallback and terminal verification"):
    val b = new Analytic(mode = "fallback")
    val counters = new DecoderCounters
    val result = success(decoder(b, energyBudget).decode(Array(0.0), counters).result)
    val expected = -0.1 / 0.56
    assertEqualsDouble(result.decoded.coordinates.head, expected, 1e-13)
    assertEquals(result.decoded.status, DecodeStatus.CurvatureNotPositive)
    assertEquals(counters.fallbacks, 1L)
    assertEquals(counters.terminalVerifications, 1L)
    assertEquals(counters.exactEvaluations, 1L)
    val jet = evidence(result)
    assertEquals(jet.raw.jet.curvature, CurvatureStatus.PositiveDefinite)
    assertEquals(jet.minimizationJet.curvature, CurvatureStatus.Indefinite)
    assertEqualsDouble(jet.raw.jet.hessian.head, 2.0, 1e-15)
    assertEqualsDouble(jet.minimizationJet.hessian.head, 12.0 * expected * expected - 1.44, 1e-13)
    assert(result.decoded.conditionalSd.forall(_.isNaN))

  test("actual E-only substitution of each method is killed by its separate path witness"):
    for method <- Vector("scoreNode", "jetAtNode", "jetAt", "energyAt") do
      val b = new Analytic(mode = if method == "jetAtNode" then "fallback" else if method == "energyAt" then "reject" else "quadratic")
      val objective = new TrialMlObjective(b, 2.0, 4)
      success(objective.pointAt(Array(0.0)))
      def replace(x: Vector[Double], out: ProfileJetBuffer): Unit =
        val raw = b.raw(x)
        out.energy = raw.energy
        raw.gradient.copyToArray(out.gradient)
        raw.hessian.copyToArray(out.hessian)
        raw.amplitudes.copyToArray(out.amplitudes)
        out.curvature = raw.curvature
      val mutant = new ShapeObjective:
        def grid: NodeGrid = b.grid
        def amplitudeCount: Int = 1
        def scoreNode(n: Int): Double =
          val j = objective.scoreNode(n)
          if method == "scoreNode" then b.e(grid.point(n).coordinates) else j
        def jetAtNode(n: Int, out: ProfileJetBuffer): Boolean =
          val ok = objective.jetAtNode(n, out)
          if ok && method == "jetAtNode" then replace(grid.point(n).coordinates, out)
          ok
        def jetAt(x: Array[Double], out: ProfileJetBuffer): Boolean =
          val ok = objective.jetAt(x, out)
          if ok && method == "jetAt" then replace(x.toVector, out)
          ok
        def energyAt(x: Array[Double], out: ProfileJetBuffer): Double =
          val j = objective.energyAt(x, out)
          if method == "energyAt" then
            out.energy = b.e(x.toVector)
            out.energy
          else j
      val budget = if method == "energyAt" then energyBudget.copy(maxCandidateAttempts = 1) else fullBudget
      val counters = new DecoderCounters
      val shapeDecoder = new ShapeDecoder(mutant, budget, None, 2.0)
      val result = shapeDecoder.decode(counters)
      val witness = method match
        case "scoreNode" => result.node == 3 && math.abs(shapeDecoder.lastNodeEnergies(3) - b.j(Vector(0.5))) < 1e-13
        case "jetAtNode" => counters.fallbacks == 1L && math.abs(result.coordinates.head + 0.1 / 0.56) < 1e-13
        case "jetAt" => result.status == DecodeStatus.Accepted && result.dataHessian == Vector(6.0)
        case _ => counters.exactEvaluations == 1L && math.abs(result.coordinates.head) < 1e-13
      assert(!witness, s"E-only $method mutant escaped its witness")

  test("a rejected last jet cannot overwrite returned raw evidence"):
    val b = new Analytic(mode = "reject")
    val counters = new DecoderCounters
    val budget = fullBudget.copy(maxCandidateAttempts = 1, maxJets = 3)
    val result = success(decoder(b, budget).decode(Array(0.0), counters).result)
    assertEquals(result.decoded.status, DecodeStatus.BudgetExceeded)
    assertEqualsDouble(result.decoded.coordinates.head, 0.0, 1e-15)
    assert(b.lastFullCoordinates.head > 0.6)
    val terminal = evidence(result)
    assertEquals(terminal.raw.reference.coordinates, Vector(0.0))
    assertEqualsDouble(terminal.raw.jet.energy, 0.16, 1e-14)
    assertEquals(terminal.raw.jet.amplitudes, Vector(3.0))
    assertEquals(result.retainedJetCount, 2)
    assertEquals(counters.jets, 2L)

  test("failed terminal verification is unavailable and next response clears the ledger"):
    val b = new Analytic()
    val facade = decoder(b, energyBudget)
    val first = success(facade.decode(Array(0.0), new DecoderCounters).result)
    assertEquals(first.retainedJetCount, 2)
    b.refuseFull = true // includes node full refusal, hence no admissible terminal
    val refused = success(facade.decode(Array(0.4), new DecoderCounters).result)
    assertEquals(refused.terminalEvidence, TerminalEvidence.Unavailable)
    assertEquals(refused.decoded.status, DecodeStatus.NoAdmissibleNode)
    assertEquals(refused.retainedJetCount, 0)
    b.refuseFull = false
    val third = success(facade.decode(Array(0.3), new DecoderCounters).result)
    assertEqualsDouble(third.decoded.coordinates.head, (0.8 + 0.3) / 3.0, 1e-13)
    assert(evidence(third).raw.epoch.sequence > evidence(first).raw.epoch.sequence)
    assertEquals(third.retainedJetCount, 2)

  test("accepted energy-only terminal refusal preserves status and has no synthesized evidence"):
    val b = new Analytic()
    val forwarding = new CoherentTrialMlBackend:
      def grid: NodeGrid = b.grid
      def amplitudeCount: Int = b.amplitudeCount
      def owner: CriterionOwner = b.owner
      def setupReceipt: TrialMlWork = b.setupReceipt
      def workerWorkSnapshot: TrialMlWork = b.workerWorkSnapshot
      def pointAt(r: Array[Double]): TrialMlAttempt[CriterionEpoch] = b.pointAt(r)
      def valueAtNode(n: Int): TrialMlAttempt[CoherentCriterionValue] = b.valueAtNode(n)
      def jetAtNode(n: Int): TrialMlAttempt[CoherentCriterionJet] = b.jetAtNode(n)
      def valueAt(x: Vector[Double]): TrialMlAttempt[CoherentCriterionValue] = b.valueAt(x)
      def jetAt(x: Vector[Double]): TrialMlAttempt[CoherentCriterionJet] =
        b.refuseFull = true
        b.jetAt(x)
    val facade = success(TrialMlDecoder.checked(Some(forwarding), 2.0, energyBudget, None))
    val counters = new DecoderCounters
    val attempt = facade.decode(Array(0.0), counters)
    val result = success(attempt.result)
    assertEquals(result.decoded.status, DecodeStatus.BudgetExceeded)
    assertEquals(result.decoded.budgetExit, Some(DecodeBudgetExit.AcceptedEnergyOnlyTerminalVerificationFailed))
    assertEquals(result.terminalEvidence, TerminalEvidence.Unavailable)
    assertEquals(result.retainedJetCount, 1)
    assert(result.decoded.dataHessian.forall(_.isNaN))
    assertEquals(counters.jets, 2L)
    assertEquals(b.fulls, 1)
    assertEquals(attempt.work.failures, 1L)
    assertEquals(result.lastRefusal.get.work.failures, 1L)

  test("strong prior is applied once; raw HE, HJ, augmented H and conditional SD remain distinct"):
    val b = new Analytic(2)
    val prior = ShapePrior(Vector(-0.2, 0.3), Vector(12.0, 2.0, 2.0, 8.0))
    val result = success(decoder(b, fullBudget, Some(prior)).decode(Array(0.0), new DecoderCounters).result)
    val jet = evidence(result)
    assertEquals(jet.raw.jet.hessian, Vector(2.0, 0.0, 0.0, 4.0))
    assertEquals(jet.minimizationJet.hessian, Vector(6.0, 0.0, 0.0, 6.0))
    assertEquals(result.decoded.dataHessian, jet.minimizationJet.hessian)
    assertEquals(result.decoded.augmentedHessian, Vector(30.0, 4.0, 4.0, 22.0))
    val hj = DMat.tabulate(2, 2)((i, j) => if i == j then 6.0 else 0.0)
    val inv = hj.cholesky(CholeskyOptions(0.0)).toOption.get.solve(DMat.eye(2)).toOption.get
    for i <- 0 until 2 do assertEqualsDouble(result.decoded.conditionalSd(i), math.sqrt(4.0 * inv(i, i)), 1e-13)
    assert(math.abs(result.decoded.conditionalSd.head - math.sqrt(4.0 * 22.0 / 644.0)) > 0.4)
    // Independently solve augmented stationarity H theta = (1.6,.8) + 2P mean.
    val expectedX = (-2.0 * 22.0 - 4.0 * 4.8) / 644.0
    val expectedY = (30.0 * 4.8 + 4.0 * 2.0) / 644.0
    assertEqualsDouble(result.decoded.coordinates(0), expectedX, 1e-13)
    assertEqualsDouble(result.decoded.coordinates(1), expectedY, 1e-13)
    assertEqualsDouble(result.decoded.energy, b.j(result.decoded.coordinates), 1e-13)

  test("missing capability, invalid variance and invalid prior refuse before response work"):
    assertEquals(TrialMlDecoder.checked(None, 2.0, fullBudget, None), Left(TrialMlFailure.MissingCapability))
    val b = new Analytic(2)
    for bad <- Vector(0.0, Double.NaN, Double.PositiveInfinity) do assert(TrialMlDecoder.checked(Some(b), bad, fullBudget, None).isLeft)
    for prior <- Vector(ShapePrior(Vector(0.0), Vector(1.0)),
      ShapePrior(Vector(Double.NaN, 0.0), Vector(1.0, 0.0, 0.0, 1.0)),
      ShapePrior(Vector(0.0, 0.0), Vector(1.0, 1e-15, 0.0, 1.0)),
      ShapePrior(Vector(0.0, 0.0), Vector(-1e-15, 0.0, 0.0, 1.0))) do
      assert(TrialMlDecoder.checked(Some(b), 2.0, fullBudget, Some(prior)).isLeft)
    assert(TrialMlDecoder.checked(Some(b), 2.0, fullBudget, Some(ShapePrior(Vector(0.0, 0.0), Vector.fill(4)(0.0)))).isRight)
    assertEquals(b.pointCalls, 0)

  test("binding refusals clear every destination, ledger repeats refuse, work stays bounded"):
    val b = new Analytic()
    val objective = new TrialMlObjective(b, 2.0, 2)
    success(objective.pointAt(Array(0.0)))
    val out = new ProfileJetBuffer(1, 1)
    assert(objective.jetAt(Array(0.23), out))
    b.staleEpoch = true
    assert(!objective.jetAt(Array(0.23), out))
    assert(out.energy.isPosInfinity && out.gradient.forall(_.isNaN) && out.hessian.forall(_.isNaN) && out.amplitudes.forall(_.isNaN))
    assertEquals(objective.lastRefusal.get.error, TrialMlFailure.Criterion(CriterionJet.Error.WrongEpoch))
    assert(objective.energyAt(Array(0.23), out).isPosInfinity)
    b.staleEpoch = false
    b.wrongOwner = true
    assert(!objective.jetAt(Array(0.23), out))
    assertEquals(objective.lastRefusal.get.error, TrialMlFailure.Criterion(CriterionJet.Error.WrongOwner))
    b.wrongOwner = false
    b.wrongCoordinates = true
    assert(!objective.jetAt(Array(0.23), out))
    assertEquals(objective.lastRefusal.get.error, TrialMlFailure.Criterion(CriterionJet.Error.WrongCoordinates))
    b.wrongCoordinates = false
    b.inconsistent = true
    assert(!objective.jetAt(Array(0.23), out))
    assertEquals(objective.coherenceFailure, Some(TrialMlFailure.IncoherentRepeatedJet(Vector(0.23))))
    assertEquals(objective.retainedJetCount, 1)
    success(objective.pointAt(Array(0.4)))
    b.inconsistent = false
    assert(objective.jetAt(Array(0.1), out))
    assert(objective.jetAt(Array(0.2), out))
    assert(!objective.jetAt(Array(0.3), out))
    assertEquals(objective.coherenceFailure, Some(TrialMlFailure.LedgerLimit))
    assertEquals(objective.retainedJetCount, 2)
    assertEquals(objective.attemptWork.solveAttempts, 0L)

  test("prior curvature cannot rescue indefinite prior-free HJ for conditional SD"):
    val b = new Analytic(mode = "fallback")
    val prior = ShapePrior(Vector(0.1 / 24.0), Vector(12.0))
    val result = success(decoder(b, fullBudget, Some(prior)).decode(Array(0.0), new DecoderCounters).result)
    assertEqualsDouble(result.decoded.coordinates.head, 0.0, 1e-13)
    assertEqualsDouble(result.decoded.dataHessian.head, -1.44, 1e-13)
    assertEqualsDouble(result.decoded.augmentedHessian.head, 22.56, 1e-13)
    assertEquals(result.decoded.status, DecodeStatus.WeaklyIdentified)
    assert(result.decoded.conditionalSd.forall(_.isNaN))
    assertEquals(evidence(result).minimizationJet.curvature, CurvatureStatus.Indefinite)

  test("value refusals yield no admissible node; malformed destinations perform no backend work"):
    val b = new Analytic()
    b.refuseValues = true
    val counters = new DecoderCounters
    val attempt = decoder(b, fullBudget).decode(Array(0.0), counters)
    val result = success(attempt.result)
    assertEquals(result.decoded.status, DecodeStatus.NoAdmissibleNode)
    assertEquals(result.terminalEvidence, TerminalEvidence.Unavailable)
    assertEquals(result.retainedJetCount, 0)
    assertEquals(attempt.work.failures, 5L)
    assertEquals(b.fullNodes, 0)
    b.refuseValues = false
    val objective = new TrialMlObjective(b, 2.0, 2)
    success(objective.pointAt(Array(0.0)))
    val bad = new ProfileJetBuffer(2, 1)
    assert(!objective.jetAt(Array(0.0), bad))
    assert(bad.amplitudes.forall(_.isNaN))
    val out = new ProfileJetBuffer(1, 1)
    assert(!objective.jetAt(Array(0.0, 1.0), out))
    assert(objective.energyAt(Array(Double.NaN), out).isPosInfinity)
    assertEquals(b.fulls, 0)
    assertEquals(b.values, 0)
