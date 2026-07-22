package scalafim.multivar

import gale.linalg.DMat

class PalmConvergenceSuite extends munit.FunSuite:

  test("admission records theorem obligations and fails closed without KL or singular-geometry policy"):
    val fixture = biconvexFixture("admission")
    val coercive = PalmLevelSetWitness.coercive(
      fixture.problem.programIdentity,
      PositiveProofConstant.nullspaceCoercivity(0.2).toOption.get,
      assumption("bounded-level-set")
    )
    val noKl = PalmAdmission.from(
      fixture.problem,
      coercive,
      PalmSubproblemPolicy.Exact,
      PalmKlEvidence.NotClaimed,
      PalmConvergenceTarget.CriticalPoint
    )
    val singularRejected = PalmLevelSetWitness.compactNormalization(
      fixture.problem.programIdentity,
      id("singular-geometry"),
      radius = 1.0,
      ambientDimension = 2,
      effectiveRank = 1,
      PalmSingularGeometryPolicy.Reject,
      assumption("bounded-normalization")
    )
    val singularSupported = accepted(
      PalmLevelSetWitness.compactNormalization(
        fixture.problem.programIdentity,
        id("singular-geometry"),
        radius = 1.0,
        ambientDimension = 2,
        effectiveRank = 1,
        PalmSingularGeometryPolicy.RestrictToSupport(id("singular-support")),
        assumption("bounded-normalization")
      )
    )
    val admitted = accepted(
      PalmAdmission.from(
        fixture.problem,
        singularSupported,
        PalmSubproblemPolicy.Exact,
        fixture.kl,
        PalmConvergenceTarget.CriticalPoint
      )
    )

    assert(noKl.left.exists(_.message.contains("KL")))
    assert(singularRejected.left.exists(_.message.contains("support restriction")))
    assertEquals(admitted.problem.blocks.map(_.assumptions.parameter), Vector(fixture.x, fixture.y))
    assert(admitted.problem.blocks.forall(_.assumptions.partialGradientLipschitz.doubleValue > 0.0))
    assertEquals(admitted.levelSet.kind, singularSupported.kind)

  test("exact PALM enforces sufficient decrease and returns stationary semantic evidence"):
    val fixture = biconvexFixture("exact")
    val solver = PalmSolver.from(fixture.criticalAdmission(PalmSubproblemPolicy.Exact))
    val initialization = fixture.initialization("exact-start", 2.0, 0.5)
    val tolerance = accepted(CertificateTolerance.from(1e-9, 1e-8))
    val config = PalmConfig(
      IterationBudget.unsafe(500),
      tolerance,
      PalmDescentPolicy.SufficientDecrease(PalmDecreaseCoefficient.unsafe(0.05))
    )
    val fit = accepted(solver.solve(initialization, config))
    val history = fit.receipt.objectiveHistory

    assertEquals(fit.receipt.termination, PalmTermination.Converged)
    assertEquals(fit.achievement.claimClass, OptimizationClaimClass.Stationary)
    assert(history.sliding(2).forall:
      case Vector(before, after) => after <= before + tolerance.threshold(Math.abs(before))
      case _ => true
    )
    assert(fit.receipt.traces.forall(_.blocks.length == 2))
    assert(fit.receipt.traces.flatMap(_.blocks).forall(_.solveKind == PalmBlockSolveKind.Exact))
    assert(fit.receipt.finalStationarity.map(_._2).max <= tolerance.threshold(fit.objective))
    assertEqualsDouble(fit.receipt.finalNormalizationResidual, 0.0, 1e-14)
    assertEqualsDouble(fit.state.block(fixture.x).toOption.get.values(0, 0), Math.sqrt(0.8), 2e-6)
    assertEqualsDouble(fit.state.block(fixture.y).toOption.get.values(0, 0), Math.sqrt(0.8), 2e-6)

  test("coordinatewise convergence needs no KL claim and never becomes a global claim"):
    val fixture = biconvexFixture("coordinatewise")
    val admission = accepted(
      PalmAdmission.from(
        fixture.problem,
        fixture.coercive,
        PalmSubproblemPolicy.Exact,
        PalmKlEvidence.NotClaimed,
        PalmConvergenceTarget.CoordinatewiseStationary
      )
    )
    val fit = accepted(
      PalmSolver.from(admission).solve(
        fixture.initialization("coordinatewise-start", -2.0, -0.5),
        PalmConfig(IterationBudget.unsafe(500), CertificateTolerance.strict, PalmDescentPolicy.Monotone)
      )
    )

    assertEquals(fit.achievement.claimClass, OptimizationClaimClass.CoordinatewiseStationary)
    assert(!fit.achievement.claimClass.isGlobal)
    assertEquals(fit.receipt.klEvidence, PalmKlEvidence.NotClaimed)

  test("an exhausted budget returns unresolved with the complete final trace"):
    val fixture = biconvexFixture("iteration-limit")
    val fit = accepted(
      PalmSolver
        .from(fixture.criticalAdmission(PalmSubproblemPolicy.Exact))
        .solve(
          fixture.initialization("short-start", 4.0, 0.1),
          PalmConfig(
            IterationBudget.unsafe(1),
            accepted(CertificateTolerance.from(0.0, 0.0)),
            PalmDescentPolicy.Monotone
          )
        )
    )

    assertEquals(fit.receipt.termination, PalmTermination.IterationLimit)
    assertEquals(fit.achievement.claimClass, OptimizationClaimClass.Unresolved)
    assertEquals(fit.receipt.traces.length, 1)
    assertEquals(fit.receipt.traces.head.blocks.map(_.parameter), Vector(fixture.x, fixture.y))
    assert(fit.receipt.traces.head.stationarity.forall(_._2.isFinite))

  test("objective increase is a typed descent violation rather than a converged result"):
    val fixture = biconvexFixture("descent-failure", badFirstUpdate = true)
    val result = PalmSolver
      .from(fixture.criticalAdmission(PalmSubproblemPolicy.Exact))
      .solve(
        fixture.initialization("bad-start", 1.0, 1.0),
        PalmConfig(IterationBudget.unsafe(5), CertificateTolerance.strict, PalmDescentPolicy.Monotone)
      )

    assert(result.left.exists(_.isInstanceOf[PalmConvergenceError.DescentViolation]))

  test("summably inexact updates are bounded iteration by iteration"):
    val schedule = accepted(GeometricInexactnessSchedule.from(1e-4, 0.5))
    val policy = PalmSubproblemPolicy.SummablyInexact(schedule, assumption("summable-inexactness"))
    val fixture = biconvexFixture(
      "inexact-accepted",
      updateProfile = iteration => PalmBlockSolveKind.Inexact -> (0.5 * schedule.bound(iteration))
    )
    val fit = accepted(
      PalmSolver
        .from(fixture.criticalAdmission(policy))
        .solve(
          fixture.initialization("inexact-start", 2.0, 0.5),
          PalmConfig(IterationBudget.unsafe(500), CertificateTolerance.strict, PalmDescentPolicy.Monotone)
        )
    )

    assert(fit.receipt.traces.flatMap(_.blocks).forall(_.solveKind == PalmBlockSolveKind.Inexact))
    assert(fit.receipt.traces.forall: trace =>
      trace.blocks.forall(_.inexactness <= schedule.bound(trace.iteration))
    )
    assertEqualsDouble(schedule.infiniteSumBound, 2e-4, 1e-15)

    val violating = biconvexFixture(
      "inexact-rejected",
      updateProfile = _ => PalmBlockSolveKind.Inexact -> 0.2
    )
    val rejected = PalmSolver
      .from(violating.criticalAdmission(policy))
      .solve(violating.initialization("inexact-bad-start", 2.0, 0.5))
    assert(rejected.left.exists(_.isInstanceOf[PalmConvergenceError.InexactnessViolation]))

  test("deterministic multi-start preserves SVD, adversarial, and sign-symmetric starts"):
    val fixture = biconvexFixture("multi-start")
    val solver = PalmSolver.from(fixture.criticalAdmission(PalmSubproblemPolicy.Exact))
    val svd = fixture.initialization(
      "svd-start",
      2.0,
      0.5,
      PalmInitializationKind.SvdDerived(id("svd-source"))
    )
    val adversarial = fixture.initialization("zero-saddle", 0.0, 0.0)
    val negative = fixture.initialization("negative-start", -2.0, -0.5)
    val fit = accepted(
      PalmMultiStart.solve(
        solver,
        Vector(adversarial, negative, svd),
        PalmConfig(IterationBudget.unsafe(500), CertificateTolerance.strict, PalmDescentPolicy.Monotone)
      )
    )

    assertEquals(fit.starts.map(_.initialization.id), Vector(adversarial.id, negative.id, svd.id))
    assert(fit.starts.forall(_.outcome.isInstanceOf[PalmStartOutcome.Succeeded]))
    assertEquals(fit.starts(2).initialization.kind, PalmInitializationKind.SvdDerived(id("svd-source")))
    assert(fit.fit.objective < 0.5)
    assertNotEquals(fit.starts(fit.selected).initialization.id, adversarial.id)
    assertEquals(fit.selection, PalmMultiStartSelection.MinimumObjectiveThenStationarityThenId)

  test("weakly separated stationary solutions retain every start and the exact selection criterion"):
    val fixture = biconvexFixture("weak-gap", ridge = 0.99)
    val solver = PalmSolver.from(fixture.criticalAdmission(PalmSubproblemPolicy.Exact))
    val zero = fixture.initialization("weak-zero", 0.0, 0.0)
    val positive = fixture.initialization("weak-positive", 0.2, 0.2)
    val negative = fixture.initialization("weak-negative", -0.2, -0.2)
    val fit = accepted(
      PalmMultiStart.solve(
        solver,
        Vector(zero, positive, negative),
        PalmConfig(IterationBudget.unsafe(5000), CertificateTolerance.strict, PalmDescentPolicy.Monotone)
      )
    )

    val objectives = fit.starts.collect:
      case PalmStartResult(_, PalmStartOutcome.Succeeded(current)) => current.objective
    assertEquals(objectives.length, 3)
    assertEqualsDouble(objectives.head, 0.5, 1e-14)
    assert(fit.fit.objective < 0.5)
    assert(0.5 - fit.fit.objective < 1e-4)
    assertNotEquals(fit.starts(fit.selected).initialization.id, zero.id)

  test("nuclear-norm global admission is a separate convex certificate path"):
    val contract = MathematicalContractCatalog.convexifiedLowRankMatrix
    val program = id("convex-low-rank-program")
    val data = id("convex-low-rank-data")
    val functional = id("convex-loss-plus-nuclear-norm")
    val result = id("convex-low-rank-result")
    val parameter = ParameterId.unsafe("matrix")
    val bindings = accepted(
      OptimizationIdentityBindings.from(
        contract.id,
        program,
        data,
        ObservationMaskIdentity.Complete,
        Vector(functional),
        Vector(parameter),
        result
      )
    )
    val theorem = contract.theorems.head
    val witnesses = theorem.assumptions.map: current =>
      accepted(
        TheoremAssumptionWitness.from(
          bindings,
          current,
          Vector(functional),
          TheoremAssumptionEvidence.StaticType
        )
      )
    val convexLoss = accepted(
      ProperClosedConvexWitness.from(bindings, functional, theorem.assumptions.head)
    )
    val global = accepted(
      GlobalOptimalityWitness.from(
        bindings,
        theorem.id,
        theorem.assumptions.toSet,
        OracleFamily.Analytic
      )
    )
    val evidence = accepted(
      SemanticOptimizationEvidence.from(bindings, NumericalTermination.Converged)
    )
    val certificate = ConvexLowRankCertificate(
      bindings,
      convexLoss,
      witnesses,
      global,
      evidence,
      NonNegativeProofBound.residual(0.0).toOption.get
    )
    val admitted = accepted(ConvexLowRankGlobalAdmission.admitExact(certificate))

    assertEquals(admitted.claimClass, OptimizationClaimClass.ExactGlobal)
    assert(admitted.claimClass.isGlobal)

    val nonzeroResidual = certificate.copy(
      subgradientResidual = NonNegativeProofBound.residual(1e-6).toOption.get
    )
    assert(ConvexLowRankGlobalAdmission.admitExact(nonzeroResidual).isLeft)

  private final class BiconvexFixture(
      val x: ParameterId,
      val y: ParameterId,
      val problem: PalmProblem,
      val coercive: PalmLevelSetWitness,
      val kl: PalmKlEvidence
  ):
    def criticalAdmission(policy: PalmSubproblemPolicy): PalmAdmission =
      accepted(
        PalmAdmission.from(
          problem,
          coercive,
          policy,
          kl,
          PalmConvergenceTarget.CriticalPoint
        )
      )

    def initialization(
        name: String,
        xValue: Double,
        yValue: Double,
        kind: PalmInitializationKind = PalmInitializationKind.Deterministic("fixed scalar pair")
    ): PalmInitialization =
      val xBlock = accepted(PalmBlockValue.from(x, scalarMatrix(xValue), id(s"$name-x")))
      val yBlock = accepted(PalmBlockValue.from(y, scalarMatrix(yValue), id(s"$name-y")))
      val state = accepted(PalmState.from(Vector(xBlock, yBlock)))
      accepted(PalmInitialization.from(ParameterId.unsafe(name), kind, state))

  private def biconvexFixture(
      name: String,
      ridge: Double = 0.2,
      badFirstUpdate: Boolean = false,
      updateProfile: Int => (PalmBlockSolveKind, Double) = _ => PalmBlockSolveKind.Exact -> 0.0
  ): BiconvexFixture =
    val x = ParameterId.unsafe(s"$name-x")
    val y = ParameterId.unsafe(s"$name-y")
    val program = id(s"$name-program")
    val data = id(s"$name-data")
    val objectiveIdentity = id(s"$name-objective")
    val xFunctional = id(s"$name-x-functional")
    val yFunctional = id(s"$name-y-functional")
    val objective = accepted(
      PalmObjective.from(objectiveIdentity, s"$name biconvex objective"): state =>
        for
          xValue <- scalar(state, x)
          yValue <- scalar(state, y)
        yield
          val residual = xValue * yValue - 1.0
          0.5 * residual * residual + 0.5 * ridge * (xValue * xValue + yValue * yValue)
    )
    val smoothness = PositiveProofConstant.smoothness(20.0).toOption.get
    val xOracle = PalmBlockOracle.from(
      PalmBlockAssumptions(
        x,
        xFunctional,
        assumption("x-block-proper-closed-convex"),
        smoothness,
        assumption("x-partial-gradient-lipschitz")
      )
    )(
      update = (state, iteration) =>
        scalar(state, y).flatMap: yValue =>
          val next = if badFirstUpdate && iteration == 0 then 10.0 else yValue / (yValue * yValue + ridge)
          val (kind, inexactness) = updateProfile(iteration)
          PalmBlockUpdate
            .from(
              scalarMatrix(next),
              ValueIdentity.derived(s"$name-x-update-$iteration", state.valueIdentity),
              kind,
              subproblemResidual = inexactness,
              normalizationResidual = 0.0,
              inexactness = inexactness
            )
            .left
            .map(_.message),
      stationarity = state => gradient(state, x, y, ridge, forX = true),
      normalization = _ => Right(0.0)
    )
    val yOracle = PalmBlockOracle.from(
      PalmBlockAssumptions(
        y,
        yFunctional,
        assumption("y-block-proper-closed-convex"),
        smoothness,
        assumption("y-partial-gradient-lipschitz")
      )
    )(
      update = (state, iteration) =>
        scalar(state, x).flatMap: xValue =>
          val next = xValue / (xValue * xValue + ridge)
          val (kind, inexactness) = updateProfile(iteration)
          PalmBlockUpdate
            .from(
              scalarMatrix(next),
              ValueIdentity.derived(s"$name-y-update-$iteration", state.valueIdentity),
              kind,
              subproblemResidual = inexactness,
              normalizationResidual = 0.0,
              inexactness = inexactness
            )
            .left
            .map(_.message),
      stationarity = state => gradient(state, x, y, ridge, forX = false),
      normalization = _ => Right(0.0)
    )
    val problem = accepted(
      PalmProblem.from(
        MathematicalContractCatalog.generalizedLowRankModel,
        program,
        data,
        ObservationMaskIdentity.Complete,
        Vector(objectiveIdentity, xFunctional, yFunctional),
        objective,
        Vector(xOracle, yOracle)
      )
    )
    val coercive = PalmLevelSetWitness.coercive(
      program,
      PositiveProofConstant.nullspaceCoercivity(ridge).toOption.get,
      assumption("bounded-level-set")
    )
    val kl = PalmKlEvidence.SemiAlgebraic(
      objectiveIdentity,
      assumption("kl-objective"),
      "a polynomial objective is semi-algebraic and therefore a KL function"
    )
    new BiconvexFixture(x, y, problem, coercive, kl)

  private def gradient(
      state: PalmState,
      x: ParameterId,
      y: ParameterId,
      ridge: Double,
      forX: Boolean
  ): Either[String, Double] =
    for
      xValue <- scalar(state, x)
      yValue <- scalar(state, y)
    yield
      val residual = xValue * yValue - 1.0
      if forX then Math.abs(residual * yValue + ridge * xValue)
      else Math.abs(residual * xValue + ridge * yValue)

  private def scalar(state: PalmState, parameter: ParameterId): Either[String, Double] =
    state.block(parameter).left.map(_.message).map(_.values(0, 0))

  private def scalarMatrix(value: Double): DMat =
    GaleNumerics.matrixFromRowMajor(1, 1, Array(value))

  private def assumption(value: String): ContractReference[AssumptionReference] =
    accepted(ContractReference.assumption(value))

  private def id(value: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(value))

  private def accepted[A](result: Either[?, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(s"unexpected error: $error")
