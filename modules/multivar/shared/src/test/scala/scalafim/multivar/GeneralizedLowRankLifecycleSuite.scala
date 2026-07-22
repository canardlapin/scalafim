package scalafim.multivar

import gale.linalg.DMat
import CanEncode.*

class GeneralizedLowRankLifecycleSuite extends munit.FunSuite:

  test("mixed masked quadratic-logistic fitting matches an independent coordinate oracle"):
    val fixture = mixedFixture("mixed-fit")
    val config = PalmConfig(
      IterationBudget.unsafe(5000),
      tolerance(1e-9, 1e-8),
      PalmDescentPolicy.Monotone
    )
    val fit = fitted(fixture.program.fit(fixture.initial, config))
    val oracle = mixedCoordinateOracle(fixture.quadraticValue, fixture.binaryValue, fixture.ridge)
    val learnedRows = fit.payload.rowCodes.values
    val learnedDecoder = fit.payload.decoder.values

    assertEqualsDouble(learnedRows(0, 0), oracle.rowCode, 3e-5)
    assertEqualsDouble(learnedDecoder(0, 0), oracle.quadraticDecoder, 3e-5)
    assertEqualsDouble(learnedDecoder(0, 1), oracle.logisticDecoder, 3e-5)
    assertEqualsDouble(learnedDecoder(0, 2), 0.0, 5e-8)
    assertEqualsDouble(fit.payload.objective.total, oracle.objective, 2e-8)
    assertNotEquals(learnedRows(0, 0), fixture.initial.rowCodes.values(0, 0))
    assert(fit.payload.trace.objectiveHistory.sliding(2).forall:
      case Vector(before, after) => after <= before + config.tolerance.threshold(Math.abs(before))
      case _ => true
    )
    assertEquals(fit.solver.achieved.claimClass, OptimizationClaimClass.Stationary)
    assertEquals(fit.payload.trace.termination, PalmTermination.Converged)
    assert(fit.payload.trace.klEvidence.isInstanceOf[PalmKlEvidence.LogExpDefinable])

  test("the fitted artifact binds factors, observation, program, receipt, guarantee, trace, and encoder"):
    val fixture = mixedFixture("evidence-fit")
    val fit = fitted(
      fixture.program.fit(
        fixture.initial,
        PalmConfig(IterationBudget.unsafe(5000), tolerance(1e-9, 1e-8), PalmDescentPolicy.Monotone)
      )
    )
    val receipt = fit.solver.receipt.asInstanceOf[GeneralizedLowRankPalmReceipt]
    val certificate = fit.solver.certificates.values.head

    assertEquals(fit.program.objective.programIdentity, fixture.program.programIdentity)
    assertEquals(fit.program.contract.value.maturity, ContractMaturity.Executable)
    assertEquals(fit.payload.programIdentity, fixture.program.programIdentity)
    assertEquals(fit.payload.observationIdentity, fixture.observations.valueIdentity)
    assertEquals(fit.binding.data, fixture.observations.valueIdentity)
    assertEquals(fit.binding.program.valueIdentity, fixture.program.programIdentity)
    assertEquals(fit.binding.compiledProgram, fixture.program.programIdentity)
    assertEquals(fit.binding.result.valueIdentity, fit.payload.resultIdentity)
    assertEquals(receipt.trace, fit.payload.trace)
    assertEquals(receipt.observationMask, ObservationMaskIdentity.Observed(fixture.observations.valueIdentity))
    assertEquals(receipt.certificateIdentities, Vector(certificate.valueIdentity))
    assertEquals(fit.solver.achieved.semanticEvidence.numericalCertificates, Vector(certificate))
    assertEquals(fit.solver.achieved.semanticEvidence.bindings.result, fit.payload.resultIdentity)
    certificate.claim match
      case CertificateClaim.SolverTrace(iterations, residual, _, converged) =>
        assertEquals(iterations, fit.payload.trace.traces.length)
        assertEqualsDouble(residual, fit.payload.trace.finalStationarity.map(_._2).max, 1e-14)
        assert(converged)
      case other => fail(s"expected solver-trace certificate, got $other")

    val newRows = space("evidence-fit-new-row", SpaceRole.Samples, 1)
    val newObservations = glrmAccepted(
      ObservationPattern.from[newRows.Id, fixture.features.Id](
        newRows.evidence,
        fixture.features.evidence,
        Vector(
          ObservationCell.Observed(1.5),
          ObservationCell.Observed(0.0),
          ObservationCell.Missing(ObservationReason.unsafe("not supplied"))
        ),
        id("evidence-fit-new-observations")
      )
    )
    val encoded = lifecycleAccepted(fit.encodeWith(newObservations))

    assert(encoded.code.values(0).isFinite)
    assertEquals(encoded.support.features.indices, Vector(0, 1))
    assertEquals(
      fit.payload.encoder.decoder.valueIdentity,
      fit.payload.decoder.valueIdentity
    )

  test("iteration exhaustion remains a fitted unresolved result with a truthful trace certificate"):
    val fixture = mixedFixture("limited-fit")
    val fit = fitted(
      fixture.program.fit(
        fixture.initial,
        PalmConfig(
          IterationBudget.unsafe(1),
          tolerance(0.0, 0.0),
          PalmDescentPolicy.Monotone
        )
      )
    )

    assertEquals(fit.payload.trace.termination, PalmTermination.IterationLimit)
    assertEquals(fit.solver.achieved.claimClass, OptimizationClaimClass.Unresolved)
    fit.solver.certificates.values.head.claim match
      case CertificateClaim.SolverTrace(1, _, _, converged) => assert(!converged)
      case other => fail(s"expected unconverged solver trace, got $other")

  test("fit fails closed without bounded-level-set capability or a uniform loss curvature bound"):
    val fixture = mixedFixture("capability-boundary")
    val missingDecoderRidge = glrmAccepted(
      GeneralizedLowRankProgram.from(
        fixture.observations,
        fixture.layout,
        Vector(fixture.rowPenalty),
        MissingnessStatement.Unspecified,
        GlrmPredictionTarget.DescribeLatentStructure
      )
    )
    val missingCapability = missingDecoderRidge.fit(fixture.initial)

    assert(missingCapability.left.exists(_.isInstanceOf[GeneralizedLowRankFitError.MissingCapability]))

    val l1Penalties = Vector(
      GlrmFactorPenaltyTerm(
        GlrmFactorTarget.RowCodes,
        GlrmFactorPenalty.ElementwiseL1,
        PenaltyWeight.unsafe(0.5),
        id("capability-boundary-row-l1")
      ),
      GlrmFactorPenaltyTerm(
        GlrmFactorTarget.FeatureDecoder,
        GlrmFactorPenalty.ElementwiseL1,
        PenaltyWeight.unsafe(0.5),
        id("capability-boundary-decoder-l1")
      )
    )
    val l1Program = glrmAccepted(
      GeneralizedLowRankProgram.from(
        fixture.observations,
        fixture.layout,
        l1Penalties,
        MissingnessStatement.Unspecified,
        GlrmPredictionTarget.DescribeLatentStructure
      )
    )
    val declaredL1 = fitted(GeneralizedLowRankLifecycle.declare(l1Program))
    val compiledL1 = fitted(GeneralizedLowRankLifecycle.compile(declaredL1, fixture.initial))

    assert(compiledL1.rowSmoothness.doubleValue > 0.0)
    assert(compiledL1.decoderSmoothness.doubleValue > 0.0)

    val rows = space("poisson-fit-rows", SpaceRole.Samples, 1)
    val features = space("poisson-fit-features", SpaceRole.Observed, 1)
    val latent = space("poisson-fit-latent", SpaceRole.Latent, 1)
    val count = glrmAccepted(
      GlrmFeatureSpec.from(GlrmFeatureId.unsafe("count"), FeatureDomain.Count, EntryLoss.Poisson)
    )
    val layout = glrmAccepted(
      GlrmFeatureLayout.from(features.evidence, Vector(count), id("poisson-fit-layout"))
    )
    val observations = glrmAccepted(
      ObservationPattern.from[rows.Id, features.Id](
        rows.evidence,
        features.evidence,
        Vector(ObservationCell.Observed(2.0)),
        id("poisson-fit-observations")
      )
    )
    val penalties = ridgePenalties("poisson-fit", 0.5)
    val program = glrmAccepted(
      GeneralizedLowRankProgram.from(
        observations,
        layout,
        penalties,
        MissingnessStatement.Complete,
        GlrmPredictionTarget.ReconstructObserved
      )
    )
    val rowCodes = glrmAccepted(
      GlrmRowCodes.from(rows.evidence, latent.evidence, matrix(Vector(Vector(1.0))), id("poisson-fit-row-codes"))
    )
    val decoder = glrmAccepted(
      FeatureDecoder.from(layout, latent.evidence, matrix(Vector(Vector(0.5))), id("poisson-fit-decoder"))
    )
    val factors = glrmAccepted(GlrmFactors.from(rowCodes, decoder))
    val unsupported = program.fit(factors)

    assert(unsupported.left.exists:
      case GeneralizedLowRankFitError.UnsupportedLoss(feature, EntryLoss.Poisson, reason) =>
        feature == count.id && reason.contains("curvature")
      case _ => false
    )

  private final class MixedFixture(
      val name: String,
      val rows: SpaceRef,
      val features: SpaceRef,
      val latent: SpaceRef,
      val layout: GlrmFeatureLayout[features.Id],
      val observations: ObservationPattern[rows.Id, features.Id],
      val rowPenalty: GlrmFactorPenaltyTerm,
      val decoderPenalty: GlrmFactorPenaltyTerm,
      val program: GeneralizedLowRankProgram[rows.Id, features.Id],
      val initial: GlrmFactors[rows.Id, features.Id, latent.Id],
      val quadraticValue: Double,
      val binaryValue: Double,
      val ridge: Double
  )

  private def mixedFixture(name: String): MixedFixture =
    val rows = space(s"$name-rows", SpaceRole.Samples, 1)
    val features = space(s"$name-features", SpaceRole.Observed, 3)
    val latent = space(s"$name-latent", SpaceRole.Latent, 1)
    val specifications = Vector(
      glrmAccepted(
        GlrmFeatureSpec.from(
          GlrmFeatureId.unsafe(s"$name-quadratic"),
          FeatureDomain.Real,
          EntryLoss.Quadratic
        )
      ),
      glrmAccepted(
        GlrmFeatureSpec.from(
          GlrmFeatureId.unsafe(s"$name-logistic"),
          FeatureDomain.Binary,
          EntryLoss.Logistic
        )
      ),
      glrmAccepted(
        GlrmFeatureSpec.from(
          GlrmFeatureId.unsafe(s"$name-withheld"),
          FeatureDomain.Real,
          EntryLoss.Quadratic
        )
      )
    )
    val layout = glrmAccepted(
      GlrmFeatureLayout.from(features.evidence, specifications, id(s"$name-layout"))
    )
    val quadraticValue = 2.0
    val binaryValue = 1.0
    val observations = glrmAccepted(
      ObservationPattern.from[rows.Id, features.Id](
        rows.evidence,
        features.evidence,
        Vector(
          ObservationCell.Observed(quadraticValue),
          ObservationCell.Observed(binaryValue),
          ObservationCell.Missing(ObservationReason.unsafe("held out by design"))
        ),
        id(s"$name-observations")
      )
    )
    val ridge = 0.4
    val penalties = ridgePenalties(name, ridge)
    val program = glrmAccepted(
      GeneralizedLowRankProgram.from(
        observations,
        layout,
        penalties,
        MissingnessStatement.Unspecified,
        GlrmPredictionTarget.DescribeLatentStructure
      )
    )
    val rowCodes = glrmAccepted(
      GlrmRowCodes.from(
        rows.evidence,
        latent.evidence,
        matrix(Vector(Vector(1.0))),
        id(s"$name-initial-row-codes")
      )
    )
    val decoder = glrmAccepted(
      FeatureDecoder.from(
        layout,
        latent.evidence,
        matrix(Vector(Vector(1.0, 0.5, 0.25))),
        id(s"$name-initial-decoder")
      )
    )
    val initial = glrmAccepted(GlrmFactors.from(rowCodes, decoder))
    new MixedFixture(
      name,
      rows,
      features,
      latent,
      layout,
      observations,
      penalties.head,
      penalties(1),
      program,
      initial,
      quadraticValue,
      binaryValue,
      ridge
    )

  private final case class MixedOracle(
      rowCode: Double,
      quadraticDecoder: Double,
      logisticDecoder: Double,
      objective: Double
  )

  /** Test-local cyclic minimization uses analytic quadratic updates and
    * bisection of monotone logistic first-order equations. It shares no PALM
    * gradient, step-size, or stopping code with the implementation.
    */
  private def mixedCoordinateOracle(
      quadraticValue: Double,
      binaryValue: Double,
      ridge: Double
  ): MixedOracle =
    var rowCode = 1.0
    var quadraticDecoder = 1.0
    var logisticDecoder = 0.5
    var iteration = 0
    var change = Double.PositiveInfinity
    while iteration < 10000 && change > 1e-14 do
      val previousRow = rowCode
      val previousQuadratic = quadraticDecoder
      val previousLogistic = logisticDecoder
      quadraticDecoder = quadraticValue * rowCode / (rowCode * rowCode + ridge)
      logisticDecoder = bisect: decoder =>
        rowCode * (sigmoid(rowCode * decoder) - binaryValue) + ridge * decoder
      rowCode = bisect: code =>
        val quadraticGradient = (code * quadraticDecoder - quadraticValue) * quadraticDecoder
        val logisticGradient = (sigmoid(code * logisticDecoder) - binaryValue) * logisticDecoder
        quadraticGradient + logisticGradient + ridge * code
      change = Math.max(
        Math.abs(rowCode - previousRow),
        Math.max(
          Math.abs(quadraticDecoder - previousQuadratic),
          Math.abs(logisticDecoder - previousLogistic)
        )
      )
      iteration += 1
    val quadraticResidual = rowCode * quadraticDecoder - quadraticValue
    val logisticNatural = rowCode * logisticDecoder
    val objective =
      0.5 * quadraticResidual * quadraticResidual +
        softplus(logisticNatural) - binaryValue * logisticNatural +
        0.5 * ridge * (
          rowCode * rowCode +
            quadraticDecoder * quadraticDecoder +
            logisticDecoder * logisticDecoder
        )
    MixedOracle(rowCode, quadraticDecoder, logisticDecoder, objective)

  private def bisect(function: Double => Double): Double =
    var lower = -32.0
    var upper = 32.0
    var iteration = 0
    while iteration < 200 do
      val middle = 0.5 * (lower + upper)
      if function(middle) <= 0.0 then lower = middle else upper = middle
      iteration += 1
    0.5 * (lower + upper)

  private def sigmoid(value: Double): Double =
    if value >= 0.0 then
      val exponential = Math.exp(-value)
      1.0 / (1.0 + exponential)
    else
      val exponential = Math.exp(value)
      exponential / (1.0 + exponential)

  private def softplus(value: Double): Double =
    if value > 0.0 then value + Math.log1p(Math.exp(-value))
    else Math.log1p(Math.exp(value))

  private def ridgePenalties(name: String, weight: Double): Vector[GlrmFactorPenaltyTerm] =
    Vector(
      GlrmFactorPenaltyTerm(
        GlrmFactorTarget.RowCodes,
        GlrmFactorPenalty.SquaredFrobenius,
        PenaltyWeight.unsafe(weight),
        id(s"$name-row-ridge")
      ),
      GlrmFactorPenaltyTerm(
        GlrmFactorTarget.FeatureDecoder,
        GlrmFactorPenalty.SquaredFrobenius,
        PenaltyWeight.unsafe(weight),
        id(s"$name-decoder-ridge")
      )
    )

  private def matrix(values: Vector[Vector[Double]]): DMat =
    GaleNumerics.matrixFromRows(values)

  private def space(name: String, role: SpaceRole, dimension: Int): SpaceRef =
    SpaceRef(MvSpace(SpaceId.unsafe(name), role, Dimension.unsafe(dimension)))

  private def id(value: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(value))

  private def tolerance(absolute: Double, relative: Double): CertificateTolerance =
    CertificateTolerance.from(absolute, relative).fold(error => fail(error.message), identity)

  private def glrmAccepted[A](value: Either[GeneralizedLowRankError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def fitted[A](value: Either[GeneralizedLowRankFitError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def lifecycleAccepted[A](value: Either[ModelLifecycleError, A]): A =
    value.fold(error => fail(error.message), identity)
