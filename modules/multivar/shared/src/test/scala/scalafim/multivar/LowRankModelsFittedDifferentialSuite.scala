package scalafim.multivar

import scalafim.multivar.core.*
import scalafim.multivar.contract.*
import scalafim.multivar.optimization.*
import scalafim.multivar.solver.*
import scalafim.multivar.lifecycle.*
import scalafim.multivar.family.glrm.*

import gale.linalg.DMat

class LowRankModelsFittedDifferentialSuite extends munit.FunSuite:
  private val valueTolerance = 2e-7

  test("deterministic PALM multi-start fits match identifiable LowRankModels.jl invariants"):
    val external = LowRankModelsFittedOracleFixtureData
    assertEquals(external.schemaVersion, "scalafim.lowrankmodels.fitted-oracle.v1")
    assertEquals(external.provenance, LowRankModelsOracleFixtureData.provenance)
    assertEquals(external.starts.map(_.id).toSet, Set("positive", "negative-sign", "skew"))
    assert(external.limitations.exists(_.contains("never requires literal factor equality")))
    val fixture = fittedFixture()
    val initializations = external.starts.map(start => fixture.initialization(start))
    val result = accepted(
      PalmMultiStart.solve(
        fixture.solver,
        initializations,
        PalmConfig(
          IterationBudget.unsafe(1000),
          accepted(CertificateTolerance.from(1e-11, 1e-10)),
          PalmDescentPolicy.Monotone
        )
      )
    )

    assertEquals(result.starts.map(_.initialization.id.value), external.starts.map(_.id))
    assert(result.starts.forall(_.outcome.isInstanceOf[PalmStartOutcome.Succeeded]))
    result.starts.zip(external.starts).foreach:
      case (PalmStartResult(_, PalmStartOutcome.Succeeded(scalaFit)), juliaFit) =>
        checkFit(fixture, scalaFit, juliaFit)
      case (PalmStartResult(initialization, PalmStartOutcome.Failed(error)), _) =>
        fail(s"${initialization.id.value} failed: ${error.message}")
    assertEquals(result.selection, PalmMultiStartSelection.MinimumObjectiveThenStationarityThenId)
    assertEquals(result.fit.achievement.claimClass, OptimizationClaimClass.Stationary)
    assert(!result.fit.achievement.claimClass.isGlobal)

  test("external full-objective checkpoints are monotone and expose the upstream history limitation"):
    LowRankModelsFittedOracleFixtureData.starts.foreach: start =>
      assertEquals(start.objectiveCheckpoints.map(_.iteration), Vector(0, 1, 2, 5, 10, 25, 50, 100, 200))
      start.objectiveCheckpoints.map(_.fullObjective).sliding(2).foreach:
        case Vector(previous, next) =>
          assert(next <= previous + 1e-10, s"${start.id} external full objective increased")
        case _ => ()
      assertEqualsDouble(start.objectiveCheckpoints.last.fullObjective, start.fullObjective, 1e-12)
      assert(start.stationarity <= 1e-6, s"${start.id} external stationarity")
      assertEquals(start.upstreamReportedHistory.length, LowRankModelsFittedOracleFixtureData.maximumIterations + 1)
      assertEqualsDouble(start.upstreamReportedHistory.last, start.upstreamReportedFinal, 1e-12)
      assertEqualsDouble(
        start.upstreamReportedFinal - start.fullObjective,
        start.upstreamHistoryObjectiveDiscrepancy,
        1e-12
      )
      assert(
        Math.abs(start.upstreamHistoryObjectiveDiscrepancy) > 0.5,
        s"${start.id} should retain evidence that ConvergenceHistory.objective is not the full objective"
      )

  private final case class FittedFixture(
      solver: PalmSolver,
      rowParameter: ParameterId,
      decoderParameter: ParameterId,
      context: ProgramContext
  ):
    def initialization(start: LowRankModelsFittedOracleStart): PalmInitialization =
      val rowCodes = accepted(
        PalmBlockValue.from(
          rowParameter,
          column(start.initialRowCodes),
          id(s"${start.id}-initial-row-codes")
        )
      )
      val decoder = accepted(
        PalmBlockValue.from(
          decoderParameter,
          column(start.initialDecoder),
          id(s"${start.id}-initial-decoder")
        )
      )
      val state = accepted(PalmState.from(Vector(rowCodes, decoder)))
      accepted(
        PalmInitialization.from(
          ParameterId.unsafe(start.id),
          PalmInitializationKind.Deterministic("explicit LowRankModels.jl differential start"),
          state
        )
      )

  private final class ProgramContext(
      val rows: SpaceRef,
      val features: SpaceRef,
      val latent: SpaceRef
  )(
      val layout: GlrmFeatureLayout[features.Id],
      val program: GeneralizedLowRankProgram[rows.Id, features.Id]
  )

  private def fittedFixture(): FittedFixture =
    val data = LowRankModelsFittedOracleFixtureData.data
    val ridge = LowRankModelsFittedOracleFixtureData.ridge
    val rowParameter = ParameterId.unsafe("lowrankmodels-fitted-row-codes")
    val decoderParameter = ParameterId.unsafe("lowrankmodels-fitted-decoder")
    val programIdentity = id("lowrankmodels-fitted-program")
    val dataIdentity = id("lowrankmodels-fitted-data")
    val objectiveIdentity = id("lowrankmodels-fitted-objective")
    val rowFunctional = id("lowrankmodels-fitted-row-functional")
    val decoderFunctional = id("lowrankmodels-fitted-decoder-functional")
    val objective = accepted(
      PalmObjective.from(objectiveIdentity, "ridge-regularized rank-one quadratic GLRM"): state =>
        for
          rowCodes <- block(state, rowParameter)
          decoder <- block(state, decoderParameter)
        yield directObjective(data, ridge, rowCodes, decoder)
    )
    val smoothness = PositiveProofConstant.smoothness(20.0).toOption.get
    val rowOracle = PalmBlockOracle.from(
      PalmBlockAssumptions(
        rowParameter,
        rowFunctional,
        assumption("row-block-proper-closed-convex"),
        smoothness,
        assumption("row-partial-gradient-lipschitz-on-level-set")
      )
    )(
      update = (state, iteration) =>
        block(state, decoderParameter).flatMap: decoder =>
          PalmBlockUpdate
            .from(
              exactRowUpdate(data, ridge, decoder),
              ValueIdentity.derived(s"row-update-$iteration", state.valueIdentity),
              PalmBlockSolveKind.Exact,
              subproblemResidual = 0.0,
              normalizationResidual = 0.0,
              inexactness = 0.0
            )
            .left
            .map(_.message),
      stationarity = state => stationarity(state, rowParameter, decoderParameter, data, ridge, forRows = true),
      normalization = _ => Right(0.0)
    )
    val decoderOracle = PalmBlockOracle.from(
      PalmBlockAssumptions(
        decoderParameter,
        decoderFunctional,
        assumption("decoder-block-proper-closed-convex"),
        smoothness,
        assumption("decoder-partial-gradient-lipschitz-on-level-set")
      )
    )(
      update = (state, iteration) =>
        block(state, rowParameter).flatMap: rowCodes =>
          PalmBlockUpdate
            .from(
              exactDecoderUpdate(data, ridge, rowCodes),
              ValueIdentity.derived(s"decoder-update-$iteration", state.valueIdentity),
              PalmBlockSolveKind.Exact,
              subproblemResidual = 0.0,
              normalizationResidual = 0.0,
              inexactness = 0.0
            )
            .left
            .map(_.message),
      stationarity = state => stationarity(state, rowParameter, decoderParameter, data, ridge, forRows = false),
      normalization = _ => Right(0.0)
    )
    val problem = accepted(
      PalmProblem.from(
        MathematicalContractCatalog.generalizedLowRankModel,
        programIdentity,
        dataIdentity,
        ObservationMaskIdentity.Complete,
        Vector(objectiveIdentity, rowFunctional, decoderFunctional),
        objective,
        Vector(rowOracle, decoderOracle)
      )
    )
    val levelSet = PalmLevelSetWitness.coercive(
      programIdentity,
      PositiveProofConstant.nullspaceCoercivity(ridge).toOption.get,
      assumption("positive-ridge-bounds-level-sets")
    )
    val kl = PalmKlEvidence.SemiAlgebraic(
      objectiveIdentity,
      assumption("polynomial-objective-is-kl"),
      "the finite-dimensional polynomial objective is semi-algebraic"
    )
    val admission = accepted(
      PalmAdmission.from(
        problem,
        levelSet,
        PalmSubproblemPolicy.Exact,
        kl,
        PalmConvergenceTarget.CriticalPoint
      )
    )
    FittedFixture(PalmSolver.from(admission), rowParameter, decoderParameter, programContext(data, ridge))

  private def programContext(data: Vector[Vector[Double]], ridge: Double): ProgramContext =
    val rows = space("lowrankmodels-fitted-rows", data.length)
    val features = space("lowrankmodels-fitted-features", data.head.length)
    val latent = space("lowrankmodels-fitted-latent", 1)
    val specifications = Vector.tabulate(data.head.length): feature =>
      acceptedGeneralized(
        GlrmFeatureSpec.from(
          GlrmFeatureId.unsafe(s"lowrankmodels-fitted-feature-$feature"),
          FeatureDomain.Real,
          EntryLoss.Quadratic
        )
      )
    val layout = acceptedGeneralized(
      GlrmFeatureLayout.from(features.evidence, specifications, id("lowrankmodels-fitted-layout"))
    )
    val pattern = acceptedGeneralized(
      ObservationPattern.from(
        rows.evidence,
        features.evidence,
        data.flatten.map(ObservationCell.Observed(_)),
        id("lowrankmodels-fitted-observations")
      )
    )
    val penalties = Vector(
      GlrmFactorPenaltyTerm(
        GlrmFactorTarget.RowCodes,
        GlrmFactorPenalty.SquaredFrobenius,
        PenaltyWeight.unsafe(ridge),
        id("lowrankmodels-fitted-row-penalty")
      ),
      GlrmFactorPenaltyTerm(
        GlrmFactorTarget.FeatureDecoder,
        GlrmFactorPenalty.SquaredFrobenius,
        PenaltyWeight.unsafe(ridge),
        id("lowrankmodels-fitted-decoder-penalty")
      )
    )
    val program = acceptedGeneralized(
      GeneralizedLowRankProgram.from(
        pattern,
        layout,
        penalties,
        MissingnessStatement.Unspecified,
        GlrmPredictionTarget.ReconstructObserved
      )
    )
    new ProgramContext(rows, features, latent)(layout, program)

  private def checkFit(
      fixture: FittedFixture,
      scalaFit: PalmFit,
      juliaFit: LowRankModelsFittedOracleStart
  ): Unit =
    val rowCodes = accepted(scalaFit.state.block(fixture.rowParameter)).values
    val decoder = accepted(scalaFit.state.block(fixture.decoderParameter)).values
    val scalaObjective = objectiveViaProgram(fixture.context, rowCodes, decoder, s"${juliaFit.id}-scala")
    val juliaObjective = objectiveViaProgram(
      fixture.context,
      column(juliaFit.fittedRowCodes),
      column(juliaFit.fittedDecoder),
      s"${juliaFit.id}-julia"
    )
    assertEqualsDouble(scalaFit.objective, scalaObjective.total, 1e-10)
    assertEqualsDouble(juliaObjective.total, juliaFit.fullObjective, 1e-10)
    assertEqualsDouble(scalaObjective.total, juliaFit.fullObjective, valueTolerance)
    assert(scalaFit.receipt.finalStationarity.map(_._2).max <= 2e-10, s"${juliaFit.id} Scala stationarity")
    assertEquals(scalaFit.receipt.termination, PalmTermination.Converged)
    scalaFit.receipt.objectiveHistory.sliding(2).foreach:
      case Vector(previous, next) => assert(next <= previous + 1e-10, s"${juliaFit.id} Scala objective increased")
      case _ => ()

    val scalaReconstruction = reconstruct(rowCodes, decoder)
    assertMatrixClose(scalaReconstruction, juliaFit.reconstruction, valueTolerance, s"${juliaFit.id} reconstruction")
    val scalaDecoded = decoded(fixture.context, rowCodes, decoder, s"${juliaFit.id}-decoded")
    assertMatrixClose(scalaDecoded, juliaFit.decoded, valueTolerance, s"${juliaFit.id} decoded")
    assertEqualsDouble(
      scalaFit.receipt.initialObjective,
      juliaFit.objectiveCheckpoints.head.fullObjective,
      1e-10
    )

  private def objectiveViaProgram(
      context: ProgramContext,
      rowCodes: DMat,
      decoder: DMat,
      label: String
  ): GlrmObjectiveValue =
    val factors = factorsFor(context, rowCodes, decoder, label)
    acceptedGeneralized(context.program.evaluate(factors))

  private def decoded(
      context: ProgramContext,
      rowCodes: DMat,
      decoder: DMat,
      label: String
  ): Vector[Vector[Double]] =
    val factors = factorsFor(context, rowCodes, decoder, label)
    Vector.tabulate(rowCodes.rows): row =>
      Vector.tabulate(decoder.rows): feature =>
        acceptedGeneralized(factors.decoded(row, feature)) match
          case DecodedPrediction.Point(value) => value
          case other => fail(s"unexpected fitted decoder $other")

  private def factorsFor(
      context: ProgramContext,
      rowCodes: DMat,
      decoder: DMat,
      label: String
  ): GlrmFactors[context.rows.Id, context.features.Id, context.latent.Id] =
    val rows = acceptedGeneralized(
      GlrmRowCodes.from(context.rows.evidence, context.latent.evidence, rowCodes, id(s"$label-row-codes"))
    )
    val featureDecoder = acceptedGeneralized(
      FeatureDecoder.from(context.layout, context.latent.evidence, decoder.t, id(s"$label-decoder"))
    )
    acceptedGeneralized(GlrmFactors.from(rows, featureDecoder))

  private def exactRowUpdate(data: Vector[Vector[Double]], ridge: Double, decoder: DMat): DMat =
    val denominator = squaredNorm(decoder) + ridge
    val values = Array.ofDim[Double](data.length)
    var row = 0
    while row < data.length do
      var numerator = 0.0
      var feature = 0
      while feature < data(row).length do
        numerator += data(row)(feature) * decoder(feature, 0)
        feature += 1
      values(row) = numerator / denominator
      row += 1
    GaleNumerics.matrixFromRowMajor(data.length, 1, values)

  private def exactDecoderUpdate(data: Vector[Vector[Double]], ridge: Double, rowCodes: DMat): DMat =
    val denominator = squaredNorm(rowCodes) + ridge
    val values = Array.ofDim[Double](data.head.length)
    var feature = 0
    while feature < data.head.length do
      var numerator = 0.0
      var row = 0
      while row < data.length do
        numerator += data(row)(feature) * rowCodes(row, 0)
        row += 1
      values(feature) = numerator / denominator
      feature += 1
    GaleNumerics.matrixFromRowMajor(data.head.length, 1, values)

  private def directObjective(
      data: Vector[Vector[Double]],
      ridge: Double,
      rowCodes: DMat,
      decoder: DMat
  ): Double =
    var loss = 0.0
    var row = 0
    while row < data.length do
      var feature = 0
      while feature < data(row).length do
        val residual = rowCodes(row, 0) * decoder(feature, 0) - data(row)(feature)
        loss += 0.5 * residual * residual
        feature += 1
      row += 1
    loss + 0.5 * ridge * (squaredNorm(rowCodes) + squaredNorm(decoder))

  private def stationarity(
      state: PalmState,
      rowParameter: ParameterId,
      decoderParameter: ParameterId,
      data: Vector[Vector[Double]],
      ridge: Double,
      forRows: Boolean
  ): Either[String, Double] =
    for
      rowCodes <- block(state, rowParameter)
      decoder <- block(state, decoderParameter)
    yield
      var squared = 0.0
      if forRows then
        var row = 0
        while row < data.length do
          var gradient = ridge * rowCodes(row, 0)
          var feature = 0
          while feature < data(row).length do
            gradient += (rowCodes(row, 0) * decoder(feature, 0) - data(row)(feature)) * decoder(feature, 0)
            feature += 1
          squared += gradient * gradient
          row += 1
      else
        var feature = 0
        while feature < data.head.length do
          var gradient = ridge * decoder(feature, 0)
          var row = 0
          while row < data.length do
            gradient += (rowCodes(row, 0) * decoder(feature, 0) - data(row)(feature)) * rowCodes(row, 0)
            row += 1
          squared += gradient * gradient
          feature += 1
      Math.sqrt(squared)

  private def reconstruct(rowCodes: DMat, decoder: DMat): Vector[Vector[Double]] =
    Vector.tabulate(rowCodes.rows): row =>
      Vector.tabulate(decoder.rows)(feature => rowCodes(row, 0) * decoder(feature, 0))

  private def squaredNorm(matrix: DMat): Double =
    var result = 0.0
    var row = 0
    while row < matrix.rows do
      var column = 0
      while column < matrix.cols do
        val value = matrix(row, column)
        result += value * value
        column += 1
      row += 1
    result

  private def block(state: PalmState, parameter: ParameterId): Either[String, DMat] =
    state.block(parameter).left.map(_.message).map(_.values)

  private def column(values: Vector[Double]): DMat =
    GaleNumerics.matrixFromRowMajor(values.length, 1, values.toArray)

  private def assertMatrixClose(
      actual: Vector[Vector[Double]],
      expected: Vector[Vector[Double]],
      tolerance: Double,
      label: String
  ): Unit =
    assertEquals(actual.length, expected.length, label)
    actual.indices.foreach: row =>
      assertEquals(actual(row).length, expected(row).length, s"$label row $row")
      actual(row).indices.foreach: column =>
        assertEqualsDouble(actual(row)(column), expected(row)(column), tolerance)

  private def space(name: String, dimension: Int): SpaceRef =
    SpaceRef(MvSpace(SpaceId.unsafe(name), SpaceRole.Observed, Dimension.unsafe(dimension)))

  private def assumption(value: String): ContractReference[AssumptionReference] =
    accepted(ContractReference.assumption(value))

  private def id(value: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(value))

  private def accepted[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(s"unexpected error: $error")

  private def acceptedGeneralized[A](value: Either[GeneralizedLowRankError, A]): A =
    value.fold(error => fail(error.message), identity)
