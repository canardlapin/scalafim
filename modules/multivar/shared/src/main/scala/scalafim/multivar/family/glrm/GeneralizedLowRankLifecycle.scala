package scalafim.multivar
package family.glrm

import scalafim.multivar.core.*
import scalafim.multivar.contract.*
import scalafim.multivar.optimization.*
import scalafim.multivar.solver.*
import scalafim.multivar.lifecycle.*

import gale.linalg.DMat

type GeneralizedLowRankFamily = MathematicalModelFamily.GeneralizedLowRankModel.type

enum GeneralizedLowRankFitError:
  case InvalidDefinition(detail: String)
  case UnsupportedLoss(feature: GlrmFeatureId, loss: EntryLoss, reason: String)
  case MissingCapability(detail: String)
  case Generalized(error: GeneralizedLowRankError)
  case Palm(error: PalmConvergenceError)
  case Lifecycle(error: ModelLifecycleError)
  case Encoding(error: LatentEncodingError)
  case Guarantee(error: OptimizationGuaranteeError)

  def message: String =
    this match
      case InvalidDefinition(detail) => detail
      case UnsupportedLoss(feature, loss, reason) =>
        s"feature '${feature.value}' uses unsupported fit loss $loss: $reason"
      case MissingCapability(detail) => detail
      case Generalized(error) => error.message
      case Palm(error) => error.message
      case Lifecycle(error) => error.message
      case Encoding(error) => error.message
      case Guarantee(error) => error.message

/** Declared GLRM objective in the common family-indexed lifecycle. */
final class GeneralizedLowRankModelProgram[
    Rows <: SemanticSpace,
    Feature <: SemanticSpace
] private (
    val objective: GeneralizedLowRankProgram[Rows, Feature],
    val id: ProgramId[GeneralizedLowRankFamily],
    val contract: FamilyContract[GeneralizedLowRankFamily],
    val provenance: SemanticProvenance
) extends ModelProgram[GeneralizedLowRankFamily]:
  val family: GeneralizedLowRankFamily = MathematicalModelFamily.GeneralizedLowRankModel
  val requestedClaim: RequestedOptimizationClaim = RequestedOptimizationClaim.Stationary

object GeneralizedLowRankModelProgram:
  def from[Rows <: SemanticSpace, Feature <: SemanticSpace](
      objective: GeneralizedLowRankProgram[Rows, Feature]
  ): Either[GeneralizedLowRankFitError, GeneralizedLowRankModelProgram[Rows, Feature]] =
    FamilyContract
      .from[GeneralizedLowRankFamily](MathematicalContractCatalog.generalizedLowRankModel)
      .left
      .map(GeneralizedLowRankFitError.Lifecycle.apply)
      .map: contract =>
        new GeneralizedLowRankModelProgram(
          objective,
          ProgramId.from(objective.programIdentity),
          contract,
          SemanticProvenance.source("generalized-low-rank-program")
        )

/** Execution-ready two-block PALM plan. Its initialization is part of the
  * admitted level set, so the static block-smoothness bounds are checkable.
  */
final class GeneralizedLowRankCompiledModel[
    Rows <: SemanticSpace,
    Feature <: SemanticSpace,
    Latent <: SemanticSpace
] private[multivar] (
    val program: GeneralizedLowRankModelProgram[Rows, Feature],
    val latentSpace: SpaceEvidence[Latent],
    val problem: PalmProblem,
    val admission: PalmAdmission,
    val initialization: PalmInitialization,
    val initialObjective: GlrmObjectiveValue,
    val rowSmoothness: SmoothnessConstant,
    val decoderSmoothness: SmoothnessConstant,
    val id: CompiledId[GeneralizedLowRankFamily],
    val solverPlan: SolverPlan[GeneralizedLowRankFamily],
    val provenance: SemanticProvenance
) extends CompiledModel[GeneralizedLowRankFamily, GeneralizedLowRankModelProgram[Rows, Feature]]:
  def executionProgramIdentity: ValueIdentity = problem.programIdentity

/** Learned GLRM state and the exact solver evidence used to admit it. */
final class GeneralizedLowRankFitPayload[
    Rows <: SemanticSpace,
    Feature <: SemanticSpace,
    Latent <: SemanticSpace
] private[multivar] (
    val factors: GlrmFactors[Rows, Feature, Latent],
    val objective: GlrmObjectiveValue,
    val trace: PalmConvergenceReceipt,
    val encoder: FittedLatentEncoder[Feature, Latent],
    val observationIdentity: ValueIdentity,
    val programIdentity: ValueIdentity,
    val resultIdentity: ValueIdentity
):
  def rowCodes: GlrmRowCodes[Rows, Latent] = factors.rowCodes
  def decoder: FeatureDecoder[Feature, Latent] = factors.decoder

type GeneralizedLowRankFit[
    Rows <: SemanticSpace,
    Feature <: SemanticSpace,
    Latent <: SemanticSpace
] = FittedModel[
  GeneralizedLowRankFamily,
  GeneralizedLowRankModelProgram[Rows, Feature],
  GeneralizedLowRankFitPayload[Rows, Feature, Latent]
]

final class GeneralizedLowRankPalmReceipt private[multivar] (
    val program: ProgramId[GeneralizedLowRankFamily],
    val compiled: CompiledId[GeneralizedLowRankFamily],
    val compiledProgram: ValueIdentity,
    val planIdentity: ValueIdentity,
    val data: ValueIdentity,
    val result: ResultId[GeneralizedLowRankFamily],
    val certificateIdentities: Vector[ValueIdentity],
    val observationMask: ObservationMaskIdentity,
    val initializationIdentity: ValueIdentity,
    val trace: PalmConvergenceReceipt
) extends SolverReceipt[GeneralizedLowRankFamily]

object GeneralizedLowRankPalmReceipt:
  private[multivar] def from(
      binding: FitBinding[GeneralizedLowRankFamily],
      plan: SolverPlan[GeneralizedLowRankFamily],
      problem: PalmProblem,
      initialization: PalmInitialization,
      fit: PalmFit
  ): GeneralizedLowRankPalmReceipt =
    new GeneralizedLowRankPalmReceipt(
      binding.program,
      binding.compiled,
      binding.compiledProgram,
      plan.valueIdentity,
      binding.data,
      binding.result,
      Vector(fit.certificate.valueIdentity),
      problem.observationMask,
      initialization.valueIdentity,
      fit.receipt
    )

object GeneralizedLowRankLifecycle:
  private val rowParameter: ParameterId = ParameterId.unsafe("glrm-row-codes")
  private val decoderParameter: ParameterId = ParameterId.unsafe("glrm-feature-decoder")
  private val initializationParameter: ParameterId = ParameterId.unsafe("glrm-initialization")
  private val stepSafety: Double = 1.05

  def declare[Rows <: SemanticSpace, Feature <: SemanticSpace](
      objective: GeneralizedLowRankProgram[Rows, Feature]
  ): Either[GeneralizedLowRankFitError, GeneralizedLowRankModelProgram[Rows, Feature]] =
    GeneralizedLowRankModelProgram.from(objective)

  def compile[
      Rows <: SemanticSpace,
      Feature <: SemanticSpace,
      Latent <: SemanticSpace
  ](
      program: GeneralizedLowRankModelProgram[Rows, Feature],
      initial: GlrmFactors[Rows, Feature, Latent]
  ): Either[GeneralizedLowRankFitError, GeneralizedLowRankCompiledModel[Rows, Feature, Latent]] =
    val objective = program.objective
    for
      _ <- validateLossCapabilities(objective)
      entries <- observedEntries(objective)
      penalties <- admittedPenalties(objective.factorPenalties)
      initialObjective <- objective.evaluate(initial).left.map(GeneralizedLowRankFitError.Generalized.apply)
      rowBound = blockSmoothnessBound(entries, initialObjective.total, penalties.decoder)
      decoderBound = blockSmoothnessBound(entries, initialObjective.total, penalties.row)
      rowSmoothness <- PositiveProofConstant
        .smoothness(rowBound)
        .left
        .map(GeneralizedLowRankFitError.Guarantee.apply)
      decoderSmoothness <- PositiveProofConstant
        .smoothness(decoderBound)
        .left
        .map(GeneralizedLowRankFitError.Guarantee.apply)
      state <- initialState(initial).left.map(GeneralizedLowRankFitError.Palm.apply)
      initialization <- PalmInitialization
        .from(
          initializationParameter,
          PalmInitializationKind.Deterministic("caller-supplied validated GLRM factors"),
          state
        )
        .left
        .map(GeneralizedLowRankFitError.Palm.apply)
      objectiveIdentity = ValueIdentity.derived("glrm-palm-objective", objective.programIdentity)
      rowFunctional = ValueIdentity.derived(
        "glrm-row-factor-functional",
        (objective.programIdentity +: objective.factorPenalties.filter(_.target == GlrmFactorTarget.RowCodes).map(_.valueIdentity))*
      )
      decoderFunctional = ValueIdentity.derived(
        "glrm-decoder-functional",
        (objective.programIdentity +: objective.factorPenalties.filter(_.target == GlrmFactorTarget.FeatureDecoder).map(_.valueIdentity))*
      )
      palmObjective <- PalmObjective
        .from(objectiveIdentity, "masked generalized low-rank objective"): current =>
          factorsFromState(program, initial.rowCodes.latentSpace, current)
            .flatMap(factors => objective.evaluate(factors).left.map(_.message))
            .map(_.total)
        .left
        .map(GeneralizedLowRankFitError.Palm.apply)
      rowOracle = blockOracle(
        program,
        initial.rowCodes.latentSpace,
        entries,
        rowParameter,
        rowFunctional,
        rowSmoothness,
        penalties.row
      )
      decoderOracle = blockOracle(
        program,
        initial.rowCodes.latentSpace,
        entries,
        decoderParameter,
        decoderFunctional,
        decoderSmoothness,
        penalties.decoder
      )
      operators = (
        Vector(objectiveIdentity, rowFunctional, decoderFunctional, objective.layout.valueIdentity) ++
          objective.factorPenalties.map(_.valueIdentity)
      ).distinct
      problem <- PalmProblem
        .from(
          program.contract.value,
          objective.programIdentity,
          objective.observations.valueIdentity,
          observationMask(objective.observations),
          operators,
          palmObjective,
          Vector(rowOracle, decoderOracle)
        )
        .left
        .map(GeneralizedLowRankFitError.Palm.apply)
      coercivity <- PositiveProofConstant
        .nullspaceCoercivity(Math.min(penalties.row.coercivityWeight, penalties.decoder.coercivityWeight))
        .left
        .map(GeneralizedLowRankFitError.Guarantee.apply)
      levelSet = PalmLevelSetWitness.coercive(
        objective.programIdentity,
        coercivity,
        assumption("bounded-iterates")
      )
      admission <- PalmAdmission
        .from(
          problem,
          levelSet,
          PalmSubproblemPolicy.Exact,
          klEvidence(objectiveIdentity, entries),
          PalmConvergenceTarget.CriticalPoint
        )
        .left
        .map(GeneralizedLowRankFitError.Palm.apply)
      solverPlan <- SolverPlan
        .from(program.id, "portable-glrm-palm-v1", program.requestedClaim)
        .left
        .map(GeneralizedLowRankFitError.Lifecycle.apply)
      compiledId = CompiledId.derive(
        program.id,
        Vector(admission.admissionIdentity, initialization.valueIdentity)
      )
      provenance = program.provenance.append(
        SemanticProvenanceEvent.Derived(
          "compile-generalized-low-rank-palm",
          Vector(objective.programIdentity, admission.admissionIdentity, initialization.valueIdentity)
        )
      )
    yield
      new GeneralizedLowRankCompiledModel(
        program,
        initial.rowCodes.latentSpace,
        problem,
        admission,
        initialization,
        initialObjective,
        rowSmoothness,
        decoderSmoothness,
        compiledId,
        solverPlan,
        provenance
      )

  def solve[
      Rows <: SemanticSpace,
      Feature <: SemanticSpace,
      Latent <: SemanticSpace
  ](
      compiled: GeneralizedLowRankCompiledModel[Rows, Feature, Latent],
      scope: TrainingScopeId,
      config: PalmConfig = PalmConfig.portable
  ): Either[GeneralizedLowRankFitError, GeneralizedLowRankFit[Rows, Feature, Latent]] =
    for
      palmFit <- PalmSolver
        .from(compiled.admission)
        .solve(compiled.initialization, config)
        .left
        .map(GeneralizedLowRankFitError.Palm.apply)
      factors <- factorsFromState(compiled.program, compiled.latentSpace, palmFit.state)
        .left
        .map(GeneralizedLowRankFitError.InvalidDefinition.apply)
      objective <- compiled.program.objective
        .evaluate(factors)
        .left
        .map(GeneralizedLowRankFitError.Generalized.apply)
      _ <-
        if closeObjective(objective.total, palmFit.objective) then Right(())
        else
          Left(
            GeneralizedLowRankFitError.InvalidDefinition(
              s"fitted GLRM objective ${objective.total} does not bind PALM objective ${palmFit.objective}"
            )
          )
      encoder <- FittedLatentEncoder
        .from(
          factors.decoder,
          compiled.program.objective.factorPenalties.filter(_.target == GlrmFactorTarget.RowCodes)
        )
        .left
        .map(GeneralizedLowRankFitError.Encoding.apply)
      payload = new GeneralizedLowRankFitPayload(
        factors,
        objective,
        palmFit.receipt,
        encoder,
        compiled.program.objective.observations.valueIdentity,
        compiled.program.objective.programIdentity,
        palmFit.resultIdentity
      )
      resultId = ResultId.from[GeneralizedLowRankFamily](palmFit.resultIdentity)
      binding = FitBinding.from(
        compiled.program.id,
        compiled.id,
        compiled.executionProgramIdentity,
        compiled.program.objective.observations.valueIdentity,
        scope,
        resultId
      )
      receipt = GeneralizedLowRankPalmReceipt.from(
        binding,
        compiled.solverPlan,
        compiled.problem,
        compiled.initialization,
        palmFit
      )
      certificates <- NonEmptyCertificates
        .from(Vector(palmFit.certificate))
        .left
        .map(GeneralizedLowRankFitError.Lifecycle.apply)
      solver <- SolverEvidence
        .from(
          binding,
          compiled.solverPlan,
          receipt,
          palmFit.achievement,
          certificates,
          compiled.provenance
        )
        .left
        .map(GeneralizedLowRankFitError.Lifecycle.apply)
      fitted <- FittedModel
        .from(
          compiled.program,
          compiled,
          payload,
          palmFit.resultIdentity,
          binding,
          solver,
          scope,
          compiled.provenance.append(
            SemanticProvenanceEvent.Certified("solver-trace", "portable-glrm-palm-v1")
          )
        )
        .left
        .map(GeneralizedLowRankFitError.Lifecycle.apply)
    yield fitted

  def fit[
      Rows <: SemanticSpace,
      Feature <: SemanticSpace,
      Latent <: SemanticSpace
  ](
      objective: GeneralizedLowRankProgram[Rows, Feature],
      initial: GlrmFactors[Rows, Feature, Latent],
      config: PalmConfig = PalmConfig.portable
  ): Either[GeneralizedLowRankFitError, GeneralizedLowRankFit[Rows, Feature, Latent]] =
    for
      program <- declare(objective)
      compiled <- compile(program, initial)
      fitted <- solve(
        compiled,
        TrainingScopeId.standalone(objective.observations.valueIdentity),
        config
      )
    yield fitted

  private def validateLossCapabilities[
      Rows <: SemanticSpace,
      Feature <: SemanticSpace
  ](
      program: GeneralizedLowRankProgram[Rows, Feature]
  ): Either[GeneralizedLowRankFitError, Unit] =
    program.layout.features.foldLeft[Either[GeneralizedLowRankFitError, Unit]](Right(())): (result, feature) =>
      result.flatMap: _ =>
        feature.loss.naturalDomain(feature.domain) match
          case NaturalParameterDomain.OrderedNonIncreasing(_) =>
            Left(
              GeneralizedLowRankFitError.UnsupportedLoss(
                feature.id,
                feature.loss,
                "the portable PALM adapter has no ordered-natural-parameter projection"
              )
            )
          case NaturalParameterDomain.Unconstrained(_) =>
            feature.loss.curvatureUpperBound match
              case None =>
                Left(
                  GeneralizedLowRankFitError.UnsupportedLoss(
                    feature.id,
                    feature.loss,
                    "no level-set-uniform curvature bound is available"
                  )
                )
              case Some(_) => Right(())

  private def admittedPenalties(
      terms: Vector[GlrmFactorPenaltyTerm]
  ): Either[GeneralizedLowRankFitError, AdmittedGlrmPenalties] =
    val row = penaltyWeights(terms, GlrmFactorTarget.RowCodes)
    val decoder = penaltyWeights(terms, GlrmFactorTarget.FeatureDecoder)
    if !row.isCoercive then
      Left(
        GeneralizedLowRankFitError.MissingCapability(
          "portable GLRM PALM requires a positive L1 or squared-Frobenius row-code penalty to certify bounded iterates"
        )
      )
    else if !decoder.isCoercive then
      Left(
        GeneralizedLowRankFitError.MissingCapability(
          "portable GLRM PALM requires a positive L1 or squared-Frobenius decoder penalty to certify bounded iterates"
        )
      )
    else Right(AdmittedGlrmPenalties(row, decoder))

  private def observedEntries[
      Rows <: SemanticSpace,
      Feature <: SemanticSpace
  ](
      program: GeneralizedLowRankProgram[Rows, Feature]
  ): Either[GeneralizedLowRankFitError, Vector[GlrmPalmEntry]] =
    var entries = Vector.empty[GlrmPalmEntry]
    var row = 0
    var failure = Option.empty[GeneralizedLowRankFitError]
    while row < program.observations.rowSpace.dimension && failure.isEmpty do
      var feature = 0
      while feature < program.observations.featureSpace.dimension && failure.isEmpty do
        program.observations.cellUnsafe(row, feature) match
          case ObservationCell.Observed(value, weight) =>
            val specification = program.layout.featureUnsafe(feature)
            FeatureEmbedding.from(specification.id, specification.domain, row, value) match
              case Left(error) => failure = Some(GeneralizedLowRankFitError.Generalized(error))
              case Right(embedding) =>
                specification.loss.curvatureUpperBound match
                  case None =>
                    failure = Some(
                      GeneralizedLowRankFitError.UnsupportedLoss(
                        specification.id,
                        specification.loss,
                        "no level-set-uniform curvature bound is available"
                      )
                    )
                  case Some(curvature) =>
                    entries :+= GlrmPalmEntry(row, feature, specification, embedding, weight.value, curvature)
          case ObservationCell.Censored(interval, _) =>
            failure = Some(
              GeneralizedLowRankFitError.Generalized(
                GeneralizedLowRankError.UnsupportedCensoring(row, feature, interval)
              )
            )
          case ObservationCell.Missing(_) | ObservationCell.StructurallyInapplicable(_) => ()
        feature += 1
      row += 1
    failure.toLeft(entries)

  private def initialState[
      Rows <: SemanticSpace,
      Feature <: SemanticSpace,
      Latent <: SemanticSpace
  ](
      factors: GlrmFactors[Rows, Feature, Latent]
  ): Either[PalmConvergenceError, PalmState] =
    for
      rows <- PalmBlockValue.from(rowParameter, factors.rowCodes.values, factors.rowCodes.valueIdentity)
      decoder <- PalmBlockValue.from(decoderParameter, factors.decoder.values, factors.decoder.valueIdentity)
      state <- PalmState.from(Vector(rows, decoder))
    yield state

  private def blockOracle[
      Rows <: SemanticSpace,
      Feature <: SemanticSpace,
      Latent <: SemanticSpace
  ](
      program: GeneralizedLowRankModelProgram[Rows, Feature],
      latent: SpaceEvidence[Latent],
      entries: Vector[GlrmPalmEntry],
      parameter: ParameterId,
      functional: ValueIdentity,
      smoothness: SmoothnessConstant,
      penalties: GlrmPenaltyWeights
  ): PalmBlockOracle =
    val assumptions = PalmBlockAssumptions(
      parameter,
      functional,
      assumption("proper-factor-penalties"),
      smoothness,
      assumption("block-lipschitz-gradient")
    )
    PalmBlockOracle.from(assumptions)(
      update = (state, iteration) =>
        proximalPoint(program, latent, entries, state, parameter, penalties, smoothness.doubleValue)
          .flatMap: next =>
            PalmBlockUpdate
              .from(
                next,
                ValueIdentity.derived(
                  s"${parameter.value}-palm-update-$iteration",
                  program.objective.programIdentity
                ),
                PalmBlockSolveKind.Exact,
                subproblemResidual = 0.0,
                normalizationResidual = 0.0,
                inexactness = 0.0
              )
              .left
              .map(_.message),
      stationarity = state =>
        for
          current <- state.block(parameter).left.map(_.message)
          next <- proximalPoint(program, latent, entries, state, parameter, penalties, smoothness.doubleValue)
        yield scaledDistance(current.values, next, smoothness.doubleValue),
      normalization = _ => Right(0.0)
    )

  private def proximalPoint[
      Rows <: SemanticSpace,
      Feature <: SemanticSpace,
      Latent <: SemanticSpace
  ](
      program: GeneralizedLowRankModelProgram[Rows, Feature],
      latent: SpaceEvidence[Latent],
      entries: Vector[GlrmPalmEntry],
      state: PalmState,
      parameter: ParameterId,
      penalties: GlrmPenaltyWeights,
      smoothness: Double
  ): Either[String, DMat] =
    for
      current <- state.block(parameter).left.map(_.message)
      gradient <- smoothGradient(program, latent, entries, state, parameter)
    yield
      val values = new Array[Double](current.values.rows * current.values.cols)
      val step = 1.0 / smoothness
      var row = 0
      while row < current.values.rows do
        var column = 0
        while column < current.values.cols do
          val forward = current.values(row, column) - step * gradient(row, column)
          val thresholded =
            if forward > step * penalties.l1 then forward - step * penalties.l1
            else if forward < -step * penalties.l1 then forward + step * penalties.l1
            else 0.0
          values(row * current.values.cols + column) = thresholded / (1.0 + step * penalties.ridge)
          column += 1
        row += 1
      GaleNumerics.matrixFromRowMajor(current.values.rows, current.values.cols, values)

  private def smoothGradient[
      Rows <: SemanticSpace,
      Feature <: SemanticSpace,
      Latent <: SemanticSpace
  ](
      program: GeneralizedLowRankModelProgram[Rows, Feature],
      latent: SpaceEvidence[Latent],
      entries: Vector[GlrmPalmEntry],
      state: PalmState,
      parameter: ParameterId
  ): Either[String, DMat] =
    for
      rowBlock <- state.block(rowParameter).left.map(_.message)
      decoderBlock <- state.block(decoderParameter).left.map(_.message)
      _ <-
        if parameter == rowParameter || parameter == decoderParameter then Right(())
        else Left(s"unknown GLRM PALM parameter '${parameter.value}'")
      gradient <- entryGradient(
        program.objective.layout,
        latent.dimension,
        entries,
        rowBlock.values,
        decoderBlock.values,
        parameter
      )
    yield gradient

  private def entryGradient[Feature <: SemanticSpace](
      layout: GlrmFeatureLayout[Feature],
      latentDimension: Int,
      entries: Vector[GlrmPalmEntry],
      rowCodes: DMat,
      decoder: DMat,
      parameter: ParameterId
  ): Either[String, DMat] =
    val rows = if parameter == rowParameter then rowCodes.rows else decoder.rows
    val columns = if parameter == rowParameter then rowCodes.cols else decoder.cols
    val result = Array.fill(rows * columns)(0.0)
    var index = 0
    var failure = Option.empty[String]
    while index < entries.length && failure.isEmpty do
      val entry = entries(index)
      val offset = layout.offset(entry.feature)
      val width = layout.width(entry.feature)
      val natural = new Array[Double](width)
      var output = 0
      while output < width do
        var value = 0.0
        var latent = 0
        while latent < latentDimension do
          value += rowCodes(entry.row, latent) * decoder(latent, offset + output)
          latent += 1
        natural(output) = value
        output += 1
      entry.specification.loss
        .naturalGradient(entry.specification.id, entry.embedding, GaleNumerics.vectorFromArray(natural)) match
        case Left(error) => failure = Some(error.message)
        case Right(naturalGradient) =>
          output = 0
          while output < width do
            val weighted = entry.weight * naturalGradient(output)
            var latent = 0
            while latent < latentDimension do
              if parameter == rowParameter then
                result(entry.row * columns + latent) += weighted * decoder(latent, offset + output)
              else
                result(latent * columns + offset + output) += weighted * rowCodes(entry.row, latent)
              latent += 1
            output += 1
      index += 1
    failure match
      case Some(detail) => Left(detail)
      case None => Right(GaleNumerics.matrixFromRowMajor(rows, columns, result))

  private def factorsFromState[
      Rows <: SemanticSpace,
      Feature <: SemanticSpace,
      Latent <: SemanticSpace
  ](
      program: GeneralizedLowRankModelProgram[Rows, Feature],
      latent: SpaceEvidence[Latent],
      state: PalmState
  ): Either[String, GlrmFactors[Rows, Feature, Latent]] =
    for
      rows <- state.block(rowParameter).left.map(_.message)
      decoder <- state.block(decoderParameter).left.map(_.message)
      rowCodes <- GlrmRowCodes
        .from(program.objective.observations.rowSpace, latent, rows.values, rows.valueIdentity)
        .left
        .map(_.message)
      featureDecoder <- FeatureDecoder
        .from(program.objective.layout, latent, decoder.values, decoder.valueIdentity)
        .left
        .map(_.message)
      factors <- GlrmFactors.from(rowCodes, featureDecoder).left.map(_.message)
    yield factors

  private def blockSmoothnessBound(
      entries: Vector[GlrmPalmEntry],
      initialObjective: Double,
      oppositePenalty: GlrmPenaltyWeights
  ): Double =
    val curvature = entries.map(entry => entry.weight * entry.curvature).max
    val oppositeNormSquared = oppositePenalty.normSquaredBound(initialObjective)
    Math.max(1e-12, stepSafety * curvature * oppositeNormSquared)

  private def penaltyWeights(
      terms: Vector[GlrmFactorPenaltyTerm],
      target: GlrmFactorTarget
  ): GlrmPenaltyWeights =
    terms.filter(_.target == target).foldLeft(GlrmPenaltyWeights(0.0, 0.0)): (weights, term) =>
      term.functional match
        case GlrmFactorPenalty.ElementwiseL1 => weights.copy(l1 = weights.l1 + term.weight.value)
        case GlrmFactorPenalty.SquaredFrobenius => weights.copy(ridge = weights.ridge + term.weight.value)

  private def klEvidence(
      objective: ValueIdentity,
      entries: Vector[GlrmPalmEntry]
  ): PalmKlEvidence =
    val usesLogExp = entries.exists: entry =>
      entry.specification.loss match
        case EntryLoss.Logistic | EntryLoss.CategoricalCrossEntropy => true
        case _ => false
    if usesLogExp then
      PalmKlEvidence.LogExpDefinable(
        objective,
        assumption("kl-objective"),
        "quadratic, Huber, logistic, softmax, L1, and squared-norm terms are definable in the log-exp structure"
      )
    else
      PalmKlEvidence.SemiAlgebraic(
        objective,
        assumption("kl-objective"),
        "quadratic, Huber, L1, and squared-norm terms are semi-algebraic"
      )

  private def observationMask[Rows <: SemanticSpace, Feature <: SemanticSpace](
      observations: ObservationPattern[Rows, Feature]
  ): ObservationMaskIdentity =
    if observations.isPointComplete then ObservationMaskIdentity.Complete
    else ObservationMaskIdentity.Observed(observations.valueIdentity)

  private def assumption(value: String): ContractReference[AssumptionReference] =
    ContractReference.unsafeAssumption(value)

  private def scaledDistance(left: DMat, right: DMat, scale: Double): Double =
    var sum = 0.0
    var row = 0
    while row < left.rows do
      var column = 0
      while column < left.cols do
        val difference = left(row, column) - right(row, column)
        sum += difference * difference
        column += 1
      row += 1
    scale * Math.sqrt(sum)

  private def closeObjective(left: Double, right: Double): Boolean =
    Math.abs(left - right) <= 1e-10 * Math.max(1.0, Math.max(Math.abs(left), Math.abs(right)))

extension [Rows <: SemanticSpace, Feature <: SemanticSpace](
    program: GeneralizedLowRankProgram[Rows, Feature]
)
  def fit[Latent <: SemanticSpace](
      initial: GlrmFactors[Rows, Feature, Latent],
      config: PalmConfig = PalmConfig.portable
  ): Either[GeneralizedLowRankFitError, GeneralizedLowRankFit[Rows, Feature, Latent]] =
    GeneralizedLowRankLifecycle.fit(program, initial, config)

given generalizedLowRankCanEncode[
    Rows <: SemanticSpace,
    Feature <: SemanticSpace,
    Latent <: SemanticSpace,
    NewRows <: SemanticSpace
]: CanEncode[
  GeneralizedLowRankFit[Rows, Feature, Latent],
  ObservationPattern[NewRows, Feature]
] with
  type Output = FittedLatentEncoding[Latent]

  def encode(
      fit: GeneralizedLowRankFit[Rows, Feature, Latent],
      input: ObservationPattern[NewRows, Feature]
  ): Either[ModelLifecycleError, FittedLatentEncoding[Latent]] =
    fit.payload.encoder
      .encode(input)
      .left
      .map(error => ModelLifecycleError.InvalidDefinition(error.message))

private final case class GlrmPalmEntry(
    row: Int,
    feature: Int,
    specification: GlrmFeatureSpec,
    embedding: FeatureEmbedding,
    weight: Double,
    curvature: Double
)

private final case class GlrmPenaltyWeights(l1: Double, ridge: Double):
  def isCoercive: Boolean = l1 > 0.0 || ridge > 0.0

  def coercivityWeight: Double =
    if ridge > 0.0 then ridge else l1

  def normSquaredBound(objectiveUpperBound: Double): Double =
    val ridgeBound =
      if ridge > 0.0 then 2.0 * objectiveUpperBound / ridge
      else Double.PositiveInfinity
    val l1Bound =
      if l1 > 0.0 then
        val radius = objectiveUpperBound / l1
        radius * radius
      else Double.PositiveInfinity
    Math.min(ridgeBound, l1Bound)

private final case class AdmittedGlrmPenalties(
    row: GlrmPenaltyWeights,
    decoder: GlrmPenaltyWeights
)
