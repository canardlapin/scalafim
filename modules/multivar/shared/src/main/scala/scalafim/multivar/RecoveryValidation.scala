package scalafim.multivar

import gale.linalg.DMat

enum RecoveryValidationError:
  case InvalidDefinition(detail: String)
  case MissingFoldComponent(component: FoldFittedComponent)
  case DuplicateFoldComponent(component: FoldFittedComponent)
  case LifecycleMismatch(component: FoldFittedComponent, detail: String)
  case MissingHyperparameter(component: FoldFittedComponent, candidate: CandidateId, name: String)
  case MissingSimulationScenarios(missing: Set[RecoverySimulationScenario])
  case ClaimRequiresTheorem(kind: ValidationClaimKind)
  case Multivar(error: MultivarError)

  def message: String =
    this match
      case InvalidDefinition(detail) => detail
      case MissingFoldComponent(component) => s"fold-safety manifest is missing $component"
      case DuplicateFoldComponent(component) => s"fold-safety manifest contains $component more than once"
      case LifecycleMismatch(component, detail) => s"fold-fitted $component is invalid: $detail"
      case MissingHyperparameter(component, candidate, name) =>
        s"fold-fitted $component requires hyperparameter '$name' in candidate '${candidate.stringValue}'"
      case MissingSimulationScenarios(missing) =>
        s"recovery validation is missing simulation scenarios: ${missing.toVector.sortBy(_.toString).mkString(", ")}"
      case ClaimRequiresTheorem(kind) => s"$kind requires a named theorem witness; empirical evidence is insufficient"
      case Multivar(error) => error.message

enum ResamplingUnit:
  case Rows
  case Columns
  case Entries
  case Groups
  case Sites
  case Errors

final case class ResamplingDesign private (
    unit: ResamplingUnit,
    membershipIdentity: Option[ValueIdentity],
    description: String,
    valueIdentity: ValueIdentity
)

object ResamplingDesign:
  def from(
      unit: ResamplingUnit,
      membershipIdentity: Option[ValueIdentity],
      description: String
  ): Either[RecoveryValidationError, ResamplingDesign] =
    val clean = description.trim
    val requiresMembership = Set(ResamplingUnit.Groups, ResamplingUnit.Sites, ResamplingUnit.Errors).contains(unit)
    if clean.isEmpty then Left(RecoveryValidationError.InvalidDefinition("resampling description must be non-empty"))
    else if requiresMembership && membershipIdentity.isEmpty then
      Left(RecoveryValidationError.InvalidDefinition(s"$unit resampling requires an explicit membership identity"))
    else if !requiresMembership && membershipIdentity.nonEmpty then
      Left(RecoveryValidationError.InvalidDefinition(s"$unit resampling does not accept an implicit grouping identity"))
    else
      Right(
        ResamplingDesign(
          unit,
          membershipIdentity,
          clean,
          ValueIdentity.derived(s"${unit.toString.toLowerCase}-resampling", membershipIdentity.toVector*)
        )
      )

enum ValidationMissingnessTarget:
  /** Predict held-out entries under one fixed, observed mask. No stochastic
    * missingness mechanism is asserted.
    */
  case FixedMaskPrediction(mask: ValueIdentity)

  /** Performance under a declared synthetic independent-deletion generator. */
  case McarSimulation(generator: ValueIdentity, deletionProbability: Double)

  /** Sensitivity analysis under an explicit observed-data-dependent mechanism. */
  case MarSensitivity(mechanism: ValueIdentity, rationale: ObservationReason)

  /** Sensitivity analysis under an explicit nonignorable selection model. */
  case MnarSensitivity(selectionModel: ValueIdentity, rationale: ObservationReason)

  def inferentialClaimGranted: Boolean = false

object ValidationMissingnessTarget:
  def mcar(
      generator: ValueIdentity,
      deletionProbability: Double
  ): Either[RecoveryValidationError, ValidationMissingnessTarget] =
    if deletionProbability.isFinite && deletionProbability > 0.0 && deletionProbability < 1.0 then
      Right(ValidationMissingnessTarget.McarSimulation(generator, deletionProbability))
    else
      Left(
        RecoveryValidationError.InvalidDefinition(
          s"MCAR deletion probability must be finite and strictly between zero and one, got $deletionProbability"
        )
      )

enum FoldFittedComponent:
  case Offsets
  case Scaling
  case LossBalancing
  case Graph
  case Encoding
  case Rank
  case Penalties

  private[multivar] def allowedStages: Set[LifecycleStage] =
    this match
      case Offsets | Scaling => Set(LifecycleStage.Preprocessing)
      case LossBalancing => Set(LifecycleStage.StatisticalEstimation, LifecycleStage.OperatorPolicy)
      case Graph => Set(LifecycleStage.GraphEstimation)
      case Encoding => Set(LifecycleStage.ChartEstimation, LifecycleStage.StatisticalEstimation)
      case Rank => Set(LifecycleStage.ProgramBuild)
      case Penalties => Set(LifecycleStage.ProgramBuild, LifecycleStage.Lowering)

object FoldFittedComponent:
  val all: Set[FoldFittedComponent] = Set(Offsets, Scaling, LossBalancing, Graph, Encoding, Rank, Penalties)

final case class FoldComponentBinding private (
    component: FoldFittedComponent,
    stage: LifecycleStage,
    artifact: String,
    hyperparameters: Set[String]
)

object FoldComponentBinding:
  def from(
      component: FoldFittedComponent,
      stage: LifecycleStage,
      artifact: String,
      hyperparameters: Set[String] = Set.empty
  ): Either[RecoveryValidationError, FoldComponentBinding] =
    val cleanArtifact = artifact.trim
    val cleanHyperparameters = hyperparameters.map(_.trim)
    if !component.allowedStages.contains(stage) then
      Left(RecoveryValidationError.LifecycleMismatch(component, s"stage $stage is not admitted"))
    else if cleanArtifact.isEmpty then
      Left(RecoveryValidationError.LifecycleMismatch(component, "artifact must be non-empty"))
    else if cleanHyperparameters.exists(_.isEmpty) then
      Left(RecoveryValidationError.LifecycleMismatch(component, "hyperparameter names must be non-empty"))
    else Right(FoldComponentBinding(component, stage, cleanArtifact, cleanHyperparameters))

final class FoldSafetyManifest private (
    val specId: ModelSpecId,
    val bindings: Vector[FoldComponentBinding],
    val valueIdentity: ValueIdentity
):
  def binding(component: FoldFittedComponent): FoldComponentBinding =
    bindings.find(_.component == component).getOrElse(
      throw new IllegalStateException(s"complete fold-safety manifest lost $component")
    )

object FoldSafetyManifest:
  def from(
      spec: ModelSpec,
      bindings: Vector[FoldComponentBinding]
  ): Either[RecoveryValidationError, FoldSafetyManifest] =
    val grouped = bindings.groupBy(_.component)
    FoldFittedComponent.all.find(component => !grouped.contains(component)) match
      case Some(component) => Left(RecoveryValidationError.MissingFoldComponent(component))
      case None =>
        grouped.collectFirst { case (component, values) if values.length != 1 => component } match
          case Some(component) => Left(RecoveryValidationError.DuplicateFoldComponent(component))
          case None =>
            traverseRecovery(bindings)(validateBinding(spec, _)).map: _ =>
              new FoldSafetyManifest(
                spec.id,
                bindings,
                ValueIdentity.derived(
                  s"fold-safety-${spec.id.stringValue}",
                  bindings.map(binding => ValueIdentity.source(ValueId.unsafe(s"fold.${binding.artifact}")))*
                )
              )

  private def validateBinding(
      spec: ModelSpec,
      binding: FoldComponentBinding
  ): Either[RecoveryValidationError, Unit] =
    binding.component match
      case FoldFittedComponent.Offsets =>
        spec.preprocessing match
          case PreprocessSpec.Center | PreprocessSpec.Standardize => Right(())
          case other => Left(RecoveryValidationError.LifecycleMismatch(binding.component, s"$other does not fit offsets"))
      case FoldFittedComponent.Scaling =>
        spec.preprocessing match
          case PreprocessSpec.Standardize => Right(())
          case other =>
            Left(
              RecoveryValidationError.LifecycleMismatch(
                binding.component,
                s"$other does not estimate scaling from training rows"
              )
            )
      case _ =>
        val planned = spec.lifecyclePlans.exists(plan => plan.stage == binding.stage && plan.artifact == binding.artifact)
        if !planned then
          Left(
            RecoveryValidationError.LifecycleMismatch(
              binding.component,
              s"ModelSpec does not declare ${binding.stage} artifact '${binding.artifact}'"
            )
          )
        else
          binding.hyperparameters.toVector.foldLeft[Either[RecoveryValidationError, Unit]](Right(())):
            (result, hyperparameter) =>
              result.flatMap: _ =>
                spec.candidates.find(candidate => !candidate.values.map(_._1).contains(hyperparameter)) match
                  case Some(candidate) =>
                    Left(
                      RecoveryValidationError.MissingHyperparameter(
                        binding.component,
                        candidate.id,
                        hyperparameter
                      )
                    )
                  case None => Right(())

enum WarmStartResetPolicy:
  case ResetAtEveryFold

final case class WarmStartEdge(
    from: CandidateId,
    to: CandidateId,
    split: SplitIdentity,
    seed: DeterministicSeed,
    stateIdentity: ValueIdentity
)

final class DeterministicWarmStartPath private (
    val split: SplitIdentity,
    val candidates: Vector[CandidateId],
    val resetPolicy: WarmStartResetPolicy,
    val edges: Vector[WarmStartEdge],
    val valueIdentity: ValueIdentity
)

object DeterministicWarmStartPath:
  def from(
      spec: ModelSpec,
      split: SplitIdentity,
      orderedCandidates: Vector[CandidateId]
  ): Either[RecoveryValidationError, DeterministicWarmStartPath] =
    val expected = spec.candidates.map(_.id)
    if orderedCandidates != expected then
      Left(
        RecoveryValidationError.InvalidDefinition(
          "warm-start path must preserve the exact deterministic ModelSpec candidate order"
        )
      )
    else
      val edges = orderedCandidates.sliding(2).toVector.collect:
        case Vector(from, to) =>
          val seed = spec.baseSeed.derive(split.stringValue, from.stringValue, to.stringValue)
          WarmStartEdge(
            from,
            to,
            split,
            seed,
            ValueIdentity.source(
              ValueId.unsafe(s"warm.${split.stringValue}.${from.stringValue}.${to.stringValue}.${seed.intValue}")
            )
          )
      Right(
        new DeterministicWarmStartPath(
          split,
          orderedCandidates,
          WarmStartResetPolicy.ResetAtEveryFold,
          edges,
          ValueIdentity.derived("deterministic-warm-start-path", edges.map(_.stateIdentity)*)
        )
      )

enum RecoverySimulationScenario:
  case SparseSmoothSignal
  case DisconnectedGraph
  case PMuchGreaterThanN
  case CorrelatedNoise
  case WeakEigengap
  case BlockImbalance
  case MisspecifiedGraph

object RecoverySimulationScenario:
  val all: Set[RecoverySimulationScenario] = Set(
    SparseSmoothSignal,
    DisconnectedGraph,
    PMuchGreaterThanN,
    CorrelatedNoise,
    WeakEigengap,
    BlockImbalance,
    MisspecifiedGraph
  )

enum RecoverySimulationDesign:
  case SparseSmoothSignal(sampleCount: Int, featureCount: Int, nonzeroCount: Int, truthRoughness: Double)
  case DisconnectedGraph(vertexCount: Int, connectedComponents: Int)
  case PMuchGreaterThanN(sampleCount: Int, featureCount: Int)
  case CorrelatedNoise(correlation: Double)
  case WeakEigengap(leadingScale: Double, eigengap: Double)
  case BlockImbalance(smallestBlock: Int, largestBlock: Int)
  case MisspecifiedGraph(truthEdges: Int, fittedEdges: Int, commonEdges: Int)

  def scenario: RecoverySimulationScenario =
    this match
      case SparseSmoothSignal(_, _, _, _) => RecoverySimulationScenario.SparseSmoothSignal
      case DisconnectedGraph(_, _) => RecoverySimulationScenario.DisconnectedGraph
      case PMuchGreaterThanN(_, _) => RecoverySimulationScenario.PMuchGreaterThanN
      case CorrelatedNoise(_) => RecoverySimulationScenario.CorrelatedNoise
      case WeakEigengap(_, _) => RecoverySimulationScenario.WeakEigengap
      case BlockImbalance(_, _) => RecoverySimulationScenario.BlockImbalance
      case MisspecifiedGraph(_, _, _) => RecoverySimulationScenario.MisspecifiedGraph

  private[multivar] def validate: Either[RecoveryValidationError, Unit] =
    this match
      case SparseSmoothSignal(samples, features, nonzero, roughness) =>
        if samples > 0 && features > 1 && nonzero > 0 && nonzero < features && roughness.isFinite && roughness >= 0.0 then
          Right(())
        else Left(RecoveryValidationError.InvalidDefinition("sparse-smooth design requires positive dimensions, strict sparsity, and finite roughness"))
      case DisconnectedGraph(vertices, components) =>
        if vertices > 1 && components >= 2 && components <= vertices then Right(())
        else Left(RecoveryValidationError.InvalidDefinition("disconnected-graph design requires 2 <= components <= vertices"))
      case PMuchGreaterThanN(samples, features) =>
        if samples > 0 && features >= 10 * samples then Right(())
        else Left(RecoveryValidationError.InvalidDefinition("p-much-greater-than-n design requires p >= 10 n"))
      case CorrelatedNoise(correlation) =>
        if correlation.isFinite && Math.abs(correlation) >= 0.3 && Math.abs(correlation) < 1.0 then Right(())
        else Left(RecoveryValidationError.InvalidDefinition("correlated-noise design requires 0.3 <= |rho| < 1"))
      case WeakEigengap(scale, gap) =>
        if scale.isFinite && scale > 0.0 && gap.isFinite && gap >= 0.0 && gap <= 0.05 * scale then Right(())
        else Left(RecoveryValidationError.InvalidDefinition("weak-eigengap design requires gap <= 5% of the leading scale"))
      case BlockImbalance(smallest, largest) =>
        if smallest > 0 && largest >= 10 * smallest then Right(())
        else Left(RecoveryValidationError.InvalidDefinition("block-imbalance design requires a largest/smallest ratio of at least ten"))
      case MisspecifiedGraph(truthEdges, fittedEdges, commonEdges) =>
        val validCounts = truthEdges > 0 && fittedEdges > 0 && commonEdges >= 0 &&
          commonEdges <= Math.min(truthEdges, fittedEdges)
        if validCounts && commonEdges.toDouble / truthEdges.toDouble <= 0.5 then Right(())
        else Left(RecoveryValidationError.InvalidDefinition("misspecified-graph design requires valid edge counts and at most 50% truth-edge overlap"))

final case class RecoverySimulationCase private (
    design: RecoverySimulationDesign,
    seed: DeterministicSeed,
    generatorIdentity: ValueIdentity,
    description: String
):
  def scenario: RecoverySimulationScenario = design.scenario

object RecoverySimulationCase:
  def from(
      design: RecoverySimulationDesign,
      seed: DeterministicSeed,
      generatorIdentity: ValueIdentity,
      description: String
  ): Either[RecoveryValidationError, RecoverySimulationCase] =
    if description.trim.isEmpty then Left(RecoveryValidationError.InvalidDefinition("simulation description must be non-empty"))
    else design.validate.map(_ => RecoverySimulationCase(design, seed, generatorIdentity, description.trim))

final class RecoverySimulationCoverage private (
    val cases: Vector[RecoverySimulationCase],
    val valueIdentity: ValueIdentity
)

object RecoverySimulationCoverage:
  def from(cases: Vector[RecoverySimulationCase]): Either[RecoveryValidationError, RecoverySimulationCoverage] =
    val covered = cases.map(_.scenario).toSet
    val missing = RecoverySimulationScenario.all.diff(covered)
    if missing.nonEmpty then Left(RecoveryValidationError.MissingSimulationScenarios(missing))
    else if cases.map(current => current.scenario -> current.seed.intValue).distinct.length != cases.length then
      Left(RecoveryValidationError.InvalidDefinition("simulation scenario and seed pairs must be distinct"))
    else
      Right(
        new RecoverySimulationCoverage(
          cases,
          ValueIdentity.derived("recovery-simulation-coverage", cases.map(_.generatorIdentity)*)
        )
      )

final case class SupportRecoveryMetrics(
    truePositive: Int,
    falsePositive: Int,
    falseNegative: Int,
    precision: Double,
    recall: Double
):
  require(truePositive >= 0 && falsePositive >= 0 && falseNegative >= 0)
  require(precision.isFinite && precision >= 0.0 && precision <= 1.0)
  require(recall.isFinite && recall >= 0.0 && recall <= 1.0)

object SupportRecoveryMetrics:
  def from(estimated: Set[Int], truth: Set[Int]): SupportRecoveryMetrics =
    val truePositive = estimated.intersect(truth).size
    val falsePositive = estimated.diff(truth).size
    val falseNegative = truth.diff(estimated).size
    val precision =
      if estimated.isEmpty then if truth.isEmpty then 1.0 else 0.0
      else truePositive.toDouble / estimated.size.toDouble
    val recall =
      if truth.isEmpty then 1.0
      else truePositive.toDouble / truth.size.toDouble
    SupportRecoveryMetrics(truePositive, falsePositive, falseNegative, precision, recall)

final case class RecoveryMetrics(
    subspaceProjectorError: Double,
    alignedFactorError: Double,
    support: SupportRecoveryMetrics,
    heldOutRisk: Double,
    roughness: Double,
    stability: Double,
    blockCalibrationError: Double
):
  require(subspaceProjectorError.isFinite && subspaceProjectorError >= 0.0)
  require(alignedFactorError.isFinite && alignedFactorError >= 0.0)
  require(heldOutRisk.isFinite && heldOutRisk >= 0.0)
  require(roughness.isFinite && roughness >= 0.0)
  require(stability.isFinite && stability >= 0.0 && stability <= 1.0)
  require(blockCalibrationError.isFinite && blockCalibrationError >= 0.0)

object RecoveryMetrics:
  def from(
      estimatedProjector: DMat,
      truthProjector: DMat,
      estimatedFactors: DMat,
      truthFactors: DMat,
      estimatedSupport: Set[Int],
      truthSupport: Set[Int],
      heldOutLosses: Vector[Double],
      roughnessOperator: DMat,
      selectedSupports: Vector[Set[Int]],
      estimatedBlockContributions: Vector[Double],
      truthBlockContributions: Vector[Double]
  ): Either[RecoveryValidationError, RecoveryMetrics] =
    if estimatedProjector.rows != truthProjector.rows || estimatedProjector.cols != truthProjector.cols then
      Left(RecoveryValidationError.InvalidDefinition("subspace projectors must have equal shapes"))
    else if estimatedProjector.rows != estimatedProjector.cols then
      Left(RecoveryValidationError.InvalidDefinition("subspace projectors must be square"))
    else if estimatedFactors.rows != truthFactors.rows || estimatedFactors.cols != truthFactors.cols then
      Left(RecoveryValidationError.InvalidDefinition("estimated and true factors must have equal shapes"))
    else if heldOutLosses.isEmpty || heldOutLosses.exists(value => !value.isFinite || value < 0.0) then
      Left(RecoveryValidationError.InvalidDefinition("held-out losses must be non-empty, finite, and non-negative"))
    else if roughnessOperator.cols != estimatedFactors.rows then
      Left(RecoveryValidationError.InvalidDefinition("roughness operator columns must match factor rows"))
    else
      blockCalibration(estimatedBlockContributions, truthBlockContributions).map: calibration =>
        RecoveryMetrics(
          frobeniusDistance(estimatedProjector, truthProjector),
          alignedColumnError(estimatedFactors, truthFactors),
          SupportRecoveryMetrics.from(estimatedSupport, truthSupport),
          heldOutLosses.sum / heldOutLosses.length.toDouble,
          0.5 * squaredFrobenius(GaleNumerics.multiply(roughnessOperator, estimatedFactors)),
          meanPairwiseJaccard(selectedSupports),
          calibration
        )

enum ValidationClaimKind:
  case PredictiveRisk
  case DescriptiveStability
  case SupportRecovery
  case Inferential

enum ValidationClaimEvidence:
  case EmpiricalOnly(design: ValueIdentity)
  case TheoremBacked(
      theorem: ContractReference[TheoremReference],
      assumptions: Set[ContractReference[AssumptionReference]],
      witness: ValueIdentity
  )

final case class ValidationClaim private (
    kind: ValidationClaimKind,
    statement: String,
    evidence: ValidationClaimEvidence
)

object ValidationClaim:
  def from(
      kind: ValidationClaimKind,
      statement: String,
      evidence: ValidationClaimEvidence
  ): Either[RecoveryValidationError, ValidationClaim] =
    if statement.trim.isEmpty then Left(RecoveryValidationError.InvalidDefinition("validation claim must be non-empty"))
    else
      (kind, evidence) match
        case (ValidationClaimKind.SupportRecovery | ValidationClaimKind.Inferential, ValidationClaimEvidence.EmpiricalOnly(_)) =>
          Left(RecoveryValidationError.ClaimRequiresTheorem(kind))
        case (_, ValidationClaimEvidence.TheoremBacked(_, assumptions, _)) if assumptions.isEmpty =>
          Left(RecoveryValidationError.InvalidDefinition("theorem-backed validation claim requires stated assumptions"))
        case _ => Right(ValidationClaim(kind, statement.trim, evidence))

final case class ScenarioRecoveryResult(
    scenario: RecoverySimulationScenario,
    seed: DeterministicSeed,
    metrics: RecoveryMetrics,
    resultIdentity: ValueIdentity
)

final class RecoveryValidationReport private (
    val resampling: ResamplingDesign,
    val missingness: ValidationMissingnessTarget,
    val foldSafety: FoldSafetyManifest,
    val warmStarts: Vector[DeterministicWarmStartPath],
    val simulations: RecoverySimulationCoverage,
    val results: Vector[ScenarioRecoveryResult],
    val claims: Vector[ValidationClaim],
    val valueIdentity: ValueIdentity
)

object RecoveryValidationReport:
  def from(
      resampling: ResamplingDesign,
      missingness: ValidationMissingnessTarget,
      foldSafety: FoldSafetyManifest,
      warmStarts: Vector[DeterministicWarmStartPath],
      simulations: RecoverySimulationCoverage,
      results: Vector[ScenarioRecoveryResult],
      claims: Vector[ValidationClaim]
  ): Either[RecoveryValidationError, RecoveryValidationReport] =
    val resultScenarios = results.map(_.scenario).toSet
    val missingResults = RecoverySimulationScenario.all.diff(resultScenarios)
    if missingResults.nonEmpty then Left(RecoveryValidationError.MissingSimulationScenarios(missingResults))
    else if results.map(current => current.scenario -> current.seed.intValue).distinct.length != results.length then
      Left(RecoveryValidationError.InvalidDefinition("recovery result scenario and seed pairs must be distinct"))
    else if warmStarts.map(_.split).distinct.length != warmStarts.length then
      Left(RecoveryValidationError.InvalidDefinition("warm-start paths must have distinct fold identities"))
    else
      Right(
        new RecoveryValidationReport(
          resampling,
          missingness,
          foldSafety,
          warmStarts,
          simulations,
          results,
          claims,
          ValueIdentity.derived(
            "recovery-validation-report",
            (Vector(resampling.valueIdentity, foldSafety.valueIdentity, simulations.valueIdentity) ++
              warmStarts.map(_.valueIdentity) ++ results.map(_.resultIdentity))*
          )
        )
      )

private def blockCalibration(
    estimated: Vector[Double],
    truth: Vector[Double]
): Either[RecoveryValidationError, Double] =
  if estimated.isEmpty || estimated.length != truth.length then
    Left(RecoveryValidationError.InvalidDefinition("block calibration vectors must be non-empty and equal length"))
  else if estimated.exists(value => !value.isFinite || value < 0.0) || truth.exists(value => !value.isFinite || value < 0.0) then
    Left(RecoveryValidationError.InvalidDefinition("block calibration values must be finite and non-negative"))
  else if estimated.sum <= 0.0 || truth.sum <= 0.0 then
    Left(RecoveryValidationError.InvalidDefinition("block calibration totals must be positive"))
  else
    val estimatedTotal = estimated.sum
    val truthTotal = truth.sum
    Right(estimated.indices.map(index => Math.abs(estimated(index) / estimatedTotal - truth(index) / truthTotal)).sum)

private def alignedColumnError(estimated: DMat, truth: DMat): Double =
  var squared = 0.0
  var truthSquared = 0.0
  var column = 0
  while column < estimated.cols do
    var plus = 0.0
    var minus = 0.0
    var row = 0
    while row < estimated.rows do
      val estimatedValue = estimated(row, column)
      val truthValue = truth(row, column)
      val plusDifference = estimatedValue - truthValue
      val minusDifference = estimatedValue + truthValue
      plus += plusDifference * plusDifference
      minus += minusDifference * minusDifference
      truthSquared += truthValue * truthValue
      row += 1
    squared += Math.min(plus, minus)
    column += 1
  if truthSquared == 0.0 then Math.sqrt(squared) else Math.sqrt(squared / truthSquared)

private def frobeniusDistance(left: DMat, right: DMat): Double =
  var squared = 0.0
  var row = 0
  while row < left.rows do
    var column = 0
    while column < left.cols do
      val difference = left(row, column) - right(row, column)
      squared += difference * difference
      column += 1
    row += 1
  Math.sqrt(squared)

private def squaredFrobenius(matrix: DMat): Double =
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

private def meanPairwiseJaccard(values: Vector[Set[Int]]): Double =
  if values.length < 2 then 1.0
  else
    var total = 0.0
    var count = 0
    var left = 0
    while left < values.length do
      var right = left + 1
      while right < values.length do
        val union = values(left).union(values(right))
        val current = if union.isEmpty then 1.0 else values(left).intersect(values(right)).size.toDouble / union.size.toDouble
        total += current
        count += 1
        right += 1
      left += 1
    total / count.toDouble

private def traverseRecovery[A, B](
    values: Vector[A]
)(
    function: A => Either[RecoveryValidationError, B]
): Either[RecoveryValidationError, Vector[B]] =
  values.foldLeft[Either[RecoveryValidationError, Vector[B]]](Right(Vector.empty)): (result, value) =>
    for
      accumulated <- result
      next <- function(value)
    yield accumulated :+ next
