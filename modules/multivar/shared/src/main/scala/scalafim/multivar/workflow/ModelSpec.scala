package scalafim.multivar
package workflow

import scalafim.multivar.core.*
import scalafim.multivar.contract.*
import scalafim.multivar.optimization.*
import scalafim.multivar.lifecycle.*
import scalafim.multivar.family.spectral.*

import gale.linalg.DMat

opaque type ModelSpecId = String

object ModelSpecId:
  def apply(value: String): Either[ModelSpecError, ModelSpecId] =
    Identifier.validate("model spec id", value).left.map(ModelSpecError.Multivar.apply)

  def unsafe(value: String): ModelSpecId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (value: ModelSpecId)
    inline def stringValue: String = value

opaque type SplitIdentity = String

object SplitIdentity:
  def apply(value: String): Either[ModelSpecError, SplitIdentity] =
    Identifier.validate("split identity", value).left.map(ModelSpecError.Multivar.apply)

  def unsafe(value: String): SplitIdentity =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (value: SplitIdentity)
    inline def stringValue: String = value

opaque type CandidateId = String

object CandidateId:
  def apply(value: String): Either[ModelSpecError, CandidateId] =
    Identifier.validate("candidate id", value).left.map(ModelSpecError.Multivar.apply)

  def unsafe(value: String): CandidateId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (value: CandidateId)
    inline def stringValue: String = value

opaque type ModelRowId = String

object ModelRowId:
  def apply(value: String): Either[ModelSpecError, ModelRowId] =
    Identifier.validate("model row id", value).left.map(ModelSpecError.Multivar.apply)

  def unsafe(value: String): ModelRowId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (value: ModelRowId)
    inline def stringValue: String = value

opaque type DeterministicSeed = Int

object DeterministicSeed:
  def apply(value: Int): DeterministicSeed = value

  extension (value: DeterministicSeed)
    inline def intValue: Int = value

    def derive(parts: String*): DeterministicSeed =
      var hash = value ^ 0x811c9dc5
      parts.foreach: part =>
        var index = 0
        while index < part.length do
          hash = (hash ^ part.charAt(index).toInt) * 16777619
          index += 1
      hash

enum ModelSpecError:
  case InvalidDefinition(reason: String)
  case InvalidSplit(reason: String)
  case LeakageDetected(violations: Vector[String])
  case UnknownHyperparameter(name: String)
  case InvalidHyperparameter(name: String, value: Double, reason: String)
  case IncompatibleFeatureSpace(expected: MvSpace, actual: MvSpace)
  case FeatureIdentityMismatch(expected: ValueIdentity, actual: ValueIdentity)
  case NonFiniteScore(candidate: CandidateId, split: SplitIdentity, value: Double)
  case Multivar(error: MultivarError)
  case Program(error: ProgramError)
  case Quadratic(error: QuadraticLoweringError)
  case ExactSpectral(error: ExactSpectralError)

  def message: String =
    this match
      case InvalidDefinition(reason) => reason
      case InvalidSplit(reason) => reason
      case LeakageDetected(violations) => s"fold lifecycle leakage: ${violations.mkString("; ")}"
      case UnknownHyperparameter(name) => s"unknown hyperparameter '$name'"
      case InvalidHyperparameter(name, value, reason) => s"invalid hyperparameter '$name'=$value: $reason"
      case IncompatibleFeatureSpace(expected, actual) =>
        s"transform feature space ${actual.id.value} does not match fitted ${expected.id.value}"
      case FeatureIdentityMismatch(expected, actual) =>
        s"transform feature identity ${actual.stableKey} does not match fitted ${expected.stableKey}"
      case NonFiniteScore(candidate, split, value) =>
        s"candidate ${candidate.stringValue} produced non-finite score $value on ${split.stringValue}"
      case Multivar(error) => error.message
      case Program(error) => error.message
      case Quadratic(error) => error.message
      case ExactSpectral(error) => error.message

final class ModelStudy private (
    val values: MatrixView,
    val rowIds: Vector[ModelRowId],
    val featureSpace: MvSpace,
    val featureIdentity: ValueIdentity,
    val sourceIdentity: ValueIdentity,
    val provenance: SemanticProvenance
):
  def rows: Int = values.rows
  def cols: Int = values.cols

  def subset(indices: IndexSet): Either[ModelSpecError, ModelStudy] =
    for
      checked <- indices.requireWithin(rows).left.map(ModelSpecError.Multivar.apply)
      rowIndices <- IndexSet
        .from(checked.indices, IndexAxis.Row, Some(rows))
        .left
        .map(ModelSpecError.Multivar.apply)
      selected <- values.selectRows(rowIndices).left.map(ModelSpecError.Multivar.apply)
      subsetIdentity = ValueIdentity.derived(s"study-row-subset-${checked.indices.mkString("-")}", sourceIdentity)
      result <- ModelStudy.from(
        selected,
        checked.indices.map(rowIds),
        featureSpace,
        featureIdentity,
        subsetIdentity,
        provenance.append(SemanticProvenanceEvent.Derived("study-row-subset", Vector(sourceIdentity)))
      )
    yield result

object ModelStudy:
  def from(
      values: MatrixView,
      rowIds: Vector[ModelRowId],
      featureSpace: MvSpace,
      featureIdentity: ValueIdentity,
      sourceIdentity: ValueIdentity,
      provenance: SemanticProvenance
  ): Either[ModelSpecError, ModelStudy] =
    if values.rows <= 0 || values.cols <= 0 then
      Left(ModelSpecError.InvalidDefinition("model study requires positive rows and columns"))
    else if rowIds.length != values.rows || rowIds.distinct.length != rowIds.length then
      Left(ModelSpecError.InvalidDefinition("model row ids must be unique and match the data rows"))
    else if featureSpace.role != SpaceRole.Observed || featureSpace.size != values.cols then
      Left(ModelSpecError.InvalidDefinition("model feature space must be observed and match the data columns"))
    else Right(new ModelStudy(values, rowIds, featureSpace, featureIdentity, sourceIdentity, provenance))

final class ProcessedStudy private (
    val values: MatrixView,
    val rowIds: Vector[ModelRowId],
    val featureSpace: MvSpace,
    val featureIdentity: ValueIdentity,
    val sourceIdentity: ValueIdentity,
    val provenance: SemanticProvenance,
    private[multivar] val trainingScope: Option[TrainingScope]
):
  require(values.rows == rowIds.length, "processed rows must match row ids")
  require(values.cols == featureSpace.size, "processed columns must match feature space")

object ProcessedStudy:
  private[multivar] def transformed(
      values: MatrixView,
      rowIds: Vector[ModelRowId],
      featureSpace: MvSpace,
      featureIdentity: ValueIdentity,
      sourceIdentity: ValueIdentity,
      provenance: SemanticProvenance,
      trainingScope: Option[TrainingScope]
  ): ProcessedStudy =
    new ProcessedStudy(values, rowIds, featureSpace, featureIdentity, sourceIdentity, provenance, trainingScope)

final case class FoldSplit private (
    id: SplitIdentity,
    training: IndexSet,
    validation: IndexSet
)

object FoldSplit:
  def from(
      id: SplitIdentity,
      training: IndexSet,
      validation: IndexSet,
      totalRows: Int
  ): Either[ModelSpecError, FoldSplit] =
    for
      checkedTraining <- training.requireWithin(totalRows).left.map(ModelSpecError.Multivar.apply)
      checkedValidation <- validation.requireWithin(totalRows).left.map(ModelSpecError.Multivar.apply)
      _ <-
        if checkedTraining.indices.toSet.intersect(checkedValidation.indices.toSet).isEmpty then Right(())
        else Left(ModelSpecError.InvalidSplit(s"split ${id.stringValue} has overlapping train and validation rows"))
    yield FoldSplit(id, checkedTraining, checkedValidation)

final case class NestedFoldPlan private (folds: Vector[FoldSplit])

object NestedFoldPlan:
  def from(folds: Vector[FoldSplit]): Either[ModelSpecError, NestedFoldPlan] =
    if folds.isEmpty then Left(ModelSpecError.InvalidDefinition("nested tuning requires at least one inner fold"))
    else if folds.map(_.id).distinct.length != folds.length then
      Left(ModelSpecError.InvalidDefinition("inner fold identities must be unique"))
    else Right(NestedFoldPlan(folds))

final case class HyperparameterCandidate private (
    id: CandidateId,
    values: Vector[(String, Double)]
):
  def value(name: String): Either[ModelSpecError, Double] =
    values.find(_._1 == name).map(_._2).toRight(ModelSpecError.UnknownHyperparameter(name))

object HyperparameterCandidate:
  def from(
      id: CandidateId,
      values: Vector[(String, Double)]
  ): Either[ModelSpecError, HyperparameterCandidate] =
    val names = values.map(_._1.trim)
    if values.isEmpty then Left(ModelSpecError.InvalidDefinition("hyperparameter candidate must not be empty"))
    else if names.exists(_.isEmpty) || names.distinct.length != names.length then
      Left(ModelSpecError.InvalidDefinition("hyperparameter names must be non-empty and unique"))
    else if values.exists(!_._2.isFinite) then
      Left(ModelSpecError.InvalidDefinition("hyperparameter values must be finite"))
    else Right(HyperparameterCandidate(id, names.zip(values.map(_._2))))

enum SelectionDirection:
  case Minimize
  case Maximize

enum MissingnessPolicy:
  case RejectNonFinite
  case PipelineHandled(reason: String)

  private[multivar] def validateDefinition: Either[ModelSpecError, Unit] =
    this match
      case RejectNonFinite => Right(())
      case PipelineHandled(reason) =>
        if reason.trim.nonEmpty then Right(())
        else Left(ModelSpecError.InvalidDefinition("pipeline-handled missingness requires a non-empty reason"))

  private[multivar] def validate(study: ModelStudy): Either[ModelSpecError, Unit] =
    this match
      case RejectNonFinite =>
        for
          dense <- study.values.toDense(StoragePolicy.AllowDense).left.map(ModelSpecError.Multivar.apply)
          _ <- MatrixOps.checkFinite("model study", dense).left.map(ModelSpecError.Multivar.apply)
        yield ()
      case PipelineHandled(_) => Right(())

final class TrainingScope private (
    val split: SplitIdentity,
    val seed: DeterministicSeed,
    val trainingRows: Vector[ModelRowId],
    val sourceIdentity: ValueIdentity,
    val valueIdentity: ValueIdentity
):
  val id: TrainingScopeId = TrainingScopeId.fromValue(valueIdentity)

object TrainingScope:
  private[multivar] def mint(
      split: SplitIdentity,
      seed: DeterministicSeed,
      trainingRows: Vector[ModelRowId],
      sourceIdentity: ValueIdentity
  ): TrainingScope =
    new TrainingScope(
      split,
      seed,
      trainingRows,
      sourceIdentity,
      ValueIdentity.derived("modelspec-training-scope", sourceIdentity)
    )

final class TrainingContext private (
    val split: SplitIdentity,
    val seed: DeterministicSeed,
    val trainingRows: Vector[ModelRowId],
    val candidate: CandidateId,
    private[multivar] val scope: TrainingScope
):
  require(trainingRows.nonEmpty && trainingRows.distinct.length == trainingRows.length,
    "training context rows must be non-empty and unique")

object TrainingContext:
  private[multivar] def mint(
      split: SplitIdentity,
      seed: DeterministicSeed,
      training: ModelStudy,
      candidate: CandidateId
  ): TrainingContext =
    new TrainingContext(
      split,
      seed,
      training.rowIds,
      candidate,
      TrainingScope.mint(split, seed, training.rowIds, training.sourceIdentity)
    )

enum LifecycleStage:
  case Preprocessing
  case AlignmentEstimation
  case ChartEstimation
  case GraphEstimation
  case StatisticalEstimation
  case OperatorPolicy
  case ProgramBuild
  case Lowering
  case Solve
  case Transform
  case Score

enum LifecycleAction:
  case Fit
  case Apply
  case Evaluate

final case class LifecyclePlan private (
    stage: LifecycleStage,
    artifact: String
)

object LifecyclePlan:
  private val pipelineStages = Set(
    LifecycleStage.AlignmentEstimation,
    LifecycleStage.ChartEstimation,
    LifecycleStage.GraphEstimation,
    LifecycleStage.StatisticalEstimation,
    LifecycleStage.OperatorPolicy,
    LifecycleStage.ProgramBuild,
    LifecycleStage.Lowering,
    LifecycleStage.Solve
  )

  def from(stage: LifecycleStage, artifact: String): Either[ModelSpecError, LifecyclePlan] =
    val clean = artifact.trim
    if !pipelineStages.contains(stage) then
      Left(ModelSpecError.InvalidDefinition(s"$stage is not a fold-fitted pipeline stage"))
    else if clean.isEmpty then Left(ModelSpecError.InvalidDefinition("lifecycle plan artifact must be non-empty"))
    else Right(LifecyclePlan(stage, clean))

  def unsafe(stage: LifecycleStage, artifact: String): LifecyclePlan =
    from(stage, artifact).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class ModelSolverPolicy private (
    artifact: String,
    splitCapabilities: Set[SplitMethod],
    acceptedClaims: Set[OptimizationClaimClass]
):
  def accepts(achieved: AchievedOptimizationGuarantee): Boolean =
    acceptedClaims.contains(achieved.claimClass)

object ModelSolverPolicy:
  def from(
      artifact: String,
      splitCapabilities: Set[SplitMethod],
      acceptedClaims: Set[OptimizationClaimClass]
  ): Either[ModelSpecError, ModelSolverPolicy] =
    val clean = artifact.trim
    if clean.isEmpty then Left(ModelSpecError.InvalidDefinition("solver policy artifact must be non-empty"))
    else if acceptedClaims.isEmpty then
      Left(ModelSpecError.InvalidDefinition("solver policy must accept at least one achieved claim class"))
    else Right(ModelSolverPolicy(clean, splitCapabilities, acceptedClaims))

  def unsafe(
      artifact: String,
      splitCapabilities: Set[SplitMethod],
      acceptedClaims: Set[OptimizationClaimClass]
  ): ModelSolverPolicy =
    from(artifact, splitCapabilities, acceptedClaims)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

final case class LifecycleEvent(
    stage: LifecycleStage,
    action: LifecycleAction,
    split: SplitIdentity,
    seed: DeterministicSeed,
    trainingRows: Vector[ModelRowId],
    appliedRows: Vector[ModelRowId],
    artifact: String,
    provenance: SemanticProvenance
):
  require(trainingRows.nonEmpty, "lifecycle event requires training rows")
  require(appliedRows.nonEmpty, "lifecycle event requires applied rows")
  require(artifact.trim.nonEmpty, "lifecycle event artifact must be non-empty")

object LifecycleEvent:
  def fit(context: TrainingContext, stage: LifecycleStage, artifact: String, provenance: SemanticProvenance): LifecycleEvent =
    LifecycleEvent(
      stage,
      LifecycleAction.Fit,
      context.split,
      context.seed,
      context.trainingRows,
      context.trainingRows,
      artifact,
      provenance
    )

  def applyTo(
      context: TrainingContext,
      stage: LifecycleStage,
      rows: Vector[ModelRowId],
      artifact: String,
      provenance: SemanticProvenance,
      action: LifecycleAction = LifecycleAction.Apply
  ): LifecycleEvent =
    LifecycleEvent(stage, action, context.split, context.seed, context.trainingRows, rows, artifact, provenance)

final case class LeakageAuditReport(valid: Boolean, violations: Vector[String])

object LeakageAudit:
  def verify(
      context: TrainingContext,
      validationRows: Vector[ModelRowId],
      events: Vector[LifecycleEvent]
  ): LeakageAuditReport =
    val training = context.trainingRows.toSet
    val validation = validationRows.toSet
    val violations = Vector.newBuilder[String]
    events.foreach: event =>
      if event.split != context.split then violations += s"${event.artifact} used split ${event.split.stringValue}"
      if event.seed != context.seed then violations += s"${event.artifact} used a different deterministic seed"
      if event.trainingRows != context.trainingRows then violations += s"${event.artifact} was fitted against different rows"
      if event.action == LifecycleAction.Fit && event.appliedRows.toSet != training then
        violations += s"${event.artifact} fit did not use exactly the training partition"
      if event.action == LifecycleAction.Evaluate && event.appliedRows.toSet.diff(validation).nonEmpty then
        violations += s"${event.artifact} evaluated rows outside the validation partition"
      if event.action != LifecycleAction.Fit && event.appliedRows.toSet.diff(training ++ validation).nonEmpty then
        violations += s"${event.artifact} applied to rows outside the declared fold"
      if event.action == LifecycleAction.Fit && event.appliedRows.toSet.intersect(validation).nonEmpty then
        violations += s"${event.artifact} fit touched validation rows"
    val result = violations.result()
    LeakageAuditReport(result.isEmpty, result)

final case class SolverExecutionRecord private (
    artifact: String,
    method: String,
    settings: Vector[(String, String)],
    attestation: SolverAttestation
)

object SolverExecutionRecord:
  def from(
      artifact: String,
      method: String,
      settings: Vector[(String, String)],
      attestation: SolverAttestation
  ): Either[ModelSpecError, SolverExecutionRecord] =
    val cleanArtifact = artifact.trim
    val cleanMethod = method.trim
    val cleanedSettings = settings.map((name, value) => name.trim -> value.trim)
    if cleanArtifact.isEmpty then Left(ModelSpecError.InvalidDefinition("solver execution artifact must be non-empty"))
    else if cleanMethod.isEmpty then Left(ModelSpecError.InvalidDefinition("solver execution method must be non-empty"))
    else if cleanedSettings.exists((name, value) => name.isEmpty || value.isEmpty) then
      Left(ModelSpecError.InvalidDefinition("solver execution settings must have non-empty names and values"))
    else if cleanedSettings.map(_._1).distinct.length != cleanedSettings.length then
      Left(ModelSpecError.InvalidDefinition("solver execution setting names must be unique"))
    else Right(SolverExecutionRecord(cleanArtifact, cleanMethod, cleanedSettings, attestation))

final class FittedModelPreprocessor private (
    val featureSpace: MvSpace,
    val featureIdentity: ValueIdentity,
    val fitted: FittedPreprocessor,
    val context: TrainingContext,
    val provenance: SemanticProvenance
):
  def transform(study: ModelStudy): Either[ModelSpecError, ProcessedStudy] =
    transformScoped(study, None)

  private[multivar] def transformTraining(study: ModelStudy): Either[ModelSpecError, ProcessedStudy] =
    if study.rowIds != context.trainingRows || study.sourceIdentity != context.scope.sourceIdentity then
      Left(ModelSpecError.LeakageDetected(Vector("preprocessor training input does not match the ModelSpec-minted scope")))
    else transformScoped(study, Some(context.scope))

  private def transformScoped(
      study: ModelStudy,
      scope: Option[TrainingScope]
  ): Either[ModelSpecError, ProcessedStudy] =
    if study.featureSpace != featureSpace then
      Left(ModelSpecError.IncompatibleFeatureSpace(featureSpace, study.featureSpace))
    else if study.featureIdentity != featureIdentity then
      Left(ModelSpecError.FeatureIdentityMismatch(featureIdentity, study.featureIdentity))
    else
      fitted
        .transform(study.values)
        .left
        .map(ModelSpecError.Multivar.apply)
        .map: transformed =>
          ProcessedStudy.transformed(
            transformed,
            study.rowIds,
            featureSpace,
            featureIdentity,
            ValueIdentity.derived("fitted-preprocess-transform", study.sourceIdentity),
            provenance.append(
              SemanticProvenanceEvent.Derived("fitted-preprocess-transform", Vector(study.sourceIdentity))
            ),
            scope
          )

object FittedModelPreprocessor:
  def fit(
      spec: PreprocessSpec,
      context: TrainingContext,
      training: ModelStudy
  ): Either[ModelSpecError, FittedModelPreprocessor] =
    spec.fit(training.values).left.map(ModelSpecError.Multivar.apply).map: fitted =>
      val provenance = training.provenance.append(
        SemanticProvenanceEvent.Derived("fit-preprocessing", Vector(training.sourceIdentity))
      )
      new FittedModelPreprocessor(
        training.featureSpace,
        training.featureIdentity,
        fitted,
        context,
        provenance
      )

final case class FoldPipelineFit private (
    requestedProgram: OperatorProgram,
    loweredProgram: OperatorProgram,
    fitBundle: OperatorFitBundle,
    operatorPolicies: Vector[OperatorPolicyRecord],
    effectiveOperators: Vector[OperatorSnapshot],
    auxiliaryVariables: Vector[AuxiliaryConstraint],
    splitMethod: Option[SplitMethod],
    solverExecution: SolverExecutionRecord,
    private[multivar] val trainingScope: TrainingScope,
    events: Vector[LifecycleEvent],
    provenance: SemanticProvenance
):
  def activePenalties: Vector[PenaltyTerm] = requestedProgram.penalties
  def activeConstraints: Vector[ConstraintTerm] = requestedProgram.constraints
  def solverAttestation: SolverAttestation = fitBundle.programFit.solverAttestation
  def achievedGuarantee: AchievedOptimizationGuarantee = fitBundle.programFit.achievedGuarantee
  def trainingProvenanceIdentity: ValueIdentity = trainingScope.valueIdentity

object FoldPipelineFit:
  def from(
      context: TrainingContext,
      training: ProcessedStudy,
      requestedProgram: OperatorProgram,
      loweredProgram: OperatorProgram,
      fitBundle: OperatorFitBundle,
      operatorPolicies: Vector[OperatorPolicyRecord],
      effectiveOperators: Vector[OperatorSnapshot],
      auxiliaryVariables: Vector[AuxiliaryConstraint],
      splitMethod: Option[SplitMethod],
      solverExecution: SolverExecutionRecord,
      events: Vector[LifecycleEvent],
      provenance: SemanticProvenance
  ): Either[ModelSpecError, FoldPipelineFit] =
    val labels = effectiveOperators.map(_.label)
    val eventAudit = LeakageAudit.verify(context, Vector.empty, events)
    val hasCurrentScope = training.trainingScope.exists(_ eq context.scope)
    val framesBoundToTraining = fitBundle.parameterFrames.forall: frame =>
      identityDependsOn(frame.sourceIdentity, training.sourceIdentity)
    val operatorsBoundToTraining = effectiveOperators.forall: operator =>
      identityDependsOn(operator.sourceIdentity, training.sourceIdentity)
    val policiesBoundToTraining = operatorPolicies.forall: policy =>
      policy.outputIdentities.forall(identityDependsOn(_, training.sourceIdentity))
    if !hasCurrentScope || training.rowIds != context.trainingRows then
      Left(ModelSpecError.LeakageDetected(Vector("pipeline fit does not carry the current ModelSpec-minted training scope")))
    else if !framesBoundToTraining then
      Left(ModelSpecError.LeakageDetected(Vector("fitted frame provenance is not derived from the scoped training study")))
    else if !operatorsBoundToTraining then
      Left(ModelSpecError.LeakageDetected(Vector("effective operator provenance is not derived from the scoped training study")))
    else if !policiesBoundToTraining then
      Left(ModelSpecError.LeakageDetected(Vector("operator-policy output provenance is not derived from the scoped training study")))
    else if fitBundle.programFit.program ne loweredProgram then
      Left(ModelSpecError.InvalidDefinition("fit bundle must reference the exact lowered program"))
    else if labels.distinct.length != labels.length then
      Left(ModelSpecError.InvalidDefinition("effective operator labels must be unique"))
    else if solverExecution.attestation != fitBundle.programFit.solverAttestation then
      Left(ModelSpecError.InvalidDefinition("solver execution record must carry the fit bundle attestation"))
    else if !eventAudit.valid then Left(ModelSpecError.LeakageDetected(eventAudit.violations))
    else
      Right(
        FoldPipelineFit(
          requestedProgram,
          loweredProgram,
          fitBundle,
          operatorPolicies,
          effectiveOperators,
          auxiliaryVariables,
          splitMethod,
          solverExecution,
          context.scope,
          events,
          provenance
        )
      )

  private def identityDependsOn(value: ValueIdentity, input: ValueIdentity): Boolean =
    value == input || (value match
      case ValueIdentity.Source(_) => false
      case ValueIdentity.Adjoint(of) => identityDependsOn(of, input)
      case ValueIdentity.Composition(first, second) =>
        identityDependsOn(first, input) || identityDependsOn(second, input)
      case ValueIdentity.Derived(_, inputs) => inputs.exists(identityDependsOn(_, input))
    )

trait FoldPipeline:
  def fit(
      context: TrainingContext,
      training: ProcessedStudy,
      candidate: HyperparameterCandidate
  ): Either[ModelSpecError, FoldPipelineFit]

trait ValidationScorer:
  def score(
      fitted: FoldPipelineFit,
      validation: ProcessedStudy,
      candidate: HyperparameterCandidate
  ): Either[ModelSpecError, Double]

final case class PipelineTransformation private (
    values: DMat,
    outputCoordinates: CoordinateDescriptor,
    sourceIdentity: ValueIdentity,
    provenance: SemanticProvenance
)

object PipelineTransformation:
  def from(
      values: DMat,
      outputCoordinates: CoordinateDescriptor,
      sourceIdentity: ValueIdentity,
      provenance: SemanticProvenance
  ): Either[ModelSpecError, PipelineTransformation] =
    if values.cols != outputCoordinates.dimension then
      Left(ModelSpecError.InvalidDefinition("pipeline transformation columns do not match its output coordinates"))
    else Right(PipelineTransformation(values, outputCoordinates, sourceIdentity, provenance))

trait ModelTransformer:
  def transform(fitted: FoldPipelineFit, study: ProcessedStudy): Either[ModelSpecError, PipelineTransformation]

final case class FoldEvaluation(
    candidate: CandidateId,
    split: SplitIdentity,
    seed: DeterministicSeed,
    score: Double,
    audit: LeakageAuditReport,
    events: Vector[LifecycleEvent]
)

final case class CandidateEvaluation(
    candidate: HyperparameterCandidate,
    folds: Vector[FoldEvaluation],
    meanScore: Double
)

final case class ModelSelectionReport(
    direction: SelectionDirection,
    candidates: Vector[CandidateEvaluation],
    selected: HyperparameterCandidate,
    outerSplit: SplitIdentity,
    baseSeed: DeterministicSeed
)

final case class ModelTransformation private (
    values: DMat,
    rowIds: Vector[ModelRowId],
    inputFeatureSpace: MvSpace,
    inputFeatureIdentity: ValueIdentity,
    outputCoordinates: CoordinateDescriptor,
    sourceIdentity: ValueIdentity,
    provenance: SemanticProvenance
):
  def rows: Int = values.rows
  def cols: Int = values.cols

object ModelTransformation:
  private[multivar] def from(
      transformed: PipelineTransformation,
      study: ProcessedStudy,
      fittedProvenance: SemanticProvenance
  ): Either[ModelSpecError, ModelTransformation] =
    if transformed.values.rows != study.rowIds.length then
      Left(ModelSpecError.InvalidDefinition("model transformation rows do not match the transformed study"))
    else
      val sourceIdentity = ValueIdentity.derived(
        "modelspec-transformation",
        study.sourceIdentity,
        transformed.sourceIdentity
      )
      Right(
        ModelTransformation(
          transformed.values,
          study.rowIds,
          study.featureSpace,
          study.featureIdentity,
          transformed.outputCoordinates,
          sourceIdentity,
          (study.provenance ++ transformed.provenance ++ fittedProvenance).append(
            SemanticProvenanceEvent.Derived(
              "modelspec-transformation",
              Vector(study.sourceIdentity, transformed.sourceIdentity)
            )
          )
        )
      )

final class ModelFit private[multivar] (
    val specId: ModelSpecId,
    val featureSpace: MvSpace,
    val featureIdentity: ValueIdentity,
    val missingness: MissingnessPolicy,
    val lifecyclePlans: Vector[LifecyclePlan],
    val solverPolicy: ModelSolverPolicy,
    val preprocessor: FittedModelPreprocessor,
    val pipeline: FoldPipelineFit,
    val selection: ModelSelectionReport,
    val lifecycleEvents: Vector[LifecycleEvent],
    val transformer: ModelTransformer,
    val provenance: SemanticProvenance
):
  def requestedProgram: OperatorProgram = pipeline.requestedProgram
  def loweredProgram: OperatorProgram = pipeline.loweredProgram
  def effectiveOperators: Vector[OperatorSnapshot] = pipeline.effectiveOperators
  def auxiliaryVariables: Vector[AuxiliaryConstraint] = pipeline.auxiliaryVariables
  def achievedGuarantee: AchievedOptimizationGuarantee = pipeline.achievedGuarantee
  def solverExecution: SolverExecutionRecord = pipeline.solverExecution

  def transform(study: ModelStudy): Either[ModelSpecError, ModelTransformation] =
    for
      processed <- preprocessor.transform(study)
      transformed <- transformer.transform(pipeline, processed)
      result <- ModelTransformation.from(transformed, processed, pipeline.provenance)
    yield result

final class ModelSpec private (
    val id: ModelSpecId,
    val preprocessing: PreprocessSpec,
    val missingness: MissingnessPolicy,
    val lifecyclePlans: Vector[LifecyclePlan],
    val candidates: Vector[HyperparameterCandidate],
    val direction: SelectionDirection,
    val innerFolds: NestedFoldPlan,
    val pipeline: FoldPipeline,
    val scorer: ValidationScorer,
    val transformer: ModelTransformer,
    val solverPolicy: ModelSolverPolicy,
    val baseSeed: DeterministicSeed
):
  def fit(study: ModelStudy, outer: FoldSplit): Either[ModelSpecError, ModelFit] =
    missingness.validate(study).flatMap: _ =>
      val outerRows = outer.training.indices.toSet
      val invalidInner = innerFolds.folds.find: fold =>
        !fold.training.indices.toSet.subsetOf(outerRows) || !fold.validation.indices.toSet.subsetOf(outerRows)
      invalidInner match
        case Some(fold) => Left(ModelSpecError.InvalidSplit(s"inner split ${fold.id.stringValue} leaves outer training rows"))
        case None => evaluateCandidates(study, outer).flatMap: selection =>
          val finalSplit = SplitIdentity.unsafe(s"${id.stringValue}.${outer.id.stringValue}.final")
          val finalSeed = baseSeed.derive(finalSplit.stringValue, selection.selected.id.stringValue)
          for
            outerTraining <- study.subset(outer.training)
            context = TrainingContext.mint(finalSplit, finalSeed, outerTraining, selection.selected.id)
            trained <- trainOne(context, outerTraining, selection.selected)
            audit = LeakageAudit.verify(context, Vector.empty, trained.events)
            _ <- if audit.valid then Right(()) else Left(ModelSpecError.LeakageDetected(audit.violations))
          yield
            new ModelFit(
              id,
              study.featureSpace,
              study.featureIdentity,
              missingness,
              lifecyclePlans,
              solverPolicy,
              trained.preprocessor,
              trained.pipeline,
              selection,
              selection.candidates.flatMap(_.folds.flatMap(_.events)) ++ trained.events,
              transformer,
              study.provenance ++ trained.pipeline.provenance
            )

  private final case class TrainedFold(
      preprocessor: FittedModelPreprocessor,
      pipeline: FoldPipelineFit,
      training: ProcessedStudy,
      events: Vector[LifecycleEvent]
  )

  private def trainOne(
      context: TrainingContext,
      training: ModelStudy,
      candidate: HyperparameterCandidate
  ): Either[ModelSpecError, TrainedFold] =
    for
      fittedPreprocessor <- FittedModelPreprocessor.fit(preprocessing, context, training)
      processed <- fittedPreprocessor.transformTraining(training)
      preprocessFit = LifecycleEvent.fit(
        context,
        LifecycleStage.Preprocessing,
        "preprocessor",
        fittedPreprocessor.provenance
      )
      preprocessApply = LifecycleEvent.applyTo(
        context,
        LifecycleStage.Transform,
        training.rowIds,
        "training-preprocess-transform",
        processed.provenance
      )
      fittedPipeline <- pipeline.fit(context, processed, candidate)
      _ <-
        if (fittedPipeline.trainingScope eq context.scope) && processed.trainingScope.exists(_ eq context.scope) then Right(())
        else Left(ModelSpecError.LeakageDetected(Vector("pipeline fit was returned from a different ModelSpec-minted training scope")))
      _ <- validatePipeline(context, processed, fittedPipeline)
      events = Vector(preprocessFit, preprocessApply) ++ fittedPipeline.events
      audit = LeakageAudit.verify(context, Vector.empty, events)
      _ <- if audit.valid then Right(()) else Left(ModelSpecError.LeakageDetected(audit.violations))
    yield TrainedFold(fittedPreprocessor, fittedPipeline, processed, events)

  private def validatePipeline(
      context: TrainingContext,
      training: ProcessedStudy,
      fitted: FoldPipelineFit
  ): Either[ModelSpecError, Unit] =
    val expected = lifecyclePlans.map(plan => (plan.stage, plan.artifact))
    val observed = fitted.events.collect:
      case event if event.action == LifecycleAction.Fit => (event.stage, event.artifact)
    val missing = expected.filterNot(observed.contains)
    val undeclared = observed.filterNot(expected.contains)
    val duplicateObserved = observed.groupBy(identity).collect:
      case (value, occurrences) if occurrences.length > 1 => value
    val declaredPolicyArtifacts = lifecyclePlans.collect:
      case plan if plan.stage == LifecycleStage.OperatorPolicy => plan.artifact
    val realizedPolicyArtifacts = fitted.operatorPolicies.map(_.id.stringValue)
    val violations = Vector.newBuilder[String]
    if !(fitted.trainingScope eq context.scope) || !training.trainingScope.exists(_ eq context.scope) then
      violations += "pipeline fit was returned from a different ModelSpec-minted training scope"
    missing.foreach:
      case (stage, artifact) => violations += s"declared $stage artifact '$artifact' was not fitted"
    undeclared.foreach:
      case (stage, artifact) => violations += s"undeclared $stage artifact '$artifact' was fitted"
    duplicateObserved.foreach:
      case (stage, artifact) => violations += s"$stage artifact '$artifact' was fitted more than once"
    declaredPolicyArtifacts.filterNot(realizedPolicyArtifacts.contains).foreach: artifact =>
      violations += s"declared operator policy '$artifact' has no certified policy record"
    realizedPolicyArtifacts.filterNot(declaredPolicyArtifacts.contains).foreach: artifact =>
      violations += s"certified operator policy '$artifact' was not declared by ModelSpec"
    fitted.splitMethod.foreach: method =>
      if !solverPolicy.splitCapabilities.contains(method) then
        violations += s"split method $method is not supported by policy ${solverPolicy.artifact}"
    if fitted.solverExecution.artifact != solverPolicy.artifact then
      violations += s"solver execution artifact ${fitted.solverExecution.artifact} does not match policy ${solverPolicy.artifact}"
    if !solverPolicy.accepts(fitted.achievedGuarantee) then
      violations +=
        s"achieved optimization claim ${fitted.achievedGuarantee.claimClass} is not accepted by policy ${solverPolicy.artifact}"
    val result = violations.result()
    if result.isEmpty then Right(()) else Left(ModelSpecError.InvalidDefinition(result.mkString("; ")))

  private def evaluateCandidates(
      study: ModelStudy,
      outer: FoldSplit
  ): Either[ModelSpecError, ModelSelectionReport] =
    val evaluations = Vector.newBuilder[CandidateEvaluation]
    var failure = Option.empty[ModelSpecError]
    val prepared = Vector.newBuilder[(FoldSplit, ModelStudy, ModelStudy)]
    var preparationIndex = 0
    while preparationIndex < innerFolds.folds.length && failure.isEmpty do
      val fold = innerFolds.folds(preparationIndex)
      (study.subset(fold.training), study.subset(fold.validation)) match
        case (Right(training), Right(validation)) => prepared += ((fold, training, validation))
        case (Left(error), _) => failure = Some(error)
        case (_, Left(error)) => failure = Some(error)
      preparationIndex += 1
    val preparedFolds = if failure.isEmpty then prepared.result() else Vector.empty
    var candidateIndex = 0
    while candidateIndex < candidates.length && failure.isEmpty do
      val candidate = candidates(candidateIndex)
      val folds = Vector.newBuilder[FoldEvaluation]
      var foldIndex = 0
      while foldIndex < preparedFolds.length && failure.isEmpty do
        val (fold, training, validation) = preparedFolds(foldIndex)
        val split = SplitIdentity.unsafe(s"${id.stringValue}.${outer.id.stringValue}.${fold.id.stringValue}.${candidate.id.stringValue}")
        val seed = baseSeed.derive(split.stringValue, candidate.id.stringValue)
        val context = TrainingContext.mint(split, seed, training, candidate.id)
        trainOne(context, training, candidate) match
          case Left(error) => failure = Some(error)
          case Right(trained) =>
            trained.preprocessor.transform(validation) match
              case Left(error) => failure = Some(error)
              case Right(processedValidation) =>
                scorer.score(trained.pipeline, processedValidation, candidate) match
                  case Left(error) => failure = Some(error)
                  case Right(score) if !score.isFinite =>
                    failure = Some(ModelSpecError.NonFiniteScore(candidate.id, split, score))
                  case Right(score) =>
                    val transformEvent = LifecycleEvent.applyTo(
                      context,
                      LifecycleStage.Transform,
                      validation.rowIds,
                      "validation-preprocess-transform",
                      processedValidation.provenance
                    )
                    val scoreEvent = LifecycleEvent.applyTo(
                      context,
                      LifecycleStage.Score,
                      validation.rowIds,
                      "validation-score",
                      trained.pipeline.provenance,
                      LifecycleAction.Evaluate
                    )
                    val events = trained.events ++ Vector(transformEvent, scoreEvent)
                    val audit = LeakageAudit.verify(context, validation.rowIds, events)
                    if !audit.valid then failure = Some(ModelSpecError.LeakageDetected(audit.violations))
                    else folds += FoldEvaluation(candidate.id, split, seed, score, audit, events)
        foldIndex += 1
      if failure.isEmpty then
        val values = folds.result()
        evaluations += CandidateEvaluation(candidate, values, values.map(_.score).sum / values.length.toDouble)
      candidateIndex += 1
    failure match
      case Some(error) => Left(error)
      case None =>
        val completed = evaluations.result()
        val selected =
          completed.tail.foldLeft(completed.head): (best, current) =>
            val better =
              direction match
                case SelectionDirection.Minimize => current.meanScore < best.meanScore
                case SelectionDirection.Maximize => current.meanScore > best.meanScore
            if better then current else best
        Right(ModelSelectionReport(direction, completed, selected.candidate, outer.id, baseSeed))

object ModelSpec:
  def from(
      id: ModelSpecId,
      preprocessing: PreprocessSpec,
      missingness: MissingnessPolicy,
      lifecyclePlans: Vector[LifecyclePlan],
      candidates: Vector[HyperparameterCandidate],
      direction: SelectionDirection,
      innerFolds: NestedFoldPlan,
      pipeline: FoldPipeline,
      scorer: ValidationScorer,
      transformer: ModelTransformer,
      solverPolicy: ModelSolverPolicy,
      baseSeed: DeterministicSeed
  ): Either[ModelSpecError, ModelSpec] =
    val requiredStages = Set(
      LifecycleStage.StatisticalEstimation,
      LifecycleStage.ProgramBuild,
      LifecycleStage.Lowering,
      LifecycleStage.Solve
    )
    val plannedStages = lifecyclePlans.map(_.stage).toSet
    val planIdentities = lifecyclePlans.map(plan => (plan.stage, plan.artifact))
    if candidates.isEmpty || candidates.map(_.id).distinct.length != candidates.length then
      Left(ModelSpecError.InvalidDefinition("model candidates must be non-empty with unique identities"))
    else if lifecyclePlans.isEmpty || planIdentities.distinct.length != planIdentities.length then
      Left(ModelSpecError.InvalidDefinition("lifecycle plans must be non-empty with unique stage/artifact identities"))
    else if !requiredStages.subsetOf(plannedStages) then
      val missing = requiredStages.diff(plannedStages).mkString(", ")
      Left(ModelSpecError.InvalidDefinition(s"lifecycle plan is missing required stages: $missing"))
    else if !lifecyclePlans.exists(plan => plan.stage == LifecycleStage.Solve && plan.artifact == solverPolicy.artifact) then
      Left(ModelSpecError.InvalidDefinition("solver policy must match a declared solve-stage artifact"))
    else
      missingness.validateDefinition.map: _ =>
        new ModelSpec(
          id,
          preprocessing,
          missingness,
          lifecyclePlans,
          candidates,
          direction,
          innerFolds,
          pipeline,
          scorer,
          transformer,
          solverPolicy,
          baseSeed
        )

/** Shared, fully executable lifecycle mill for single-view GPCA. Candidate
  * hyperparameter `components` is selected inside the nested folds; every fold
  * builds and solves a fresh operator program from freshly preprocessed rows.
  */
final class GpcaFoldPipeline extends FoldPipeline:
  def fit(
      context: TrainingContext,
      training: ProcessedStudy,
      candidate: HyperparameterCandidate
  ): Either[ModelSpecError, FoldPipelineFit] =
    for
      rawComponents <- candidate.value("components")
      components <-
        if rawComponents.isValidInt && rawComponents == rawComponents.toInt.toDouble && rawComponents > 0.0 then
          Right(ComponentCount.unsafe(rawComponents.toInt))
        else Left(ModelSpecError.InvalidHyperparameter("components", rawComponents, "must be a positive integer"))
      rowSpace <- MvSpace
        .of(s"${context.split.stringValue}.rows", SpaceRole.Samples, training.values.rows)
        .left
        .map(ModelSpecError.Multivar.apply)
      rowMetric <- MetricSpec.identity(training.values.rows, Some(rowSpace)).left.map(ModelSpecError.Multivar.apply)
      featureMetric <- MetricSpec
        .identity(training.values.cols, Some(training.featureSpace))
        .left
        .map(ModelSpecError.Multivar.apply)
      problem <- DynamicGpcaProblem
        .from(
          training.values,
          rowSpace,
          training.featureSpace,
          rowMetric,
          featureMetric,
          ValueIdentity.derived("modelspec-gpca", training.sourceIdentity),
          training.provenance
        )
        .left
        .map(ModelSpecError.Multivar.apply)
      fitted <- problem.fit(components).left.map(ModelSpecError.Multivar.apply)
      bundle <- fitted.toBundle(problem.value.table).left.map(ModelSpecError.Multivar.apply)
      requested = bundle.programFit.program
      solverExecution <- SolverExecutionRecord.from(
        "gale-generalized-eigen",
        "gale.spectral generalized symmetric-definite eigen",
        Vector("components" -> components.value.toString),
        bundle.programFit.solverAttestation
      )
      events = Vector(
        LifecycleEvent.fit(context, LifecycleStage.StatisticalEstimation, "gpca-operator-diagram", fitted.provenance),
        LifecycleEvent.fit(context, LifecycleStage.ProgramBuild, "gpca-operator-program", requested.provenance),
        LifecycleEvent.fit(context, LifecycleStage.Lowering, "generalized-rayleigh-ritz", fitted.provenance),
        LifecycleEvent.fit(context, LifecycleStage.Solve, "gale-generalized-eigen", fitted.provenance)
      )
      result <- FoldPipelineFit.from(
        context,
        training,
        requested,
        requested,
        bundle,
        Vector.empty,
        bundle.derivedOperators,
        Vector.empty,
        None,
        solverExecution,
        events,
        fitted.provenance
      )
    yield result

object FittedFrameCapturedVariance extends ValidationScorer:
  def score(
      fitted: FoldPipelineFit,
      validation: ProcessedStudy,
      candidate: HyperparameterCandidate
  ): Either[ModelSpecError, Double] =
    fitted.fitBundle.parameterFrames.headOption match
      case None => Left(ModelSpecError.InvalidDefinition("operator fit has no functional frame"))
      case Some(frame) =>
        validation.values.rightMultiply(frame.values).left.map(ModelSpecError.Multivar.apply).map: scores =>
          modelSpecSquaredNorm(scores) / validation.values.rows.toDouble

object GpcaCapturedVariance extends ValidationScorer:
  def score(
      fitted: FoldPipelineFit,
      validation: ProcessedStudy,
      candidate: HyperparameterCandidate
  ): Either[ModelSpecError, Double] =
    FittedFrameCapturedVariance.score(fitted, validation, candidate)

object FittedFrameTransformer extends ModelTransformer:
  def transform(
      fitted: FoldPipelineFit,
      study: ProcessedStudy
  ): Either[ModelSpecError, PipelineTransformation] =
    fitted.fitBundle.parameterFrames.headOption match
      case None => Left(ModelSpecError.InvalidDefinition("operator fit has no functional frame"))
      case Some(frame) =>
        for
          values <- study.values.rightMultiply(frame.values).left.map(ModelSpecError.Multivar.apply)
          result <- PipelineTransformation.from(
            values,
            frame.domain,
            ValueIdentity.derived("modelspec-frame-transform", study.sourceIdentity, frame.sourceIdentity),
            study.provenance ++ frame.provenance
          )
        yield result

object GpcaFrameTransformer extends ModelTransformer:
  def transform(
      fitted: FoldPipelineFit,
      study: ProcessedStudy
  ): Either[ModelSpecError, PipelineTransformation] =
    FittedFrameTransformer.transform(fitted, study)

private def modelSpecSquaredNorm(value: DMat): Double =
  var result = 0.0
  var row = 0
  while row < value.rows do
    var column = 0
    while column < value.cols do
      val current = value(row, column)
      result += current * current
      column += 1
    row += 1
  result
