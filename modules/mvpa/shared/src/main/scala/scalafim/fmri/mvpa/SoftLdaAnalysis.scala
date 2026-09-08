package scalafim.fmri.mvpa

import gale.linalg.{DMat, Matrix}
import multivar.core.{MultivarError, SemanticSpace}
import multivar.family.canonical.*
import resample4s.core.{Selection, UnitKey}
import scalafim.fmri.mvpa.predictive.*

enum TrialNuisanceError:
  case Empty
  case RowMismatch(expected: Int, actual: Int)
  case NonFinite(row: Int, column: Int, value: Double)
  case SampleAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case SampleWitnessMismatch
  case Reindexing(error: AxisRefError)
  case Multivar(error: MultivarError)

  def message: String =
    this match
      case Empty                         => "trial nuisance evidence requires rows and columns"
      case RowMismatch(expected, actual) =>
        s"trial nuisance evidence has $actual rows, expected $expected"
      case NonFinite(row, column, value) =>
        s"trial nuisance value at ($row,$column) is non-finite: $value"
      case SampleAxisMismatch(expected, actual) =>
        s"trial nuisance selection belongs to ${actual.value}, expected ${expected.value}"
      case SampleWitnessMismatch =>
        "trial nuisance selection and values use different nominal sample witnesses"
      case Reindexing(error) => error.message
      case Multivar(error)   => error.message

/** Axis-bound trial/sample nuisance evidence. Temporal nuisance remains in the fMRI readout; these columns are selected
  * strictly inside each training partition.
  */
final class TrialNuisanceTable[S <: SemanticSpace] private (
    val samples: AxisRef.Aux[SampleId, S],
    private[mvpa] val values: DMat,
    val fingerprint: String
):
  def columns: Int = values.cols

  private[mvpa] def select(
      by: ReindexingLeg[S, SampleId, SampleId, Selection]
  ): Either[TrialNuisanceError, TrialNuisanceDesign] =
    if samples.identity != by.parentIdentity then
      Left(
        TrialNuisanceError.SampleAxisMismatch(
          samples.identity.fingerprint,
          by.parentIdentity.fingerprint
        )
      )
    else if !(samples.evidence eq by.parentEvidence) then Left(TrialNuisanceError.SampleWitnessMismatch)
    else
      val selected = Matrix.newBuilder(by.size, values.cols)
      var row = 0
      while row < by.size do
        by.sourcePositionAt(row) match
          case Left(error)      => return Left(TrialNuisanceError.Reindexing(error))
          case Right(sourceRow) =>
            var column = 0
            while column < values.cols do
              selected(row, column) = values(sourceRow, column)
              column += 1
        row += 1
      TrialNuisanceDesign
        .from(selected.result())
        .left
        .map(TrialNuisanceError.Multivar.apply)

object TrialNuisanceTable:
  private val Protocol = "scalafim-mvpa-trial-nuisance/v1"

  def apply[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S],
      values: DMat
  ): Either[TrialNuisanceError, TrialNuisanceTable[S]] =
    if values.rows == 0 || values.cols == 0 then Left(TrialNuisanceError.Empty)
    else if values.rows != samples.size then Left(TrialNuisanceError.RowMismatch(samples.size, values.rows))
    else
      var row = 0
      while row < values.rows do
        var column = 0
        while column < values.cols do
          val value = values(row, column)
          if !value.isFinite then return Left(TrialNuisanceError.NonFinite(row, column, value))
          column += 1
        row += 1
      val writer = CanonicalWriter()
      writer.string(Protocol)
      writer.string(samples.identity.fingerprint.value)
      writer.int(values.rows)
      writer.int(values.cols)
      row = 0
      while row < values.rows do
        var column = 0
        while column < values.cols do
          writer.string(java.lang.Double.toHexString(values(row, column)))
          column += 1
        row += 1
      Right(
        new TrialNuisanceTable(
          samples,
          values,
          AxisDigest.sha256Hex(writer.result())
        )
      )

enum SoftLdaConfigurationError:
  case Identity(error: ScientificIdentityError)

  def message: String =
    this match
      case Identity(error) => error.message

final class SoftLdaConfiguration[S <: SemanticSpace] private (
    val withinPolicy: WithinScatterPolicy,
    val objective: LdaObjective,
    val components: SoftLdaComponents,
    val trialNuisance: Option[TrialNuisanceTable[S]],
    val identity: EstimandIdentity
):
  private[mvpa] def kernelConfig: SoftLdaKernelConfig =
    SoftLdaKernelConfig(withinPolicy, objective, components)

object SoftLdaConfiguration:
  def apply[S <: SemanticSpace](
      withinPolicy: WithinScatterPolicy,
      objective: LdaObjective = LdaObjective.FisherRayleigh,
      components: SoftLdaComponents = SoftLdaComponents.Maximum,
      trialNuisance: Option[TrialNuisanceTable[S]] = None
  ): Either[SoftLdaConfigurationError, SoftLdaConfiguration[S]] =
    EstimandIdentity(
      EstimandKind.unsafe("soft-lda-class-membership"),
      Vector(
        "adaptation" -> "inductive-within-domain",
        "components" -> componentIdentity(components),
        "decision-score" -> "uncalibrated-normalized-discriminant-weight",
        "fit-scope" -> "outer-analysis-only",
        "model-disposition" -> "assessment-only",
        "objective" -> objectiveIdentity(objective),
        "trial-nuisance" -> trialNuisance.fold("none")(_.fingerprint),
        "within-scatter" -> withinIdentity(withinPolicy)
      )
    ).left
      .map(SoftLdaConfigurationError.Identity.apply)
      .map: identity =>
        new SoftLdaConfiguration(
          withinPolicy,
          objective,
          components,
          trialNuisance,
          identity
        )

  private[mvpa] def objectiveIdentity(objective: LdaObjective): String =
    objective match
      case LdaObjective.FisherRayleigh => "fisher-rayleigh"
      case LdaObjective.TraceRatio     => "trace-ratio"

  private def componentIdentity(components: SoftLdaComponents): String =
    components match
      case SoftLdaComponents.Maximum      => "maximum"
      case SoftLdaComponents.Fixed(count) => s"fixed:${count.value}"

  private def withinIdentity(policy: WithinScatterPolicy): String =
    policy match
      case WithinScatterPolicy.RequirePositiveDefinite =>
        "require-positive-definite"
      case WithinScatterPolicy.FixedTraceScaledRidge(fraction) =>
        s"fixed-trace-scaled-ridge:${java.lang.Double.toHexString(fraction.value)}"

enum SoftLdaSolverError:
  case Identity(error: ExecutionPlanError)
  case InvalidChoice(detail: String)

  def message: String =
    this match
      case Identity(error)       => error.message
      case InvalidChoice(detail) => detail

final class SoftLdaSolverSettings private (
    val objective: LdaObjective,
    val identity: SolverIdentity
)

object SoftLdaSolverSettings:
  private val Solver = SolverId.unsafe("multivar-soft-lda")

  def apply(
      objective: LdaObjective
  ): Either[SoftLdaSolverError, SoftLdaSolverSettings] =
    SolverIdentity(
      Solver,
      Vector(
        "implementation" -> "multivar-operator-program",
        "objective" -> SoftLdaConfiguration.objectiveIdentity(objective)
      )
    ).left
      .map(SoftLdaSolverError.Identity.apply)
      .map(new SoftLdaSolverSettings(objective, _))

  def choice(settings: SoftLdaSolverSettings): SolverChoice =
    SolverChoice.Selected(settings.identity)

  def from(
      choice: SolverChoice,
      objective: LdaObjective
  ): Either[SoftLdaSolverError, SoftLdaSolverSettings] =
    choice match
      case SolverChoice.Selected(identity) if identity.id == Solver =>
        apply(objective).flatMap: expected =>
          if expected.identity == identity then Right(expected)
          else
            Left(
              SoftLdaSolverError.InvalidChoice(
                "SoftLDA solver identity does not match its scientific objective"
              )
            )
      case SolverChoice.Selected(identity) =>
        Left(
          SoftLdaSolverError.InvalidChoice(
            s"expected '${Solver.value}', obtained '${identity.id.value}'"
          )
        )
      case SolverChoice.NotApplicable =>
        Left(SoftLdaSolverError.InvalidChoice("no SoftLDA solver was selected"))

final class SoftLdaValidationFoldReceipt private[mvpa] (
    val unit: UnitKey,
    val fit: SoftLdaKernelFitReceipt
)

final class SoftLdaValidationEstimate[S <: SemanticSpace] private[mvpa] (
    val predictions: CategoricalPredictions[S],
    val membershipMse: MembershipMeanSquaredError,
    val targetArgmaxAccuracy: MembershipArgmaxAccuracy,
    val targetKind: MembershipTargetKind,
    val configuration: SoftLdaConfiguration[S],
    val solver: SoftLdaSolverSettings,
    val folds: Vector[SoftLdaValidationFoldReceipt],
    private[mvpa] val operatorApplications: Long
)

enum SoftLdaBindRejection:
  case Target(error: ClassMembershipTargetError)
  case Schedule(error: BoundScheduleError)

  def message: String =
    this match
      case Target(error)   => error.message
      case Schedule(error) => error.message

enum SoftLdaCompileError:
  case Schedule(error: BoundScheduleError)
  case Evidence(error: EvidenceTableError)
  case Column(error: ColumnError)
  case Target(error: ClassMembershipTargetError)
  case Nuisance(error: TrialNuisanceError)
  case Kernel(error: SoftLdaError)
  case Categorical(error: CategoricalError)
  case Solver(error: SoftLdaSolverError)
  case DuplicateAssessment(sample: SampleId)
  case MissingAssessment(sample: SampleId)
  case ExecutionEvidence(error: ExecutionReceiptError)

  def message: String =
    this match
      case Schedule(error)             => error.message
      case Evidence(error)             => error.message
      case Column(error)               => error.message
      case Target(error)               => error.message
      case Nuisance(error)             => error.message
      case Kernel(error)               => error.message
      case Categorical(error)          => error.message
      case Solver(error)               => error.message
      case DuplicateAssessment(sample) =>
        s"SoftLDA emitted duplicate assessment for '${sample.value}'"
      case MissingAssessment(sample) =>
        s"SoftLDA emitted no assessment for '${sample.value}'"
      case ExecutionEvidence(error) => error.message

final class SoftLdaValidation[
    S <: SemanticSpace,
    N <: SemanticSpace,
    K
] private (
    val configuration: SoftLdaConfiguration[S]
) extends Estimand[
      MembershipObservationSource[S, N, K],
      ExactClassificationValidation[S]
    ]:
  override type Result = SoftLdaValidationEstimate[S]
  override type Rejection = SoftLdaBindRejection
  override type Failure = SoftLdaCompileError

  override def identity: EstimandIdentity = configuration.identity

  override val defaultBoundaries: RequestedBoundaries =
    RequestedBoundaries.trusted(
      Vector(
        OutputBoundaryIdentity.trusted(
          OutputBoundaryId.unsafe("out-of-fold-predictions")
        ),
        OutputBoundaryIdentity.trusted(
          OutputBoundaryId.unsafe("decision-scores")
        ),
        OutputBoundaryIdentity.trusted(
          OutputBoundaryId.unsafe("lda-fit-evidence")
        )
      )
    )

  override def rejectionMessage(value: SoftLdaBindRejection): String =
    value.message

  override def failureMessage(value: SoftLdaCompileError): String =
    value.message

object SoftLdaValidation:
  def apply[S <: SemanticSpace, N <: SemanticSpace, K](
      configuration: SoftLdaConfiguration[S]
  ): SoftLdaValidation[S, N, K] =
    new SoftLdaValidation(configuration)

final class SoftLdaPrepared private[mvpa] (
    val samples: AxisIdentity,
    val classes: AxisIdentity,
    val design: DesignIdentity
)

object SoftLdaAnalysis:
  given compiler[S <: SemanticSpace, N <: SemanticSpace, K, R]: Compile[
    MembershipObservationSource[S, N, K],
    ExactClassificationValidation[S],
    SoftLdaValidation[S, N, K],
    R
  ] with
    override type Prepared = SoftLdaPrepared

    override def prepare(
        specification: ScientificSpecification[
          MembershipObservationSource[S, N, K],
          ExactClassificationValidation[S],
          SoftLdaValidation[S, N, K],
          R
        ]
    ): Either[specification.Rejection, SoftLdaPrepared] =
      val iterator = specification.design.schedule.iterator
      while iterator.hasNext do
        val (_, bound) = iterator.next()
        bound match
          case Left(error)  => return Left(SoftLdaBindRejection.Schedule(error))
          case Right(split) =>
            ClassMembershipTarget.validateTrainingMass(
              specification.source.target,
              split.analysis
            ) match
              case Left(error) => return Left(SoftLdaBindRejection.Target(error))
              case Right(_)    => ()
      Right(
        new SoftLdaPrepared(
          specification.source.target.samples.identity,
          specification.source.target.classes.identity,
          specification.design.identity
        )
      )

  given task[S <: SemanticSpace, N <: SemanticSpace, K, R]: MeasurementTask[
    MembershipObservationSource[S, N, K],
    ExactClassificationValidation[S],
    SoftLdaValidation[S, N, K],
    R,
    SoftLdaPrepared
  ] with
    override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
      if strategy.representation != ExecutionRepresentation.Operator then
        Left(
          ExecutionPlanError.UnsupportedRepresentation(
            strategy.representation,
            Vector(ExecutionRepresentation.Operator)
          )
        )
      else if strategy.precision != NumericPrecision.Binary64 then
        Left(
          ExecutionPlanError.UnsupportedPrecision(
            strategy.precision,
            Vector(NumericPrecision.Binary64)
          )
        )
      else Right(())

    override def execute(
        plan: BoundScientificPlan[
          MembershipObservationSource[S, N, K],
          ExactClassificationValidation[S],
          SoftLdaValidation[S, N, K],
          R,
          SoftLdaPrepared
        ]
    )(
        measurement: MeasurementEntry[
          plan.specification.source.Neural,
          plan.specification.source.NeuralKey,
          ?,
          R
        ],
        context: TaskContext
    ): Either[
      TaskReportError,
      TaskReport[plan.Result, plan.Rejection, plan.Failure]
    ] =
      SoftLdaSolverSettings.from(
        context.strategy.solver,
        plan.specification.estimand.configuration.objective
      ) match
        case Left(error) =>
          TaskReport.failed(
            SoftLdaCompileError.Solver(error),
            context.strategy.target
          )
        case Right(solver) =>
          SoftLdaCompiler.run(
            plan.specification.source.evidence,
            plan.specification.source.target,
            plan.specification.design,
            measurement.measurement,
            plan.specification.estimand.configuration,
            solver
          ) match
            case Left(error)   => TaskReport.failed(error, context.strategy.target)
            case Right(result) =>
              val convergence = Vector.newBuilder[IterativeConvergence]
              var failure: Option[SoftLdaCompileError] = None
              val iterator = result.folds.iterator
              while iterator.hasNext && failure.isEmpty do
                val fold = iterator.next()
                IterativeConvergence(
                  ExecutionScope.Measurement(measurement.measurement.identity.id),
                  solver.identity,
                  ConvergenceOutcome.Converged,
                  0,
                  fold.fit.fit.diagnostics.residual
                ) match
                  case Left(error) =>
                    failure = Some(SoftLdaCompileError.ExecutionEvidence(error))
                  case Right(value) => convergence += value
              failure match
                case Some(error) => TaskReport.failed(error, context.strategy.target)
                case None        =>
                  TaskReport.success(
                    result,
                    context.strategy.target,
                    operatorApplications = result.operatorApplications,
                    convergence = convergence.result()
                  )

object SoftLdaCompiler:
  def run[S <: SemanticSpace, N <: SemanticSpace, K, LocalKey](
      evidence: EvidenceTable[S, N, SampleId, K],
      target: ClassMembershipTarget[S],
      design: ExactClassificationValidation[S],
      measurement: Measurement[N, K, LocalKey],
      configuration: SoftLdaConfiguration[S],
      solver: SoftLdaSolverSettings
  ): Either[SoftLdaCompileError, SoftLdaValidationEstimate[S]] =
    evidence
      .measureColumns(measurement)
      .left
      .map(SoftLdaCompileError.Evidence.apply)
      .flatMap: measured =>
        evaluate(measured, target, design, configuration, solver)

  private def evaluate[S <: SemanticSpace, N <: SemanticSpace, K](
      measured: EvidenceTable[S, N, SampleId, K],
      target: ClassMembershipTarget[S],
      design: ExactClassificationValidation[S],
      configuration: SoftLdaConfiguration[S],
      solver: SoftLdaSolverSettings
  ): Either[SoftLdaCompileError, SoftLdaValidationEstimate[S]] =
    val scoreBuilder = Matrix.newBuilder(target.samples.size, target.classes.size)
    val predicted = new Array[ClassId](target.samples.size)
    val seen = Array.fill(target.samples.size)(false)
    val receipts = Vector.newBuilder[SoftLdaValidationFoldReceipt]
    var squaredError = 0.0
    var correct = 0
    var assessed = 0
    var operatorApplications = 0L

    val keys = design.schedule.keys
    var foldPosition = 0
    while foldPosition < keys.length do
      val unit = keys(foldPosition)
      val split = design.at(unit) match
        case Left(error)  => return Left(SoftLdaCompileError.Schedule(error))
        case Right(value) => value
      val train = measured.restrictRows(split.analysis) match
        case Left(error)  => return Left(SoftLdaCompileError.Evidence(error))
        case Right(value) => value
      val test = measured.restrictRows(split.assessment) match
        case Left(error)  => return Left(SoftLdaCompileError.Evidence(error))
        case Right(value) => value
      val membership = target.select(split.analysis) match
        case Left(error)  => return Left(SoftLdaCompileError.Target(error))
        case Right(value) => value
      val nuisance = configuration.trialNuisance match
        case None        => None
        case Some(table) =>
          table.select(split.analysis) match
            case Left(error)  => return Left(SoftLdaCompileError.Nuisance(error))
            case Right(value) => Some(value)
      val fitted = SoftLdaKernel.fit(
        train,
        test,
        target.classes,
        membership,
        nuisance,
        configuration.kernelConfig,
        s"repeat-${unit.repeat}-fold-${unit.fold}"
      ) match
        case Left(error)  => return Left(SoftLdaCompileError.Kernel(error))
        case Right(value) => value

      var assessmentPosition = 0
      while assessmentPosition < split.assessment.size do
        val sourcePosition = split.assessment.sourcePositionAt(assessmentPosition) match
          case Left(error) =>
            return Left(
              SoftLdaCompileError.Target(
                ClassMembershipTargetError.Reindexing(error)
              )
            )
          case Right(value) => value
        if seen(sourcePosition) then
          return Left(
            SoftLdaCompileError.DuplicateAssessment(target.samples.keys(sourcePosition))
          )
        seen(sourcePosition) = true
        var bestClass = 0
        var klass = 0
        while klass < target.classes.size do
          val score = fitted.decisionWeights(assessmentPosition, klass)
          scoreBuilder(sourcePosition, klass) = score
          val difference = score - target.values(sourcePosition, klass)
          squaredError += difference * difference
          if klass > 0 && score > fitted.decisionWeights(assessmentPosition, bestClass) then bestClass = klass
          klass += 1
        val predictedClass = target.classes.keys(bestClass)
        predicted(sourcePosition) = predictedClass
        if predictedClass == target.argmaxAt(sourcePosition) then correct += 1
        assessed += 1
        assessmentPosition += 1

      receipts += new SoftLdaValidationFoldReceipt(unit, fitted.receipt)
      operatorApplications += fitted.receipt.operatorApplications
      foldPosition += 1

    val missing = seen.indexWhere(!_)
    if missing >= 0 then Left(SoftLdaCompileError.MissingAssessment(target.samples.keys(missing)))
    else
      for
        scores <- ClassScoreTable(target.samples, target.classes, scoreBuilder.result()).left
          .map(SoftLdaCompileError.Categorical.apply)
        predictedColumn <- Column
          .fromIArray(target.samples, IArray.unsafeFromArray(predicted))
          .left
          .map(SoftLdaCompileError.Column.apply)
        predictions <- CategoricalPredictions(
          target.samples,
          target.classes,
          predictedColumn,
          scores = Some(scores)
        ).left.map(SoftLdaCompileError.Categorical.apply)
      yield new SoftLdaValidationEstimate(
        predictions,
        MembershipMeanSquaredError.unsafe(
          squaredError / (assessed.toDouble * target.classes.size.toDouble)
        ),
        MembershipArgmaxAccuracy.unsafe(correct.toDouble / assessed.toDouble),
        target.kind,
        configuration,
        solver,
        receipts.result(),
        operatorApplications
      )
