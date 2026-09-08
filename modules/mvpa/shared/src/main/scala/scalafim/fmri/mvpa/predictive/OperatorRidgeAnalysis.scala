package scalafim.fmri.mvpa.predictive

import gale.linalg.{DMat, Matrix}
import multivar.core.SemanticSpace
import resample4s.core.{Selection, UnitKey}
import scalafim.fmri.mvpa.*

enum MembershipTargetKind:
  case HardIncidence
  case SoftSimplex

  def identity: String =
    this match
      case HardIncidence => "hard-incidence"
      case SoftSimplex   => "soft-simplex"

enum ClassMembershipTargetError:
  case Axis(error: CategoricalError)
  case SampleAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case SampleWitnessMismatch
  case ShapeMismatch(
      expectedRows: Int,
      expectedClasses: Int,
      actualRows: Int,
      actualClasses: Int
  )
  case InvalidMass(row: Int, classPosition: Int, value: Double)
  case RowSum(row: Int, actual: Double)
  case MissingClassMass(classId: ClassId)
  case Reindexing(error: AxisRefError)

  def message: String =
    this match
      case Axis(error)                          => error.message
      case SampleAxisMismatch(expected, actual) =>
        s"membership target belongs to sample axis ${actual.value}, expected ${expected.value}"
      case SampleWitnessMismatch =>
        "membership target and sample axis use different nominal witnesses"
      case ShapeMismatch(expectedRows, expectedClasses, actualRows, actualClasses) =>
        s"membership target expected ${expectedRows}x$expectedClasses values, obtained ${actualRows}x$actualClasses"
      case InvalidMass(row, classPosition, value) =>
        s"membership mass at row $row, class $classPosition must be finite and in [0, 1], obtained $value"
      case RowSum(row, actual) =>
        s"membership masses at row $row sum to $actual rather than one"
      case MissingClassMass(classId) =>
        s"class '${classId.value}' has no membership mass"
      case Reindexing(error) => error.message

/** Sample-by-class target mass bound to exact sample and class axes.
  *
  * A soft simplex row is a fractional training target. It is deliberately not a prediction output and carries no
  * calibration semantics.
  */
final class ClassMembershipTarget[S <: SemanticSpace] private (
    val samples: AxisRef.Aux[SampleId, S],
    val classes: AxisRef[ClassId],
    private[mvpa] val values: DMat,
    val kind: MembershipTargetKind,
    private[predictive] val fingerprint: String
):
  def rows: Int = values.rows
  def classCount: Int = values.cols

  private[mvpa] def select(
      by: ReindexingLeg[S, SampleId, SampleId, Selection]
  ): Either[ClassMembershipTargetError, DMat] =
    if samples.identity != by.parentIdentity then
      Left(
        ClassMembershipTargetError.SampleAxisMismatch(
          samples.identity.fingerprint,
          by.parentIdentity.fingerprint
        )
      )
    else if !(samples.evidence eq by.parentEvidence) then Left(ClassMembershipTargetError.SampleWitnessMismatch)
    else
      val selected = Matrix.newBuilder(by.size, classes.size)
      var row = 0
      while row < by.size do
        by.sourcePositionAt(row) match
          case Left(error) =>
            return Left(ClassMembershipTargetError.Reindexing(error))
          case Right(sourceRow) =>
            var klass = 0
            while klass < classes.size do
              selected(row, klass) = values(sourceRow, klass)
              klass += 1
        row += 1
      Right(selected.result())

  private[mvpa] def argmaxAt(row: Int): ClassId =
    var best = 0
    var klass = 1
    while klass < classes.size do
      if values(row, klass) > values(row, best) then best = klass
      klass += 1
    classes.keys(best)

object ClassMembershipTarget:
  private val SumTolerance = 1e-10
  private val MassTolerance = 1e-12
  private val Protocol = "scalafim-mvpa-class-membership/v1"

  def hard[S <: SemanticSpace](
      target: CategoricalTarget[S]
  ): Either[ClassMembershipTargetError, ClassMembershipTarget[S]] =
    val values = Matrix.newBuilder(target.samples.size, target.classes.size)
    var row = 0
    while row < target.samples.size do
      target.classes.positionOf(target.labels.values(row)) match
        case None =>
          return Left(
            ClassMembershipTargetError.Axis(
              CategoricalError.UnknownClass(target.labels.values(row))
            )
          )
        case Some(classPosition) =>
          values(row, classPosition) = 1.0
      row += 1
    Right(
      build(
        target.samples,
        target.classes,
        values.result(),
        MembershipTargetKind.HardIncidence
      )
    )

  def simplex[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S],
      classes: AxisRef[ClassId],
      values: DMat
  ): Either[ClassMembershipTargetError, ClassMembershipTarget[S]] =
    for
      _ <- CategoricalTarget
        .validateClassAxis(classes)
        .left
        .map(ClassMembershipTargetError.Axis.apply)
      _ <- validate(samples.size, classes, values, requireAllClasses = true)
    yield build(samples, classes, values, MembershipTargetKind.SoftSimplex)

  private[mvpa] def validateTrainingMass[S <: SemanticSpace](
      target: ClassMembershipTarget[S],
      selection: ReindexingLeg[S, SampleId, SampleId, Selection]
  ): Either[ClassMembershipTargetError, Unit] =
    target
      .select(selection)
      .flatMap: selected =>
        validate(selection.size, target.classes, selected, requireAllClasses = true)

  private def validate(
      expectedRows: Int,
      classes: AxisRef[ClassId],
      values: DMat,
      requireAllClasses: Boolean
  ): Either[ClassMembershipTargetError, Unit] =
    if values.rows != expectedRows || values.cols != classes.size then
      Left(
        ClassMembershipTargetError.ShapeMismatch(
          expectedRows,
          classes.size,
          values.rows,
          values.cols
        )
      )
    else
      var row = 0
      while row < values.rows do
        var sum = 0.0
        var klass = 0
        while klass < values.cols do
          val value = values(row, klass)
          if !value.isFinite || value < 0.0 || value > 1.0 then
            return Left(
              ClassMembershipTargetError.InvalidMass(row, klass, value)
            )
          sum += value
          klass += 1
        if math.abs(sum - 1.0) > SumTolerance then return Left(ClassMembershipTargetError.RowSum(row, sum))
        row += 1
      if requireAllClasses then
        var klass = 0
        while klass < values.cols do
          var mass = 0.0
          row = 0
          while row < values.rows do
            mass += values(row, klass)
            row += 1
          if mass <= MassTolerance then
            return Left(
              ClassMembershipTargetError.MissingClassMass(classes.keys(klass))
            )
          klass += 1
      Right(())

  private def build[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S],
      classes: AxisRef[ClassId],
      values: DMat,
      kind: MembershipTargetKind
  ): ClassMembershipTarget[S] =
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.string(samples.identity.fingerprint.value)
    writer.string(classes.identity.fingerprint.value)
    writer.string(kind.identity)
    writer.int(values.rows)
    writer.int(values.cols)
    var row = 0
    while row < values.rows do
      var klass = 0
      while klass < values.cols do
        writer.string(java.lang.Double.toHexString(values(row, klass)))
        klass += 1
      row += 1
    new ClassMembershipTarget(
      samples,
      classes,
      values,
      kind,
      AxisDigest.sha256Hex(writer.result())
    )

enum MembershipObservationSourceError:
  case Observations(error: ObservationsError)
  case Identity(error: ScientificIdentityError)
  case SampleAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case SampleWitnessMismatch

  def message: String =
    this match
      case Observations(error)                  => error.message
      case Identity(error)                      => error.message
      case SampleAxisMismatch(expected, actual) =>
        s"membership target sample axis ${actual.value} does not match evidence ${expected.value}"
      case SampleWitnessMismatch =>
        "membership target and evidence use different nominal sample witnesses"

/** Observation evidence paired with a class-membership target. */
final class MembershipObservationSource[
    Samples <: SemanticSpace,
    NeuralSpace <: SemanticSpace,
    NeuralCoordinate
] private (
    val observations: Observations[Samples, NeuralSpace, NeuralCoordinate],
    val target: ClassMembershipTarget[Samples],
    val classAxisName: ScientificAxisName,
    val identity: ScientificSourceIdentity
) extends ScientificSource:
  override type Neural = NeuralSpace
  override type NeuralKey = NeuralCoordinate

  def evidence: EvidenceTable[Samples, NeuralSpace, SampleId, NeuralCoordinate] =
    observations.evidence

  def sampleAxisName: ScientificAxisName = observations.sampleAxisName
  def neuralAxisName: ScientificAxisName = observations.neuralAxisName

  override def neuralAxis: AxisRef.Aux[NeuralCoordinate, NeuralSpace] =
    observations.neuralAxis

  def operatorRidge(
      configuration: OperatorRidgeConfiguration
  ): OperatorRidgeValidation[Samples, NeuralSpace, NeuralCoordinate] =
    new OperatorRidgeValidation(configuration)

object MembershipObservationSource:
  private val Protocol = "scalafim-mvpa-membership-observations/v1"

  def apply[S <: SemanticSpace, N <: SemanticSpace, K](
      evidence: EvidenceTable[S, N, SampleId, K],
      target: ClassMembershipTarget[S],
      sampleAxisName: ScientificAxisName = ScientificAxisName.unsafe("samples"),
      neuralAxisName: ScientificAxisName = ScientificAxisName.unsafe("neural"),
      classAxisName: ScientificAxisName = ScientificAxisName.unsafe("classes")
  ): Either[
    MembershipObservationSourceError,
    MembershipObservationSource[S, N, K]
  ] =
    Observations(evidence, sampleAxisName, neuralAxisName).left
      .map(MembershipObservationSourceError.Observations.apply)
      .flatMap: observations =>
        if observations.samples.identity != target.samples.identity then
          Left(
            MembershipObservationSourceError.SampleAxisMismatch(
              observations.samples.identity.fingerprint,
              target.samples.identity.fingerprint
            )
          )
        else if !(observations.samples.evidence eq target.samples.evidence) then
          Left(MembershipObservationSourceError.SampleWitnessMismatch)
        else
          ScientificSourceIdentity(
            ScientificSourceKind.unsafe("membership-observations"),
            Vector(
              ScientificSourceAxis(sampleAxisName, observations.samples.identity),
              ScientificSourceAxis(neuralAxisName, observations.neuralAxis.identity),
              ScientificSourceAxis(classAxisName, target.classes.identity)
            ),
            Vector(
              "membership-kind" -> target.kind.identity,
              "membership-target" -> target.fingerprint,
              "observations" -> observations.identity.fingerprint.value,
              "protocol" -> Protocol
            )
          ).left
            .map(MembershipObservationSourceError.Identity.apply)
            .map: identity =>
              new MembershipObservationSource(
                observations,
                target,
                classAxisName,
                identity
              )

enum OperatorRidgeConfigurationError:
  case InvalidPenalty(value: Double)
  case Identity(error: ScientificIdentityError)

  def message: String =
    this match
      case InvalidPenalty(value) =>
        s"operator-ridge penalty must be positive and finite, obtained $value"
      case Identity(error) => error.message

final class OperatorRidgeConfiguration private (
    val penalty: RidgePenalty,
    val identity: EstimandIdentity
)

object OperatorRidgeConfiguration:
  def fixed(
      penalty: Double
  ): Either[OperatorRidgeConfigurationError, OperatorRidgeConfiguration] =
    RidgePenalty(penalty).left
      .map(_ => OperatorRidgeConfigurationError.InvalidPenalty(penalty))
      .flatMap: admitted =>
        EstimandIdentity(
          EstimandKind.unsafe("operator-ridge-class-membership"),
          Vector(
            "adaptation" -> ClassifierAdaptation.InductiveWithinDomain.identity,
            "decision-score" -> "uncalibrated-membership-regression",
            "fit-scope" -> "outer-analysis-only",
            "model-disposition" -> "assessment-only",
            "penalty" -> java.lang.Double.toHexString(admitted.value)
          )
        ).left
          .map(OperatorRidgeConfigurationError.Identity.apply)
          .map(new OperatorRidgeConfiguration(admitted, _))

opaque type MembershipMeanSquaredError = Double

object MembershipMeanSquaredError:
  private[mvpa] def unsafe(value: Double): MembershipMeanSquaredError = value
  extension (value: MembershipMeanSquaredError) inline def value: Double = value

opaque type MembershipArgmaxAccuracy = Double

object MembershipArgmaxAccuracy:
  private[mvpa] def unsafe(value: Double): MembershipArgmaxAccuracy = value
  extension (value: MembershipArgmaxAccuracy) inline def value: Double = value

final class OperatorRidgeValidationFoldReceipt private[predictive] (
    val unit: UnitKey,
    val analysisSamples: AxisIdentity,
    val assessmentSamples: AxisIdentity,
    val fit: OperatorRidgeKernelFitReceipt
)

/** Exact-once membership-regression result with uncalibrated decision scores. */
final class OperatorRidgeValidationEstimate[S <: SemanticSpace] private[predictive] (
    val predictions: CategoricalPredictions[S],
    val membershipMse: MembershipMeanSquaredError,
    val targetArgmaxAccuracy: MembershipArgmaxAccuracy,
    val targetKind: MembershipTargetKind,
    val scoreKind: ClassificationScoreKind,
    val configuration: OperatorRidgeConfiguration,
    val solver: OperatorRidgeSolverSettings,
    val folds: Vector[OperatorRidgeValidationFoldReceipt],
    private[predictive] val operatorApplications: Long
)

enum OperatorRidgeBindRejection:
  case Target(error: ClassMembershipTargetError)
  case Schedule(error: BoundScheduleError)

  def message: String =
    this match
      case Target(error)   => error.message
      case Schedule(error) => error.message

enum OperatorRidgeCompileError:
  case Schedule(error: BoundScheduleError)
  case Evidence(error: EvidenceTableError)
  case Column(error: ColumnError)
  case Target(error: ClassMembershipTargetError)
  case Kernel(error: OperatorRidgeError)
  case Categorical(error: CategoricalError)
  case InvalidSolver(error: OperatorRidgeError)
  case DuplicateAssessment(sample: SampleId)
  case MissingAssessment(sample: SampleId)
  case ExecutionEvidence(error: ExecutionReceiptError)

  def message: String =
    this match
      case Schedule(error)             => error.message
      case Evidence(error)             => error.message
      case Column(error)               => error.message
      case Target(error)               => error.message
      case Kernel(error)               => error.message
      case Categorical(error)          => error.message
      case InvalidSolver(error)        => error.message
      case DuplicateAssessment(sample) =>
        s"operator ridge emitted duplicate assessment for '${sample.value}'"
      case MissingAssessment(sample) =>
        s"operator ridge emitted no assessment for '${sample.value}'"
      case ExecutionEvidence(error) => error.message

final class OperatorRidgeValidation[
    S <: SemanticSpace,
    N <: SemanticSpace,
    K
] private[predictive] (
    val configuration: OperatorRidgeConfiguration
) extends Estimand[
      MembershipObservationSource[S, N, K],
      ExactClassificationValidation[S]
    ]:
  override type Result = OperatorRidgeValidationEstimate[S]
  override type Rejection = OperatorRidgeBindRejection
  override type Failure = OperatorRidgeCompileError

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
          OutputBoundaryId.unsafe("solver-convergence")
        )
      )
    )

  override def rejectionMessage(value: OperatorRidgeBindRejection): String =
    value.message

  override def failureMessage(value: OperatorRidgeCompileError): String =
    value.message

final class OperatorRidgePrepared private[predictive] (
    val samples: AxisIdentity,
    val classes: AxisIdentity,
    val design: DesignIdentity
)

object OperatorRidgeAnalysis:
  given compiler[S <: SemanticSpace, N <: SemanticSpace, K, R]: Compile[
    MembershipObservationSource[S, N, K],
    ExactClassificationValidation[S],
    OperatorRidgeValidation[S, N, K],
    R
  ] with
    override type Prepared = OperatorRidgePrepared

    override def prepare(
        specification: ScientificSpecification[
          MembershipObservationSource[S, N, K],
          ExactClassificationValidation[S],
          OperatorRidgeValidation[S, N, K],
          R
        ]
    ): Either[specification.Rejection, OperatorRidgePrepared] =
      val iterator = specification.design.schedule.iterator
      while iterator.hasNext do
        val (_, bound) = iterator.next()
        bound match
          case Left(error) =>
            return Left(OperatorRidgeBindRejection.Schedule(error))
          case Right(split) =>
            ClassMembershipTarget.validateTrainingMass(
              specification.source.target,
              split.analysis
            ) match
              case Left(error) =>
                return Left(OperatorRidgeBindRejection.Target(error))
              case Right(_) => ()
      Right(
        new OperatorRidgePrepared(
          specification.source.target.samples.identity,
          specification.source.target.classes.identity,
          specification.design.identity
        )
      )

  given task[S <: SemanticSpace, N <: SemanticSpace, K, R]: MeasurementTask[
    MembershipObservationSource[S, N, K],
    ExactClassificationValidation[S],
    OperatorRidgeValidation[S, N, K],
    R,
    OperatorRidgePrepared
  ] with
    override def validate(
        strategy: ExecutionStrategy
    ): Either[ExecutionPlanError, Unit] =
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
      else
        OperatorRidgeSolverSettings.from(strategy.solver) match
          case Left(_)  => Left(ExecutionPlanError.UnsupportedSolver(strategy.solver))
          case Right(_) => Right(())

    override def execute(
        plan: BoundScientificPlan[
          MembershipObservationSource[S, N, K],
          ExactClassificationValidation[S],
          OperatorRidgeValidation[S, N, K],
          R,
          OperatorRidgePrepared
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
      OperatorRidgeSolverSettings.from(context.strategy.solver) match
        case Left(error) =>
          TaskReport.failed(
            OperatorRidgeCompileError.InvalidSolver(error),
            context.strategy.target
          )
        case Right(solver) =>
          OperatorRidgeCompiler.run(
            plan.specification.source.evidence,
            plan.specification.source.target,
            plan.specification.design,
            measurement.measurement,
            plan.specification.estimand.configuration,
            solver
          ) match
            case Left(error) =>
              TaskReport.failed(error, context.strategy.target)
            case Right(result) =>
              val convergence = Vector.newBuilder[IterativeConvergence]
              var failure: Option[OperatorRidgeCompileError] = None
              val foldIterator = result.folds.iterator
              while foldIterator.hasNext && failure.isEmpty do
                val fold = foldIterator.next()
                val classIterator = fold.fit.classFits.iterator
                while classIterator.hasNext && failure.isEmpty do
                  val classFit = classIterator.next()
                  IterativeConvergence(
                    ExecutionScope.Measurement(measurement.measurement.identity.id),
                    solver.identity,
                    ConvergenceOutcome.Converged,
                    classFit.iterations,
                    classFit.normalResidual
                  ) match
                    case Left(error) =>
                      failure = Some(
                        OperatorRidgeCompileError.ExecutionEvidence(error)
                      )
                    case Right(value) => convergence += value
              failure match
                case Some(error) =>
                  TaskReport.failed(error, context.strategy.target)
                case None =>
                  TaskReport.success(
                    result,
                    context.strategy.target,
                    operatorApplications = result.operatorApplications,
                    convergence = convergence.result()
                  )

object OperatorRidgeCompiler:
  def run[
      S <: SemanticSpace,
      N <: SemanticSpace,
      K,
      LocalKey
  ](
      evidence: EvidenceTable[S, N, SampleId, K],
      target: ClassMembershipTarget[S],
      design: ExactClassificationValidation[S],
      measurement: Measurement[N, K, LocalKey],
      configuration: OperatorRidgeConfiguration,
      solver: OperatorRidgeSolverSettings
  ): Either[OperatorRidgeCompileError, OperatorRidgeValidationEstimate[S]] =
    for
      measured <- evidence
        .measureColumns(measurement)
        .left
        .map(OperatorRidgeCompileError.Evidence.apply)
      result <- evaluate(
        measured,
        target,
        design,
        configuration,
        solver
      )
    yield result

  private def evaluate[
      S <: SemanticSpace,
      N <: SemanticSpace,
      K
  ](
      measured: EvidenceTable[S, N, SampleId, K],
      target: ClassMembershipTarget[S],
      design: ExactClassificationValidation[S],
      configuration: OperatorRidgeConfiguration,
      solver: OperatorRidgeSolverSettings
  ): Either[OperatorRidgeCompileError, OperatorRidgeValidationEstimate[S]] =
    val scoreBuilder = Matrix.newBuilder(target.samples.size, target.classes.size)
    val predicted = new Array[ClassId](target.samples.size)
    val seen = Array.fill(target.samples.size)(false)
    val receipts = Vector.newBuilder[OperatorRidgeValidationFoldReceipt]
    var squaredError = 0.0
    var correct = 0
    var assessed = 0
    var operatorApplications = 0L

    val keys = design.schedule.keys
    var foldPosition = 0
    while foldPosition < keys.length do
      val unit = keys(foldPosition)
      val split = design.at(unit) match
        case Left(error)  => return Left(OperatorRidgeCompileError.Schedule(error))
        case Right(value) => value
      val train = measured.restrictRows(split.analysis) match
        case Left(error)  => return Left(OperatorRidgeCompileError.Evidence(error))
        case Right(value) => value
      val test = measured.restrictRows(split.assessment) match
        case Left(error)  => return Left(OperatorRidgeCompileError.Evidence(error))
        case Right(value) => value
      val trainTarget = target.select(split.analysis) match
        case Left(error)  => return Left(OperatorRidgeCompileError.Target(error))
        case Right(value) => value
      val model = OperatorRidgeKernel.fit(
        train,
        target.classes,
        trainTarget,
        configuration.penalty,
        solver,
        unit.toString
      ) match
        case Left(error)  => return Left(OperatorRidgeCompileError.Kernel(error))
        case Right(value) => value
      val scores = model.predict(test) match
        case Left(error)  => return Left(OperatorRidgeCompileError.Kernel(error))
        case Right(value) => value

      var assessmentPosition = 0
      while assessmentPosition < split.assessment.size do
        val sourcePosition = split.assessment.sourcePositionAt(assessmentPosition) match
          case Left(error) =>
            return Left(
              OperatorRidgeCompileError.Target(
                ClassMembershipTargetError.Reindexing(error)
              )
            )
          case Right(value) => value
        if seen(sourcePosition) then
          return Left(
            OperatorRidgeCompileError.DuplicateAssessment(
              target.samples.keys(sourcePosition)
            )
          )
        seen(sourcePosition) = true
        var bestClass = 0
        var klass = 0
        while klass < target.classes.size do
          val score = scores(assessmentPosition, klass)
          scoreBuilder(sourcePosition, klass) = score
          val difference = score - target.values(sourcePosition, klass)
          squaredError += difference * difference
          if klass > 0 && score > scores(assessmentPosition, bestClass) then bestClass = klass
          klass += 1
        val predictedClass = target.classes.keys(bestClass)
        predicted(sourcePosition) = predictedClass
        if predictedClass == target.argmaxAt(sourcePosition) then correct += 1
        assessed += 1
        assessmentPosition += 1

      receipts += new OperatorRidgeValidationFoldReceipt(
        unit,
        split.analysis.child.identity,
        split.assessment.child.identity,
        model.receipt
      )
      operatorApplications += model.receipt.operatorApplications + target.classes.size.toLong
      foldPosition += 1

    val missing = seen.indexWhere(!_)
    if missing >= 0 then
      Left(
        OperatorRidgeCompileError.MissingAssessment(target.samples.keys(missing))
      )
    else
      val scoreValues = scoreBuilder.result()
      for
        scoreTable <- ClassScoreTable(target.samples, target.classes, scoreValues).left
          .map(OperatorRidgeCompileError.Categorical.apply)
        predictedColumn <- Column
          .fromIArray(target.samples, IArray.unsafeFromArray(predicted))
          .left
          .map(OperatorRidgeCompileError.Column.apply)
        predictions <- CategoricalPredictions(
          target.samples,
          target.classes,
          predictedColumn,
          scores = Some(scoreTable)
        ).left.map(OperatorRidgeCompileError.Categorical.apply)
      yield new OperatorRidgeValidationEstimate(
        predictions,
        MembershipMeanSquaredError.unsafe(
          squaredError / (assessed.toDouble * target.classes.size.toDouble)
        ),
        MembershipArgmaxAccuracy.unsafe(correct.toDouble / assessed.toDouble),
        target.kind,
        ClassificationScoreKind.UncalibratedDecision,
        configuration,
        solver,
        receipts.result(),
        operatorApplications
      )
