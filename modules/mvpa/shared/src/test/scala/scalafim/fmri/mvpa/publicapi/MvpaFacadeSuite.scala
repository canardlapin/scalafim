package scalafim.fmri.mvpa.publicapi

import resample4s.core.IndexSpace
import resample4s.core.Injection
import scalafim.fmri.mvpa.*

final class ExternalSource(
    val samples: AxisRef[SampleId],
    val features: AxisRef[FeatureId],
    val identity: ScientificSourceIdentity
) extends ScientificSource:
  override type Neural = features.Id
  override type NeuralKey = FeatureId

  override val neuralAxisName: ScientificAxisName =
    ExternalFixtures.NeuralAxis

  override def neuralAxis: AxisRef.Aux[FeatureId, Neural] =
    features

final class ExternalDesign(
    val identity: DesignIdentity,
    val referencedAxes: Vector[DesignAxisReference]
) extends EvidenceDesign

final case class PredictiveEstimate(outOfFoldPredictions: Int)
final case class RelationalEstimate(effectPairs: Int)

enum ExternalRejection:
  case MissingTrainingCapability

enum ExternalFailure:
  case NumericalFailure

case object PredictiveQuestion extends Estimand[ExternalSource, ExternalDesign]:
  override type Result = PredictiveEstimate
  override type Rejection = ExternalRejection
  override type Failure = ExternalFailure

  override val identity: EstimandIdentity =
    EstimandIdentity(
      EstimandKind.unsafe("external-prediction"),
      Vector("metric" -> "accuracy")
    ).toOption.get

  override val defaultBoundaries: RequestedBoundaries =
    ExternalFixtures.boundaries("out-of-fold-predictions")

  override def rejectionMessage(value: ExternalRejection): String =
    value match
      case ExternalRejection.MissingTrainingCapability =>
        "training role lacks the required prediction capability"

  override def failureMessage(value: ExternalFailure): String =
    value match
      case ExternalFailure.NumericalFailure => "predictive kernel failed"

case object RelationalQuestion extends Estimand[ExternalSource, ExternalDesign]:
  override type Result = RelationalEstimate
  override type Rejection = ExternalRejection
  override type Failure = ExternalFailure

  override val identity: EstimandIdentity =
    EstimandIdentity(
      EstimandKind.unsafe("external-relation"),
      Vector("query" -> "cross-partition")
    ).toOption.get

  override val defaultBoundaries: RequestedBoundaries =
    ExternalFixtures.boundaries("effect-form")

  override def rejectionMessage(value: ExternalRejection): String =
    value match
      case ExternalRejection.MissingTrainingCapability =>
        "training role lacks the required relation capability"

  override def failureMessage(value: ExternalFailure): String =
    value match
      case ExternalFailure.NumericalFailure => "relational kernel failed"

case object NeedsTrainingCapability extends Estimand[ExternalSource, ExternalDesign]:
  override type Result = PredictiveEstimate
  override type Rejection = ExternalRejection
  override type Failure = ExternalFailure

  override val identity: EstimandIdentity =
    EstimandIdentity(EstimandKind.unsafe("needs-training-capability")).toOption.get

  override val defaultBoundaries: RequestedBoundaries =
    ExternalFixtures.boundaries("estimate")

  override def rejectionMessage(value: ExternalRejection): String =
    value match
      case ExternalRejection.MissingTrainingCapability =>
        "training role lacks the residual-moments capability"

  override def failureMessage(value: ExternalFailure): String =
    value match
      case ExternalFailure.NumericalFailure => "capability example failed"

final case class PredictivePrepared(sampleCount: Int)
final case class RelationalPrepared(partitionCount: Int)

object ExternalExtensions:
  given Compile[
    ExternalSource,
    ExternalDesign,
    PredictiveQuestion.type,
    NoRendition.type
  ] with
    override type Prepared = PredictivePrepared

    override def prepare(
        specification: ScientificSpecification[
          ExternalSource,
          ExternalDesign,
          PredictiveQuestion.type,
          NoRendition.type
        ]
    ): Either[specification.Rejection, PredictivePrepared] =
      Right(PredictivePrepared(specification.source.samples.size))

  given Compile[
    ExternalSource,
    ExternalDesign,
    RelationalQuestion.type,
    NoRendition.type
  ] with
    override type Prepared = RelationalPrepared

    override def prepare(
        specification: ScientificSpecification[
          ExternalSource,
          ExternalDesign,
          RelationalQuestion.type,
          NoRendition.type
        ]
    ): Either[specification.Rejection, RelationalPrepared] =
      Right(RelationalPrepared(specification.design.referencedAxes.size))

  given Compile[
    ExternalSource,
    ExternalDesign,
    NeedsTrainingCapability.type,
    NoRendition.type
  ] with
    override type Prepared = PredictivePrepared

    override def prepare(
        specification: ScientificSpecification[
          ExternalSource,
          ExternalDesign,
          NeedsTrainingCapability.type,
          NoRendition.type
        ]
    ): Either[specification.Rejection, PredictivePrepared] =
      Left(ExternalRejection.MissingTrainingCapability)

  given MeasurementTask[
    ExternalSource,
    ExternalDesign,
    PredictiveQuestion.type,
    NoRendition.type,
    PredictivePrepared
  ] with
    override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
      Right(())

    override def execute(
        plan: BoundScientificPlan[
          ExternalSource,
          ExternalDesign,
          PredictiveQuestion.type,
          NoRendition.type,
          PredictivePrepared
        ]
    )(
        measurement: MeasurementEntry[
          plan.specification.source.Neural,
          plan.specification.source.NeuralKey,
          ?,
          NoRendition.type
        ],
        context: TaskContext
    ): Either[
      TaskReportError,
      TaskReport[plan.Result, plan.Rejection, plan.Failure]
    ] =
      TaskReport.success(
        PredictiveEstimate(plan.prepared.sampleCount),
        context.strategy.target
      )

  given MeasurementTask[
    ExternalSource,
    ExternalDesign,
    RelationalQuestion.type,
    NoRendition.type,
    RelationalPrepared
  ] with
    override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
      Right(())

    override def execute(
        plan: BoundScientificPlan[
          ExternalSource,
          ExternalDesign,
          RelationalQuestion.type,
          NoRendition.type,
          RelationalPrepared
        ]
    )(
        measurement: MeasurementEntry[
          plan.specification.source.Neural,
          plan.specification.source.NeuralKey,
          ?,
          NoRendition.type
        ],
        context: TaskContext
    ): Either[
      TaskReportError,
      TaskReport[plan.Result, plan.Rejection, plan.Failure]
    ] =
      TaskReport.success(
        RelationalEstimate(plan.prepared.partitionCount),
        context.strategy.target
      )

  given MeasurementTask[
    ExternalSource,
    ExternalDesign,
    NeedsTrainingCapability.type,
    NoRendition.type,
    PredictivePrepared
  ] with
    override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
      Right(())

    override def execute(
        plan: BoundScientificPlan[
          ExternalSource,
          ExternalDesign,
          NeedsTrainingCapability.type,
          NoRendition.type,
          PredictivePrepared
        ]
    )(
        measurement: MeasurementEntry[
          plan.specification.source.Neural,
          plan.specification.source.NeuralKey,
          ?,
          NoRendition.type
        ],
        context: TaskContext
    ): Either[
      TaskReportError,
      TaskReport[plan.Result, plan.Rejection, plan.Failure]
    ] =
      TaskReport.success(
        PredictiveEstimate(plan.prepared.sampleCount),
        context.strategy.target
      )

object ExternalFixtures:
  val SamplesAxis: ScientificAxisName = ScientificAxisName.unsafe("samples")
  val NeuralAxis: ScientificAxisName = ScientificAxisName.unsafe("neural")

  def source(): ExternalSource =
    val samples = AxisRef
      .create(
        AxisId.unsafe("external-samples"),
        AxisPurpose.Samples,
        Vector.tabulate(4)(index => SampleId.unsafe(s"sample-$index")),
        CoordinateBasis.unsafe("trial-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("external-suite", "v1")
      )
      .toOption
      .get
    val features = AxisRef
      .create(
        AxisId.unsafe("external-features"),
        AxisPurpose.NeuralFeatures,
        Vector.tabulate(3)(index => FeatureId.unsafe(s"feature-$index")),
        CoordinateBasis.unsafe("voxel-order"),
        Some(AxisUnits.unsafe("arbitrary-signal")),
        AxisScale.nominal,
        CoordinateProvenance.unsafe("external-suite", "v1")
      )
      .toOption
      .get
    val identity = ScientificSourceIdentity(
      ScientificSourceKind.unsafe("external-observations"),
      Vector(
        ScientificSourceAxis(SamplesAxis, samples.identity),
        ScientificSourceAxis(NeuralAxis, features.identity)
      )
    ).toOption.get
    new ExternalSource(samples, features, identity)

  def design(source: ExternalSource): ExternalDesign =
    new ExternalDesign(
      DesignIdentity(
        DesignKind.unsafe("external-validation"),
        Vector("policy" -> "leave-one-block-out")
      ).toOption.get,
      Vector(
        DesignAxisReference(SamplesAxis, source.samples.identity),
        DesignAxisReference(NeuralAxis, source.features.identity)
      )
    )

  def invalidDesign(source: ExternalSource): ExternalDesign =
    new ExternalDesign(
      DesignIdentity(DesignKind.unsafe("external-validation")).toOption.get,
      Vector(
        DesignAxisReference(
          ScientificAxisName.unsafe("subjects"),
          source.samples.identity
        )
      )
    )

  def frame(
      source: ExternalSource
  ): MeasurementFrame[source.Neural, source.NeuralKey, NoRendition.type] =
    val population = IndexSpace.of(source.features.size).toOption.get
    val selection = Injection
      .from(IArray.unsafeFromArray(Array(0, 2)), population)
      .toOption
      .get
    val measurement = Measurement
      .hardSelection(
        source.features,
        MeasurementId.unsafe("external-roi"),
        selection
      )
      .toOption
      .get
    MeasurementFrame(source.features)(
      Vector(MeasurementEntry(measurement, NoRendition))
    ).toOption.get

  def boundaries(id: String): RequestedBoundaries =
    RequestedBoundaries(
      Vector(
        OutputBoundaryIdentity(OutputBoundaryId.unsafe(id)).toOption.get
      )
    ).toOption.get

  def strategy: ExecutionStrategy =
    ExecutionStrategy(
      BackendId.unsafe("external-portable"),
      ExecutionRepresentation.Operator,
      NumericPrecision.Binary64,
      SolverChoice.NotApplicable,
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Reject,
      FallbackPolicy.forbidden,
      ResultDelivery.Collected
    ).toOption.get

class MvpaFacadeSuite extends munit.FunSuite:
  import ExternalExtensions.given

  test("one inspection API describes predictive and relational plans before execution"):
    val source = ExternalFixtures.source()
    val design = ExternalFixtures.design(source)
    val frame = ExternalFixtures.frame(source)
    val predictive = Mvpa
      .specify(source)(design, frame, PredictiveQuestion)
      .toOption
      .get
    val relational = Mvpa
      .specify(source)(design, frame, RelationalQuestion)
      .toOption
      .get
    val predictiveInspection: ScientificPlanInspection = Mvpa.inspect(predictive)
    val relationalInspection: ScientificPlanInspection = Mvpa.inspect(relational)

    assertEquals(predictiveInspection.source, relationalInspection.source)
    assertEquals(predictiveInspection.design, relationalInspection.design)
    assertEquals(predictiveInspection.frame, relationalInspection.frame)
    assertEquals(predictiveInspection.measurements, relationalInspection.measurements)
    assertEquals(predictiveInspection.measurementCount, 1)
    assertNotEquals(predictiveInspection.estimand, relationalInspection.estimand)
    assertNotEquals(predictiveInspection.boundaries, relationalInspection.boundaries)

    val bound = Mvpa.bind(predictive).toOption.get
    val planned = Mvpa.plan(bound, ExternalFixtures.strategy).toOption.get
    assertEquals(Mvpa.inspect(bound), predictiveInspection)
    assertEquals(Mvpa.inspect(planned), predictiveInspection)

  test("predictive and relational estimands use one public vocabulary"):
    val source = ExternalFixtures.source()
    val design = ExternalFixtures.design(source)
    val frame = ExternalFixtures.frame(source)

    val predictive: Either[
      MvpaRunError[ExternalRejection],
      AnalysisResult[
        PredictiveEstimate,
        ExternalRejection,
        ExternalFailure,
        NoRendition.type
      ]
    ] = Mvpa.run(source)(
      design,
      frame,
      PredictiveQuestion,
      ExternalFixtures.strategy
    )
    val relational: Either[
      MvpaRunError[ExternalRejection],
      AnalysisResult[
        RelationalEstimate,
        ExternalRejection,
        ExternalFailure,
        NoRendition.type
      ]
    ] = Mvpa.run(source)(
      design,
      frame,
      RelationalQuestion,
      ExternalFixtures.strategy
    )

    assertEquals(
      predictive.toOption.get.values.collect:
        case MeasurementValue(_, _, MeasurementOutcome.Success(value, _)) => value
      ,
      Vector(PredictiveEstimate(4))
    )
    assertEquals(
      relational.toOption.get.values.collect:
        case MeasurementValue(_, _, MeasurementOutcome.Success(value, _)) => value
      ,
      Vector(RelationalEstimate(2))
    )
    assertEquals(
      predictive.toOption.get.plan.frame,
      relational.toOption.get.plan.frame
    )
    assertEquals(
      predictive.toOption.get.receipt.execution.strategy,
      relational.toOption.get.receipt.execution.strategy
    )

  test("the shortest public run path reports the specification stage and axis"):
    val source = ExternalFixtures.source()
    val result = Mvpa.run(source)(
      ExternalFixtures.invalidDesign(source),
      ExternalFixtures.frame(source),
      PredictiveQuestion,
      ExternalFixtures.strategy
    )

    assert(result.left.exists: error =>
      error.message.contains("specification stage") &&
        error.message.contains("axis") &&
        error.message.contains("subjects"))

  test("the shortest public run path preserves a compiler capability rejection"):
    val source = ExternalFixtures.source()
    val result = Mvpa.run(source)(
      ExternalFixtures.design(source),
      ExternalFixtures.frame(source),
      NeedsTrainingCapability,
      ExternalFixtures.strategy
    )

    assert(result.left.exists: error =>
      error.message.contains("binding stage") &&
        error.message.contains("training role") &&
        error.message.contains("residual-moments capability"))
