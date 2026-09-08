package scalafim.fmri.mvpa

import multivar.core.ValueId
import multivar.core.ValueIdentity
import multivar.family.canonical.ResidualRegularization
import resample4s.core.IndexSpace
import resample4s.core.Injection
import scalafim.fmri.mvpa.CanonicalAnalysis.given

private[mvpa] object CanonicalAnalysisTestSupport:
  type CanonicalResult = AnalysisResult[
    CanonicalEffectEstimate,
    CanonicalBindRejection,
    CanonicalTaskFailure,
    NoRendition.type
  ]

  type ConstrainedResult = AnalysisResult[
    ConstrainedCanonicalEstimate,
    CanonicalBindRejection,
    CanonicalTaskFailure,
    NoRendition.type
  ]

  type SignedResult = AnalysisResult[
    SignedCrossRunRayleighEstimate,
    CanonicalBindRejection,
    CanonicalTaskFailure,
    NoRendition.type
  ]

  type ManovaResult = AnalysisResult[
    ManovaEstimate,
    CanonicalBindRejection,
    CanonicalTaskFailure,
    NoRendition.type
  ]

  def canonical[N <: multivar.core.SemanticSpace](
      dataset: CanonicalEffectDataset[N, FeatureId],
      positions: Vector[Int],
      regularization: ResidualRegularization,
      revision: String = "canonical"
  ): CanonicalResult =
    scalarFixture(dataset, positions, revision).canonical(regularization)

  def constrained[N <: multivar.core.SemanticSpace](
      dataset: CanonicalEffectDataset[N, FeatureId],
      positions: Vector[Int],
      regularization: ResidualRegularization,
      revision: String = "constrained"
  ): ConstrainedResult =
    scalarFixture(dataset, positions, revision).constrained(regularization)

  def signed[N <: multivar.core.SemanticSpace](
      dataset: CanonicalEffectDataset[N, FeatureId],
      positions: Vector[Int],
      regularization: ResidualRegularization,
      revision: String = "signed"
  ): SignedResult =
    scalarFixture(dataset, positions, revision).signed(regularization)

  def manova[N <: multivar.core.SemanticSpace](
      dataset: ManovaDataset[N, FeatureId],
      positions: Vector[Int],
      regularization: ResidualRegularization,
      revision: String = "manova"
  ): ManovaResult =
    val partitionAxisRef = partitions(dataset.runs.map(_.partition.value), revision)
    val partitionAxis = PartitionAxis(ScientificAxisName.unsafe("runs"), partitionAxisRef)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val effects = AxisRef
      .create(
        AxisId.unsafe(s"$revision-effects"),
        AxisPurpose.Effects,
        Vector.tabulate(dataset.contrastRank)(index => AxisKey.unsafe(s"hypothesis-${index + 1}")),
        CoordinateBasis.unsafe("orthonormal-hypothesis"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("canonical-analysis-test", revision)
      )
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val neural = dataset.neural
    val source = RunwiseCanonicalRelations
      .manova(
        dataset,
        partitionAxis,
        effects,
        revisions(partitionAxisRef, revision)
      )
      .fold(error => throw new IllegalStateException(error.message), identity)
    val design = validation(partitionAxis, partitionAxisRef)
    val frame = measurementFrame(neural, positions, revision)
    Mvpa
      .run(source)(
        design,
        frame,
        CanonicalAnalysis.manova(source, regularization),
        strategy
      )
      .fold(error => throw new IllegalStateException(error.message), identity)

  def success[A](
      result: AnalysisResult[A, CanonicalBindRejection, CanonicalTaskFailure, NoRendition.type]
  ): A =
    result.values.head.outcome match
      case MeasurementOutcome.Success(value, _) => value
      case other                                => throw new IllegalStateException(s"expected success, obtained $other")

  private final class ScalarFixture[
      P <: multivar.core.SemanticSpace,
      E <: multivar.core.SemanticSpace,
      N <: multivar.core.SemanticSpace
  ](
      source: CrossValidatedRelations[
        P,
        E,
        N,
        AxisKey,
        FeatureId,
        ScalarEffectFitCapabilities[N, FeatureId]
      ],
      design: RelationValidationDesign[P],
      frame: MeasurementFrame[N, FeatureId, NoRendition.type]
  ):
    def canonical(regularization: ResidualRegularization): CanonicalResult =
      Mvpa
        .run(source)(
          design,
          frame,
          CanonicalAnalysis.canonicalEffect(source, regularization),
          strategy
        )
        .fold(error => throw new IllegalStateException(error.message), identity)

    def constrained(regularization: ResidualRegularization): ConstrainedResult =
      Mvpa
        .run(source)(
          design,
          frame,
          CanonicalAnalysis.nonnegativeCanonical(source, regularization),
          strategy
        )
        .fold(error => throw new IllegalStateException(error.message), identity)

    def signed(regularization: ResidualRegularization): SignedResult =
      Mvpa
        .run(source)(
          design,
          frame,
          CanonicalAnalysis.signedCrossRunRayleigh(source, regularization),
          strategy
        )
        .fold(error => throw new IllegalStateException(error.message), identity)

  private def scalarFixture[N <: multivar.core.SemanticSpace](
      dataset: CanonicalEffectDataset[N, FeatureId],
      positions: Vector[Int],
      revision: String
  ): ScalarFixture[? <: multivar.core.SemanticSpace, ? <: multivar.core.SemanticSpace, N] =
    val partitionAxisRef = partitions(dataset.runs.map(_.partition.value), revision)
    val partitionAxis = PartitionAxis(ScientificAxisName.unsafe("runs"), partitionAxisRef)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val effects = AxisRef
      .create(
        AxisId.unsafe(s"$revision-effect"),
        AxisPurpose.Effects,
        Vector(AxisKey.unsafe("contrast")),
        CoordinateBasis.unsafe("normalized-contrast"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("canonical-analysis-test", revision)
      )
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val neural = dataset.neural
    val source = RunwiseCanonicalRelations
      .scalar(
        dataset,
        partitionAxis,
        effects,
        revisions(partitionAxisRef, revision)
      )
      .fold(error => throw new IllegalStateException(error.message), identity)
    new ScalarFixture(
      source,
      validation(partitionAxis, partitionAxisRef),
      measurementFrame(neural, positions, revision)
    )

  private def partitions(
      ids: Vector[String],
      revision: String
  ): AxisRef[PartitionId] =
    AxisRef
      .create(
        AxisId.unsafe(s"$revision-runs"),
        AxisPurpose.Partitions,
        ids.map(PartitionId.unsafe),
        CoordinateBasis.unsafe("run-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("canonical-analysis-test", revision)
      )
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def revisions(
      partitions: AxisRef[PartitionId],
      revision: String
  ): Map[PartitionId, ValueIdentity] =
    partitions.keys
      .map: partition =>
        partition -> ValueIdentity.source(
          ValueId.unsafe(s"$revision-response-${partition.value}")
        )
      .toMap

  private def validation[P <: multivar.core.SemanticSpace](
      partitions: PartitionAxis[P],
      axis: AxisRef.Aux[PartitionId, P]
  ): RelationValidationDesign[P] =
    RelationValidationDesign
      .leaveOnePartitionOut(
        partitions,
        GeneralizationAxis(ScientificAxisName.unsafe("runs"), axis.identity)
      )
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def measurementFrame[N <: multivar.core.SemanticSpace](
      neural: AxisRef.Aux[FeatureId, N],
      positions: Vector[Int],
      revision: String
  ): MeasurementFrame[N, FeatureId, NoRendition.type] =
    val injection = Injection
      .from(
        IArray.unsafeFromArray(positions.toArray),
        IndexSpace.of(neural.size).fold(error => throw new IllegalArgumentException(error.message), identity)
      )
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val measurement = Measurement
      .hardSelection(neural, MeasurementId.unsafe(s"$revision-measurement"), injection)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    MeasurementFrame(neural)(Vector(MeasurementEntry(measurement, NoRendition)))
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private val strategy = ExecutionStrategy(
    BackendId.unsafe("canonical-portable"),
    ExecutionRepresentation.SufficientStatistics,
    NumericPrecision.Binary64,
    SolverChoice.NotApplicable,
    Vector.empty,
    Scheduling.serial,
    MaterializationPolicy.Reject,
    FallbackPolicy.forbidden,
    ResultDelivery.Collected
  ).fold(error => throw new IllegalArgumentException(error.message), identity)
