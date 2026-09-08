package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.DVec
import multivar.core.ComponentCount
import multivar.core.MatrixView
import multivar.core.MultivarError
import multivar.core.PreprocessSpec
import multivar.core.SemanticSpace
import multivar.core.ValueIdentity
import multivar.family.spectral.Pca
import multivar.family.spectral.PcaFit

opaque type ComponentId = Int

object ComponentId:
  def apply(value: Int): Either[GlobalDecompositionError, ComponentId] =
    if value <= 0 then Left(GlobalDecompositionError.InvalidComponentId(value))
    else Right(value)

  private[mvpa] def unsafe(value: Int): ComponentId =
    value

  extension (id: ComponentId) inline def value: Int = id

  given AxisKeyCodec[ComponentId] with
    override def encode(key: ComponentId): AxisKey =
      AxisKey.unsafe(s"component-${key.value}")

enum PcaPreparation:
  case Centered
  case Standardized
  case PassThrough

  def label: String =
    this match
      case Centered     => "centered"
      case Standardized => "standardized"
      case PassThrough  => "pass-through"

  private[mvpa] def multivar: PreprocessSpec =
    this match
      case Centered     => PreprocessSpec.Center
      case Standardized => PreprocessSpec.Standardize
      case PassThrough  => PreprocessSpec.Pass

enum GlobalDecompositionError:
  case Identity(error: ScientificIdentityError)
  case Axis(error: AxisRefError)
  case Evidence(error: EvidenceTableError)
  case Multivar(error: MultivarError)
  case ExecutionEvidence(error: ExecutionReceiptError)
  case InvalidComponentId(value: Int)

  def message: String =
    this match
      case Identity(error)           => error.message
      case Axis(error)               => error.message
      case Evidence(error)           => error.message
      case Multivar(error)           => error.message
      case ExecutionEvidence(error)  => error.message
      case InvalidComponentId(value) =>
        s"component id must be positive, obtained $value"

/** The exact open scientific object to which PCA is applied. The boundary includes ordered rows, ordered columns, and
  * the evidence value identity; it is not inferred from matrix dimensions.
  */
final class ObservationTableBoundary[
    S <: SemanticSpace,
    N <: SemanticSpace,
    K
] private (
    val rows: AxisRef.Aux[SampleId, S],
    val columns: AxisRef.Aux[K, N],
    val value: ValueIdentity,
    val identity: ScientificComponentFingerprint
)

object ObservationTableBoundary:
  private val Kind = EstimandKind.unsafe("observation-table-boundary")

  private[mvpa] def trusted[S <: SemanticSpace, N <: SemanticSpace, K](
      source: Observations[S, N, K]
  ): ObservationTableBoundary[S, N, K] =
    val identity = EstimandIdentity.trusted(
      Kind,
      Vector(
        "columns" -> source.neuralAxis.identity.fingerprint.value,
        "rows" -> source.samples.identity.fingerprint.value,
        "value" -> source.evidence.table.valueIdentity.stableKey
      )
    )
    new ObservationTableBoundary(
      source.samples,
      source.neuralAxis,
      source.evidence.table.valueIdentity,
      identity.fingerprint
    )

  def apply[S <: SemanticSpace, N <: SemanticSpace, K](
      source: Observations[S, N, K]
  ): Either[
    ScientificIdentityError,
    ObservationTableBoundary[S, N, K]
  ] =
    EstimandIdentity(
      Kind,
      Vector(
        "columns" -> source.neuralAxis.identity.fingerprint.value,
        "rows" -> source.samples.identity.fingerprint.value,
        "value" -> source.evidence.table.valueIdentity.stableKey
      )
    ).map: identity =>
      new ObservationTableBoundary(
        source.samples,
        source.neuralAxis,
        source.evidence.table.valueIdentity,
        identity.fingerprint
      )

enum PcaBindRejection:
  case BoundaryMismatch(
      expected: ScientificComponentFingerprint,
      actual: ScientificComponentFingerprint
  )
  case FitWitnessMismatch
  case InsufficientTrainingRows(actual: Int)
  case ComponentsExceedTrainingRows(requested: Int, rows: Int)

  def message: String =
    this match
      case BoundaryMismatch(expected, actual) =>
        s"PCA observation boundary ${actual.value} does not match source ${expected.value}"
      case FitWitnessMismatch =>
        "PCA fit design and observations use different nominal sample witnesses"
      case InsufficientTrainingRows(actual) =>
        s"PCA requires at least two fit rows, obtained $actual"
      case ComponentsExceedTrainingRows(requested, rows) =>
        s"PCA requests $requested components from only $rows fit rows"

final class PcaPrepared[
    S <: SemanticSpace,
    N <: SemanticSpace,
    K
] private[mvpa] (
    val boundary: ObservationTableBoundary[S, N, K],
    val fitRows: AxisIdentity
)

final class PcaEstimand[
    S <: SemanticSpace,
    N <: SemanticSpace,
    K
] private[mvpa] (
    val boundary: ObservationTableBoundary[S, N, K],
    val components: ComponentCount,
    val preparation: PcaPreparation,
    val identity: EstimandIdentity
) extends Estimand[Observations[S, N, K], ObservationFitDesign[S]]:
  override type Result = MeasuredPca[S]
  override type Rejection = PcaBindRejection
  override type Failure = GlobalDecompositionError

  override val defaultBoundaries: RequestedBoundaries =
    GlobalDecomposition.PcaBoundaries

  override def rejectionMessage(value: PcaBindRejection): String = value.message
  override def failureMessage(value: GlobalDecompositionError): String = value.message

final class PcaComputationReceipt private[mvpa] (
    val source: ScientificComponentFingerprint,
    val design: ScientificComponentFingerprint,
    val boundary: ScientificComponentFingerprint,
    val measurement: MeasurementIdentity,
    val trainingRows: AxisIdentity,
    val components: AxisIdentity,
    val preparation: PcaPreparation,
    val materialization: MaterializationReceipt,
    val requestedComponents: Int,
    val effectiveComponents: Int,
    val solver: SolverIdentity,
    val identity: ScientificComponentFingerprint
)

/** A measured PCA result retains the exact Multivar fit rather than copying it into a second decomposition model.
  * ScalaFIM adds the scientific owners, declared fit scope, and execution evidence Multivar does not know about.
  */
final class MeasuredPca[S <: SemanticSpace] private[mvpa] (
    val measurement: MeasurementIdentity,
    val sourceSamples: AxisRef.Aux[SampleId, S],
    val trainingRows: AxisIdentity,
    val local: AxisIdentity,
    val components: AxisRef[ComponentId],
    val multivar: PcaFit,
    val definition: EstimandIdentity,
    val computation: PcaComputationReceipt
):
  def loadings: DMat = multivar.result.v
  def trainingScores: DMat = multivar.transform.trainingValues
  def singularValues: DVec = multivar.result.singularValues

object GlobalDecomposition:
  private val PcaKind = EstimandKind.unsafe("observation-table-pca")
  private val ReceiptKind = EstimandKind.unsafe("measured-pca-fit")

  val PcaSolver: SolverIdentity =
    SolverIdentity.trusted(
      SolverId.unsafe("multivar-pca-gram-svd"),
      Vector(
        "implementation" -> "multivar.family.spectral.Pca",
        "spectral-kernel" -> "gale.symmetric-eigen"
      )
    )

  private[mvpa] val PcaBoundaries =
    val ids = Vector(
      "pca-components",
      "pca-loadings",
      "pca-training-scores",
      "pca-singular-values"
    )
    val boundaries = ids.map: id =>
      OutputBoundaryIdentity.trusted(OutputBoundaryId.unsafe(id))
    RequestedBoundaries.trusted(boundaries)

  def pca[S <: SemanticSpace, N <: SemanticSpace, K](
      source: Observations[S, N, K],
      components: ComponentCount,
      preparation: PcaPreparation = PcaPreparation.Centered
  ): PcaEstimand[S, N, K] =
    val boundary = ObservationTableBoundary.trusted(source)
    val identity = EstimandIdentity.trusted(
      PcaKind,
      Vector(
        "components" -> components.value.toString,
        "input" -> boundary.identity.value,
        "open-object" -> "observation-table",
        "preparation" -> preparation.label
      )
    )
    new PcaEstimand(boundary, components, preparation, identity)

  given compiler[S <: SemanticSpace, N <: SemanticSpace, K, R]: Compile[
    Observations[S, N, K],
    ObservationFitDesign[S],
    PcaEstimand[S, N, K],
    R
  ] with
    override type Prepared = PcaPrepared[S, N, K]

    override def prepare(
        specification: ScientificSpecification[
          Observations[S, N, K],
          ObservationFitDesign[S],
          PcaEstimand[S, N, K],
          R
        ]
    ): Either[specification.Rejection, PcaPrepared[S, N, K]] =
      val actual = ObservationTableBoundary.trusted(specification.source)
      if specification.estimand.boundary.identity != actual.identity then
        Left(
          PcaBindRejection.BoundaryMismatch(
            actual.identity,
            specification.estimand.boundary.identity
          )
        )
      else if !(specification.design.samples.evidence eq specification.source.samples.evidence) then
        Left(PcaBindRejection.FitWitnessMismatch)
      else if specification.design.selection.size < 2 then
        Left(PcaBindRejection.InsufficientTrainingRows(specification.design.selection.size))
      else if specification.estimand.components.value > specification.design.selection.size then
        Left(
          PcaBindRejection.ComponentsExceedTrainingRows(
            specification.estimand.components.value,
            specification.design.selection.size
          )
        )
      else
        Right(
          new PcaPrepared(
            actual,
            specification.design.selection.child.identity
          )
        )

  given task[S <: SemanticSpace, N <: SemanticSpace, K, R]: MeasurementTask[
    Observations[S, N, K],
    ObservationFitDesign[S],
    PcaEstimand[S, N, K],
    R,
    PcaPrepared[S, N, K]
  ] with
    override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
      if strategy.representation != ExecutionRepresentation.Dense then
        Left(
          ExecutionPlanError.UnsupportedRepresentation(
            strategy.representation,
            Vector(ExecutionRepresentation.Dense)
          )
        )
      else if strategy.precision != NumericPrecision.Binary64 then
        Left(
          ExecutionPlanError.UnsupportedPrecision(
            strategy.precision,
            Vector(NumericPrecision.Binary64)
          )
        )
      else if strategy.solver != SolverChoice.Selected(PcaSolver) then
        Left(ExecutionPlanError.UnsupportedSolver(strategy.solver))
      else
        strategy.materialization match
          case MaterializationPolicy.Reject =>
            Left(
              ExecutionPlanError.MaterializationRequired(
                "Multivar PCA consumes one explicitly budgeted local observation table"
              )
            )
          case MaterializationPolicy.Allow(_) => Right(())

    override def execute(
        plan: BoundScientificPlan[
          Observations[S, N, K],
          ObservationFitDesign[S],
          PcaEstimand[S, N, K],
          R,
          PcaPrepared[S, N, K]
        ]
    )(
        entry: MeasurementEntry[N, K, ?, R],
        context: TaskContext
    ): Either[TaskReportError, TaskReport[plan.Result, plan.Rejection, plan.Failure]] =
      evaluate(
        plan.specification.source,
        plan.specification.design,
        plan.specification.estimand,
        entry.measurement,
        context.strategy.materialization
      ) match
        case Left(error)   => TaskReport.failed(error, context.strategy.target)
        case Right(result) =>
          ExecutionMaterialization(
            ExecutionScope.Measurement(entry.measurement.identity.id),
            result.computation.materialization,
            "Multivar PCA requires the declared local observation table"
          ) match
            case Left(error) =>
              TaskReport.failed(
                GlobalDecompositionError.ExecutionEvidence(error),
                context.strategy.target,
                operatorApplications = entry.measurement.local.size.toLong
              )
            case Right(materialization) =>
              TaskReport.success(
                result,
                context.strategy.target,
                operatorApplications = entry.measurement.local.size.toLong,
                materializations = Vector(materialization)
              )

  private def evaluate[
      S <: SemanticSpace,
      N <: SemanticSpace,
      K,
      L
  ](
      source: Observations[S, N, K],
      design: ObservationFitDesign[S],
      estimand: PcaEstimand[S, N, K],
      measurement: Measurement[N, K, L],
      policy: MaterializationPolicy
  ): Either[GlobalDecompositionError, MeasuredPca[S]] =
    for
      selected <- source.evidence
        .restrictRows(design.selection)
        .left
        .map(GlobalDecompositionError.Evidence.apply)
      measured <- selected
        .measureColumns(measurement)
        .left
        .map(GlobalDecompositionError.Evidence.apply)
      materialized <- measured
        .materialize(policy)
        .left
        .map(GlobalDecompositionError.Evidence.apply)
      fit <- Pca
        .fit(
          MatrixView.dense(materialized.value),
          estimand.components,
          estimand.preparation.multivar
        )
        .left
        .map(GlobalDecompositionError.Multivar.apply)
      componentAxis <- components(
        measurement,
        estimand,
        fit.result.singularValues.length
      )
      receipt <- computationReceipt(
        source,
        design,
        estimand,
        measurement,
        componentAxis,
        materialized.receipt
      )
    yield new MeasuredPca(
      measurement.identity,
      source.samples,
      design.selection.child.identity,
      measurement.local.identity,
      componentAxis,
      fit,
      estimand.identity,
      receipt
    )

  private def components[
      N <: SemanticSpace,
      K,
      L,
      S <: SemanticSpace
  ](
      measurement: Measurement[N, K, L],
      estimand: PcaEstimand[S, N, K],
      count: Int
  ): Either[GlobalDecompositionError, AxisRef[ComponentId]] =
    val keys = Vector.tabulate(count)(position => ComponentId.unsafe(position + 1))
    AxisRef
      .create(
        AxisId.unsafe(
          s"pca-${estimand.identity.fingerprint.value.takeRight(12)}-${measurement.identity.fingerprint.value.takeRight(12)}"
        ),
        AxisPurpose.unsafe("components"),
        keys,
        CoordinateBasis.unsafe(
          "multivar-pca-components",
          "measurement" -> measurement.identity.fingerprint.value,
          "request" -> estimand.identity.fingerprint.value
        ),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe(
          "scalafim-global-decomposition",
          "multivar-pca/v1",
          measurement.local.identity.fingerprint.value
        )
      )
      .left
      .map(GlobalDecompositionError.Axis.apply)

  private def computationReceipt[
      S <: SemanticSpace,
      N <: SemanticSpace,
      K,
      L
  ](
      source: Observations[S, N, K],
      design: ObservationFitDesign[S],
      estimand: PcaEstimand[S, N, K],
      measurement: Measurement[N, K, L],
      components: AxisRef[ComponentId],
      materialization: MaterializationReceipt
  ): Either[GlobalDecompositionError, PcaComputationReceipt] =
    EstimandIdentity(
      ReceiptKind,
      Vector(
        "boundary" -> estimand.boundary.identity.value,
        "components" -> components.identity.fingerprint.value,
        "design" -> design.identity.fingerprint.value,
        "effective-components" -> components.size.toString,
        "materialized-value" -> materialization.valueIdentity.stableKey,
        "measurement" -> measurement.identity.fingerprint.value,
        "preparation" -> estimand.preparation.label,
        "requested-components" -> estimand.components.value.toString,
        "solver" -> PcaSolver.id.value,
        "source" -> source.identity.fingerprint.value,
        "training-rows" -> design.selection.child.identity.fingerprint.value
      )
    ).left
      .map(GlobalDecompositionError.Identity.apply)
      .map: identity =>
        new PcaComputationReceipt(
          source.identity.fingerprint,
          design.identity.fingerprint,
          estimand.boundary.identity,
          measurement.identity,
          design.selection.child.identity,
          components.identity,
          estimand.preparation,
          materialization,
          estimand.components.value,
          components.size,
          PcaSolver,
          identity.fingerprint
        )
