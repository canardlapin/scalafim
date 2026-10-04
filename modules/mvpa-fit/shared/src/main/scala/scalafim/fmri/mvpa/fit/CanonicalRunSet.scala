package scalafim.fmri.mvpa.fit

import gale.linalg.DMat
import multivar.core.{MultivarError, SemanticError, SemanticSpace, SpaceEvidence}
import scalafim.dataset.RunId
import scalafim.fmri.fit.{ResponseBlock, RunIndex, TrainingRunScope}
import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, AxisRef, Observations}
import scalafim.fmri.mvpa.analysis.*

enum CanonicalArtifactError:
  case Input(detail: String)
  case Temporal(error: OneShotMvpaError)
  case Resource(error: ResourceError)
  case Access(error: ObservationProductError)
  case MultivarFailure(error: MultivarError)
  case SemanticFailure(error: SemanticError)

final class CanonicalTrainingRuns[P <: SemanticSpace] private[fit] (
    val partitionAxis: AxisDescriptor, val runIds: Vector[RunId], private[fit] val indices: Vector[Int],
    private[fit] val scope: TrainingRunScope
)

/** One run retains its own time domain and the shared neural domain. Provider
  * acquisition and expiration belong to ObservationProduct, not this record.
  */
sealed abstract class CanonicalRunEvidence[N <: SemanticSpace, G]:
  type Time <: SemanticSpace
  def runId: RunId
  def neural: SpaceEvidence[N]
  def neuralAxis: AxisDescriptor
  def timeAxis: AxisDescriptor
  def observations: Observations[Time, N]
  def geometry: G
  def providerCosts: ObservationProviderCosts
  def identity: String
  private[fit] def read(budget: ResourceBudget): Either[CanonicalArtifactError, ResponseBlock]

object CanonicalRunEvidence:
  def fromObservations[TK, NK, G](
      id: RunId, time: AxisRef[TK], features: AxisRef[NK],
      values: Observations[time.Id, features.Id], preparation: G,
      replay: ObservationReplay, resource: ObservationProductResource,
      costs: ObservationProviderCosts
  ): Either[CanonicalArtifactError, CanonicalRunEvidence[features.Id, G]] =
    if values.sampleAxis != time.descriptor || values.neuralAxis != features.descriptor then
      Left(CanonicalArtifactError.Input("run evidence axes disagree with their nominal witnesses"))
    else if values.rows <= 0 || values.columns <= 0 then
      Left(CanonicalArtifactError.Input("canonical runs require nonempty time and neural domains"))
    else Right(new CanonicalRunEvidence[features.Id, G]:
      type Time = time.Id
      val runId = id
      val neural = features.evidence
      val neuralAxis = features.descriptor
      val timeAxis = time.descriptor
      val observations = values
      val geometry = preparation
      val providerCosts = costs
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("canonical-run-evidence-v1")
        writer.string(id.value)
        values.identity.writeFramed(writer)
      private[fit] def read(budget: ResourceBudget): Either[CanonicalArtifactError, ResponseBlock] =
        val boundedCosts = costs.copy(applicationBuffers = ResourceBound.Known(
          32 * (BigInt(values.rows) + values.columns), "canonical column-at-a-time input/output buffers"))
        ObservationProduct.withPrepared(time, features, values, replay, resource, boundedCosts,
          ObservationProductRoute.Direct, budget, "canonical temporal preparation reads the same identified values"): product =>
          val cells = BigInt(values.rows) * values.columns
          if cells > Int.MaxValue then Left(ObservationProductError.Capacity(cells))
          else
            val data = new Array[Double](cells.toInt)
            var column = 0
            var failure: Option[ObservationProductError] = None
            while column < values.columns && failure.isEmpty do
              val basis = DMat.tabulate(values.columns, 1)((index, _) => if index == column then 1.0 else 0.0)
              product.observations.patterns(basis) match
                case Left(error) => failure = Some(ObservationProductError.Materialization(error.message))
                case Right(result) =>
                  var row = 0
                  while row < values.rows do
                    data(row * values.columns + column) = result(row, 0)
                    row += 1
              column += 1
            failure match
              case Some(error) => Left(error)
              case None => Right(ResponseBlock.unsafe(DMat.dense(values.rows, values.columns, data.toVector)))
        .left.map(CanonicalArtifactError.Access.apply)
    )

/** A run partition is a real identified axis, never a synthetic singleton ROI.
  * The geometry type distinguishes contrast and MANOVA preparation schedules.
  */
final class CanonicalRunSet[P <: SemanticSpace, N <: SemanticSpace, G] private (
    val partitions: SpaceEvidence[P], val partitionAxis: AxisDescriptor,
    val neural: SpaceEvidence[N], val neuralAxis: AxisDescriptor,
    val runs: Vector[CanonicalRunEvidence[N, G]]
):
  val evidenceIdentity: String = AxisDigest.sha256Hex: writer =>
    writer.string("canonical-run-set-v1")
    writer.string(partitionAxis.stableKey)
    writer.string(neuralAxis.stableKey)
    writer.intLE(runs.length)
    runs.foreach(run => writer.string(run.identity))

  /** Selection is checked against this partition and retains its nominal P.
    * Numerical methods never interpret a bare run ordinal from another source.
    */
  def selectRuns(ids: Vector[RunId]): Either[CanonicalArtifactError, CanonicalTrainingRuns[P]] =
    val positions = runs.map(_.runId).zipWithIndex.toMap
    if ids.isEmpty || ids.distinct.length != ids.length || ids.exists(id => !positions.contains(id)) then
      Left(CanonicalArtifactError.Input("training run identities must be nonempty, unique and belong to this partition"))
    else
      val indices = ids.map(positions)
      scope(indices).map(value => new CanonicalTrainingRuns(partitionAxis, ids, indices, value))

  private[fit] def scope(indices: Vector[Int]): Either[CanonicalArtifactError, TrainingRunScope] =
    if indices.isEmpty || indices.distinct.length != indices.length || indices.exists(i => i < 0 || i >= runs.length) then
      Left(CanonicalArtifactError.Input("training run indices must be nonempty, unique, and in range"))
    else TrainingRunScope.fromInts(indices).left.map(error => CanonicalArtifactError.Input(error.toString))

  /** Declares only storage owned by temporal preparation and moment retention.
    * External solver/provider costs are explicit unknowns; WholeNumeric refuses
    * unless the caller supplies their independently established declarations.
    */
  private[fit] def admit(budget: ResourceBudget, backend: ResourceBound, additionalMomentCells: BigInt = 0): Either[CanonicalArtifactError, NumericResourceAdmission] =
    val p = BigInt(neuralAxis.size)
    val sourceCells = runs.map(run => BigInt(run.observations.rows) * p).sum
    val largestSource = runs.map(run => BigInt(run.observations.rows) * p).max
    val matrixCells = p * p
    val bounds = backend +: runs.flatMap(run => Vector(run.providerCosts.source, run.providerCosts.workerScratch))
    val malformed = bounds.exists:
      case ResourceBound.Known(bytes, receipt) => bytes < 0 || receipt.trim.isEmpty
      case ResourceBound.Unknown(reason) => reason.trim.isEmpty
    if malformed then Left(CanonicalArtifactError.Resource(ResourceError.InvalidBound("canonical provider declaration")))
    else if runs.exists(_.providerCosts.workers != 1) then
      Left(CanonicalArtifactError.Input("canonical temporal preparation admits one provider worker"))
    else if matrixCells > Int.MaxValue || largestSource > Int.MaxValue || additionalMomentCells < 0 then
      Left(CanonicalArtifactError.Input("canonical dense moment or response exceeds primitive array capacity"))
    else
      val owned = 8 * (4 * sourceCells + 8 * runs.length * matrixCells + additionalMomentCells)
      def combined(bounds: Vector[ResourceBound], label: String): ResourceBound =
        val unknown = bounds.collect { case ResourceBound.Unknown(reason) => reason }
        if unknown.nonEmpty then ResourceBound.Unknown(s"$label: ${unknown.mkString("; ")}")
        else ResourceBound.Known(bounds.collect { case ResourceBound.Known(bytes, _) => bytes }.sum, s"combined $label declarations")
      ResourceAdmission.evaluateFootprint(
        ResourceFootprint(combined(runs.map(_.providerCosts.source), "run sources"),
          combined(backend +: runs.map(_.providerCosts.workerScratch), "solver and run scratch"),
          1, owned, 0, 0), 8 * sourceCells, budget
      ).left.map(CanonicalArtifactError.Resource.apply)

object CanonicalRunSet:
  def make[PK, N <: SemanticSpace, G](
      partitions: AxisRef[PK]
  )(runs: Vector[CanonicalRunEvidence[N, G]])(runKey: PK => RunId): Either[CanonicalArtifactError, CanonicalRunSet[partitions.Id, N, G]] =
    if runs.isEmpty then Left(CanonicalArtifactError.Input("canonical evidence requires at least one run"))
    else if partitions.size != runs.length then Left(CanonicalArtifactError.Input("run partition length mismatch"))
    else if runs.map(_.runId).distinct.length != runs.length then Left(CanonicalArtifactError.Input("duplicate run identities"))
    else if runs.indices.exists(i => partitions.keyAt(i).map(runKey) != Right(runs(i).runId)) then Left(CanonicalArtifactError.Input("run partition ordering mismatch"))
    else if runs.exists(_.neuralAxis != runs.head.neuralAxis) then Left(CanonicalArtifactError.Input("canonical runs use different neural coordinates"))
    else Right(new CanonicalRunSet(partitions.evidence, partitions.descriptor, runs.head.neural, runs.head.neuralAxis, runs))
