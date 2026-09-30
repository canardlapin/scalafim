package scalafim.fmri.mvpa.fit

import multivar.core.{
  CoordinateEvidence,
  Lin,
  SemanticProvenance,
  SemanticProvenanceEvent,
  ValueId,
  ValueIdentity
}
import gale.linalg.{DoubleLinearOperator, DVec, MutableDVec}
import scala.util.control.NonFatal
import scalafim.fmri.mvpa.{AxisDigest, AxisRef, EvidenceError, EvidenceOrigins}
import scalafim.fmri.mvpa.relation.{EffectEstimability, Relation, RelationAccess, RelationOrigins, RelationSet, RelationSource}

/** The declared access mode belongs to the response provider, not to
  * `DoubleLinearOperator`. A callable operator alone establishes neither a
  * replay guarantee nor ownership of a reader.
  */
sealed trait ReadoutRelationAccess:
  def relationAccess: RelationAccess

object ReadoutRelationAccess:
  case object OneShot extends ReadoutRelationAccess:
    val relationAccess: RelationAccess = RelationAccess.OneShot

trait ReadoutResource:
  def acquire(): Either[String, Unit]
  def close(): Either[String, Unit]

/** Domain-owned source metadata. The adapter carries it unchanged through
  * effect restriction; it does not invent acquisition or preparation claims.
  */
final case class ReadoutRelationOrigins(
    acquisition: String,
    responseRevision: String,
    readout: String,
    preparation: String,
    noiseRevision: String,
    access: ReadoutRelationAccess,
    support: EvidenceOrigins = EvidenceOrigins.Unknown
):
  require(acquisition.nonEmpty && responseRevision.nonEmpty && readout.nonEmpty && preparation.nonEmpty && noiseRevision.nonEmpty,
    "relation revisions must be non-empty")
  def relationIdentity(effectAxis: AxisRef[?], neuralAxis: AxisRef[?]): String =
    AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.identified-readout-relation.v2")
      writer.string(acquisition)
      writer.string(responseRevision)
      writer.string(readout)
      writer.string(preparation)
      writer.string(noiseRevision)
      writer.string(effectAxis.descriptor.coordinateSignature.value)
      writer.string(neuralAxis.descriptor.coordinateSignature.value)
      writer.string(support.identityDigest)
  def relationOrigins: RelationOrigins =
    RelationOrigins(RelationSource(acquisition, responseRevision, readout, preparation, noiseRevision), access.relationAccess, support)

final class RunReadoutRelation private (
    private[fit] val run: RunTrialReadout,
    private[fit] val origins: ReadoutRelationOrigins,
    private[fit] val resource: ReadoutResource
):
  private var closed = false
  private var consumed = false
  private[fit] def acquire(): Either[String, RunTrialReadout] = synchronized:
    if closed then Left("relation source scope is closed")
    else if consumed then Left("relation source scope has already been consumed")
    else resource.acquire().map: _ =>
      consumed = true
      run
  private[fit] def close(): Either[String, Unit] = synchronized:
    if !consumed || closed then Right(())
    else
      closed = true
      resource.close()
  private[fit] def whileOpen(task: => Unit): Unit = synchronized:
    if closed then throw IllegalStateException("relation source scope is closed")
    task

object RunReadoutRelation:
  def oneShot(run: RunTrialReadout, origins: ReadoutRelationOrigins, resource: ReadoutResource): RunReadoutRelation =
    require(origins.access == ReadoutRelationAccess.OneShot, "one-shot scope requires one-shot access")
    new RunReadoutRelation(run, origins, resource)

enum IdentifiedReadoutRelationError:
  case Axis(error: EvidenceError)
  case PartitionRunMismatch(expected: Vector[String], actual: Vector[String])
  case EffectReadoutMismatch(run: String, expected: Vector[String], actual: Vector[String])
  case NeuralFeatureMismatch(run: String, expected: Vector[String], actual: Vector[String])
  case Composition(run: String, detail: String)
  case Access(run: String, detail: String)
  case TaskFailure(detail: String)
  case CloseFailure(detail: String)
  case TaskAndCloseFailure(task: IdentifiedReadoutRelationError, closes: Vector[String])

  def message: String =
    this match
      case Axis(error) => error.message
      case PartitionRunMismatch(expected, actual) =>
        s"partition keys ${expected.mkString(",")} do not match run ids ${actual.mkString(",")}"
      case EffectReadoutMismatch(run, expected, actual) =>
        s"run $run effect keys ${actual.mkString(",")} do not match readout keys ${expected.mkString(",")}"
      case NeuralFeatureMismatch(run, expected, actual) =>
        s"run $run neural keys ${actual.mkString(",")} do not match feature keys ${expected.mkString(",")}"
      case Composition(run, detail) => s"run $run could not compose TrialReadout with response: $detail"
      case Access(run, detail) => s"run $run relation source access refused: $detail"
      case TaskFailure(detail) => s"relation task failed: $detail"
      case CloseFailure(detail) => s"relation resource close failed: $detail"
      case TaskAndCloseFailure(task, closes) => s"${task.message}; close failures: ${closes.mkString(",")}"

/** Builds B_r = A_r Y_r through Multivar's identified `Table`/`Lin` boundary.
  * It never materializes a trial image matrix. The supplied axes make the
  * scientific row and feature spaces explicit before composition.
  */
object IdentifiedReadoutRelations:
  private def guarded(operator: DoubleLinearOperator, input: RunReadoutRelation): DoubleLinearOperator =
    new DoubleLinearOperator:
      val rows = operator.rows
      val cols = operator.cols
      def applyTo(value: DVec, output: MutableDVec): Unit = input.whileOpen:
        operator.applyTo(value, output)
      override def transposeApplyTo(value: DVec, output: MutableDVec): Unit = input.whileOpen:
        operator.transposeApplyTo(value, output)

  def withRuns[PK, EK, NK, A](
      partitions: AxisRef[PK],
      effects: AxisRef[EK],
      neural: AxisRef[NK],
      inputs: Vector[RunReadoutRelation]
  )(
      task: RelationSet[partitions.Id, effects.Id, neural.Id] => Either[IdentifiedReadoutRelationError, A]
  ): Either[IdentifiedReadoutRelationError, A] =
    val acquired = scala.collection.mutable.ArrayBuffer.empty[RunReadoutRelation]
    val result =
      try fromRuns(partitions, effects, neural, inputs, acquired).flatMap(task)
      catch case NonFatal(error) => Left(IdentifiedReadoutRelationError.TaskFailure(detail(error)))
    val closes = acquired.toVector.flatMap: input =>
      try input.close().left.toOption
      catch case NonFatal(error) => Some(detail(error))
    if closes.isEmpty then result
    else result match
      case Left(error) => Left(IdentifiedReadoutRelationError.TaskAndCloseFailure(error, closes))
      case Right(_) => Left(IdentifiedReadoutRelationError.CloseFailure(closes.mkString(",")))

  private def detail(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getName)

  private def fromRuns[PK, EK, NK](
      partitions: AxisRef[PK],
      effects: AxisRef[EK],
      neural: AxisRef[NK],
      inputs: Vector[RunReadoutRelation],
      acquired: scala.collection.mutable.ArrayBuffer[RunReadoutRelation]
  ): Either[IdentifiedReadoutRelationError, RelationSet[partitions.Id, effects.Id, neural.Id]] =
    if inputs.length != partitions.size then
      Left(IdentifiedReadoutRelationError.PartitionRunMismatch(partitions.toRecord.stableKeys, inputs.map(_.run.runId.value)))
    else if partitions.toRecord.stableKeys != inputs.map(_.run.runId.value) then
      Left(IdentifiedReadoutRelationError.PartitionRunMismatch(partitions.toRecord.stableKeys, inputs.map(_.run.runId.value)))
    else
      val relations = Vector.newBuilder[Relation[effects.Id, neural.Id]]
      var index = 0
      while index < inputs.length do
        one(effects, neural, inputs(index), acquired) match
          case Left(error) => return Left(error)
          case Right(value) => relations += value
        index += 1
      RelationSet(partitions, effects, neural, relations.result())
        .left
        .map(IdentifiedReadoutRelationError.Axis.apply)

  private def one[EK, NK](
      effects: AxisRef[EK],
      neural: AxisRef[NK],
      input: RunReadoutRelation,
      acquired: scala.collection.mutable.ArrayBuffer[RunReadoutRelation]
  ): Either[IdentifiedReadoutRelationError, Relation[effects.Id, neural.Id]] =
    val run = input.run
    val readoutKeys = run.readout.axis.names
    val featureKeys = run.featureIndices.map(_.value.toString)
    if effects.toRecord.stableKeys != readoutKeys then
      Left(IdentifiedReadoutRelationError.EffectReadoutMismatch(run.runId.value, effects.toRecord.stableKeys, readoutKeys))
    else if neural.toRecord.stableKeys != featureKeys then
      Left(IdentifiedReadoutRelationError.NeuralFeatureMismatch(run.runId.value, neural.toRecord.stableKeys, featureKeys))
    else
      input.acquire().left.map(IdentifiedReadoutRelationError.Access(run.runId.value, _)).flatMap: _ =>
        acquired += input
        run.readout.operator.compose(run.timeSeries.value)
        .left
        .map(error => IdentifiedReadoutRelationError.Composition(run.runId.value, error.getMessage))
        .map(composed => guarded(composed, input))
        .flatMap: composed =>
          val identity = ValueIdentity.source(
            ValueId.unsafe("identified-readout-relation-" + input.origins.relationIdentity(effects, neural))
          )
          Lin.decode(
            composed,
            CoordinateEvidence.dual(neural.evidence),
            CoordinateEvidence.primal(effects.evidence),
            CoordinateEvidence.dual(neural.evidence).descriptor,
            CoordinateEvidence.primal(effects.evidence).descriptor,
            identity,
            SemanticProvenance
              .source("scalafim-identified-readout-relation")
              .append(SemanticProvenanceEvent.Derived("trial-readout-compose", Vector(identity)))
          ).left.map(error => IdentifiedReadoutRelationError.Composition(run.runId.value, error.message)).flatMap: table =>
            val estimability = run.readout.axis.estimability.map:
              case scalafim.fmri.fit.TrialEstimability.Estimable => EffectEstimability.Estimable
              case scalafim.fmri.fit.TrialEstimability.ZeroRegressor => EffectEstimability.NotEstimable("zero trial regressor")
            Relation(effects, neural, table, input.origins.relationOrigins, estimability)
              .left
              .map(IdentifiedReadoutRelationError.Axis.apply)
