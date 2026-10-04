package scalafim.fmri.mvpa.relation

import gale.linalg.{DMat, DVec, DoubleLinearOperator, LinearOperator, MutableDVec}
import multivar.core.{CoordinateEvidence, Lin, Primal, SemanticProvenance, SemanticProvenanceEvent, SemanticSpace, ValueId, ValueIdentity}
import scala.collection.mutable
import scala.util.control.NonFatal
import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, AxisRef, Column, EvidenceError, EvidenceIdentity, EvidenceOrigins, Observations}

sealed trait ObservationMeanAccess
object ObservationMeanAccess:
  case object OneShot extends ObservationMeanAccess
  final case class ScopedReplay(owner: String, revision: String) extends ObservationMeanAccess:
    require(owner.trim.nonEmpty && revision.trim.nonEmpty, "scoped replay owner and revision must be non-empty")

trait ObservationMeanResource:
  def acquire(): Either[String, Unit]
  def close(): Either[String, Unit]

final class ObservationMeanPlan[SK, PK, EK, S <: SemanticSpace, P <: SemanticSpace, E <: SemanticSpace] private (
    val samples: AxisRef[SK] { type Id = S },
    val partitions: AxisRef[PK] { type Id = P },
    val effects: AxisRef[EK] { type Id = E },
    val partition: Column[S, PK],
    val condition: Column[S, EK]
)

object ObservationMeanPlan:
  def apply[SK, PK, EK](
      samples: AxisRef[SK],
      partitions: AxisRef[PK],
      effects: AxisRef[EK],
      partition: Column[samples.Id, PK],
      condition: Column[samples.Id, EK]
  ): Either[EvidenceError, ObservationMeanPlan[SK, PK, EK, samples.Id, partitions.Id, effects.Id]] =
    if partition.rowAxis != samples.descriptor then
      Left(EvidenceError.AxisMismatch("mean partition column", samples.descriptor.stableKey, partition.rowAxis.stableKey))
    else if condition.rowAxis != samples.descriptor then
      Left(EvidenceError.AxisMismatch("mean condition column", samples.descriptor.stableKey, condition.rowAxis.stableKey))
    else Right(new ObservationMeanPlan[SK, PK, EK, samples.Id, partitions.Id, effects.Id](samples, partitions, effects, partition, condition))

final case class ObservationMeanOrigins(
    acquisitionRevision: String,
    responseRevision: String,
    readoutRevision: String,
    preparationRevision: String,
    noiseRevision: String,
    access: ObservationMeanAccess,
    support: EvidenceOrigins = EvidenceOrigins.Unknown
):
  require(acquisitionRevision.nonEmpty && responseRevision.nonEmpty && readoutRevision.nonEmpty && preparationRevision.nonEmpty && noiseRevision.nonEmpty, "mean relation revisions must be non-empty")
  private[relation] def source: RelationSource =
    RelationSource(acquisitionRevision, responseRevision, readoutRevision, preparationRevision, noiseRevision)
  private[relation] def binding[SK, PK, EK, NK, S <: SemanticSpace, P <: SemanticSpace, E <: SemanticSpace](
      plan: ObservationMeanPlan[SK, PK, EK, S, P, E], neural: AxisRef[NK],
      observationIdentity: EvidenceIdentity, memberships: Vector[Vector[Vector[Int]]]
  ): String =
    AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.observation-mean-relation.v1")
      writer.string(plan.samples.descriptor.stableKey)
      writer.string(plan.partitions.descriptor.stableKey)
      writer.string(plan.effects.descriptor.stableKey)
      writer.string(neural.descriptor.stableKey)
      EvidenceIdentity.writeValues(writer, plan.partition.valueIdentity)
      EvidenceIdentity.writeValues(writer, plan.condition.valueIdentity)
      writer.intLE(memberships.length)
      memberships.foreach: rows =>
        writer.intLE(rows.length)
        rows.foreach: members =>
          writer.intLE(members.length)
          members.foreach: member =>
            writer.intLE(member)
            writer.intLE(1)
            writer.intLE(members.length)
      writer.string("ordinary-cell-mean")
      observationIdentity.writeFramed(writer)
      support.writeFramed(writer)
      writer.string(acquisitionRevision)
      writer.string(responseRevision)
      writer.string(readoutRevision)
      writer.string(preparationRevision)
      writer.string(noiseRevision)

final class ObservationMeanSource[S <: multivar.core.SemanticSpace, N <: multivar.core.SemanticSpace] private (
    private[relation] val observations: Observations[S, N],
    private[relation] val origins: ObservationMeanOrigins,
    private[relation] val resource: ObservationMeanResource
):
  private var closed = false
  private var consumed = false
  private var scope: Option[ScopeReplay] = None

  private[relation] def acquire(): Either[String, Unit] = synchronized:
    if closed then Left("observation mean source scope is closed")
    else if consumed then Left("observation mean source scope has already been consumed")
    else resource.acquire().map(_ => consumed = true)

  private[relation] def mintScope(binding: String): Unit = synchronized:
    origins.access match
      case ObservationMeanAccess.ScopedReplay(owner, revision) if scope.isEmpty =>
        scope = Some(new ScopeReplay(owner, revision, binding))
      case _ => ()

  private[relation] def liveAccess: RelationAccess = synchronized:
    origins.access match
      case ObservationMeanAccess.OneShot => RelationAccess.OneShot
      case ObservationMeanAccess.ScopedReplay(_, _) =>
        RelationAccess.ScopedReplay(scope.getOrElse(throw IllegalStateException("scoped replay was not acquired")))

  private[relation] def close(): Either[String, Unit] = synchronized:
    if !consumed || closed then Right(())
    else
      closed = true
      scope.foreach(_.expire())
      resource.close()

  private[relation] def whileOpen(task: => Unit): Unit = synchronized:
    if !consumed then throw IllegalStateException("observation mean source has not been acquired")
    if closed then throw IllegalStateException("observation mean source scope is closed")
    task

object ObservationMeanSource:
  def oneShot[S <: multivar.core.SemanticSpace, N <: multivar.core.SemanticSpace](
      observations: Observations[S, N], origins: ObservationMeanOrigins, resource: ObservationMeanResource
  ): ObservationMeanSource[S, N] =
    require(origins.access == ObservationMeanAccess.OneShot, "one-shot source requires one-shot declaration")
    new ObservationMeanSource(observations, origins, resource)

  def scopedReplay[S <: multivar.core.SemanticSpace, N <: multivar.core.SemanticSpace](
      observations: Observations[S, N], origins: ObservationMeanOrigins, resource: ObservationMeanResource
  ): ObservationMeanSource[S, N] =
    require(origins.access.isInstanceOf[ObservationMeanAccess.ScopedReplay], "scoped source requires explicit scoped declaration")
    new ObservationMeanSource(observations, origins, resource)

enum ObservationMeanRelationError:
  case Axis(error: EvidenceError)
  case ForeignSamples(expected: AxisDescriptor, actual: AxisDescriptor)
  case Shape(detail: String, expected: Int, actual: Int)
  case UnknownPartition(sample: Int)
  case UnknownEffect(sample: Int)
  case Budget(required: BigInt, maximum: BigInt)
  case Capacity(required: BigInt)
  case Access(detail: String)
  case Composition(detail: String)
  case TaskFailure(detail: String)
  case CloseFailure(detail: String)
  case TaskAndCloseFailure(task: ObservationMeanRelationError, close: String)
  def message: String = this match
    case Axis(error) => error.message
    case ForeignSamples(expected, actual) => s"mean-plan samples ${expected.stableKey} do not match observations ${actual.stableKey}"
    case Shape(detail, expected, actual) => s"$detail expected $expected, got $actual"
    case UnknownPartition(sample) => s"sample $sample has a partition outside the declared partition axis"
    case UnknownEffect(sample) => s"sample $sample has an effect outside the declared effect axis"
    case Budget(required, maximum) => s"mean relation workspace $required exceeds declared budget $maximum"
    case Capacity(required) => s"mean relation workspace $required exceeds Int capacity"
    case Access(detail) => s"mean relation source access refused: $detail"
    case Composition(detail) => s"mean relation composition failed: $detail"
    case TaskFailure(detail) => s"mean relation task failed: $detail"
    case CloseFailure(detail) => s"mean relation resource close failed: $detail"
    case TaskAndCloseFailure(task, close) => s"${task.message}; close failure: $close"

final case class ObservationMeanBudget(maximumCells: Long):
  require(maximumCells >= 0L, "maximum mean cells must be non-negative")

object ObservationMeanRelations:
  private final case class Compiled[SK, PK, EK](
      memberships: Vector[Vector[Vector[Int]]],
      estimability: Vector[Vector[EffectEstimability]]
  )

  private def detail(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getName)

  private def compile[SK, PK, EK, S <: SemanticSpace, P <: SemanticSpace, E <: SemanticSpace](plan: ObservationMeanPlan[SK, PK, EK, S, P, E]): Either[ObservationMeanRelationError, Compiled[SK, PK, EK]] =
    if plan.partition.size != plan.samples.size then Left(ObservationMeanRelationError.Shape("partition column", plan.samples.size, plan.partition.size))
    else if plan.condition.size != plan.samples.size then Left(ObservationMeanRelationError.Shape("condition column", plan.samples.size, plan.condition.size))
    else
      val cells = Vector.fill(plan.partitions.size)(Vector.fill(plan.effects.size)(mutable.ArrayBuffer.empty[Int]))
      var sample = 0
      while sample < plan.samples.size do
        (plan.partition(sample), plan.condition(sample)) match
          case (Right(partition), Right(effect)) =>
            (plan.partitions.ordinalOf(partition), plan.effects.ordinalOf(effect)) match
              case (Some(partitionOrdinal), Some(effectOrdinal)) => cells(partitionOrdinal)(effectOrdinal) += sample
              case (None, _) => return Left(ObservationMeanRelationError.UnknownPartition(sample))
              case (_, None) => return Left(ObservationMeanRelationError.UnknownEffect(sample))
          case (Left(error), _) => return Left(ObservationMeanRelationError.Axis(error))
          case (_, Left(error)) => return Left(ObservationMeanRelationError.Axis(error))
        sample += 1
      val memberships = cells.map(_.map(_.toVector))
      val estimability = memberships.map(_.map: members =>
        if members.nonEmpty then EffectEstimability.Estimable
        else EffectEstimability.NotEstimable("no observations for declared partition/effect cell")
      )
      Right(Compiled(memberships, estimability))

  private def preflight[SK, PK, EK, NK, S <: SemanticSpace, P <: SemanticSpace, E <: SemanticSpace](
      plan: ObservationMeanPlan[SK, PK, EK, S, P, E], neural: AxisRef[NK],
      observations: Observations[plan.samples.Id, neural.Id], budget: ObservationMeanBudget
  ): Either[ObservationMeanRelationError, Unit] =
    if observations.sampleAxis != plan.samples.descriptor then Left(ObservationMeanRelationError.ForeignSamples(plan.samples.descriptor, observations.sampleAxis))
    else if observations.neuralAxis != neural.descriptor then
      Left(ObservationMeanRelationError.Axis(EvidenceError.AxisMismatch(
        "observation neural axis", neural.descriptor.stableKey, observations.neuralAxis.stableKey
      )))
    else preflightDimensions(plan, neural, budget)

  private def preflightDimensions[SK, PK, EK, NK, S <: SemanticSpace, P <: SemanticSpace, E <: SemanticSpace](
      plan: ObservationMeanPlan[SK, PK, EK, S, P, E], neural: AxisRef[NK],
      budget: ObservationMeanBudget, ownedSnapshot: Boolean = false
  ): Either[ObservationMeanRelationError, Unit] =
    val partitions = BigInt(plan.partitions.size)
    val effects = BigInt(plan.effects.size)
    val samples = BigInt(plan.samples.size)
    val neuralWidth = BigInt(neural.size)
    val cells = partitions * effects
    // Membership entries, per-cell metadata, and both forward/adjoint
    // operator buffers are explicit before any source or snapshot allocation.
    val scopedRequired = 16 * (samples + effects + neuralWidth) + (cells * 2) + (cells * neuralWidth * 2)
    val required = if ownedSnapshot then scopedRequired + (samples * neuralWidth * 4) else scopedRequired
    if required > BigInt(Int.MaxValue) then Left(ObservationMeanRelationError.Capacity(required))
    else if required > BigInt(budget.maximumCells) then Left(ObservationMeanRelationError.Budget(required, BigInt(budget.maximumCells)))
    else Right(())

  /** Copy a dense fixture under an actual owned reader identity. This is the
    * only path that returns retained mean relations directly; it does not
    * promote a one-shot or scoped provider to owned replay.
    */
  def fromOwnedDense[SK, PK, EK, NK, S <: SemanticSpace, P <: SemanticSpace, E <: SemanticSpace](
      plan: ObservationMeanPlan[SK, PK, EK, S, P, E],
      neural: AxisRef[NK],
      values: DMat,
      evidenceSource: scalafim.fmri.mvpa.EvidenceSource,
      relationSource: RelationSource,
      readerOwner: String,
      budget: ObservationMeanBudget
  ): Either[ObservationMeanRelationError, RelationSet[plan.partitions.Id, plan.effects.Id, neural.Id]] =
    if readerOwner.trim.isEmpty then Left(ObservationMeanRelationError.Access("owned reader owner must be non-empty"))
    else if values.rows != plan.samples.size then Left(ObservationMeanRelationError.Shape("owned observations rows", plan.samples.size, values.rows))
    else if values.cols != neural.size then Left(ObservationMeanRelationError.Shape("owned observations columns", neural.size, values.cols))
    else
      preflightDimensions(plan, neural, budget, ownedSnapshot = true).flatMap: _ =>
        val copy = DMat.dense(values.rows, values.cols, values.valuesRowMajor.toArray.toSeq)
        val content = AxisDigest.sha256Hex: writer =>
          writer.string("scalafim.observation-mean-owned-values.v1")
          writer.intLE(copy.rows)
          writer.intLE(copy.cols)
          copy.valuesRowMajor.foreach: value =>
            val bits = java.lang.Double.doubleToLongBits(value)
            writer.intLE((bits >>> 32).toInt)
            writer.intLE(bits.toInt)
        val identity = ValueIdentity.source(ValueId.unsafe(s"owned-observation-mean-$content"))
        val observationOrigins = EvidenceOrigins.unknown(evidenceSource, identity, plan.samples.descriptor)
        Observations.fromDense(plan.samples, neural, copy, identity, evidenceSource, observationOrigins)
          .left.map(ObservationMeanRelationError.Axis.apply)
          .flatMap: observations =>
            for
              _ <- preflight(plan, neural, observations, budget)
              compiled <- compile(plan)
              relations <- buildOwnedRelations(plan, neural, observations, relationSource, readerOwner, compiled)
            yield relations

  private def meanLeg[SK, EK](
      samples: AxisRef[SK], effects: AxisRef[EK], members: Vector[Vector[Int]], identity: ValueIdentity
  ): Either[ObservationMeanRelationError, Lin[Primal[samples.Id], Primal[effects.Id]]] =
    val operator = LinearOperator.fromFunctions(effects.size, samples.size)(
      (input, output) =>
        var effect = 0
        while effect < effects.size do
          val cell = members(effect)
          var sum = 0.0
          var correction = 0.0
          var index = 0
          while index < cell.length do
            // Divide before reduction, matching the adjoint coefficients and
            // avoiding overflow for a finite convex mean such as 1e308.
            val value = input(cell(index)) / cell.length.toDouble
            val next = sum + value
            correction += (if math.abs(sum) >= math.abs(value) then (sum - next) + value else (value - next) + sum)
            sum = next
            index += 1
          output(effect) = sum + correction
          effect += 1,
      (input, output) =>
        output.clear()
        var effect = 0
        while effect < effects.size do
          val cell = members(effect)
          if cell.nonEmpty then
            val weight = input(effect) / cell.length.toDouble
            var index = 0
            while index < cell.length do
              val sample = cell(index)
              output(sample) = output(sample) + weight
              index += 1
          effect += 1
    )
    Lin.fromLinearMap(
      operator, CoordinateEvidence.primal(samples.evidence), CoordinateEvidence.primal(effects.evidence), identity,
      SemanticProvenance.source("scalafim-observation-means").append(SemanticProvenanceEvent.Derived("ordinary-cell-mean", Vector(identity)))
    ).left.map(error => ObservationMeanRelationError.Composition(error.message))

  def withSource[SK, PK, EK, NK, A, S <: SemanticSpace, P <: SemanticSpace, E <: SemanticSpace](
      plan: ObservationMeanPlan[SK, PK, EK, S, P, E], neural: AxisRef[NK],
      source: ObservationMeanSource[plan.samples.Id, neural.Id], budget: ObservationMeanBudget
  )(task: RelationSet[plan.partitions.Id, plan.effects.Id, neural.Id] => Either[ObservationMeanRelationError, A]): Either[ObservationMeanRelationError, A] =
    var acquired = false
    val result =
      try
        for
          _ <- preflight(plan, neural, source.observations, budget)
          compiled <- compile(plan)
          _ <- source.acquire().left.map(ObservationMeanRelationError.Access.apply).map: _ =>
            acquired = true
          _ = source.mintScope(source.origins.binding(plan, neural, source.observations.identity, compiled.memberships))
          relations <- buildRelations(plan, neural, source, compiled)
          value <- task(relations)
        yield value
      catch case NonFatal(error) => Left(ObservationMeanRelationError.TaskFailure(detail(error)))
    val close = if !acquired then Right(()) else
      try source.close() catch case NonFatal(error) => Left(detail(error))
    close match
      case Right(_) => result
      case Left(closeError) => result match
        case Right(_) => Left(ObservationMeanRelationError.CloseFailure(closeError))
        case Left(error) => Left(ObservationMeanRelationError.TaskAndCloseFailure(error, closeError))

  private def buildRelations[SK, PK, EK, NK, S <: SemanticSpace, P <: SemanticSpace, E <: SemanticSpace](
      plan: ObservationMeanPlan[SK, PK, EK, S, P, E], neural: AxisRef[NK],
      source: ObservationMeanSource[plan.samples.Id, neural.Id], compiled: Compiled[SK, PK, EK]
  ): Either[ObservationMeanRelationError, RelationSet[plan.partitions.Id, plan.effects.Id, neural.Id]] =
    val relations = Vector.newBuilder[Relation[plan.effects.Id, neural.Id]]
    val binding = source.origins.binding(plan, neural, source.observations.identity, compiled.memberships)
    var partition = 0
    while partition < plan.partitions.size do
      val identity = ValueIdentity.source(ValueId.unsafe(s"observation-mean-$binding-$partition"))
      val built = meanLeg(plan.samples, plan.effects, compiled.memberships(partition), identity).flatMap: leg =>
        val estimate = source.observations.patterns.andThen(leg)
        Lin.fromLinearMap(
          guardedEstimateOperator(estimate, source),
          CoordinateEvidence.dual(neural.evidence), CoordinateEvidence.primal(plan.effects.evidence), estimate.valueIdentity,
          SemanticProvenance.source("scalafim-observation-mean-guard").append(SemanticProvenanceEvent.Derived("guarded-mean", Vector(estimate.valueIdentity)))
        ).left.map(error => ObservationMeanRelationError.Composition(error.message)).flatMap: table =>
          val support = source.observations.origins.reindexOutput(plan.effects.descriptor, table.valueIdentity)
          Relation.fromAcquired(plan.effects, neural, table, RelationOrigins(source.origins.source, source.liveAccess, support), compiled.estimability(partition))
            .left.map(ObservationMeanRelationError.Axis.apply)
      built match
        case Left(error) => return Left(error)
        case Right(relation) => relations += relation
      partition += 1
    RelationSet(plan.partitions, plan.effects, neural, relations.result()).left.map(ObservationMeanRelationError.Axis.apply)

  private def buildOwnedRelations[SK, PK, EK, NK, S <: SemanticSpace, P <: SemanticSpace, E <: SemanticSpace](
      plan: ObservationMeanPlan[SK, PK, EK, S, P, E],
      neural: AxisRef[NK],
      observations: Observations[plan.samples.Id, neural.Id],
      relationSource: RelationSource,
      readerOwner: String,
      compiled: Compiled[SK, PK, EK]
  ): Either[ObservationMeanRelationError, RelationSet[plan.partitions.Id, plan.effects.Id, neural.Id]] =
    val relations = Vector.newBuilder[Relation[plan.effects.Id, neural.Id]]
    val binding = ownedBinding(plan, neural, observations.identity, relationSource, compiled.memberships)
    var partition = 0
    while partition < plan.partitions.size do
      val identity = ValueIdentity.source(ValueId.unsafe("owned-observation-mean-" + binding + "-" + partition))
      val built = meanLeg(plan.samples, plan.effects, compiled.memberships(partition), identity).flatMap: leg =>
        val table = observations.patterns.andThen(leg)
        val support = observations.origins.reindexOutput(plan.effects.descriptor, table.valueIdentity)
        Relation.fromAcquired(
          plan.effects, neural, table,
          RelationOrigins(relationSource, RelationAccess.OwnedReplay(readerOwner), support),
          compiled.estimability(partition)
        ).left.map(ObservationMeanRelationError.Axis.apply)
      built match
        case Left(error) => return Left(error)
        case Right(relation) => relations += relation
      partition += 1
    RelationSet(plan.partitions, plan.effects, neural, relations.result()).left.map(ObservationMeanRelationError.Axis.apply)

  private def ownedBinding[SK, PK, EK, NK, S <: SemanticSpace, P <: SemanticSpace, E <: SemanticSpace](
      plan: ObservationMeanPlan[SK, PK, EK, S, P, E],
      neural: AxisRef[NK],
      observationIdentity: EvidenceIdentity,
      source: RelationSource,
      memberships: Vector[Vector[Vector[Int]]]
  ): String =
    AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.observation-mean-owned-relation.v1")
      writer.string(plan.samples.descriptor.stableKey)
      writer.string(plan.partitions.descriptor.stableKey)
      writer.string(plan.effects.descriptor.stableKey)
      writer.string(neural.descriptor.stableKey)
      EvidenceIdentity.writeValues(writer, plan.partition.valueIdentity)
      EvidenceIdentity.writeValues(writer, plan.condition.valueIdentity)
      writer.intLE(memberships.length)
      memberships.foreach: effects =>
        writer.intLE(effects.length)
        effects.foreach: members =>
          writer.intLE(members.length)
          members.foreach: member =>
            writer.intLE(member)
            writer.intLE(1)
            writer.intLE(members.length)
      writer.string("ordinary-cell-mean")
      observationIdentity.writeFramed(writer)
      writer.string(source.acquisitionRevision)
      writer.string(source.responseRevision)
      writer.string(source.readoutRevision)
      writer.string(source.preparationRevision)
      writer.string(source.noiseRevision)

  private def guardedEstimateOperator[E <: multivar.core.SemanticSpace, N <: multivar.core.SemanticSpace](
      table: Lin[multivar.core.Dual[N], Primal[E]], source: ObservationMeanSource[?, ?]
  ): DoubleLinearOperator =
    new DoubleLinearOperator:
      val rows = table.rows
      val cols = table.cols
      def applyTo(input: DVec, output: MutableDVec): Unit = source.whileOpen:
        table.apply(DMat.dense(input.length, 1, Vector.tabulate(input.length)(input.apply))) match
          case Left(error) => throw IllegalStateException(error.message)
          case Right(result) =>
            var row = 0
            while row < rows do
              output(row) = result(row, 0)
              row += 1
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = source.whileOpen:
        table.star.apply(DMat.dense(input.length, 1, Vector.tabulate(input.length)(input.apply))) match
          case Left(error) => throw IllegalStateException(error.message)
          case Right(result) =>
            var row = 0
            while row < cols do
              output(row) = result(row, 0)
              row += 1
