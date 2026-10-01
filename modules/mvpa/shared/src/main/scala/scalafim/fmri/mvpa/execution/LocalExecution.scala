package scalafim.fmri.mvpa.execution

import resample4s.core.{Seed, StreamDomain, StreamPath}
import scalafim.fmri.mvpa.analysis.PlanId
import scalafim.fmri.mvpa.measurement.MeasurementId
import scalafim.pipeline.{ArtifactKind, LocalPipelineRunner, PipelineBuilder, PipelineError, PipelineStep, RunContext, StepId}
import scala.util.control.NonFatal

/** A scientific coordinate, supplied by the bound design rather than assigned
  * by a worker or traversal.  These values are also the only ordinals used in
  * random-stream derivation.
  */
opaque type SplitCoordinate = Int
object SplitCoordinate:
  def apply(value: Int): Either[AddressError, SplitCoordinate] =
    if value < 0 then Left(AddressError.NegativeCoordinate("split", value)) else Right(value)
  extension (value: SplitCoordinate) def value: Int = value

opaque type ReplicateCoordinate = Int
object ReplicateCoordinate:
  def apply(value: Int): Either[AddressError, ReplicateCoordinate] =
    if value < 0 then Left(AddressError.NegativeCoordinate("replicate", value)) else Right(value)
  extension (value: ReplicateCoordinate) def value: Int = value

opaque type StageCoordinate = Int
object StageCoordinate:
  def apply(value: Int): Either[AddressError, StageCoordinate] =
    if value < 0 then Left(AddressError.NegativeCoordinate("stage", value)) else Right(value)
  extension (value: StageCoordinate) def value: Int = value

/** The canonical ordinal emitted with a frame entry.  It is deliberately not
  * derived from a measurement id (or from a traversal). */
opaque type MeasurementCoordinate = Int
object MeasurementCoordinate:
  def apply(value: Int): Either[AddressError, MeasurementCoordinate] =
    if value < 0 then Left(AddressError.NegativeCoordinate("measurement", value)) else Right(value)
  extension (value: MeasurementCoordinate) def value: Int = value

enum AddressError:
  case NegativeCoordinate(name: String, observed: Int)
  case DuplicateAddress(address: WorkAddress)
  case UnexpectedAttempt(address: WorkAddress)
  case MixedPlans(expected: PlanId, observed: PlanId)
  case InconsistentMeasurement(address: WorkAddress)

/** Immutable identity of one attempted scientific unit. */
final case class WorkAddress(
    plan: PlanId,
    split: SplitCoordinate,
    replicate: ReplicateCoordinate,
    stage: StageCoordinate,
    measurement: MeasurementId,
    measurementCoordinate: MeasurementCoordinate
):
  def canonicalKey: (String, Int, Int, Int, String, Int) =
    (plan.text, split.value, replicate.value, stage.value, measurement.value, measurementCoordinate.value)

object WorkAddress:
  given Ordering[WorkAddress] with
    def compare(left: WorkAddress, right: WorkAddress): Int =
      Ordering.Tuple6[String, Int, Int, Int, String, Int].compare(left.canonicalKey, right.canonicalKey)

  /** Domain-separated streams use only design/frame coordinates.  No string
    * hash or worker index participates in seed assignment. */
  def seed(root: Seed, address: WorkAddress): Seed =
    val splitDomain = StreamDomain.custom(311).fold(error => throw IllegalStateException(error.toString), identity)
    val replicateDomain = StreamDomain.custom(312).fold(error => throw IllegalStateException(error.toString), identity)
    val stageDomain = StreamDomain.custom(313).fold(error => throw IllegalStateException(error.toString), identity)
    val measurementDomain = StreamDomain.custom(314).fold(error => throw IllegalStateException(error.toString), identity)
    val path =
      StreamPath.of(splitDomain, address.split.value)
        .flatMap(_.append(replicateDomain, address.replicate.value))
        .flatMap(_.append(stageDomain, address.stage.value))
        .flatMap(_.append(measurementDomain, address.measurementCoordinate.value))
        .fold(error => throw IllegalStateException(error.toString), identity)
    val planDomain = StreamDomain.custom(315).fold(error => throw IllegalStateException(error.toString), identity)
    val idDomain = StreamDomain.custom(316).fold(error => throw IllegalStateException(error.toString), identity)
    val withPlan = address.plan.text.grouped(7).foldLeft(path): (current, part) =>
      current.append(planDomain, Integer.parseInt(part, 16)).fold(error => throw IllegalStateException(error.toString), identity)
    val complete = address.measurement.value.foldLeft(withPlan): (current, character) =>
      current.append(idDomain, character.toInt).fold(error => throw IllegalStateException(error.toString), identity)
    root.derive(complete)

/** An owned resource is acquired for one unit and released by that unit only. */
trait ExecutionResource:
  def close(): Unit

enum UnitError:
  case Compute(detail: String)
  case Acquire(detail: String)
  case Close(detail: String)
  case ComputeAndClose(compute: UnitError, close: UnitError)
  case BeforeCommit(detail: String)
  case Pipeline(detail: String)
  case Control(detail: String)

/** A contribution fingerprint is supplied by the numerical program.  It is
  * compared before a retry is admitted, so conflicting duplicate results are
  * visible instead of being reduced twice. */
final case class UnitContribution[+C, +O](
    fingerprint: String,
    reduction: C,
    outOfFold: Vector[O]
):
  require(fingerprint.nonEmpty && fingerprint == fingerprint.trim, "contribution fingerprint must be non-empty and trimmed")

enum UnitEvaluation[+C, +O]:
  case Complete(value: UnitContribution[C, O])
  case Partial(reason: String)

/** The program owns compute; the execution layer owns its resource lifetime,
  * seed derivation, pipeline invocation, and commit boundary. */
trait WorkUnit[C, O]:
  val address: WorkAddress
  def acquire(): Either[UnitError, ExecutionResource]
  def compute(resource: ExecutionResource, seed: Seed): Either[UnitError, UnitEvaluation[C, O]]

trait BeforeCommit:
  def check(address: WorkAddress): Either[UnitError, Unit]

object BeforeCommit:
  val allow: BeforeCommit = new BeforeCommit:
    def check(address: WorkAddress): Either[UnitError, Unit] = Right(())

trait Reduction[C, R]:
  def empty: R
  def add(current: R, contribution: C): R

final case class CommittedUnit[+C, +O](address: WorkAddress, contribution: UnitContribution[C, O])

final case class Coverage(expected: Int, committed: Int, missing: Vector[WorkAddress]):
  require(expected >= 0 && committed >= 0 && committed <= expected, "coverage must be within the expected family")

final case class CompleteFamily[+C, +O, +R](
    coverage: Coverage,
    reduction: R,
    outOfFold: Vector[O],
    committed: Vector[CommittedUnit[C, O]]
)

final case class IncompleteFamily[+C, +O](
    coverage: Coverage,
    committed: Vector[CommittedUnit[C, O]],
    partial: Vector[(WorkAddress, String)]
)

enum FamilyState[+C, +O, +R]:
  case Complete(value: CompleteFamily[C, O, R])
  case Partial(value: IncompleteFamily[C, O])
  case Cancelled(value: IncompleteFamily[C, O])
  case Failed(value: IncompleteFamily[C, O], failures: Vector[(WorkAddress, UnitError)])
  case ReductionFailed(value: IncompleteFamily[C, O], detail: String)

enum ResumeRefusal:
  case InMemoryOnly(reason: String)

object LocalExecution:
  val resumeRefusal: ResumeRefusal =
    ResumeRefusal.InMemoryOnly("local execution retains no durable committed-unit checkpoint")

  private final case class PipelineWork[C, O](unit: WorkUnit[C, O], seed: Seed, beforeCommit: BeforeCommit)

  private final class ComputeStep[C, O] extends PipelineStep[PipelineWork[C, O], Either[UnitError, UnitEvaluation[C, O]]]:
    val id: StepId = StepId.unsafe("compute")
    val outputKind: ArtifactKind[Either[UnitError, UnitEvaluation[C, O]]] =
      ArtifactKind.unsafe("mvpa-unit-evaluation")

    def run(input: PipelineWork[C, O], context: RunContext): Either[PipelineError, Either[UnitError, UnitEvaluation[C, O]]] =
      Right(evaluate(input))

    private def evaluate(input: PipelineWork[C, O]): Either[UnitError, UnitEvaluation[C, O]] =
      val acquisition =
        try input.unit.acquire()
        catch case NonFatal(error) => Left(UnitError.Acquire(detail(error)))
      acquisition match
        case Left(error) => Left(error)
        case Right(resource) =>
          var computed = Option.empty[Either[UnitError, UnitEvaluation[C, O]]]
          var closeFailure = Option.empty[UnitError]
          try computed = Some(input.unit.compute(resource, input.seed))
          catch case NonFatal(error) => computed = Some(Left(UnitError.Compute(detail(error))) )
          try resource.close()
          catch case NonFatal(error) => closeFailure = Some(UnitError.Close(detail(error)))
          (computed, closeFailure) match
            case (Some(Left(compute)), Some(close)) => Left(UnitError.ComputeAndClose(compute, close))
            case (_, Some(close)) => Left(close)
            case (Some(Left(error)), None) => Left(error)
            case (Some(Right(value @ UnitEvaluation.Complete(_))), None) =>
              try input.beforeCommit.check(input.unit.address).map(_ => value)
              catch case NonFatal(error) => Left(UnitError.BeforeCommit(detail(error)))
            case (Some(Right(value @ UnitEvaluation.Partial(_))), None) => Right(value)
            case (None, None) => Left(UnitError.Compute("compute returned no result"))

  private def detail(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getName)

  /** Runs a finite family through the existing local pipeline runner.  The
    * reducer is reconstructed in canonical address order after each committed
    * boundary, which makes reductions and OOF rows independent of scheduling.
    */
  def run[C, O, R](
      expected: Vector[WorkUnit[C, O]],
      rootSeed: Seed,
      reduction: Reduction[C, R],
      attempts: Option[Vector[WorkUnit[C, O]]] = None,
      cancellationRequested: () => Boolean = () => false,
      beforeCommit: BeforeCommit = BeforeCommit.allow
  ): Either[AddressError, FamilyState[C, O, R]] =
    validate(expected).flatMap: family =>
      val attempted = attempts.getOrElse(family)
      val expectedAddresses = family.map(_.address).toSet
      val admissible = attempted.find(unit => !expectedAddresses.contains(unit.address)) match
        case Some(unit) => Left(AddressError.UnexpectedAttempt(unit.address))
        case None => Right(())
      admissible.map: _ =>
        var committed = Map.empty[WorkAddress, UnitContribution[C, O]]
        var failures = Vector.empty[(WorkAddress, UnitError)]
        var fatalFailures = Vector.empty[(WorkAddress, UnitError)]
        var partial = Vector.empty[(WorkAddress, String)]
        var cancelled = false
        val schedule = attempted
        var index = 0
        while index < schedule.length && !cancelled && fatalFailures.isEmpty do
          val unit = schedule(index)
          val cancellation =
            try Right(cancellationRequested())
            catch case NonFatal(error) => Left(UnitError.Control(detail(error)))
          cancellation match
            case Left(error) =>
              fatalFailures = fatalFailures :+ (unit.address -> error)
              index = schedule.length
            case Right(true) => cancelled = true
            case Right(false) => evaluateThroughPipeline(unit, WorkAddress.seed(rootSeed, unit.address), beforeCommit) match
                case Left(error @ (UnitError.Close(_) | UnitError.ComputeAndClose(_, _))) =>
                  fatalFailures = fatalFailures :+ (unit.address -> error)
                case Left(error) => failures = failures :+ (unit.address -> error)
                case Right(UnitEvaluation.Partial(reason)) => partial = partial :+ (unit.address -> reason)
                case Right(UnitEvaluation.Complete(value)) =>
                  committed.get(unit.address) match
                    case None => committed = committed.updated(unit.address, value)
                    case Some(previous) if previous == value => ()
                    case Some(_) => fatalFailures = fatalFailures :+ (unit.address -> UnitError.BeforeCommit("conflicting retry contribution"))
          index += 1
        val ordered = committed.toVector.sortBy(_._1).map((address, value) => CommittedUnit(address, value))
        val missing = family.map(_.address).filterNot(committed.contains).sorted
        val coverage = Coverage(family.length, ordered.length, missing)
        val unresolvedFailures = failures.filter((address, _) => !committed.contains(address)) ++ fatalFailures
        val unresolvedPartial = partial.filter((address, _) => !committed.contains(address)).sortBy(_._1)
        val incomplete = IncompleteFamily(coverage, ordered, unresolvedPartial)
        if unresolvedFailures.nonEmpty then FamilyState.Failed(incomplete, unresolvedFailures.sortBy(_._1))
        else if cancelled then FamilyState.Cancelled(incomplete)
        else if unresolvedPartial.nonEmpty || missing.nonEmpty then FamilyState.Partial(incomplete)
        else
          try
            val aggregate = ordered.foldLeft(reduction.empty)((state, unit) => reduction.add(state, unit.contribution.reduction))
            FamilyState.Complete(CompleteFamily(coverage, aggregate, ordered.flatMap(_.contribution.outOfFold), ordered))
          catch case NonFatal(error) => FamilyState.ReductionFailed(incomplete, detail(error))

  private def validate[C, O](units: Vector[WorkUnit[C, O]]): Either[AddressError, Vector[WorkUnit[C, O]]] =
    units.headOption match
      case None => Right(Vector.empty)
      case Some(first) =>
        units.find(_.address.plan != first.address.plan) match
          case Some(other) => Left(AddressError.MixedPlans(first.address.plan, other.address.plan))
          case None =>
            val ids = scala.collection.mutable.HashMap.empty[String, Int]
            val coordinates = scala.collection.mutable.HashMap.empty[Int, String]
            var i = 0
            while i < units.size do
              val address = units(i).address
              val id = address.measurement.value
              val coordinate = address.measurementCoordinate.value
              if ids.get(id).exists(_ != coordinate) || coordinates.get(coordinate).exists(_ != id) then
                return Left(AddressError.InconsistentMeasurement(address))
              ids.update(id, coordinate)
              coordinates.update(coordinate, id)
              i += 1
            units.groupBy(_.address).collectFirst { case (address, values) if values.length > 1 => address } match
              case Some(address) => Left(AddressError.DuplicateAddress(address))
              case None => Right(units.sortBy(_.address))

  private def evaluateThroughPipeline[C, O](
      unit: WorkUnit[C, O],
      seed: Seed,
      beforeCommit: BeforeCommit
  ): Either[UnitError, UnitEvaluation[C, O]] =
    val inputKind = ArtifactKind.unsafe[PipelineWork[C, O]]("mvpa-unit-work")
    val step = new ComputeStep[C, O]
    val built =
      for
        builder <- PipelineBuilder("mvpa.execution")
        input <- builder.input("work", inputKind)
        output <- input.builder.step("compute", step, input.ref)
      yield (output.builder.build, input.ref, output.ref)
    built match
      case Left(error) => Left(UnitError.Pipeline(error.message))
      case Right((graph, input, output)) =>
        val run = LocalPipelineRunner.run(graph, RunContext.empty.withInput(input, PipelineWork(unit, seed, beforeCommit)))
        run.error match
          case Some(error) => Left(UnitError.Pipeline(error.message))
          case None => run.get(output).fold(error => Left(UnitError.Pipeline(error.message)), identity)
