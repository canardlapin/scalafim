package scalafim.fmri.mvpa.measurement

import multivar.core.SemanticSpace
import scalafim.fmri.mvpa.{AxisDigest, AxisRef, Observations}
import scala.util.control.NonFatal

trait PackedMeasurementEntry[N <: SemanticSpace, SK, +R]:
  type Local <: SemanticSpace
  def measurement: MeasurementLeg[N, SK, Local]
  def rendition: R

object PackedMeasurementEntry:
  def apply[N <: SemanticSpace, SK, LK <: SemanticSpace, R](value: MeasurementLeg[N, SK, LK], metadata: R): PackedMeasurementEntry[N, SK, R] =
    new PackedMeasurementEntry[N, SK, R]:
      type Local = LK
      val measurement: MeasurementLeg[N, SK, LK] = value
      val rendition: R = metadata

trait MeasurementResource[+A]:
  def value: A
  def close(): Unit

object MeasurementResource:
  def apply[A](opened: A)(release: => Unit): MeasurementResource[A] =
    new MeasurementResource[A]:
      val value: A = opened
      def close(): Unit = release

enum MeasurementFailure:
  case Open(error: MeasurementError)
  case Measure(error: MeasurementError)
  case Task(detail: String)
  case Ordering(previous: String, next: String)

trait MeasurementVisitor[S <: SemanticSpace, N <: SemanticSpace, SK, R, A]:
  def visit[L <: SemanticSpace](entry: PackedMeasurementEntry[N, SK, R] { type Local = L }, measured: MeasuredObservations[S, L]): Either[MeasurementFailure, A]

final case class FrameOutcome[R, A](index: Int, descriptor: MeasurementDescriptor, rendition: R, value: Either[MeasurementFailure, A])
enum FrameTraversalError:
  case InvalidBudget(value: Int)
  case Open(error: MeasurementError)
  case OpenException(detail: String)
  case Iterator(detail: String)
  case Close(detail: String)

final case class FrameTraversalResult[A](value: A, error: Option[FrameTraversalError])

final case class MeasurementFrameDeclaration(
    key: String,
    revision: String,
    parameters: Vector[(String, String)]
):
  require(key.nonEmpty && revision.nonEmpty, "frame declaration key and revision must be non-empty")

  val fingerprint: String =
    AxisDigest.sha256Hex: writer =>
      writer.string("scalafim-measurement-frame/v1")
      writer.string(key)
      writer.string(revision)
      writer.intLE(parameters.length)
      parameters.sortBy((name, value) => (name, value)).foreach: (name, value) =>
        writer.string(name)
        writer.string(value)

final case class MeasurementFrameIdentity(
    source: scalafim.fmri.mvpa.AxisDescriptor,
    declaration: MeasurementFrameDeclaration
)

final class MeasurementFrame[N <: SemanticSpace, SK, R] private[measurement] (
    val source: AxisRef[SK] { type Id = N },
    private val openEntries: () => Iterator[PackedMeasurementEntry[N, SK, R]],
    val identity: MeasurementFrameIdentity
):
  def traverse[S <: SemanticSpace, A](maxOpenResources: Int)(open: => Either[MeasurementError, MeasurementResource[Observations[S, N]]])(visitor: MeasurementVisitor[S, N, SK, R, A]): FrameTraversalResult[Vector[FrameOutcome[R, A]]] =
    fold(maxOpenResources)(open)(visitor)(Vector.empty)((values, outcome) => values :+ outcome)

  def fold[S <: SemanticSpace, A, B](maxOpenResources: Int)(open: => Either[MeasurementError, MeasurementResource[Observations[S, N]]])(visitor: MeasurementVisitor[S, N, SK, R, A])(initial: B)(add: (B, FrameOutcome[R, A]) => B): FrameTraversalResult[B] =
    if maxOpenResources < 1 then FrameTraversalResult(initial, Some(FrameTraversalError.InvalidBudget(maxOpenResources)))
    else
      val opened: Either[FrameTraversalError, MeasurementResource[Observations[S, N]]] =
        try open.left.map(FrameTraversalError.Open.apply)
        catch case NonFatal(error) => Left(FrameTraversalError.OpenException(Option(error.getMessage).getOrElse(error.getClass.getName)))
      opened match
        case Left(error) => FrameTraversalResult(initial, Some(error))
        case Right(resource) =>
          var state = initial
          var failure: Option[FrameTraversalError] = None
          try
            val iterator = openEntries()
            var index = 0
            var previous: Option[String] = None
            while failure.isEmpty && iterator.hasNext do
              val entry = iterator.next()
              val semanticId = entry.measurement.descriptor.id.value
              val outcome =
                if entry.measurement.source.descriptor != source.descriptor then Left(MeasurementFailure.Measure(MeasurementError.SourceMismatch(source.descriptor, entry.measurement.source.descriptor)))
                else if previous.exists(_ >= semanticId) then Left(MeasurementFailure.Ordering(previous.get, semanticId))
                else
                  entry.measurement.measure(resource.value) match
                    case Left(error) => Left(MeasurementFailure.Measure(error))
                    case Right(measured) =>
                      try visitor.visit(entry, measured)
                      catch case NonFatal(error) => Left(MeasurementFailure.Task(Option(error.getMessage).getOrElse(error.getClass.getName)))
              state = add(state, FrameOutcome(index, entry.measurement.descriptor, entry.rendition, outcome))
              if previous.forall(_ < semanticId) then previous = Some(semanticId)
              index += 1
          catch case NonFatal(error) => failure = Some(FrameTraversalError.Iterator(Option(error.getMessage).getOrElse(error.getClass.getName)))
          finally
            try resource.close()
            catch case NonFatal(error) => if failure.isEmpty then failure = Some(FrameTraversalError.Close(Option(error.getMessage).getOrElse(error.getClass.getName)))
          FrameTraversalResult(state, failure)

object MeasurementFrame:
  def lazyFrame[N <: SemanticSpace, SK, R](source: AxisRef[SK] { type Id = N }, declaration: MeasurementFrameDeclaration)(entries: => Iterator[PackedMeasurementEntry[N, SK, R]]): MeasurementFrame[N, SK, R] =
    new MeasurementFrame(source, () => entries, MeasurementFrameIdentity(source.descriptor, declaration))

  def apply[N <: SemanticSpace, SK, R](source: AxisRef[SK] { type Id = N }, values: Seq[PackedMeasurementEntry[N, SK, R]]): Either[MeasurementError, MeasurementFrame[N, SK, R]] =
    if values.isEmpty then Left(MeasurementError.EmptyFrame)
    else
      val ordered = values.toVector.sortBy(_.measurement.descriptor.id.value)
      var index = 0
      while index < ordered.length do
        val entry = ordered(index)
        if entry.measurement.source.descriptor != source.descriptor then return Left(MeasurementError.SourceMismatch(source.descriptor, entry.measurement.source.descriptor))
        if index > 0 && ordered(index - 1).measurement.descriptor.id == entry.measurement.descriptor.id then return Left(MeasurementError.DuplicateMeasurement(entry.measurement.descriptor.id))
        index += 1
      val declaration = MeasurementFrameDeclaration(
        "materialized-measurement-frame",
        "v1",
        ordered.map(entry => "measurement" -> entry.measurement.descriptor.semanticId)
      )
      Right(lazyFrame(source, declaration)(ordered.iterator))
