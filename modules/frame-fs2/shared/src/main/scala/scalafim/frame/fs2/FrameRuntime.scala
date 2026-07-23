package scalafim.frame.fs2

import cats.effect.Async
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.kernel.Outcome
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.Stream
import scala.NamedTuple
import scalafim.frame.*

final case class ExecutionFailure(error: ExecutionError)
    extends RuntimeException(error.message)

final class FrameRuntime[F[_]](
    sources: ReferenceSources
)(using F: Async[F]):
  private def failure[A](result: Either[ExecutionError, A]): F[A] =
    F.fromEither(result.left.map(ExecutionFailure.apply))

  private def batches(cursor: ExecutionCursor): Stream[F, RecordBatch] =
    Stream.eval(F.delay(cursor.nextBatch()).flatMap(failure)).flatMap:
      case None => Stream.empty
      case Some(batch) =>
        Stream
          .bracket(F.pure(batch))(value => F.delay(value.close()))
          .flatMap(Stream.emit) ++ batches(cursor)

  def stream[S <: NamedTuple.AnyNamedTuple](frame: Frame[S]): Stream[F, RecordBatch] =
    val execution = ReferenceInterpreter.prepare(frame.plan, sources)
    Stream
      .bracket(failure(execution.open()))(cursor => F.delay(cursor.close()))
      .flatMap(batches)

  def collect[S <: NamedTuple.AnyNamedTuple](frame: Frame[S])(using
      descriptor: SchemaDescriptor[S]
  ): Resource[F, Table[S]] =
    Resource.makeFull[F, Table[S]](
      poll =>
        Ref.of[F, Vector[RecordBatch]](Vector.empty).flatMap: retained =>
          def closeRetained: F[Unit] =
            retained.get.flatMap(_.traverse_(batch => F.delay(batch.close())))

          val copyBatches = stream(frame).evalMap: batch =>
            failure(batch.slice(0, batch.rowCount).left.map(ExecutionError.Storage.apply))
              .flatTap(copy => retained.update(_ :+ copy))
          poll(copyBatches.compile.drain)
            .guaranteeCase:
              case Outcome.Succeeded(_) => F.unit
              case _ => closeRetained
            .flatMap: _ =>
              retained.get.flatMap: batches =>
                Table[S](batches) match
                  case Right(table) => F.pure(table)
                  case Left(error) =>
                    closeRetained *> F.raiseError[Table[S]](
                      ExecutionFailure(ExecutionError.Storage(error))
                    )
    )(table => F.delay(table.close()))

  def physicalExplain[S <: NamedTuple.AnyNamedTuple](frame: Frame[S]): String =
    ReferenceInterpreter.prepare(frame.plan, sources).physicalExplain

object FrameRuntime:
  def apply[F[_]: Async](sources: ReferenceSources): FrameRuntime[F] =
    new FrameRuntime(sources)
