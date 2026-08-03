package scalafim.image

import cats.effect.{Deferred, IO, Ref, Resource}
import cats.effect.unsafe.implicits.global

import scala.concurrent.Future

class CatsEffectSharedBoundarySuite extends munit.FunSuite:
  test("shared Resource releases after success and failure"):
    run:
      for
        events <- Ref.of[IO, Vector[String]](Vector.empty)
        resource =
          Resource.make(events.update(_ :+ "acquire").as("opened")): _ =>
            events.update(_ :+ "release")
        _ <- resource.use(_ => events.update(_ :+ "success"))
        failed <- resource
          .use(_ => events.update(_ :+ "failure") *> IO.raiseError[Unit](ExpectedFailure))
          .attempt
        observed <- events.get
      yield
        assert(failed.isLeft)
        assertEquals(
          observed,
          Vector("acquire", "success", "release", "acquire", "failure", "release")
        )

  test("shared Resource releases when its fiber is cancelled"):
    run:
      for
        events <- Ref.of[IO, Vector[String]](Vector.empty)
        acquired <- Deferred[IO, Unit]
        resource =
          Resource.make(
            events.update(_ :+ "acquire") *> acquired.complete(()).as("opened")
          ): _ =>
            events.update(_ :+ "release")
        fiber <- resource.use(_ => IO.never).start
        _ <- acquired.get
        _ <- fiber.cancel
        observed <- events.get
      yield assertEquals(observed, Vector("acquire", "release"))

  private def run[A](program: IO[A]): Future[A] =
    program.unsafeToFuture()

  private object ExpectedFailure extends RuntimeException("expected phase-zero failure")
