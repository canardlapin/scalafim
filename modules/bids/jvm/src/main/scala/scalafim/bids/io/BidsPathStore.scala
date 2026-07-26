package scalafim.bids.io

import scalafim.bids.*

import cats.Applicative
import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.effect.syntax.all.*
import cats.syntax.all.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

private[io] trait BidsReadObserver[F[_]]:
  def beforeRead(path: Path): F[Unit]
  def afterRead(path: Path): F[Unit]

private[io] object BidsReadObserver:
  def noop[F[_]: Applicative]: BidsReadObserver[F] =
    new BidsReadObserver[F]:
      def beforeRead(path: Path): F[Unit] = Applicative[F].unit
      def afterRead(path: Path): F[Unit] = Applicative[F].unit

private[io] final class BidsPathRuntime[F[_]](using F: Async[F]):
  def blocking[A](thunk: => A): F[A] =
    F.blocking(thunk)

  def useAutoCloseable[A <: AutoCloseable, B](acquire: F[A])(use: A => F[B]): F[B] =
    Resource.fromAutoCloseable(acquire).use(use)

private[io] object BidsPathRuntime:
  def apply[F[_]: Async]: BidsPathRuntime[F] =
    new BidsPathRuntime[F]

final class BidsPathStore[F[_]] private[io] (
    root: Path,
    observer: BidsReadObserver[F],
    runtime: BidsPathRuntime[F]
)(using F: Async[F]) extends BidsStore[F]:
  private val rootAbs = root.toAbsolutePath.normalize()

  def entries: F[Either[BidsError, Vector[BidsStoreEntry]]] =
    runtime.blocking(Files.isDirectory(rootAbs)).attempt.flatMap {
      case Left(error) =>
        F.pure(Left(ioError(rootAbs, "inspect store root", error)))
      case Right(false) =>
        F.pure(Left(BidsError.Io(rootAbs.toString, "directory does not exist")))
      case Right(true) =>
        runtime
          .useAutoCloseable(runtime.blocking(Files.walk(rootAbs))) { stream =>
            runtime.blocking(
              stream
                .iterator()
                .asScala
                .filter(path => Files.isRegularFile(path))
                .map { path =>
                  val relative = rootAbs.relativize(path).toString.replace('\\', '/')
                  BidsPath.relative(relative).map(bidsPath => BidsStoreEntry(bidsPath, Some(Files.size(path))))
                }
                .toVector
                .sortBy(_.fold(_.message, _.path.value))
            )
          }
          .attempt
          .map(
            _.leftMap(error => ioError(rootAbs, "walk dataset", error))
              .flatMap(_.sequence)
          )
    }

  def readUtf8(path: BidsPath): F[Either[BidsError, String]] =
    BidsPath.relative(path.value) match
      case Left(error) => F.pure(Left(error))
      case Right(relative) =>
        val resolved = rootAbs.resolve(relative.value).normalize()
        if !resolved.startsWith(rootAbs) then
          F.pure(Left(BidsError.InvalidPath(path.value, s"path escapes store root '$rootAbs'")))
        else
          (observer.beforeRead(resolved) *>
            runtime.blocking(Files.readString(resolved, StandardCharsets.UTF_8))
              .attempt
              .map(_.leftMap(error => ioError(resolved, "read UTF-8 file", error))))
            .guarantee(observer.afterRead(resolved))

  private def ioError(path: Path, operation: String, error: Throwable): BidsError =
    val detail = Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
    BidsError.Io(path.toString, s"$operation failed: $detail")

object BidsPathStore:
  def apply[F[_]: Async](root: Path): BidsPathStore[F] =
    new BidsPathStore(root, BidsReadObserver.noop[F], BidsPathRuntime[F])

  private[io] def observed[F[_]: Async](root: Path, observer: BidsReadObserver[F]): BidsPathStore[F] =
    new BidsPathStore(root, observer, BidsPathRuntime[F])
