package scalafim.bids

import cats.Applicative

final case class BidsStoreEntry(path: BidsPath, size: Option[Long])

trait BidsStore[F[_]]:
  def entries: F[Either[BidsError, Vector[BidsStoreEntry]]]
  def readUtf8(path: BidsPath): F[Either[BidsError, String]]

final class InMemoryBidsStore[F[_]] private (
    files: Map[BidsPath, String],
    readFailures: Map[BidsPath, BidsError],
    listingFailure: Option[BidsError]
)(using F: Applicative[F]) extends BidsStore[F]:
  def entries: F[Either[BidsError, Vector[BidsStoreEntry]]] =
    F.pure(
      listingFailure.toLeft(
        files.keys.toVector.sortBy(_.value).map(path => BidsStoreEntry(path, None))
      )
    )

  def readUtf8(path: BidsPath): F[Either[BidsError, String]] =
    F.pure(
      readFailures
        .get(path)
        .toLeft(files.get(path))
        .flatMap(_.toRight(BidsError.Io(path.value, "store entry does not exist")))
    )

object InMemoryBidsStore:
  def fromText[F[_]: Applicative](
      files: IterableOnce[(String, String)],
      readFailures: Map[String, BidsError] = Map.empty,
      listingFailure: Option[BidsError] = None
  ): Either[BidsError, InMemoryBidsStore[F]] =
    for
      parsedFiles <- BidsEither.traverse(files.iterator.toVector) { case (path, text) =>
        BidsPath.relative(path).map(_ -> text)
      }
      parsedFailures <- BidsEither.traverse(readFailures.toVector) { case (path, error) =>
        BidsPath.relative(path).map(_ -> error)
      }
    yield new InMemoryBidsStore(parsedFiles.toMap, parsedFailures.toMap, listingFailure)
