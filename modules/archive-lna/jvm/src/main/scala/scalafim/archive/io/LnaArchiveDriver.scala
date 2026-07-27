package scalafim.archive.io

import cats.data.EitherT
import cats.effect.{Async, Resource}
import scalafim.archive.*
import scalafim.archive.lna.*

import java.nio.file.Path
import scala.util.control.NonFatal

final class LnaArchiveDriver[F[_]] private (
    store: LnaHdf5Store
)(using F: Async[F]) extends ArchiveDriver[F]:

  def open(location: ArchiveLocation): ArchiveResource[F, OpenArchive[F]] =
    Resource.eval(EitherT(F.blocking:
      LnaArchiveDriver.path(location).flatMap: path =>
        store.read(path).flatMap: archive =>
          LnaArchiveDriver.opened[F](archive, location)
    ))

object LnaArchiveDriver:
  def default[F[_]: Async]: LnaArchiveDriver[F] =
    new LnaArchiveDriver[F](LnaHdf5Store.default)

  def using[F[_]: Async](store: LnaHdf5Store): LnaArchiveDriver[F] =
    new LnaArchiveDriver[F](store)

  private[io] def path(location: ArchiveLocation): Either[ArchiveError, Path] =
    try Right(Path.of(location.value))
    catch
      case NonFatal(error) =>
        Left(ArchiveError.InvalidPath(location.value, error.getMessage))

  private[io] def opened[F[_]: Async](
      archive: LnaArchive,
      location: ArchiveLocation
  ): Either[ArchiveError, OpenArchive[F]] =
    for
      revision <- revision(archive, location)
    yield new EagerLnaOpenArchive[F](archive, revision)

  private def revision(
      archive: LnaArchive,
      location: ArchiveLocation
  ): Either[ArchiveError, ArchiveRevision] =
    for
      manifest <- LegacyLnaManifestTranslator.translate(archive.manifest)
      revisionId <- ArchiveRevisionId.fromString:
        archive.manifest.checksum.fold(
          s"lna-unverified-${location.value.hashCode.toHexString}"
        )(checksum => s"lna-$checksum")
      publication <- archive.manifest.checksum match
        case Some(checksum) => ContentDigest.sha256(checksum).map(PublicationStatus.Published.apply)
        case None => Right(PublicationStatus.Staging)
      creator <- CreatorId(archive.manifest.creator)
      result <- ArchiveRevision.from(
        revisionId,
        manifest,
        publication,
        ArchiveProvenance(creator)
      )
    yield result

private final class EagerLnaOpenArchive[F[_]](
    archive: LnaArchive,
    val revision: ArchiveRevision
)(using F: Async[F]) extends OpenArchive[F]:

  val structure: ArchiveValidation =
    ArchiveValidation(
      ArchiveValidationScope.Structure,
      revision.manifest.payloads.map(_.id),
      Vector.empty
    )

  val payloads: PayloadExecutor[F] =
    new PayloadExecutor[F]:
      def execute[A](
          plan: PayloadPlan[A]
      ): EitherT[F, ArchiveError, Observed[A]] =
        plan match
          case concrete: LnaDoubleMatrixPlan =>
            executePayload(
              concrete.path,
              concrete.summary,
              {
                case value: Payload.DoubleMatrix => Right(value)
                case other => Left(ArchiveError.ShapeMismatch(
                  s"${concrete.path.value} is ${other.getClass.getSimpleName}, expected DoubleMatrix"
                ))
              }
            )
          case concrete: LnaDoubleVectorPlan =>
            executePayload(
              concrete.path,
              concrete.summary,
              {
                case value: Payload.DoubleVector => Right(value)
                case other => Left(ArchiveError.ShapeMismatch(
                  s"${concrete.path.value} is ${other.getClass.getSimpleName}, expected DoubleVector"
                ))
              }
            )
          case concrete: LnaIntMatrixPlan =>
            executePayload(
              concrete.path,
              concrete.summary,
              {
                case value: Payload.IntMatrix => Right(value)
                case other => Left(ArchiveError.ShapeMismatch(
                  s"${concrete.path.value} is ${other.getClass.getSimpleName}, expected IntMatrix"
                ))
              }
            )
          case other =>
            EitherT.leftT(ArchiveError.UnsupportedPayloadPlan(
              "lna-hdf5",
              other.summary.operation
            ))

      private def executePayload[A](
          path: ArchivePath,
          summary: PayloadPlanSummary,
          refine: Payload => Either[ArchiveError, A]
      ): EitherT[F, ArchiveError, Observed[A]] =
        EitherT.fromEither:
          for
            payload <- archive.payload(path).toRight(ArchiveError.MissingPayload(path))
            value <- refine(payload)
            observation <- ReadObservation.wholePayload(summary.payload, payloadBytes(payload))
            receipt <- ArchiveReadReceipt.from(
              summary,
              PhysicalLocality.WholePayload,
              ReceiptAccumulator.Empty.append(observation)
            )
          yield Observed(value, receipt)

  val validateContents: EitherT[F, ArchiveError, ArchiveValidation] =
    EitherT.fromEither:
      archive.validate.map: _ =>
        ArchiveValidation(
          ArchiveValidationScope.Contents,
          revision.manifest.payloads.map(_.id),
          revision.manifest.integrity.entries.map(_.payload)
        )

  private def payloadBytes(payload: Payload): Long =
    payload.dims.foldLeft(payload.dtype.bytes.toLong): (total, dimension) =>
      Math.multiplyExact(total, dimension.toLong)
