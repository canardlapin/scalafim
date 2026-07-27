package scalafim.interop.archive.lna

import cats.data.EitherT
import cats.effect.Async
import scalafim.archive.{
  ArchiveError,
  ArchiveLocation,
  ArchiveResource,
  ContentDigest,
  PublicationStatus
}
import scalafim.archive.io.{LnaArchiveDriver, LnaHdf5Store}
import scalafim.interop.archive.{
  ArchiveResponseAccess,
  RepresentationEnvelope
}
import scalafim.response.SourceId

import java.nio.file.{Files, Path}

enum TemporalDctLnaWriteVisibility:
  case Absent
  case Published(digest: ContentDigest)

final case class TemporalDctLnaWriteReceipt(
    completed: Vector[TemporalDctLnaWriteStep],
    visibility: TemporalDctLnaWriteVisibility,
    location: ArchiveLocation
)

enum TemporalDctLnaWriteError:
  case Archive(error: ArchiveError)
  case DestinationExists(location: ArchiveLocation)
  case InvalidInterruption(completedSteps: Int, totalSteps: Int)
  case Interrupted(receipt: TemporalDctLnaWriteReceipt)
  case PublicationSafety(location: ArchiveLocation)

  def message: String =
    this match
      case Archive(error) =>
        error.message
      case DestinationExists(location) =>
        s"immutable LNA destination '${location.value}' already exists"
      case InvalidInterruption(completed, total) =>
        s"interruption prefix $completed must be in [0, $total)"
      case Interrupted(receipt) =>
        s"LNA write interrupted after ${receipt.completed.length} steps"
      case PublicationSafety(location) =>
        s"interrupted LNA write exposed '${location.value}' as a destination"

final class TemporalDctLnaHdf5Writer[F[_]] private (
    store: LnaHdf5Store
)(using F: Async[F]):
  def execute(
      location: ArchiveLocation,
      plan: TemporalDctLnaWritePlan
  ): EitherT[F, TemporalDctLnaWriteError, TemporalDctLnaWriteReceipt] =
    executeInternal(location, plan, None)

  def executeInterruptedAfter(
      location: ArchiveLocation,
      plan: TemporalDctLnaWritePlan,
      completedSteps: Int
  ): EitherT[F, TemporalDctLnaWriteError, TemporalDctLnaWriteReceipt] =
    executeInternal(location, plan, Some(completedSteps))

  private def executeInternal(
      location: ArchiveLocation,
      plan: TemporalDctLnaWritePlan,
      interruption: Option[Int]
  ): EitherT[F, TemporalDctLnaWriteError, TemporalDctLnaWriteReceipt] =
    EitherT(F.blocking:
      val path = Path.of(location.value)
      if Files.exists(path) then
        Left(TemporalDctLnaWriteError.DestinationExists(location))
      else
        interruption match
          case Some(completed)
              if completed < 0 || completed >= plan.steps.length =>
            Left(TemporalDctLnaWriteError.InvalidInterruption(
              completed,
              plan.steps.length
            ))
          case Some(completed) =>
            val receipt =
              TemporalDctLnaWriteReceipt(
                plan.steps.take(completed),
                TemporalDctLnaWriteVisibility.Absent,
                location
              )
            if Files.exists(path) then
              Left(TemporalDctLnaWriteError.PublicationSafety(location))
            else Left(TemporalDctLnaWriteError.Interrupted(receipt))
          case None =>
            store
              .write(path, plan.archive)
              .left
              .map(TemporalDctLnaWriteError.Archive.apply)
              .flatMap: _ =>
                store
                  .read(path)
                  .left
                  .map(TemporalDctLnaWriteError.Archive.apply)
                  .flatMap: stored =>
                    stored.manifest.checksum match
                      case Some(value) =>
                        ContentDigest
                          .sha256(value)
                          .left
                          .map(TemporalDctLnaWriteError.Archive.apply)
                          .map: digest =>
                            TemporalDctLnaWriteReceipt(
                              plan.steps,
                              TemporalDctLnaWriteVisibility.Published(digest),
                              location
                            )
                      case None =>
                        Left(TemporalDctLnaWriteError.Archive(
                          ArchiveError.InvalidArchive(
                            "published LNA archive has no root checksum"
                          )
                        ))
    )

object TemporalDctLnaHdf5Writer:
  def default[F[_]: Async]: TemporalDctLnaHdf5Writer[F] =
    new TemporalDctLnaHdf5Writer[F](LnaHdf5Store.default)

  def using[F[_]: Async](
      store: LnaHdf5Store
  ): TemporalDctLnaHdf5Writer[F] =
    new TemporalDctLnaHdf5Writer[F](store)

object TemporalDctLnaHdf5:
  def open[F[_]: Async](
      location: ArchiveLocation,
      sourceId: SourceId,
      model: scalafim.latent.TemporalDctRepresentation
  ): ArchiveResource[F, ArchivedTemporalDctSource[F]] =
    openUsing(
      location,
      sourceId,
      model,
      LnaArchiveDriver.default[F]
    )

  def openUsing[F[_]: Async](
      location: ArchiveLocation,
      sourceId: SourceId,
      model: scalafim.latent.TemporalDctRepresentation,
      driver: LnaArchiveDriver[F]
  ): ArchiveResource[F, ArchivedTemporalDctSource[F]] =
    driver
      .open(location)
      .evalMap: opened =>
        EitherT.fromEither:
          for
            digest <- opened.revision.publication match
              case PublicationStatus.Published(value) =>
                Right(value)
              case other =>
                Left(ArchiveError.IncompletePublication(
                  s"temporal DCT response requires a published revision, found $other"
                ))
            envelope <- RepresentationEnvelope.fromRevision(opened.revision)
            storedModel <- TemporalDctLnaDescriptor.decode(envelope)
            _ <-
              if TemporalDctLnaModel.fingerprint(storedModel) ==
                  TemporalDctLnaModel.fingerprint(model)
              then Right(())
              else Left(ArchiveError.InvalidArchive(
                "requested temporal DCT model does not match the persisted descriptor"
              ))
            layout <- TemporalDctLnaLayout.fromEnvelope(model, envelope)
            access = new ArchiveResponseAccess(
              location,
              opened.revision.id,
              digest,
              opened.payloads,
              opened.validateContents
            )
            source <- ArchivedTemporalDctSource.make(
              sourceId,
              model,
              access,
              layout
            )
          yield source
