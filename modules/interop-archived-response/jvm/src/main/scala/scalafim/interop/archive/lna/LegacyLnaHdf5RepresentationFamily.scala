package scalafim.interop.archive.lna

import cats.data.EitherT
import cats.effect.Async
import scalafim.archive.{
  ArchiveError,
  ArchiveLocation,
  ArchiveRevisionId
}
import scalafim.archive.io.LnaHdf5Store
import scalafim.response.ResponseSource

import java.nio.file.Path
import scala.util.control.NonFatal

object LegacyLnaHdf5RepresentationFamily:
  def default[F[_]: Async]: LegacyLnaRepresentationFamily[F] =
    using(LnaHdf5Store.default)

  def using[F[_]: Async](
      store: LnaHdf5Store
  ): LegacyLnaRepresentationFamily[F] =
    LegacyLnaRepresentationFamily.using(
      new LegacyLnaResponseOpener[F]:
        def open(
            location: ArchiveLocation,
            revisionId: ArchiveRevisionId
        ): EitherT[F, ArchiveError, ResponseSource[F]] =
          EitherT(summon[Async[F]].blocking:
            try
              store
                .read(Path.of(location.value))
                .flatMap(archive =>
                  LegacyLnaRepresentationFamily.source[F](
                    archive,
                    revisionId
                  )
                )
            catch
              case NonFatal(error) =>
                Left(ArchiveError.InvalidPath(
                  location.value,
                  error.getMessage
                ))
          )
    )
