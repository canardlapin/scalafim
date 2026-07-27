package scalafim.interop.archive.lna

import cats.effect.Async
import scalafim.archive.io.LnaArchiveDriver
import scalafim.interop.archive.{
  ArchiveDriverId,
  ArchiveDriverRegistration
}

object LnaArchiveDriverRegistration:
  val Id: ArchiveDriverId =
    ArchiveDriverId.unsafe("lna-hdf5")

  def default[F[_]: Async]: ArchiveDriverRegistration[F] =
    using(LnaArchiveDriver.default[F])

  def using[F[_]](
      driver: LnaArchiveDriver[F]
  ): ArchiveDriverRegistration[F] =
    ArchiveDriverRegistration.make(
      Id,
      driver,
      location => location.value.endsWith(".lna.h5")
    )
