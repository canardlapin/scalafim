package scalafim.archive.io

import scalafim.archive.ArchiveError
import scalafim.archive.lna.LnaArchive

import java.nio.file.Path

trait LnaHdf5Store:
  def read(path: Path): Either[ArchiveError, LnaArchive]
  def write(path: Path, archive: LnaArchive): Either[ArchiveError, Unit]

object LnaHdf5Store:
  def default: LnaHdf5Store = JhdfLnaHdf5Store

  def jhdf: LnaHdf5Store = JhdfLnaHdf5Store

  def unavailable(detail: String = "no HDF5 implementation is wired yet"): LnaHdf5Store =
    new LnaHdf5Store:
      def read(path: Path): Either[ArchiveError, LnaArchive] =
        Left(ArchiveError.UnsupportedStorage(detail))

      def write(path: Path, archive: LnaArchive): Either[ArchiveError, Unit] =
        Left(ArchiveError.UnsupportedStorage(detail))
