package scalafim.surface.reference

import scalafim.image.io.Nifti
import scalafim.surface.SurfaceError

import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

/** Loads a NIfTI source volume under a frame declaration. The file is read
  * once; its bytes are checked against the declared digest and then decoded
  * from a private copy of exactly those bytes, so a file replaced between
  * hashing and decoding cannot be admitted.
  */
object DeclaredVolumeReader:
  def readNifti(path: Path, declaration: FrameDeclaration): Either[ReferenceError, DeclaredVolume] =
    val name = Option(path.getFileName).map(_.toString).getOrElse("")
    val suffix = if name.endsWith(".nii.gz") then ".nii.gz" else ".nii"
    for
      bytes <- attempt(path)(Files.readAllBytes(path))
      _ <- declaration.checkDigest(bytes)
      volume <- attempt(path) {
        val copy = Files.createTempFile("scalafim-declared-", suffix)
        try
          Files.write(copy, bytes)
          Nifti.readVolume(copy).map(_.image)
        finally Files.deleteIfExists(copy)
      }.flatMap(_.left.map(error => ReferenceError.AssetReadFailure(s"$path: ${error.message}")))
      declared <- DeclaredVolume.verified(declaration, bytes, volume)
    yield declared

  private def attempt[A](path: Path)(body: => A): Either[ReferenceError, A] =
    try Right(body)
    catch case NonFatal(error) => Left(ReferenceError.AssetReadFailure(s"$path: ${SurfaceError.reason(error)}"))
