package scalafim.archive.zarr

import scalafim.zarr.OwnedBytes
import scalafim.zarr.PortableSha256
import scalafim.zarr.Sha256Hash.*

/** Domain refinement over the portable hash implementation owned by the Zarr core. */
object Sha256:
  def digest(bytes: OwnedBytes): Sha256Digest =
    Sha256Digest.unsafe(PortableSha256.digest(bytes).value)

  def digestUtf8(value: String): Sha256Digest =
    Sha256Digest.unsafe(PortableSha256.digestUtf8(value).value)

  def digest(input: Array[Byte]): Sha256Digest =
    Sha256Digest.unsafe(PortableSha256.digest(input).value)
