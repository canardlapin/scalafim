package scalafim.surface

/** The single call site of the portable SHA-256 used to bind declared assets
  * to their exact bytes on the JVM and Scala.js. Keeping it here lets the
  * implementation move to a smaller shared library without touching callers.
  */
private[surface] object SurfaceDigest:
  /** Lowercase hexadecimal SHA-256 of `bytes`. */
  def sha256Hex(bytes: Array[Byte]): String =
    zarr4s.PortableSha256.digest(bytes).value
