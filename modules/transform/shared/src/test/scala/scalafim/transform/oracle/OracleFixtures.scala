package scalafim.transform.oracle

/** Native-tool oracle fixtures, readable byte-for-byte on both platforms.
  *
  * Fixtures live under `modules/transform/shared/src/test/resources/scalafim/transform/oracle/`. The JVM reads them
  * from the test classpath; Scala.js reads the same files from disk through Node, so every convention and numerical
  * parity test in shared code runs against identical bytes on both platforms. Paths are relative to that root.
  */
object OracleFixtures:
  val Root = "scalafim/transform/oracle"

  /** The file's bytes exactly as stored. */
  def bytes(path: String): Array[Byte] =
    OracleFixturePlatform.read(s"$Root/$path")

  /** Stored bytes, gunzipped when the path ends in `.gz`. */
  def decoded(path: String): Array[Byte] =
    val raw = bytes(path)
    if path.endsWith(".gz") then OracleFixturePlatform.gunzip(raw) else raw

  def text(path: String): String =
    String(decoded(path), "UTF-8")

  def sha256Hex(path: String): String =
    Sha256.hex(bytes(path))
