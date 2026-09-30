package scalafim.transform

import java.security.MessageDigest

import scalafim.transform.oracle.OracleFixtures

/** P5.03 CI check: writers still produce the committed goldens, and the committed receipt (native tools reading those
  * goldens) belongs to exactly these bytes. Changing a writer fails here until tools/transform/writer-acceptance.sh is
  * re-run and its receipt committed.
  */
class WriterGoldensSuite extends munit.FunSuite:
  private val dir = WriterGoldens.Directory

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString

  private lazy val receipt = OracleFixtures.text(s"$dir/receipt.json")

  private def recordedHash(name: String): Option[String] =
    s""""${java.util.regex.Pattern.quote(name)}":\\s*"([0-9a-f]{64})"""".r.findFirstMatchIn(receipt).map(_.group(1))

  test("every writer reproduces its committed golden byte for byte"):
    val listed = OracleFixtures.text(s"$dir/goldens.txt").linesIterator.filter(_.nonEmpty).toVector
    val rendered = WriterGoldens.render()
    assertEquals(rendered.map(_._1), listed)
    rendered.foreach: (name, bytes) =>
      assert(OracleFixtures.bytes(s"$dir/$name").sameElements(bytes), s"$name drifted from its golden; re-run tools/transform/writer-acceptance.sh")

  test("the receipt was produced from exactly the committed goldens"):
    val listed = OracleFixtures.text(s"$dir/goldens.txt").linesIterator.filter(_.nonEmpty).toVector
    (listed ++ Vector("queries.tsv", "pullback.json", "goldens.txt")).foreach: name =>
      assertEquals(recordedHash(name), Some(sha256(OracleFixtures.bytes(s"$dir/$name"))), s"receipt is stale for $name")

  test("no native check in the receipt failed"):
    assert(!receipt.contains("\"status\": \"fail\""), receipt)
    assert(receipt.contains("\"status\": \"pass\""), "the receipt records no passing native check")
