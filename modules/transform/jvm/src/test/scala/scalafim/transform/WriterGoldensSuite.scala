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

  private def recordedHash(name: String, text: String = receipt): Option[String] =
    s""""${java.util.regex.Pattern.quote(name)}":\\s*"([0-9a-f]{64})"""".r.findFirstMatchIn(text).map(_.group(1))

  private lazy val listed = OracleFixtures.text(s"$dir/goldens.txt").linesIterator.filter(_.nonEmpty).toVector
  private lazy val goldenHashes = (listed ++ Vector("queries.tsv", "pullback.json", "goldens.txt"))
    .map(name => name -> sha256(OracleFixtures.bytes(s"$dir/$name"))).toMap
  private lazy val inputHashes = Vector("movable.nii", "reference.nii", "manifest.json")
    .map(name => s"writer_geometry/$name" -> sha256(OracleFixtures.bytes(s"writer_geometry/$name"))).toMap +
    ("pullback.json" -> goldenHashes("pullback.json"))

  /** Receipts are emitted in canonical indent=1 JSON. Bound only this section,
    * so reference-library passes cannot stand in for missing native records.
    */
  private def nativeRecords(text: String): Vector[(String, String)] =
    val start = text.indexOf("\"native_checks\": {")
    val end = text.indexOf("\"native_harness\":", start)
    if start < 0 || end < 0 then Vector.empty
    else "(?ms)^  \"([^\"]+)\": \\{(.*?)^  \\},?$".r
      .findAllMatchIn(text.substring(start, end)).map(m => m.group(1) -> m.group(2)).toVector

  private def staleBindings(text: String): Vector[String] =
    val top = goldenHashes.toVector.collect:
      case (name, hash) if !recordedHash(name, text).contains(hash) => name
    val records = nativeRecords(text)
    val missing = if records.map(_._1).toSet != listed.toSet then Vector("native record set") else Vector.empty
    val native = records.flatMap: (name, body) =>
      if body.contains("\"status\": \"pass\"") then
        val wrongGolden = !recordedHash("golden_sha256", body).contains(goldenHashes.getOrElse(name, "unlisted"))
        val wrongInputs = inputHashes.exists((key, hash) => !recordedHash(key, body).contains(hash))
        val missingImage = !"\"image_id\":\\s*\"sha256:[0-9a-f]{64}\"".r.findFirstIn(body).isDefined
        if wrongGolden || wrongInputs || missingImage || !body.contains("\"output_hashes\": {") then Vector(s"native:$name")
        else Vector.empty
      else Vector.empty
    top ++ missing ++ native

  test("every writer reproduces its committed golden byte for byte"):
    val listed = OracleFixtures.text(s"$dir/goldens.txt").linesIterator.filter(_.nonEmpty).toVector
    val rendered = WriterGoldens.render()
    assertEquals(rendered.map(_._1), listed)
    rendered.foreach: (name, bytes) =>
      assert(OracleFixtures.bytes(s"$dir/$name").sameElements(bytes), s"$name drifted from its golden; re-run tools/transform/writer-acceptance.sh")

  test("the receipt was produced from exactly the committed goldens"):
    assertEquals(staleBindings(receipt), Vector.empty, "receipt does not bind current golden and geometry bytes")
    inputHashes.foreach: (name, digest) =>
      assertEquals(recordedHash(name), Some(digest), s"receipt is stale for $name")

  test("a planted stale native golden or geometry receipt is rejected even when top-level golden hashes remain current"):
    val name = "affine.tfm"
    val staleGolden = receipt.replace(s"\"golden_sha256\": \"${goldenHashes(name)}\"", s"\"golden_sha256\": \"${"0" * 64}\"")
    assert(staleGolden != receipt, "expected a passing native ANTs writer record")
    assert(staleBindings(staleGolden).contains(s"native:$name"))
    val geometry = "writer_geometry/movable.nii"
    val staleGeometry = receipt.replace(inputHashes(geometry), "0" * 64)
    assert(staleBindings(staleGeometry).exists(_.startsWith("native:")))

  test("no native check in the receipt failed"):
    assert(!receipt.contains("\"status\": \"fail\""), receipt)
    val required = Set("affine.tfm", "affine.mat", "field_1Warp.nii", "flirt.mat", "field_fnirt_relative.nii", "affine.aff12.1D")
    val passed = nativeRecords(receipt).collect:
      case (name, body) if body.contains("\"status\": \"pass\"") => name
    assert(required.subsetOf(passed.toSet), s"missing required native numerical passes: ${required -- passed.toSet}")
