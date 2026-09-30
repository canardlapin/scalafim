package scalafim.group.research.bootstrap

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

/** The committed manifest must be byte-identical to the canonical serialization. */
class ManifestFileSuite extends munit.FunSuite:
  private val tools = Paths.get(sys.props.getOrElse("user.dir", ".")).resolve("tools/group-bootstrap-research")

  test("tools/group-bootstrap-research/cells.json is the canonical manifest and its recorded SHA-256 matches"):
    val committed = new String(Files.readAllBytes(tools.resolve("cells.json")), StandardCharsets.UTF_8)
    assertEquals(committed, CellManifest.canonical)
    val recorded = new String(Files.readAllBytes(tools.resolve("cells.json.sha256")), StandardCharsets.UTF_8).trim.split("\\s+").head
    assertEquals(recorded, CellManifest.sha256)
    val digest = java.security.MessageDigest.getInstance("SHA-256").digest(committed.getBytes(StandardCharsets.UTF_8))
    assertEquals(digest.map(b => f"${b & 0xff}%02x").mkString, CellManifest.sha256, "portable SHA-256 agrees with the JDK")

  test("the committed R reference inputs are what the writer produces now"):
    val committed = new String(Files.readAllBytes(tools.resolve("inputs/bootstrap-cases.json")), StandardCharsets.UTF_8)
    assertEquals(Sha256.hex(committed), Sha256.hex(ReferenceInputWriter.casesJson))
    assertEquals(Sha256.hex(committed), BootstrapReferenceFixtures.InputSha256)

  test("manifest-v2.json: parent is the unchanged cells.json and every harness source hash is current"):
    val text = new String(Files.readAllBytes(tools.resolve("manifest-v2.json")), StandardCharsets.UTF_8)
    val recorded = new String(Files.readAllBytes(tools.resolve("manifest-v2.json.sha256")), StandardCharsets.UTF_8).trim.split("\\s+").head
    assertEquals(Sha256.hex(text), recorded, "manifest-v2.json.sha256 is stale")
    assert(text.contains(s"\"parent\":{\"path\":\"tools/group-bootstrap-research/cells.json\",\"sha256\":\"${CellManifest.sha256}\"}"))
    assertEquals(CellManifest.sha256, "76e6785bf77a6971d542a7de0601d6b0ca9b3c12d791a2a5b26756154f03bc73")
    assert(text.contains("\"version\":\"selection-rule/v1\"") && text.contains(s"\"version\":\"decision-rule/v2\""))
    assertEquals(SelectionRule.Version, "selection-rule/v1")
    assert(text.contains("\"accounting\":\"CountAsRejection\"") && text.contains("\"ties\":\"LowestIdString\"") && text.contains("\"count\":6"))
    assertEquals(SelectionRule.Owner.accounting, PilotFailureAccounting.CountAsRejection)
    assertEquals(SelectionRule.Owner.ties, TieOrder.LowestIdString)
    assertEquals(SelectionRule.Owner.count, 6)
    val entry = "\"((?:modules|tools)/[^\"]+)\":\"([0-9a-f]{64})\"".r
    val listed = entry.findAllMatchIn(text).map(m => m.group(1) -> m.group(2)).toMap
    val root = Paths.get(sys.props.getOrElse("user.dir", "."))
    val onDisk = Vector(
      "modules/group/shared/src/test/scala/scalafim/group/research/bootstrap",
      "modules/group/jvm/src/test/scala/scalafim/group/research/bootstrap"
    ).flatMap { d =>
      val stream = Files.list(root.resolve(d))
      try stream.iterator().asScala.map(p => s"$d/${p.getFileName}").filter(_.endsWith(".scala")).toVector
      finally stream.close()
    } :+ "tools/group-bootstrap-research/generate_reference_fixtures.R"
    assertEquals(listed.keySet, onDisk.toSet, "manifest-v2 must list exactly the harness sources")
    onDisk.foreach { path =>
      val digest = java.security.MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(root.resolve(path)))
      assertEquals(digest.map(b => f"${b & 0xff}%02x").mkString, listed(path), s"$path changed without rewriting manifest-v2.json")
    }
