package scalafim.group.research.bootstrap

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

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
