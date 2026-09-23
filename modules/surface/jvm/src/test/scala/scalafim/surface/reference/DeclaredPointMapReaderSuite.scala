package scalafim.surface.reference

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** The JVM reader on the committed synthetic canonical directory. */
class DeclaredPointMapReaderSuite extends munit.FunSuite:
  private val resource = Path.of(getClass.getResource("/pointmap-synthetic/manifest.json").toURI).getParent

  private def copyWith(manifest: String => String, stage: Array[Byte] => Array[Byte] = identity)(f: Path => Unit): Unit =
    val dir = Files.createTempDirectory("scalafim-point-map-")
    try
      Files.writeString(dir.resolve("manifest.json"),
        manifest(Files.readString(resource.resolve("manifest.json"), StandardCharsets.UTF_8)))
      Files.write(dir.resolve("stage-0-displacement.nii"), stage(Files.readAllBytes(resource.resolve("stage-0-displacement.nii"))))
      f(dir)
    finally
      Files.deleteIfExists(dir.resolve("manifest.json"))
      Files.deleteIfExists(dir.resolve("stage-0-displacement.nii"))
      Files.deleteIfExists(dir)

  test("the canonical synthetic directory reads, verifies and agrees with the SimpleITK oracle to 1e-9 mm"):
    val declared = DeclaredPointMapReader.read(resource, SyntheticItkOracle.sourceSha256).fold(e => fail(e.message), d => d)
    val oracle = ujson.read(Files.readString(resource.resolve("synthetic_composite_oracle.json")))
    val points = oracle("points").arr.map(_.arr.map(_.num).toVector).toVector
    val mapped = oracle("mapped").arr.map(_.arr.map(_.num).toVector).toVector
    assertEquals(points, SyntheticItkOracle.points, "generated shared fixture matches the committed oracle")
    val out = new Array[Double](3)
    val scratch = new Array[Double](3)
    for (p, i) <- points.zipWithIndex do
      declared.map.forwardInto(p(0), p(1), p(2), out, scratch)
      for c <- 0 until 3 do assertEqualsDouble(out(c), mapped(i)(c), 1e-9, s"point $i")

  test("quarantine in either manifest form is refused"):
    copyWith(_.replaceAll("\"quarantine\"\\s*:\\s*null", "\"quarantine\": {\"archivePath\": \"x\", \"reason\": \"direction disputed\", \"evidence\": \"e\"}")) { dir =>
      assertEquals(DeclaredPointMapReader.read(dir, SyntheticItkOracle.sourceSha256).left.toOption,
        Some(ReferenceError.QuarantinedTransform("x", "direction disputed")))
    }
    copyWith(_.replaceAll("\"derivation\"\\s*:\\s*\"", "\"derivation\": \"QUARANTINED: ")) { dir =>
      assert(DeclaredPointMapReader.read(dir, SyntheticItkOracle.sourceSha256).left.exists(_.isInstanceOf[ReferenceError.QuarantinedTransform]))
    }

  test("tampered stage bytes, an unexpected source and a malformed manifest are refused"):
    copyWith(identity, bytes => bytes.updated(bytes.length - 1, (bytes.last ^ 1).toByte)) { dir =>
      assert(DeclaredPointMapReader.read(dir, SyntheticItkOracle.sourceSha256).left.exists(_.isInstanceOf[ReferenceError.DigestMismatch]))
    }
    assert(DeclaredPointMapReader.read(resource, "f" * 64).left.exists(_.isInstanceOf[ReferenceError.DigestMismatch]))
    copyWith(_ => "{\"schema\": 3}") { dir =>
      assert(DeclaredPointMapReader.read(dir, SyntheticItkOracle.sourceSha256).left.exists(_.isInstanceOf[ReferenceError.InvalidPointMap]))
    }
    assert(DeclaredPointMapReader.read(Path.of("/nonexistent"), SyntheticItkOracle.sourceSha256)
      .left.exists(_.isInstanceOf[ReferenceError.AssetReadFailure]))
