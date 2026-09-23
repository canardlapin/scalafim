package scalafim.surface.reference

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** The JVM reader on the committed synthetic canonical directory. */
class DeclaredPointMapReaderSuite extends munit.FunSuite:
  private val resource = Path.of(getClass.getResource("/pointmap-synthetic/manifest.json").toURI).getParent

  private val source = SyntheticItkOracle.sourceSha256
  private val manifestSha = SyntheticItkOracle.manifestSha256

  /** Copy the fixture with an edited manifest; `f` receives the directory and the edited manifest's own digest. */
  private def copyWith(manifest: String => String, stage: Array[Byte] => Array[Byte] = identity)(f: (Path, String) => Unit): Unit =
    val dir = Files.createTempDirectory("scalafim-point-map-")
    try
      val text = manifest(Files.readString(resource.resolve("manifest.json"), StandardCharsets.UTF_8))
      Files.writeString(dir.resolve("manifest.json"), text)
      Files.write(dir.resolve("stage-0-displacement.nii"), stage(Files.readAllBytes(resource.resolve("stage-0-displacement.nii"))))
      f(dir, AssetSha256.of(text.getBytes(StandardCharsets.UTF_8)).value)
    finally
      Files.deleteIfExists(dir.resolve("manifest.json"))
      Files.deleteIfExists(dir.resolve("stage-0-displacement.nii"))
      Files.deleteIfExists(dir)

  test("the canonical synthetic directory reads, verifies and agrees with the SimpleITK oracle to 1e-9 mm"):
    val declared = DeclaredPointMapReader.read(resource, source, manifestSha).fold(e => fail(e.message), d => d)
    val oracle = ujson.read(Files.readString(resource.resolve("synthetic_composite_oracle.json")))
    val points = oracle("points").arr.map(_.arr.map(_.num).toVector).toVector
    val mapped = oracle("mapped").arr.map(_.arr.map(_.num).toVector).toVector
    def arrays(value: ujson.Value) = value.arr.map(_.arr.map(_.num).toVector).toVector
    // The shared suite runs on Scala.js from generated Scala data; it must be exactly the committed oracle.
    assertEquals(points, SyntheticItkOracle.points, "generated shared fixture matches the committed oracle")
    assertEquals(mapped, SyntheticItkOracle.mapped)
    assertEquals(arrays(oracle("stages")("displacementOnly")), SyntheticItkOracle.displacementOnly)
    assertEquals(arrays(oracle("stages")("affineOnly")), SyntheticItkOracle.affineOnly)
    assertEquals(oracle("pointClass").arr.map(_.str).toVector, SyntheticItkOracle.pointClass)
    assertEquals(java.util.Base64.getEncoder.encodeToString(Files.readAllBytes(resource.resolve("stage-0-displacement.nii"))),
      SyntheticItkOracle.stageFile)
    assertEquals(DeclaredPointMapReader.parse(Files.readString(resource.resolve("manifest.json"))), Right(SyntheticItkOracle.manifest.fields))
    assertEquals(declared.manifest.sha256.value, manifestSha)
    val out = new Array[Double](3)
    val scratch = new Array[Double](3)
    for (p, i) <- points.zipWithIndex do
      declared.map.forwardInto(p(0), p(1), p(2), out, scratch)
      for c <- 0 until 3 do assertEqualsDouble(out(c), mapped(i)(c), 1e-9, s"point $i")

  test("quarantine in either manifest form is refused, even with the edited manifest's own digest"):
    copyWith(_.replaceAll("\"quarantine\"\\s*:\\s*null", "\"quarantine\": {\"archivePath\": \"x\", \"reason\": \"direction disputed\", \"evidence\": \"e\"}")) { (dir, sha) =>
      assertEquals(DeclaredPointMapReader.read(dir, source, sha).left.toOption,
        Some(ReferenceError.QuarantinedTransform("x", "direction disputed")))
    }
    copyWith(_.replaceAll("\"derivation\"\\s*:\\s*\"", "\"derivation\": \"QUARANTINED: ")) { (dir, sha) =>
      assert(DeclaredPointMapReader.read(dir, source, sha).left.exists(_.isInstanceOf[ReferenceError.QuarantinedTransform]))
    }

  test("an edited affine, reordered stages or swapped frames are refused"):
    // Against the declared manifest digest, any edit is a digest mismatch.
    copyWith(_.replace("1.0269298001694105", "1.0269298001694106")) { (dir, _) =>
      assert(DeclaredPointMapReader.read(dir, source, manifestSha).left.exists(_.isInstanceOf[ReferenceError.DigestMismatch]))
    }
    copyWith { text =>
      val json = ujson.read(text)
      json("stages") = ujson.Arr(json("stages").arr.reverse.toSeq*)
      ujson.write(json)
    } { (dir, sha) =>
      assert(DeclaredPointMapReader.read(dir, source, manifestSha).left.exists(_.isInstanceOf[ReferenceError.DigestMismatch]))
      // Even a caller that trusts the reordered manifest gets the same map's stages in its declared order only.
      assert(DeclaredPointMapReader.read(dir, source, sha).map(_.map.stages.head.isInstanceOf[PointMapStage.AffineStage]) == Right(true))
    }
    copyWith { text =>
      val json = ujson.read(text)
      val input = json("frames")("input").str
      json("frames")("input") = json("frames")("output").str
      json("frames")("output") = input
      ujson.write(json)
    } { (dir, sha) =>
      assert(DeclaredPointMapReader.read(dir, source, sha).left.exists(_.isInstanceOf[ReferenceError.PointMapFrameMismatch]))
    }

  test("tampered stage bytes, an unexpected source and a malformed manifest are refused"):
    copyWith(identity, bytes => bytes.updated(bytes.length - 1, (bytes.last ^ 1).toByte)) { (dir, sha) =>
      assert(DeclaredPointMapReader.read(dir, source, sha).left.exists(_.isInstanceOf[ReferenceError.DigestMismatch]))
    }
    assert(DeclaredPointMapReader.read(resource, "f" * 64, manifestSha).left.exists(_.isInstanceOf[ReferenceError.DigestMismatch]))
    copyWith(_ => "{\"schema\": 3}") { (dir, sha) =>
      assert(DeclaredPointMapReader.read(dir, source, sha).left.exists(_.isInstanceOf[ReferenceError.InvalidPointMap]))
    }
    assert(DeclaredPointMapReader.read(Path.of("/nonexistent"), source, manifestSha)
      .left.exists(_.isInstanceOf[ReferenceError.AssetReadFailure]))
