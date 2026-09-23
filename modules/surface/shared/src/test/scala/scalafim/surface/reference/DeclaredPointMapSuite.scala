package scalafim.surface.reference

import scalafim.image.WorldPoint
import PointMapFixtures.*

/** Manifest verification: schema, quarantine, source and stage digests, and
  * the canonical NIfTI header, all before any stage is used.
  */
class DeclaredPointMapSuite extends munit.FunSuite:
  private val dims = Vector(4, 5, 3)
  private val grid = affine(1.5, 0.2, 0.0, -3.0, -0.1, 2.0, 0.0, -4.0, 0.0, 0.3, 1.25, -2.0, 0, 0, 0, 1)
  private val values = sampled(dims, grid)(p => Vector(0.1 * p(1), -0.05 * p(0), 0.2))
  private val after = affine(1.01, 0.0, 0.02, 0.5, 0.0, 0.99, 0.0, -0.3, 0.01, 0.0, 1.0, 0.2, 0, 0, 0, 1)
  private val (manifest, bytes) = PointMapFixtures.manifest(dims, grid, values, after)
  private val sourceSha = manifest.source.sha256

  private def load(m: PointMapManifest = manifest, b: Array[Byte] = bytes, expected: String = sourceSha) =
    DeclaredPointMap.fromManifest(m, expected, name => Option.when(name == "stage-0-displacement.nii")(b))

  test("a verified manifest evaluates exactly like the directly built composite"):
    val declared = load().fold(e => fail(e.message), d => d)
    val direct = PointMap.make(Vector(PointMapStage.DisplacementStage(DisplacementField.make(dims, grid, values).toOption.get),
      PointMapStage.AffineStage(after))).toOption.get
    val x = WorldPoint(-1.2, 0.4, -0.3)
    assertEquals(declared.map.forward(x), direct.forward(x))
    assertEquals(declared.input.value, "SynthIn")
    assertEquals(declared.output.value, "SynthOut")
    assertEquals(declared.catalogRevision.map(_.value), Some("templateflow@synthetic"))
    assertEquals(declared.stageFiles.map(_.sha256.value), Vector(AssetSha256.of(bytes).value))
    assert(declared.source.isInstanceOf[AssetProvenance])

  test("a quarantined manifest is refused, with no override"):
    val quarantined = manifest.copy(quarantine = Some(PointMapQuarantine(manifest.source.archivePath, "direction disputed")))
    assertEquals(load(quarantined).left.toOption,
      Some(ReferenceError.QuarantinedTransform(manifest.source.archivePath, "direction disputed")))

  test("the source digest must be the one the caller expects, and stage bytes must match their digest"):
    assert(load(expected = "b" * 64).left.exists(_.isInstanceOf[ReferenceError.DigestMismatch]))
    val tampered = bytes.clone()
    tampered(tampered.length - 3) = (tampered(tampered.length - 3) ^ 1).toByte
    assert(load(b = tampered).left.exists(_.isInstanceOf[ReferenceError.DigestMismatch]))
    assert(DeclaredPointMap.fromManifest(manifest, sourceSha, _ => None).left.exists(_.isInstanceOf[ReferenceError.AssetReadFailure]))

  test("header fields, sform and file names are checked against the manifest"):
    def stage(f: ManifestStage.DisplacementEntry => ManifestStage.DisplacementEntry) =
      manifest.copy(stages = manifest.stages.map {
        case d: ManifestStage.DisplacementEntry => f(d)
        case other => other
      })
    val shiftedGrid = grid.rowMajor.updated(3, -3.001)
    assert(load(stage(_.copy(voxelToRasRowMajor = shiftedGrid))).left.exists(_.message.contains("sform")))
    assert(load(stage(_.copy(dims = Vector(5, 4, 3)))).left.exists(_.message.contains("dim")))
    assert(load(stage(_.copy(file = "../stage-0-displacement.nii"))).left.exists(_.message.contains("file name")))
    val wrongType = bytes.clone()
    wrongType(70) = 16
    val retyped = stage(_.copy(sha256 = AssetSha256.of(wrongType).value))
    assert(load(retyped, wrongType).left.exists(_.message.contains("float64")))
    assert(load(manifest.copy(schema = "templateflow4s.point-map/2")).isLeft)
    assert(load(manifest.copy(output = manifest.input)).isLeft)
    assert(load(manifest.copy(input = "MNI152")).left.exists(_.isInstanceOf[ReferenceError.AmbiguousTemplate]))

  test("a source without a catalog revision is a named data asset"):
    val (unrevisioned, raw) = PointMapFixtures.manifest(dims, grid, values, after, revision = None)
    val declared = DeclaredPointMap.fromManifest(unrevisioned, unrevisioned.source.sha256,
      name => Option.when(name == "stage-0-displacement.nii")(raw)).fold(e => fail(e.message), d => d)
    assertEquals(declared.catalogRevision, None)
    assert(declared.source.isInstanceOf[DataAsset])
