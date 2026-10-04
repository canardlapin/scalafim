package scalafim.atlas.io

import java.nio.file.Path
import scalafim.atlas.*
import scalafim.image.world.XformCode

/** Source-backed qualification is opt-in: the immutable files are deliberately
  * outside the repository and supplied by the evidence runner. */
class PinnedAtlasQualificationSuite extends munit.FunSuite:
  private def root: Path =
    sys.props.get("scalafim.pinnedAtlasRoot") match
      case Some(value) => Path.of(value)
      case None =>
        assume(false, "source-backed qualification requires -Dscalafim.pinnedAtlasRoot=<directory>")
        Path.of(".")

  private def path(name: String): Path = root.resolve(name)

  test("HCPex parser admits complete aligned rows and refuses malformed or duplicate coverage"):
    def labels(ids: Vector[Int]) = ids.map(id => s"$id ${if id <= 213 then "L" else "R"} R$id $id").mkString("\n")
    def lut(ids: Vector[Int]) = ids.map(id => s"$id Full_${if id <= 213 then "L" else "R"} 1 2 3 0").mkString("\n")
    val ids = (1 to 426).toVector
    val parsed = HcpExLoader.parse(labels(ids), lut(ids)).fold(error => fail(error.message), identity)
    assertEquals(parsed.map(_.id.value), ids)
    assertEquals(parsed.head.hemisphere, Some(Hemisphere.Left))
    assertEquals(parsed.last.hemisphere, Some(Hemisphere.Right))
    assert(HcpExLoader.parse(labels(ids), lut(ids.updated(425, 425))).isLeft)
    assert(HcpExLoader.parse(labels(ids), lut(ids).replace("Full_L", "Full_X")).isLeft)

  test("HCPex labels and LUT reject paths whose pinned digest does not match"):
    val request = HcpExRequest()
    HcpExLoader.loadFromPaths(request, path("hcpex-1mm.nii.gz"), path("hcpex-labels.txt"), path("hcpex-labels.txt")) match
      case Left(AtlasAcquisitionError.Integrity("hcpex-lut", _, _)) => ()
      case other => fail(s"expected LUT integrity failure, got $other")

  test("HCPex one and two millimetre immutable payloads realize all ordered parcels and provenance"):
    Vector(
      HcpExRequest(VoxelResolution.OneMm) -> path("hcpex-1mm.nii.gz"),
      HcpExRequest(VoxelResolution.TwoMm) -> path("hcpex-2mm.nii")
    ).foreach: (request, volume) =>
      val atlas = HcpExLoader.loadFromPaths(request, volume, path("hcpex-labels.txt"), path("hcpex-lut.txt"))
        .fold(error => fail(error.message), identity)
      assertEquals(atlas.regions.ids.map(_.value), (1 to 426).toVector)
      assertEquals(atlas.region(RegionId(1)).flatMap(_.hemisphere), Some(Hemisphere.Left))
      assertEquals(atlas.region(RegionId(181)).flatMap(_.hemisphere), Some(Hemisphere.Right))
      assertEquals(atlas.provenance.sourceArtifacts.map(_.digest.map(_.value)),
        Vector(Some(request.volume.sha256), Some(request.labels.sha256), Some(request.lut.sha256)))
      assertEquals(atlas.ref.coordSpace.value, s"pinned-atlas-${request.volume.sha256}")
      assertEquals(atlas.ref.confidence, Confidence.Uncertain)
      assert(atlas.provenance.derivation.exists {
        case DerivationStep.DeclaredDescriptor(detail) => detail.contains("Artifact-coordinate admission")
        case _ => false
      })
      assertEquals(atlas.provenance.validate(strict = true), Vector(ProvenanceIssue.UncertainConfidence))

  test("Olsen immutable payload realizes full MTL and exact hippocampal subset"):
    val volume = path("olsen-mtl.nii.gz")
    val mtl = OlsenMtlLoader.loadFromPath(OlsenMtlRequest(), volume).fold(error => fail(error.message), identity)
    val hippocampus = OlsenMtlLoader.loadFromPath(OlsenMtlRequest(OlsenMtlMode.Hippocampus), volume)
      .fold(error => fail(error.message), identity)
    assertEquals(mtl.regions.ids.map(_.value), (1 to 16).toVector)
    assertEquals(hippocampus.regions.ids.map(_.value), OlsenMtlRequest.hippocampalIds)
    assertEquals(hippocampus.region(RegionId(1)).flatMap(_.hemisphere), Some(Hemisphere.Left))
    assertEquals(hippocampus.region(RegionId(16)).flatMap(_.hemisphere), Some(Hemisphere.Right))
    assertEquals(mtl.provenance.sourceArtifacts.head.digest.map(_.value), Some(OlsenMtlRequest.volume.sha256))
    assertEquals(mtl.ref.coordSpace.value, s"pinned-atlas-${OlsenMtlRequest.volume.sha256}")
    assertEquals(mtl.provenance.validate(strict = true), Vector(ProvenanceIssue.UncertainConfidence))

  test("artifact-coordinate admission refuses an unexpected selected NIfTI xform"):
    val request = OlsenMtlRequest()
    val result = PinnedVolumeLoader.load(request.ref, OlsenMtlRequest.regions, path("olsen-mtl.nii.gz"),
      Vector((request.volume, ArtifactRole.ParcellationVolume, path("olsen-mtl.nii.gz"))),
      AtlasCoordinateAdmission.DeclaredArtifactCoordinates(XformCode.AlignedAnatomical, "negative qualification fixture"))
    assert(result.isLeft)
