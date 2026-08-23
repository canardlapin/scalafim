package scalafim.atlas.io

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.Files
import scalafim.atlas.*

class AsegLoaderSuite extends munit.FunSuite:

  test("FreeSurferAseg descriptor exposes canonical atlas reference") {
    val spec = FreeSurferAseg.default
    assertEquals(spec.id, "aseg-bundled_extdata")
    assertEquals(spec.atlasRef().family, "aseg")
    assertEquals(spec.atlasRef().model, "FreeSurferASEG")
    assertEquals(spec.atlasRef().templateSpace, SpaceId.MNI152NLin6Asym)
    assertEquals(spec.atlasRef().coordSpace, SpaceId.MNI152)
    assertEquals(spec.atlasRef().resolution, Some("1mm"))
  }

  test("AsegLoader built-in label rows match neuroatlas ASEG metadata") {
    val rows = AsegLoader.labelRows
    assertEquals(rows.map(_.id.value), Vector(10, 11, 12, 13, 16, 17, 18, 26, 28, 49, 50, 51, 52, 53, 54, 58, 60))
    assertEquals(rows.length, 17)
    assertEquals(rows.head.label, "Thalamus")
    assertEquals(rows.head.hemisphere, Some(Hemisphere.Left))
    assertEquals(rows(4).label, "Brainstem")
    assertEquals(rows(4).hemisphere, None)
    assertEquals(rows(8).label, "VentralDC")
    assertEquals(rows(8).hemisphere, None)
    assertEquals(rows(9).label, "Thalamus")
    assertEquals(rows(9).hemisphere, Some(Hemisphere.Right))
    assertEquals(rows(16).label, "VentralDC")
    assertEquals(rows(16).hemisphere, Some(Hemisphere.Right))
  }

  test("AsegLoader parses FreeSurfer color LUT rows") {
    val text =
      """# id name r g b a
        |10 Left-Thalamus-Proper 0 118 14 0
        |16 Brain-Stem 119 159 176 0
        |49 Right-Thalamus-Proper 0 118 14 0
        |0 Unknown 0 0 0 0
        |""".stripMargin

    val regions = AsegLoader.parseColorLut(text)
    assertEquals(regions.map(_.id), Vector(RegionId(10), RegionId(16), RegionId(49)))
    assertEquals(regions.head.label, "Thalamus")
    assertEquals(regions.head.hemisphere, Some(Hemisphere.Left))
    assertEquals(regions(1).label, "Brainstem")
    assertEquals(regions(1).hemisphere, None)
    assertEquals(regions(2).hemisphere, Some(Hemisphere.Right))
    assertEquals(regions(2).attributes("freesurfer_label"), "Right-Thalamus-Proper")
  }

  test("AsegLoader loadFromPaths reads NIfTI labels and filters absent rows") {
    val tmp = Files.createTempDirectory("scalafim-aseg-nifti-test")
    val volumePath = tmp.resolve("aseg.nii")
    writeInt16Nifti(volumePath, Vector(2, 2, 1), Vector(10, 16, 49, 0))

    val atlas = AsegLoader.loadFromPaths(FreeSurferAseg.default, volumePath)

    assertEquals(atlas.space.spatialDims, Vector(2, 2, 1))
    assertEquals(atlas.regions.ids.map(_.value), Vector(10, 16, 49))
    assertEquals(atlas.regions.ids, Vector(RegionId(10), RegionId(16), RegionId(49)))
    assertEquals(atlas.region(RegionId(10)).map(_.label), Some("Thalamus"))
    assertEquals(atlas.region(RegionId(10)).flatMap(_.hemisphere), Some(Hemisphere.Left))
    assertEquals(atlas.region(RegionId(16)).flatMap(_.hemisphere), None)
    assertEquals(atlas.region(RegionId(49)).flatMap(_.hemisphere), Some(Hemisphere.Right))
    assertEquals(atlas.ref.artifacts.map(_.role), Vector(ArtifactRole.ParcellationVolume))
    assertEquals(atlas.ref.artifacts.head.citationDoi, Some("10.1016/S0896-6273(02)00569-X"))
    assertEquals(atlas.ref.history.head.details, "Loaded FreeSurfer ASEG atlas.")
    assertEquals(atlas.provenance.sourceArtifacts.map(_.role), Vector(ArtifactRole.ParcellationVolume))
    assertEquals(atlas.provenance.sourceArtifacts.head.localPath, Some(volumePath.toString))
    assertEquals(atlas.provenance.sourceArtifacts.head.digest.map(_.value.length), Some(64))
    assert(atlas.provenance.sourceArtifacts.head.licenseStatus.isAccounted, clue = atlas.provenance.sourceArtifacts.head.toString)
    assert(!atlas.provenance.validate(strict = true).exists(_.isInstanceOf[ProvenanceIssue.MissingDigest]))
    assert(!atlas.provenance.validate(strict = true).exists(_.isInstanceOf[ProvenanceIssue.MissingLicense]))
    assert(atlas.provenance.summaryLines.exists(_.contains("neuroatlas bundled FreeSurfer ASEG extdata")), clue = atlas.provenance.summary)
    assert(atlas.provenance.derivation.exists(_.isInstanceOf[DerivationStep.FilteredLabels]), clue = atlas.provenance.derivation.toString)
  }

  private def writeInt16Nifti(path: java.nio.file.Path, dims: Vector[Int], values: Vector[Int]): Unit =
    require(dims.length == 3, "test writer only supports 3D fixtures")
    require(values.length == dims.product, "value count must match dimensions")

    val voxOffset = 352
    val bytes = ByteBuffer.allocate(voxOffset + values.length * 2).order(ByteOrder.LITTLE_ENDIAN)
    bytes.putInt(0, 348)
    bytes.putShort(40, 3.toShort)
    dims.zipWithIndex.foreach { case (dim, i) =>
      bytes.putShort(42 + i * 2, dim.toShort)
    }
    bytes.putShort(70, 4.toShort)
    bytes.putShort(72, 16.toShort)
    bytes.putFloat(80, 1.0f)
    bytes.putFloat(84, 1.0f)
    bytes.putFloat(88, 1.0f)
    bytes.putFloat(108, voxOffset.toFloat)
    bytes.putFloat(112, 1.0f)
    bytes.putFloat(116, 0.0f)
    bytes.putShort(254, 1.toShort)
    bytes.putFloat(280, 1.0f)
    bytes.putFloat(296 + 1 * 4, 1.0f)
    bytes.putFloat(312 + 2 * 4, 1.0f)
    bytes.put(344, 'n'.toByte)
    bytes.put(345, '+'.toByte)
    bytes.put(346, '1'.toByte)
    var offset = voxOffset
    values.foreach { value =>
      bytes.putShort(offset, value.toShort)
      offset += 2
    }
    Files.write(path, bytes.array())
