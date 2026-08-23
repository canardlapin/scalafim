package scalafim.atlas.io

import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}
import scalafim.atlas.*
import scalafim.image.*

class AtlasIoSuite extends munit.FunSuite:

  test("FileAtlasStore resolves cached downloads and validates minBytes") {
    val tmp = Files.createTempDirectory("scalafim-atlas-store-test")
    val source = tmp.resolve("source.txt")
    Files.writeString(source, "atlas-data", StandardCharsets.UTF_8)
    val cache = tmp.resolve("cache")
    val store = FileAtlasStore(cache)
    val asset = AtlasAsset(
      key = "toy",
      fileName = "toy.txt",
      uri = source.toUri,
      minBytes = 4L
    )

    val resolved = store.resolve(asset)
    assertEquals(Files.readString(resolved, StandardCharsets.UTF_8), "atlas-data")
    assertEquals(AtlasAsset.sha256(resolved).length, 64)

    val cached = store.resolve(asset, AssetPolicy.CacheOnly)
    assertEquals(cached, resolved)

    interceptMessage[IllegalStateException]("atlas asset 'too-large' is not present in cache: " + cache.resolve("too-large.txt").toString) {
      store.resolve(asset.copy(key = "too-large", fileName = "too-large.txt", minBytes = 100L), AssetPolicy.CacheOnly)
    }
  }

  test("FileAtlasStore validates SHA-256 on downloaded assets") {
    val tmp = Files.createTempDirectory("scalafim-atlas-sha-test")
    val source = tmp.resolve("source.txt")
    Files.writeString(source, "atlas-data", StandardCharsets.UTF_8)
    val cache = tmp.resolve("cache")
    val store = FileAtlasStore(cache)
    val sha = AtlasAsset.sha256(source)
    val asset = AtlasAsset(
      key = "sha-ok",
      fileName = "sha-ok.txt",
      uri = source.toUri,
      minBytes = 4L,
      sha256 = Some(sha)
    )

    val resolved = store.resolve(asset, AssetPolicy.Refresh)
    assertEquals(AtlasAsset.sha256(resolved), sha)

    val bad = asset.copy(key = "bad-sha", fileName = "bad-sha.txt", sha256 = Some("0" * 64))
    val err =
      intercept[IllegalStateException] {
        store.resolve(bad, AssetPolicy.Refresh)
      }
    assertEquals(err.getMessage, "downloaded atlas asset 'bad-sha' failed validation")
    assert(!Files.exists(cache.resolve("bad-sha.txt")), clue = "failed downloads should not be cached")
  }

  test("FileAtlasStore refreshes stale cached assets and replaces cache on refresh") {
    val tmp = Files.createTempDirectory("scalafim-atlas-cache-refresh-test")
    val source = tmp.resolve("source.txt")
    val cache = tmp.resolve("cache")
    val cached = cache.resolve("atlas.txt")
    Files.createDirectories(cache)
    Files.writeString(cached, "stale-cache", StandardCharsets.UTF_8)
    Files.writeString(source, "fresh-cache", StandardCharsets.UTF_8)

    val store = FileAtlasStore(cache)
    val freshSha = AtlasAsset.sha256(source)
    val asset =
      AtlasAsset(
        key = "refresh",
        fileName = "atlas.txt",
        uri = source.toUri,
        minBytes = 1L,
        sha256 = Some(freshSha)
      )

    val resolved = store.resolve(asset, AssetPolicy.CacheOrDownload)
    assertEquals(resolved, cached)
    assertEquals(Files.readString(cached, StandardCharsets.UTF_8), "fresh-cache")
    assertEquals(AtlasAsset.sha256(cached), freshSha)

    Files.writeString(source, "new-refresh", StandardCharsets.UTF_8)
    val refreshedAsset = asset.copy(sha256 = Some(AtlasAsset.sha256(source)))
    val refreshed = store.resolve(refreshedAsset, AssetPolicy.Refresh)
    assertEquals(refreshed, cached)
    assertEquals(Files.readString(cached, StandardCharsets.UTF_8), "new-refresh")
  }

  test("AtlasAsset validates metadata fields") {
    val tmp = Files.createTempDirectory("scalafim-atlas-asset-validation-test")
    val source = tmp.resolve("source.txt")
    Files.writeString(source, "data", StandardCharsets.UTF_8)

    interceptMessage[IllegalArgumentException]("requirement failed: asset key must be non-empty") {
      AtlasAsset("", "atlas.txt", source.toUri)
    }

    interceptMessage[IllegalArgumentException]("requirement failed: asset fileName must be non-empty") {
      AtlasAsset("atlas", " ", source.toUri)
    }

    interceptMessage[IllegalArgumentException]("requirement failed: asset minBytes must be non-negative") {
      AtlasAsset("atlas", "atlas.txt", source.toUri, minBytes = -1L)
    }
  }

  test("AtlasLabelMaps converts finite integer-valued volumes") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val data = PrimitiveBuffers.fillConst[Double](4, 0.0)
    data(0) = 1.0
    data(1) = 2.0
    data(2) = 0.0
    data(3) = 2.0
    val labels = AtlasLabelMaps.fromDouble(NeuroVol.copyFromCanonicalArray[Double](data, sp), "toy")

    assertEquals(AtlasTestImages.labelAtCanonicalOrdinal(labels, 0), 1)
    assertEquals(AtlasTestImages.labelAtCanonicalOrdinal(labels, 1), 2)
    assertEquals(AtlasTestImages.labelAtCanonicalOrdinal(labels, 2), 0)
    assertEquals(AtlasTestImages.labelAtCanonicalOrdinal(labels, 3), 2)

    data(0) = 1.25
    interceptMessage[IllegalArgumentException]("label volume contains non-integer value 1.25 at linear index 0") {
      AtlasLabelMaps.fromDouble(NeuroVol.copyFromCanonicalArray[Double](data, sp))
    }
  }

  test("AtlasLabelMaps rejects non-finite and negative labels and preserves label fallback") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val fallbackData = PrimitiveBuffers.fromArray(Array(1.0, 2.0, 0.0, 2.0))
    val fallback = AtlasLabelMaps.fromDouble(NeuroVol.copyFromCanonicalArray[Double](fallbackData, sp, "source-label"))
    assertEquals(fallback.metadata.label, "source-label")
    assertEquals(AtlasTestImages.labelAtCanonicalOrdinal(fallback, 1), 2)

    val explicit = AtlasLabelMaps.fromDouble(NeuroVol.copyFromCanonicalArray[Double](fallbackData, sp, "source-label"), "explicit-label")
    assertEquals(explicit.metadata.label, "explicit-label")

    val nonFinite = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.fromArray(Array(1.0, Double.NaN, 0.0, 2.0)), sp)
    interceptMessage[IllegalArgumentException]("label volume contains non-finite value at linear index 1") {
      AtlasLabelMaps.fromDouble(nonFinite)
    }

    val negative = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.fromArray(Array(1.0, -1.0, 0.0, 2.0)), sp)
    interceptMessage[IllegalArgumentException]("label volume contains negative region id -1 at linear index 1") {
      AtlasLabelMaps.fromDouble(negative)
    }
  }

  test("SchaeferLoader parses CBIG LUT rows into typed regions") {
    val spec = Schaefer2018(SchaeferParcels.P100, YeoNetworks.Seven, VoxelResolution.TwoMm)
    val text =
      """1 7Networks_LH_Vis_1 120 10 20 0
        |2 7Networks_RH_Default_PFC_2 30 40 50 0
        |0 Background 0 0 0 0
        |""".stripMargin

    val regions = SchaeferLoader.parseLut(text, spec)
    assertEquals(regions.map(_.id), Vector(RegionId(1), RegionId(2)))
    assertEquals(regions.head.label, "Vis_1")
    assertEquals(regions.head.hemisphere, Some(Hemisphere.Left))
    assertEquals(regions.head.network, Some(NetworkId("Vis")))
    assertEquals(regions.head.color, Some(Rgb(120, 10, 20)))
    assertEquals(regions(1).label, "PFC_2")
    assertEquals(regions(1).hemisphere, Some(Hemisphere.Right))
    assertEquals(regions(1).network, Some(NetworkId("Default")))

    val ref = SchaeferLoader.refFor(spec)
    assertEquals(ref.artifacts.length, 2)
    assertEquals(ref.history.length, 1)
    assertEquals(ref.artifacts.head.citationDoi, Some("10.1093/cercor/bhx179"))
    assertEquals(ref.toProvenance(RegionIndex(regions)).sourceArtifacts.map(_.role), Vector(ArtifactRole.ParcellationVolume, ArtifactRole.LabelTable))
  }

  test("SchaeferLoader load resolves assets through supplied store without downloads") {
    val spec = Schaefer2018(SchaeferParcels.P100, YeoNetworks.Seven, VoxelResolution.TwoMm)
    val assets = SchaeferLoader.assets(spec)
    val tmp = Files.createTempDirectory("scalafim-schaefer-load-store-test")
    val volumePath = tmp.resolve("schaefer-load.nii")
    val labelPath = tmp.resolve(assets.labels.fileName)
    writeInt16Nifti(volumePath, Vector(2, 2, 1), Vector(1, 2, 1, 2))
    Files.writeString(
      labelPath,
      """1 7Networks_LH_Vis_1 120 10 20 0
        |2 7Networks_RH_Default_PFC_2 30 40 50 0
        |""".stripMargin,
      StandardCharsets.UTF_8
    )
    val store =
      FixtureStore(
        Map(
          assets.volume.fileName -> volumePath,
          assets.labels.fileName -> labelPath
        )
      )

    val atlas = SchaeferLoader.load(spec, store, AssetPolicy.CacheOnly)

    assertEquals(atlas.regions.ids, Vector(RegionId(1), RegionId(2)))
    assertEquals(atlas.ref.source, Some("cbig_mni"))
    assertEquals(store.resolved, Vector(assets.volume.key -> AssetPolicy.CacheOnly, assets.labels.key -> AssetPolicy.CacheOnly))
  }

  test("GlasserLoader parses node labels with hemisphere metadata") {
    val text =
      """L_V1_ROI
        |R_7Pm_ROI
        |""".stripMargin
    val regions = GlasserLoader.parseLabels(text)
    assertEquals(regions.map(_.id), Vector(RegionId(1), RegionId(2)))
    assertEquals(regions.head.label, "V1")
    assertEquals(regions.head.hemisphere, Some(Hemisphere.Left))
    assertEquals(regions(1).label, "7Pm")
    assertEquals(regions(1).hemisphere, Some(Hemisphere.Right))

    val ref = GlasserLoader.refFor(GlasserHcpMmp1(GlasserSource.Mni2009c))
    assertEquals(ref.artifacts.length, 2)
    assertEquals(ref.history.head.confidence, Confidence.High)
    assertEquals(ref.templateSpace, SpaceId.MNI152NLin2009cAsym)
    assertEquals(ref.toProvenance(RegionIndex(regions)).support, SpatialSupport.Volume(SpaceId.MNI152NLin2009cAsym, SpaceId.MNI152, Some(VoxelSize.isotropic(1.0))))
  }

  test("GlasserLoader descriptors cover all volume asset variants and unknown hemispheres") {
    val mniAssets = GlasserLoader.assets(GlasserHcpMmp1(GlasserSource.Mni2009c))
    assertEquals(mniAssets.volume.fileName, "MMP_in_MNI_corr.nii.gz")
    assertEquals(mniAssets.volume.key, "glasser-mni2009c-volume")
    assertEquals(mniAssets.labels.fileName, "glasser360NodeNames.txt")

    val xcpSpec = GlasserHcpMmp1(GlasserSource.XcpEngine)
    val xcpAssets = GlasserLoader.assets(xcpSpec)
    assertEquals(xcpAssets.volume.fileName, "glasser360MNI.nii.gz")
    assertEquals(xcpAssets.volume.key, "glasser-xcpengine-volume")

    val ref = GlasserLoader.refFor(xcpSpec)
    assertEquals(ref.templateSpace, SpaceId("MNI152_unspecified"))
    assertEquals(ref.confidence, Confidence.Uncertain)
    assertEquals(ref.artifacts.head.sourceRef, "glasser360MNI.nii.gz")
    assert(ref.artifacts.head.notes.exists(_.contains("less explicit template provenance")), clue = ref.artifacts.head.toString)

    val regions =
      GlasserLoader.parseLabels(
        """# comment
          |UnknownParcel_ROI
          |LH_Area_ROI
          |right_Area2_ROI
          |""".stripMargin
      )

    assertEquals(regions.map(_.id), Vector(RegionId(1), RegionId(2), RegionId(3)))
    assertEquals(regions.head.label, "ROI")
    assertEquals(regions.head.hemisphere, None)
    assertEquals(regions(1).hemisphere, Some(Hemisphere.Left))
    assertEquals(regions(2).hemisphere, Some(Hemisphere.Right))
  }

  test("AsegLoader load resolves default asset through supplied store without downloads") {
    val assets = AsegLoader.assets()
    val tmp = Files.createTempDirectory("scalafim-aseg-load-store-test")
    val volumePath = tmp.resolve("aseg-load.nii")
    writeInt16Nifti(volumePath, Vector(2, 2, 1), Vector(10, 49, 10, 49))
    val store = FixtureStore(Map(assets.volume.fileName -> volumePath))

    val atlas = AsegLoader.load(store = store, policy = AssetPolicy.CacheOnly)

    assertEquals(atlas.regions.ids, Vector(RegionId(10), RegionId(49)))
    assertEquals(atlas.ref.source, Some("bundled_extdata"))
    assertEquals(store.resolved, Vector(assets.volume.key -> AssetPolicy.CacheOnly))
  }

  test("GlasserLoader load resolves assets through supplied store without downloads") {
    val tmp = Files.createTempDirectory("scalafim-glasser-load-store-test")
    val volumePath = tmp.resolve("glasser360MNI.nii")
    val labelPath = tmp.resolve("glasser360NodeNames.txt")
    writeInt16Nifti(volumePath, Vector(2, 2, 1), Vector(1, 2, 0, 2))
    Files.writeString(
      labelPath,
      """L_V1_ROI
        |R_7Pm_ROI
        |""".stripMargin,
      StandardCharsets.UTF_8
    )
    val store =
      FixtureStore(
        Map(
          "glasser360MNI.nii.gz" -> volumePath,
          labelPath.getFileName.toString -> labelPath
        )
      )

    val atlas = GlasserLoader.load(store = store, policy = AssetPolicy.Refresh)

    assertEquals(atlas.ref.source, Some("xcpengine"))
    assertEquals(atlas.regions.ids, Vector(RegionId(1), RegionId(2)))
    assertEquals(store.resolved.map(_._1), Vector("glasser-xcpengine-volume", "glasser-xcpengine-labels"))
    assert(store.resolved.forall(_._2 == AssetPolicy.Refresh), clue = store.resolved.toString)
  }

  test("AtlasProvenanceFiles attaches local paths and digests by artifact role") {
    val tmp = Files.createTempDirectory("scalafim-atlas-provenance-files-test")
    val volumePath = tmp.resolve("toy.nii.gz")
    val labelPath = tmp.resolve("toy.tsv")
    Files.writeString(volumePath, "volume-bytes", StandardCharsets.UTF_8)
    Files.writeString(labelPath, "1\tA\n", StandardCharsets.UTF_8)
    val regions =
      RegionIndex(
        Vector(
          AtlasRegionMetadata(RegionId(1), "A", hemisphere = Some(Hemisphere.Left))
        )
      )
    val ref =
      AtlasRef.volume(
        family = "toy",
        model = "Toy",
        templateSpace = SpaceId.MNI152,
        coordSpace = SpaceId.MNI152,
        confidence = Confidence.High,
        artifacts = Vector(
          AtlasArtifact(role = ArtifactRole.ParcellationVolume, sourceName = "toy", sourceRef = "toy.nii.gz", license = Some("CC0")),
          AtlasArtifact(role = ArtifactRole.LabelTable, sourceName = "toy", sourceRef = "toy.tsv", license = Some("CC0"))
        )
      )

    val enriched = AtlasProvenanceFiles.withLocalFiles(ref.toProvenance(regions), ArtifactRole.ParcellationVolume -> volumePath)
    val volume = enriched.sourceArtifacts.find(_.role == ArtifactRole.ParcellationVolume).get
    val label = enriched.sourceArtifacts.find(_.role == ArtifactRole.LabelTable).get

    assertEquals(volume.localPath, Some(volumePath.toString))
    assertEquals(volume.digest.map(_.value), Some(AtlasAsset.sha256(volumePath)))
    assertEquals(label.localPath, None)
    assertEquals(label.digest, None)
  }

  test("AtlasLabelMaps builds VolumeAtlas from label volume and regions") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val data = PrimitiveBuffers.fillConst[Int](4, 0)
    data(0) = 1
    data(1) = 2
    data(2) = 1
    data(3) = 2
    val labelVol = AtlasTestImages.labelVolume(sp, data, "toy")
    val regions =
      RegionIndex(
        Vector(
          AtlasRegionMetadata(RegionId(1), "A", hemisphere = Some(Hemisphere.Left)),
          AtlasRegionMetadata(RegionId(2), "B", hemisphere = Some(Hemisphere.Right))
        )
      )
    val ref =
      AtlasRef.volume(
        family = "toy",
        model = "Toy",
        templateSpace = SpaceId.Custom,
        coordSpace = SpaceId.MNI152,
        confidence = Confidence.Exact
      )

    val atlas = AtlasLabelMaps.buildAtlas(ref, regions, labelVol)
    assertEquals(atlas.regions.ids.map(_.value), Vector(1, 2))
    assertEquals(atlas.region(RegionId(2)).map(_.label), Some("B"))
  }

  test("SchaeferLoader loadFromPaths reads a synthetic NIfTI fixture and filters absent LUT rows") {
    val tmp = Files.createTempDirectory("scalafim-schaefer-nifti-test")
    val volumePath = tmp.resolve("schaefer.nii")
    val labelPath = tmp.resolve("schaefer-lut.txt")
    writeInt16Nifti(volumePath, Vector(2, 2, 1), Vector(1, 2, 1, 2))
    Files.writeString(
      labelPath,
      """1 7Networks_LH_Vis_1 120 10 20 0
        |2 7Networks_RH_Default_PFC_2 30 40 50 0
        |3 7Networks_LH_SomMot_3 1 2 3 0
        |""".stripMargin,
      StandardCharsets.UTF_8
    )

    val atlas = SchaeferLoader.loadFromPaths(
      Schaefer2018(SchaeferParcels.P100, YeoNetworks.Seven, VoxelResolution.TwoMm),
      volumePath,
      labelPath
    )

    assertEquals(atlas.space.spatialDims, Vector(2, 2, 1))
    assertEquals(atlas.regions.ids.map(_.value), Vector(1, 2))
    assertEquals(atlas.regions.ids, Vector(RegionId(1), RegionId(2)))
    assertEquals(atlas.region(RegionId(1)).map(_.network), Some(Some(NetworkId("Vis"))))
    assertEquals(atlas.region(RegionId(2)).map(_.hemisphere), Some(Some(Hemisphere.Right)))
    assertLoadedFileProvenance(atlas, ArtifactRole.ParcellationVolume, volumePath.toString)
    assertLoadedFileProvenance(atlas, ArtifactRole.LabelTable, labelPath.toString)
    assertNoMissingSourceCompleteness(atlas)
    assert(atlas.provenance.summaryLines.exists(_.contains("release: Schaefer2018 CBIG release")), clue = atlas.provenance.summary)
  }

  test("GlasserLoader loadFromPaths reads a synthetic NIfTI fixture and keeps present labels only") {
    val tmp = Files.createTempDirectory("scalafim-glasser-nifti-test")
    val volumePath = tmp.resolve("glasser.nii")
    val labelPath = tmp.resolve("glasser-labels.txt")
    writeInt16Nifti(volumePath, Vector(2, 2, 1), Vector(1, 2, 0, 2))
    Files.writeString(
      labelPath,
      """L_V1_ROI
        |R_7Pm_ROI
        |L_absent_ROI
        |""".stripMargin,
      StandardCharsets.UTF_8
    )

    val atlas = GlasserLoader.loadFromPaths(GlasserHcpMmp1(GlasserSource.Mni2009c), volumePath, labelPath)

    assertEquals(atlas.space.spatialDims, Vector(2, 2, 1))
    assertEquals(atlas.regions.ids.map(_.value), Vector(1, 2))
    assertEquals(atlas.regions.labels, Vector("V1", "7Pm"))
    assertEquals(atlas.ref.source, Some("mni2009c"))
    assertEquals(atlas.ref.confidence, Confidence.High)
    assertLoadedFileProvenance(atlas, ArtifactRole.ParcellationVolume, volumePath.toString)
    assertLoadedFileProvenance(atlas, ArtifactRole.LabelTable, labelPath.toString)
    assertNoMissingSourceCompleteness(atlas)
    assert(atlas.provenance.summaryLines.exists(_.contains("version=1.0")), clue = atlas.provenance.summary)
  }

  private def assertLoadedFileProvenance(atlas: VolumeAtlas, role: ArtifactRole, expectedPath: String): Unit =
    val source = atlas.provenance.sourceArtifacts.find(_.role == role).getOrElse(fail(s"missing source role $role"))
    assertEquals(source.localPath, Some(expectedPath))
    assertEquals(source.digest.map(_.algorithm), Some("sha256"))
    assertEquals(source.digest.map(_.value.length), Some(64))
    assert(source.licenseStatus.isAccounted, clue = source.toString)

  private def assertNoMissingSourceCompleteness(atlas: VolumeAtlas): Unit =
    val issues = atlas.provenance.validate(strict = true)
    assert(!issues.exists(_.isInstanceOf[ProvenanceIssue.MissingDigest]), clue = issues.toString)
    assert(!issues.exists(_.isInstanceOf[ProvenanceIssue.MissingLicense]), clue = issues.toString)

  private final class FixtureStore(paths: Map[String, Path]) extends AtlasStore:
    var resolved: Vector[(String, AssetPolicy)] = Vector.empty

    def resolve(asset: AtlasAsset, policy: AssetPolicy = AssetPolicy.CacheOrDownload): Path =
      resolved = resolved :+ (asset.key -> policy)
      paths.getOrElse(asset.fileName, throw new NoSuchElementException(s"missing fixture for ${asset.fileName}"))

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
