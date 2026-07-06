package scalafim.atlas.io

import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}
import scalafim.atlas.*

class BrainnetomeLoaderSuite extends munit.FunSuite:

  test("Brainnetome246 descriptor exposes canonical atlas reference") {
    val spec = Brainnetome246.default
    assertEquals(spec.id, "brainnetome-246-brainnetome_download")
    assertEquals(spec.atlasRef().family, "brainnetome")
    assertEquals(spec.atlasRef().model, "BrainnetomeAtlas246")
    assertEquals(spec.atlasRef().templateSpace, SpaceId.MNI152)
    assertEquals(spec.atlasRef().resolution, Some("1mm"))
  }

  test("BrainnetomeLoader parses LUT and Yeo network metadata") {
    val lut =
      """0 Unknown 0 0 0 0
        |1 A8m_L 255 0 0 0
        |2 A8m_R 0 255 0 0
        |3 A9_46d_L 0 0 255 0
        |""".stripMargin
    val networks =
      """metadata row to skip
        |Label,subregion_name,region,Yeo_7network,Yeo_17network
        |1,A8m left,Frontal,7,16
        |2,A8m right,Frontal,7,17
        |3,"A9, 46d left",Frontal,6,12
        |""".stripMargin

    val regions = BrainnetomeLoader.parseLut(lut, Some(networks))

    assertEquals(regions.map(_.id), Vector(RegionId(1), RegionId(2), RegionId(3)))
    assertEquals(regions.head.label, "A8m_L")
    assertEquals(regions.head.hemisphere, Some(Hemisphere.Left))
    assertEquals(regions.head.network, Some(NetworkId("Default A")))
    assertEquals(regions.head.color, Some(Rgb(255, 0, 0)))
    assertEquals(regions.head.attributes("subregion_name"), "A8m left")
    assertEquals(regions.head.attributes("yeo_7network_name"), "Default")
    assertEquals(regions(1).hemisphere, Some(Hemisphere.Right))
    assertEquals(regions(1).network, Some(NetworkId("Default B")))
    assertEquals(regions(2).attributes("subregion_name"), "A9, 46d left")
    assertEquals(regions(2).network, Some(NetworkId("Control A")))
  }

  test("BrainnetomeLoader parses LUT without network table and tolerates unknown networks") {
    val lut =
      """# comment
        |1 A8m_L 255 0 0 0
        |2 Midline 0 255 0 0
        |bad row
        |""".stripMargin

    val withoutNetworks = BrainnetomeLoader.parseLut(lut, None)
    assertEquals(withoutNetworks.map(_.id), Vector(RegionId(1), RegionId(2)))
    assertEquals(withoutNetworks.head.hemisphere, Some(Hemisphere.Left))
    assertEquals(withoutNetworks.head.network, None)
    assertEquals(withoutNetworks.head.attributes, Map("atlas" -> "BrainnetomeAtlas246", "parcels" -> "246"))
    assertEquals(withoutNetworks(1).hemisphere, None)

    val networks =
      """Label,subregion_name,region,Yeo_7network,Yeo_17network
        |1,A8m left,Frontal,99,99
        |2,Midline,Midline,1,1
        |not-an-id,Ignored,Ignored,7,17
        |""".stripMargin
    val withUnknown = BrainnetomeLoader.parseLut(lut, Some(networks))
    assertEquals(withUnknown.head.network, None)
    assertEquals(withUnknown.head.attributes("yeo_7network"), "99")
    assert(!withUnknown.head.attributes.contains("yeo_7network_name"), clue = withUnknown.head.attributes.toString)
    assertEquals(withUnknown(1).network, Some(NetworkId("Visual peripheral")))
    assertEquals(withUnknown(1).attributes("yeo_7network_name"), "Visual")

    val noHeader = BrainnetomeLoader.parseLut(lut, Some("not,the,expected,header\n1,A,B,1,1"))
    assertEquals(noHeader.map(_.network), Vector(None, None))

    val assets = BrainnetomeLoader.assets()
    assertEquals(assets.volume.fileName, "BN_Atlas_246_1mm.nii.gz")
    assertEquals(assets.lut.fileName, "BN_Atlas_246_LUT.txt")
    assertEquals(assets.networks.fileName, "subregion_func_network_Yeo_updated.csv")
    assert(assets.volume.uri.toString.contains("shareDownload"), clue = assets.volume.uri.toString)
  }

  test("BrainnetomeLoader load resolves all assets through supplied store without downloads") {
    val tmp = Files.createTempDirectory("scalafim-brainnetome-load-store-test")
    val assets = BrainnetomeLoader.assets()
    val volumePath = tmp.resolve("BN_Atlas_246_1mm.nii")
    val lutPath = tmp.resolve(assets.lut.fileName)
    val networkPath = tmp.resolve(assets.networks.fileName)
    writeInt16Nifti(volumePath, Vector(2, 2, 1), Vector(1, 2, 1, 2))
    Files.writeString(
      lutPath,
      """1 A8m_L 255 0 0 0
        |2 A8m_R 0 255 0 0
        |""".stripMargin,
      StandardCharsets.UTF_8
    )
    Files.writeString(
      networkPath,
      """Label,subregion_name,region,Yeo_7network,Yeo_17network
        |1,A8m left,Frontal,7,16
        |2,A8m right,Frontal,7,17
        |""".stripMargin,
      StandardCharsets.UTF_8
    )
    val store =
      FixtureStore(
        Map(
          assets.volume.fileName -> volumePath,
          assets.lut.fileName -> lutPath,
          assets.networks.fileName -> networkPath
        )
      )

    val atlas = BrainnetomeLoader.load(store = store, policy = AssetPolicy.CacheOnly)

    assertEquals(atlas.regions.ids, Vector(RegionId(1), RegionId(2)))
    assertEquals(atlas.ref.source, Some("brainnetome_download"))
    assertEquals(
      store.resolved,
      Vector(
        assets.volume.key -> AssetPolicy.CacheOnly,
        assets.lut.key -> AssetPolicy.CacheOnly,
        assets.networks.key -> AssetPolicy.CacheOnly
      )
    )
  }

  test("BrainnetomeLoader loadFromPaths reads NIfTI labels and filters absent rows") {
    val tmp = Files.createTempDirectory("scalafim-brainnetome-nifti-test")
    val volumePath = tmp.resolve("brainnetome.nii")
    val lutPath = tmp.resolve("BN_Atlas_246_LUT.txt")
    val networkPath = tmp.resolve("subregion_func_network_Yeo_updated.csv")
    writeInt16Nifti(volumePath, Vector(2, 2, 1), Vector(1, 2, 1, 2))
    Files.writeString(
      lutPath,
      """1 A8m_L 255 0 0 0
        |2 A8m_R 0 255 0 0
        |3 Absent_L 0 0 255 0
        |""".stripMargin,
      StandardCharsets.UTF_8
    )
    Files.writeString(
      networkPath,
      """skip this line
        |Label,subregion_name,region,Yeo_7network,Yeo_17network
        |1,A8m left,Frontal,7,16
        |2,A8m right,Frontal,7,17
        |3,Absent,Frontal,1,1
        |""".stripMargin,
      StandardCharsets.UTF_8
    )

    val atlas = BrainnetomeLoader.loadFromPaths(Brainnetome246.default, volumePath, lutPath, Some(networkPath))

    assertEquals(atlas.space.spatialDims, Vector(2, 2, 1))
    assertEquals(atlas.volume.clusterIds, Vector(1, 2))
    assertEquals(atlas.regions.ids, Vector(RegionId(1), RegionId(2)))
    assertEquals(atlas.region(RegionId(1)).map(_.label), Some("A8m_L"))
    assertEquals(atlas.region(RegionId(1)).flatMap(_.network), Some(NetworkId("Default A")))
    assertEquals(atlas.region(RegionId(2)).flatMap(_.network), Some(NetworkId("Default B")))
    assertEquals(atlas.ref.artifacts.map(_.role), Vector("parcellation_volume", "label_table", "network_table"))
    assertEquals(atlas.ref.artifacts.head.citationDoi, Some("10.1093/cercor/bhw157"))
    assertEquals(atlas.ref.history.head.confidence, Confidence.High)
    assertEquals(
      atlas.provenance.sourceArtifacts.map(_.role),
      Vector(ArtifactRole.ParcellationVolume, ArtifactRole.LabelTable, ArtifactRole.NetworkTable)
    )
    assert(atlas.provenance.sourceArtifacts.forall(_.localPath.nonEmpty), clue = atlas.provenance.sourceArtifacts.toString)
    assert(atlas.provenance.sourceArtifacts.forall(_.digest.exists(_.value.length == 64)), clue = atlas.provenance.sourceArtifacts.toString)
    assert(atlas.provenance.sourceArtifacts.forall(_.licenseStatus.isAccounted), clue = atlas.provenance.sourceArtifacts.toString)
    assert(!atlas.provenance.validate(strict = true).exists(_.isInstanceOf[ProvenanceIssue.MissingDigest]))
    assert(!atlas.provenance.validate(strict = true).exists(_.isInstanceOf[ProvenanceIssue.MissingLicense]))
    assert(atlas.provenance.summaryLines.exists(_.contains("restricted: Brainnetome website legal agreement")), clue = atlas.provenance.summary)
    assert(atlas.provenance.derivation.exists(_.isInstanceOf[DerivationStep.FilteredLabels]), clue = atlas.provenance.derivation.toString)
  }

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
