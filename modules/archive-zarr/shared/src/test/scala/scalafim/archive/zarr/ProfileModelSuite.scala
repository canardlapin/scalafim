package scalafim.archive.zarr

class ProfileModelSuite extends munit.FunSuite:
  test("portable SHA-256 matches a standard oracle vector"):
    assertEquals(
      Sha256.digestUtf8("abc").value,
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    )

  test("published start-indexed shard fixture is byte-identical"):
    assertEquals(ProfileFixtures.publishedStartShard.length, 380)
    assertEquals(
      Sha256.digest(ProfileFixtures.publishedStartShard.toArray).value,
      "131db1c0a41d6ae9279582a49944c180c6ccdd40f9287fa267f4c2425d45b920"
    )

  test("scientific manifest has a deterministic checked round trip"):
    val manifest = ProfileFixtures.manifest
    val rendered = NeuroArchiveManifestCodec.render(manifest)
    assertEquals(NeuroArchiveManifestCodec.parse(rendered), Right(manifest))
    assertEquals(NeuroArchiveManifestCodec.render(manifest), rendered)

  test("CanonicalBold refines only exact t z y x profile arrays"):
    val manifest = ProfileFixtures.manifest
    val valid = ProfileFixtures.descriptor()
    assert(CanonicalBold.refine(valid, manifest).isRight)

    val wrongAxes = ProfileFixtures.descriptor(ProfileFixtures.wrongAxesMetadata)
    assert(CanonicalBold.refine(wrongAxes, manifest).isLeft)

    val endIndexed = ProfileFixtures.descriptor(ProfileFixtures.endIndexedMetadata)
    assert(CanonicalBold.refine(endIndexed, manifest).isLeft)

  test("manifest construction rejects geometry and timing disagreement"):
    val manifest = ProfileFixtures.manifest
    val wrongSpatial = zarr4s.Shape(2L, 3L, 2L)
      .fold(error => fail(error.message), identity)
    val geometry = VoxelGeometry(wrongSpatial, manifest.geometry.voxelToWorld)
      .fold(error => fail(error.message), identity)
    val result = NeuroArchiveManifest(
      manifest.acquisitionId,
      manifest.payloadId,
      manifest.contentRevision,
      manifest.source,
      manifest.shape,
      manifest.calibration,
      geometry,
      manifest.timing,
      manifest.logicalPayloadHash
    )
    assert(result.isLeft)

    assert(AcquisitionTiming.Regular(0.0, 0.0, 2L, TimeUnits.Second).validate.isLeft)

  test("publication codec retains exact physical inventory"):
    val descriptor = ProfileFixtures.descriptor()
    val manifestJson = NeuroArchiveManifestCodec.render(ProfileFixtures.manifest)
    val objects = ProfileFixtures.publicationObjects(descriptor)
    val receipt = ProfileFixtures.publication(
      descriptor,
      manifestJson,
      ProfileFixtures.directMetadata,
      objects
    )
    val rendered = PublicationReceiptCodec.render(receipt)
    assertEquals(PublicationReceiptCodec.parse(rendered), Right(receipt))

  test("measured canonical chunk profile is valid, inspectable, and rank-local"):
    val shape = zarr4s.Shape(1200L, 72L, 96L, 96L)
      .fold(error => fail(error.message), identity)
    val dataType = zarr4s.BuiltInDataTypes.all.find(_.name == "int16").get
    val sizing = CanonicalChunkProfile.balancedV01.sizing(shape, dataType)
      .fold(error => fail(error.message), identity)
    assertEquals(sizing.innerChunkBytes.toLong, 786432L)
    assertEquals(sizing.shardBytes.toLong, 84934656L)
    assertEquals(sizing.shardIndexBytes.toLong, 1732L)
    assertEquals(sizing.innerChunksPerShard.toVector, Vector(4L, 3L, 3L, 3L))

    val badShard = zarr4s.Shape(64L, 70L, 96L, 96L)
      .fold(error => fail(error.message), identity)
    assert(CanonicalChunkProfile(
      "invalid",
      CanonicalChunkProfile.balancedV01.innerChunkShape,
      badShard
    ).isLeft)
