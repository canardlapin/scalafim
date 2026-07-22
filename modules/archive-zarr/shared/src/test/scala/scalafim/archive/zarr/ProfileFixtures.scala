package scalafim.archive.zarr

import scalafim.zarr.*

object ProfileFixtures:
  val expectedHttpOpenTrace = Vector(
    "GET publication.json -",
    "GET zarr.json -",
    "GET neuroarchive.json -",
    "GET canonical/zarr.json -",
    "HEAD canonical/c/0/0/0/0 -"
  )

  val expectedHttpDataTrace = Vector(
    "GET canonical/c/0/0/0/0 bytes=0-131",
    "GET canonical/c/0/0/0/0 bytes=132-379"
  )

  val directMetadata =
    """{"zarr_format":3,"node_type":"array","shape":[2,2,2,3],"data_type":"int16","chunk_grid":{"name":"regular","configuration":{"chunk_shape":[1,1,2,2]}},"chunk_key_encoding":{"name":"default","configuration":{"separator":"/"}},"fill_value":-9,"codecs":[{"name":"bytes","configuration":{"endian":"little"}},{"name":"gzip","configuration":{"level":1}},{"name":"crc32c"}],"dimension_names":["t","z","y","x"],"attributes":{"neuroarchive_profile":"neuroarchive-zarr-0.1","payload_id":"bold-01"},"storage_transformers":[]}"""

  val wrongAxesMetadata = directMetadata.replace(
    "[\"t\",\"z\",\"y\",\"x\"]",
    "[\"x\",\"y\",\"z\",\"t\"]"
  )

  val endIndexedMetadata =
    """{"zarr_format":3,"node_type":"array","shape":[2,2,2,3],"data_type":"int16","chunk_grid":{"name":"regular","configuration":{"chunk_shape":[2,2,2,4]}},"chunk_key_encoding":{"name":"default","configuration":{"separator":"/"}},"fill_value":-9,"codecs":[{"name":"sharding_indexed","configuration":{"chunk_shape":[1,1,2,2],"codecs":[{"name":"bytes","configuration":{"endian":"little"}},{"name":"gzip","configuration":{"level":1}},{"name":"crc32c"}],"index_codecs":[{"name":"bytes","configuration":{"endian":"little"}},{"name":"crc32c"}],"index_location":"end"}}],"dimension_names":["t","z","y","x"],"attributes":{},"storage_transformers":[]}"""

  val startIndexedMetadata = endIndexedMetadata.replace(
    "\"index_location\":\"end\"",
    "\"index_location\":\"start\""
  )

  /** Byte-identical start-indexed shard emitted by JvmNeuroArchivePublisher.
    * It contains canonical int16 values 0 through 23 in [t,z,y,x] order.
    */
  val publishedStartShard: OwnedBytes = hex(
    "84000000000000002000000000000000a4000000000000001e0000000000" +
      "0000c2000000000000002000000000000000e2000000000000001e000000" +
      "000000000001000000000000200000000000000020010000000000001e00" +
      "0000000000003e0100000000000020000000000000005e01000000000000" +
      "1e000000000000009f2e76501f8b08000000000000ff6360606460666061" +
      "00002666a7d808000000e34d7ed81f8b08000000000000ff6362f8fe9f15" +
      "8801fc5ed5a4080000007af9468f1f8b08000000000000ff63636067e064" +
      "e06200005641af3908000000963e4fb71f8b08000000000000ffe360f8fe" +
      "9f1b88010735cad508000000d73301121f8b08000000000000ffe361e065" +
      "e06710600000ffdbb29c08000000716acdb21f8b08000000000000ffe363" +
      "f8fe5f1088017b8b012c08000000989877ee1f8b08000000000000ff1362" +
      "1066106510630000708ba1a508000000a0422c451f8b08000000000000ff" +
      "1361f8fe5f1c880144f911e4080000002a65b56e"
  )

  def descriptor(metadata: String = directMetadata): ArrayDescriptor =
    val array = ZarrMetadata.parse(metadata) match
      case Right(ZarrNodeMetadata.Array(found)) => found
      case Right(_) => throw IllegalArgumentException("expected array metadata")
      case Left(error) => throw IllegalArgumentException(error.message)
    ArrayDescriptor.compile(array).fold(error => throw IllegalArgumentException(error.message), identity)

  def manifest: NeuroArchiveManifest =
    val shape = Shape(2L, 2L, 2L, 3L).fold(error => throw IllegalArgumentException(error.message), identity)
    val spatial = Shape(2L, 2L, 3L).fold(error => throw IllegalArgumentException(error.message), identity)
    val affine = Affine4x4(Vector(
      2.0, 0.0, 0.0, -10.0,
      0.0, 2.0, 0.0, -12.0,
      0.0, 0.0, 2.5, -8.0,
      0.0, 0.0, 0.0, 1.0
    )).fold(error => throw IllegalArgumentException(error.message), identity)
    val geometry = VoxelGeometry(spatial, affine)
      .fold(error => throw IllegalArgumentException(error.message), identity)
    val calibration = ScalarCalibration("int16", 0.25, -2.0)
      .fold(error => throw IllegalArgumentException(error.message), identity)
    val sourceHash = Sha256.digestUtf8("source-nifti")
    val source = SourceArtifact("sub-01/func/sub-01_task-rest_bold.nii.gz", sourceHash)
      .fold(error => throw IllegalArgumentException(error.message), identity)
    NeuroArchiveManifest(
      AcquisitionId.unsafe("sub-01_task-rest_run-1"),
      PayloadId.unsafe("bold-01"),
      ContentRevision.unsafe("11" * 32),
      source,
      shape,
      calibration,
      geometry,
      AcquisitionTiming.Regular(0.0, 1.5, 2L, TimeUnits.Second),
      Sha256Digest.unsafe("22" * 32)
    ).fold(error => throw IllegalArgumentException(error.message), identity)

  def publicationObjects(descriptor: ArrayDescriptor): Vector[PublishedObject] =
    val gridShape = descriptor.grid.gridShape.toVector
    val result = Vector.newBuilder[PublishedObject]
    val coordinate = new Array[Long](gridShape.length)
    var done = false
    while !done do
      val key = StoreKey.from(s"canonical/c/${coordinate.mkString("/")}")
        .fold(error => throw IllegalArgumentException(error.message), identity)
      result += PublishedObject(key, ByteCount(1L).fold(error => throw IllegalArgumentException(error.message), identity), Sha256.digest(Array(0.toByte)))
      var axis = coordinate.length - 1
      var advanced = false
      while axis >= 0 && !advanced do
        coordinate(axis) += 1L
        if coordinate(axis) < gridShape(axis) then advanced = true
        else
          coordinate(axis) = 0L
          axis -= 1
      if !advanced then done = true
    result.result()

  def publication(
      descriptor: ArrayDescriptor,
      manifestJson: String,
      canonicalJson: String,
      objects: Vector[PublishedObject]
  ): PublicationReceipt =
    PublicationReceipt.complete(
      manifest.contentRevision,
      manifest.logicalPayloadHash,
      Sha256.digestUtf8(NeuroArchiveRootMetadata.render),
      Sha256.digestUtf8(manifestJson),
      Sha256.digestUtf8(canonicalJson),
      descriptor.grid.gridShape.elementCount.fold(error => throw IllegalArgumentException(error.message), identity),
      objects,
      "test-writer-0.1"
    ).fold(error => throw IllegalArgumentException(error.message), identity)

  def completeStartIndexedStore: Map[String, OwnedBytes] =
    val descriptor = this.descriptor(startIndexedMetadata)
    val manifestJson = NeuroArchiveManifestCodec.render(manifest)
    val objectKey = StoreKey.from("canonical/c/0/0/0/0")
      .fold(error => throw IllegalArgumentException(error.message), identity)
    val objects = Vector(PublishedObject(
      objectKey,
      publishedStartShard.byteCount,
      Sha256.digest(publishedStartShard.toArray)
    ))
    val receipt = PublicationReceipt.complete(
      manifest.contentRevision,
      manifest.logicalPayloadHash,
      Sha256.digestUtf8(NeuroArchiveRootMetadata.render),
      Sha256.digestUtf8(manifestJson),
      Sha256.digestUtf8(startIndexedMetadata),
      descriptor.grid.gridShape.elementCount
        .fold(error => throw IllegalArgumentException(error.message), identity),
      objects,
      "scalafim-archive-zarr-0.1"
    ).fold(error => throw IllegalArgumentException(error.message), identity)
    Map(
      "zarr.json" -> bytes(NeuroArchiveRootMetadata.render),
      "neuroarchive.json" -> bytes(manifestJson),
      "publication.json" -> bytes(PublicationReceiptCodec.render(receipt)),
      "canonical/zarr.json" -> bytes(startIndexedMetadata),
      objectKey.value -> publishedStartShard
    )

  def bytes(value: String): OwnedBytes = OwnedBytes.copyOf(value.getBytes("UTF-8"))

  private def hex(value: String): OwnedBytes =
    require(value.length % 2 == 0, "hex fixture must contain complete bytes")
    val result = new Array[Byte](value.length / 2)
    var index = 0
    while index < result.length do
      result(index) = Integer.parseInt(value.substring(index * 2, index * 2 + 2), 16).toByte
      index += 1
    OwnedBytes.copyOf(result)
