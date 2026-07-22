package scalafim.archive.zarr

import scalafim.zarr.*

object NeuroArchiveManifestCodec:
  def render(manifest: NeuroArchiveManifest): String =
    val timing = manifest.timing match
      case AcquisitionTiming.Regular(origin, step, count, units) =>
        s"{\"count\":$count,\"kind\":\"regular\",\"origin\":${number(origin)},\"step\":${number(step)},\"units\":${quoted(units.id)}}"
      case AcquisitionTiming.Explicit(coordinates, units) =>
        s"{\"coordinates\":${doubleArray(coordinates)},\"kind\":\"explicit\",\"units\":${quoted(units.id)}}"
    "{" +
      s"\"acquisition_id\":${quoted(manifest.acquisitionId.value)}," +
      "\"canonical\":{" +
      "\"axes\":[\"t\",\"z\",\"y\",\"x\"]," +
      "\"calibration\":{" +
      s"\"offset\":${number(manifest.calibration.offset)}," +
      "\"physical_data_type\":\"float64\"," +
      s"\"scale\":${number(manifest.calibration.scale)}," +
      s"\"signal_units\":${quoted(manifest.calibration.units.id)}," +
      s"\"stored_data_type\":${quoted(manifest.calibration.storedDataType)}}," +
      "\"geometry\":{" +
      s"\"spatial_shape\":${longArray(manifest.geometry.spatialShape.toVector)}," +
      s"\"spatial_units\":${quoted(manifest.geometry.units.id)}," +
      s"\"voxel_to_world\":${doubleArray(manifest.geometry.voxelToWorld.toVector)}}," +
      s"\"logical_payload_sha256\":${quoted(manifest.logicalPayloadHash.value)}," +
      "\"path\":\"canonical\"," +
      s"\"shape\":${longArray(manifest.shape.toVector)}," +
      s"\"timing\":$timing}," +
      s"\"content_revision\":${quoted(manifest.contentRevision.value)}," +
      s"\"payload_id\":${quoted(manifest.payloadId.value)}," +
      s"\"profile\":${quoted(NeuroArchiveManifest.profileId)}," +
      "\"source\":{" +
      s"\"relative_path\":${quoted(manifest.source.relativePath)}," +
      s"\"sha256\":${quoted(manifest.source.sha256.value)}}}"

  def parse(input: String): Either[NeuroArchiveZarrError, NeuroArchiveManifest] =
    ProfileJson.root(input, "neuroarchive.json").flatMap: root =>
      for
        profile <- ProfileJson.string(root, "profile", "profile")
        _ <- ProfileJson.expect(profile == NeuroArchiveManifest.profileId, "profile", s"expected ${NeuroArchiveManifest.profileId}")
        acquisitionRaw <- ProfileJson.string(root, "acquisition_id", "acquisition_id")
        acquisition <- AcquisitionId.from(acquisitionRaw)
        payloadRaw <- ProfileJson.string(root, "payload_id", "payload_id")
        payload <- PayloadId.from(payloadRaw)
        revisionRaw <- ProfileJson.string(root, "content_revision", "content_revision")
        revision <- ContentRevision.from(revisionRaw)
        sourceObject <- ProfileJson.obj(root, "source", "source")
        sourcePath <- ProfileJson.string(sourceObject, "relative_path", "source.relative_path")
        sourceHashRaw <- ProfileJson.string(sourceObject, "sha256", "source.sha256")
        sourceHash <- Sha256Digest.from(sourceHashRaw)
        source <- SourceArtifact(sourcePath, sourceHash)
        canonical <- ProfileJson.obj(root, "canonical", "canonical")
        path <- ProfileJson.string(canonical, "path", "canonical.path")
        _ <- ProfileJson.expect(path == "canonical", "canonical.path", "must equal canonical")
        axes <- ProfileJson.strings(canonical, "axes", "canonical.axes")
        _ <- ProfileJson.expect(axes == NeuroArchiveManifest.canonicalAxes, "canonical.axes", "must equal [t,z,y,x]")
        shapeValues <- ProfileJson.longs(canonical, "shape", "canonical.shape")
        shape <- Shape.from(shapeValues).left.map(NeuroArchiveZarrError.Kernel.apply)
        calibrationObject <- ProfileJson.obj(canonical, "calibration", "canonical.calibration")
        storedType <- ProfileJson.string(calibrationObject, "stored_data_type", "canonical.calibration.stored_data_type")
        physicalType <- ProfileJson.string(calibrationObject, "physical_data_type", "canonical.calibration.physical_data_type")
        _ <- ProfileJson.expect(physicalType == "float64", "canonical.calibration.physical_data_type", "must equal float64")
        scale <- ProfileJson.double(calibrationObject, "scale", "canonical.calibration.scale")
        offset <- ProfileJson.double(calibrationObject, "offset", "canonical.calibration.offset")
        signalRaw <- ProfileJson.string(calibrationObject, "signal_units", "canonical.calibration.signal_units")
        signalUnits <- signalUnitsFrom(signalRaw)
        calibration <- ScalarCalibration(storedType, scale, offset, signalUnits)
        geometryObject <- ProfileJson.obj(canonical, "geometry", "canonical.geometry")
        spatialValues <- ProfileJson.longs(geometryObject, "spatial_shape", "canonical.geometry.spatial_shape")
        spatialShape <- Shape.from(spatialValues).left.map(NeuroArchiveZarrError.Kernel.apply)
        affineValues <- ProfileJson.doubles(geometryObject, "voxel_to_world", "canonical.geometry.voxel_to_world")
        affine <- Affine4x4(affineValues)
        spatialRaw <- ProfileJson.string(geometryObject, "spatial_units", "canonical.geometry.spatial_units")
        spatialUnits <- spatialUnitsFrom(spatialRaw)
        geometry <- VoxelGeometry(spatialShape, affine, spatialUnits)
        timingObject <- ProfileJson.obj(canonical, "timing", "canonical.timing")
        timing <- parseTiming(timingObject)
        logicalRaw <- ProfileJson.string(canonical, "logical_payload_sha256", "canonical.logical_payload_sha256")
        logicalHash <- Sha256Digest.from(logicalRaw)
        manifest <- NeuroArchiveManifest(
          acquisition,
          payload,
          revision,
          source,
          shape,
          calibration,
          geometry,
          timing,
          logicalHash
        )
      yield manifest

  private def parseTiming(value: JsonObject): Either[NeuroArchiveZarrError, AcquisitionTiming] =
    for
      kind <- ProfileJson.string(value, "kind", "canonical.timing.kind")
      unitsRaw <- ProfileJson.string(value, "units", "canonical.timing.units")
      units <- timeUnitsFrom(unitsRaw)
      timing <- kind match
        case "regular" =>
          ProfileJson.double(value, "origin", "canonical.timing.origin").flatMap: origin =>
            ProfileJson.double(value, "step", "canonical.timing.step").flatMap: step =>
              ProfileJson.long(value, "count", "canonical.timing.count").flatMap: count =>
                AcquisitionTiming.Regular(origin, step, count, units).validate
        case "explicit" =>
          ProfileJson.doubles(value, "coordinates", "canonical.timing.coordinates").flatMap: coordinates =>
            AcquisitionTiming.Explicit(coordinates, units).validate
        case found => Left(NeuroArchiveZarrError.InvalidManifest("canonical.timing.kind", s"unsupported timing kind $found"))
    yield timing

  private def signalUnitsFrom(value: String): Either[NeuroArchiveZarrError, SignalUnits] =
    SignalUnits.values.find(_.id == value).toRight(NeuroArchiveZarrError.InvalidManifest("canonical.calibration.signal_units", s"unsupported units $value"))

  private def spatialUnitsFrom(value: String): Either[NeuroArchiveZarrError, SpatialUnits] =
    SpatialUnits.values.find(_.id == value).toRight(NeuroArchiveZarrError.InvalidManifest("canonical.geometry.spatial_units", s"unsupported units $value"))

  private def timeUnitsFrom(value: String): Either[NeuroArchiveZarrError, TimeUnits] =
    TimeUnits.values.find(_.id == value).toRight(NeuroArchiveZarrError.InvalidManifest("canonical.timing.units", s"unsupported units $value"))

  private def quoted(value: String): String = JsonValue.Str(value).render
  private def number(value: Double): String = java.lang.Double.toString(value)
  private def longArray(values: Vector[Long]): String = values.mkString("[", ",", "]")
  private def doubleArray(values: Vector[Double]): String = values.map(number).mkString("[", ",", "]")

object PublicationReceiptCodec:
  def render(receipt: PublicationReceipt): String =
    val objects = receipt.objects.map: objectValue =>
      s"{\"key\":${JsonValue.Str(objectValue.key.value).render},\"length\":${objectValue.length.toLong},\"sha256\":${JsonValue.Str(objectValue.sha256.value).render}}"
    .mkString("[", ",", "]")
    "{" +
      s"\"canonical_metadata_sha256\":\"${receipt.canonicalMetadataHash.value}\"," +
      s"\"content_revision\":\"${receipt.contentRevision.value}\"," +
      s"\"expected_outer_objects\":${receipt.expectedOuterObjects}," +
      s"\"logical_payload_sha256\":\"${receipt.logicalPayloadHash.value}\"," +
      s"\"manifest_sha256\":\"${receipt.manifestHash.value}\"," +
      s"\"objects\":$objects," +
      s"\"profile\":\"${NeuroArchiveManifest.profileId}\"," +
      s"\"root_metadata_sha256\":\"${receipt.rootMetadataHash.value}\"," +
      s"\"state\":\"complete\",\"version\":${PublicationReceipt.version}," +
      s"\"writer_version\":${JsonValue.Str(receipt.writerVersion).render}}"

  def parse(input: String): Either[NeuroArchiveZarrError, PublicationReceipt] =
    ProfileJson.root(input, "publication.json").flatMap: root =>
      for
        version <- ProfileJson.long(root, "version", "version")
        _ <- ProfileJson.expect(version == PublicationReceipt.version.toLong, "version", s"expected ${PublicationReceipt.version}")
        profile <- ProfileJson.string(root, "profile", "profile")
        _ <- ProfileJson.expect(profile == NeuroArchiveManifest.profileId, "profile", s"expected ${NeuroArchiveManifest.profileId}")
        state <- ProfileJson.string(root, "state", "state")
        _ <- ProfileJson.expect(state == "complete", "state", "must equal complete")
        revisionRaw <- ProfileJson.string(root, "content_revision", "content_revision")
        revision <- ContentRevision.from(revisionRaw)
        logicalRaw <- ProfileJson.string(root, "logical_payload_sha256", "logical_payload_sha256")
        logical <- Sha256Digest.from(logicalRaw)
        rootRaw <- ProfileJson.string(root, "root_metadata_sha256", "root_metadata_sha256")
        rootHash <- Sha256Digest.from(rootRaw)
        manifestRaw <- ProfileJson.string(root, "manifest_sha256", "manifest_sha256")
        manifestHash <- Sha256Digest.from(manifestRaw)
        canonicalRaw <- ProfileJson.string(root, "canonical_metadata_sha256", "canonical_metadata_sha256")
        canonicalHash <- Sha256Digest.from(canonicalRaw)
        expected <- ProfileJson.long(root, "expected_outer_objects", "expected_outer_objects")
        objectValues <- ProfileJson.array(root, "objects", "objects")
        objects <- parseObjects(objectValues)
        writer <- ProfileJson.string(root, "writer_version", "writer_version")
        receipt <- PublicationReceipt.complete(
          revision,
          logical,
          rootHash,
          manifestHash,
          canonicalHash,
          expected,
          objects,
          writer
        )
      yield receipt

  private def parseObjects(values: Vector[JsonValue]): Either[NeuroArchiveZarrError, Vector[PublishedObject]] =
    val result = Vector.newBuilder[PublishedObject]
    var index = 0
    while index < values.length do
      values(index) match
        case JsonValue.Obj(value) =>
          val parsed = for
            keyRaw <- ProfileJson.string(value, "key", s"objects[$index].key")
            key <- StoreKey.from(keyRaw).left.map(NeuroArchiveZarrError.Kernel.apply)
            lengthRaw <- ProfileJson.long(value, "length", s"objects[$index].length")
            length <- ByteCount(lengthRaw).left.map(NeuroArchiveZarrError.Kernel.apply)
            hashRaw <- ProfileJson.string(value, "sha256", s"objects[$index].sha256")
            hash <- Sha256Digest.from(hashRaw)
          yield PublishedObject(key, length, hash)
          parsed match
            case Left(error) => return Left(error)
            case Right(found) => result += found
        case _ => return Left(NeuroArchiveZarrError.InvalidManifest(s"objects[$index]", "must be an object"))
      index += 1
    Right(result.result())

object NeuroArchiveRootMetadata:
  val render: String =
    s"{\"attributes\":{\"neuroarchive_profile\":\"${NeuroArchiveManifest.profileId}\"},\"node_type\":\"group\",\"zarr_format\":3}"

  def validate(input: String): Either[NeuroArchiveZarrError, Unit] =
    ZarrMetadata.parse(input).left.map(NeuroArchiveZarrError.Kernel.apply).flatMap:
      case ZarrNodeMetadata.Array(_) => Left(NeuroArchiveZarrError.InvalidManifest("zarr.json", "root must be a group"))
      case ZarrNodeMetadata.Group(group) => group.attributes.get("neuroarchive_profile") match
        case Some(JsonValue.Str(NeuroArchiveManifest.profileId)) => Right(())
        case _ => Left(NeuroArchiveZarrError.InvalidManifest("zarr.json.attributes.neuroarchive_profile", "profile link is missing or invalid"))

private object ProfileJson:
  def root(input: String, subject: String): Either[NeuroArchiveZarrError, JsonObject] =
    JsonParser.parse(input).left.map(error => NeuroArchiveZarrError.InvalidManifest(subject, error.message)).flatMap:
      case JsonValue.Obj(value) => Right(value)
      case _ => Left(NeuroArchiveZarrError.InvalidManifest(subject, "root must be an object"))

  def expect(condition: Boolean, path: String, detail: String): Either[NeuroArchiveZarrError, Unit] =
    if condition then Right(()) else Left(NeuroArchiveZarrError.InvalidManifest(path, detail))

  def obj(parent: JsonObject, name: String, path: String): Either[NeuroArchiveZarrError, JsonObject] = parent.get(name) match
    case Some(JsonValue.Obj(value)) => Right(value)
    case Some(_) => Left(NeuroArchiveZarrError.InvalidManifest(path, "must be an object"))
    case None => Left(NeuroArchiveZarrError.InvalidManifest(path, "is required"))

  def array(parent: JsonObject, name: String, path: String): Either[NeuroArchiveZarrError, Vector[JsonValue]] = parent.get(name) match
    case Some(JsonValue.Arr(values)) => Right(values)
    case Some(_) => Left(NeuroArchiveZarrError.InvalidManifest(path, "must be an array"))
    case None => Left(NeuroArchiveZarrError.InvalidManifest(path, "is required"))

  def string(parent: JsonObject, name: String, path: String): Either[NeuroArchiveZarrError, String] = parent.get(name) match
    case Some(JsonValue.Str(value)) => Right(value)
    case Some(_) => Left(NeuroArchiveZarrError.InvalidManifest(path, "must be a string"))
    case None => Left(NeuroArchiveZarrError.InvalidManifest(path, "is required"))

  def long(parent: JsonObject, name: String, path: String): Either[NeuroArchiveZarrError, Long] = parent.get(name) match
    case Some(JsonValue.Num(value)) => value.toLongExact.left.map(detail => NeuroArchiveZarrError.InvalidManifest(path, detail))
    case Some(_) => Left(NeuroArchiveZarrError.InvalidManifest(path, "must be an integer"))
    case None => Left(NeuroArchiveZarrError.InvalidManifest(path, "is required"))

  def double(parent: JsonObject, name: String, path: String): Either[NeuroArchiveZarrError, Double] = parent.get(name) match
    case Some(JsonValue.Num(value)) =>
      val found = value.toDouble
      if found.isFinite then Right(found) else Left(NeuroArchiveZarrError.InvalidManifest(path, "must be finite"))
    case Some(_) => Left(NeuroArchiveZarrError.InvalidManifest(path, "must be a number"))
    case None => Left(NeuroArchiveZarrError.InvalidManifest(path, "is required"))

  def strings(parent: JsonObject, name: String, path: String): Either[NeuroArchiveZarrError, Vector[String]] =
    array(parent, name, path).flatMap(values => parseStrings(values, path))

  def longs(parent: JsonObject, name: String, path: String): Either[NeuroArchiveZarrError, Vector[Long]] =
    array(parent, name, path).flatMap(values => parseLongs(values, path))

  def doubles(parent: JsonObject, name: String, path: String): Either[NeuroArchiveZarrError, Vector[Double]] =
    array(parent, name, path).flatMap(values => parseDoubles(values, path))

  private def parseStrings(
      values: Vector[JsonValue],
      path: String
  ): Either[NeuroArchiveZarrError, Vector[String]] =
    val result = Vector.newBuilder[String]
    var index = 0
    while index < values.length do
      values(index) match
        case JsonValue.Str(value) => result += value
        case _ => return Left(NeuroArchiveZarrError.InvalidManifest(s"$path[$index]", "must be a string"))
      index += 1
    Right(result.result())

  private def parseLongs(
      values: Vector[JsonValue],
      path: String
  ): Either[NeuroArchiveZarrError, Vector[Long]] =
    val result = Vector.newBuilder[Long]
    var index = 0
    while index < values.length do
      values(index) match
        case JsonValue.Num(value) => value.toLongExact match
          case Left(detail) => return Left(NeuroArchiveZarrError.InvalidManifest(s"$path[$index]", detail))
          case Right(found) => result += found
        case _ => return Left(NeuroArchiveZarrError.InvalidManifest(s"$path[$index]", "must be an integer"))
      index += 1
    Right(result.result())

  private def parseDoubles(
      values: Vector[JsonValue],
      path: String
  ): Either[NeuroArchiveZarrError, Vector[Double]] =
    val result = Vector.newBuilder[Double]
    var index = 0
    while index < values.length do
      values(index) match
        case JsonValue.Num(value) =>
          val found = value.toDouble
          if !found.isFinite then return Left(NeuroArchiveZarrError.InvalidManifest(s"$path[$index]", "must be finite"))
          result += found
        case _ => return Left(NeuroArchiveZarrError.InvalidManifest(s"$path[$index]", "must be a number"))
      index += 1
    Right(result.result())
