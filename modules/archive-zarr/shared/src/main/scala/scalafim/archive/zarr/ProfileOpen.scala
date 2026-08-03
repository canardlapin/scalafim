package scalafim.archive.zarr

import zarr4s.*

final case class ProfileOpenLimits(
    maxPublicationBytes: ByteCount,
    maxManifestBytes: ByteCount,
    maxInventoryObjects: Int,
    zarr: OpenLimits
):
  require(maxInventoryObjects >= 0, "maxInventoryObjects must be non-negative")

object ProfileOpenLimits:
  def default: ProfileOpenLimits = ProfileOpenLimits(
    ByteCount(16L * 1024L * 1024L).fold(error => throw IllegalStateException(error.message), identity),
    ByteCount(4L * 1024L * 1024L).fold(error => throw IllegalStateException(error.message), identity),
    1000000,
    OpenLimits()
  )

final class OpenedCanonicalBold private[archive] (
    val canonical: CanonicalBold,
    val array: OpenedArray,
    val publication: PublicationReceipt
)

object NeuroArchiveZarr:
  def openCanonical(
      store: ObjectReader,
      capabilities: ZarrCapabilities = ZarrCapabilities(),
      limits: ProfileOpenLimits = ProfileOpenLimits.default,
      runtime: SyncCodecRuntime = SyncCodecRuntime.core
  ): Either[NeuroArchiveZarrError, OpenedCanonicalBold] =
    for
      publicationJson <- readText(store, "publication.json", limits.maxPublicationBytes)
      publication <- PublicationReceiptCodec.parse(publicationJson)
      _ <- ProfileOpenValidation.inventoryLimit(publication, limits.maxInventoryObjects)
      rootJson <- readText(store, "zarr.json", limits.zarr.maxMetadataBytes)
      manifestJson <- readText(store, "neuroarchive.json", limits.maxManifestBytes)
      canonicalJson <- readText(store, "canonical/zarr.json", limits.zarr.maxMetadataBytes)
      manifest <- ProfileOpenValidation.documents(rootJson, manifestJson, canonicalJson, publication)
      path <- ZarrPath("canonical").left.map(NeuroArchiveZarrError.Kernel.apply)
      opened <- SyncZarr.openArray(store, path, capabilities, limits.zarr, runtime)
        .left.map(NeuroArchiveZarrError.Kernel.apply)
      canonical <- CanonicalBold.refine(opened.descriptor, manifest)
      _ <- ProfileOpenValidation.inventory(opened.descriptor, publication)
      _ <- validateLengths(store, publication)
    yield new OpenedCanonicalBold(canonical, opened, publication)

  private def readText(
      store: ObjectReader,
      keyValue: String,
      limit: ByteCount
  ): Either[NeuroArchiveZarrError, String] =
    StoreKey.from(keyValue).left.map(NeuroArchiveZarrError.Kernel.apply).flatMap: key =>
      store.readAll(key, limit).left.map(error => NeuroArchiveZarrError.Kernel(ZarrError.StoreFailure(error))).map: bytes =>
        new String(bytes.toArray, "UTF-8")

  private def validateLengths(
      store: ObjectReader,
      publication: PublicationReceipt
  ): Either[NeuroArchiveZarrError, Unit] =
    var index = 0
    while index < publication.objects.length do
      val expected = publication.objects(index)
      store.length(expected.key) match
        case Left(error) =>
          return Left(NeuroArchiveZarrError.IncompletePublication(error.message))
        case Right(found) if found != expected.length.toLong =>
          return Left(NeuroArchiveZarrError.IncompletePublication(
            s"${expected.key.value} has length $found, expected ${expected.length.toLong}"
          ))
        case Right(_) => ()
      index += 1
    Right(())

private[zarr] object ProfileOpenValidation:
  def inventoryLimit(
      publication: PublicationReceipt,
      maximum: Int
  ): Either[NeuroArchiveZarrError, Unit] =
    if publication.objects.length > maximum then
      Left(NeuroArchiveZarrError.IncompletePublication(
        s"object inventory exceeds limit $maximum"
      ))
    else Right(())

  def documents(
      rootJson: String,
      manifestJson: String,
      canonicalJson: String,
      publication: PublicationReceipt
  ): Either[NeuroArchiveZarrError, NeuroArchiveManifest] =
    for
      _ <- digest("root zarr.json", rootJson, publication.rootMetadataHash)
      _ <- digest("neuroarchive.json", manifestJson, publication.manifestHash)
      _ <- digest("canonical zarr.json", canonicalJson, publication.canonicalMetadataHash)
      _ <- NeuroArchiveRootMetadata.validate(rootJson)
      manifest <- NeuroArchiveManifestCodec.parse(manifestJson)
      _ <- if manifest.contentRevision == publication.contentRevision then Right(())
        else Left(NeuroArchiveZarrError.ManifestArrayMismatch("content revision differs from publication"))
      _ <- if manifest.logicalPayloadHash == publication.logicalPayloadHash then Right(())
        else Left(NeuroArchiveZarrError.ManifestArrayMismatch("logical payload hash differs from publication"))
    yield manifest

  def inventory(
      descriptor: ArrayDescriptor,
      publication: PublicationReceipt
  ): Either[NeuroArchiveZarrError, Unit] =
    val grid = descriptor.layout match
      case PhysicalLayout.Direct(_) => descriptor.grid
      case PhysicalLayout.Sharded(sharded, _, _, _, _) => sharded.outerGrid
    grid.gridShape.elementCount.left.map(NeuroArchiveZarrError.Kernel.apply).flatMap: expected =>
      if expected != publication.expectedOuterObjects then
        Left(NeuroArchiveZarrError.IncompletePublication(
          s"descriptor expects $expected outer objects, publication declares ${publication.expectedOuterObjects}"
        ))
      else descriptor.chunkKeyEncoding match
        case encoding: DefaultChunkKeyEncoding => validateCoordinates(
          publication.objects,
          encoding,
          grid.gridShape,
          expected
        )
        case encoding => Left(NeuroArchiveZarrError.InvalidProfileLayout(
          s"canonical arrays require default chunk keys, found ${encoding.name}"
        ))

  private def validateCoordinates(
      objects: Vector[PublishedObject],
      encoding: DefaultChunkKeyEncoding,
      gridShape: Shape,
      expected: Long
  ): Either[NeuroArchiveZarrError, Unit] =
    val seen = scala.collection.mutable.HashSet.empty[Vector[Long]]
    var index = 0
    while index < objects.length do
      parseCoordinate(objects(index).key, encoding, gridShape) match
        case Left(error) => return Left(error)
        case Right(coordinate) =>
          if seen.contains(coordinate) then
            return Left(NeuroArchiveZarrError.IncompletePublication(
              s"duplicate outer coordinate ${coordinate.mkString("[", ",", "]")}"
            ))
          seen += coordinate
      index += 1
    if seen.size.toLong != expected then
      Left(NeuroArchiveZarrError.IncompletePublication(
        s"inventory covers ${seen.size} of $expected outer coordinates"
      ))
    else Right(())

  private def digest(
      subject: String,
      value: String,
      expected: Sha256Digest
  ): Either[NeuroArchiveZarrError, Unit] =
    val actual = Sha256.digestUtf8(value)
    if actual == expected then Right(())
    else Left(NeuroArchiveZarrError.DigestMismatch(subject, expected, actual))

  private def parseCoordinate(
      key: StoreKey,
      encoding: DefaultChunkKeyEncoding,
      gridShape: Shape
  ): Either[NeuroArchiveZarrError, Vector[Long]] =
    val relative = key.value.stripPrefix("canonical/")
    val raw = encoding.separator match
      case ChunkSeparator.Slash if relative.startsWith("c/") => relative.drop(2).split('/').toVector
      case ChunkSeparator.Dot if relative.startsWith("c.") => relative.drop(2).split('.').toVector
      case _ => return Left(NeuroArchiveZarrError.IncompletePublication(s"invalid canonical chunk key ${key.value}"))
    if raw.length != gridShape.rank.toInt then
      Left(NeuroArchiveZarrError.IncompletePublication(s"wrong-rank canonical chunk key ${key.value}"))
    else
      val coordinate = Vector.newBuilder[Long]
      var axis = 0
      while axis < raw.length do
        val value = try raw(axis).toLong
        catch case _: NumberFormatException =>
          return Left(NeuroArchiveZarrError.IncompletePublication(s"invalid canonical chunk key ${key.value}"))
        if value < 0L || value >= gridShape.axis(axis) then
          return Left(NeuroArchiveZarrError.IncompletePublication(s"out-of-grid canonical chunk key ${key.value}"))
        coordinate += value
        axis += 1
      Right(coordinate.result())
