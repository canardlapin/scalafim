package scalafim.archive.zarr

import java.nio.charset.StandardCharsets
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal
import zarr4s.*

object JvmNeuroArchivePublisher:
  val writerVersion = "scalafim-archive-zarr-0.1"

  def create(
      target: Path,
      descriptor: ArrayDescriptor,
      manifest: NeuroArchiveManifest,
      provider: ChunkProvider,
      writerLimits: WriterLimits = WriterLimits(),
      openLimits: ProfileOpenLimits = ProfileOpenLimits.default
  ): Either[NeuroArchiveZarrError, PublicationReceipt] = CanonicalBold.refine(descriptor, manifest) match
    case Left(error) => Left(error)
    case Right(_) => createRefined(target, descriptor, manifest, provider, writerLimits, openLimits)

  private def createRefined(
      target: Path,
      descriptor: ArrayDescriptor,
      manifest: NeuroArchiveManifest,
      provider: ChunkProvider,
      writerLimits: WriterLimits,
      openLimits: ProfileOpenLimits
  ): Either[NeuroArchiveZarrError, PublicationReceipt] =
    val absolute = target.toAbsolutePath.normalize()
    val parent = absolute.getParent
    if parent == null then Left(NeuroArchiveZarrError.PublicationFailure("target must have a parent directory"))
    else if Files.exists(absolute) then Left(NeuroArchiveZarrError.PublicationFailure(s"target already exists: $absolute"))
    else prepareStage(parent, absolute).flatMap: stage =>
        var published = false
        try
          for
            write <- JvmZarrWriter.create(stage.resolve("canonical"), descriptor, provider, writerLimits)
              .left.map(NeuroArchiveZarrError.Kernel.apply)
            expected <- expectedOuterObjects(descriptor)
            objects <- publishedObjects(write)
            rootJson = NeuroArchiveRootMetadata.render
            manifestJson = NeuroArchiveManifestCodec.render(manifest)
            canonicalJson <- readUtf8(stage.resolve("canonical/zarr.json"))
            _ <- writeUtf8(stage.resolve("zarr.json"), rootJson)
            _ <- writeUtf8(stage.resolve("neuroarchive.json"), manifestJson)
            receipt <- PublicationReceipt.complete(
              manifest.contentRevision,
              manifest.logicalPayloadHash,
              Sha256.digestUtf8(rootJson),
              Sha256.digestUtf8(manifestJson),
              Sha256.digestUtf8(canonicalJson),
              expected,
              objects,
              writerVersion
            )
            _ <- writeUtf8(stage.resolve("publication.json"), PublicationReceiptCodec.render(receipt))
            store <- JvmFileStore.open(stage).left.map(NeuroArchiveZarrError.PublicationFailure.apply)
            _ <- NeuroArchiveZarr.openCanonical(
              store,
              limits = openLimits,
              runtime = JvmCodecRuntime.portable
            ).map(_ => ())
            _ <- publish(stage, absolute)
          yield
            published = true
            receipt
        catch case NonFatal(error) => Left(NeuroArchiveZarrError.PublicationFailure(error.getMessage))
        finally if !published then deleteRecursively(stage)

  private def prepareStage(parent: Path, target: Path): Either[NeuroArchiveZarrError, Path] =
    try
      Files.createDirectories(parent)
      Right(Files.createTempDirectory(parent, s".${target.getFileName}.publication-"))
    catch case NonFatal(error) => Left(NeuroArchiveZarrError.PublicationFailure(error.getMessage))

  private def expectedOuterObjects(
      descriptor: ArrayDescriptor
  ): Either[NeuroArchiveZarrError, Long] =
    val shape = descriptor.layout match
      case PhysicalLayout.Direct(_) => descriptor.grid.gridShape
      case PhysicalLayout.Sharded(sharded, _, _, _, _) => sharded.outerGrid.gridShape
    shape.elementCount.left.map(NeuroArchiveZarrError.Kernel.apply)

  private def publishedObjects(
      receipt: WriteReceipt
  ): Either[NeuroArchiveZarrError, Vector[PublishedObject]] =
    val result = Vector.newBuilder[PublishedObject]
    var index = 0
    while index < receipt.objects.length do
      val objectValue = receipt.objects(index)
      val parsed = for
        key <- StoreKey.from(s"canonical/${objectValue.key.value}")
          .left.map(NeuroArchiveZarrError.Kernel.apply)
        digest <- Sha256Digest.from(objectValue.sha256.value)
      yield PublishedObject(key, objectValue.length, digest)
      parsed match
        case Left(error) => return Left(error)
        case Right(found) => result += found
      index += 1
    Right(result.result())

  private def readUtf8(path: Path): Either[NeuroArchiveZarrError, String] =
    try Right(Files.readString(path, StandardCharsets.UTF_8))
    catch case NonFatal(error) => Left(NeuroArchiveZarrError.PublicationFailure(error.getMessage))

  private def writeUtf8(path: Path, value: String): Either[NeuroArchiveZarrError, Unit] =
    try
      Files.writeString(path, value, StandardCharsets.UTF_8)
      Right(())
    catch case NonFatal(error) => Left(NeuroArchiveZarrError.PublicationFailure(error.getMessage))

  private def publish(stage: Path, target: Path): Either[NeuroArchiveZarrError, Unit] =
    try
      Files.move(stage, target, StandardCopyOption.ATOMIC_MOVE)
      Right(())
    catch
      case _: AtomicMoveNotSupportedException =>
        Left(NeuroArchiveZarrError.PublicationFailure("atomic directory publication is unavailable"))
      case NonFatal(error) => Left(NeuroArchiveZarrError.PublicationFailure(error.getMessage))

  private def deleteRecursively(root: Path): Unit =
    if Files.exists(root) then
      val stream = Files.walk(root)
      try stream.iterator().asScala.toVector.sortBy(_.getNameCount).reverse.foreach: path =>
        try Files.deleteIfExists(path)
        catch case NonFatal(_) => ()
      finally stream.close()
