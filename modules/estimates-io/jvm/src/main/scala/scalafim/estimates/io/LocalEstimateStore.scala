package scalafim.estimates.io

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal
import scalafim.archive.io.{LocalObjectStore, LocalStoreError, VerifiedFileObject}
import scalafim.estimates.*

/** Local metadata publication and fully verified reopening. Collection coverage
  * remains distinct from a successfully published unit and consumer admission.
  */
final class LocalEstimateStore private[io] (private[io] val objects: LocalObjectStore) extends EstimateSetReader:
  val root: Path = objects.root

  private[io] def fromStore(error: LocalStoreError): EstimateError = error match
    case LocalStoreError.Invalid(detail) => EstimateError.Invalid(detail)
    case LocalStoreError.Integrity(detail) => EstimateError.Integrity(detail)
    case LocalStoreError.Conflict(detail) => EstimateError.Conflict(detail)
    case LocalStoreError.Io(detail) => EstimateError.Io(detail)

  private[io] def reference(value: VerifiedFileObject): FileReference = FileReference(value.path, value.digest, value.bytes)
  private[io] def verified(value: FileReference): VerifiedFileObject = VerifiedFileObject(value.path, value.digest, value.bytes)

  private[io] def protect[A](body: => Either[EstimateError, A]): Either[EstimateError, A] =
    try body
    catch case NonFatal(error) => Left(EstimateError.Io(Option(error.getMessage).getOrElse(error.getClass.getName)))

  private[io] def text(ref: FileReference): Either[EstimateError, String] = protect:
    if ref.bytes > 16L * 1024L * 1024L then Left(EstimateError.Unsupported("metadata exceeds 16 MiB inspection budget"))
    else objects.verify(verified(ref)).left.map(fromStore).map(_ => Files.readString(root.resolve(ref.path), UTF_8))

  private[io] def writeText(path: String, text: String): Either[EstimateError, FileReference] =
    val bytes = text.getBytes(UTF_8)
    if bytes.length > 16 * 1024 * 1024 then Left(EstimateError.Unsupported("metadata exceeds 16 MiB inspection budget"))
    else objects.write(path)(_.write(bytes)).left.map(fromStore).map(reference)

  private[io] def publishUnit(unit: EstimateUnit, representations: Vector[NiftiRepresentation]): Either[EstimateError, PinnedUnit] =
    val prefix = s"units/${unit.revision.value}"
    // A fresh unit owns its catalog reference. Identical catalog bytes still
    // retain model identity; content-addressed deduplication is optional.
    for
      catalog <- writeText(s"$prefix/estimands.json", EstimateMetadata.catalog(unit.catalog))
      manifest <- writeText(s"$prefix/estimates.json", EstimateMetadata.unit(unit, catalog, representations))
    yield PinnedUnit(unit.unit, unit.revision, manifest)

  private[io] def inspectWithRepresentations(reference: PinnedUnit): Either[EstimateError, (EstimateUnit, Vector[NiftiRepresentation])] =
    for
      manifest <- text(reference.manifest)
      catalogRef <- EstimateMetadata.catalogReference(manifest)
      catalogText <- text(catalogRef)
      catalog <- EstimateMetadata.readCatalog(catalogText)
      unit <- EstimateMetadata.readUnit(manifest, catalog)
      _ <- if unit.unit == reference.unit && unit.revision == reference.revision then Right(())
           else Left(EstimateError.Integrity("pinned unit identity does not match the manifest"))
      representations <- EstimateMetadata.representations(manifest)
    yield (unit, representations)

  def inspect(reference: PinnedUnit): Either[EstimateError, EstimateUnit] = inspectWithRepresentations(reference).map(_._1)

  def open(reference: PinnedUnit, limits: ReadLimits): Either[EstimateError, EstimateSource] =
    inspectWithRepresentations(reference).flatMap((unit, representations) => NiftiEstimateSource.open(this, unit, representations, limits))

  def newSink(unit: EstimateUnit, maximumBlockCells: Int): Either[EstimateError, EstimateSink] =
    NiftiEstimateSink.open(this, unit, maximumBlockCells)

  private def validateCollection(collection: EstimateCollection): Either[EstimateError, Unit] =
    val published = collection.units.values.collect { case UnitOutcome.Published(ref) => ref }.toVector
    published.foldLeft[Either[EstimateError, Option[EstimandCatalog]]](Right(None)): (previous, ref) =>
      previous.flatMap: expectedCatalog =>
        inspect(ref).flatMap: unit =>
          if unit.dataset != collection.dataset || unit.catalog.model != collection.model then
            Left(EstimateError.Invalid("collection unit has a different dataset or model"))
          else if expectedCatalog.exists(_ != unit.catalog) then
            Left(EstimateError.Conflict("one model revision cannot identify different immutable catalogs"))
          else Right(Some(unit.catalog))
    .map(_ => ())

  def publishCollection(collection: EstimateCollection): Either[EstimateError, PinnedEstimateSet] =
    validateCollection(collection).flatMap: _ =>
      writeText(s"collections/${collection.revision.value}/estimateset.json", EstimateMetadata.collection(collection))
        .map(PinnedEstimateSet(collection.revision, _))

  def current(): Either[EstimateError, Option[(PinnedEstimateSet, scalafim.archive.ContentDigest)]] =
    if !Files.exists(root.resolve("current.json")) then Right(None)
    else for
      pointer <- objects.inspect("current.json").left.map(fromStore)
      encoded <- text(reference(pointer))
      pinned <- EstimateMetadata.readPointer(encoded)
      _ <- openCollection(pinned)
    yield Some(pinned -> pointer.digest)

  def openCollection(reference: PinnedEstimateSet): Either[EstimateError, EstimateCollection] =
    text(reference.manifest).flatMap(EstimateMetadata.readCollection).flatMap: collection =>
      if collection.revision == reference.revision then validateCollection(collection).map(_ => collection)
      else Left(EstimateError.Integrity("collection identity differs from pinned reference"))

  /** Stale writers receive a conflict; the scientific owner must merge compatible
    * additions into a new immutable collection before retrying.
    */
  def discover(reference: PinnedEstimateSet, expectedPointerDigest: Option[scalafim.archive.ContentDigest]): Either[EstimateError, Unit] =
    openCollection(reference).flatMap: _ =>
      objects.compareAndSwapPointer("current.json", expectedPointerDigest, EstimateMetadata.pointer(reference).getBytes(UTF_8))
        .left.map(fromStore).map(_ => ())

object LocalEstimateStore:
  def open(root: Path): Either[EstimateError, LocalEstimateStore] =
    LocalObjectStore.open(root).left.map(e => EstimateError.Io(e.message)).map(new LocalEstimateStore(_))
