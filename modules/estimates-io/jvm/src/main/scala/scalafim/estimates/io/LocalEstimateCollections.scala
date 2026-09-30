package scalafim.estimates.io

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import scalafim.archive.ContentDigest
import scalafim.estimates.*

/** One immutable collection/CAS lifecycle with an explicitly supplied backend
  * inspector. The inspector's success is metadata admission, not science or
  * numerical payload qualification. Unsupported membership must fail here.
  */
private[io] final class LocalEstimateCollections(
    store: LocalEstimateStore,
    inspectPublished: PinnedUnit => Either[EstimateError, EstimateUnit]
):
  private def validate(collection: EstimateCollection): Either[EstimateError, Unit] =
    val published = collection.units.values.collect { case UnitOutcome.Published(ref) => ref }.toVector
    published.foldLeft[Either[EstimateError, Option[EstimandCatalog]]](Right(None)): (previous, ref) =>
      previous.flatMap: expectedCatalog =>
        inspectPublished(ref).flatMap: unit =>
          if unit.unit != ref.unit || unit.revision != ref.revision then
            Left(EstimateError.Integrity("pinned unit identity does not match inspected collection member"))
          else if unit.dataset != collection.dataset || unit.catalog.model != collection.model then
            Left(EstimateError.Invalid("collection unit has a different dataset or model"))
          else if expectedCatalog.exists(_ != unit.catalog) then
            Left(EstimateError.Conflict("one model revision cannot identify different immutable catalogs"))
          else Right(Some(unit.catalog))
    .map(_ => ())

  def publish(collection: EstimateCollection): Either[EstimateError, PinnedEstimateSet] =
    validate(collection).flatMap: _ =>
      store.writeText(s"collections/${collection.revision.value}/estimateset.json", EstimateMetadata.collection(collection))
        .map(PinnedEstimateSet(collection.revision, _))

  def open(reference: PinnedEstimateSet): Either[EstimateError, EstimateCollection] =
    store.text(reference.manifest).flatMap(EstimateMetadata.readCollection).flatMap: collection =>
      if collection.revision == reference.revision then validate(collection).map(_ => collection)
      else Left(EstimateError.Integrity("collection identity differs from pinned reference"))

  def current(): Either[EstimateError, Option[(PinnedEstimateSet, ContentDigest)]] =
    if !Files.exists(store.root.resolve("current.json")) then Right(None)
    else for
      pointer <- store.objects.inspect("current.json").left.map(store.fromStore)
      encoded <- store.text(store.reference(pointer))
      pinned <- EstimateMetadata.readPointer(encoded)
      _ <- open(pinned)
    yield Some(pinned -> pointer.digest)

  /** Stale writers must merge into a new immutable collection before retrying. */
  def discover(reference: PinnedEstimateSet, expectedPointerDigest: Option[ContentDigest]): Either[EstimateError, Unit] =
    open(reference).flatMap: _ =>
      store.objects.compareAndSwapPointer("current.json", expectedPointerDigest, EstimateMetadata.pointer(reference).getBytes(UTF_8))
        .left.map(store.fromStore).map(_ => ())
