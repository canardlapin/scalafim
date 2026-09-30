package scalafim.estimates.io.hdf5

import java.nio.file.Path
import scalafim.archive.ContentDigest
import scalafim.archive.hdf5.*
import scalafim.estimates.*
import scalafim.estimates.io.*

/** HDF-only local lifecycle. No default NIfTI fallback or native-provider discovery. */
final class Hdf5EstimateStore private[hdf5] (
    private[hdf5] val local: LocalEstimateStore,
    private[hdf5] val archive: Hdf5Archive,
    val resourceLimits: Hdf5EstimateLimits
) extends EstimateSetReader:
  val root: Path = local.root
  private[hdf5] def protect[A](body: => Either[EstimateError, A]): Either[EstimateError, A] = local.protect(body)

  private[hdf5] def datasets[A](path: Path, plan: Hdf5EstimateLayout)
      (use: (Hdf5Dataset, Hdf5Dataset) => Either[Hdf5Error, A]): Either[EstimateError, A] =
    Hdf5Scope.file(archive.openReadOnly(path.toString)): file =>
      for
        inventory <- Hdf5FlatInventory(Vector(plan.values.name, plan.validity.name))
        _ <- file.verifyFlatInventory(inventory)
        result <- Hdf5Scope.dataset(file.inspect(plan.values.name)): values =>
          Hdf5Scope.dataset(file.inspect(plan.validity.name)): validity =>
            if values.info != plan.values || validity.info != plan.validity then
              Left(Hdf5Error.UnsupportedStored("physical dtype/extent/chunks/filter differ from descriptor layout"))
            else use(values, validity)
      yield result
    .left.map(Hdf5EstimateLayout.error)

  private[hdf5] def verify(path: Path, plan: Hdf5EstimateLayout): Either[EstimateError, Unit] =
    datasets(path, plan)((_, _) => Right(()))

  private[hdf5] def metadata(reference: PinnedUnit): Either[EstimateError, (EstimateUnit, Vector[Hdf5Representation], Vector[Hdf5EstimateLayout])] =
    local.inspectWithRepresentations(reference).flatMap: (unit, all, status) =>
      val records = all.collect { case EstimateRepresentation.Hdf5(value) => value }
      if records.size != all.size || status.nonEmpty then Left(EstimateError.Unsupported("HDF5-only store refuses mixed or evidence-bearing units"))
      else for
        _ <- Hdf5Representation.validateInventory(unit, records)
        compact = records.filter(_.layout == Hdf5PayloadLayout.SharedNormalizedUpperTriangle).map(_.product).toSet
        plans <- Hdf5EstimateLayout.plans(unit, compact, resourceLimits)
        _ <- local.verifyEstimability(unit)
        _ <- plans.foldLeft[Either[EstimateError, Unit]](Right(())):
          (previous, plan) => previous.flatMap: _ =>
            val ref = records.find(_.product == plan.product.id).get.container
            local.objects.verify(local.verified(ref)).left.map(local.fromStore)
              .flatMap(_ => verify(root.resolve(ref.path), plan))
              .flatMap(_ => if plan.shared then Hdf5EstimateSource.invariance(this, root.resolve(ref.path), unit, plan) else Right(()))
      yield (unit, records, plans)

  def inspect(reference: PinnedUnit): Either[EstimateError, EstimateUnit] = metadata(reference).map(_._1)
  def open(reference: PinnedUnit, limits: ReadLimits): Either[EstimateError, EstimateSource] =
    if limits.maximumCells > resourceLimits.maximumBlockCells then Left(EstimateError.Unsupported("read block exceeds physical resource cap"))
    else if limits.maximumCells.toLong * 9L > limits.maximumStagingBytes then Left(EstimateError.Unsupported("bounded read buffers exceed staging byte cap"))
    else metadata(reference).map: (unit, records, plans) =>
      new Hdf5EstimateSource(this, unit, limits, records, plans)

  def newSink(unit: EstimateUnit, compactProducts: Set[ProductId] = Set.empty,
      cancelled: () => Boolean = () => false): Either[EstimateError, SharedCovarianceSink] =
    for
      plans <- Hdf5EstimateLayout.plans(unit, compactProducts, resourceLimits)
      _ <- local.verifyEstimability(unit)
      _ <- if cancelled == null then Left(EstimateError.Invalid("cancellation callback required")) else Right(())
      result <- protect:
        if cancelled() then Left(EstimateError.Cancelled)
        else Right(new Hdf5EstimateSink(this, unit, plans, cancelled))
    yield result

  private val collections = new LocalEstimateCollections(local, inspect)
  def publishCollection(collection: EstimateCollection): Either[EstimateError, PinnedEstimateSet] = collections.publish(collection)
  def openCollection(reference: PinnedEstimateSet): Either[EstimateError, EstimateCollection] = collections.open(reference)
  def current(): Either[EstimateError, Option[(PinnedEstimateSet, ContentDigest)]] = collections.current()
  def discover(reference: PinnedEstimateSet, expectedPointerDigest: Option[ContentDigest]): Either[EstimateError, Unit] =
    collections.discover(reference, expectedPointerDigest)

object Hdf5EstimateStore:
  def open(root: Path, archive: Hdf5Archive, limits: Hdf5EstimateLimits = Hdf5EstimateLimits()): Either[EstimateError, Hdf5EstimateStore] =
    if root == null || archive == null || limits == null then Left(EstimateError.Unsupported("explicit archive, local root and limits required"))
    else if archive.receipt.limits != limits.archive then Left(EstimateError.Unsupported("archive capability must use the exact bounded estimate resource profile"))
    else LocalEstimateStore.open(root).map(new Hdf5EstimateStore(_, archive, limits))
