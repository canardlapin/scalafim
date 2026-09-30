package scalafim.estimates.io.hdf5

import java.nio.file.Files
import scalafim.archive.hdf5.*
import scalafim.archive.io.StagedFile
import scalafim.estimates.*
import scalafim.estimates.io.Hdf5Representation

/** Bounded bit ledger, never a payload table. Allocation follows aggregate preflight. */
private[hdf5] final class Hdf5Coverage(plan: Hdf5EstimateLayout):
  private val bits = new Array[Byte](plan.coverageBytes)
  var remaining: Long = plan.cells
  def contains(index: Long): Boolean = (bits((index / 8).toInt) & (1 << (index % 8).toInt)) != 0
  def mark(index: Long): Unit =
    val byte = (index / 8).toInt
    bits(byte) = (bits(byte) | (1 << (index % 8).toInt)).toByte
    remaining -= 1

private[hdf5] final class Hdf5EstimateSink(
    store: Hdf5EstimateStore, val unit: EstimateUnit,
    plans: Vector[Hdf5EstimateLayout], cancelled: () => Boolean
) extends SharedCovarianceSink:
  val maximumBlockCells: Int = store.resourceLimits.maximumBlockCells
  override val supportsCovariance = true
  val sharedCovarianceProducts: Set[ProductId] = plans.filter(_.shared).map(_.product.id).toSet
  private val coverage = plans.map(p => p.product.id -> new Hdf5Coverage(p)).toMap
  private var stages = Map.empty[ProductId, StagedFile]
  private var active: Option[(Hdf5EstimateLayout, Hdf5File, Hdf5Dataset, Hdf5Dataset)] = None
  private var closed = false
  private var sealing = false
  private var abortRequested = false
  private var published: Option[PinnedUnit] = None
  private var failure: Option[EstimateError] = None

  private def closeActive(): Either[EstimateError, Unit] =
    val owned = active
    active = None
    owned.map(_._2.close().left.map(Hdf5EstimateLayout.error)).getOrElse(Right(()))

  private def discard(): Either[EstimateError, Unit] =
    stages.values.foldLeft[Either[EstimateError, Unit]](Right(())): (previous, stage) =>
      val result = store.protect:
        Files.deleteIfExists(stage.path)
        Files.deleteIfExists(stage.path.getParent)
        Right(())
      previous.flatMap(_ => result)

  private def stop(error: EstimateError): Either[EstimateError, Nothing] =
    sealing = false
    closed = true
    val cleanup = Vector(closeActive(), discard()).flatMap(_.left.toOption)
    val combined = if cleanup.isEmpty then error else EstimateError.Io(s"${error.message}; cleanup: ${cleanup.map(_.message).mkString("; ")}")
    failure = Some(combined)
    Left(combined)

  private def acquire(plan: Hdf5EstimateLayout): Either[EstimateError, Unit] =
    active match
      case Some((current, _, _, _)) if current.product.id == plan.product.id => Right(())
      case Some(_) => Left(EstimateError.Conflict("finish the active product before opening another transaction"))
      case None =>
        store.local.objects.stage(".h5").left.map(store.local.fromStore).flatMap: stage =>
          stages += plan.product.id -> stage
          store.archive.createExclusive(stage.path.toString).left.map(Hdf5EstimateLayout.error).flatMap: file =>
            val result = for
              values <- file.create(plan.values)
              validity <- file.create(plan.validity)
            yield
              active = Some((plan, file, values, validity))
            result.left.map(Hdf5EstimateLayout.error) match
              case Left(error) =>
                file.close() match
                  case Left(cleanup) => Left(EstimateError.Io(s"${error.message}; cleanup: ${cleanup.message}"))
                  case Right(_) => Left(error)
              case other => other

  private def capacities(values: Array[Double], validity: Array[Byte]): Either[EstimateError, (Int, Int)] =
    if values == null || validity == null then Left(EstimateError.Invalid("caller buffers required"))
    else Right(values.length -> validity.length)

  def write(product: ProductId, selection: EstimateSelection, values: Array[Double], validity: Array[Byte]): Either[EstimateError, Unit] = synchronized:
    if closed || sealing then Left(EstimateError.Closed)
    else for
      capacity <- capacities(values, validity)
      descriptor <- EstimateReadValidation.check(unit, product, selection, capacity._1, capacity._2, ReadLimits(maximumBlockCells))
      result <- deliver(product, selection.observations, selection.estimands.map(descriptor.targets.estimands.indexOf), selection.samples, values, validity)
    yield result

  override def writeCovariance(product: ProductId, selection: CovarianceSelection, values: Array[Double], validity: Array[Byte]): Either[EstimateError, Unit] = synchronized:
    if closed || sealing then Left(EstimateError.Closed)
    else if sharedCovarianceProducts.contains(product) then Left(EstimateError.Unsupported("compact covariance requires invariant pair-only delivery"))
    else for
      capacity <- capacities(values, validity)
      descriptor <- CovarianceReadValidation.check(unit, product, selection, capacity._1, capacity._2, ReadLimits(maximumBlockCells))
      result <- deliver(product, selection.observations, selection.pairs.map(CovarianceReadValidation.volume(descriptor, _)), selection.samples, values, validity)
    yield result

  def writeSharedCovariance(product: ProductId, selection: SharedCovarianceSelection, values: Array[Double], validity: Array[Byte]): Either[EstimateError, Unit] = synchronized:
    if closed || sealing then Left(EstimateError.Closed)
    else if !sharedCovarianceProducts.contains(product) then Left(EstimateError.Unsupported("product is not compact shared covariance"))
    else for
      capacity <- capacities(values, validity)
      descriptor <- SharedCovarianceValidation.check(unit, product, selection, capacity._1, capacity._2, maximumBlockCells)
      result <- deliver(product, selection.observations, selection.pairs.map(CovarianceReadValidation.volume(descriptor, _)), Vector(0), values, validity)
    yield result

  private def deliver(product: ProductId, observations: Vector[ObservationId], targets: Vector[Int], samples: Vector[Int],
      values: Array[Double], validity: Array[Byte]): Either[EstimateError, Unit] =
    val plan = plans.find(_.product.id == product).get
    val ledger = coverage(product)
    val diagonal = if plan.product.kind == ProductKind.Covariance then
      plan.product.targets.estimands.map(id => CovarianceReadValidation.volume(plan.product, EstimandPair(id, id))).toSet
    else Set.empty[Int]
    // Validate the entire caller request and coverage before acquiring or writing native payload.
    var invalid: Option[EstimateError] = None
    var offset = 0
    for observation <- observations; target <- targets; sample <- samples do
      val index = plan.index(plan.product.observations.indexOf(observation), target, sample)
      if ledger.contains(index) then invalid = Some(EstimateError.Conflict("duplicate product cell delivery"))
      Validity.fromCode(validity(offset)) match
        case Left(error) => invalid = Some(error)
        case Right(status) =>
          if plan.shared && (status == Validity.OutsideSupport || !values(offset).isFinite || (diagonal(target) && values(offset) < 0)) then
            invalid = Some(EstimateError.Invalid("shared covariance requires finite normalized values and invariant validity"))
          else if !plan.shared && unit.domain.contains(sample) == (status == Validity.OutsideSupport) then
            invalid = Some(EstimateError.Invalid("validity disagrees with declared support"))
          else if status == Validity.Valid && (!plan.product.kind.accepts(values(offset)) || (diagonal(target) && values(offset) < 0)) then
            invalid = Some(EstimateError.Invalid("valid product value violates numeric domain"))
          else if status == Validity.Valid && plan.product.precision == NumericPrecision.Float32 && values(offset).toFloat.toDouble != values(offset) then
            invalid = Some(EstimateError.Invalid("Float32 delivery requires explicit exact conversion"))
      offset += 1
    invalid match
      case Some(error) => Left(error)
      case None =>
        val result = store.protect:
          if cancelled() then Left(EstimateError.Cancelled)
          else if closed then Left(EstimateError.Closed)
          else acquire(plan).flatMap: _ =>
            val (_, _, data, codes) = active.get
            offset = 0
            var error: Option[EstimateError] = None
            for observation <- observations; target <- targets do
              var first = 0
              while first < samples.size && error.isEmpty do
                var count = 1
                if !plan.shared then
                  while first + count < samples.size && samples(first + count) == samples(first) + count do count += 1
                val slab = plan.slab(plan.product.observations.indexOf(observation), target, samples(first), count)
                val block = if plan.product.precision == NumericPrecision.Float32 then Hdf5Block.Float32(values.slice(offset, offset + count).map(_.toFloat))
                  else Hdf5Block.Float64(values.slice(offset, offset + count))
                val mask = Hdf5Block.UInt8(validity.slice(offset, offset + count))
                def cancelledOrClosed(): Boolean = cancelled() || closed
                val written = for
                  _ <- data.write(slab, block, cancelledOrClosed)
                  _ <- codes.write(slab, mask, cancelledOrClosed)
                yield ()
                written.left.map(Hdf5EstimateLayout.error) match
                  case Left(value) => error = Some(value)
                  case Right(_) if closed => error = Some(EstimateError.Closed)
                  case Right(_) =>
                    var i = 0
                    while i < count do
                      ledger.mark(plan.index(plan.product.observations.indexOf(observation), target, samples(first + i)))
                      i += 1
                first += count
                offset += count
            error match
              case Some(value) => Left(value)
              case None if ledger.remaining == 0 => closeActive().flatMap(_ => store.verify(stages(product).path, plan))
              case None => Right(())
        result match
          case Left(error: EstimateError.Conflict) => Left(error)
          case Left(error) => stop(error)
          case other => other

  def seal(): Either[EstimateError, PinnedUnit] = synchronized:
    published match
      case Some(reference) => Right(reference)
      case None if closed => Left(failure.getOrElse(EstimateError.Closed))
      case None if sealing => Left(EstimateError.Closed)
      case None if coverage.values.exists(_.remaining != 0) => Left(EstimateError.Invalid("cannot seal incomplete physical coverage"))
      case None =>
        sealing = true
        def permitPublication(): Either[EstimateError, Unit] =
          if abortRequested then Left(EstimateError.Cancelled) else Right(())
        val result = store.protect:
          for
            _ <- closeActive()
            _ <- if cancelled() then Left(EstimateError.Cancelled) else Right(())
            _ <- permitPublication()
            // Reopen every CLOSED stage read-only, exact links first and precise metadata second.
            _ <- plans.foldLeft[Either[EstimateError, Unit]](Right(()))((previous, plan) => previous.flatMap(_ => store.verify(stages(plan.product.id).path, plan)))
            _ <- plans.filter(_.shared).foldLeft[Either[EstimateError, Unit]](Right(())):
              (previous, plan) => previous.flatMap(_ => Hdf5EstimateSource.invariance(store, stages(plan.product.id).path, unit, plan))
            _ <- permitPublication()
            records <- plans.zipWithIndex.foldLeft[Either[EstimateError, Vector[Hdf5Representation]]](Right(Vector.empty)):
              case (previous, (plan, ordinal)) => previous.flatMap: records =>
                permitPublication().flatMap(_ => store.local.publishStagedIdempotent(stages(plan.product.id), s"units/${unit.revision.value}/product-$ordinal.h5")).map: container =>
                  records ++ plan.product.observations.map(observation => Hdf5Representation(plan.product.id, observation, container, plan.layout))
            _ <- permitPublication()
            ref <- store.local.publishHdf5Unit(unit, records)
          yield ref
        result match
          case Left(error) => stop(error)
          case Right(ref) =>
            sealing = false
            closed = true
            discard() match
              case Left(error) => failure = Some(error); Left(error)
              case Right(_) => published = Some(ref); Right(ref)

  def abort(): Either[EstimateError, Unit] = synchronized:
    if sealing then
      // The enclosing seal owns cleanup; a reentrant request must survive its callback.
      abortRequested = true
      Right(())
    else if closed then Right(())
    else
      closed = true
      val closing = closeActive()
      val discarded = discard()
      closing.flatMap(_ => discarded)
