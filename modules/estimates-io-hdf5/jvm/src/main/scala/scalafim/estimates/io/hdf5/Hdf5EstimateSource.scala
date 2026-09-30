package scalafim.estimates.io.hdf5

import java.nio.file.Path
import scalafim.archive.hdf5.*
import scalafim.estimates.*
import scalafim.estimates.io.Hdf5Representation

/** One read-only file scope per request; the destination remains unpublished on failure. */
private[hdf5] final class Hdf5EstimateSource(
    store: Hdf5EstimateStore, val unit: EstimateUnit, val limits: ReadLimits,
    records: Vector[Hdf5Representation], plans: Vector[Hdf5EstimateLayout]
) extends EstimateSource:
  private var closed = false
  private def capacities(values: Array[Double], validity: Array[Byte]): Either[EstimateError, (Int, Int)] =
    if values == null || validity == null then Left(EstimateError.Invalid("caller buffers required"))
    else Right(values.length -> validity.length)

  def read(product: ProductId, selection: EstimateSelection, values: Array[Double], validity: Array[Byte],
      cancelled: () => Boolean): Either[EstimateError, EstimateReadReceipt] = synchronized:
    if closed then Left(EstimateError.Closed)
    else for
      capacity <- capacities(values, validity)
      descriptor <- EstimateReadValidation.check(unit, product, selection, capacity._1, capacity._2, limits)
      _ <- readCells(product, selection.observations, selection.estimands.map(descriptor.targets.estimands.indexOf), selection.samples, values, validity, cancelled)
    yield EstimateReadReceipt(product, selection, selection.cells.toInt)

  override def readCovariance(product: ProductId, selection: CovarianceSelection, values: Array[Double], validity: Array[Byte],
      cancelled: () => Boolean): Either[EstimateError, CovarianceReadReceipt] = synchronized:
    if closed then Left(EstimateError.Closed)
    else for
      capacity <- capacities(values, validity)
      descriptor <- CovarianceReadValidation.check(unit, product, selection, capacity._1, capacity._2, limits)
      _ <- readCells(product, selection.observations, selection.pairs.map(CovarianceReadValidation.volume(descriptor, _)), selection.samples, values, validity, cancelled)
    yield CovarianceReadReceipt(product, selection, selection.cells.toInt)

  private def readCells(product: ProductId, observations: Vector[ObservationId], targets: Vector[Int], samples: Vector[Int],
      values: Array[Double], validity: Array[Byte], cancelled: () => Boolean): Either[EstimateError, Unit] =
    val plan = plans.find(_.product.id == product).get
    val ref = records.find(_.product == product).get.container
    if cancelled == null then Left(EstimateError.Invalid("cancellation callback required"))
    else store.protect:
      if cancelled() then Left(EstimateError.Cancelled)
      else if closed then Left(EstimateError.Closed)
      else store.datasets(store.root.resolve(ref.path), plan): (data, codes) =>
        var offset = 0
        var error: Option[Hdf5Error] = None
        for observation <- observations; target <- targets do
          var first = 0
          while first < samples.size && error.isEmpty do
            var count = 1
            if !plan.shared then
              while first + count < samples.size && samples(first + count) == samples(first) + count do count += 1
            val slab = plan.slab(plan.product.observations.indexOf(observation), target, if plan.shared then 0 else samples(first), if plan.shared then 1 else count)
            val block = if plan.product.precision == NumericPrecision.Float32 then Hdf5Block.Float32(new Array[Float](count))
              else Hdf5Block.Float64(new Array[Double](count))
            val mask = new Array[Byte](count)
            def cancelledOrClosed(): Boolean = cancelled() || closed
            val read = for
              _ <- data.readInto(slab, block, cancelledOrClosed)
              _ <- codes.readInto(slab, Hdf5Block.UInt8(mask), cancelledOrClosed)
            yield ()
            read match
              case Left(value) => error = Some(value)
              case Right(_) if closed => error = Some(Hdf5Error.Closed("source closed by callback"))
              case Right(_) =>
                var i = 0
                while i < count && error.isEmpty do
                  val number = block match
                    case Hdf5Block.Float32(array) => array(i).toDouble
                    case Hdf5Block.Float64(array) => array(i)
                    case _ => Double.NaN
                  val code = if plan.shared && !unit.domain.contains(samples(first + i)) then Validity.OutsideSupport.code else mask(i)
                  val diagonal = plan.product.kind == ProductKind.Covariance && plan.product.targets.estimands.exists(id => CovarianceReadValidation.volume(plan.product, EstimandPair(id, id)) == target)
                  Validity.fromCode(mask(i)) match
                    case Left(_) => error = Some(Hdf5Error.UnsupportedStored("invalid validity code"))
                    case Right(status) =>
                      if plan.shared && (status == Validity.OutsideSupport || !number.isFinite || (diagonal && number < 0)) then
                        error = Some(Hdf5Error.UnsupportedStored("invalid shared normalized covariance payload"))
                      else if !plan.shared && unit.domain.contains(samples(first + i)) == (status == Validity.OutsideSupport) then
                        error = Some(Hdf5Error.UnsupportedStored("validity disagrees with support"))
                      else if status == Validity.Valid && (!plan.product.kind.accepts(number) || (diagonal && number < 0)) then
                        error = Some(Hdf5Error.UnsupportedStored("valid value violates numeric domain"))
                  values(offset + i) = if plan.shared && !unit.domain.contains(samples(first + i)) then Double.NaN else number
                  validity(offset + i) = code
                  i += 1
            first += count
            offset += count
        error.toLeft(())

  def close(): Either[EstimateError, Unit] = synchronized:
    closed = true
    Right(())

private[hdf5] object Hdf5EstimateSource:
  /** Compare rows with bounded temporary slabs; no complete shared-U table. */
  def invariance(store: Hdf5EstimateStore, path: Path, unit: EstimateUnit, plan: Hdf5EstimateLayout): Either[EstimateError, Unit] =
    if !unit.covariance.find(_.product == plan.product.id).exists(_.invariantObservations) then Right(())
    else store.datasets(path, plan): (data, codes) =>
      var error: Option[Hdf5Error] = None
      var first = 0
      while first < plan.product.targets.width && error.isEmpty do
        val count = math.min(store.resourceLimits.maximumBlockCells, (plan.product.targets.width - first).toInt)
        val baseline = new Array[Double](count)
        val mask = new Array[Byte](count)
        def row(observation: Int, values: Array[Double], validity: Array[Byte]): Either[Hdf5Error, Unit] =
          val slab = plan.slab(observation, first, 0, count)
          data.readInto(slab, Hdf5Block.Float64(values)).flatMap(_ => codes.readInto(slab, Hdf5Block.UInt8(validity)))
        row(0, baseline, mask) match
          case Left(value) => error = Some(value)
          case Right(_) =>
            var observation = 1
            while observation < plan.product.observations.size && error.isEmpty do
              val values = new Array[Double](count)
              val validity = new Array[Byte](count)
              row(observation, values, validity) match
                case Left(value) => error = Some(value)
                case Right(_) if !values.sameElements(baseline) || !validity.sameElements(mask) =>
                  error = Some(Hdf5Error.UnsupportedStored("shared deliveries contradict observation invariance"))
                case _ => ()
              observation += 1
        first += count
      error.toLeft(())
