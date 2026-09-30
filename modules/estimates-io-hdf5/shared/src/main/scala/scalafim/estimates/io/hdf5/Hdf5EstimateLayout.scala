package scalafim.estimates.io.hdf5

import scalafim.archive.hdf5.*
import scalafim.estimates.*
import scalafim.estimates.io.Hdf5PayloadLayout

final class Hdf5EstimateLayout private (
    val product: ProductDescriptor,
    val layout: Hdf5PayloadLayout,
    val values: Hdf5DatasetInfo,
    val validity: Hdf5DatasetInfo,
    val coverageBytes: Int
):
  def cells: Long = values.extent.elements
  def shared: Boolean = layout == Hdf5PayloadLayout.SharedNormalizedUpperTriangle
  private[hdf5] def index(observation: Int, target: Int, sample: Int): Long =
    if shared then observation.toLong * product.targets.width + target
    else (observation.toLong * product.targets.width + target) * values.extent.dimensions(2) + sample
  private[hdf5] def slab(observation: Int, target: Int, sample: Int, count: Int): Hdf5Slab =
    val offset = if shared then Vector(observation.toLong, target.toLong) else Vector(observation.toLong, target.toLong, sample.toLong)
    val size = if shared then Vector(1L, count.toLong) else Vector(1L, 1L, count.toLong)
    Hdf5Slab(offset, size).toOption.get // Axis/capacity validation precedes this internal lowering.

object Hdf5EstimateLayout:
  private[hdf5] def error(e: Hdf5Error): EstimateError = e match
    case Hdf5Error.Cancelled => EstimateError.Cancelled
    case Hdf5Error.MissingCapability(_, _, _, detail) => EstimateError.Unsupported(detail)
    case Hdf5Error.AlreadyExists(path) => EstimateError.Conflict(path)
    case Hdf5Error.UnsupportedStored(detail) => EstimateError.Integrity(detail)
    case Hdf5Error.ScopeFailure(primary, cleanup) => EstimateError.Io(s"${primary.message}; cleanup: ${cleanup.map(_.message).mkString("; ")}")
    case _ => EstimateError.Io(e.message)

  def shape(dimensions: Vector[Long], precision: NumericPrecision, limits: Hdf5EstimateLimits): Either[EstimateError, (Hdf5DatasetInfo, Hdf5DatasetInfo, Int)] =
    for
      extent <- Hdf5Extent(dimensions).left.map(error)
      // Compute ceil(n/8) without n+7 overflow, before any coverage allocation.
      bytes = extent.elements / 8 + (if extent.elements % 8 == 0 then 0 else 1)
      _ <- if bytes <= limits.maximumCoverageBytes && bytes <= Int.MaxValue then Right(())
           else Left(EstimateError.Unsupported("coverage ledger byte cap exceeded"))
      chunks <- Hdf5Extent(dimensions.init.map(_ => 1L) :+ math.min(dimensions.last, math.min(256, limits.maximumBlockCells).toLong)).left.map(error)
      valueName <- Hdf5DatasetName("values").left.map(error)
      validName <- Hdf5DatasetName("validity").left.map(error)
      dtype = if precision == NumericPrecision.Float32 then Hdf5DType.Float32 else Hdf5DType.Float64
      values = Hdf5DatasetInfo(valueName, dtype, extent, chunks, Hdf5Filter.None)
      validity = Hdf5DatasetInfo(validName, Hdf5DType.UInt8, extent, chunks, Hdf5Filter.None)
      _ <- Hdf5Plan.dataset(values, limits.archive).left.map(error)
      _ <- Hdf5Plan.dataset(validity, limits.archive).left.map(error)
    yield (values, validity, bytes.toInt)

  def derive(unit: EstimateUnit, product: ProductId, compact: Boolean, limits: Hdf5EstimateLimits): Either[EstimateError, Hdf5EstimateLayout] =
    for
      _ <- if unit.inferenceEvidence.isEmpty then Right(()) else Left(EstimateError.Unsupported("HDF5 refuses inference evidence"))
      descriptor <- unit.products.find(_.id == product).toRight(EstimateError.Invalid("unknown product"))
      _ <- if compact then SharedCovarianceValidation.descriptor(unit, product).map(_ => ()) else Right(())
      dimensions = if compact then Vector(descriptor.observations.size.toLong, descriptor.targets.width)
        else Vector(descriptor.observations.size.toLong, descriptor.targets.width, unit.domain.sampleCount.toLong)
      plan <- shape(dimensions, descriptor.precision, limits)
    yield new Hdf5EstimateLayout(descriptor,
      if compact then Hdf5PayloadLayout.SharedNormalizedUpperTriangle else Hdf5PayloadLayout.PerSample,
      plan._1, plan._2, plan._3)

  private[hdf5] def plans(unit: EstimateUnit, compact: Set[ProductId], limits: Hdf5EstimateLimits): Either[EstimateError, Vector[Hdf5EstimateLayout]] =
    if unit.inferenceEvidence.nonEmpty then Left(EstimateError.Unsupported("HDF5 refuses inference evidence"))
    else if unit.products.size > limits.maximumProducts || !compact.subsetOf(unit.products.map(_.id).toSet) then
      Left(EstimateError.Unsupported("product inventory exceeds configured admission"))
    else unit.products.foldLeft[Either[EstimateError, Vector[Hdf5EstimateLayout]]](Right(Vector.empty)):
      (previous, product) => previous.flatMap: accumulated =>
        derive(unit, product.id, compact.contains(product.id), limits).flatMap: plan =>
          if accumulated.map(_.coverageBytes.toLong).sum + plan.coverageBytes > limits.maximumCoverageBytes then
            Left(EstimateError.Unsupported("aggregate coverage ledger byte cap exceeded"))
          else Right(accumulated :+ plan)
