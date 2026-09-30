package scalafim.estimates

/** Delivery arrays are borrowed only for the duration of write. Successful
  * write is not publication. Seal must verify declared coverage and close all
  * payloads before returning an immutable, digest-pinned unit reference.
  */
trait EstimateSink:
  def unit: EstimateUnit
  def maximumBlockCells: Int
  def supportsCovariance: Boolean = false
  def write(
      product: ProductId,
      selection: EstimateSelection,
      values: Array[Double],
      validity: Array[Byte]
  ): Either[EstimateError, Unit]
  def writeCovariance(
      product: ProductId,
      selection: CovarianceSelection,
      values: Array[Double],
      validity: Array[Byte]
  ): Either[EstimateError, Unit] =
    Left(EstimateError.Unsupported("sink does not implement pair-axis covariance"))
  def seal(): Either[EstimateError, PinnedUnit]
  def abort(): Either[EstimateError, Unit]

/** Invariant delivery has no sample axis. Values remain normalized covariance,
  * with validity broadcast only inside the unit's separately declared support.
  */
final case class SharedCovarianceSelection(observations: Vector[ObservationId], pairs: Vector[EstimandPair]):
  require(Invariants.unique(observations) && Invariants.unique(pairs))
  def cells: Long = observations.size.toLong * pairs.size.toLong

trait SharedCovarianceSink extends EstimateSink:
  def sharedCovarianceProducts: Set[ProductId]
  def writeSharedCovariance(product: ProductId, selection: SharedCovarianceSelection,
      values: Array[Double], validity: Array[Byte]): Either[EstimateError, Unit]

object SharedCovarianceValidation:
  def descriptor(unit: EstimateUnit, product: ProductId): Either[EstimateError, ProductDescriptor] =
    for
      descriptor <- unit.products.find(_.id == product).toRight(EstimateError.Invalid("unknown shared covariance product"))
      covariance <- unit.covariance.find(_.product == product).toRight(EstimateError.Invalid("shared delivery requires declared covariance"))
      _ <- if descriptor.kind == ProductKind.Covariance && descriptor.precision == NumericPrecision.Float64 &&
          covariance.invariantSamples && covariance.equation.isInstanceOf[CovarianceEquation.Normalized] then Right(())
        else Left(EstimateError.Unsupported("shared delivery requires sample-invariant normalized Float64 covariance"))
    yield descriptor

  def check(unit: EstimateUnit, product: ProductId, selection: SharedCovarianceSelection,
      valueCapacity: Int, validityCapacity: Int, maximumCells: Int): Either[EstimateError, ProductDescriptor] =
    descriptor(unit, product).flatMap: descriptor =>
      if !selection.observations.forall(descriptor.observations.contains) then Left(EstimateError.Invalid("unknown observation"))
      else if selection.pairs.exists(CovarianceReadValidation.volume(descriptor, _) < 0) then Left(EstimateError.Invalid("unknown or reversed covariance pair"))
      else if selection.cells > maximumCells then Left(EstimateError.Invalid("shared delivery exceeds maximum block cells"))
      else if selection.cells > valueCapacity || selection.cells > validityCapacity then Left(EstimateError.Invalid("destination capacity is too small"))
      else Right(descriptor)
