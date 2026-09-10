package scalafim.estimates

final case class ReadLimits(maximumCells: Int):
  require(maximumCells > 0)

/** Scalar-product requests preserve caller axis order. Covariance has a separate
  * pair-axis API; it cannot masquerade as an ordinary estimand read.
  */
final case class EstimateSelection(
    observations: Vector[ObservationId],
    estimands: Vector[EstimandId],
    samples: Vector[Int]
):
  require(Invariants.unique(observations) && Invariants.unique(estimands) && Invariants.unique(samples))
  def cells: Long = observations.size.toLong * estimands.size.toLong * samples.size.toLong

/** An ordered, named covariance entry. Its orientation is checked against the
  * product's estimand axis, not lexical ID order.
  */
final case class EstimandPair(first: EstimandId, second: EstimandId)

final case class CovarianceSelection(
    observations: Vector[ObservationId],
    pairs: Vector[EstimandPair],
    samples: Vector[Int]
):
  require(Invariants.unique(observations) && Invariants.unique(pairs) && Invariants.unique(samples))
  def cells: Long = observations.size.toLong * pairs.size.toLong * samples.size.toLong

final case class CovarianceReadReceipt(product: ProductId, selection: CovarianceSelection, cells: Int)

final case class EstimateReadReceipt(product: ProductId, selection: EstimateSelection, cells: Int)

trait EstimateSource:
  def unit: EstimateUnit
  def limits: ReadLimits

  /** Fill in observation/estimand/sample order. A failed/cancelled destination
    * is unpublished scratch, including when a physical provider wrote a prefix.
    */
  def read(
      product: ProductId,
      selection: EstimateSelection,
      values: Array[Double],
      validity: Array[Byte],
      cancelled: () => Boolean = () => false
  ): Either[EstimateError, EstimateReadReceipt]

  /** Return stored covariance entries. Apply the descriptor's variance scale
    * exactly once when reconstructing absolute covariance.
    */
  def readCovariance(
      product: ProductId,
      selection: CovarianceSelection,
      values: Array[Double],
      validity: Array[Byte],
      cancelled: () => Boolean = () => false
  ): Either[EstimateError, CovarianceReadReceipt] =
    Left(EstimateError.Unsupported("source does not implement pair-axis covariance"))

  def close(): Either[EstimateError, Unit]

object EstimateReadValidation:
  def check(
      unit: EstimateUnit,
      product: ProductId,
      selection: EstimateSelection,
      valueCapacity: Int,
      validityCapacity: Int,
      limits: ReadLimits
  ): Either[EstimateError, ProductDescriptor] =
    unit.products.find(_.id == product).toRight(EstimateError.Invalid(s"unknown product ${product.value}")).flatMap: descriptor =>
      if descriptor.kind == ProductKind.Covariance then Left(EstimateError.Unsupported("covariance requires a pair-axis read"))
      else if !selection.observations.forall(descriptor.observations.contains) then Left(EstimateError.Invalid("unknown observation"))
      else if !selection.estimands.forall(descriptor.targets.estimands.contains) then Left(EstimateError.Invalid("unknown estimand"))
      else if selection.samples.exists(i => i < 0 || i >= unit.domain.sampleCount) then Left(EstimateError.Invalid("unknown sample"))
      else if selection.cells > limits.maximumCells then Left(EstimateError.Invalid("read exceeds maximum block cells"))
      else if selection.cells > valueCapacity || selection.cells > validityCapacity then Left(EstimateError.Invalid("destination capacity is too small"))
      else Right(descriptor)

object CovarianceReadValidation:
  def check(unit: EstimateUnit, product: ProductId, selection: CovarianceSelection,
      valueCapacity: Int, validityCapacity: Int, limits: ReadLimits): Either[EstimateError, ProductDescriptor] =
    unit.products.find(_.id == product).toRight(EstimateError.Invalid(s"unknown product ${product.value}")).flatMap: descriptor =>
      if descriptor.kind != ProductKind.Covariance then Left(EstimateError.Invalid("pair-axis access requires covariance"))
      else if !selection.observations.forall(descriptor.observations.contains) then Left(EstimateError.Invalid("unknown observation"))
      else if selection.pairs.exists(pair => volume(descriptor, pair) < 0) then Left(EstimateError.Invalid("unknown or reversed covariance pair"))
      else if selection.samples.exists(i => i < 0 || i >= unit.domain.sampleCount) then Left(EstimateError.Invalid("unknown sample"))
      else if selection.cells > limits.maximumCells then Left(EstimateError.Invalid("read exceeds maximum block cells"))
      else if selection.cells > valueCapacity || selection.cells > validityCapacity then Left(EstimateError.Invalid("destination capacity is too small"))
      else Right(descriptor)

  /** Upper-triangle row-major ordinal, computed without materializing all pairs. */
  def volume(product: ProductDescriptor, pair: EstimandPair): Int =
    val axis = product.targets.estimands
    val i = axis.indexOf(pair.first)
    val j = axis.indexOf(pair.second)
    if i < 0 || j < i then -1
    else (i.toLong * axis.size - i.toLong * (i - 1) / 2 + j - i).toInt

/** Implementations inspect without loading products, then verify immutable
  * manifests and required payloads before returning an owned analysis source.
  */
trait EstimateSetReader:
  def inspect(reference: PinnedUnit): Either[EstimateError, EstimateUnit]
  def open(reference: PinnedUnit, limits: ReadLimits): Either[EstimateError, EstimateSource]
