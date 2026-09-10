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
