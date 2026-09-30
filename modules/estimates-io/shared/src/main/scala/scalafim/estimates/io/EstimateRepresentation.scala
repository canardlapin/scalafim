package scalafim.estimates.io

import scalafim.estimates.*

enum SharedValidityBroadcast:
  case SupportedSamples

final case class SharedCovarianceRepresentation(
    product: ProductId,
    observation: ObservationId,
    table: FileReference,
    estimands: Vector[EstimandId],
    precision: NumericPrecision = NumericPrecision.Float64,
    validityBroadcast: SharedValidityBroadcast = SharedValidityBroadcast.SupportedSamples
):
  require(estimands.nonEmpty && estimands.distinct.size == estimands.size)
  require(precision == NumericPrecision.Float64)
  def pairCount: Long = estimands.size.toLong * (estimands.size.toLong + 1L) / 2L
  def validate(unit: EstimateUnit): Either[EstimateError, Unit] =
    SharedCovarianceValidation.descriptor(unit, product).flatMap: descriptor =>
      if descriptor.observations.contains(observation) && descriptor.targets.estimands == estimands then Right(())
      else Left(EstimateError.Integrity("shared covariance representation differs from the named scientific axes"))

enum EstimateRepresentation:
  case Nifti(value: NiftiRepresentation)
  case SharedNormalizedUpperTriangle(value: SharedCovarianceRepresentation)

  def product: ProductId = this match
    case Nifti(value) => value.product
    case SharedNormalizedUpperTriangle(value) => value.product
  def observation: ObservationId = this match
    case Nifti(value) => value.observation
    case SharedNormalizedUpperTriangle(value) => value.observation

final case class SharedCovarianceLimits(maximumPairs: Int = 4096, maximumTableBytes: Long = 1024L * 1024L):
  require(maximumPairs > 0 && maximumTableBytes > 0)

/** Core-1 remains the default. Compact output is an explicit unit layout choice. */
enum CovarianceLayout:
  case PairNifti
  case SharedNormalizedTable(limits: SharedCovarianceLimits = SharedCovarianceLimits())
