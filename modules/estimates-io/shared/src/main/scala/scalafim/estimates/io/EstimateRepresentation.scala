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
  case Hdf5(value: Hdf5Representation)

  def product: ProductId = this match
    case Nifti(value) => value.product
    case SharedNormalizedUpperTriangle(value) => value.product
    case Hdf5(value) => value.product
  def observation: ObservationId = this match
    case Nifti(value) => value.observation
    case SharedNormalizedUpperTriangle(value) => value.observation
    case Hdf5(value) => value.observation

enum Hdf5PayloadLayout:
  case PerSample, SharedNormalizedUpperTriangle

/** A closed container declaration, not evidence of physical dataset conformance.
  * Dataset axes and precision derive from the unchanged scientific descriptor.
  */
final case class Hdf5Representation(
    product: ProductId,
    observation: ObservationId,
    container: FileReference,
    layout: Hdf5PayloadLayout
):
  require(container.path.endsWith(".h5"))

  def validate(unit: EstimateUnit): Either[EstimateError, Unit] =
    val descriptor = layout match
      case Hdf5PayloadLayout.PerSample =>
        unit.products.find(_.id == product).toRight(EstimateError.Invalid("unknown HDF5 product"))
      case Hdf5PayloadLayout.SharedNormalizedUpperTriangle => SharedCovarianceValidation.descriptor(unit, product)
    descriptor.flatMap: value =>
      if value.observations.contains(observation) then Right(())
      else Left(EstimateError.Integrity("HDF5 observation differs from the named product axis"))

object Hdf5Representation:
  def validateInventory(unit: EstimateUnit, records: Vector[Hdf5Representation]): Either[EstimateError, Unit] =
    if unit.inferenceEvidence.nonEmpty then return Left(EstimateError.Unsupported("Core-HDF5-1 refuses inference evidence"))
    validatePairs(unit.products, records).flatMap: _ =>
      records.foldLeft[Either[EstimateError, Unit]](Right(()))((previous, record) => previous.flatMap(_ => record.validate(unit)))

  private[io] def validatePairs(products: Vector[ProductDescriptor], records: Vector[Hdf5Representation]): Either[EstimateError, Unit] =
    val expected = products.flatMap(p => p.observations.map(o => p.id -> o)).toSet
    val actual = records.map(r => r.product -> r.observation)
    if actual.distinct.size != actual.size || actual.toSet != expected then
      Left(EstimateError.Integrity("HDF5 inventory must exactly cover declared product/observation pairs"))
    else if records.exists(_.container.bytes > 9007199254740991L) then
      Left(EstimateError.Unsupported("HDF5 metadata requires exact JSON byte counts through 2^53 - 1"))
    else if records.groupBy(_.product).values.exists(rows => rows.map(r => r.container -> r.layout).distinct.size != 1) then
      Left(EstimateError.Integrity("one HDF5 product must name one identical container and layout"))
    else if records.groupBy(_.container.path).values.exists(rows => rows.map(_.product).distinct.size != 1) then
      Left(EstimateError.Integrity("different HDF5 products must use distinct container paths"))
    else Right(())

final case class SharedCovarianceLimits(maximumPairs: Int = 4096, maximumTableBytes: Long = 1024L * 1024L):
  require(maximumPairs > 0 && maximumTableBytes > 0)

/** Core-1 remains the default. Compact output is an explicit unit layout choice. */
enum CovarianceLayout:
  case PairNifti
  case SharedNormalizedTable(limits: SharedCovarianceLimits = SharedCovarianceLimits())
