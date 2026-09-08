package scalafim.fmri.mvpa

import scalafim.fmri.fit.FitError
import scalafim.fmri.fit.TrainingRunScope

/** Fail-closed errors at the fMRI response/readout to identified-evidence boundary. Numerical estimands keep their own
  * error types; this enum is the single error model for constructing observation and relation evidence.
  */
enum FmriEvidenceError:
  case EmptyRuns
  case InsufficientPartitions(actual: Int)
  case DuplicatePartition(partition: PartitionId)
  case UnknownPartition(partition: PartitionId)
  case PartitionCountMismatch(expected: Int, actual: Int)
  case PartitionKeyMismatch(position: Int, expected: PartitionId, actual: PartitionId)
  case TimepointMismatch(partition: PartitionId, expected: Int, actual: Int)
  case NeuralCountMismatch(partition: PartitionId, expected: Int, actual: Int)
  case NeuralAxisMismatch(
      partition: PartitionId,
      expected: AxisFingerprint,
      actual: AxisFingerprint
  )
  case NeuralWitnessMismatch(partition: PartitionId)
  case EffectCountMismatch(expected: Int, actual: Int)
  case ReadoutEffectCountMismatch(partition: PartitionId, expected: Int, actual: Int)
  case EffectKeyMismatch(
      partition: PartitionId,
      position: Int,
      expected: String,
      actual: String
  )
  case SampleCountMismatch(partition: PartitionId, expected: Int, actual: Int)
  case SampleKeyMismatch(
      partition: PartitionId,
      position: Int,
      expected: String,
      actual: String
  )
  case DuplicateSample(sample: SampleId)
  case MissingResponseRevision(partition: PartitionId)
  case InvalidGeometrySchedule(detail: String)
  case MissingFoldGeometry(partition: PartitionId, training: TrainingRunScope)
  case TemporalPreparationFailure(partition: PartitionId, cause: FitError)
  case CompositionFailure(partition: PartitionId, detail: String)
  case Axis(error: AxisRefError)
  case Identity(error: ScientificIdentityError)
  case Evidence(error: EvidenceTableError)
  case Observations(error: ObservationsError)
  case Column(error: ColumnError)
  case Relation(error: RelationError)
  case Schedule(error: CrossValidatedRelationError)

  def message: String =
    this match
      case EmptyRuns =>
        "fMRI evidence requires at least one run"
      case InsufficientPartitions(actual) =>
        s"cross-validated fMRI evidence requires at least two partitions, obtained $actual"
      case DuplicatePartition(partition) =>
        s"partition '${partition.value}' occurs more than once"
      case UnknownPartition(partition) =>
        s"fMRI evidence references unknown partition '${partition.value}'"
      case PartitionCountMismatch(expected, actual) =>
        s"fMRI evidence expected $expected partitions, obtained $actual"
      case PartitionKeyMismatch(position, expected, actual) =>
        s"fMRI evidence partition $position is '${actual.value}', expected '${expected.value}'"
      case TimepointMismatch(partition, expected, actual) =>
        s"partition '${partition.value}' expects $expected timepoints, obtained $actual"
      case NeuralCountMismatch(partition, expected, actual) =>
        s"partition '${partition.value}' neural axis contains $expected coordinates, response contains $actual"
      case NeuralAxisMismatch(partition, expected, actual) =>
        s"partition '${partition.value}' neural axis ${actual.value} does not match ${expected.value}"
      case NeuralWitnessMismatch(partition) =>
        s"partition '${partition.value}' uses a foreign nominal neural witness"
      case EffectCountMismatch(expected, actual) =>
        s"effect axis contains $actual coordinates, expected $expected"
      case ReadoutEffectCountMismatch(partition, expected, actual) =>
        s"partition '${partition.value}' readout contains $actual effects, expected $expected"
      case EffectKeyMismatch(partition, position, expected, actual) =>
        s"partition '${partition.value}' effect $position is '$actual', expected '$expected'"
      case SampleCountMismatch(partition, expected, actual) =>
        s"partition '${partition.value}' sample axis contains $actual rows, expected $expected"
      case SampleKeyMismatch(partition, position, expected, actual) =>
        s"partition '${partition.value}' sample $position is '$actual', expected '$expected'"
      case DuplicateSample(sample) =>
        s"sample '${sample.value}' occurs in more than one run"
      case MissingResponseRevision(partition) =>
        s"partition '${partition.value}' has no response revision"
      case InvalidGeometrySchedule(detail) =>
        s"invalid temporal geometry schedule: $detail"
      case MissingFoldGeometry(partition, training) =>
        s"partition '${partition.value}' has no prepared geometry for training runs ${training.runs.map(_.value).mkString("[", ", ", "]")}"
      case TemporalPreparationFailure(partition, cause) =>
        s"partition '${partition.value}' temporal preparation failed: ${cause.message}"
      case CompositionFailure(partition, detail) =>
        s"partition '${partition.value}' readout composition failed: $detail"
      case Axis(error)         => error.message
      case Identity(error)     => error.message
      case Evidence(error)     => error.message
      case Observations(error) => error.message
      case Column(error)       => error.message
      case Relation(error)     => error.message
      case Schedule(error)     => error.message
