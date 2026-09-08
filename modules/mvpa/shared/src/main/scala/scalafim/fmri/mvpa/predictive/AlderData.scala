package scalafim.fmri.mvpa.predictive

import alder.data.CoordinateError
import alder.data.CoordinateWriter
import alder.data.FeatureSchema
import alder.data.FeatureView
import alder.data.InMemoryData
import alder.data.Schema
import alder.data.SchemaError
import alder.kernel.Data
import alder.kernel.DataFingerprint
import alder.kernel.Example
import alder.kernel.FingerprintPolicy
import alder.kernel.RowId
import alder.kernel.SchemaFingerprint
import alder.kernel.Use
import multivar.core.SemanticSpace
import multivar.core.OperatorRepresentation
import resample4s.core.Reindexing
import scalafim.fmri.mvpa.*

import scala.collection.mutable
import scala.reflect.ClassTag

enum AlderDataError:
  case Evidence(error: EvidenceTableError)
  case Target(error: ColumnError)
  case Metadata(error: ColumnError)
  case FeatureSchema(error: SchemaError)
  case RowIdOutOfRange(value: Long, size: Int)
  case DuplicateRowId(value: Long)
  case MissingRow(sample: SampleId)
  case InvalidSampleAxisPurpose(actual: AxisPurpose)
  case PatternAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case PatternDimensionMismatch(expected: Int, actual: Int)
  case CoordinateOutOfBounds(coordinate: Int, size: Int)

  def message: String =
    this match
      case Evidence(error)              => error.message
      case Target(error)                => s"target column: ${error.message}"
      case Metadata(error)              => s"metadata column: ${error.message}"
      case FeatureSchema(error)         => s"Alder feature schema is invalid: $error"
      case RowIdOutOfRange(value, size) =>
        s"Alder RowId $value is outside the execution ledger [0, $size)"
      case DuplicateRowId(value) =>
        s"Alder output contains duplicate RowId $value"
      case MissingRow(sample) =>
        s"Alder output is missing sample '${sample.value}'"
      case InvalidSampleAxisPurpose(actual) =>
        s"Alder row ledger requires a sample axis, obtained '${actual.value}'"
      case PatternAxisMismatch(expected, actual) =>
        s"matrix row belongs to local axis ${actual.value}, expected ${expected.value}"
      case PatternDimensionMismatch(expected, actual) =>
        s"matrix row contains $actual coordinates, expected $expected"
      case CoordinateOutOfBounds(coordinate, size) =>
        s"pattern coordinate $coordinate is outside [0, $size)"

/** Total correspondence between Alder's local execution ordinals and the authoritative, ordered ScalaFIM sample axis.
  *
  * Alder deliberately owns `RowId` construction. The ledger consumes those values after Alder emits them; it never
  * promotes a bare ordinal into a scientific sample identity.
  */
final class AlderRowLedger[S <: SemanticSpace] private (
    val samples: AxisRef.Aux[SampleId, S]
):
  def size: Int = samples.size

  def sampleAt(rowId: RowId): Either[AlderDataError, SampleId] =
    position(rowId).map(samples.keys)

  def executionOrdinal(sample: SampleId): Option[Long] =
    samples.positionOf(sample).map(_.toLong)

  /** Validate Alder-emitted row identities and restore scientific source order. Subsets are allowed unless
    * `requireComplete` is true.
    */
  def reconstruct[U <: Use, A](
      data: Data[U, A],
      requireComplete: Boolean
  ): Either[AlderDataError, Vector[(SampleId, A)]] =
    val observed = mutable.HashMap.empty[Int, A]
    var failure: Option[AlderDataError] = None
    data.foreachRow: (rowId, value) =>
      if failure.isEmpty then
        position(rowId) match
          case Left(error)    => failure = Some(error)
          case Right(ordinal) =>
            if observed.contains(ordinal) then failure = Some(AlderDataError.DuplicateRowId(rowId.value))
            else observed.update(ordinal, value)

    failure match
      case Some(error) => Left(error)
      case None        =>
        val restored = Vector.newBuilder[(SampleId, A)]
        var ordinal = 0
        while ordinal < size do
          observed.get(ordinal) match
            case Some(value)             => restored += samples.keys(ordinal) -> value
            case None if requireComplete =>
              return Left(AlderDataError.MissingRow(samples.keys(ordinal)))
            case None => ()
          ordinal += 1
        Right(restored.result())

  private def position(rowId: RowId): Either[AlderDataError, Int] =
    val value = rowId.value
    if value < 0L || value >= size.toLong then Left(AlderDataError.RowIdOutOfRange(value, size))
    else Right(value.toInt)

object AlderRowLedger:
  def apply[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S]
  ): Either[AlderDataError, AlderRowLedger[S]] =
    if samples.identity.purpose != AxisPurpose.Samples then
      Left(AlderDataError.InvalidSampleAxisPurpose(samples.identity.purpose))
    else Right(new AlderRowLedger(samples))

/** Zero-copy view of one row in the explicitly materialized local pattern matrix. The view retains the nominal
  * local-space type and runtime axis fingerprint.
  */
final class AlderPatternRow[Local <: SemanticSpace] private[predictive] (
    private[predictive] val matrix: gale.linalg.DMat,
    private[predictive] val ordinal: Int,
    val localIdentity: AxisFingerprint
):
  def size: Int = matrix.cols

  def at(coordinate: Int): Either[AlderDataError, Double] =
    if coordinate < 0 || coordinate >= size then Left(AlderDataError.CoordinateOutOfBounds(coordinate, size))
    else Right(coordinateUnsafe(coordinate))

  private[predictive] def coordinateUnsafe(coordinate: Int): Double =
    matrix(ordinal, coordinate)

/** Alder capabilities for one exact local neural axis. They are values rather than global givens because two
  * same-shaped measurements remain distinct.
  */
final class AlderPatternCapabilities[Local <: SemanticSpace] private (
    val localIdentity: AxisIdentity,
    val featureView: FeatureView[AlderPatternRow[Local]],
    val schema: Schema[AlderPatternRow[Local]]
)

object AlderPatternCapabilities:
  private val Protocol = "scalafim-mvpa-alder-pattern-row/v1"

  private[predictive] def apply[K, Local <: SemanticSpace](
      local: AxisRef.Aux[K, Local]
  ): Either[AlderDataError, AlderPatternCapabilities[Local]] =
    val names = IArray.unsafeFromArray(
      local.identity.orderedKeys.map(_.value).toArray
    )
    FeatureSchema
      .named[AlderPatternRow[Local]](names)
      .left
      .map(AlderDataError.FeatureSchema.apply)
      .map: alderSchema =>
        val view = new FeatureView[AlderPatternRow[Local]]:
          override val names: IArray[String] = alderSchema.names
          override val size: Int = alderSchema.size
          override val featureSchema: FeatureSchema[?] = alderSchema

          override def read(
              value: AlderPatternRow[Local]
          ): Either[CoordinateError, IArray[Double]] =
            validate(value).map: _ =>
              IArray.tabulate(size)(value.coordinateUnsafe)

          override def writeTo(
              value: AlderPatternRow[Local],
              destination: CoordinateWriter
          ): Either[CoordinateError, Unit] =
            validate(value).flatMap: _ =>
              if destination.size != size then Left(CoordinateError.DestinationArityMismatch(size, destination.size))
              else
                var coordinate = 0
                var failure: Option[CoordinateError] = None
                while coordinate < size && failure.isEmpty do
                  destination.write(
                    coordinate,
                    names(coordinate),
                    value.coordinateUnsafe(coordinate)
                  ) match
                    case Left(error) => failure = Some(error)
                    case Right(_)    => ()
                  coordinate += 1
                failure.toLeft(())

          private def validate(
              value: AlderPatternRow[Local]
          ): Either[CoordinateError, Unit] =
            if value.localIdentity != local.identity.fingerprint then
              Left(
                CoordinateError.WriterRejected(
                  0,
                  names(0),
                  AlderDataError
                    .PatternAxisMismatch(
                      local.identity.fingerprint,
                      value.localIdentity
                    )
                    .message
                )
              )
            else if value.size != size then Left(CoordinateError.ArityMismatch(size, value.size))
            else Right(())

        val writer = CanonicalWriter()
        writer.string(Protocol)
        writer.string(local.identity.fingerprint.value)
        val digest = AxisDigest.sha256Hex(writer.result())
        val rowSchema = new Schema[AlderPatternRow[Local]]:
          override val descriptor: String =
            s"$Protocol:${local.identity.fingerprint.value}"
          override val fingerprint: SchemaFingerprint =
            new SchemaFingerprint(
              FingerprintPolicy.ContentDigest("sha256"),
              digest
            )
        new AlderPatternCapabilities(local.identity, view, rowSchema)

final class AlderRestrictionWork private[predictive] (
    val sourceRows: Int,
    val selectedRows: Int,
    val mappingEntries: Long
)

final class AlderMeasurementWork private[predictive] (
    val sourceFeatures: Int,
    val localFeatures: Int,
    val compositions: Long,
    val sourceRepresentation: OperatorRepresentation,
    val measuredRepresentation: OperatorRepresentation
)

final class AlderLearnerInputWork private[predictive] (
    val rows: Int,
    val features: Int,
    val rowViewsAllocated: Long,
    val exampleRecordsAllocated: Long,
    val coordinateArraysAllocatedAtBoundary: Long,
    val coordinateArraysPerFeatureViewRead: Int,
    val coordinateArraysPerFeatureViewWrite: Int
)

/** Receipt for the entire ScalaFIM-to-Alder boundary. Each stage is distinct: sparse row restriction, measurement
  * composition, numerical materialization, and learner-facing row views cannot be conflated.
  */
final class AlderInputReceipt private[predictive] (
    val plan: ScientificPlanFingerprint,
    val restriction: AlderRestrictionWork,
    val measurement: AlderMeasurementWork,
    val materialization: MaterializationReceipt,
    val learnerInput: AlderLearnerInputWork,
    val dataFingerprint: DataFingerprint
)

final class AlderPreparedData[
    Samples <: SemanticSpace,
    Local <: SemanticSpace,
    Y,
    M
] private[predictive] (
    val ledger: AlderRowLedger[Samples],
    val capabilities: AlderPatternCapabilities[Local],
    val data: Data[
      Use.Unsplit,
      Example[AlderPatternRow[Local], Y, M]
    ],
    val receipt: AlderInputReceipt
)

object AlderDataBoundary:
  private val Protocol = "scalafim-mvpa-alder-input/v1"

  /** Measure and expose the complete authoritative sample population. This is the predictive cross-validation boundary:
    * Alder's `CompleteResampler` owns all fold restriction, so an identity reindexing must not create a second
    * scientific sample space merely to satisfy the data adapter.
    */
  def prepareAll[
      Samples <: SemanticSpace,
      Neural <: SemanticSpace,
      NeuralKey,
      LocalKey,
      Y,
      M
  ](
      plan: ScientificPlanFingerprint,
      evidence: EvidenceTable[Samples, Neural, SampleId, NeuralKey],
      target: Column[Samples, Y],
      metadata: Column[Samples, M],
      measurement: Measurement[Neural, NeuralKey, LocalKey],
      materialization: MaterializationPolicy
  ): Either[
    AlderDataError,
    AlderPreparedData[Samples, measurement.local.Id, Y, M]
  ] =
    for
      _ <- validateAligned(evidence.rows, target, AlderDataError.Target.apply)
      _ <- validateAligned(evidence.rows, metadata, AlderDataError.Metadata.apply)
      prepared <- prepareAligned(
        plan,
        evidence,
        target,
        metadata,
        measurement,
        materialization,
        new AlderRestrictionWork(
          evidence.rowCount,
          evidence.rowCount,
          0L
        )
      )
    yield prepared

  /** Restrict, measure, explicitly materialize, and expose local patterns to Alder. No dense conversion or row copy can
    * occur before the supplied materialization policy admits it.
    */
  def prepare[
      Samples <: SemanticSpace,
      Neural <: SemanticSpace,
      NeuralKey,
      LocalKey,
      Y: ClassTag,
      M: ClassTag,
      R <: Reindexing
  ](
      plan: ScientificPlanFingerprint,
      evidence: EvidenceTable[Samples, Neural, SampleId, NeuralKey],
      target: Column[Samples, Y],
      metadata: Column[Samples, M],
      restriction: ReindexingLeg[
        Samples,
        SampleId,
        SampleId,
        R
      ],
      measurement: Measurement[Neural, NeuralKey, LocalKey],
      materialization: MaterializationPolicy
  ): Either[
    AlderDataError,
    AlderPreparedData[
      restriction.child.Id,
      measurement.local.Id,
      Y,
      M
    ]
  ] =
    for
      restrictedEvidence <- evidence
        .restrictRows(restriction)
        .left
        .map(AlderDataError.Evidence.apply)
      restrictedTarget <- target
        .reindex(restriction)
        .left
        .map(AlderDataError.Target.apply)
      restrictedMetadata <- metadata
        .reindex(restriction)
        .left
        .map(AlderDataError.Metadata.apply)
      prepared <- prepareAligned(
        plan,
        restrictedEvidence,
        restrictedTarget,
        restrictedMetadata,
        measurement,
        materialization,
        new AlderRestrictionWork(
          evidence.rowCount,
          restriction.size,
          restriction.size.toLong
        )
      )
    yield prepared

  private def prepareAligned[
      Samples <: SemanticSpace,
      Neural <: SemanticSpace,
      NeuralKey,
      LocalKey,
      Y,
      M
  ](
      plan: ScientificPlanFingerprint,
      evidence: EvidenceTable[Samples, Neural, SampleId, NeuralKey],
      target: Column[Samples, Y],
      metadata: Column[Samples, M],
      measurement: Measurement[Neural, NeuralKey, LocalKey],
      materialization: MaterializationPolicy,
      restrictionWork: AlderRestrictionWork
  ): Either[
    AlderDataError,
    AlderPreparedData[Samples, measurement.local.Id, Y, M]
  ] =
    for
      measured <- evidence
        .measureColumns(measurement)
        .left
        .map(AlderDataError.Evidence.apply)
      dense <- measured
        .materialize(materialization)
        .left
        .map(AlderDataError.Evidence.apply)
      capabilities <- AlderPatternCapabilities(measurement.local)
      ledger <- AlderRowLedger(evidence.rows)
    yield
      val fingerprint = inputFingerprint(
        plan,
        evidence.rows.identity,
        measurement.identity
      )
      val rows = Vector.tabulate(ledger.size): ordinal =>
        Example(
          new AlderPatternRow[measurement.local.Id](
            dense.value,
            ordinal,
            measurement.local.identity.fingerprint
          ),
          target.values(ordinal),
          metadata.values(ordinal)
        )
      val alderData = InMemoryData.unsplit(rows, fingerprint)
      val receipt = new AlderInputReceipt(
        plan,
        restrictionWork,
        new AlderMeasurementWork(
          evidence.columnCount,
          measurement.local.size,
          1L,
          evidence.representation,
          measured.representation
        ),
        dense.receipt,
        new AlderLearnerInputWork(
          ledger.size,
          measurement.local.size,
          ledger.size.toLong,
          ledger.size.toLong,
          0L,
          1,
          0
        ),
        fingerprint
      )
      new AlderPreparedData(
        ledger,
        capabilities,
        alderData,
        receipt
      )

  private def validateAligned[S <: SemanticSpace, A](
      samples: AxisRef.Aux[SampleId, S],
      column: Column[S, A],
      wrap: ColumnError => AlderDataError
  ): Either[AlderDataError, Unit] =
    if column.rowIdentity != samples.identity then
      Left(
        wrap(
          ColumnError.OwnerMismatch(
            samples.identity.fingerprint,
            column.rowIdentity.fingerprint
          )
        )
      )
    else if !(column.rows eq samples.evidence) then Left(wrap(ColumnError.NominalWitnessMismatch))
    else Right(())

  private def inputFingerprint(
      plan: ScientificPlanFingerprint,
      samples: AxisIdentity,
      measurement: MeasurementIdentity
  ): DataFingerprint =
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.string(plan.value)
    writer.string(samples.fingerprint.value)
    writer.string(measurement.fingerprint.value)
    new DataFingerprint(
      FingerprintPolicy.Summary(Protocol),
      AxisDigest.sha256Hex(writer.result())
    )
