package scalafim.fmri.mvpa

import gale.linalg.CholeskyOptions
import gale.linalg.DMat
import gale.linalg.Matrix
import multivar.core.SemanticSpace
import multivar.core.ValueId
import multivar.core.ValueIdentity

opaque type PrecisionTolerance = Double

object PrecisionTolerance:
  def apply(value: Double): Either[DistanceEstimandError, PrecisionTolerance] =
    if !value.isFinite || value <= 0.0 then Left(DistanceEstimandError.InvalidPrecisionTolerance(value))
    else Right(value)

  private[mvpa] def unsafe(value: Double): PrecisionTolerance =
    value

  extension (value: PrecisionTolerance) inline def toDouble: Double = value

enum DistanceEstimandError:
  case Identity(error: ScientificIdentityError)
  case Evidence(error: EvidenceTableError)
  case Relation(error: RelationError)
  case Query(error: RelationalQueryError)
  case Linear(detail: String)
  case InvalidPrecisionTolerance(value: Double)
  case PrecisionShapeMismatch(expected: Int, actualRows: Int, actualColumns: Int)
  case AsymmetricPrecision(row: Int, column: Int, left: Double, right: Double)
  case NonPositiveDefinitePrecision(detail: String)
  case Independence(error: PairingDesignError)
  case UnsupportedPrecisionMeasurement(kind: MeasurementKind)
  case PrecisionAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case PrecisionWitnessMismatch
  case InvalidPooledPrecision(detail: String)

  def message: String =
    this match
      case Identity(error)                  => error.message
      case Evidence(error)                  => error.message
      case Relation(error)                  => error.message
      case Query(error)                     => error.message
      case Linear(detail)                   => detail
      case InvalidPrecisionTolerance(value) =>
        s"precision tolerance must be finite and positive, obtained $value"
      case PrecisionShapeMismatch(expected, actualRows, actualColumns) =>
        s"precision certificate expected ${expected}x$expected matrix, obtained ${actualRows}x$actualColumns"
      case AsymmetricPrecision(row, column, left, right) =>
        s"precision is asymmetric at ($row,$column): $left versus $right"
      case NonPositiveDefinitePrecision(detail) =>
        s"precision is not certified positive definite: $detail"
      case Independence(error)                   => error.message
      case UnsupportedPrecisionMeasurement(kind) =>
        s"relation precision can currently be localized only through an identity or hard selection, obtained '${kind.label}'"
      case PrecisionAxisMismatch(expected, actual) =>
        s"precision axis ${actual.value} does not match ${expected.value}"
      case PrecisionWitnessMismatch =>
        "precision certificate and evidence use different nominal witnesses"
      case InvalidPooledPrecision(detail) =>
        s"invalid pooled precision: $detail"

/** Executable certificate tied to the exact precision value and neural axis. */
final class NoisePrecisionCertificate[
    N <: SemanticSpace,
    K
] private (
    val neural: AxisRef.Aux[K, N],
    val valueIdentity: ValueIdentity,
    val contentDigest: String,
    val tolerance: PrecisionTolerance,
    val identity: ScientificComponentFingerprint
)

object NoisePrecisionCertificate:
  private val Kind = EstimandKind.unsafe("noise-precision-certificate")

  private[mvpa] def verify[K](
      neural: AxisRef[K],
      precision: DMat,
      valueIdentity: ValueIdentity,
      tolerance: PrecisionTolerance = PrecisionTolerance.unsafe(1e-10)
  ): Either[
    DistanceEstimandError,
    NoisePrecisionCertificate[neural.Id, K]
  ] =
    if precision.rows != neural.size || precision.cols != neural.size then
      Left(
        DistanceEstimandError.PrecisionShapeMismatch(
          neural.size,
          precision.rows,
          precision.cols
        )
      )
    else
      for
        _ <- EvidenceTable
          .validateFinite(precision, "noise precision")
          .left
          .map(DistanceEstimandError.Evidence.apply)
        _ <- validateSymmetry(precision, tolerance)
        _ <- precision
          .cholesky(CholeskyOptions(tolerance.toDouble))
          .left
          .map(error => DistanceEstimandError.NonPositiveDefinitePrecision(error.getMessage))
        component <- EstimandIdentity(
          Kind,
          Vector(
            "contents" -> matrixDigest(precision),
            "method" -> "symmetric-positive-definite-cholesky",
            "neural" -> neural.identity.fingerprint.value,
            "tolerance" -> tolerance.toDouble.toString,
            "value" -> valueIdentity.stableKey
          )
        ).left.map(DistanceEstimandError.Identity.apply)
      yield new NoisePrecisionCertificate(
        neural,
        valueIdentity,
        matrixDigest(precision),
        tolerance,
        component.fingerprint
      )

  private def matrixDigest(precision: DMat): String =
    val writer = CanonicalWriter()
    writer.string("scalafim-mvpa-noise-precision/v1")
    writer.int(precision.rows)
    writer.int(precision.cols)
    var row = 0
    while row < precision.rows do
      var column = 0
      while column < precision.cols do
        writer.double(precision(row, column))
        column += 1
      row += 1
    AxisDigest.sha256Hex(writer.result())

  private def validateSymmetry(
      precision: DMat,
      tolerance: PrecisionTolerance
  ): Either[DistanceEstimandError, Unit] =
    var row = 0
    while row < precision.rows do
      var column = row + 1
      while column < precision.cols do
        val left = precision(row, column)
        val right = precision(column, row)
        val scale = math.max(1.0, math.max(math.abs(left), math.abs(right)))
        if math.abs(left - right) > tolerance.toDouble * scale then
          return Left(
            DistanceEstimandError.AsymmetricPrecision(
              row,
              column,
              left,
              right
            )
          )
        column += 1
      row += 1
    Right(())

/** A precision table and the executable certificate obtained by evaluating that exact table. The private constructor
  * prevents a certificate from one table being paired with another table that reuses its [[ValueIdentity]].
  */
final class CertifiedNoisePrecision[
    N <: SemanticSpace,
    K
] private (
    val table: EvidenceTable[N, N, K, K],
    val certificate: NoisePrecisionCertificate[N, K]
):
  def neural: AxisRef.Aux[K, N] = table.rows

object CertifiedNoisePrecision:
  def apply[N <: SemanticSpace, K](
      table: EvidenceTable[N, N, K, K],
      tolerance: PrecisionTolerance = PrecisionTolerance.unsafe(1e-10)
  ): Either[DistanceEstimandError, CertifiedNoisePrecision[N, K]] =
    if table.rows.identity.purpose != AxisPurpose.NeuralFeatures then
      Left(
        DistanceEstimandError.Relation(
          RelationError.InvalidNeuralPurpose(table.rows.identity.purpose)
        )
      )
    else if table.columns.identity != table.rows.identity then
      Left(
        DistanceEstimandError.PrecisionAxisMismatch(
          table.rows.identity.fingerprint,
          table.columns.identity.fingerprint
        )
      )
    else if !(table.columns.evidence eq table.rows.evidence) then Left(DistanceEstimandError.PrecisionWitnessMismatch)
    else
      for
        precision <- table
          .rightMultiply(DMat.eye(table.columnCount))
          .left
          .map(DistanceEstimandError.Evidence.apply)
        certificate <- NoisePrecisionCertificate.verify(
          table.rows,
          precision,
          table.table.valueIdentity,
          tolerance
        )
      yield new CertifiedNoisePrecision(table, certificate)

final case class PartitionPrecisionReceipt(
    partition: PartitionId,
    precisionValue: ValueIdentity,
    certificate: ScientificComponentFingerprint
)

final class CrossnobisPrecisionReceipt private[mvpa] (
    val measurement: MeasurementIdentity,
    val partitions: Vector[PartitionPrecisionReceipt],
    val localPrecision: ValueIdentity,
    val localCertificate: ScientificComponentFingerprint,
    val operatorApplications: Long
)

final case class DistanceUnits(
    sourceUnits: Option[AxisUnits],
    whitened: Boolean,
    normalization: RdmNormalization
):
  def label: String =
    val base =
      if whitened then "whitened-squared"
      else sourceUnits.fold("dimensionless-squared")(units => s"${units.value}-squared")
    normalization match
      case RdmNormalization.Raw                     => base
      case RdmNormalization.DivideByNeuralDimension => s"$base-per-neural-coordinate"

final class IdentityPrecisionSquaredDistance[
    P <: SemanticSpace,
    E <: SemanticSpace,
    Local <: SemanticSpace,
    EK,
    LK
] private[mvpa] (
    val rdm: IdentifiedRdm[P, P, E, Local, EK, LK],
    val units: DistanceUnits,
    val computation: RelationalComputationReceipt
)

final class CrossnobisDistance[
    P <: SemanticSpace,
    E <: SemanticSpace,
    Local <: SemanticSpace,
    EK,
    LK
] private[mvpa] (
    val rdm: IdentifiedRdm[P, P, E, Local, EK, LK],
    val units: DistanceUnits,
    val precision: CrossnobisPrecisionReceipt,
    val computation: RelationalComputationReceipt
)

private[mvpa] object DistanceEstimands:

  private[mvpa] final case class CertifiedLocalPrecision[
      L <: SemanticSpace,
      K
  ](
      query: NeuralQuery[L, K],
      certificate: NoisePrecisionCertificate[L, K],
      receipt: CrossnobisPrecisionReceipt
  )

  private[mvpa] def pooledRelationPrecision[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      LK,
      C <: HasNoisePrecision[N, NK]
  ](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      design: PairingDesign[P, P],
      measurement: Measurement[N, NK, LK]
  ): Either[
    DistanceEstimandError,
    CertifiedLocalPrecision[measurement.local.Id, LK]
  ] =
    if measurement.identity.kind != MeasurementKind.HardSelection &&
      measurement.identity.kind != MeasurementKind.Identity
    then Left(DistanceEstimandError.UnsupportedPrecisionMeasurement(measurement.identity.kind))
    else
      val needed = design.edges.iterator.flatMap(edge => Iterator(edge.left, edge.right)).toSet
      val partitions = source.partitionKeys.filter(needed.contains)
      val accumulated = Matrix.newBuilder(measurement.local.size, measurement.local.size)
      val receipts = Vector.newBuilder[PartitionPrecisionReceipt]
      var position = 0
      while position < partitions.length do
        val partition = partitions(position)
        val relation = source.relation(partition) match
          case Left(error)  => return Left(DistanceEstimandError.Relation(error))
          case Right(value) => value
        val capability = relation.capabilities
        val certified = capability.noisePrecision
        if certified.neural.identity != source.neuralAxis.identity then
          return Left(
            DistanceEstimandError.PrecisionAxisMismatch(
              source.neuralAxis.identity.fingerprint,
              certified.neural.identity.fingerprint
            )
          )
        if !(certified.neural.evidence eq source.neuralAxis.evidence) then
          return Left(DistanceEstimandError.PrecisionWitnessMismatch)
        val rightMeasured = certified.table.measureColumns(measurement) match
          case Left(error)  => return Left(DistanceEstimandError.Evidence(error))
          case Right(value) => value
        val sourceByLocal = rightMeasured.rightMultiply(DMat.eye(measurement.local.size)) match
          case Left(error)  => return Left(DistanceEstimandError.Evidence(error))
          case Right(value) => value
        val local = measurement.operator.applyTo(sourceByLocal) match
          case Left(error)  => return Left(DistanceEstimandError.Linear(error.getMessage))
          case Right(value) => value
        var row = 0
        while row < local.rows do
          var column = 0
          while column < local.cols do
            accumulated(row, column) += local(row, column) / partitions.length.toDouble
            column += 1
          row += 1
        receipts += PartitionPrecisionReceipt(
          partition,
          certified.table.table.valueIdentity,
          certified.certificate.identity
        )
        position += 1

      if partitions.isEmpty then Left(DistanceEstimandError.InvalidPooledPrecision("no paired partitions"))
      else
        val pooled = accumulated.result()
        val precisionReceipts = receipts.result()
        val writer = CanonicalWriter()
        writer.string("scalafim-crossnobis-pooled-precision/v1")
        writer.string("equal-partition-mean")
        writer.string(measurement.identity.fingerprint.value)
        precisionReceipts.foreach: value =>
          writer.string(value.partition.value)
          writer.string(value.precisionValue.stableKey)
          writer.string(value.certificate.value)
        val valueId = ValueId.unsafe(
          s"pooled-precision-${AxisDigest.sha256Hex(writer.result())}"
        )
        val valueIdentity = ValueIdentity.source(valueId)
        for
          certificate <- NoisePrecisionCertificate.verify(
            measurement.local,
            pooled,
            valueIdentity
          )
          query <- NeuralQuery
            .fixedPrecision(measurement.local, pooled, valueId)
            .left
            .map(DistanceEstimandError.Query.apply)
        yield CertifiedLocalPrecision(
          query,
          certificate,
          new CrossnobisPrecisionReceipt(
            measurement.identity,
            precisionReceipts,
            valueIdentity,
            certificate.identity,
            operatorApplications = partitions.length.toLong * 2L
          )
        )

  private[mvpa] def validateIndependent[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: RelationCapabilities[N, NK]
  ](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      design: PairingDesign[P, P]
  ): Either[DistanceEstimandError, Unit] =
    design
      .validateSource(source.identity, source.partitions)
      .left
      .map(DistanceEstimandError.Independence.apply)

  private[mvpa] def normalize(
      values: Vector[Double],
      neuralDimension: Int,
      normalization: RdmNormalization
  ): Vector[Double] =
    normalization match
      case RdmNormalization.Raw                     => values
      case RdmNormalization.DivideByNeuralDimension =>
        values.map(_ / neuralDimension.toDouble)
