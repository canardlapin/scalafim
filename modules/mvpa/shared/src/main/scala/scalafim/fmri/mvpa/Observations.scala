package scalafim.fmri.mvpa

import gale.linalg.{DMat, DoubleLinearOperator}
import gale.sparse.CSR
import multivar.core.{
  CoordinateEvidence,
  Lin,
  OperatorRepresentation,
  SemanticProvenance,
  SemanticProvenanceEvent,
  SemanticSpace,
  SpaceEvidence,
  SpaceRef,
  Table,
  ValueIdentity
}
import resample4s.core.Reindexing
import scalafim.response.{Provenance, ProvenanceEvidence, ProvenanceId, ProvenanceNode, ProvenanceOperation, SourceId}

final class EvidenceSource private (
    val sourceId: SourceId,
    val provenance: Provenance
):
  val nodes: Vector[ProvenanceNode] =
    provenance.nodes

  val roots: Vector[ProvenanceId] =
    provenance.roots

  override def equals(other: Any): Boolean =
    other match
      case that: EvidenceSource =>
        sourceId == that.sourceId && nodes == that.nodes && roots == that.roots
      case _ => false

  override def hashCode(): Int =
    (sourceId, nodes, roots).hashCode

object EvidenceSource:
  def apply(sourceId: SourceId, provenance: Provenance): Either[EvidenceError, EvidenceSource] =
    val declaresSource = provenance.nodes.exists: node =>
      node.operation match
        case ProvenanceOperation.SourceRead(found) => found == sourceId
        case _                                     => false
    if declaresSource then Right(new EvidenceSource(sourceId, provenance))
    else
      Left(
        EvidenceError.InvalidSource(
          s"response provenance does not contain source ${sourceId.value}"
        )
      )

final case class EvidenceIdentity(
    rows: AxisDescriptor,
    columns: AxisDescriptor,
    source: SourceId,
    provenanceNodes: Vector[ProvenanceNode],
    provenanceRoots: Vector[ProvenanceId],
    values: ValueIdentity,
    origins: EvidenceOrigins
):
  /** Complete, versioned metadata framing. Support can be unknown without
    * erasing the independently retained source and provenance declarations.
    * This binds declared evidence; it does not verify payload contents.
    */
  private[mvpa] def writeFramed(writer: AxisDigest.Writer): Unit =
    writer.string("scalafim.mvpa.evidence-identity.v1")
    writer.string(rows.stableKey)
    writer.string(columns.stableKey)
    writer.string(source.value)
    writer.intLE(provenanceNodes.length)
    provenanceNodes.foreach: node =>
      writer.string(node.id.value)
      node.operation match
        case ProvenanceOperation.SourceRead(id) =>
          writer.string("source-read")
          writer.string(id.value)
        case ProvenanceOperation.Selection => writer.string("selection")
        case ProvenanceOperation.Assembly => writer.string("assembly")
        case ProvenanceOperation.Adapter(id) =>
          writer.string("adapter")
          writer.string(id.value)
        case ProvenanceOperation.Derived(id) =>
          writer.string("derived")
          writer.string(id.value)
      writer.intLE(node.parents.length)
      node.parents.foreach(parent => writer.string(parent.value))
      writer.intLE(node.evidence.length)
      node.evidence.foreach:
        case ProvenanceEvidence.Domain(reference) =>
          writer.string("domain")
          writer.string(reference.namespace.value)
          writer.string(reference.value)
        case ProvenanceEvidence.External(reference) =>
          writer.string("external")
          writer.string(reference.namespace.value)
          writer.string(reference.value)
        case ProvenanceEvidence.NoneDeclared => writer.string("none-declared")
    writer.intLE(provenanceRoots.length)
    provenanceRoots.foreach(root => writer.string(root.value))
    EvidenceIdentity.writeValues(writer, values)
    origins.writeFramed(writer)

object EvidenceIdentity:
  private[mvpa] def writeValues(writer: AxisDigest.Writer, value: ValueIdentity): Unit =
    value match
      case ValueIdentity.Source(id) =>
        writer.string("source")
        writer.string(id.value)
      case ValueIdentity.Adjoint(of) =>
        writer.string("adjoint")
        writeValues(writer, of)
      case ValueIdentity.Composition(first, second) =>
        writer.string("composition")
        writeValues(writer, first)
        writeValues(writer, second)
      case ValueIdentity.Derived(operation, inputs) =>
        writer.string("derived")
        writer.string(operation)
        writer.intLE(inputs.length)
        inputs.foreach(writeValues(writer, _))

final case class EvidenceRecord(
    rows: AxisRecord,
    columns: AxisRecord,
    source: EvidenceSource,
    valueIdentity: ValueIdentity,
    origins: EvidenceOrigins = EvidenceOrigins.Unknown
)

/** Identified sample-by-neural evidence. Storage representation remains a
  * Multivar `Table`; it is deliberately absent from `identity`.
  */
final class Observations[S <: SemanticSpace, N <: SemanticSpace] private (
    val samples: SpaceEvidence[S],
    val neural: SpaceEvidence[N],
    val sampleAxis: AxisDescriptor,
    val neuralAxis: AxisDescriptor,
    private val sampleRecord: AxisRecord,
    private val neuralRecord: AxisRecord,
    val patterns: Table[S, N],
    val source: EvidenceSource,
    val origins: EvidenceOrigins
):
  val identity: EvidenceIdentity =
    EvidenceIdentity(
      sampleAxis,
      neuralAxis,
      source.sourceId,
      source.nodes,
      source.roots,
      patterns.valueIdentity,
      origins
    )

  def rows: Int =
    patterns.rows

  def columns: Int =
    patterns.cols

  def representation: OperatorRepresentation =
    patterns.descriptor.representation

  def toRecord: EvidenceRecord =
    EvidenceRecord(sampleRecord, neuralRecord, source, patterns.valueIdentity, origins)

  def reindex[K, R <: Reindexing](by: ReindexingLeg[S, K, R]): Observations[by.Child, N] =
    val reindexed = patterns.andThen(by.leg)
    new Observations(
      by.child.evidence,
      neural,
      by.child.descriptor,
      neuralAxis,
      by.child.toRecord,
      neuralRecord,
      reindexed,
      source,
      origins.reindexOutput(by.child.descriptor, reindexed.valueIdentity)
    )

object Observations:
  /** Retain an already identified Multivar table without decoding or
    * materializing an operator. The table's nominal coordinates and complete
    * axis witnesses are checked before provenance is attached.
    */
  def fromTable[SK, NK](
      samples: AxisRef[SK],
      neural: AxisRef[NK],
      table: Table[samples.Id, neural.Id],
      source: EvidenceSource,
      origins: EvidenceOrigins
  ): Either[EvidenceError, Observations[samples.Id, neural.Id]] =
    if !origins.matches(source, table.valueIdentity, samples.descriptor) then
      Left(EvidenceError.InvalidSource("evidence origins do not match the observation source, values, or output axis"))
    else if table.rows != samples.size then
      Left(EvidenceError.ShapeMismatch("observation table rows", samples.size, table.rows))
    else if table.cols != neural.size then
      Left(EvidenceError.ShapeMismatch("observation table columns", neural.size, table.cols))
    else
      Right(new Observations(
        samples.evidence,
        neural.evidence,
        samples.descriptor,
        neural.descriptor,
        samples.toRecord,
        neural.toRecord,
        table,
        source,
        origins
      ))

  def fromDense[SK, NK](samples: AxisRef[SK], neural: AxisRef[NK], values: DMat, valueIdentity: ValueIdentity, source: EvidenceSource): Either[EvidenceError, Observations[samples.Id, neural.Id]] =
    fromDense(samples, neural, values, valueIdentity, source, EvidenceOrigins.unknown(source, valueIdentity, samples.descriptor))

  def fromDense[SK, NK](
      samples: AxisRef[SK],
      neural: AxisRef[NK],
      values: DMat,
      valueIdentity: ValueIdentity,
      source: EvidenceSource,
      origins: EvidenceOrigins
  ): Either[EvidenceError, Observations[samples.Id, neural.Id]] =
    decode(samples, neural, samples.toRecord, neural.toRecord, values, valueIdentity, source, origins)

  def fromOperator[SK, NK](samples: AxisRef[SK], neural: AxisRef[NK], operator: DoubleLinearOperator, valueIdentity: ValueIdentity, source: EvidenceSource): Either[EvidenceError, Observations[samples.Id, neural.Id]] =
    fromOperator(samples, neural, operator, valueIdentity, source, EvidenceOrigins.unknown(source, valueIdentity, samples.descriptor))

  def fromOperator[SK, NK](
      samples: AxisRef[SK],
      neural: AxisRef[NK],
      operator: DoubleLinearOperator,
      valueIdentity: ValueIdentity,
      source: EvidenceSource,
      origins: EvidenceOrigins
  ): Either[EvidenceError, Observations[samples.Id, neural.Id]] =
    decode(samples, neural, samples.toRecord, neural.toRecord, operator, valueIdentity, source, origins)

  def fromSparse[SK, NK](samples: AxisRef[SK], neural: AxisRef[NK], values: CSR, valueIdentity: ValueIdentity, source: EvidenceSource): Either[EvidenceError, Observations[samples.Id, neural.Id]] =
    fromSparse(samples, neural, values, valueIdentity, source, EvidenceOrigins.unknown(source, valueIdentity, samples.descriptor))

  def fromSparse[SK, NK](
      samples: AxisRef[SK],
      neural: AxisRef[NK],
      values: CSR,
      valueIdentity: ValueIdentity,
      source: EvidenceSource,
      origins: EvidenceOrigins
  ): Either[EvidenceError, Observations[samples.Id, neural.Id]] =
    decode(samples, neural, samples.toRecord, neural.toRecord, values, valueIdentity, source, origins)

  /** Decode external storage only after both complete axis records agree with
    * the expected nominal witnesses. Constructing the semantic operator does
    * not evaluate or materialize its payload.
    */
  def decode[SK, NK](samples: AxisRef[SK], neural: AxisRef[NK], declaredSamples: AxisRecord, declaredNeural: AxisRecord, operator: DoubleLinearOperator, valueIdentity: ValueIdentity, source: EvidenceSource): Either[EvidenceError, Observations[samples.Id, neural.Id]] =
    decode(samples, neural, declaredSamples, declaredNeural, operator, valueIdentity, source, EvidenceOrigins.unknown(source, valueIdentity, samples.descriptor))

  def decode[SK, NK](
      samples: AxisRef[SK],
      neural: AxisRef[NK],
      declaredSamples: AxisRecord,
      declaredNeural: AxisRecord,
      operator: DoubleLinearOperator,
      valueIdentity: ValueIdentity,
      source: EvidenceSource,
      origins: EvidenceOrigins
  ): Either[EvidenceError, Observations[samples.Id, neural.Id]] =
    if !origins.matches(source, valueIdentity, samples.descriptor) then
      Left(EvidenceError.InvalidSource("evidence origins do not match the observation source, values, or output axis"))
    else EvidenceTableBinding
      .decode(samples, neural, declaredSamples, declaredNeural, operator, valueIdentity, source)
      .map: table =>
        new Observations(
          samples.evidence,
          neural.evidence,
          samples.descriptor,
          neural.descriptor,
          samples.toRecord,
          neural.toRecord,
          table,
          source,
          origins
        )

  def decode[SK, NK](
      samples: AxisRef[SK],
      neural: AxisRef[NK],
      record: EvidenceRecord,
      operator: DoubleLinearOperator
  ): Either[EvidenceError, Observations[samples.Id, neural.Id]] =
    decode(samples, neural, record.rows, record.columns, operator, record.valueIdentity, record.source, record.origins)

/** An identified, genuinely multivariate target table. Its target-feature
  * axis is nominal and fully described rather than hidden inside a vector
  * valued scalar column.
  */
final class MultiResponse[S <: SemanticSpace, F <: SemanticSpace] private (
    val samples: SpaceEvidence[S],
    val features: SpaceEvidence[F],
    val sampleAxis: AxisDescriptor,
    val featureAxis: AxisDescriptor,
    private val sampleRecord: AxisRecord,
    private val featureRecord: AxisRecord,
    val targets: Table[S, F],
    val source: EvidenceSource,
    val origins: EvidenceOrigins
):
  val identity: EvidenceIdentity =
    EvidenceIdentity(
      sampleAxis,
      featureAxis,
      source.sourceId,
      source.nodes,
      source.roots,
      targets.valueIdentity,
      origins
    )

  def rows: Int =
    targets.rows

  def columns: Int =
    targets.cols

  def representation: OperatorRepresentation =
    targets.descriptor.representation

  def toRecord: EvidenceRecord =
    EvidenceRecord(sampleRecord, featureRecord, source, targets.valueIdentity, origins)

  def reindex[K, R <: Reindexing](by: ReindexingLeg[S, K, R]): MultiResponse[by.Child, F] =
    val reindexed = targets.andThen(by.leg)
    new MultiResponse(
      by.child.evidence,
      features,
      by.child.descriptor,
      featureAxis,
      by.child.toRecord,
      featureRecord,
      reindexed,
      source,
      origins.reindexOutput(by.child.descriptor, reindexed.valueIdentity)
    )

object MultiResponse:
  def fromDense[SK, FK](samples: AxisRef[SK], features: AxisRef[FK], values: DMat, valueIdentity: ValueIdentity, source: EvidenceSource): Either[EvidenceError, MultiResponse[samples.Id, features.Id]] =
    fromDense(samples, features, values, valueIdentity, source, EvidenceOrigins.unknown(source, valueIdentity, samples.descriptor))

  def fromDense[SK, FK](
      samples: AxisRef[SK],
      features: AxisRef[FK],
      values: DMat,
      valueIdentity: ValueIdentity,
      source: EvidenceSource,
      origins: EvidenceOrigins
  ): Either[EvidenceError, MultiResponse[samples.Id, features.Id]] =
    decode(samples, features, samples.toRecord, features.toRecord, values, valueIdentity, source, origins)

  def fromOperator[SK, FK](samples: AxisRef[SK], features: AxisRef[FK], operator: DoubleLinearOperator, valueIdentity: ValueIdentity, source: EvidenceSource): Either[EvidenceError, MultiResponse[samples.Id, features.Id]] =
    fromOperator(samples, features, operator, valueIdentity, source, EvidenceOrigins.unknown(source, valueIdentity, samples.descriptor))

  def fromOperator[SK, FK](
      samples: AxisRef[SK],
      features: AxisRef[FK],
      operator: DoubleLinearOperator,
      valueIdentity: ValueIdentity,
      source: EvidenceSource,
      origins: EvidenceOrigins
  ): Either[EvidenceError, MultiResponse[samples.Id, features.Id]] =
    decode(samples, features, samples.toRecord, features.toRecord, operator, valueIdentity, source, origins)

  def fromSparse[SK, FK](samples: AxisRef[SK], features: AxisRef[FK], values: CSR, valueIdentity: ValueIdentity, source: EvidenceSource): Either[EvidenceError, MultiResponse[samples.Id, features.Id]] =
    fromSparse(samples, features, values, valueIdentity, source, EvidenceOrigins.unknown(source, valueIdentity, samples.descriptor))

  def fromSparse[SK, FK](
      samples: AxisRef[SK],
      features: AxisRef[FK],
      values: CSR,
      valueIdentity: ValueIdentity,
      source: EvidenceSource,
      origins: EvidenceOrigins
  ): Either[EvidenceError, MultiResponse[samples.Id, features.Id]] =
    decode(samples, features, samples.toRecord, features.toRecord, values, valueIdentity, source, origins)

  def decode[SK, FK](samples: AxisRef[SK], features: AxisRef[FK], declaredSamples: AxisRecord, declaredFeatures: AxisRecord, operator: DoubleLinearOperator, valueIdentity: ValueIdentity, source: EvidenceSource): Either[EvidenceError, MultiResponse[samples.Id, features.Id]] =
    decode(samples, features, declaredSamples, declaredFeatures, operator, valueIdentity, source, EvidenceOrigins.unknown(source, valueIdentity, samples.descriptor))

  def decode[SK, FK](
      samples: AxisRef[SK],
      features: AxisRef[FK],
      declaredSamples: AxisRecord,
      declaredFeatures: AxisRecord,
      operator: DoubleLinearOperator,
      valueIdentity: ValueIdentity,
      source: EvidenceSource,
      origins: EvidenceOrigins
  ): Either[EvidenceError, MultiResponse[samples.Id, features.Id]] =
    if !origins.matches(source, valueIdentity, samples.descriptor) then
      Left(EvidenceError.InvalidSource("evidence origins do not match the response source, values, or output axis"))
    else EvidenceTableBinding
      .decode(samples, features, declaredSamples, declaredFeatures, operator, valueIdentity, source)
      .map: table =>
        new MultiResponse(
          samples.evidence,
          features.evidence,
          samples.descriptor,
          features.descriptor,
          samples.toRecord,
          features.toRecord,
          table,
          source,
          origins
        )

  def decode[SK, FK](
      samples: AxisRef[SK],
      features: AxisRef[FK],
      record: EvidenceRecord,
      operator: DoubleLinearOperator
  ): Either[EvidenceError, MultiResponse[samples.Id, features.Id]] =
    decode(samples, features, record.rows, record.columns, operator, record.valueIdentity, record.source, record.origins)

final class Supervised[S <: SemanticSpace, N <: SemanticSpace, Y] private (
    val observations: Observations[S, N],
    val target: Column[S, Y]
)

object Supervised:
  def apply[S <: SemanticSpace, N <: SemanticSpace, Y](
      observations: Observations[S, N],
      target: Column[S, Y]
  ): Either[EvidenceError, Supervised[S, N, Y]] =
    if observations.sampleAxis == target.rowAxis then
      Right(new Supervised(observations, target))
    else
      Left(
        EvidenceError.AxisMismatch(
          "supervised target rows",
          observations.sampleAxis.stableKey,
          target.rowAxis.stableKey
        )
      )

final class MultiResponseSupervised[
    S <: SemanticSpace,
    N <: SemanticSpace,
    F <: SemanticSpace
] private (
    val observations: Observations[S, N],
    val target: MultiResponse[S, F]
)

object MultiResponseSupervised:
  def apply[S <: SemanticSpace, N <: SemanticSpace, F <: SemanticSpace](
      observations: Observations[S, N],
      target: MultiResponse[S, F]
  ): Either[EvidenceError, MultiResponseSupervised[S, N, F]] =
    if observations.sampleAxis == target.sampleAxis then
      Right(new MultiResponseSupervised(observations, target))
    else
      Left(
        EvidenceError.AxisMismatch(
          "multiresponse target rows",
          observations.sampleAxis.stableKey,
          target.sampleAxis.stableKey
        )
      )

private object EvidenceTableBinding:
  def decode[RK, CK](
      rows: AxisRef[RK],
      columns: AxisRef[CK],
      declaredRows: AxisRecord,
      declaredColumns: AxisRecord,
      operator: DoubleLinearOperator,
      valueIdentity: ValueIdentity,
      source: EvidenceSource
  ): Either[EvidenceError, Table[rows.Id, columns.Id]] =
    for
      decodedRows <- AxisDescriptor.decode(declaredRows)
      decodedColumns <- AxisDescriptor.decode(declaredColumns)
      _ <- rows.validateDeclared("table rows", declaredRows)
      _ <- columns.validateDeclared("table columns", declaredColumns)
      declaredRowRef <- SpaceRef
        .of(decodedRows.stableKey, decodedRows.role, decodedRows.size)
        .left
        .map(EvidenceError.MultivarFailure.apply)
      declaredColumnRef <- SpaceRef
        .of(decodedColumns.stableKey, decodedColumns.role, decodedColumns.size)
        .left
        .map(EvidenceError.MultivarFailure.apply)
      table <- Lin
        .decode(
          operator,
          CoordinateEvidence.dual(columns.evidence),
          CoordinateEvidence.primal(rows.evidence),
          CoordinateEvidence.dual(declaredColumnRef.evidence).descriptor,
          CoordinateEvidence.primal(declaredRowRef.evidence).descriptor,
          valueIdentity,
          SemanticProvenance
            .source(s"scalafim-response-${source.sourceId.value}")
            .append(SemanticProvenanceEvent.Adapted("ScalaFIMIdentifiedEvidence"))
        )
        .left
        .map(EvidenceError.SemanticFailure.apply)
    yield table
