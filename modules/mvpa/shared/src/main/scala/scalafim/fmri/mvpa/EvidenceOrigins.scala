package scalafim.fmri.mvpa

import multivar.core.ValueIdentity
import scalafim.response.{ProvenanceEvidence, ProvenanceOperation}

/** Acquisition coordinates are deliberately separate from the row axis of a
  * derived table. A trial/effect row does not establish which original scans
  * contributed to it.
  */
enum AcquisitionCoordinates:
  case Unknown
  case OriginalTemporalAxis(axis: AxisDescriptor)

/** Declared value support. `Unknown` is conservative: it does not claim that
  * an operator is local, or that it did not read a value.
  */
enum ValueSupport:
  case Unknown
  case Bounded(axis: AxisDescriptor, values: ValueIdentity, ordinals: Vector[Int])

/** Inputs used while preparing values. A fixed shared transform and a
  * transform jointly learned with targets are scientifically distinct.
  */
enum PreparationSupport:
  case Unknown
  case FixedShared(inputs: ValueSupport)
  case JointlyLearned(inputs: ValueSupport, targetInputs: ValueSupport)

final case class BoundedEvidenceOrigins private[mvpa] (
    source: EvidenceSource,
    values: ValueIdentity,
    acquisition: AcquisitionCoordinates,
    direct: ValueSupport,
    preparation: PreparationSupport,
    outputAxis: AxisDescriptor
)

/** Immutable support declaration for a scientific value. This describes only
  * declared support; it is not an executable provenance graph or a claim
  * about stochastic independence or actual callback/operator reads.
  */
enum EvidenceOrigins:
  /** Used where legacy metadata has no source-bound support declaration. */
  case Unknown
  case Bounded(value: BoundedEvidenceOrigins)

  def outputAssociation: Option[AxisDescriptor] =
    this match
      case Unknown        => None
      case Bounded(value) => Some(value.outputAxis)

  /** Reindexing changes the output association only. It cannot narrow the
    * support declared for the upstream acquisition/readout values.
    */
  def reindexOutput(outputAxis: AxisDescriptor): EvidenceOrigins =
    reindexOutput(outputAxis, None)

  private[mvpa] def reindexOutput(
      outputAxis: AxisDescriptor,
      outputValues: ValueIdentity
  ): EvidenceOrigins =
    reindexOutput(outputAxis, Some(outputValues))

  private def reindexOutput(
      outputAxis: AxisDescriptor,
      outputValues: Option[ValueIdentity]
  ): EvidenceOrigins =
    this match
      case Unknown => Unknown
      case Bounded(value) => Bounded(value.copy(outputAxis = outputAxis, values = outputValues.getOrElse(value.values)))

  /** Writes the complete structural declaration into the repository's framed
    * scientific-identity digest. Consumers that frame `EvidenceIdentity`
    * manually must call this rather than collapsing support to a display key.
    */
  private[mvpa] def writeFramed(writer: AxisDigest.Writer): Unit =
    this match
      case Unknown => writer.string("unknown")
      case Bounded(value) =>
        writer.string("bounded")
        writer.string(value.source.sourceId.value)
        writer.intLE(value.source.nodes.length)
        value.source.nodes.foreach: node =>
          writer.string(node.id.value)
          node.operation match
            case ProvenanceOperation.SourceRead(source) =>
              writer.string("source-read")
              writer.string(source.value)
            case ProvenanceOperation.Selection => writer.string("selection")
            case ProvenanceOperation.Assembly => writer.string("assembly")
            case ProvenanceOperation.Adapter(adapter) =>
              writer.string("adapter")
              writer.string(adapter.value)
            case ProvenanceOperation.Derived(operation) =>
              writer.string("derived")
              writer.string(operation.value)
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
        writer.intLE(value.source.roots.length)
        value.source.roots.foreach(root => writer.string(root.value))
        EvidenceOrigins.writeValueIdentity(writer, value.values)
        EvidenceOrigins.writeAcquisition(writer, value.acquisition)
        EvidenceOrigins.writeSupport(writer, value.direct)
        EvidenceOrigins.writePreparation(writer, value.preparation)
        writer.string(value.outputAxis.coordinateSignature.value)

  private[mvpa] def identityDigest: String =
    AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.mvpa.evidence-origins.v1")
      writeFramed(writer)

  private[mvpa] def matches(
      source: EvidenceSource,
      values: ValueIdentity,
      outputAxis: AxisDescriptor
  ): Boolean =
    this match
      case Unknown => true
      case Bounded(value) =>
        value.source.sourceId == source.sourceId &&
          value.source.nodes == source.nodes &&
          value.source.roots == source.roots &&
          value.values == values &&
          value.outputAxis == outputAxis

object EvidenceOrigins:
  /** Legacy observation constructors retain their known source/value identity
    * while making all support claims explicit unknowns.
    */
  def unknown(
      source: EvidenceSource,
      values: ValueIdentity,
      outputAxis: AxisDescriptor
  ): EvidenceOrigins =
    Bounded(
      BoundedEvidenceOrigins(
        source,
        values,
        AcquisitionCoordinates.Unknown,
        ValueSupport.Unknown,
        PreparationSupport.Unknown,
        outputAxis
      )
    )

  def make(
      source: EvidenceSource,
      values: ValueIdentity,
      acquisition: AcquisitionCoordinates,
      direct: ValueSupport,
      preparation: PreparationSupport,
      outputAxis: AxisDescriptor
  ): Either[EvidenceError, EvidenceOrigins] =
    for
      _ <- validate(acquisition, direct, "direct value support", requireTemporalAxis = true)
      _ <- validatePreparation(preparation)
    yield Bounded(BoundedEvidenceOrigins(source, values, acquisition, direct, preparation, outputAxis))

  private def validatePreparation(preparation: PreparationSupport): Either[EvidenceError, Unit] =
    preparation match
      case PreparationSupport.Unknown => Right(())
      case PreparationSupport.FixedShared(inputs) =>
        validateOrdinals(inputs, "fixed preparation inputs")
      case PreparationSupport.JointlyLearned(inputs, targetInputs) =>
        for
          _ <- validateOrdinals(inputs, "jointly learned preparation inputs")
          _ <- validateOrdinals(targetInputs, "jointly learned preparation target inputs")
        yield ()

  private def validate(
      acquisition: AcquisitionCoordinates,
      support: ValueSupport,
      field: String,
      requireTemporalAxis: Boolean
  ): Either[EvidenceError, Unit] =
    for
      _ <- validateOrdinals(support, field)
      _ <-
        (acquisition, support) match
          case (AcquisitionCoordinates.OriginalTemporalAxis(axis), ValueSupport.Bounded(found, _, _)) if axis != found =>
            Left(EvidenceError.AxisMismatch(field, axis.stableKey, found.stableKey))
          case (AcquisitionCoordinates.Unknown, ValueSupport.Bounded(_, _, _)) if requireTemporalAxis =>
            Left(EvidenceError.InvalidSource(s"$field is bounded but original temporal coordinates are unknown"))
          case _ => Right(())
    yield ()

  private def validateOrdinals(support: ValueSupport, field: String): Either[EvidenceError, Unit] =
    support match
      case ValueSupport.Unknown => Right(())
      case ValueSupport.Bounded(axis, _, ordinals) =>
        val seen = scala.collection.mutable.HashSet.empty[Int]
        var index = 0
        while index < ordinals.length do
          val ordinal = ordinals(index)
          if ordinal < 0 || ordinal >= axis.size then
            return Left(EvidenceError.InvalidOrdinal(ordinal, axis.size))
          if seen.contains(ordinal) then
            return Left(EvidenceError.InvalidAxis(field, s"duplicate support ordinal $ordinal"))
          seen += ordinal
          index += 1
        Right(())

  private def writeAcquisition(writer: AxisDigest.Writer, acquisition: AcquisitionCoordinates): Unit =
    acquisition match
      case AcquisitionCoordinates.Unknown => writer.string("unknown-acquisition")
      case AcquisitionCoordinates.OriginalTemporalAxis(axis) =>
        writer.string("original-temporal-axis")
        writer.string(axis.coordinateSignature.value)

  private def writeSupport(writer: AxisDigest.Writer, support: ValueSupport): Unit =
    support match
      case ValueSupport.Unknown => writer.string("unknown-support")
      case ValueSupport.Bounded(axis, values, ordinals) =>
        writer.string("bounded-support")
        writer.string(axis.coordinateSignature.value)
        writeValueIdentity(writer, values)
        writer.intLE(ordinals.length)
        ordinals.foreach(writer.intLE)

  private def writePreparation(writer: AxisDigest.Writer, preparation: PreparationSupport): Unit =
    preparation match
      case PreparationSupport.Unknown => writer.string("unknown-preparation")
      case PreparationSupport.FixedShared(inputs) =>
        writer.string("fixed-shared")
        writeSupport(writer, inputs)
      case PreparationSupport.JointlyLearned(inputs, targetInputs) =>
        writer.string("jointly-learned")
        writeSupport(writer, inputs)
        writeSupport(writer, targetInputs)

  private def writeValueIdentity(writer: AxisDigest.Writer, identity: ValueIdentity): Unit =
    identity match
      case ValueIdentity.Source(id) =>
        writer.string("source")
        writer.string(id.value)
      case ValueIdentity.Adjoint(of) =>
        writer.string("adjoint")
        writeValueIdentity(writer, of)
      case ValueIdentity.Composition(first, second) =>
        writer.string("composition")
        writeValueIdentity(writer, first)
        writeValueIdentity(writer, second)
      case ValueIdentity.Derived(operation, inputs) =>
        writer.string("derived")
        writer.string(operation)
        writer.intLE(inputs.length)
        inputs.foreach(writeValueIdentity(writer, _))
