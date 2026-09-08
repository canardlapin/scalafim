package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.DoubleLinearOperator
import gale.sparse.Sparse
import multivar.core.CoordinateEvidence
import multivar.core.Lin
import multivar.core.Primal
import multivar.core.SemanticError
import multivar.core.SemanticProvenance
import multivar.core.SemanticSpace
import multivar.core.ValueId
import multivar.core.ValueIdentity
import resample4s.core.Injection

opaque type MeasurementId = String

object MeasurementId:
  def apply(value: String): Either[MeasurementError, MeasurementId] =
    AxisText
      .identifier("measurement id", value)
      .left
      .map(MeasurementError.InvalidText.apply)

  private[mvpa] def unsafe(value: String): MeasurementId =
    value

  extension (id: MeasurementId) inline def value: String = id

opaque type MeasurementFingerprint = String

object MeasurementFingerprint:
  private val Prefix = "scalafim-mvpa-measurement-v1-"

  def apply(value: String): Either[MeasurementError, MeasurementFingerprint] =
    val digest = value.stripPrefix(Prefix)
    if value.startsWith(Prefix) && digest.length == 64 && digest.forall(AxisText.isLowerHexDigit) then Right(value)
    else Left(MeasurementError.InvalidFingerprint(value))

  private[mvpa] def fromDigest(digest: String): MeasurementFingerprint =
    unsafe(Prefix + digest)

  private[mvpa] def unsafe(value: String): MeasurementFingerprint =
    value

  extension (fingerprint: MeasurementFingerprint) inline def value: String = fingerprint

enum MeasurementKind:
  case Identity
  case HardSelection
  case WeightedRegion
  case FixedProjection

  def label: String =
    this match
      case Identity        => "identity"
      case HardSelection   => "hard-selection"
      case WeightedRegion  => "weighted-region"
      case FixedProjection => "fixed-projection"

final case class MeasurementIdentity(
    id: MeasurementId,
    fingerprint: MeasurementFingerprint,
    source: AxisFingerprint,
    local: AxisFingerprint,
    kind: MeasurementKind
)

enum MeasurementError:
  case InvalidText(error: AxisIdentityError)
  case InvalidFingerprint(value: String)
  case Axis(error: AxisRefError)
  case ShapeMismatch(expectedRows: Int, expectedColumns: Int, actualRows: Int, actualColumns: Int)
  case EmptySupport
  case DuplicateSupportKey(key: String)
  case UnknownSupportKey(key: String)
  case InvalidWeight(key: String, value: Double)
  case Semantic(error: SemanticError)
  case SourceIdentityMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case NominalSourceWitnessMismatch
  case EmptyFrame
  case DuplicateMeasurementId(id: MeasurementId)

  def message: String =
    this match
      case InvalidText(error) =>
        error.message
      case InvalidFingerprint(value) =>
        s"invalid measurement fingerprint '$value'"
      case Axis(error) =>
        error.message
      case ShapeMismatch(expectedRows, expectedColumns, actualRows, actualColumns) =>
        s"measurement expected ${expectedRows}x$expectedColumns weights, got ${actualRows}x$actualColumns"
      case EmptySupport =>
        "measurement support must contain at least one source coordinate"
      case DuplicateSupportKey(key) =>
        s"measurement support contains duplicate source key '$key'"
      case UnknownSupportKey(key) =>
        s"measurement support key '$key' is not present in the source axis"
      case InvalidWeight(key, value) =>
        s"measurement weight for '$key' must be finite and non-zero, obtained $value"
      case Semantic(error) =>
        error.message
      case SourceIdentityMismatch(expected, actual) =>
        s"measurement source ${actual.value} does not match evidence source ${expected.value}"
      case NominalSourceWitnessMismatch =>
        "measurement and evidence use different nominal source witnesses"
      case EmptyFrame =>
        "measurement frame must contain at least one measurement"
      case DuplicateMeasurementId(id) =>
        s"measurement frame contains duplicate id '${id.value}'"

/** A fixed linear measurement from one exact neural space into a local scientific space. Learned projections remain
  * workflow artifacts until a design-owned compiler can authorize their fit scope.
  */
final class Measurement[
    Source <: SemanticSpace,
    SourceKey,
    LocalKey
] private (
    val source: AxisRef.Aux[SourceKey, Source],
    val local: AxisRef[LocalKey],
    val identity: MeasurementIdentity,
    private[mvpa] val operator: DoubleLinearOperator
)(
    val leg: Lin[Primal[Source], Primal[local.Id]]
)

object Measurement:
  private val Protocol = "scalafim-mvpa-measurement/v1"
  private val WeightedRegionProtocol = "scalafim-mvpa-weighted-region/v1"

  def identity[K](
      source: AxisRef[K],
      id: MeasurementId
  ): Either[MeasurementError, Measurement[source.Id, K, K]] =
    build(
      source,
      source,
      id,
      MeasurementKind.Identity,
      Sparse.identity(source.size)
    ): writer =>
      writer.int(source.size)

  def hardSelection[K](
      source: AxisRef[K],
      id: MeasurementId,
      selection: Injection
  )(using
      AxisKeyCodec[K]
  ): Either[
    MeasurementError,
    Measurement[source.Id, K, K]
  ] =
    ReindexingLeg
      .injection(source, selection)
      .left
      .map(MeasurementError.Axis.apply)
      .map: relation =>
        val identity = measurementIdentity(
          id,
          source.identity,
          relation.child.identity,
          MeasurementKind.HardSelection
        ): writer =>
          writer.int(selection.domain)
          selection.foreachIndex(writer.int)
        new Measurement[source.Id, K, K](
          source,
          relation.child,
          identity,
          relation.operator
        )(relation.leg)

  def weightedRegion[K](
      source: AxisRef[K],
      id: MeasurementId,
      support: Seq[(K, Double)]
  )(using
      codec: AxisKeyCodec[K]
  ): Either[
    MeasurementError,
    Measurement[source.Id, K, FeatureId]
  ] =
    canonicalSupport(source, support).flatMap: entries =>
      val supportDigest = weightedSupportDigest(source.identity, id, entries)
      val localKey = FeatureId.unsafe(s"measurement-${id.value}")
      val local = AxisRef
        .create(
          AxisId.unsafe(s"weighted-$supportDigest"),
          AxisPurpose.NeuralFeatures,
          Vector(localKey),
          CoordinateBasis.unsafe(
            "weighted-region",
            "measurement" -> id.value,
            "support-digest" -> supportDigest,
            "source" -> source.identity.fingerprint.value
          ),
          source.identity.units,
          source.identity.scale,
          CoordinateProvenance.unsafe(
            "scalafim-measurement",
            WeightedRegionProtocol,
            source.identity.fingerprint.value
          )
        )
        .left
        .map(MeasurementError.Axis.apply)

      local.flatMap: localAxis =>
        val builder = Sparse.coo(1, source.size)
        entries.foreach(entry => builder.add(0, entry._1, entry._3))
        val operator = builder.toCSR()
        build(
          source,
          localAxis,
          id,
          MeasurementKind.WeightedRegion,
          operator
        ): writer =>
          writeWeightedSupport(writer, entries)

  def fixedProjection[K, L](
      source: AxisRef[K],
      local: AxisRef[L],
      id: MeasurementId,
      weights: DMat
  ): Either[MeasurementError, Measurement[source.Id, K, L]] =
    buildDense(
      source,
      local,
      id,
      MeasurementKind.FixedProjection,
      weights
    )

  private def buildDense[N <: SemanticSpace, K, L](
      source: AxisRef.Aux[K, N],
      local: AxisRef[L],
      id: MeasurementId,
      kind: MeasurementKind,
      weights: DMat
  ): Either[MeasurementError, Measurement[N, K, L]] =
    validateDense(source, local, weights).flatMap: _ =>
      build(source, local, id, kind, weights): writer =>
        writer.int(weights.rows)
        writer.int(weights.cols)
        var row = 0
        while row < weights.rows do
          var column = 0
          while column < weights.cols do
            writer.double(weights(row, column))
            column += 1
          row += 1

  private def build[N <: SemanticSpace, K, L](
      source: AxisRef.Aux[K, N],
      local: AxisRef[L],
      id: MeasurementId,
      kind: MeasurementKind,
      operator: DoubleLinearOperator
  )(
      payload: CanonicalWriter => Unit
  ): Either[MeasurementError, Measurement[N, K, L]] =
    val identity =
      measurementIdentity(id, source.identity, local.identity, kind)(payload)
    Lin
      .fromLinearMap(
        operator,
        CoordinateEvidence.primal(source.evidence),
        CoordinateEvidence.primal(local.evidence),
        ValueIdentity.source(
          ValueId.unsafe(identity.fingerprint.value)
        ),
        SemanticProvenance.source("fixed-measurement")
      )
      .left
      .map(MeasurementError.Semantic.apply)
      .map: linear =>
        new Measurement[N, K, L](
          source,
          local,
          identity,
          operator
        )(linear)

  private def validateDense(
      source: AxisRef[?],
      local: AxisRef[?],
      weights: DMat
  ): Either[MeasurementError, Unit] =
    if weights.rows != local.size || weights.cols != source.size then
      Left(
        MeasurementError.ShapeMismatch(
          local.size,
          source.size,
          weights.rows,
          weights.cols
        )
      )
    else
      var row = 0
      while row < weights.rows do
        var column = 0
        while column < weights.cols do
          val value = weights(row, column)
          if !value.isFinite then
            return Left(
              MeasurementError.InvalidWeight(
                s"row-major-${row * weights.cols + column}",
                value
              )
            )
          column += 1
        row += 1
      Right(())

  private def canonicalSupport[K](
      source: AxisRef[K],
      support: Seq[(K, Double)]
  )(using codec: AxisKeyCodec[K]): Either[MeasurementError, Vector[(Int, K, Double)]] =
    if support.isEmpty then Left(MeasurementError.EmptySupport)
    else
      val seen = scala.collection.mutable.HashSet.empty[Int]
      val out = Vector.newBuilder[(Int, K, Double)]
      val iterator = support.iterator
      while iterator.hasNext do
        val (key, weight) = iterator.next()
        val encoded = codec.encode(key).value
        source.positionOf(key) match
          case None =>
            return Left(MeasurementError.UnknownSupportKey(encoded))
          case Some(position) =>
            if seen.contains(position) then return Left(MeasurementError.DuplicateSupportKey(encoded))
            if !weight.isFinite || weight == 0.0 then return Left(MeasurementError.InvalidWeight(encoded, weight))
            seen += position
            out += ((position, key, canonicalZero(weight)))
      Right(out.result().sortBy(_._1))

  private def weightedSupportDigest[K](
      source: AxisIdentity,
      id: MeasurementId,
      entries: Vector[(Int, K, Double)]
  ): String =
    val writer = CanonicalWriter()
    writer.string(WeightedRegionProtocol)
    writer.string(source.fingerprint.value)
    writer.string(id.value)
    writeWeightedSupport(writer, entries)
    AxisDigest.sha256Hex(writer.result())

  private def writeWeightedSupport[K](
      writer: CanonicalWriter,
      entries: Vector[(Int, K, Double)]
  ): Unit =
    writer.int(entries.length)
    entries.foreach: entry =>
      writer.int(entry._1)
      writer.double(entry._3)

  private def measurementIdentity(
      id: MeasurementId,
      source: AxisIdentity,
      local: AxisIdentity,
      kind: MeasurementKind
  )(
      payload: CanonicalWriter => Unit
  ): MeasurementIdentity =
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.string(id.value)
    writer.string(source.fingerprint.value)
    writer.string(local.fingerprint.value)
    writer.string(kind.label)
    payload(writer)
    MeasurementIdentity(
      id,
      MeasurementFingerprint.fromDigest(AxisDigest.sha256Hex(writer.result())),
      source.fingerprint,
      local.fingerprint,
      kind
    )

  private def canonicalZero(value: Double): Double =
    if value == 0.0 then 0.0 else value

final class MeasurementEntry[
    Source <: SemanticSpace,
    SourceKey,
    LocalKey,
    +Rendition
] private (
    val measurement: Measurement[Source, SourceKey, LocalKey],
    val rendition: Rendition
)

object MeasurementEntry:
  def apply[N <: SemanticSpace, K, L, R](
      measurement: Measurement[N, K, L],
      rendition: R
  ): MeasurementEntry[N, K, L, R] =
    new MeasurementEntry(measurement, rendition)

case object NoRendition

final case class MeasurementFrameIdentity(
    source: AxisFingerprint,
    measurements: Vector[MeasurementFingerprint]
)

final class MeasurementFrame[
    Source <: SemanticSpace,
    SourceKey,
    +Rendition
] private (
    val source: AxisRef.Aux[SourceKey, Source],
    val entries: Vector[MeasurementEntry[Source, SourceKey, ?, Rendition]],
    val identity: MeasurementFrameIdentity
):
  def size: Int =
    entries.length

object MeasurementFrame:
  def apply[K, R](
      source: AxisRef[K]
  )(
      entries: Seq[MeasurementEntry[source.Id, K, ?, R]]
  ): Either[MeasurementError, MeasurementFrame[source.Id, K, R]] =
    if entries.isEmpty then Left(MeasurementError.EmptyFrame)
    else
      val ordered = entries.toVector.sortBy: entry =>
        (
          entry.measurement.identity.id.value,
          entry.measurement.identity.fingerprint.value
        )
      var index = 0
      while index < ordered.length do
        val measurement = ordered(index).measurement
        if measurement.source.identity != source.identity then
          return Left(
            MeasurementError.SourceIdentityMismatch(
              source.identity.fingerprint,
              measurement.source.identity.fingerprint
            )
          )
        if !(measurement.source.evidence eq source.evidence) then
          return Left(MeasurementError.NominalSourceWitnessMismatch)
        if index > 0 && ordered(index - 1).measurement.identity.id == measurement.identity.id then
          return Left(MeasurementError.DuplicateMeasurementId(measurement.identity.id))
        index += 1
      Right(
        new MeasurementFrame(
          source,
          ordered,
          MeasurementFrameIdentity(
            source.identity.fingerprint,
            ordered.map(_.measurement.identity.fingerprint)
          )
        )
      )
