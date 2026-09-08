package scalafim.fmri.mvpa

import multivar.core.MultivarError
import multivar.core.SemanticSpace
import multivar.core.SemanticError
import multivar.core.SpaceEvidence
import multivar.core.SpaceRef
import multivar.core.SpaceRole
import resample4s.core.Permutation

import scala.reflect.ClassTag

enum AxisRefError:
  case InvalidIdentity(error: AxisIdentityError)
  case InvalidMultivarSpace(error: MultivarError)
  case KeyCountMismatch(expected: Int, actual: Int)
  case DuplicateSemanticKey(key: String)
  case KeyMismatch(position: Int, expected: AxisKey, actual: AxisKey)
  case PositionOutOfBounds(position: Int, size: Int)
  case InvalidReindexing(detail: String)
  case ReindexingCodomainMismatch(expected: Int, actual: Int)
  case InvalidSemanticOperator(error: SemanticError)
  case RuntimeIdentityMismatch(expected: AxisFingerprint, actual: AxisFingerprint)

  def message: String =
    this match
      case InvalidIdentity(error) =>
        error.message
      case InvalidMultivarSpace(error) =>
        error.message
      case KeyCountMismatch(expected, actual) =>
        s"axis identity contains $expected keys, but the typed index contains $actual"
      case DuplicateSemanticKey(key) =>
        s"axis index contains duplicate semantic key '$key'"
      case KeyMismatch(position, expected, actual) =>
        s"axis key at position $position is '${actual.value}', expected '${expected.value}'"
      case PositionOutOfBounds(position, size) =>
        s"axis position $position is outside [0, $size)"
      case InvalidReindexing(detail) =>
        s"invalid axis reindexing: $detail"
      case ReindexingCodomainMismatch(expected, actual) =>
        s"reindexing population has size $actual, expected axis size $expected"
      case InvalidSemanticOperator(error) =>
        error.message
      case RuntimeIdentityMismatch(expected, actual) =>
        s"runtime axis ${actual.value} does not match nominal axis ${expected.value}"

opaque type SampleId = String

object SampleId:
  def apply(value: String): Either[AxisIdentityError, SampleId] =
    AxisKey(value).map(_.value)

  private[mvpa] def unsafe(value: String): SampleId =
    value

  extension (id: SampleId) inline def value: String = id

  given ClassTag[SampleId] = ClassTag(classOf[String])

opaque type FeatureId = String

object FeatureId:
  def apply(value: String): Either[AxisIdentityError, FeatureId] =
    AxisKey(value).map(_.value)

  private[mvpa] def unsafe(value: String): FeatureId =
    value

  extension (id: FeatureId) inline def value: String = id

/** Canonical projection from a typed scientific key to the language-neutral coordinate stored in [[AxisIdentity]].
  */
trait AxisKeyCodec[K]:
  def encode(key: K): AxisKey

object AxisKeyCodec:
  given AxisKeyCodec[AxisKey] with
    override def encode(key: AxisKey): AxisKey = key

  given AxisKeyCodec[SampleId] with
    override def encode(key: SampleId): AxisKey = AxisKey.unsafe(key.value)

  given AxisKeyCodec[FeatureId] with
    override def encode(key: FeatureId): AxisKey = AxisKey.unsafe(key.value)

/** Public key lookup never confuses a semantic key with its storage position. The private [[RowOrdinal]] refinement is
  * used by numerical plans inside the MVPA package.
  */
trait AxisIndex[K]:
  def size: Int
  def keys: Vector[K]
  def keyAt(position: Int): Either[AxisRefError, K]
  def positionOf(key: K): Option[Int]

private[mvpa] opaque type RowOrdinal = Int

private[mvpa] object RowOrdinal:
  def from(position: Int, size: Int): Either[AxisRefError, RowOrdinal] =
    if position < 0 || position >= size then Left(AxisRefError.PositionOutOfBounds(position, size))
    else Right(position)

  private[mvpa] def unsafe(position: Int): RowOrdinal =
    position

  extension (ordinal: RowOrdinal) inline def value: Int = ordinal

/** An identified scientific axis and the sole nominal Multivar witness for values bound through this reference.
  */
final class AxisRef[K] private (
    val identity: AxisIdentity,
    override val keys: Vector[K],
    val space: SpaceRef,
    private val ordinals: Map[K, RowOrdinal]
) extends AxisIndex[K]:
  type Id = space.Id

  val evidence: SpaceEvidence[Id] =
    space.evidence

  override def size: Int =
    keys.length

  override def keyAt(position: Int): Either[AxisRefError, K] =
    RowOrdinal.from(position, size).map(keyAtOrdinal)

  override def positionOf(key: K): Option[Int] =
    ordinalOf(key).map(_.value)

  def sameIdentity(other: AxisRef[?]): Boolean =
    identity == other.identity

  /** Validate a decoded runtime descriptor before returning this reference's existing nominal witness. No witness is
    * synthesized from runtime shape.
    */
  def bind(record: AxisIdentityRecord): Either[AxisRefError, SpaceEvidence[Id]] =
    AxisIdentity.decode(record).left.map(AxisRefError.InvalidIdentity.apply).flatMap(bind)

  def bind(decoded: AxisIdentity): Either[AxisRefError, SpaceEvidence[Id]] =
    if decoded == identity then Right(evidence)
    else
      Left(
        AxisRefError.RuntimeIdentityMismatch(
          identity.fingerprint,
          decoded.fingerprint
        )
      )

  /** A total reorder retains semantic keys but derives a child axis whose coordinate order and Multivar witness are
    * distinct.
    */
  def reorder(
      positions: Seq[Int]
  )(using AxisKeyCodec[K]): Either[AxisRefError, ReindexingLeg[Id, K, K, Permutation]] =
    val values = IArray.unsafeFromArray(positions.toArray)
    Permutation
      .from(values)
      .left
      .map(error => AxisRefError.InvalidReindexing(error.message))
      .flatMap(ReindexingLeg.permutation(this, _))

  private[mvpa] def keyAtOrdinal(ordinal: RowOrdinal): K =
    keys(ordinal.value)

  private[mvpa] def ordinalOf(key: K): Option[RowOrdinal] =
    ordinals.get(key)

object AxisRef:
  type Aux[K, S <: SemanticSpace] = AxisRef[K] { type Id = S }

  def apply[K](
      identity: AxisIdentity,
      keys: Seq[K]
  )(using codec: AxisKeyCodec[K]): Either[AxisRefError, AxisRef[K]] =
    val keyVector = keys.toVector
    if keyVector.length != identity.size then Left(AxisRefError.KeyCountMismatch(identity.size, keyVector.length))
    else
      val ordinalBuilder = Map.newBuilder[K, RowOrdinal]
      val seen = scala.collection.mutable.HashSet.empty[K]
      var position = 0
      while position < keyVector.length do
        val key = keyVector(position)
        val encoded = codec.encode(key)
        if seen.contains(key) then return Left(AxisRefError.DuplicateSemanticKey(encoded.value))
        if encoded != identity.orderedKeys(position) then
          return Left(
            AxisRefError.KeyMismatch(
              position,
              identity.orderedKeys(position),
              encoded
            )
          )
        seen += key
        ordinalBuilder += key -> RowOrdinal.unsafe(position)
        position += 1

      multivarSpace(identity).map: multivarRef =>
        new AxisRef(identity, keyVector, multivarRef, ordinalBuilder.result())

  def create[K](
      id: AxisId,
      purpose: AxisPurpose,
      keys: Seq[K],
      basis: CoordinateBasis,
      units: Option[AxisUnits],
      scale: AxisScale,
      coordinateProvenance: CoordinateProvenance
  )(using codec: AxisKeyCodec[K]): Either[AxisRefError, AxisRef[K]] =
    val keyVector = keys.toVector
    val encoded = keyVector.map(codec.encode)
    AxisIdentity(
      id,
      purpose,
      encoded,
      basis,
      units,
      scale,
      coordinateProvenance
    ).left.map(AxisRefError.InvalidIdentity.apply).flatMap(AxisRef(_, keyVector))

  private def multivarSpace(identity: AxisIdentity): Either[AxisRefError, SpaceRef] =
    SpaceRef
      .of(identity.fingerprint.value, multivarRole(identity.purpose), identity.size)
      .left
      .map(AxisRefError.InvalidMultivarSpace.apply)

  private def multivarRole(purpose: AxisPurpose): SpaceRole =
    purpose match
      case AxisPurpose.Samples        => SpaceRole.Samples
      case AxisPurpose.Components     => SpaceRole.Latent
      case AxisPurpose.Partitions     => SpaceRole.Block
      case AxisPurpose.NeuralFeatures => SpaceRole.Observed
      case AxisPurpose.Effects        => SpaceRole.Observed
      case AxisPurpose.Covariates     => SpaceRole.Observed
      case _                          => SpaceRole.Observed
