package scalafim.fmri.mvpa

import multivar.core.SemanticSpace

opaque type EstimandKind = String

object EstimandKind:
  def apply(value: String): Either[ScientificIdentityError, EstimandKind] =
    ScientificIdentityText.lowerIdentifier("estimand kind", value)

  private[mvpa] def unsafe(value: String): EstimandKind =
    value

  extension (kind: EstimandKind) inline def value: String = kind

final class EstimandIdentity private (
    val kind: EstimandKind,
    val fields: Vector[AxisDescriptorField],
    val fingerprint: ScientificComponentFingerprint
):
  def canonicalEncoding: IArray[Byte] =
    IArray.unsafeFromArray(EstimandIdentity.canonicalBytes(kind, fields))

  def canonicalHex: String =
    AxisDigest.hex(EstimandIdentity.canonicalBytes(kind, fields))

  override def equals(other: Any): Boolean =
    other match
      case that: EstimandIdentity =>
        kind == that.kind && fields == that.fields && fingerprint == that.fingerprint
      case _ => false

  override def hashCode(): Int =
    fingerprint.hashCode

  override def toString: String =
    s"EstimandIdentity(${kind.value},${fingerprint.value})"

object EstimandIdentity:
  val Protocol = "scalafim-mvpa-estimand/v1"

  def apply(
      kind: EstimandKind,
      fields: Seq[(String, String)] = Vector.empty
  ): Either[ScientificIdentityError, EstimandIdentity] =
    ScientificIdentityComponents
      .fields(fields)
      .map: canonicalFields =>
        val bytes = canonicalBytes(kind, canonicalFields)
        new EstimandIdentity(
          kind,
          canonicalFields,
          ScientificComponentFingerprint.fromDigest(AxisDigest.sha256Hex(bytes))
        )

  private[mvpa] def trusted(
      kind: EstimandKind,
      fields: Seq[(String, String)] = Vector.empty
  ): EstimandIdentity =
    val canonicalFields = ScientificIdentityComponents.trustedFields(fields)
    val bytes = canonicalBytes(kind, canonicalFields)
    new EstimandIdentity(
      kind,
      canonicalFields,
      ScientificComponentFingerprint.fromDigest(AxisDigest.sha256Hex(bytes))
    )

  private def canonicalBytes(
      kind: EstimandKind,
      fields: Vector[AxisDescriptorField]
  ): Array[Byte] =
    ScientificIdentityComponents.canonicalBytes(Protocol, kind.value, fields)

/** A scientifically identified evidence source. Its numerical representation is deliberately absent: dense storage,
  * matrix-free operators, chunking, and materialization belong to execution planning.
  */
trait ScientificSource:
  type Neural <: SemanticSpace
  type NeuralKey

  def identity: ScientificSourceIdentity
  def neuralAxisName: ScientificAxisName
  def neuralAxis: AxisRef.Aux[NeuralKey, Neural]

/** Scientific design semantics plus the exact source axes to which they are bound. Validation, pairing, fitting, and
  * randomization remain separate implementations rather than cases of a universal fold enum.
  */
trait EvidenceDesign:
  def identity: DesignIdentity
  def referencedAxes: Vector[DesignAxisReference]

/** An open scientific question with its own success and failure vocabulary. Source and design compatibility is checked
  * statically at specification construction; no central analysis or payload registry is involved.
  */
trait Estimand[-Source <: ScientificSource, -Design <: EvidenceDesign]:
  type Result
  type Rejection
  type Failure

  def identity: EstimandIdentity
  def defaultBoundaries: RequestedBoundaries
  def rejectionMessage(value: Rejection): String
  def failureMessage(value: Failure): String
