package scalafim.fmri.mvpa

enum ScientificIdentityError:
  case InvalidText(error: AxisIdentityError)
  case EmptySourceAxes
  case DuplicateSourceAxis(name: ScientificAxisName)
  case DuplicateDesignAxis(name: ScientificAxisName)
  case UnknownDesignAxis(name: ScientificAxisName)
  case DesignAxisMismatch(
      name: ScientificAxisName,
      expected: AxisFingerprint,
      actual: AxisFingerprint
  )
  case EmptyRequestedBoundaries
  case DuplicateOutputBoundary(id: OutputBoundaryId)
  case InvalidComponentFingerprint(value: String)
  case InvalidPlanFingerprint(value: String)

  def message: String =
    this match
      case InvalidText(error) => error.message
      case EmptySourceAxes    =>
        "scientific source identity must contain at least one identified axis"
      case DuplicateSourceAxis(name) =>
        s"scientific source contains duplicate axis name '${name.value}'"
      case DuplicateDesignAxis(name) =>
        s"scientific design contains duplicate axis reference '${name.value}'"
      case UnknownDesignAxis(name) =>
        s"scientific design references unknown source axis '${name.value}'"
      case DesignAxisMismatch(name, expected, actual) =>
        s"scientific design axis '${name.value}' is ${actual.value}, expected ${expected.value}"
      case EmptyRequestedBoundaries =>
        "scientific specification must request at least one output boundary"
      case DuplicateOutputBoundary(id) =>
        s"scientific specification contains duplicate output boundary '${id.value}'"
      case InvalidComponentFingerprint(value) =>
        s"invalid scientific component fingerprint '$value'"
      case InvalidPlanFingerprint(value) =>
        s"invalid scientific plan fingerprint '$value'"

opaque type ScientificAxisName = String

object ScientificAxisName:
  def apply(value: String): Either[ScientificIdentityError, ScientificAxisName] =
    ScientificIdentityText.lowerIdentifier("scientific axis name", value)

  private[mvpa] def unsafe(value: String): ScientificAxisName =
    value

  extension (name: ScientificAxisName) inline def value: String = name

opaque type ScientificSourceKind = String

object ScientificSourceKind:
  def apply(value: String): Either[ScientificIdentityError, ScientificSourceKind] =
    ScientificIdentityText.lowerIdentifier("scientific source kind", value)

  private[mvpa] def unsafe(value: String): ScientificSourceKind =
    value

  extension (kind: ScientificSourceKind) inline def value: String = kind

opaque type DesignKind = String

object DesignKind:
  def apply(value: String): Either[ScientificIdentityError, DesignKind] =
    ScientificIdentityText.lowerIdentifier("design kind", value)

  private[mvpa] def unsafe(value: String): DesignKind =
    value

  extension (kind: DesignKind) inline def value: String = kind

opaque type NormalizationStepId = String

object NormalizationStepId:
  def apply(value: String): Either[ScientificIdentityError, NormalizationStepId] =
    ScientificIdentityText.lowerIdentifier("normalization step id", value)

  private[mvpa] def unsafe(value: String): NormalizationStepId =
    value

  extension (id: NormalizationStepId) inline def value: String = id

opaque type OutputBoundaryId = String

object OutputBoundaryId:
  def apply(value: String): Either[ScientificIdentityError, OutputBoundaryId] =
    ScientificIdentityText.lowerIdentifier("output boundary id", value)

  private[mvpa] def unsafe(value: String): OutputBoundaryId =
    value

  extension (id: OutputBoundaryId) inline def value: String = id

opaque type ScientificComponentFingerprint = String

object ScientificComponentFingerprint:
  private val Prefix = "scalafim-mvpa-component-v1-"

  def apply(value: String): Either[ScientificIdentityError, ScientificComponentFingerprint] =
    val digest = value.stripPrefix(Prefix)
    if value.startsWith(Prefix) && digest.length == 64 && digest.forall(AxisText.isLowerHexDigit) then Right(value)
    else Left(ScientificIdentityError.InvalidComponentFingerprint(value))

  private[mvpa] def fromDigest(digest: String): ScientificComponentFingerprint =
    Prefix + digest

  extension (fingerprint: ScientificComponentFingerprint) inline def value: String = fingerprint

opaque type ScientificPlanFingerprint = String

object ScientificPlanFingerprint:
  private val Prefix = "scalafim-mvpa-plan-v1-"

  def apply(value: String): Either[ScientificIdentityError, ScientificPlanFingerprint] =
    val digest = value.stripPrefix(Prefix)
    if value.startsWith(Prefix) && digest.length == 64 && digest.forall(AxisText.isLowerHexDigit) then Right(value)
    else Left(ScientificIdentityError.InvalidPlanFingerprint(value))

  private[mvpa] def fromDigest(digest: String): ScientificPlanFingerprint =
    Prefix + digest

  extension (fingerprint: ScientificPlanFingerprint) inline def value: String = fingerprint

final case class ScientificSourceAxis(
    name: ScientificAxisName,
    identity: AxisIdentity
)

final class ScientificSourceIdentity private (
    val kind: ScientificSourceKind,
    val axes: Vector[ScientificSourceAxis],
    val fields: Vector[AxisDescriptorField],
    val fingerprint: ScientificComponentFingerprint
):
  def axis(name: ScientificAxisName): Option[AxisIdentity] =
    axes.find(_.name == name).map(_.identity)

  def canonicalEncoding: IArray[Byte] =
    IArray.unsafeFromArray(ScientificSourceIdentity.canonicalBytes(kind, axes, fields))

  def canonicalHex: String =
    AxisDigest.hex(ScientificSourceIdentity.canonicalBytes(kind, axes, fields))

  override def equals(other: Any): Boolean =
    other match
      case that: ScientificSourceIdentity =>
        kind == that.kind &&
        axes == that.axes &&
        fields == that.fields &&
        fingerprint == that.fingerprint
      case _ => false

  override def hashCode(): Int =
    fingerprint.hashCode

  override def toString: String =
    s"ScientificSourceIdentity(${kind.value},${fingerprint.value})"

object ScientificSourceIdentity:
  val Protocol = "scalafim-mvpa-source/v1"

  def apply(
      kind: ScientificSourceKind,
      axes: Seq[ScientificSourceAxis],
      fields: Seq[(String, String)] = Vector.empty
  ): Either[ScientificIdentityError, ScientificSourceIdentity] =
    val orderedAxes = axes.toVector.sortBy(_.name.value)
    if orderedAxes.isEmpty then Left(ScientificIdentityError.EmptySourceAxes)
    else
      orderedAxes
        .sliding(2)
        .collectFirst:
          case Vector(left, right) if left.name == right.name => left.name
      match
        case Some(duplicate) => Left(ScientificIdentityError.DuplicateSourceAxis(duplicate))
        case None            =>
          ScientificIdentityComponents
            .fields(fields)
            .map: canonicalFields =>
              val bytes = canonicalBytes(kind, orderedAxes, canonicalFields)
              new ScientificSourceIdentity(
                kind,
                orderedAxes,
                canonicalFields,
                ScientificComponentFingerprint.fromDigest(AxisDigest.sha256Hex(bytes))
              )

  private def canonicalBytes(
      kind: ScientificSourceKind,
      axes: Vector[ScientificSourceAxis],
      fields: Vector[AxisDescriptorField]
  ): Array[Byte] =
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.string(kind.value)
    writer.int(axes.length)
    axes.foreach: axis =>
      writer.string(axis.name.value)
      writer.string(axis.identity.fingerprint.value)
    writer.fields(fields)
    writer.result()

final class DesignIdentity private (
    val kind: DesignKind,
    val fields: Vector[AxisDescriptorField],
    val fingerprint: ScientificComponentFingerprint
):
  def canonicalEncoding: IArray[Byte] =
    IArray.unsafeFromArray(DesignIdentity.canonicalBytes(kind, fields))

  def canonicalHex: String =
    AxisDigest.hex(DesignIdentity.canonicalBytes(kind, fields))

  override def equals(other: Any): Boolean =
    other match
      case that: DesignIdentity =>
        kind == that.kind && fields == that.fields && fingerprint == that.fingerprint
      case _ => false

  override def hashCode(): Int =
    fingerprint.hashCode

object DesignIdentity:
  val Protocol = "scalafim-mvpa-design/v1"

  def apply(
      kind: DesignKind,
      fields: Seq[(String, String)] = Vector.empty
  ): Either[ScientificIdentityError, DesignIdentity] =
    ScientificIdentityComponents
      .fields(fields)
      .map: canonicalFields =>
        val bytes = canonicalBytes(kind, canonicalFields)
        new DesignIdentity(
          kind,
          canonicalFields,
          ScientificComponentFingerprint.fromDigest(AxisDigest.sha256Hex(bytes))
        )

  private def canonicalBytes(
      kind: DesignKind,
      fields: Vector[AxisDescriptorField]
  ): Array[Byte] =
    ScientificIdentityComponents.canonicalBytes(Protocol, kind.value, fields)

final case class DesignAxisReference(
    name: ScientificAxisName,
    identity: AxisIdentity
)

final class NormalizationStepIdentity private (
    val id: NormalizationStepId,
    val fields: Vector[AxisDescriptorField],
    val fingerprint: ScientificComponentFingerprint
):
  def canonicalEncoding: IArray[Byte] =
    IArray.unsafeFromArray(NormalizationStepIdentity.canonicalBytes(id, fields))

  override def equals(other: Any): Boolean =
    other match
      case that: NormalizationStepIdentity =>
        id == that.id && fields == that.fields && fingerprint == that.fingerprint
      case _ => false

  override def hashCode(): Int =
    fingerprint.hashCode

object NormalizationStepIdentity:
  val Protocol = "scalafim-mvpa-normalization-step/v1"

  def apply(
      id: NormalizationStepId,
      fields: Seq[(String, String)] = Vector.empty
  ): Either[ScientificIdentityError, NormalizationStepIdentity] =
    ScientificIdentityComponents
      .fields(fields)
      .map: canonicalFields =>
        val bytes = canonicalBytes(id, canonicalFields)
        new NormalizationStepIdentity(
          id,
          canonicalFields,
          ScientificComponentFingerprint.fromDigest(AxisDigest.sha256Hex(bytes))
        )

  private def canonicalBytes(
      id: NormalizationStepId,
      fields: Vector[AxisDescriptorField]
  ): Array[Byte] =
    ScientificIdentityComponents.canonicalBytes(Protocol, id.value, fields)

final class NormalizationIdentity private (
    val steps: Vector[NormalizationStepIdentity],
    val fingerprint: ScientificComponentFingerprint
):
  def canonicalEncoding: IArray[Byte] =
    IArray.unsafeFromArray(NormalizationIdentity.canonicalBytes(steps))

  override def equals(other: Any): Boolean =
    other match
      case that: NormalizationIdentity =>
        steps == that.steps && fingerprint == that.fingerprint
      case _ => false

  override def hashCode(): Int =
    fingerprint.hashCode

object NormalizationIdentity:
  val Protocol = "scalafim-mvpa-normalization/v1"

  val none: NormalizationIdentity =
    apply(Vector.empty)

  def apply(steps: Seq[NormalizationStepIdentity]): NormalizationIdentity =
    val values = steps.toVector
    val bytes = canonicalBytes(values)
    new NormalizationIdentity(
      values,
      ScientificComponentFingerprint.fromDigest(AxisDigest.sha256Hex(bytes))
    )

  private def canonicalBytes(steps: Vector[NormalizationStepIdentity]): Array[Byte] =
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.int(steps.length)
    steps.foreach(step => writer.string(step.fingerprint.value))
    writer.result()

final class OutputBoundaryIdentity private (
    val id: OutputBoundaryId,
    val fields: Vector[AxisDescriptorField],
    val fingerprint: ScientificComponentFingerprint
):
  def canonicalEncoding: IArray[Byte] =
    IArray.unsafeFromArray(OutputBoundaryIdentity.canonicalBytes(id, fields))

  override def equals(other: Any): Boolean =
    other match
      case that: OutputBoundaryIdentity =>
        id == that.id && fields == that.fields && fingerprint == that.fingerprint
      case _ => false

  override def hashCode(): Int =
    fingerprint.hashCode

object OutputBoundaryIdentity:
  val Protocol = "scalafim-mvpa-output-boundary/v1"

  def apply(
      id: OutputBoundaryId,
      fields: Seq[(String, String)] = Vector.empty
  ): Either[ScientificIdentityError, OutputBoundaryIdentity] =
    ScientificIdentityComponents
      .fields(fields)
      .map: canonicalFields =>
        val bytes = canonicalBytes(id, canonicalFields)
        new OutputBoundaryIdentity(
          id,
          canonicalFields,
          ScientificComponentFingerprint.fromDigest(AxisDigest.sha256Hex(bytes))
        )

  private[mvpa] def trusted(
      id: OutputBoundaryId,
      fields: Seq[(String, String)] = Vector.empty
  ): OutputBoundaryIdentity =
    val canonicalFields = ScientificIdentityComponents.trustedFields(fields)
    val bytes = canonicalBytes(id, canonicalFields)
    new OutputBoundaryIdentity(
      id,
      canonicalFields,
      ScientificComponentFingerprint.fromDigest(AxisDigest.sha256Hex(bytes))
    )

  private def canonicalBytes(
      id: OutputBoundaryId,
      fields: Vector[AxisDescriptorField]
  ): Array[Byte] =
    ScientificIdentityComponents.canonicalBytes(Protocol, id.value, fields)

final class RequestedBoundaries private (
    val values: Vector[OutputBoundaryIdentity],
    val fingerprint: ScientificComponentFingerprint
):
  override def equals(other: Any): Boolean =
    other match
      case that: RequestedBoundaries =>
        values == that.values && fingerprint == that.fingerprint
      case _ => false

  override def hashCode(): Int =
    fingerprint.hashCode

object RequestedBoundaries:
  val Protocol = "scalafim-mvpa-requested-boundaries/v1"

  def apply(
      values: Seq[OutputBoundaryIdentity]
  ): Either[ScientificIdentityError, RequestedBoundaries] =
    val ordered = values.toVector.sortBy(value => (value.id.value, value.fingerprint.value))
    if ordered.isEmpty then Left(ScientificIdentityError.EmptyRequestedBoundaries)
    else
      ordered
        .sliding(2)
        .collectFirst:
          case Vector(left, right) if left.id == right.id => left.id
      match
        case Some(duplicate) => Left(ScientificIdentityError.DuplicateOutputBoundary(duplicate))
        case None            =>
          val writer = CanonicalWriter()
          writer.string(Protocol)
          writer.int(ordered.length)
          ordered.foreach(value => writer.string(value.fingerprint.value))
          val fingerprint = ScientificComponentFingerprint.fromDigest(
            AxisDigest.sha256Hex(writer.result())
          )
          Right(new RequestedBoundaries(ordered, fingerprint))

  private[mvpa] def trusted(
      values: Seq[OutputBoundaryIdentity]
  ): RequestedBoundaries =
    val ordered = values.toVector.sortBy(value => (value.id.value, value.fingerprint.value))
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.int(ordered.length)
    ordered.foreach(value => writer.string(value.fingerprint.value))
    new RequestedBoundaries(
      ordered,
      ScientificComponentFingerprint.fromDigest(AxisDigest.sha256Hex(writer.result()))
    )

final class ScientificPlanIdentity private (
    val source: ScientificSourceIdentity,
    val design: DesignIdentity,
    val designAxes: Vector[DesignAxisReference],
    val frame: MeasurementFrameIdentity,
    val estimand: EstimandIdentity,
    val normalization: NormalizationIdentity,
    val boundaries: RequestedBoundaries,
    val fingerprint: ScientificPlanFingerprint
):
  def canonicalEncoding: IArray[Byte] =
    IArray.unsafeFromArray(
      ScientificPlanIdentity.canonicalBytes(
        source,
        design,
        designAxes,
        frame,
        estimand,
        normalization,
        boundaries
      )
    )

  def canonicalHex: String =
    AxisDigest.hex(
      ScientificPlanIdentity.canonicalBytes(
        source,
        design,
        designAxes,
        frame,
        estimand,
        normalization,
        boundaries
      )
    )

  override def equals(other: Any): Boolean =
    other match
      case that: ScientificPlanIdentity =>
        source == that.source &&
        design == that.design &&
        designAxes == that.designAxes &&
        frame == that.frame &&
        estimand == that.estimand &&
        normalization == that.normalization &&
        boundaries == that.boundaries &&
        fingerprint == that.fingerprint
      case _ => false

  override def hashCode(): Int =
    fingerprint.hashCode

  override def toString: String =
    s"ScientificPlanIdentity(${fingerprint.value})"

object ScientificPlanIdentity:
  val Protocol = "scalafim-mvpa-scientific-plan/v1"

  /** Construct canonical plan-identity data after checking that every design axis is an exact source-axis reference.
    * This computes identity only; it does not admit a [[BoundScientificPlan]].
    */
  def fromSpecification(
      source: ScientificSourceIdentity,
      design: DesignIdentity,
      designAxes: Seq[DesignAxisReference],
      frame: MeasurementFrameIdentity,
      estimand: EstimandIdentity,
      normalization: NormalizationIdentity,
      boundaries: RequestedBoundaries
  ): Either[ScientificIdentityError, ScientificPlanIdentity] =
    val orderedAxes = designAxes.toVector.sortBy(_.name.value)
    orderedAxes
      .sliding(2)
      .collectFirst:
        case Vector(left, right) if left.name == right.name => left.name
    match
      case Some(duplicate) => Left(ScientificIdentityError.DuplicateDesignAxis(duplicate))
      case None            =>
        validateSourceAxes(source, orderedAxes).map: _ =>
          val bytes = canonicalBytes(
            source,
            design,
            orderedAxes,
            frame,
            estimand,
            normalization,
            boundaries
          )
          new ScientificPlanIdentity(
            source,
            design,
            orderedAxes,
            frame,
            estimand,
            normalization,
            boundaries,
            ScientificPlanFingerprint.fromDigest(AxisDigest.sha256Hex(bytes))
          )

  private def validateSourceAxes(
      source: ScientificSourceIdentity,
      references: Vector[DesignAxisReference]
  ): Either[ScientificIdentityError, Unit] =
    val iterator = references.iterator
    while iterator.hasNext do
      val reference = iterator.next()
      source.axis(reference.name) match
        case None =>
          return Left(ScientificIdentityError.UnknownDesignAxis(reference.name))
        case Some(expected) if expected != reference.identity =>
          return Left(
            ScientificIdentityError.DesignAxisMismatch(
              reference.name,
              expected.fingerprint,
              reference.identity.fingerprint
            )
          )
        case Some(_) => ()
    Right(())

  private def canonicalBytes(
      source: ScientificSourceIdentity,
      design: DesignIdentity,
      designAxes: Vector[DesignAxisReference],
      frame: MeasurementFrameIdentity,
      estimand: EstimandIdentity,
      normalization: NormalizationIdentity,
      boundaries: RequestedBoundaries
  ): Array[Byte] =
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.string(source.fingerprint.value)
    writer.string(design.fingerprint.value)
    writer.int(designAxes.length)
    designAxes.foreach: axis =>
      writer.string(axis.name.value)
      writer.string(axis.identity.fingerprint.value)
    writer.string(frame.source.value)
    writer.int(frame.measurements.length)
    frame.measurements.foreach(value => writer.string(value.value))
    writer.string(estimand.fingerprint.value)
    writer.string(normalization.fingerprint.value)
    writer.string(boundaries.fingerprint.value)
    writer.result()

private[mvpa] object ScientificIdentityComponents:
  def fields(
      values: Seq[(String, String)]
  ): Either[ScientificIdentityError, Vector[AxisDescriptorField]] =
    CoordinateBasis
      .descriptorFields(values)
      .left
      .map(ScientificIdentityError.InvalidText.apply)

  def trustedFields(
      values: Seq[(String, String)]
  ): Vector[AxisDescriptorField] =
    values.iterator
      .map((name, value) => AxisDescriptorField.unsafe(name, value))
      .toVector
      .sortBy(_.name)

  def canonicalBytes(
      protocol: String,
      kind: String,
      fields: Vector[AxisDescriptorField]
  ): Array[Byte] =
    val writer = CanonicalWriter()
    writer.string(protocol)
    writer.string(kind)
    writer.fields(fields)
    writer.result()

private[mvpa] object ScientificIdentityText:
  def lowerIdentifier(
      kind: String,
      value: String
  ): Either[ScientificIdentityError, String] =
    AxisText
      .identifier(kind, value)
      .flatMap: valid =>
        if valid == valid.toLowerCase then Right(valid)
        else Left(AxisIdentityError.InvalidIdentifier(kind, value, "must be lowercase"))
      .left
      .map(ScientificIdentityError.InvalidText.apply)
