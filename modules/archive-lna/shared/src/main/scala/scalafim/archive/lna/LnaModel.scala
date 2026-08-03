package scalafim.archive.lna

import scalafim.archive.{ArchiveError, ArchivePath, CreatorId, DatasetShape, RunLabel, TransformName, TransformPort}
import scalafim.image.{DMat, NeuroSpace}

enum LnaVersion(val id: String):
  case V2 extends LnaVersion("LNA R v2.0")

enum LnaDType:
  case Float32, Float64, UInt8, UInt16, Int32

  def bytes: Int =
    this match
      case Float32 => 4
      case Float64 => 8
      case UInt8   => 1
      case UInt16  => 2
      case Int32   => 4

  def isFloat: Boolean =
    this == Float32 || this == Float64

  def isInteger: Boolean =
    this == UInt8 || this == UInt16 || this == Int32

  def integerRange: Option[(Int, Int)] =
    this match
      case UInt8  => Some((0, 255))
      case UInt16 => Some((0, 65535))
      case Int32  => None
      case _      => None

enum DatasetRole:
  case RawData, Quantized, Scale, Offset, BasisMatrix, Coefficients, TemporalBasis, Loadings, SampleOffset, DeltaStream, FirstValues, Mask, Metadata
  case Other(name: String)

  def value: String =
    this match
      case RawData       => "raw_data"
      case Quantized     => "quantized"
      case Scale         => "scale"
      case Offset        => "offset"
      case BasisMatrix   => "basis_matrix"
      case Coefficients  => "coefficients"
      case TemporalBasis => "temporal_basis"
      case Loadings      => "loadings"
      case SampleOffset  => "sample_offset"
      case DeltaStream   => "delta_stream"
      case FirstValues   => "first_values"
      case Mask          => "mask"
      case Metadata      => "metadata"
      case Other(name)   => name

enum TransformKind:
  case Quant, Basis, Embed, Delta, Temporal
  case Custom(name: String)

  def value: String =
    this match
      case Quant        => "quant"
      case Basis        => "basis"
      case Embed        => "embed"
      case Delta        => "delta"
      case Temporal     => "temporal"
      case Custom(name) => name

enum QuantScaleScope:
  case Global, Voxel

enum QuantMethod:
  case Range, Sd

  def value: String =
    this match
      case Range => "range"
      case Sd    => "sd"

enum DeltaAxis:
  case Time, Feature

  def value: String =
    this match
      case Time    => "time"
      case Feature => "feature"

enum DeltaReferenceStorage:
  case FirstValueVerbatim

  def value: String =
    this match
      case FirstValueVerbatim => "first_value_verbatim"

enum DeltaCodingMethod:
  case None

  def value: String =
    this match
      case None => "none"

enum TemporalDctNorm(val value: String):
  case Ortho extends TemporalDctNorm("ortho")
  case None extends TemporalDctNorm("none")

opaque type LnaMetadata = Map[String, String]

object LnaMetadata:
  val Empty: LnaMetadata = Map.empty

  def apply(values: Map[String, String]): Either[ArchiveError, LnaMetadata] =
    if values.keys.exists(_.trim.isEmpty) then Left(ArchiveError.InvalidArchive("metadata keys must be non-empty"))
    else if values.keys.exists(_.exists(_.isControl)) then Left(ArchiveError.InvalidArchive("metadata keys must not contain control characters"))
    else Right(values)

  def unsafe(values: Map[String, String]): LnaMetadata =
    apply(values).fold(err => throw IllegalArgumentException(err.message), identity)

  extension (metadata: LnaMetadata)
    def values: Map[String, String] = metadata
    def get(key: String): Option[String] = metadata.get(key)

opaque type QuantBits = Int

object QuantBits:
  def apply(value: Int): Either[ArchiveError, QuantBits] =
    if value < 1 || value > 16 then Left(ArchiveError.InvalidArchive(s"quant bits must be between 1 and 16, got $value"))
    else Right(value)

  def unsafe(value: Int): QuantBits =
    apply(value).fold(err => throw IllegalArgumentException(err.message), identity)

  extension (bits: QuantBits)
    def value: Int = bits
    def levels: Int = (1 << bits) - 1
    def storageDType: LnaDType =
      if bits <= 8 then LnaDType.UInt8 else LnaDType.UInt16

enum CenteringPolicy:
  case Centered, Uncentered

  def enabled: Boolean =
    this match
      case Centered   => true
      case Uncentered => false

object CenteringPolicy:
  def fromBoolean(value: Boolean): CenteringPolicy =
    if value then CenteringPolicy.Centered else CenteringPolicy.Uncentered

enum ClipPolicy:
  case RejectClipping, AllowClipping

  def allowClip: Boolean =
    this match
      case RejectClipping => false
      case AllowClipping  => true

object ClipPolicy:
  def fromBoolean(value: Boolean): ClipPolicy =
    if value then ClipPolicy.AllowClipping else ClipPolicy.RejectClipping

final case class QuantParams(
    bits: Int = 8,
    method: QuantMethod = QuantMethod.Range,
    center: Boolean = true,
    scaleScope: QuantScaleScope = QuantScaleScope.Global,
    allowClip: Boolean = false
):
  require(bits >= 1 && bits <= 16, "quant bits must be between 1 and 16")

  def quantBits: QuantBits =
    QuantBits.unsafe(bits)

  def centeringPolicy: CenteringPolicy =
    CenteringPolicy.fromBoolean(center)

  def clipPolicy: ClipPolicy =
    ClipPolicy.fromBoolean(allowClip)

object QuantParams:
  def checked(
      bits: Int = 8,
      method: QuantMethod = QuantMethod.Range,
      center: Boolean = true,
      scaleScope: QuantScaleScope = QuantScaleScope.Global,
      allowClip: Boolean = false
  ): Either[ArchiveError, QuantParams] =
    QuantBits(bits).map { value =>
      QuantParams(value.value, method, center, scaleScope, allowClip)
    }

  def typed(
      bits: QuantBits,
      method: QuantMethod = QuantMethod.Range,
      centering: CenteringPolicy = CenteringPolicy.Centered,
      scaleScope: QuantScaleScope = QuantScaleScope.Global,
      clipping: ClipPolicy = ClipPolicy.RejectClipping
  ): QuantParams =
    QuantParams(bits.value, method, centering.enabled, scaleScope, clipping.allowClip)

final case class QuantReport(
    bits: Int,
    method: QuantMethod,
    scaleScope: QuantScaleScope,
    nClippedTotal: Int,
    clipPct: Double
):
  require(bits >= 1 && bits <= 16, "quant report bits must be between 1 and 16")
  require(nClippedTotal >= 0, "clipped sample count must be non-negative")
  require(clipPct.isFinite && clipPct >= 0.0 && clipPct <= 100.0, "clip percentage must be finite and between 0 and 100")

object QuantReport:
  def checked(
      bits: Int,
      method: QuantMethod,
      scaleScope: QuantScaleScope,
      nClippedTotal: Int,
      clipPct: Double
  ): Either[ArchiveError, QuantReport] =
    if nClippedTotal < 0 then Left(ArchiveError.InvalidArchive("clipped sample count must be non-negative"))
    else if !clipPct.isFinite || clipPct < 0.0 || clipPct > 100.0 then Left(ArchiveError.InvalidArchive("clip percentage must be finite and between 0 and 100"))
    else QuantBits(bits).map(value => QuantReport(value.value, method, scaleScope, nClippedTotal, clipPct))

sealed trait TransformReport:
  def kind: TransformKind

object TransformReport:
  final case class Quant(report: QuantReport) extends TransformReport:
    val kind: TransformKind = TransformKind.Quant

final case class DeltaParams(
    order: Int = 1,
    axis: DeltaAxis = DeltaAxis.Time,
    referenceValueStorage: DeltaReferenceStorage = DeltaReferenceStorage.FirstValueVerbatim,
    codingMethod: DeltaCodingMethod = DeltaCodingMethod.None
):
  require(order == 1, "delta currently supports only first-order differences")

object DeltaParams:
  def checked(
      order: Int = 1,
      axis: DeltaAxis = DeltaAxis.Time,
      referenceValueStorage: DeltaReferenceStorage = DeltaReferenceStorage.FirstValueVerbatim,
      codingMethod: DeltaCodingMethod = DeltaCodingMethod.None
  ): Either[ArchiveError, DeltaParams] =
    if order != 1 then Left(ArchiveError.UnsupportedTransform(s"delta order=$order"))
    else Right(DeltaParams(order, axis, referenceValueStorage, codingMethod))

final case class TemporalDctParams(
    components: Int,
    norm: TemporalDctNorm = TemporalDctNorm.Ortho,
    center: Boolean = false,
    ridge: Double = 0.0
):
  require(components > 0, "temporal DCT components must be positive")
  require(ridge >= 0.0 && ridge.isFinite, "temporal DCT ridge must be finite and non-negative")

object TemporalDctParams:
  def checked(
      components: Int,
      norm: TemporalDctNorm = TemporalDctNorm.Ortho,
      center: Boolean = false,
      ridge: Double = 0.0
  ): Either[ArchiveError, TemporalDctParams] =
    if components <= 0 then Left(ArchiveError.InvalidArchive(s"temporal DCT components must be positive, got $components"))
    else if ridge < 0.0 || !ridge.isFinite then Left(ArchiveError.InvalidArchive(s"temporal DCT ridge must be finite and non-negative, got $ridge"))
    else Right(TemporalDctParams(components, norm, center, ridge))

sealed trait TransformParams:
  def kind: TransformKind

object TransformParams:
  case object Empty extends TransformParams:
    val kind: TransformKind = TransformKind.Custom("empty")

  final case class Quant(params: QuantParams) extends TransformParams:
    val kind: TransformKind = TransformKind.Quant

  final case class Delta(params: DeltaParams) extends TransformParams:
    val kind: TransformKind = TransformKind.Delta

  final case class TemporalDct(params: TemporalDctParams) extends TransformParams:
    val kind: TransformKind = TransformKind.Temporal

  final case class Basis(
      method: String = "pca",
      k: Int = 20,
      center: Boolean = true,
      scale: Boolean = false
  ) extends TransformParams:
    require(method.nonEmpty, "basis method must be non-empty")
    require(k > 0, "basis k must be positive")
    val kind: TransformKind = TransformKind.Basis

  final case class Embed(
      basisPath: ArchivePath,
      centerDataWith: Option[ArchivePath] = None,
      scaleDataWith: Option[ArchivePath] = None,
      sourceDomain: Option[String] = None,
      targetDomain: Option[String] = None,
      label: Option[String] = None,
      metadata: Map[String, String] = Map.empty
  ) extends TransformParams:
    require(sourceDomain.forall(_.trim.nonEmpty), "source domain must be non-empty when provided")
    require(targetDomain.forall(_.trim.nonEmpty), "target domain must be non-empty when provided")
    require(label.forall(_.trim.nonEmpty), "label must be non-empty when provided")
    require(metadata.keys.forall(_.trim.nonEmpty), "embed metadata keys must be non-empty")
    val kind: TransformKind = TransformKind.Embed
    def lnaMetadata: LnaMetadata =
      LnaMetadata.unsafe(metadata)

  final case class SharedBasisEmbed(
      basis: SharedBasisRef,
      centerDataWith: Option[ArchivePath] = None,
      scaleDataWith: Option[ArchivePath] = None,
      sourceDomain: Option[String] = None,
      targetDomain: Option[String] = None,
      label: Option[String] = None,
      metadata: Map[String, String] = Map.empty
  ) extends TransformParams:
    require(sourceDomain.forall(_.trim.nonEmpty), "source domain must be non-empty when provided")
    require(targetDomain.forall(_.trim.nonEmpty), "target domain must be non-empty when provided")
    require(label.forall(_.trim.nonEmpty), "label must be non-empty when provided")
    require(metadata.keys.forall(_.trim.nonEmpty), "shared basis embed metadata keys must be non-empty")
    val kind: TransformKind = TransformKind.Embed
    def lnaMetadata: LnaMetadata =
      LnaMetadata.unsafe(metadata)

  final case class Custom(
      name: String,
      sourceDomain: Option[String] = None,
      targetDomain: Option[String] = None,
      label: Option[String] = None,
      metadata: Map[String, String] = Map.empty
  ) extends TransformParams:
    require(name.trim.nonEmpty, "custom transform name must be non-empty")
    require(sourceDomain.forall(_.trim.nonEmpty), "source domain must be non-empty when provided")
    require(targetDomain.forall(_.trim.nonEmpty), "target domain must be non-empty when provided")
    require(label.forall(_.trim.nonEmpty), "label must be non-empty when provided")
    require(metadata.keys.forall(_.trim.nonEmpty), "custom transform metadata keys must be non-empty")
    val kind: TransformKind = TransformKind.Custom(name)
    def lnaMetadata: LnaMetadata =
      LnaMetadata.unsafe(metadata)

final case class DatasetRef(
    path: ArchivePath,
    role: DatasetRole,
    dims: Vector[Int],
    dtype: Option[LnaDType] = None
):
  require(dims.nonEmpty, "dataset dims must be non-empty")
  require(dims.forall(_ > 0), "dataset dims must be positive")

  def shape: DatasetShape =
    DatasetShape.unsafe(dims)

object DatasetRef:
  def checked(
      path: ArchivePath,
      role: DatasetRole,
      dims: Vector[Int],
      dtype: Option[LnaDType] = None
  ): Either[ArchiveError, DatasetRef] =
    DatasetShape(dims).map(shape => DatasetRef(path, role, shape.toVector, dtype))

final case class TransformDescriptor(
    name: String,
    kind: TransformKind,
    params: TransformParams,
    inputs: Vector[String],
    outputs: Vector[String],
    datasets: Vector[DatasetRef],
    report: Option[TransformReport] = None
):
  require(name.nonEmpty, "transform descriptor name must be non-empty")
  require(name.endsWith(".json"), "transform descriptor name must end in .json")
  require(inputs.nonEmpty, "transform descriptor requires at least one input key")
  require(outputs.nonEmpty, "transform descriptor requires at least one output key")
  require(params.kind == kind || params == TransformParams.Empty, "transform params kind must match descriptor kind")
  require(report.forall(_.kind == kind), "transform report kind must match descriptor kind")

  def transformName: TransformName =
    TransformName.unsafe(name)

  def inputPorts: Vector[TransformPort] =
    inputs.map(TransformPort.unsafe)

  def outputPorts: Vector[TransformPort] =
    outputs.map(TransformPort.unsafe)

object TransformDescriptor:
  def checked(
      name: String,
      kind: TransformKind,
      params: TransformParams,
      inputs: Vector[String],
      outputs: Vector[String],
      datasets: Vector[DatasetRef],
      report: Option[TransformReport] = None
  ): Either[ArchiveError, TransformDescriptor] =
    for
      checkedName <- TransformName(name)
      checkedInputs <- traverseModel(inputs)(TransformPort.apply)
      checkedOutputs <- traverseModel(outputs)(TransformPort.apply)
      _ <-
        if checkedInputs.nonEmpty then Right(())
        else Left(ArchiveError.InvalidArchive("transform descriptor requires at least one input key"))
      _ <-
        if checkedOutputs.nonEmpty then Right(())
        else Left(ArchiveError.InvalidArchive("transform descriptor requires at least one output key"))
      _ <-
        if params.kind == kind || params == TransformParams.Empty then Right(())
        else Left(ArchiveError.InvalidArchive("transform params kind must match descriptor kind"))
      _ <-
        if report.forall(_.kind == kind) then Right(())
        else Left(ArchiveError.InvalidArchive("transform report kind must match descriptor kind"))
    yield TransformDescriptor(
      name = checkedName.value,
      kind = kind,
      params = params,
      inputs = checkedInputs.map(_.value),
      outputs = checkedOutputs.map(_.value),
      datasets = datasets,
      report = report
    )

final case class LnaShape(space: NeuroSpace, timepoints: Int):
  require(timepoints > 0, "archive timepoints must be positive")
  require(space.spatialDims.length == 3, "archive space must be 3D")
  def spatialSize: Int = space.spatialDims.product

sealed trait Payload:
  def dims: Vector[Int]
  def dtype: LnaDType

object Payload:
  final case class DoubleMatrix(data: DMat, dtype: LnaDType = LnaDType.Float64) extends Payload:
    require(dtype == LnaDType.Float32 || dtype == LnaDType.Float64, "double matrix payload must use float dtype")
    def dims: Vector[Int] = Vector(data.rows, data.cols)

  final case class IntMatrix(rows: Int, cols: Int, values: Vector[Int], dtype: LnaDType) extends Payload:
    require(rows > 0 && cols > 0, "integer matrix dimensions must be positive")
    require(values.length == rows * cols, "integer matrix payload length must match dimensions")
    require(dtype == LnaDType.UInt8 || dtype == LnaDType.UInt16 || dtype == LnaDType.Int32, "integer matrix payload must use integer dtype")
    Payload.integerRange(dtype).foreach { case (min, max) =>
      require(
        values.forall(value => value >= min && value <= max),
        s"$dtype integer matrix values must be between $min and $max"
      )
    }
    def dims: Vector[Int] = Vector(rows, cols)
    def apply(row: Int, col: Int): Int =
      require(row >= 0 && row < rows, "row index out of bounds")
      require(col >= 0 && col < cols, "column index out of bounds")
      values(row * cols + col)

  final case class DoubleVector(values: Vector[Double], dtype: LnaDType = LnaDType.Float64) extends Payload:
    require(values.nonEmpty, "double vector payload must be non-empty")
    require(dtype == LnaDType.Float32 || dtype == LnaDType.Float64, "double vector payload must use float dtype")
    def dims: Vector[Int] = Vector(values.length)

  private[lna] def integerRange(dtype: LnaDType): Option[(Int, Int)] =
    dtype.integerRange

final case class LnaRun(
    label: RunLabel,
    shape: LnaShape,
    output: ArchivePath,
    mask: Option[ArchivePath] = None
)

final case class LnaManifest(
    version: LnaVersion = LnaVersion.V2,
    creator: String = "scalafim-archive",
    requiredTransforms: Vector[TransformKind],
    transforms: Vector[TransformDescriptor],
    runs: Vector[LnaRun],
    datasets: Vector[DatasetRef],
    header: Map[String, String] = Map.empty,
    checksum: Option[String] = None
):
  require(creator.trim.nonEmpty, "archive creator must be non-empty")
  require(runs.nonEmpty, "archive manifest requires at least one run")

  def creatorId: CreatorId =
    CreatorId.unsafe(creator)

  def lnaHeader: LnaMetadata =
    LnaMetadata.unsafe(header)

object LnaManifest:
  def checked(
      version: LnaVersion = LnaVersion.V2,
      creator: String = "scalafim-archive",
      requiredTransforms: Vector[TransformKind],
      transforms: Vector[TransformDescriptor],
      runs: Vector[LnaRun],
      datasets: Vector[DatasetRef],
      header: Map[String, String] = Map.empty,
      checksum: Option[String] = None
  ): Either[ArchiveError, LnaManifest] =
    for
      checkedCreator <- CreatorId(creator)
      checkedHeader <- LnaMetadata(header)
      _ <-
        if runs.nonEmpty then Right(())
        else Left(ArchiveError.InvalidArchive("archive manifest requires at least one run"))
    yield LnaManifest(
      version = version,
      creator = checkedCreator.value,
      requiredTransforms = requiredTransforms,
      transforms = transforms,
      runs = runs,
      datasets = datasets,
      header = checkedHeader.values,
      checksum = checksum
    )

final case class LnaArchive(
    manifest: LnaManifest,
    payloads: Map[ArchivePath, Payload]
):
  def run(label: RunLabel): Option[LnaRun] =
    manifest.runs.find(_.label == label)

  def payload(path: ArchivePath): Option[Payload] =
    payloads.get(path)

  def validate: Either[scalafim.archive.ArchiveError, LnaArchive] =
    LnaValidator.validateArchive(this)

private[lna] def traverseModel[A, B](values: Iterable[A])(f: A => Either[ArchiveError, B]): Either[ArchiveError, Vector[B]] =
  val out = Vector.newBuilder[B]
  val it = values.iterator
  var error = Option.empty[ArchiveError]
  while it.hasNext && error.isEmpty do
    f(it.next()) match
      case Right(value) => out += value
      case Left(err)    => error = Some(err)
  error match
    case Some(err) => Left(err)
    case None      => Right(out.result())
