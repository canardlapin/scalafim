package scalafim.archive.lna

import scalafim.archive.{ArchivePath, RunLabel}
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

final case class QuantParams(
    bits: Int = 8,
    method: QuantMethod = QuantMethod.Range,
    center: Boolean = true,
    scaleScope: QuantScaleScope = QuantScaleScope.Global,
    allowClip: Boolean = false
):
  require(bits >= 1 && bits <= 16, "quant bits must be between 1 and 16")

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

final case class TemporalDctParams(
    components: Int,
    norm: TemporalDctNorm = TemporalDctNorm.Ortho,
    center: Boolean = false,
    ridge: Double = 0.0
):
  require(components > 0, "temporal DCT components must be positive")
  require(ridge >= 0.0 && ridge.isFinite, "temporal DCT ridge must be finite and non-negative")

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

final case class DatasetRef(
    path: ArchivePath,
    role: DatasetRole,
    dims: Vector[Int],
    dtype: Option[LnaDType] = None
):
  require(dims.nonEmpty, "dataset dims must be non-empty")
  require(dims.forall(_ > 0), "dataset dims must be positive")

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
    dtype match
      case LnaDType.UInt8  => Some((0, 255))
      case LnaDType.UInt16 => Some((0, 65535))
      case LnaDType.Int32  => None
      case _               => None

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
