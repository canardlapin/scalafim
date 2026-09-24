package scalafim.spatial.io

import scalafim.image.world.{SessionId, SubjectId}

import scalafim.spatial.*
import scalafim.transform.TransformFormat

import java.nio.file.Path

enum TransformTool:
  case ANTs, FSL, AFNI, FreeSurfer, X5

enum TransformKind:
  case Affine3D, Warp3D, DisplacementField3D, CoordinateField3D

  def morphismKind: MorphismKind =
    this match
      case Affine3D => MorphismKind.Affine3D
      case Warp3D | DisplacementField3D | CoordinateField3D => MorphismKind.Warp3D

/** The graph-level reading of a toolkit file format: which tool wrote it and which morphism kind it declares before
  * loading. Every numeric convention (LPS vs RAS, FSL scaled voxels, pullback vs forward storage) is intrinsic to the
  * format and handled by its `scalafim.transform` interpretation, never by the descriptor.
  */
extension (format: TransformFormat)
  def tool: TransformTool =
    format match
      case TransformFormat.ItkText | TransformFormat.ItkMatlab | TransformFormat.ItkHdf5 |
          TransformFormat.AntsDisplacementNifti => TransformTool.ANTs
      case TransformFormat.FslFlirt | TransformFormat.FslFnirtField | TransformFormat.FslFnirtCoefficients =>
        TransformTool.FSL
      case TransformFormat.AfniAff12 | TransformFormat.AfniQwarp => TransformTool.AFNI
      case TransformFormat.FreeSurferLta | TransformFormat.FreeSurferXfm | TransformFormat.FreeSurferRegisterDat =>
        TransformTool.FreeSurfer
      case TransformFormat.X5 => TransformTool.X5

  /** The kind a descriptor declares from the format alone. Containers that may hold either an affine or a field (ITK
    * HDF5, X5) declare `Warp3D`, which admits both; formats that can only hold an affine declare `Affine3D`.
    */
  def defaultKind: TransformKind =
    format match
      case TransformFormat.ItkText | TransformFormat.ItkMatlab | TransformFormat.FslFlirt | TransformFormat.AfniAff12 |
          TransformFormat.FreeSurferLta | TransformFormat.FreeSurferXfm | TransformFormat.FreeSurferRegisterDat =>
        TransformKind.Affine3D
      case TransformFormat.ItkHdf5 | TransformFormat.AntsDisplacementNifti | TransformFormat.FslFnirtField |
          TransformFormat.FslFnirtCoefficients | TransformFormat.AfniQwarp | TransformFormat.X5 =>
        TransformKind.Warp3D

/** How a transform file's own endpoints relate to the descriptor's route.
  *
  * Every format's interpretation yields a transform from the file's moving (source) space to its fixed (target) space.
  * A descriptor usually routes the same way (`AsFile`). When the graph needs the opposite route from the same file,
  * e.g. an fMRIPrep `from-T1w_to-MNI` file declared as an MNI -> T1w edge, the adapter inverts it: exactly for affines,
  * and for dense maps only when an inverse asset supplied the other direction (otherwise a typed refusal).
  */
enum TransformFileEndpoints:
  case AsFile, Reversed

enum InverseQuality:
  case Exact(method: String)
  case Provided(method: String, score: Double)
  case Approximate(method: String, score: Double)
  case Missing

  this match
    case Exact(method) =>
      require(method.trim.nonEmpty, "inverse method must be non-empty")
    case Provided(method, score) =>
      require(method.trim.nonEmpty, "inverse method must be non-empty")
      require(score.isFinite && score >= 0.0 && score <= 1.0, "inverse quality must be in [0, 1]")
    case Approximate(method, score) =>
      require(method.trim.nonEmpty, "inverse method must be non-empty")
      require(score.isFinite && score >= 0.0 && score <= 1.0, "inverse quality must be in [0, 1]")
    case _ =>
      ()

  def declared: Boolean =
    this != InverseQuality.Missing

  def toInverse: Inverse =
    this match
      case Exact(method) => Inverse.Exact(method)
      case Provided(method, score) => Inverse.Provided(method, score)
      case Approximate(method, score) => Inverse.Approximate(method, score)
      case InverseQuality.Missing => Inverse.None

object InverseQuality:
  def exact(method: String): Either[SpatialIoError, InverseQuality] =
    nonEmpty("exact inverse method", method).map(InverseQuality.Exact.apply)

  def provided(method: String, score: Double): Either[SpatialIoError, InverseQuality] =
    for
      clean <- nonEmpty("provided inverse method", method)
      _ <- validScore("provided inverse", score)
    yield InverseQuality.Provided(clean, score)

  def approximate(method: String, score: Double): Either[SpatialIoError, InverseQuality] =
    for
      clean <- nonEmpty("approximate inverse method", method)
      _ <- validScore("approximate inverse", score)
    yield InverseQuality.Approximate(clean, score)

  private def nonEmpty(label: String, value: String): Either[SpatialIoError, String] =
    val clean = value.trim
    if clean.isEmpty then Left(SpatialIoError.InvalidTransformDescriptor(label, "must be non-empty"))
    else Right(clean)

  private def validScore(label: String, score: Double): Either[SpatialIoError, Unit] =
    if score.isFinite && score >= 0.0 && score <= 1.0 then Right(())
    else Left(SpatialIoError.InvalidTransformDescriptor(label, s"quality must be in [0, 1], got $score"))

final case class TransformDescriptor private (
  id: MorphismId,
  source: DomainId,
  target: DomainId,
  tool: TransformTool,
  kind: TransformKind,
  path: Path,
  routeTag: RouteTag,
  cost: Double,
  inverseQuality: InverseQuality,
  coordinateMap: CoordinateMap,
  format: Option[TransformFormat],
  endpoints: TransformFileEndpoints
):
  def inverseQualityDeclared: Boolean =
    inverseQuality.declared

  def requireInverseQuality: Either[SpatialIoError, InverseQuality] =
    inverseQuality match
      case InverseQuality.Missing => Left(SpatialIoError.MissingInverseQuality(id.value))
      case declared => Right(declared)

  def toMorphism: Either[SpatialIoError, Morphism] =
    Morphism.build(
      id = id,
      source = source,
      target = target,
      kind = kind.morphismKind,
      routeTag = routeTag,
      cost = cost,
      inverse = inverseQuality.toInverse,
      coordinateMap = coordinateMap
    ).left.map(error => SpatialIoError.InvalidTransformDescriptor(id.value, error.message))

  /** Read, decode and interpret the file, and build this descriptor's morphism from the resulting world transform. */
  def load(
    sourceDomain: Domain,
    targetDomain: Domain,
    options: TransformLoadOptions = TransformLoadOptions(),
    inverseAsset: Option[TransformAssetSpec] = None
  ): Either[SpatialIoError, LoadedTransform] =
    TransformAssetLoader.load(this, sourceDomain, targetDomain, options, inverseAsset)

object TransformDescriptor:
  def build(
    id: MorphismId,
    source: DomainId,
    target: DomainId,
    tool: TransformTool,
    kind: TransformKind,
    path: Path,
    routeTag: RouteTag = RouteTag.Anatomical,
    cost: Double = 1.0,
    inverseQuality: InverseQuality = InverseQuality.Missing,
    coordinateMap: CoordinateMap = CoordinateMap.Unspecified,
    format: Option[TransformFormat] = None,
    endpoints: TransformFileEndpoints = TransformFileEndpoints.AsFile
  ): Either[SpatialIoError, TransformDescriptor] =
    if !cost.isFinite || cost < 0.0 then
      Left(SpatialIoError.InvalidTransformDescriptor(id.value, s"cost must be finite and non-negative, got $cost"))
    else
      Right(
        new TransformDescriptor(
          id,
          source,
          target,
          tool,
          kind,
          path,
          routeTag,
          cost,
          inverseQuality,
          coordinateMap,
          format,
          endpoints
        )
      )

  def fromFile(
    id: MorphismId,
    source: DomainId,
    target: DomainId,
    path: Path,
    format: TransformFormat,
    routeTag: RouteTag = RouteTag.Anatomical,
    cost: Double = 1.0,
    inverseQuality: InverseQuality = InverseQuality.Missing,
    coordinateMap: CoordinateMap = CoordinateMap.Unspecified,
    endpoints: TransformFileEndpoints = TransformFileEndpoints.AsFile
  ): Either[SpatialIoError, TransformDescriptor] =
    build(
      id = id,
      source = source,
      target = target,
      tool = format.tool,
      kind = format.defaultKind,
      path = path,
      routeTag = routeTag,
      cost = cost,
      inverseQuality = inverseQuality,
      coordinateMap = coordinateMap,
      format = Some(format),
      endpoints = endpoints
    )

final case class FmriprepSpatialGraph private (
  subject: SubjectId,
  session: Option[SessionId],
  domains: Vector[Domain],
  transforms: Vector[TransformDescriptor]
):
  def toSpatialGraph: Either[SpatialIoError, SpatialGraph] =
    for
      morphisms <- collectMorphisms
      graph <- SpatialGraph.build(domains, morphisms).left.map(error =>
        SpatialIoError.InvalidTransformDescriptor("fMRIPrep graph", error.message)
      )
    yield graph

  private def collectMorphisms: Either[SpatialIoError, Vector[Morphism]] =
    val builder = Vector.newBuilder[Morphism]
    var i = 0
    var error = Option.empty[SpatialIoError]
    while i < transforms.length && error.isEmpty do
      transforms(i).toMorphism match
        case Right(morphism) => builder += morphism
        case Left(err) => error = Some(err)
      i += 1
    error match
      case Some(err) => Left(err)
      case None => Right(builder.result())

object FmriprepSpatialGraph:
  def build(
    subject: SubjectId,
    session: Option[SessionId],
    domains: Vector[Domain],
    transforms: Vector[TransformDescriptor]
  ): Either[SpatialIoError, FmriprepSpatialGraph] =
    if domains.isEmpty then
      Left(SpatialIoError.InvalidTransformDescriptor("fMRIPrep graph", "at least one domain is required"))
    else Right(new FmriprepSpatialGraph(subject, session, domains, transforms))
