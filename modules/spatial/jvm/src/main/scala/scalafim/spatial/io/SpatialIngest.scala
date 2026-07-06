package scalafim.spatial.io

import scalafim.spatial.*

import java.nio.file.Path

enum TransformTool:
  case ANTs, FSL, AFNI, FreeSurfer, X5

enum TransformKind:
  case Affine3D, Warp3D, DisplacementField3D, CoordinateField3D

  def morphismKind: MorphismKind =
    this match
      case Affine3D => MorphismKind.Affine3D
      case Warp3D | DisplacementField3D | CoordinateField3D => MorphismKind.Warp3D

enum TransformFileFormat:
  case ANTsH5, FslFlirt, FslFnirt, AfniAffine, AfniWarp, FreeSurferLta, X5

  def tool: TransformTool =
    this match
      case ANTsH5 => TransformTool.ANTs
      case FslFlirt | FslFnirt => TransformTool.FSL
      case AfniAffine | AfniWarp => TransformTool.AFNI
      case FreeSurferLta => TransformTool.FreeSurfer
      case X5 => TransformTool.X5

  def defaultKind: TransformKind =
    this match
      case ANTsH5 | FslFnirt | AfniWarp => TransformKind.Warp3D
      case FslFlirt | AfniAffine | FreeSurferLta => TransformKind.Affine3D
      case X5 => TransformKind.Warp3D

object TransformFileFormat:
  def detect(path: Path): Option[TransformFileFormat] =
    val name = path.getFileName.toString.toLowerCase
    if name.endsWith(".h5") || name.endsWith(".hdf5") then Some(TransformFileFormat.ANTsH5)
    else if name.endsWith(".lta") then Some(TransformFileFormat.FreeSurferLta)
    else if name.endsWith(".x5") then Some(TransformFileFormat.X5)
    else if name.endsWith(".aff12.1d") || name.endsWith(".1d") then Some(TransformFileFormat.AfniAffine)
    else if name.contains("fnirt") || name.contains("warpcoef") then Some(TransformFileFormat.FslFnirt)
    else if name.endsWith(".mat") then Some(TransformFileFormat.FslFlirt)
    else None

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
  coordinateMap: CoordinateMap
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
    coordinateMap: CoordinateMap = CoordinateMap.Unspecified
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
          coordinateMap
        )
      )

  def fromFile(
    id: MorphismId,
    source: DomainId,
    target: DomainId,
    path: Path,
    format: TransformFileFormat,
    routeTag: RouteTag = RouteTag.Anatomical,
    cost: Double = 1.0,
    inverseQuality: InverseQuality = InverseQuality.Missing,
    coordinateMap: CoordinateMap = CoordinateMap.Unspecified
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
      coordinateMap = coordinateMap
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
