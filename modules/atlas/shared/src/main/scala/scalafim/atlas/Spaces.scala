package scalafim.atlas

import image4s.geometry.Affine as ProviderAffine
import image4s.geometry.D3
import scalafim.image.{
  GridSpec,
  SpatialPoint,
  SpatialPullback,
  SpatialPullbacks
}

type Point3D = SpatialPoint

object Point3D:
  val Origin: Point3D =
    SpatialPoint.Origin

  def apply(x: Double, y: Double, z: Double): Point3D =
    SpatialPoint(x, y, z)

  def fromVector(values: Vector[Double]): Point3D =
    SpatialPoint.unsafeFromVector(values, "Point3D")

enum TransformKind:
  case Identity, Affine, NonlinearWarp, SphereResample, VolToSurf, SurfToVol

enum TransformBackend:
  case Identity, InternalAffine, TemplateFlowAnts, SphereNearest, Workbench, NeuroSurf, RibbonFill

enum TransformStatus:
  case Available, Planned

enum DataKind:
  case Parcel, Vertex, Voxel

final case class TransformStep(
  from: AnySpaceId,
  to: AnySpaceId,
  kind: TransformKind,
  backend: TransformBackend,
  confidence: Confidence,
  reversible: Boolean,
  dataFiles: Vector[String],
  status: TransformStatus,
  notes: Option[String] = None,
  affine: Option[ProviderAffine[D3]] = None
):
  require(dataFiles.forall(_.trim.nonEmpty), "transform data file names must be non-empty")

final case class TransformPlan(
  from: AnySpaceId,
  to: AnySpaceId,
  steps: Vector[TransformStep],
  status: TransformStatus,
  confidence: Confidence,
  warnings: Vector[String]
):
  require(steps.nonEmpty, "transform plan must contain at least one step")

  def nSteps: Int =
    steps.length

  def isExecutable: Boolean =
    executableCoordinatePlan.isRight

  def executableCoordinatePlan: Either[AtlasError, ExecutableCoordinateTransformPlan] =
    ExecutableCoordinateTransformPlan.fromRoute(this)

final case class ExecutableCoordinateTransformPlan private (
  route: TransformPlan,
  affine: ProviderAffine[D3]
):
  def from: AnySpaceId =
    route.from

  def to: AnySpaceId =
    route.to

  def steps: Vector[TransformStep] =
    route.steps

  def transform(points: Vector[Point3D]): Vector[Point3D] =
    points.map: point =>
      affine(point.toVector).fold(
        error => throw new IllegalStateException(error.message),
        Point3D.fromVector
      )

  /** Render the atlas route as the provider pullback required by image
    * resampling: target world coordinates to source world coordinates.
    */
  def pullback(
      source: GridSpec,
      target: GridSpec
  ): SpatialPullback =
    SpatialPullbacks.affine(source, target, affine.inverse)

object ExecutableCoordinateTransformPlan:
  def fromRoute(route: TransformPlan): Either[AtlasError, ExecutableCoordinateTransformPlan] =
    val unavailable =
      route.steps.filter(_.status != TransformStatus.Available)
    val missingAffine =
      route.steps.filter(_.affine.isEmpty)
    if unavailable.nonEmpty || missingAffine.nonEmpty then
      val unavailableReason =
        if unavailable.isEmpty then Vector.empty
        else Vector(s"unavailable steps=${unavailable.map(stepLabel).mkString(",")}")
      val missingReason =
        if missingAffine.isEmpty then Vector.empty
        else Vector(s"non-affine steps=${missingAffine.map(stepLabel).mkString(",")}")
      Left(AtlasError.TransformNotExecutable(route.from, route.to, (unavailableReason ++ missingReason).mkString("; ")))
    else
      val affines = route.steps.map(_.affine.get)
      val composed =
        affines.tail.foldLeft[Either[AtlasError, ProviderAffine[D3]]](Right(affines.head)):
          (current, next) =>
            current.flatMap(
              _.andThen(next).left.map(AtlasError.Geometry.apply)
            )
      composed.map(ExecutableCoordinateTransformPlan(route, _))

  private def stepLabel(step: TransformStep): String =
    s"${step.from.value}->${step.to.value}:${step.kind}/${step.backend}"

object SpaceTransforms:
  val mni305ToMni152: ProviderAffine[D3] =
    ProviderAffine.fromRowMajor[D3](
      Vector(
        0.9975, -0.0073, 0.0176, -0.0429,
        0.0146, 1.0009, -0.0024, 1.5496,
        -0.0130, -0.0093, 0.9971, 1.1840,
        0.0, 0.0, 0.0, 1.0
      )
    ).fold(error => throw new IllegalStateException(error.message), identity)

  val mni152ToMni305: ProviderAffine[D3] =
    mni305ToMni152.inverse

  val manifest: Vector[TransformStep] =
    Vector(
      TransformStep(
        SpaceId.MNI305,
        SpaceId.MNI152,
        TransformKind.Affine,
        TransformBackend.InternalAffine,
        Confidence.Exact,
        reversible = true,
        dataFiles = Vector.empty,
        TransformStatus.Available,
        notes = Some("FreeSurfer mni152.register.dat"),
        affine = Some(mni305ToMni152)
      ),
      TransformStep(
        SpaceId.MNI152,
        SpaceId.MNI305,
        TransformKind.Affine,
        TransformBackend.InternalAffine,
        Confidence.Exact,
        reversible = true,
        dataFiles = Vector.empty,
        TransformStatus.Available,
        notes = Some("Inverse of FreeSurfer mni152.register.dat"),
        affine = Some(mni152ToMni305)
      ),
      TransformStep(
        SpaceId.MNI152NLin6Asym,
        SpaceId.MNI152NLin2009cAsym,
        TransformKind.NonlinearWarp,
        TransformBackend.TemplateFlowAnts,
        Confidence.High,
        reversible = true,
        dataFiles = Vector("from-MNI152NLin6Asym_to-MNI152NLin2009cAsym_mode-image_xfm.h5"),
        TransformStatus.Planned,
        notes = Some("TemplateFlow composite warp")
      ),
      TransformStep(
        SpaceId.MNI152NLin2009cAsym,
        SpaceId.MNI152NLin6Asym,
        TransformKind.NonlinearWarp,
        TransformBackend.TemplateFlowAnts,
        Confidence.High,
        reversible = true,
        dataFiles = Vector("from-MNI152NLin2009cAsym_to-MNI152NLin6Asym_mode-image_xfm.h5"),
        TransformStatus.Planned,
        notes = Some("TemplateFlow composite warp inverse")
      ),
      TransformStep(
        SpaceId.FsAverage,
        SpaceId.FsAverage6,
        TransformKind.SphereResample,
        TransformBackend.SphereNearest,
        Confidence.Exact,
        reversible = true,
        dataFiles = Vector("fsaverage/surf/?h.sphere.reg"),
        TransformStatus.Available,
        notes = Some("Downsample 164k to 41k on registration sphere")
      ),
      TransformStep(
        SpaceId.FsAverage6,
        SpaceId.FsAverage,
        TransformKind.SphereResample,
        TransformBackend.SphereNearest,
        Confidence.Exact,
        reversible = true,
        dataFiles = Vector("fsaverage/surf/?h.sphere.reg"),
        TransformStatus.Available,
        notes = Some("Upsample 41k to 164k on registration sphere")
      ),
      TransformStep(
        SpaceId.FsAverage,
        SpaceId.FsAverage5,
        TransformKind.SphereResample,
        TransformBackend.SphereNearest,
        Confidence.Exact,
        reversible = true,
        dataFiles = Vector("fsaverage/surf/?h.sphere.reg"),
        TransformStatus.Available,
        notes = Some("Downsample 164k to 10k on registration sphere")
      ),
      TransformStep(
        SpaceId.FsAverage5,
        SpaceId.FsAverage,
        TransformKind.SphereResample,
        TransformBackend.SphereNearest,
        Confidence.Exact,
        reversible = true,
        dataFiles = Vector("fsaverage/surf/?h.sphere.reg"),
        TransformStatus.Available,
        notes = Some("Upsample 10k to 164k on registration sphere")
      ),
      TransformStep(
        SpaceId.FsAverage,
        SpaceId.FsLR32k,
        TransformKind.SphereResample,
        TransformBackend.Workbench,
        Confidence.High,
        reversible = true,
        dataFiles = Vector("fs_LR-deformed_to-fsaverage.?H.sphere.reg.surf.gii"),
        TransformStatus.Planned,
        notes = Some("HCP sphere registration via Workbench")
      ),
      TransformStep(
        SpaceId.FsLR32k,
        SpaceId.FsAverage,
        TransformKind.SphereResample,
        TransformBackend.Workbench,
        Confidence.High,
        reversible = true,
        dataFiles = Vector("fsaverage_to-fs_LR.?H.sphere.reg.surf.gii"),
        TransformStatus.Planned,
        notes = Some("HCP sphere registration via Workbench")
      ),
      TransformStep(
        SpaceId.MNI152NLin2009cAsym,
        SpaceId.FsAverage,
        TransformKind.VolToSurf,
        TransformBackend.NeuroSurf,
        Confidence.Approximate,
        reversible = false,
        dataFiles = Vector.empty,
        TransformStatus.Planned,
        notes = Some("Requires white and pial surfaces")
      ),
      TransformStep(
        SpaceId.FsAverage,
        SpaceId.MNI152NLin2009cAsym,
        TransformKind.SurfToVol,
        TransformBackend.RibbonFill,
        Confidence.Approximate,
        reversible = false,
        dataFiles = Vector.empty,
        TransformStatus.Planned,
        notes = Some("Requires ribbon mask construction")
      )
    )

  def plan(
    from: AnySpaceId,
    to: AnySpaceId,
    dataKind: DataKind = DataKind.Parcel,
    registry: Vector[TransformStep] = manifest
  ): Either[AtlasError, TransformPlan] =
    val fromNorm = SpaceId.normalize(from)
    val toNorm = SpaceId.normalize(to)
    if fromNorm == toNorm then
      val step =
        TransformStep(
          fromNorm,
          toNorm,
          TransformKind.Identity,
          TransformBackend.Identity,
          Confidence.Exact,
          reversible = true,
          dataFiles = Vector.empty,
          TransformStatus.Available,
          notes = Some("No transform required."),
          affine = Some(ProviderAffine.identity[D3])
        )
      Right(TransformPlan(fromNorm, toNorm, Vector(step), TransformStatus.Available, Confidence.Exact, Vector.empty))
    else
      shortestRoute(fromNorm, toNorm, registry).map { steps =>
        val status = combineStatus(steps)
        val confidence = combineConfidence(steps)
        TransformPlan(fromNorm, toNorm, steps, status, confidence, warningsFor(steps, dataKind))
      }.toRight(AtlasError.NoTransformRoute(fromNorm, toNorm))

  def transformCoords(
    points: Vector[Point3D],
    from: AnySpaceId,
    to: AnySpaceId,
    registry: Vector[TransformStep] = manifest
  ): Either[AtlasError, Vector[Point3D]] =
    plan(from, to, DataKind.Voxel, registry)
      .flatMap(_.executableCoordinatePlan)
      .map(_.transform(points))

  def spatialPullback(
    from: AnySpaceId,
    to: AnySpaceId,
    source: GridSpec,
    target: GridSpec,
    registry: Vector[TransformStep] = manifest
  ): Either[AtlasError, SpatialPullback] =
    plan(from, to, DataKind.Voxel, registry)
      .flatMap(_.executableCoordinatePlan)
      .map(_.pullback(source, target))

  private final case class Candidate(space: AnySpaceId, steps: Vector[TransformStep], score: Int)

  private def shortestRoute(from: AnySpaceId, to: AnySpaceId, registry: Vector[TransformStep]): Option[Vector[TransformStep]] =
    val edges = registry.groupBy(step => SpaceId.normalize(step.from))
    var frontier = Vector(Candidate(from, Vector.empty, 0))
    var best = Map(from -> 0)
    var done = false
    var found: Option[Vector[TransformStep]] = None

    while frontier.nonEmpty && !done do
      val idx = frontier.indices.minBy(i => frontier(i).score)
      val current = frontier(idx)
      frontier = frontier.patch(idx, Nil, 1)
      if current.space == to then
        found = Some(current.steps)
        done = true
      else
        val nextEdges = edges.getOrElse(current.space, Vector.empty)
        nextEdges.foreach { step =>
          val dest = SpaceId.normalize(step.to)
          if !current.steps.exists(s => SpaceId.normalize(s.from) == dest) then
            val nextScore = current.score + stepScore(step)
            val keep = best.get(dest).forall(nextScore < _)
            if keep then
              best = best.updated(dest, nextScore)
              frontier = frontier :+ Candidate(dest, current.steps :+ step.copy(from = current.space, to = dest), nextScore)
        }
    found

  private def stepScore(step: TransformStep): Int =
    statusRank(step.status) * 100 + confidenceRank(step.confidence) * 10 + 1

  private def statusRank(status: TransformStatus): Int =
    status match
      case TransformStatus.Available => 0
      case TransformStatus.Planned => 1

  private def confidenceRank(confidence: Confidence): Int =
    confidence match
      case Confidence.Exact => 0
      case Confidence.High => 1
      case Confidence.Approximate => 2
      case Confidence.Uncertain => 3

  private def combineStatus(steps: Vector[TransformStep]): TransformStatus =
    if steps.exists(_.status == TransformStatus.Planned) then TransformStatus.Planned
    else TransformStatus.Available

  private def combineConfidence(steps: Vector[TransformStep]): Confidence =
    val worst = steps.map(s => confidenceRank(s.confidence)).max
    worst match
      case 0 => Confidence.Exact
      case 1 => Confidence.High
      case 2 => Confidence.Approximate
      case _ => Confidence.Uncertain

  private def warningsFor(steps: Vector[TransformStep], dataKind: DataKind): Vector[String] =
    val planned =
      if steps.exists(_.status == TransformStatus.Planned) then Vector("Plan includes unimplemented/planned transform step(s).")
      else Vector.empty
    val lowConfidence =
      if steps.exists(s => s.confidence == Confidence.Approximate || s.confidence == Confidence.Uncertain) then
        Vector("Plan includes low-confidence transform step(s).")
      else Vector.empty
    val vertexNearest =
      if dataKind == DataKind.Vertex && steps.exists(_.backend == TransformBackend.SphereNearest) then
        Vector("Nearest-neighbor surface resampling may be suboptimal for continuous data.")
      else Vector.empty
    planned ++ lowConfidence ++ vertexNearest
