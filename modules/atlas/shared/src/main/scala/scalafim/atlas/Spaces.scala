package scalafim.atlas

import image4s.geometry.Affine as ProviderAffine
import image4s.geometry.D3
import image4s.geometry.Frame
import scalafim.image.{
  GridSpec,
  SpatialPoint,
  SpatialPullback,
  SpatialPullbacks
}
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.spatial.{CoordinateMap, ExecutableAffinePath, Morphism, MorphismPath}
import scalafim.transform.WorldTransform

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

/** A provider transform implementing a manifest step: a `scalafim.transform` world transform between the catalog
  * frames of the step's spaces, and an identity that determines its values (e.g. asset SHA-256 digests plus every
  * loading policy). The identity keys non-affine maps, whose values cannot be fingerprinted structurally.
  */
final case class TransformAsset(transform: WorldTransform[?, ?], identity: String):
  require(identity.trim.nonEmpty, "transform asset identity must be non-empty")

/** One entry of a transform manifest: a declared edge between two template spaces.
  *
  * A step can carry coordinates only when it is `Available` and holds either an internal `affine` (the forward,
  * source-to-target matrix) or a provider `asset`. Every other step, e.g. a TemplateFlow warp whose H5 file has not been
  * loaded, stays in the graph as a typed non-executable edge: routes through it are planned and reported, never
  * guessed.
  */
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
  affine: Option[ProviderAffine[D3]] = None,
  asset: Option[TransformAsset] = None
):
  require(dataFiles.forall(_.trim.nonEmpty), "transform data file names must be non-empty")
  require(affine.isEmpty || asset.isEmpty, "a transform step carries an internal affine or a provider asset, not both")

  /** Implement this step with a loaded provider transform; the step becomes `Available`. */
  def withAsset(value: TransformAsset): TransformStep =
    copy(status = TransformStatus.Available, affine = None, asset = Some(value))

/** A route through the transform manifest, resolved by `SpatialGraph` routing.
  *
  * `steps` describe the route in order (a step run through its inverse appears with its endpoints swapped); `path` is
  * the spatial route that executes it. Coordinates are carried only when every step is available and holds a
  * provider map; otherwise [[executability]] says why not.
  */
final case class TransformPlan private (
  from: AnySpaceId,
  to: AnySpaceId,
  steps: Vector[TransformStep],
  status: TransformStatus,
  confidence: Confidence,
  warnings: Vector[String],
  path: MorphismPath
)(
  /** The world spaces of the route's endpoints, as the transform catalog frames them. */
  private val fromWorld: WorldSpace,
  private val toWorld: WorldSpace
):
  require(steps.nonEmpty, "transform plan must contain at least one step")
  require(steps.length == path.morphisms.length, "transform plan steps must match its route")

  def nSteps: Int =
    steps.length

  def usedInverses: Boolean =
    path.usedInverses

  def isExecutable: Boolean =
    executability.isRight

  /** `Right` when the route can carry coordinates; otherwise the unavailable and map-less steps. */
  def executability: Either[AtlasError, Unit] =
    val unavailable = steps.filter(_.status != TransformStatus.Available)
    val mapless =
      steps.zip(path.morphisms).collect {
        case (step, morphism) if !TransformPlan.carriesCoordinates(morphism) => step
      }
    if unavailable.isEmpty && mapless.isEmpty then Right(())
    else
      val reasons =
        Vector(
          Option.when(unavailable.nonEmpty)(s"unavailable steps=${unavailable.map(TransformPlan.label).mkString(",")}"),
          Option.when(mapless.nonEmpty)(s"steps without a coordinate map=${mapless.map(TransformPlan.label).mkString(",")}")
        ).flatten
      Left(AtlasError.TransformNotExecutable(from, to, reasons.mkString("; ")))

  /** Carry points from `from` to `to` through every step's provider map. */
  def transform(points: Vector[Point3D]): Either[AtlasError, Vector[Point3D]] =
    executability.flatMap { _ =>
      points.foldLeft[Either[AtlasError, Vector[Point3D]]](Right(Vector.empty)) { (acc, point) =>
        acc.flatMap(out => path.push(point).left.map(error => AtlasError.InvalidCoordinate(error.message)).map(out :+ _))
      }
    }

  /** The route as the provider pullback image resampling needs (target world to source world) between two grids.
    * Available only for routes made of affine steps, which fuse into one matrix.
    *
    * The route carries `from` points to `to` points, so the target grid (whose points are pulled back) must be in the
    * `from` world and the source grid in the `to` world. Grids in any other world space, including the unresolved one,
    * are rejected: the route says nothing about their coordinates.
    */
  def pullback[S <: Frame[D3], T <: Frame[D3]](
    source: GridSpec[S],
    target: GridSpec[T]
  ): Either[AtlasError, SpatialPullback[T, S]] =
    for
      _ <- TransformPlan.requireWorld("target", from, fromWorld, target.frame)
      _ <- TransformPlan.requireWorld("source", to, toWorld, source.frame)
      _ <- executability
      executable <- ExecutableAffinePath
        .from(path)
        .left
        .map(error => AtlasError.TransformNotExecutable(from, to, s"grid pullbacks need an affine route: ${error.message}"))
      operator <- executable.coordinateMap match
        case CoordinateMap.Identity => Right(ProviderAffine.identity[D3])
        case CoordinateMap.Geometric(binding) =>
          binding.affineOperator.toRight(AtlasError.TransformNotExecutable(from, to, "route did not fuse into one affine"))
        case _ => Left(AtlasError.TransformNotExecutable(from, to, "route did not fuse into one affine"))
    yield SpatialPullbacks.affine(source, target, operator)

object TransformPlan:
  private def requireWorld(role: String, space: AnySpaceId, expected: WorldSpace, frame: Frame[D3]): Either[AtlasError, Unit] =
    FrameCatalog.worldOf(frame) match
      case Right(world) if world == expected => Right(())
      case Right(world) =>
        Left(AtlasError.GridWorldMismatch(role, space, s"expected ${expected.displayName}, got ${world.displayName}"))
      case Left(error) =>
        Left(AtlasError.GridWorldMismatch(role, space, error.message))

  private[atlas] def build(
    from: AnySpaceId,
    to: AnySpaceId,
    steps: Vector[TransformStep],
    path: MorphismPath,
    dataKind: DataKind,
    fromWorld: WorldSpace,
    toWorld: WorldSpace
  ): TransformPlan =
    val status =
      if steps.exists(_.status == TransformStatus.Planned) then TransformStatus.Planned
      else TransformStatus.Available
    val worst = steps.map(step => confidenceRank(step.confidence)).max
    val confidence =
      worst match
        case 0 => Confidence.Exact
        case 1 => Confidence.High
        case 2 => Confidence.Approximate
        case _ => Confidence.Uncertain
    new TransformPlan(from, to, steps, status, confidence, warningsFor(steps, path, dataKind), path)(fromWorld, toWorld)

  private[atlas] def confidenceRank(confidence: Confidence): Int =
    confidence match
      case Confidence.Exact => 0
      case Confidence.High => 1
      case Confidence.Approximate => 2
      case Confidence.Uncertain => 3

  private def carriesCoordinates(morphism: Morphism): Boolean =
    morphism.coordinateMap match
      case CoordinateMap.Identity | CoordinateMap.Geometric(_) => true
      case _ => false

  private def label(step: TransformStep): String =
    s"${step.from.value}->${step.to.value}:${step.kind}/${step.backend}"

  private def warningsFor(steps: Vector[TransformStep], path: MorphismPath, dataKind: DataKind): Vector[String] =
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
    val inverses =
      if path.usedInverses then Vector("Plan runs transform step(s) through their inverse.")
      else Vector.empty
    planned ++ lowConfidence ++ vertexNearest ++ inverses

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

  /** The standard manifest as a routing graph over [[TemplateCatalog.standard]]. */
  lazy val standardGraph: Either[AtlasError, SpaceTransformGraph] =
    SpaceTransformGraph.build(manifest)

  /** A manifest as a routing graph; spaces outside `catalog` receive fresh template frames. */
  def graph(
    registry: Vector[TransformStep],
    catalog: TemplateCatalog = TemplateCatalog.standard
  ): Either[AtlasError, SpaceTransformGraph] =
    if (registry eq manifest) && (catalog eq TemplateCatalog.standard) then standardGraph
    else SpaceTransformGraph.build(registry, catalog)

  def plan(
    from: AnySpaceId,
    to: AnySpaceId,
    dataKind: DataKind = DataKind.Parcel,
    registry: Vector[TransformStep] = manifest
  ): Either[AtlasError, TransformPlan] =
    graph(registry).flatMap(_.plan(from, to, dataKind))

  def transformCoords(
    points: Vector[Point3D],
    from: AnySpaceId,
    to: AnySpaceId,
    registry: Vector[TransformStep] = manifest
  ): Either[AtlasError, Vector[Point3D]] =
    plan(from, to, DataKind.Voxel, registry).flatMap(_.transform(points))

  def spatialPullback[S <: Frame[D3], T <: Frame[D3]](
    from: AnySpaceId,
    to: AnySpaceId,
    source: GridSpec[S],
    target: GridSpec[T],
    registry: Vector[TransformStep] = manifest
  ): Either[AtlasError, SpatialPullback[T, S]] =
    plan(from, to, DataKind.Voxel, registry).flatMap(_.pullback(source, target))
