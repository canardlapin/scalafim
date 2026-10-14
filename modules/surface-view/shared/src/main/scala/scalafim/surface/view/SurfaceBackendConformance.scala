package scalafim.surface.view

/** Versioned boundary accepted by every surface renderer. Bumping this value
  * makes an incompatible backend visible at admission time instead of during
  * a render.
  */
opaque type SurfacePlanRevision = Int

object SurfacePlanRevision:
  val Current: SurfacePlanRevision = 7

  def make(value: Int): Either[String, SurfacePlanRevision] =
    if value > 0 then Right(value)
    else Left(s"surface plan revision must be positive; got $value")

  def unsafe(value: Int): SurfacePlanRevision =
    make(value).fold(error => throw new IllegalArgumentException(error), identity)

  extension (revision: SurfacePlanRevision)
    def value: Int = revision

opaque type SurfaceBackendId = String

object SurfaceBackendId:
  def make(value: String): Either[String, SurfaceBackendId] =
    val normalized = value.trim
    if normalized.isEmpty then Left("surface backend id must be non-empty")
    else Right(normalized)

  def unsafe(value: String): SurfaceBackendId =
    make(value).fold(error => throw new IllegalArgumentException(error), identity)

  extension (id: SurfaceBackendId)
    def value: String = id

enum SurfaceBackendFeature:
  case DeterministicPixels
  case HardwareAcceleration
  case DepthBuffer
  case BackFaceCulling
  case Lighting
  case WorldClipping
  case BilateralViewports
  case NativePicking
  case HighResolutionSnapshot
  case GpuVolumeProjection
  case FacewiseData
  case NearestVertexSampling
  case ScalarInterpolation
  case FragmentComposition

final case class SurfaceBackendCapabilities(
  id: SurfaceBackendId,
  acceptedRevision: SurfacePlanRevision,
  features: Set[SurfaceBackendFeature],
  caveats: Vector[String] = Vector.empty
):
  def supports(feature: SurfaceBackendFeature): Boolean = features(feature)

enum SurfaceResourceEvent:
  case MeshUploaded(key: SurfaceResourceKey, vertices: Int, triangles: Int, bytes: Long)
  case LayerUploaded(key: SurfaceResourceKey, values: Int, width: Int, height: Int, bytes: Long)
  case CacheHit(key: SurfaceResourceKey)
  case ResourcesDisposed(keys: Vector[SurfaceResourceKey])
  case DrawSubmitted(drawCalls: Int, triangles: Int)
  case Resized(width: Int, height: Int)
  case Picked(surface: SurfaceId, face: Int, vertex: Int)

enum SurfaceRenderPhase:
  case Compile
  case MeshUpload
  case LayerUpload
  case RenderSubmission
  case Snapshot
  case Pick

final case class SurfacePhaseTiming private (phase: SurfaceRenderPhase, elapsedNanos: Long)

object SurfacePhaseTiming:
  def make(phase: SurfaceRenderPhase, elapsedNanos: Long): Either[String, SurfacePhaseTiming] =
    if elapsedNanos < 0L then Left(s"negative $phase timing: $elapsedNanos ns")
    else Right(new SurfacePhaseTiming(phase, elapsedNanos))

  def unsafe(phase: SurfaceRenderPhase, elapsedNanos: Long): SurfacePhaseTiming =
    make(phase, elapsedNanos).fold(error => throw new IllegalArgumentException(error), identity)

enum SurfaceAdmissionPath:
  case ColdLoad
  case CameraOnly
  case StyleUpdate
  case LayerDataUpdate
  case TimepointUpdate
  /** Explicitly permits rebuilding approximation-dependent display topology. */
  case DerivedGeometryUpdate
  case Resize
  case Pick
  case Snapshot

final case class SurfaceBackendObservation(
  capabilities: SurfaceBackendCapabilities,
  revision: SurfacePlanRevision,
  path: SurfaceAdmissionPath,
  events: Vector[SurfaceResourceEvent],
  timings: Vector[SurfacePhaseTiming]
):
  def geometryUploads: Int =
    events.count:
      case _: SurfaceResourceEvent.MeshUploaded => true
      case _ => false

  def layerUploads: Int =
    events.count:
      case _: SurfaceResourceEvent.LayerUploaded => true
      case _ => false

  def uploadedBytes: Long =
    events.iterator.map:
      case SurfaceResourceEvent.MeshUploaded(_, _, _, bytes) => bytes
      case SurfaceResourceEvent.LayerUploaded(_, _, _, _, bytes) => bytes
      case _ => 0L
    .sum

  def drawCalls: Int =
    events.iterator.map:
      case SurfaceResourceEvent.DrawSubmitted(count, _) => count
      case _ => 0
    .sum

enum SurfaceConformanceFamily:
  case Orientation
  case Geometry
  case Layers
  case Thresholds
  case Composition
  case Lighting
  case Layout
  case Picking
  case Lifecycle

enum SurfaceSemanticCase(val family: SurfaceConformanceFamily):
  case AsymmetricOrientation extends SurfaceSemanticCase(SurfaceConformanceFamily.Orientation)
  case WindingAndCulling extends SurfaceSemanticCase(SurfaceConformanceFamily.Geometry)
  case DepthOrdering extends SurfaceSemanticCase(SurfaceConformanceFamily.Geometry)
  case ScalarLayer extends SurfaceSemanticCase(SurfaceConformanceFamily.Layers)
  case LabelLayer extends SurfaceSemanticCase(SurfaceConformanceFamily.Layers)
  case MaskLayer extends SurfaceSemanticCase(SurfaceConformanceFamily.Layers)
  case PackedRgbaLayer extends SurfaceSemanticCase(SurfaceConformanceFamily.Layers)
  case SparseField extends SurfaceSemanticCase(SurfaceConformanceFamily.Layers)
  case ThresholdBoundaries extends SurfaceSemanticCase(SurfaceConformanceFamily.Thresholds)
  case BlendOrder extends SurfaceSemanticCase(SurfaceConformanceFamily.Composition)
  case LitAndUnlit extends SurfaceSemanticCase(SurfaceConformanceFamily.Lighting)
  case BilateralLayout extends SurfaceSemanticCase(SurfaceConformanceFamily.Layout)
  case FaceAndVertexPick extends SurfaceSemanticCase(SurfaceConformanceFamily.Picking)
  case ResizeAndDispose extends SurfaceSemanticCase(SurfaceConformanceFamily.Lifecycle)

final case class SurfaceAdmissionViolation(problem: String)

object SurfaceBackendAdmission:
  def validate(observation: SurfaceBackendObservation): Vector[SurfaceAdmissionViolation] =
    val violations = Vector.newBuilder[SurfaceAdmissionViolation]
    if observation.revision != SurfacePlanRevision.Current then
      violations += SurfaceAdmissionViolation(
        s"backend accepted plan revision ${observation.revision.value}, expected ${SurfacePlanRevision.Current.value}"
      )
    if observation.capabilities.acceptedRevision != observation.revision then
      violations += SurfaceAdmissionViolation("capability and observation plan revisions differ")
    observation.events.foreach:
      case SurfaceResourceEvent.MeshUploaded(_, vertices, triangles, bytes)
          if vertices <= 0 || triangles <= 0 || bytes <= 0L =>
        violations += SurfaceAdmissionViolation("mesh upload counts and bytes must be positive")
      case SurfaceResourceEvent.LayerUploaded(_, values, width, height, bytes)
          if values <= 0 || width <= 0 || height <= 0 || bytes <= 0L =>
        violations += SurfaceAdmissionViolation("layer upload dimensions, counts, and bytes must be positive")
      case SurfaceResourceEvent.DrawSubmitted(drawCalls, triangles)
          if drawCalls <= 0 || triangles < 0 =>
        violations += SurfaceAdmissionViolation("draw-call count must be positive and triangle count non-negative")
      case SurfaceResourceEvent.Resized(width, height) if width <= 0 || height <= 0 =>
        violations += SurfaceAdmissionViolation("resize dimensions must be positive")
      case SurfaceResourceEvent.Picked(_, face, vertex) if face < 0 || vertex < 0 =>
        violations += SurfaceAdmissionViolation("picked face and vertex must be non-negative")
      case _ => ()
    observation.path match
      case SurfaceAdmissionPath.CameraOnly if observation.geometryUploads != 0 || observation.layerUploads != 0 =>
        violations += SurfaceAdmissionViolation("camera-only rendering must perform zero mesh and layer uploads")
      case SurfaceAdmissionPath.StyleUpdate if observation.geometryUploads != 0 =>
        violations += SurfaceAdmissionViolation("style updates must perform zero mesh uploads")
      case SurfaceAdmissionPath.LayerDataUpdate | SurfaceAdmissionPath.TimepointUpdate
          if observation.geometryUploads != 0 =>
        violations += SurfaceAdmissionViolation("layer-data and timepoint updates must perform zero mesh uploads")
      case _ => ()
    violations.result()

  val requiredSemanticCases: Vector[SurfaceSemanticCase] = SurfaceSemanticCase.values.toVector
