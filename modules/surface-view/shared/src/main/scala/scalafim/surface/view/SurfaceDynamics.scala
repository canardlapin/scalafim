package scalafim.surface.view

import scalafim.image.SampleSpaces.*

import intaglio.*
import scalafim.image.*
import scalafim.surface.*

opaque type SurfaceSeconds = Double

object SurfaceSeconds:
  def make(value: Double): Either[SurfaceViewError, SurfaceSeconds] =
    if value.isFinite then Right(value)
    else Left(SurfaceViewError.InvalidTimeAxis(s"time must be finite; got $value"))

  def unsafe(value: Double): SurfaceSeconds =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (time: SurfaceSeconds)
    def value: Double = time

enum SurfaceInterpolation:
  case Nearest, Linear

final case class SurfaceFrameSample(lowerFrame: Int, upperFrame: Int, alpha: Double):
  require(lowerFrame >= 0 && upperFrame >= lowerFrame, "surface frame sample indices must be ordered")
  require(alpha.isFinite && alpha >= 0.0 && alpha <= 1.0, "surface frame sample alpha must be in [0, 1]")

final case class SurfaceTimeAxis private (times: Vector[SurfaceSeconds]):
  def frameCount: Int = times.length
  def first: SurfaceSeconds = times.head
  def last: SurfaceSeconds = times.last
  def timeAt(frame: Int): Either[SurfaceViewError, SurfaceSeconds] =
    if frame >= 0 && frame < frameCount then Right(times(frame))
    else Left(SurfaceViewError.TimepointOutOfBounds(frame, frameCount))

  def sample(time: SurfaceSeconds, interpolation: SurfaceInterpolation): Either[SurfaceViewError, SurfaceFrameSample] =
    val value = time.value
    if value < first.value || value > last.value then
      Left(SurfaceViewError.TimeOutsideAxis(value, first.value, last.value))
    else if frameCount == 1 || value == last.value then
      Right(SurfaceFrameSample(frameCount - 1, frameCount - 1, 0.0))
    else
      var upper = 1
      while upper < frameCount && times(upper).value < value do upper += 1
      val lower = upper - 1
      val span = times(upper).value - times(lower).value
      val alpha = (value - times(lower).value) / span
      interpolation match
        case SurfaceInterpolation.Linear => Right(SurfaceFrameSample(lower, upper, alpha))
        case SurfaceInterpolation.Nearest =>
          val frame = if alpha < 0.5 then lower else upper
          Right(SurfaceFrameSample(frame, frame, 0.0))

object SurfaceTimeAxis:
  def make(times: Vector[SurfaceSeconds]): Either[SurfaceViewError, SurfaceTimeAxis] =
    if times.isEmpty then Left(SurfaceViewError.InvalidTimeAxis("at least one time is required"))
    else
      var index = 1
      while index < times.length do
        if times(index).value <= times(index - 1).value then
          return Left(SurfaceViewError.InvalidTimeAxis("times must be strictly increasing"))
        index += 1
      Right(new SurfaceTimeAxis(times))

  def uniform(
    frameCount: Int,
    start: SurfaceSeconds = SurfaceSeconds.unsafe(0.0),
    step: SurfaceSeconds = SurfaceSeconds.unsafe(1.0)
  ): Either[SurfaceViewError, SurfaceTimeAxis] =
    if frameCount <= 0 then Left(SurfaceViewError.InvalidFrameCount(frameCount))
    else if step.value <= 0.0 then Left(SurfaceViewError.InvalidTimeAxis(s"uniform step must be positive; got ${step.value}"))
    else make(Vector.tabulate(frameCount)(index => SurfaceSeconds.unsafe(start.value + index * step.value)))

opaque type SurfaceTemporalFieldId = String

object SurfaceTemporalFieldId:
  def make(value: String): Either[SurfaceViewError, SurfaceTemporalFieldId] =
    val normalized = value.trim
    if normalized.isEmpty then Left(SurfaceViewError.BlankTemporalFieldId)
    else Right(normalized)

  def unsafe(value: String): SurfaceTemporalFieldId =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: SurfaceTemporalFieldId)
    def value: String = id

trait SurfaceDoubleFrameSource:
  def frameCount: Int
  def readFrame(index: Int): Either[SurfaceViewError, Array[Double]]

final class SurfaceTemporalField private (
  val id: SurfaceTemporalFieldId,
  val geometry: SurfaceGeometry,
  val axis: SurfaceTimeAxis,
  val label: String,
  source: SurfaceDoubleFrameSource
):
  def frameCount: Int = axis.frameCount

  private[view] def readFrame(index: Int): Either[SurfaceViewError, Array[Double]] =
    if index < 0 || index >= frameCount then Left(SurfaceViewError.TimepointOutOfBounds(index, frameCount))
    else
      source.readFrame(index).flatMap: values =>
        if values.length == geometry.vertexCount then Right(values.clone())
        else Left(SurfaceViewError.FrameReadFailed(
          id.value,
          index,
          s"expected ${geometry.vertexCount} values; got ${values.length}"
        ))

object SurfaceTemporalField:
  def fromMatrix(
    id: SurfaceTemporalFieldId,
    matrix: SurfaceMatrix[Double],
    axis: SurfaceTimeAxis,
    label: String = ""
  ): Either[SurfaceViewError, SurfaceTemporalField] =
    if matrix.columns != axis.frameCount then
      Left(SurfaceViewError.IncompatibleFrameCounts(Vector(matrix.columns, axis.frameCount)))
    else if matrix.rows != matrix.geometry.vertexCount then
      Left(SurfaceViewError.InvalidDataLength(matrix.geometry.vertexCount, matrix.rows))
    else
      var row = 0
      while row < matrix.rows do
        if matrix.rowVertex(row).index != row then
          return Left(SurfaceViewError.InvalidTimeAxis("temporal matrices must contain every vertex in canonical order"))
        row += 1
      val source = new SurfaceDoubleFrameSource:
        val frameCount: Int = matrix.columns
        def readFrame(index: Int): Either[SurfaceViewError, Array[Double]] =
          if index < 0 || index >= frameCount then Left(SurfaceViewError.TimepointOutOfBounds(index, frameCount))
          else
            val values = new Array[Double](matrix.rows)
            var row = 0
            while row < values.length do
              values(row) = matrix(row, index)
              row += 1
            Right(values)
      Right(new SurfaceTemporalField(id, matrix.geometry, axis, label, source))

  def lazyFrames(
    id: SurfaceTemporalFieldId,
    geometry: SurfaceGeometry,
    axis: SurfaceTimeAxis,
    source: SurfaceDoubleFrameSource,
    label: String = ""
  ): Either[SurfaceViewError, SurfaceTemporalField] =
    if source.frameCount != axis.frameCount then
      Left(SurfaceViewError.IncompatibleFrameCounts(Vector(source.frameCount, axis.frameCount)))
    else Right(new SurfaceTemporalField(id, geometry, axis, label, source))

final class SurfaceDoubleFrame private[view] (private val values: Array[Double]):
  def length: Int = values.length
  inline def apply(vertex: Int): Double = values(vertex)
  private[view] def copyValues: Array[Double] = values.clone()

private final case class SurfaceFrameCacheKey(field: SurfaceTemporalFieldId, frame: Int)

final case class SurfaceFrameCacheReceipt(
  requestedFrames: Vector[Int],
  hits: Int,
  misses: Int,
  sourceReads: Int,
  evictedFrames: Vector[Int],
  prefetchedFrames: Vector[Int]
):
  private[view] def +(other: SurfaceFrameCacheReceipt): SurfaceFrameCacheReceipt =
    SurfaceFrameCacheReceipt(
      requestedFrames ++ other.requestedFrames,
      hits + other.hits,
      misses + other.misses,
      sourceReads + other.sourceReads,
      evictedFrames ++ other.evictedFrames,
      prefetchedFrames ++ other.prefetchedFrames
    )

object SurfaceFrameCacheReceipt:
  val Empty: SurfaceFrameCacheReceipt = SurfaceFrameCacheReceipt(Vector.empty, 0, 0, 0, Vector.empty, Vector.empty)

final case class SurfaceFrameCache private (
  capacity: Int,
  private val entries: Map[SurfaceFrameCacheKey, SurfaceDoubleFrame],
  private val recency: Vector[SurfaceFrameCacheKey]
):
  def size: Int = entries.size
  def cachedFrames(field: SurfaceTemporalFieldId): Vector[Int] =
    recency.collect { case SurfaceFrameCacheKey(`field`, frame) => frame }

  private[view] def load(
    field: SurfaceTemporalField,
    frame: Int
  ): Either[SurfaceViewError, (SurfaceDoubleFrame, SurfaceFrameCache, SurfaceFrameCacheReceipt)] =
    val key = SurfaceFrameCacheKey(field.id, frame)
    entries.get(key) match
      case Some(values) =>
        val refreshed = copy(recency = recency.filterNot(_ == key) :+ key)
        Right((values, refreshed, SurfaceFrameCacheReceipt(Vector(frame), 1, 0, 0, Vector.empty, Vector.empty)))
      case None =>
        field.readFrame(frame).map: values =>
          val loaded = new SurfaceDoubleFrame(values)
          if capacity == 0 then
            (loaded, this, SurfaceFrameCacheReceipt(Vector(frame), 0, 1, 1, Vector.empty, Vector.empty))
          else
            val (baseEntries, baseRecency, evicted) =
              if entries.size >= capacity then
                val victim = recency.head
                (entries.removed(victim), recency.tail, Vector(victim.frame))
              else (entries, recency, Vector.empty[Int])
            val next = copy(entries = baseEntries.updated(key, loaded), recency = baseRecency :+ key)
            (loaded, next, SurfaceFrameCacheReceipt(Vector(frame), 0, 1, 1, evicted, Vector.empty))

  def sample(
    field: SurfaceTemporalField,
    time: SurfaceSeconds,
    interpolation: SurfaceInterpolation
  ): Either[SurfaceViewError, SurfaceCachedSample] =
    field.axis.sample(time, interpolation).flatMap: sample =>
      load(field, sample.lowerFrame).flatMap: (lower, afterLower, lowerReceipt) =>
        if sample.upperFrame == sample.lowerFrame then
          Right(SurfaceCachedSample(lower, sample, afterLower, lowerReceipt))
        else
          afterLower.load(field, sample.upperFrame).map: (upper, afterUpper, upperReceipt) =>
            val values = new Array[Double](lower.length)
            var vertex = 0
            while vertex < values.length do
              values(vertex) = lower(vertex) + sample.alpha * (upper(vertex) - lower(vertex))
              vertex += 1
            SurfaceCachedSample(new SurfaceDoubleFrame(values), sample, afterUpper, lowerReceipt + upperReceipt)

  def prefetch(
    field: SurfaceTemporalField,
    frames: Vector[Int]
  ): Either[SurfaceViewError, SurfacePrefetchResult] =
    var current = this
    var receipt = SurfaceFrameCacheReceipt.Empty
    val distinct = frames.distinct
    var index = 0
    while index < distinct.length do
      val frame = distinct(index)
      current.load(field, frame) match
        case Left(error) => return Left(error)
        case Right((_, next, readReceipt)) =>
          current = next
          receipt = receipt + readReceipt.copy(prefetchedFrames = Vector(frame))
      index += 1
    Right(SurfacePrefetchResult(current, receipt))

object SurfaceFrameCache:
  val Disabled: SurfaceFrameCache = new SurfaceFrameCache(0, Map.empty, Vector.empty)

  def make(capacity: Int): Either[SurfaceViewError, SurfaceFrameCache] =
    if capacity < 0 then Left(SurfaceViewError.InvalidFrameCacheCapacity(capacity))
    else Right(new SurfaceFrameCache(capacity, Map.empty, Vector.empty))

  def empty(capacity: Int): SurfaceFrameCache =
    make(capacity).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class SurfaceCachedSample(
  frame: SurfaceDoubleFrame,
  sample: SurfaceFrameSample,
  cache: SurfaceFrameCache,
  receipt: SurfaceFrameCacheReceipt
)

final case class SurfacePrefetchResult(cache: SurfaceFrameCache, receipt: SurfaceFrameCacheReceipt)

opaque type SurfacePlaybackRate = Double

object SurfacePlaybackRate:
  def make(value: Double): Either[SurfaceViewError, SurfacePlaybackRate] =
    if value.isFinite && value != 0.0 then Right(value)
    else Left(SurfaceViewError.InvalidPlaybackRate(value))

  def unsafe(value: Double): SurfacePlaybackRate =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (rate: SurfacePlaybackRate)
    def value: Double = rate

enum SurfacePlaybackStatus:
  case Paused, Playing

final case class SurfacePlaybackState(
  playhead: SurfaceSeconds,
  status: SurfacePlaybackStatus,
  rate: SurfacePlaybackRate,
  looping: Boolean
)

object SurfacePlaybackState:
  def initial(axis: SurfaceTimeAxis): SurfacePlaybackState =
    SurfacePlaybackState(axis.first, SurfacePlaybackStatus.Paused, SurfacePlaybackRate.unsafe(1.0), looping = false)

enum SurfacePlaybackAction:
  case Play
  case Pause
  case Seek(time: SurfaceSeconds)
  case SetRate(rate: SurfacePlaybackRate)
  case SetLooping(enabled: Boolean)
  case Step(frames: Int)
  case Tick(delta: SurfaceSeconds)

object SurfacePlayback:
  def reduce(
    axis: SurfaceTimeAxis,
    state: SurfacePlaybackState,
    action: SurfacePlaybackAction
  ): Either[SurfaceViewError, SurfacePlaybackState] =
    action match
      case SurfacePlaybackAction.Play => Right(state.copy(status = SurfacePlaybackStatus.Playing))
      case SurfacePlaybackAction.Pause => Right(state.copy(status = SurfacePlaybackStatus.Paused))
      case SurfacePlaybackAction.SetRate(rate) => Right(state.copy(rate = rate))
      case SurfacePlaybackAction.SetLooping(enabled) => Right(state.copy(looping = enabled))
      case SurfacePlaybackAction.Seek(time) =>
        axis.sample(time, SurfaceInterpolation.Nearest).map(_ => state.copy(playhead = time))
      case SurfacePlaybackAction.Step(frames) =>
        axis.sample(state.playhead, SurfaceInterpolation.Nearest).flatMap: current =>
          val target = (current.lowerFrame + frames).max(0).min(axis.frameCount - 1)
          axis.timeAt(target).map(time => state.copy(playhead = time))
      case SurfacePlaybackAction.Tick(delta) =>
        if delta.value < 0.0 then Left(SurfaceViewError.InvalidPlaybackDelta(delta.value))
        else if state.status == SurfacePlaybackStatus.Paused then Right(state)
        else
          val candidate = state.playhead.value + delta.value * state.rate.value
          val minimum = axis.first.value
          val maximum = axis.last.value
          if candidate >= minimum && candidate <= maximum then
            Right(state.copy(playhead = SurfaceSeconds.unsafe(candidate)))
          else if state.looping && maximum > minimum then
            val span = maximum - minimum
            val wrapped = minimum + ((candidate - minimum) % span + span) % span
            Right(state.copy(playhead = SurfaceSeconds.unsafe(wrapped)))
          else
            val clamped = candidate.max(minimum).min(maximum)
            Right(state.copy(playhead = SurfaceSeconds.unsafe(clamped), status = SurfacePlaybackStatus.Paused))

  def prefetchFrames(axis: SurfaceTimeAxis, state: SurfacePlaybackState, count: Int): Vector[Int] =
    if count <= 0 then Vector.empty
    else
      val current = axis.sample(state.playhead, SurfaceInterpolation.Nearest).toOption.map(_.lowerFrame).getOrElse(0)
      val direction = if state.rate.value >= 0.0 then 1 else -1
      Vector.tabulate(count)(offset => current + direction * (offset + 1))
        .filter(frame => frame >= 0 && frame < axis.frameCount)

  def prepareScalar(
    field: SurfaceTemporalField,
    layerId: SurfaceLayerId,
    surfaceId: SurfaceId,
    time: SurfaceSeconds,
    interpolation: SurfaceInterpolation,
    colorizer: Colorizer[Double],
    cache: SurfaceFrameCache,
    prefetchFrames: Vector[Int] = Vector.empty,
    previousSample: Option[SurfaceFrameSample] = None,
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal
  ): Either[SurfaceViewError, PreparedSurfaceLayer] =
    cache.sample(field, time, interpolation).flatMap: sampled =>
      sampled.cache.prefetch(field, prefetchFrames).flatMap: prefetched =>
        SurfaceLayer.scalar(
          layerId,
          surfaceId,
          field.geometry,
          sampled.frame.copyValues,
          colorizer,
          opacity = opacity,
          blendMode = blendMode
        ).map: layer =>
          PreparedSurfaceLayer(
            layer,
            prefetched.cache,
            SurfacePlaybackReceipt(
              sampled.sample,
              sampled.receipt + prefetched.receipt,
              previousSample.forall(_ != sampled.sample)
            )
          )

final case class SurfacePlaybackReceipt(
  sample: SurfaceFrameSample,
  cache: SurfaceFrameCacheReceipt,
  layerUploadRequired: Boolean
)

final case class PreparedSurfaceLayer(layer: SurfaceLayer, cache: SurfaceFrameCache, receipt: SurfacePlaybackReceipt)

opaque type SurfaceMorphFraction = Double

object SurfaceMorphFraction:
  def make(value: Double): Either[SurfaceViewError, SurfaceMorphFraction] =
    if value.isFinite && value >= 0.0 && value <= 1.0 then Right(value)
    else Left(SurfaceViewError.InvalidMorphFraction(value))

  def unsafe(value: Double): SurfaceMorphFraction =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (fraction: SurfaceMorphFraction)
    def value: Double = fraction

opaque type SurfaceLensRadius = Double

object SurfaceLensRadius:
  def make(value: Double): Either[SurfaceViewError, SurfaceLensRadius] =
    if value.isFinite && value >= 0.0 then Right(value)
    else Left(SurfaceViewError.InvalidLensRadius(value))

  def unsafe(value: Double): SurfaceLensRadius =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (radius: SurfaceLensRadius)
    def value: Double = radius

opaque type SurfaceLensRelaxationSteps = Int

object SurfaceLensRelaxationSteps:
  def make(value: Int): Either[SurfaceViewError, SurfaceLensRelaxationSteps] =
    if value >= 1 && value <= 1000 then Right(value)
    else Left(SurfaceViewError.InvalidLensRelaxationSteps(value))

  def unsafe(value: Int): SurfaceLensRelaxationSteps =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (steps: SurfaceLensRelaxationSteps)
    def value: Int = steps

enum SurfaceLensDeformationPolicy:
  /** Corresponding target vertices are blended independently. This is useful
    * as a diagnostic oracle, but can visibly shear a transition collar.
    */
  case DirectCorrespondence

  /** Keep the selected vertex fixed in space, preserve the aligned target
    * displacement in the core, and solve a discrete harmonic field through
    * the collar. The solve happens when the lens is pinned, never per frame.
    */
  case PinnedHarmonic(steps: SurfaceLensRelaxationSteps)

object SurfaceLensDeformationPolicy:
  val Natural: SurfaceLensDeformationPolicy =
    SurfaceLensDeformationPolicy.PinnedHarmonic(SurfaceLensRelaxationSteps.unsafe(64))

/** Immutable geodesic deformation mask computed once when a lens is pinned.
  * Distances are measured along the source mesh in surface-coordinate units;
  * the smootherstep falloff is one inside `innerRadius` and zero at and beyond
  * `outerRadius`.
  */
final class SurfaceGeodesicLens private (
  val domain: SurfaceMeshDomain,
  val center: VertexId,
  val innerRadius: SurfaceLensRadius,
  val outerRadius: SurfaceLensRadius,
  private val weights: Array[Double]
):
  def vertexCount: Int = weights.length

  def weightAt(vertex: VertexId): Option[Double] =
    if vertex.index >= 0 && vertex.index < weights.length then Some(weights(vertex.index)) else None

  def activeVertexCount: Int =
    var count = 0
    var vertex = 0
    while vertex < weights.length do
      if weights(vertex) > 0.0 then count += 1
      vertex += 1
    count

  private[view] inline def weightAtUnsafe(vertex: Int): Double = weights(vertex)

  private[view] def isCompatibleWith(geometry: SurfaceGeometry): Boolean =
    geometry.meshDomainEither.toOption.contains(domain) && geometry.vertexCount == weights.length

  override def equals(other: Any): Boolean =
    other match
      case that: SurfaceGeodesicLens =>
        domain == that.domain && center == that.center && innerRadius == that.innerRadius &&
          outerRadius == that.outerRadius && java.util.Arrays.equals(weights, that.weights)
      case _ => false

  override def hashCode(): Int =
    var result = domain.hashCode()
    result = 31 * result + center.hashCode()
    result = 31 * result + innerRadius.hashCode()
    result = 31 * result + outerRadius.hashCode()
    31 * result + java.util.Arrays.hashCode(weights)

object SurfaceGeodesicLens:
  def make(
    geometry: SurfaceGeometry,
    center: VertexId,
    innerRadius: SurfaceLensRadius,
    outerRadius: SurfaceLensRadius
  ): Either[SurfaceViewError, SurfaceGeodesicLens] =
    if center.index < 0 || center.index >= geometry.vertexCount then
      Left(SurfaceViewError.InvalidVertexIndex(center.index, geometry.vertexCount))
    else if innerRadius.value >= outerRadius.value then
      Left(SurfaceViewError.InvalidLensBand(innerRadius.value, outerRadius.value))
    else
      geometry.meshDomainEither
        .left.map(error => SurfaceViewError.IncompatibleMorph(error.message))
        .map: domain =>
          val topology = MeshTopology.from(geometry)
          val targets = Vector.tabulate(geometry.vertexCount)(VertexId.apply)
          val distances = SurfaceGeodesics.distances(topology, center, targets)
          val weights = new Array[Double](geometry.vertexCount)
          var vertex = 0
          while vertex < weights.length do
            val distance = distances(vertex)
            weights(vertex) =
              if distance <= innerRadius.value then 1.0
              else if distance >= outerRadius.value || !distance.isFinite then 0.0
              else
                val amount = (outerRadius.value - distance) / (outerRadius.value - innerRadius.value)
                amount * amount * amount * (amount * (amount * 6.0 - 15.0) + 10.0)
            vertex += 1
          new SurfaceGeodesicLens(domain, center, innerRadius, outerRadius, weights)

final case class SurfaceLensQuality(
  invertedTriangles: Int,
  minimumAreaRatio: Double,
  maximumAreaRatio: Double,
  maximumEdgeStrain: Double,
  p95EdgeStrain: Double
):
  require(invertedTriangles >= 0, "inverted triangle count must be non-negative")
  require(minimumAreaRatio.isFinite && minimumAreaRatio >= 0.0, "minimum area ratio must be finite and non-negative")
  require(maximumAreaRatio.isFinite && maximumAreaRatio >= minimumAreaRatio, "maximum area ratio must be finite and ordered")
  require(maximumEdgeStrain.isFinite && maximumEdgeStrain >= 0.0, "maximum edge strain must be finite and non-negative")
  require(p95EdgeStrain.isFinite && p95EdgeStrain >= 0.0, "p95 edge strain must be finite and non-negative")

/** A deformation field owned by one exact mesh domain. Its primitive
  * displacement buffer is computed once at pin time and reused by JVM and JS
  * compilers for every animation frame.
  */
final class SurfaceLensDeformation private (
  val lens: SurfaceGeodesicLens,
  val policy: SurfaceLensDeformationPolicy,
  val quality: SurfaceLensQuality,
  private val displacements: Array[Double]
):
  def vertexCount: Int = lens.vertexCount

  def displacementAt(vertex: VertexId): Option[Point3D] =
    if vertex.index < 0 || vertex.index >= vertexCount then None
    else
      val offset = vertex.index * 3
      Some(Point3D(displacements(offset), displacements(offset + 1), displacements(offset + 2)))

  private[view] inline def displacementAtUnsafe(offset: Int): Double = displacements(offset)

  private[view] def isCompatibleWith(geometry: SurfaceGeometry): Boolean =
    lens.isCompatibleWith(geometry) && displacements.length == geometry.vertexCount * 3

object SurfaceLensDeformation:
  def make(
    from: SurfaceGeometry,
    to: SurfaceGeometry,
    lens: SurfaceGeodesicLens,
    policy: SurfaceLensDeformationPolicy = SurfaceLensDeformationPolicy.Natural
  ): Either[SurfaceViewError, SurfaceLensDeformation] =
    if !from.hasSameMeshDomain(to) then
      Left(SurfaceViewError.IncompatibleMorph("lens endpoints must have the same exact ordered topology"))
    else if from.surfaceToWorld != to.surfaceToWorld then
      Left(SurfaceViewError.IncompatibleMorph("lens endpoint surface-to-world transforms must match"))
    else if !lens.isCompatibleWith(from) then
      Left(SurfaceViewError.IncompatibleMorph("lens weights do not belong to the source mesh domain"))
    else
      val topology = MeshTopology.from(from)
      val displacement = rawDisplacement(from, to, lens, policy)
      policy match
        case SurfaceLensDeformationPolicy.DirectCorrespondence => ()
        case SurfaceLensDeformationPolicy.PinnedHarmonic(steps) =>
          relaxCollar(topology, lens, displacement, steps.value)
      val quality = measureQuality(from.mesh, topology, displacement)
      policy match
        case SurfaceLensDeformationPolicy.PinnedHarmonic(_) if quality.invertedTriangles > 0 =>
          Left(SurfaceViewError.UnsafeLensDeformation(quality.invertedTriangles))
        case _ => Right(new SurfaceLensDeformation(lens, policy, quality, displacement))

  private def rawDisplacement(
    from: SurfaceGeometry,
    to: SurfaceGeometry,
    lens: SurfaceGeodesicLens,
    policy: SurfaceLensDeformationPolicy
  ): Array[Double] =
    val result = new Array[Double](from.vertexCount * 3)
    val centerOffset = lens.center.index * 3
    val (pinX, pinY, pinZ) = policy match
      case SurfaceLensDeformationPolicy.DirectCorrespondence => (0.0, 0.0, 0.0)
      case SurfaceLensDeformationPolicy.PinnedHarmonic(_) =>
        (
          to.mesh.coordinates(centerOffset) - from.mesh.coordinates(centerOffset),
          to.mesh.coordinates(centerOffset + 1) - from.mesh.coordinates(centerOffset + 1),
          to.mesh.coordinates(centerOffset + 2) - from.mesh.coordinates(centerOffset + 2)
        )
    var vertex = 0
    while vertex < from.vertexCount do
      val offset = vertex * 3
      val weight = lens.weightAtUnsafe(vertex)
      result(offset) = weight * (to.mesh.coordinates(offset) - from.mesh.coordinates(offset) - pinX)
      result(offset + 1) = weight * (to.mesh.coordinates(offset + 1) - from.mesh.coordinates(offset + 1) - pinY)
      result(offset + 2) = weight * (to.mesh.coordinates(offset + 2) - from.mesh.coordinates(offset + 2) - pinZ)
      vertex += 1
    result

  private def relaxCollar(
    topology: MeshTopology,
    lens: SurfaceGeodesicLens,
    displacement: Array[Double],
    steps: Int
  ): Unit =
    val collar = new Array[Int](lens.vertexCount)
    var collarCount = 0
    var vertex = 0
    while vertex < lens.vertexCount do
      val weight = lens.weightAtUnsafe(vertex)
      if weight > 0.0 && weight < 1.0 then
        collar(collarCount) = vertex
        collarCount += 1
      vertex += 1
    var current = displacement
    var next = displacement.clone()
    var iteration = 0
    while iteration < steps do
      var collarIndex = 0
      while collarIndex < collarCount do
        vertex = collar(collarIndex)
        val neighbors = topology.neighborsOf(VertexId.unsafe(vertex))
        var x = 0.0
        var y = 0.0
        var z = 0.0
        var neighbor = 0
        while neighbor < neighbors.length do
          val offset = neighbors(neighbor).index * 3
          x += current(offset)
          y += current(offset + 1)
          z += current(offset + 2)
          neighbor += 1
        val offset = vertex * 3
        if neighbors.nonEmpty then
          val inverseDegree = 1.0 / neighbors.length.toDouble
          next(offset) = x * inverseDegree
          next(offset + 1) = y * inverseDegree
          next(offset + 2) = z * inverseDegree
        else
          next(offset) = 0.0
          next(offset + 1) = 0.0
          next(offset + 2) = 0.0
        collarIndex += 1
      val swap = current
      current = next
      next = swap
      iteration += 1
    if current ne displacement then
      var index = 0
      while index < displacement.length do
        displacement(index) = current(index)
        index += 1

  private def measureQuality(
    mesh: TriangleMesh,
    topology: MeshTopology,
    displacement: Array[Double]
  ): SurfaceLensQuality =
    var inverted = 0
    var minimumAreaRatio = Double.PositiveInfinity
    var maximumAreaRatio = 0.0
    var face = 0
    while face < mesh.faceCount do
      val offset = face * 3
      val a = mesh.faceIndices(offset) * 3
      val b = mesh.faceIndices(offset + 1) * 3
      val c = mesh.faceIndices(offset + 2) * 3
      val sourceAbX = mesh.coordinates(b) - mesh.coordinates(a)
      val sourceAbY = mesh.coordinates(b + 1) - mesh.coordinates(a + 1)
      val sourceAbZ = mesh.coordinates(b + 2) - mesh.coordinates(a + 2)
      val sourceAcX = mesh.coordinates(c) - mesh.coordinates(a)
      val sourceAcY = mesh.coordinates(c + 1) - mesh.coordinates(a + 1)
      val sourceAcZ = mesh.coordinates(c + 2) - mesh.coordinates(a + 2)
      val displacementAbX = displacement(b) - displacement(a)
      val displacementAbY = displacement(b + 1) - displacement(a + 1)
      val displacementAbZ = displacement(b + 2) - displacement(a + 2)
      val displacementAcX = displacement(c) - displacement(a)
      val displacementAcY = displacement(c + 1) - displacement(a + 1)
      val displacementAcZ = displacement(c + 2) - displacement(a + 2)
      val sourceNx = sourceAbY * sourceAcZ - sourceAbZ * sourceAcY
      val sourceNy = sourceAbZ * sourceAcX - sourceAbX * sourceAcZ
      val sourceNz = sourceAbX * sourceAcY - sourceAbY * sourceAcX
      val deformedAbX = sourceAbX + displacementAbX
      val deformedAbY = sourceAbY + displacementAbY
      val deformedAbZ = sourceAbZ + displacementAbZ
      val deformedAcX = sourceAcX + displacementAcX
      val deformedAcY = sourceAcY + displacementAcY
      val deformedAcZ = sourceAcZ + displacementAcZ
      val deformedNx = deformedAbY * deformedAcZ - deformedAbZ * deformedAcY
      val deformedNy = deformedAbZ * deformedAcX - deformedAbX * deformedAcZ
      val deformedNz = deformedAbX * deformedAcY - deformedAbY * deformedAcX
      val sourceNorm = math.sqrt(sourceNx * sourceNx + sourceNy * sourceNy + sourceNz * sourceNz)
      val deformedNorm = math.sqrt(deformedNx * deformedNx + deformedNy * deformedNy + deformedNz * deformedNz)
      val ratio = if sourceNorm <= 1e-15 then 1.0 else deformedNorm / sourceNorm
      minimumAreaRatio = math.min(minimumAreaRatio, ratio)
      maximumAreaRatio = math.max(maximumAreaRatio, ratio)
      if sourceNorm > 1e-15 then
        val linearNx =
          displacementAbY * sourceAcZ - displacementAbZ * sourceAcY +
            sourceAbY * displacementAcZ - sourceAbZ * displacementAcY
        val linearNy =
          displacementAbZ * sourceAcX - displacementAbX * sourceAcZ +
            sourceAbZ * displacementAcX - sourceAbX * displacementAcZ
        val linearNz =
          displacementAbX * sourceAcY - displacementAbY * sourceAcX +
            sourceAbX * displacementAcY - sourceAbY * displacementAcX
        val quadraticNx = displacementAbY * displacementAcZ - displacementAbZ * displacementAcY
        val quadraticNy = displacementAbZ * displacementAcX - displacementAbX * displacementAcZ
        val quadraticNz = displacementAbX * displacementAcY - displacementAbY * displacementAcX
        val constant = sourceNorm * sourceNorm
        val linear = sourceNx * linearNx + sourceNy * linearNy + sourceNz * linearNz
        val quadratic = sourceNx * quadraticNx + sourceNy * quadraticNy + sourceNz * quadraticNz
        var minimumOrientation = math.min(constant, constant + linear + quadratic)
        if quadratic > 0.0 then
          val turningPoint = -linear / (2.0 * quadratic)
          if turningPoint > 0.0 && turningPoint < 1.0 then
            minimumOrientation = math.min(
              minimumOrientation,
              constant + linear * turningPoint + quadratic * turningPoint * turningPoint
            )
        if minimumOrientation <= 0.0 then inverted += 1
      face += 1

    val strains = new Array[Double](topology.edgeCount)
    var edgeIndex = 0
    var maximumEdgeStrain = 0.0
    while edgeIndex < topology.edgeCount do
      val edge = topology.edges(edgeIndex)
      val a = edge.a.index * 3
      val b = edge.b.index * 3
      val dx = mesh.coordinates(b) + displacement(b) - mesh.coordinates(a) - displacement(a)
      val dy = mesh.coordinates(b + 1) + displacement(b + 1) - mesh.coordinates(a + 1) - displacement(a + 1)
      val dz = mesh.coordinates(b + 2) + displacement(b + 2) - mesh.coordinates(a + 2) - displacement(a + 2)
      val deformedLength = math.sqrt(dx * dx + dy * dy + dz * dz)
      val sourceLength = topology.edgeLengths(edgeIndex)
      val strain = if sourceLength <= 1e-15 then 0.0 else math.abs(deformedLength / sourceLength - 1.0)
      strains(edgeIndex) = strain
      maximumEdgeStrain = math.max(maximumEdgeStrain, strain)
      edgeIndex += 1
    scala.util.Sorting.quickSort(strains)
    val p95Index = math.ceil(strains.length.toDouble * 0.95).toInt.min(strains.length - 1).max(0)
    SurfaceLensQuality(inverted, minimumAreaRatio, maximumAreaRatio, maximumEdgeStrain, strains(p95Index))

enum SurfaceNormalPolicy:
  case RecomputeFromGeometry

enum SurfaceGeometryPresentation:
  case Fixed(kind: SurfaceKind)
  case Morphing(from: SurfaceKind, to: SurfaceKind, fraction: SurfaceMorphFraction)
  case RevealLens(
    from: SurfaceKind,
    to: SurfaceKind,
    deformation: SurfaceLensDeformation,
    fraction: SurfaceMorphFraction
  )

  private[view] def resolve(surfaces: SurfaceSet): Either[SurfaceViewError, SurfaceGeometry] =
    this match
      case Fixed(kind) =>
        surfaces.get(kind).toRight(SurfaceViewError.IncompatibleMorph(
          s"surface variant '${kind.label}' does not exist"
        ))
      case Morphing(from, to, fraction) =>
        SurfaceMorph.between(surfaces, from, to, fraction)
      case RevealLens(from, to, deformation, fraction) =>
        SurfaceMorph.reveal(surfaces, from, to, deformation, fraction)

object SurfaceMorph:
  def interpolate(
    from: SurfaceGeometry,
    to: SurfaceGeometry,
    fraction: SurfaceMorphFraction,
    normalPolicy: SurfaceNormalPolicy = SurfaceNormalPolicy.RecomputeFromGeometry
  ): Either[SurfaceViewError, SurfaceGeometry] =
    if !from.hasSameMeshDomain(to) then
      Left(SurfaceViewError.IncompatibleMorph("hemisphere and exact ordered topology must match"))
    else if from.surfaceToWorld != to.surfaceToWorld then
      Left(SurfaceViewError.IncompatibleMorph("surface-to-world transforms must match"))
    else if fraction.value == 0.0 then Right(from)
    else if fraction.value == 1.0 then Right(to)
    else
      normalPolicy match
        case SurfaceNormalPolicy.RecomputeFromGeometry =>
          val coordinates = new Array[Double](from.vertexCount * 3)
          var index = 0
          while index < coordinates.length do
            coordinates(index) = from.mesh.coordinates(index) +
              fraction.value * (to.mesh.coordinates(index) - from.mesh.coordinates(index))
            index += 1
          val faces = new Array[Int](from.mesh.faceIndices.length)
          index = 0
          while index < faces.length do
            faces(index) = from.mesh.faceIndices(index)
            index += 1
          Right(SurfaceGeometry(
            TriangleMesh.fromArrays(coordinates, faces),
            from.hemisphere,
            SurfaceKind.Custom(s"morph:${from.kind.label}->${to.kind.label}"),
            from.surfaceToWorld
          ))

  def between(
    surfaces: SurfaceSet,
    from: SurfaceKind,
    to: SurfaceKind,
    fraction: SurfaceMorphFraction,
    normalPolicy: SurfaceNormalPolicy = SurfaceNormalPolicy.RecomputeFromGeometry
  ): Either[SurfaceViewError, SurfaceGeometry] =
    (surfaces.get(from), surfaces.get(to)) match
      case (Some(start), Some(end)) => interpolate(start, end, fraction, normalPolicy)
      case _ => Left(SurfaceViewError.IncompatibleMorph("both requested variants must exist in the SurfaceSet"))

  def reveal(
    surfaces: SurfaceSet,
    from: SurfaceKind,
    to: SurfaceKind,
    deformation: SurfaceLensDeformation,
    fraction: SurfaceMorphFraction
  ): Either[SurfaceViewError, SurfaceGeometry] =
    (surfaces.get(from), surfaces.get(to)) match
      case (Some(start), Some(end)) => reveal(start, end, deformation, fraction)
      case _ => Left(SurfaceViewError.IncompatibleMorph("both requested lens variants must exist in the SurfaceSet"))

  def reveal(
    surfaces: SurfaceSet,
    from: SurfaceKind,
    to: SurfaceKind,
    lens: SurfaceGeodesicLens,
    fraction: SurfaceMorphFraction
  ): Either[SurfaceViewError, SurfaceGeometry] =
    (surfaces.get(from), surfaces.get(to)) match
      case (Some(start), Some(end)) => reveal(start, end, lens, fraction)
      case _ => Left(SurfaceViewError.IncompatibleMorph("both requested lens variants must exist in the SurfaceSet"))

  def reveal(
    from: SurfaceGeometry,
    to: SurfaceGeometry,
    deformation: SurfaceLensDeformation,
    fraction: SurfaceMorphFraction
  ): Either[SurfaceViewError, SurfaceGeometry] =
    if !from.hasSameMeshDomain(to) then
      Left(SurfaceViewError.IncompatibleMorph("lens endpoints must have the same exact ordered topology"))
    else if from.surfaceToWorld != to.surfaceToWorld then
      Left(SurfaceViewError.IncompatibleMorph("lens endpoint surface-to-world transforms must match"))
    else if !deformation.isCompatibleWith(from) then
      Left(SurfaceViewError.IncompatibleMorph("lens deformation does not belong to the source mesh domain"))
    else if fraction.value == 0.0 then Right(from)
    else
      val coordinates = new Array[Double](from.vertexCount * 3)
      var index = 0
      while index < coordinates.length do
        coordinates(index) = from.mesh.coordinates(index) + fraction.value * deformation.displacementAtUnsafe(index)
        index += 1
      val faces = new Array[Int](from.mesh.faceIndices.length)
      index = 0
      while index < faces.length do
        faces(index) = from.mesh.faceIndices(index)
        index += 1
      Right(SurfaceGeometry(
        TriangleMesh.fromArrays(coordinates, faces),
        from.hemisphere,
        SurfaceKind.Custom(s"lens:${from.kind.label}->${to.kind.label}"),
        from.surfaceToWorld
      ))

  def reveal(
    from: SurfaceGeometry,
    to: SurfaceGeometry,
    lens: SurfaceGeodesicLens,
    fraction: SurfaceMorphFraction
  ): Either[SurfaceViewError, SurfaceGeometry] =
    if !from.hasSameMeshDomain(to) then
      Left(SurfaceViewError.IncompatibleMorph("lens endpoints must have the same exact ordered topology"))
    else if from.surfaceToWorld != to.surfaceToWorld then
      Left(SurfaceViewError.IncompatibleMorph("lens endpoint surface-to-world transforms must match"))
    else if !lens.isCompatibleWith(from) then
      Left(SurfaceViewError.IncompatibleMorph("lens weights do not belong to the source mesh domain"))
    else if fraction.value == 0.0 then Right(from)
    else
      val coordinates = new Array[Double](from.vertexCount * 3)
      var vertex = 0
      while vertex < from.vertexCount do
        val offset = vertex * 3
        val amount = fraction.value * lens.weightAtUnsafe(vertex)
        coordinates(offset) = from.mesh.coordinates(offset) +
          amount * (to.mesh.coordinates(offset) - from.mesh.coordinates(offset))
        coordinates(offset + 1) = from.mesh.coordinates(offset + 1) +
          amount * (to.mesh.coordinates(offset + 1) - from.mesh.coordinates(offset + 1))
        coordinates(offset + 2) = from.mesh.coordinates(offset + 2) +
          amount * (to.mesh.coordinates(offset + 2) - from.mesh.coordinates(offset + 2))
        vertex += 1
      val faces = new Array[Int](from.mesh.faceIndices.length)
      var index = 0
      while index < faces.length do
        faces(index) = from.mesh.faceIndices(index)
        index += 1
      Right(SurfaceGeometry(
        TriangleMesh.fromArrays(coordinates, faces),
        from.hemisphere,
        SurfaceKind.Custom(s"lens:${from.kind.label}->${to.kind.label}"),
        from.surfaceToWorld
      ))

opaque type SurfaceLinkRadius = Double

object SurfaceLinkRadius:
  def make(value: Double): Either[SurfaceViewError, SurfaceLinkRadius] =
    if value.isFinite && value >= 0.0 then Right(value)
    else Left(SurfaceViewError.InvalidLinkRadius(value))

  def unsafe(value: Double): SurfaceLinkRadius =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (radius: SurfaceLinkRadius)
    def value: Double = radius

final case class SurfaceLinkedSelection(selection: SurfaceSelection, world: WorldPoint, voxel: VoxelPoint)

object SurfaceWorldLink:
  def worldPoint(geometry: SurfaceGeometry, vertex: VertexId): Either[SurfaceViewError, WorldPoint] =
    if vertex.index < 0 || vertex.index >= geometry.vertexCount then
      Left(SurfaceViewError.InvalidVertexIndex(vertex.index, geometry.vertexCount))
    else
      val point = geometry.mesh.vertex(vertex)
      geometry.surfaceToWorld(Vector(point.x, point.y, point.z))
        .left.map(SurfaceViewError.GeometryFailure.apply)
        .map(coordinates => WorldPoint(coordinates(0), coordinates(1), coordinates(2)))

  def toVolume(
    selection: SurfaceSelection,
    geometry: SurfaceGeometry,
    volume: image4s.geometry.Grid[? <: image4s.geometry.Frame[image4s.geometry.D3], image4s.geometry.D3]
  ): Either[SurfaceViewError, SurfaceLinkedSelection] =
    worldPoint(geometry, selection.vertex).flatMap: world =>
      volume.worldToVoxel(world)
        .left.map(SurfaceViewError.GeometryFailure.apply)
        .map(voxel => SurfaceLinkedSelection(selection, world, voxel))

  def nearestVertex(
    surface: SurfaceId,
    geometry: SurfaceGeometry,
    world: WorldPoint,
    maximumDistance: SurfaceLinkRadius
  ): Either[SurfaceViewError, SurfaceSelection] =
    var bestVertex = 0
    var bestSquared = Double.PositiveInfinity
    var vertex = 0
    while vertex < geometry.vertexCount do
      val candidate = worldPoint(geometry, VertexId.unsafe(vertex)) match
        case Left(error) => return Left(error)
        case Right(value) => value
      val dx = candidate.x - world.x
      val dy = candidate.y - world.y
      val dz = candidate.z - world.z
      val squared = dx * dx + dy * dy + dz * dz
      if squared < bestSquared then
        bestSquared = squared
        bestVertex = vertex
      vertex += 1
    val distance = math.sqrt(bestSquared)
    if distance <= maximumDistance.value then Right(SurfaceSelection(surface, VertexId.unsafe(bestVertex)))
    else Left(SurfaceViewError.LinkDistanceExceeded(distance, maximumDistance.value))

opaque type SurfaceAnnotationId = String

object SurfaceAnnotationId:
  def make(value: String): Either[SurfaceViewError, SurfaceAnnotationId] =
    val normalized = value.trim
    if normalized.isEmpty then Left(SurfaceViewError.InvalidAnnotation("id must be non-empty"))
    else Right(normalized)

  def unsafe(value: String): SurfaceAnnotationId =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: SurfaceAnnotationId)
    def value: String = id

final case class SurfaceAnnotation private (
  id: SurfaceAnnotationId,
  selection: SurfaceSelection,
  label: String,
  color: Rgba32
)

object SurfaceAnnotation:
  def make(
    id: SurfaceAnnotationId,
    selection: SurfaceSelection,
    label: String,
    color: Rgba32
  ): Either[SurfaceViewError, SurfaceAnnotation] =
    val normalized = label.trim
    if normalized.isEmpty then Left(SurfaceViewError.InvalidAnnotation("label must be non-empty"))
    else Right(new SurfaceAnnotation(id, selection, normalized, color))

object SurfaceAnnotations:
  /** Materialize point annotations as an ordinary packed-RGBA overlay. The
    * ordinary layer contract keeps rendering, opacity, ordering, and backend
    * resource receipts identical to every other overlay.
    */
  def layer(
    id: SurfaceLayerId,
    surface: SurfaceId,
    geometry: SurfaceGeometry,
    annotations: Vector[SurfaceAnnotation],
    opacity: DisplayOpacity = DisplayOpacity.Opaque,
    blendMode: DisplayBlendMode = DisplayBlendMode.Normal
  ): Either[SurfaceViewError, SurfaceLayer] =
    val transparent = Rgba32.unsafe(0, 0, 0, 0)
    val colors = Array.fill(geometry.vertexCount)(transparent)
    var index = 0
    while index < annotations.length do
      val annotation = annotations(index)
      if annotation.selection.surface != surface then
        return Left(SurfaceViewError.InvalidAnnotation(
          s"annotation '${annotation.id.value}' belongs to surface '${annotation.selection.surface.value}'"
        ))
      val vertex = annotation.selection.vertex.index
      if vertex < 0 || vertex >= geometry.vertexCount then
        return Left(SurfaceViewError.InvalidVertexIndex(vertex, geometry.vertexCount))
      colors(vertex) = annotation.color
      index += 1
    SurfaceLayer.packedRgba(id, surface, geometry, colors.toIndexedSeq, opacity = opacity, blendMode = blendMode)

final case class SurfaceSelectionRecord(
  sequence: Long,
  selection: SurfaceSelection,
  world: WorldPoint,
  annotation: Option[SurfaceAnnotationId]
)

final case class SurfaceSelectionHistory private (capacity: Int, records: Vector[SurfaceSelectionRecord]):
  def append(
    selection: SurfaceSelection,
    world: WorldPoint,
    annotation: Option[SurfaceAnnotationId] = None
  ): SurfaceSelectionHistory =
    val sequence = records.lastOption.map(_.sequence + 1L).getOrElse(0L)
    val appended = records :+ SurfaceSelectionRecord(sequence, selection, world, annotation)
    copy(records = if appended.length <= capacity then appended else appended.takeRight(capacity))

object SurfaceSelectionHistory:
  def make(capacity: Int): Either[SurfaceViewError, SurfaceSelectionHistory] =
    if capacity <= 0 then Left(SurfaceViewError.InvalidSelectionHistoryCapacity(capacity))
    else Right(new SurfaceSelectionHistory(capacity, Vector.empty))
