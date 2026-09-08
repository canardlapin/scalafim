package scalafim.surface

import scalafim.image.Affine
import scalafim.image.NeuroVol
import scalafim.image.PrimitiveBuffers
import scalafim.image.{VoxelCoord, WorldPoint}
import scala.util.control.NonFatal

final case class SurfaceGeometryPair(white: SurfaceGeometry, pial: SurfaceGeometry):
  require(white.vertexCount == pial.vertexCount, "white and pial surfaces must have the same vertex count")
  require(white.hemisphere == pial.hemisphere, "white and pial surfaces must have the same hemisphere")
  require(white.mesh.hasSameTopology(pial.mesh), "white and pial surfaces must share ordered triangle topology")

  def domainEither: Either[SurfaceError, SurfaceDomain] =
    for
      whiteDomain <- white.domainEither
      pialDomain <- pial.domainEither
      _ <- if whiteDomain == pialDomain then scala.util.Right(()) else scala.util.Left(SurfaceError.DomainMismatch(whiteDomain, pialDomain))
    yield whiteDomain

object SurfaceGeometryPair:
  def fromEither(white: SurfaceGeometry, pial: SurfaceGeometry): Either[SurfaceError, SurfaceGeometryPair] =
    try scala.util.Right(SurfaceGeometryPair(white, pial))
    catch case NonFatal(error) => scala.util.Left(SurfaceError.InvalidGeometry(SurfaceError.reason(error)))

enum SurfaceSamplingPath:
  case White
  case Pial
  case Midpoint
  case FractionalThickness(fractions: Vector[Double])
  case NormalLine(offsets: Vector[Double])

enum SurfaceSampleAggregation:
  case Nearest, Average, Mode

final case class VolumeSurfaceSamplingPlan(
  surfaces: SurfaceGeometryPair,
  path: SurfaceSamplingPath = SurfaceSamplingPath.Midpoint,
  aggregation: SurfaceSampleAggregation = SurfaceSampleAggregation.Nearest
):
  SurfaceSamplingPath.validate(path)

final case class SurfaceSampleResult(
  values: SurfaceField[Double],
  sampleCounts: SurfaceField[Int]
)

/** Outcome of a requested sample. Included values may be nonfinite, matching
  * ordinary sampling semantics; inclusion describes geometry and mask admission.
  */
enum SurfaceSampleOutcome:
  case OutsideVolume
  case Masked(voxel: VoxelCoord)
  case Included(voxel: VoxelCoord, value: Double)

final case class SurfacePointSample(world: WorldPoint, outcome: SurfaceSampleOutcome)

/** Ordered requests and aggregation inputs for one vertex. Duplicate voxel hits
  * remain distinct requests. Nearest uses the first included request; Average
  * and Mode use every included request, including duplicates.
  */
final case class SurfaceVertexSample private[surface] (
    vertex: VertexId,
    path: SurfaceSamplingPath,
    aggregation: SurfaceSampleAggregation,
    samples: Vector[SurfacePointSample],
    value: Double
):
  def acceptedSampleIndices: Vector[Int] = samples.indices.filter { index =>
    samples(index).outcome match
      case SurfaceSampleOutcome.Included(_, _) => true
      case _ => false
  }.toVector

  def contributingSampleIndices: Vector[Int] = aggregation match
    case SurfaceSampleAggregation.Nearest => acceptedSampleIndices.take(1)
    case _ => acceptedSampleIndices

  def acceptedCount: Int = acceptedSampleIndices.size

  def hasNonFiniteSamples: Boolean = samples.exists { sample => sample.outcome match
    case SurfaceSampleOutcome.Included(_, value) => !value.isFinite
    case _ => false
  }

final case class VolumeSurfaceSampler(plan: VolumeSurfaceSamplingPlan):

  def sample(volume: NeuroVol[Double], mask: Option[NeuroVol[Boolean]] = None): SurfaceSampleResult =
    mask.foreach(validateMask(volume, _))

    val vertexCount = plan.surfaces.white.vertexCount
    val values = Array.fill(vertexCount)(Double.NaN)
    val counts = Array.ofDim[Int](vertexCount)

    var i = 0
    while i < vertexCount do
      val vertex = VertexId.unsafe(i)
      val samples = sampleValues(volume, mask, samplePoints(vertex))
      counts(i) = samples.length
      if samples.nonEmpty then values(i) = aggregate(samples)
      i += 1

    SurfaceSampleResult(
      values = SurfaceField.full(plan.surfaces.white, values.toVector, "surface-sample"),
      sampleCounts = SurfaceField.full(plan.surfaces.white, counts.toVector, "surface-sample-count")
    )

  /** Inspect just one vertex without allocating receipts for the whole mesh. */
  def inspectVertex(volume: NeuroVol[Double], vertex: VertexId,
      mask: Option[NeuroVol[Boolean]] = None): SurfaceVertexSample =
    require(vertex.index < plan.surfaces.white.vertexCount, "vertex id out of range")
    mask.foreach(validateMask(volume, _))
    val points = Vector.newBuilder[SurfacePointSample]
    val values = sampleValues(volume, mask, samplePoints(vertex), Some(point => { points += point; () }))
    SurfaceVertexSample(vertex, plan.path, plan.aggregation, points.result(),
      if values.isEmpty then Double.NaN else aggregate(values))

  def inspectVertexEither(volume: NeuroVol[Double], vertex: VertexId,
      mask: Option[NeuroVol[Boolean]] = None): Either[SurfaceError, SurfaceVertexSample] =
    try scala.util.Right(inspectVertex(volume, vertex, mask))
    catch case NonFatal(error) => scala.util.Left(SurfaceError.InvalidGeometry(SurfaceError.reason(error)))

  private def samplePoints(vertex: VertexId): Vector[Vector[Double]] =
    val white = worldPoint(plan.surfaces.white, vertex)
    val pial = worldPoint(plan.surfaces.pial, vertex)
    val delta = subtract(pial, white)

    plan.path match
      case SurfaceSamplingPath.White =>
        Vector(white)
      case SurfaceSamplingPath.Pial =>
        Vector(pial)
      case SurfaceSamplingPath.Midpoint =>
        Vector(add(white, scale(delta, 0.5)))
      case SurfaceSamplingPath.FractionalThickness(fractions) =>
        fractions.map(f => add(white, scale(delta, f)))
      case SurfaceSamplingPath.NormalLine(offsets) =>
        val midpoint = add(white, scale(delta, 0.5))
        val n = norm(delta)
        val unit = if n == 0.0 then Vector(0.0, 0.0, 0.0) else scale(delta, 1.0 / n)
        offsets.map(offset => add(midpoint, scale(unit, offset)))

  private def worldPoint(surface: SurfaceGeometry, vertex: VertexId): Vector[Double] =
    val point = surface.mesh.vertex(vertex)
    Affine.applyAffine(surface.surfaceToWorld, Vector(point.x, point.y, point.z))

  private def sampleValues(
    volume: NeuroVol[Double],
    mask: Option[NeuroVol[Boolean]],
    points: Vector[Vector[Double]],
    observe: Option[SurfacePointSample => Unit] = None
  ): Vector[Double] =
    val out = Vector.newBuilder[Double]
    points.foreach { point =>
      nearestGrid(volume, point) match
        case None => observe.foreach(callback => callback(SurfacePointSample(
          WorldPoint(point(0), point(1), point(2)), SurfaceSampleOutcome.OutsideVolume)))
        case Some(grid) =>
          val lin = volume.gridToIndex(grid(0), grid(1), grid(2))
          if mask.forall(_.linear(lin)) then
            val value = volume.linear(lin)
            out += value
            observe.foreach(callback => callback(SurfacePointSample(WorldPoint(point(0), point(1), point(2)),
              SurfaceSampleOutcome.Included(VoxelCoord(grid(0), grid(1), grid(2)), value))))
          else observe.foreach(callback => callback(SurfacePointSample(WorldPoint(point(0), point(1), point(2)),
            SurfaceSampleOutcome.Masked(VoxelCoord(grid(0), grid(1), grid(2))))))
    }
    out.result()

  private def nearestGrid(volume: NeuroVol[Double], point: Vector[Double]): Option[Vector[Int]] =
    val index = volume.space.coordToIndex(point)
    val grid = index.map(v => math.round(v).toInt)
    val dims = volume.space.spatialDims
    if grid.indices.forall(i => grid(i) >= 0 && grid(i) < dims(i)) then Some(grid) else None

  private def aggregate(values: Vector[Double]): Double =
    plan.aggregation match
      case SurfaceSampleAggregation.Nearest =>
        values.head
      case SurfaceSampleAggregation.Average =>
        values.sum / values.length.toDouble
      case SurfaceSampleAggregation.Mode =>
        val counts = scala.collection.mutable.Map.empty[Double, Int]
        values.foreach(value => counts.update(value, counts.getOrElse(value, 0) + 1))
        counts.toVector.minBy { case (value, count) => (-count, value) }._1

  private def validateMask(volume: NeuroVol[Double], mask: NeuroVol[Boolean]): Unit =
    require(mask.space.spatialDims == volume.space.spatialDims, "mask/volume space mismatch")
    require(mask.space.trans == volume.space.trans, "mask/volume space mismatch")

  private def add(a: Vector[Double], b: Vector[Double]): Vector[Double] =
    Vector.tabulate(3)(i => a(i) + b(i))

  private def subtract(a: Vector[Double], b: Vector[Double]): Vector[Double] =
    Vector.tabulate(3)(i => a(i) - b(i))

  private def scale(a: Vector[Double], value: Double): Vector[Double] =
    Vector.tabulate(3)(i => a(i) * value)

  private def norm(a: Vector[Double]): Double =
    math.sqrt(a.map(x => x * x).sum)

object VolumeSurfaceSampler:

  def sample(
    volume: NeuroVol[Double],
    surfaces: SurfaceGeometryPair,
    path: SurfaceSamplingPath = SurfaceSamplingPath.Midpoint,
    aggregation: SurfaceSampleAggregation = SurfaceSampleAggregation.Nearest,
    mask: Option[NeuroVol[Boolean]] = None
  ): SurfaceSampleResult =
    VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(surfaces, path, aggregation)).sample(volume, mask)

object SurfaceSamplingPath:

  private[surface] def validate(path: SurfaceSamplingPath): Unit =
    path match
      case SurfaceSamplingPath.FractionalThickness(fractions) =>
        require(fractions.nonEmpty, "fractional thickness path must contain at least one fraction")
        require(fractions.forall(f => f.isFinite && f >= 0.0 && f <= 1.0), "fractional thickness values must be in [0, 1]")
      case SurfaceSamplingPath.NormalLine(offsets) =>
        require(offsets.nonEmpty, "normal-line path must contain at least one offset")
        require(offsets.forall(_.isFinite), "normal-line offsets must be finite")
      case _ =>
        ()
