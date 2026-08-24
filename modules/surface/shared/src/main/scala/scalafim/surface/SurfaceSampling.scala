package scalafim.surface

import scalafim.image.SampleSpaces.*

import scalafim.image.*
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

final case class VolumeSurfaceSampler(plan: VolumeSurfaceSamplingPlan):

  def sample(volume: SomeScalarVolume[Double], mask: Option[SomeMaskVolume] = None): SurfaceSampleResult =
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
    surface.surfaceToWorld(Vector(point.x, point.y, point.z)).fold(
      error => throw new IllegalStateException(error.message),
      identity
    )

  private def sampleValues(
    volume: SomeScalarVolume[Double],
    mask: Option[SomeMaskVolume],
    points: Vector[Vector[Double]]
  ): Vector[Double] =
    val out = Vector.newBuilder[Double]
    points.foreach { point =>
      nearestGrid(volume, point).foreach { grid =>
        val lin = volume.gridToIndex(grid(0), grid(1), grid(2))
        if mask.forall(_.valueAtCanonicalOrdinal(lin)) then out += volume.valueAtCanonicalOrdinal(lin)
      }
    }
    out.result()

  private def nearestGrid(volume: SomeScalarVolume[Double], point: Vector[Double]): Option[Vector[Int]] =
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

  private def validateMask(volume: SomeScalarVolume[Double], mask: SomeMaskVolume): Unit =
    require(
      mask.grid.sameRuntimeOwnerAs(volume.grid),
      "mask/volume space mismatch"
    )

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
    volume: SomeScalarVolume[Double],
    surfaces: SurfaceGeometryPair,
    path: SurfaceSamplingPath = SurfaceSamplingPath.Midpoint,
    aggregation: SurfaceSampleAggregation = SurfaceSampleAggregation.Nearest,
    mask: Option[SomeMaskVolume] = None
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
