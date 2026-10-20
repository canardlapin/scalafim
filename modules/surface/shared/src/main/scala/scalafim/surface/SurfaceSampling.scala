package scalafim.surface

import neuroim4s.image.SampleSpaces.*

import image4s.{BoundaryPolicy, Continuous, ImageError, SampleSpace, Sampled}
import image4s.geometry.{D3, Frame, GeometryError, Point}
import reframe4s.resample.{Interpolation, SampledInterpolator}
import ravel.Rank
import neuroim4s.image.*
import scala.util.control.NonFatal

final case class SurfaceGeometryPair private (white: SurfaceGeometry, pial: SurfaceGeometry):
  require(white.vertexCount == pial.vertexCount, "white and pial surfaces must have the same vertex count")
  require(white.hemisphere == pial.hemisphere, "white and pial surfaces must have the same hemisphere")
  require(white.mesh.hasSameTopology(pial.mesh), "white and pial surfaces must share ordered triangle topology")

  require(white.mesh.topology eq pial.mesh.topology, "white and pial must share one topology owner")

  def copy(white: SurfaceGeometry = this.white, pial: SurfaceGeometry = this.pial): SurfaceGeometryPair =
    SurfaceGeometryPair(white, pial)

  def domainEither: Either[SurfaceError, SurfaceDomain] =
    for
      whiteDomain <- white.domainEither
      pialDomain <- pial.domainEither
      _ <- if whiteDomain == pialDomain then scala.util.Right(()) else scala.util.Left(SurfaceError.DomainMismatch(whiteDomain, pialDomain))
    yield whiteDomain

object SurfaceGeometryPair:
  def apply(white: SurfaceGeometry, pial: SurfaceGeometry): SurfaceGeometryPair =
    require(white.vertexCount == pial.vertexCount, "white and pial surfaces must have the same vertex count")
    require(white.hemisphere == pial.hemisphere, "white and pial surfaces must have the same hemisphere")
    require(white.mesh.hasSameTopology(pial.mesh), "white and pial surfaces must share ordered triangle topology")
    val mesh = pial.mesh.sharingTopology(white.mesh)
      .fold(error => throw new IllegalArgumentException(error.message), value => value)
    new SurfaceGeometryPair(white, SurfaceGeometry(mesh, pial.hemisphere, pial.kind, pial.surfaceToWorld))

  def fromEither(white: SurfaceGeometry, pial: SurfaceGeometry): Either[SurfaceError, SurfaceGeometryPair] =
    try scala.util.Right(SurfaceGeometryPair(white, pial))
    catch case NonFatal(error) => scala.util.Left(SurfaceError.InvalidGeometry(SurfaceError.reason(error)))

enum SurfaceSamplingPath:
  case White
  case Pial
  case Midpoint
  case FractionalThickness(fractions: Vector[Double])
  case NormalLine(offsets: Vector[Double])

/** How a vertex's samples reduce to one value. Every reducer skips non-finite
  * samples: a non-finite value is not an observation. A vertex with no finite
  * sample reduces to NaN.
  *
  *  - `Nearest`: the first finite sample in the path's declared order (the
  *    order of `fractions` or `offsets`, as given). Each sample has already
  *    used the plan's voxel interpolation (nearest by default). Samples are
  *    totally ordered, so no further tie rule is needed.
  *  - `Average`: the arithmetic mean of the finite samples.
  *  - `Mode`: the most frequent finite value. Ties go to the smallest value.
  */
enum SurfaceSampleAggregation:
  case Nearest, Average, Mode

/** The voxel interpolation kernel used for each requested surface point. */
enum SurfaceVoxelInterpolation:
  /** One voxel, selected by absolute half-up rounding. */
  case Nearest
  /** Provider-owned trilinear interpolation over the positive-weight stencil. */
  case Linear

final case class VolumeSurfaceSamplingPlan(
  surfaces: SurfaceGeometryPair,
  path: SurfaceSamplingPath = SurfaceSamplingPath.Midpoint,
  aggregation: SurfaceSampleAggregation = SurfaceSampleAggregation.Nearest,
  interpolation: SurfaceVoxelInterpolation = SurfaceVoxelInterpolation.Nearest
):
  SurfaceSamplingPath.validate(path)

/** What one sampling pass observed, over all vertices. Every requested sample
  * point lands in exactly one category: outside the volume grid, excluded by
  * the mask, a non-finite volume value, or an accepted finite value.
  */
final case class SurfaceSampleTally(
  requested: Long,
  outsideVolume: Long,
  masked: Long,
  nonFinite: Long,
  accepted: Long
):
  require(
    requested >= 0L && outsideVolume >= 0L && masked >= 0L && nonFinite >= 0L && accepted >= 0L,
    "sample tallies must be non-negative"
  )
  require(
    outsideVolume + masked + nonFinite + accepted == requested,
    "every requested sample must be outside the volume, masked, non-finite, or accepted"
  )

  def rejected: Long =
    outsideVolume + masked + nonFinite

/** Per-vertex accounting splits the in-volume, in-mask samples in two.
  * `sampleCounts` counts finite values only: a non-finite value is not an
  * observation, so it never counts toward a minimum-sample rule.
  * `nonFiniteCounts` counts the non-finite values separately so that they stay
  * visible. `values` reduces the finite samples only (see
  * [[SurfaceSampleAggregation]]), so it is NaN exactly when a vertex has no
  * finite sample.
  * Summed over the sampled vertices, the two fields equal `tally.accepted` and
  * `tally.nonFinite`.
  */
final case class SurfaceSampleResult(
  values: SurfaceField[Double],
  sampleCounts: SurfaceField[Int],
  nonFiniteCounts: SurfaceField[Int],
  tally: SurfaceSampleTally
):
  require(
    sampleCounts.size == values.size && nonFiniteCounts.size == values.size,
    "per-vertex sample counts must cover every vertex"
  )

enum SurfaceSampleOutcome:
  case OutsideVolume
  case Masked(voxel: VoxelCoord)
  case NonFinite(voxel: VoxelCoord, value: Double)
  case Included(voxel: VoxelCoord, value: Double)
  case Interpolated(contributors: Vector[SurfaceVoxelContributor], value: Double)

/** One positive-weight source voxel used by a linear sample. */
final case class SurfaceVoxelContributor(voxel: VoxelCoord, weight: Double):
  require(weight.isFinite && weight > 0.0 && weight <= 1.0, "voxel contribution must have finite positive weight at most one")

final case class SurfacePointSample(world: WorldPoint, outcome: SurfaceSampleOutcome)

/** Ordered lookup receipts; duplicate voxel requests remain distinct. */
final case class SurfaceVertexSample private[surface] (
  vertex: VertexId,
  path: SurfaceSamplingPath,
  aggregation: SurfaceSampleAggregation,
  samples: Vector[SurfacePointSample],
  value: Double
):
  /** In-mask samples with finite values, in declared path order. These are the
    * samples that every reducer considers.
    */
  def acceptedSampleIndices: Vector[Int] = samples.indices.filter: index =>
    samples(index).outcome match
      case SurfaceSampleOutcome.Included(_, value) => value.isFinite
      case SurfaceSampleOutcome.Interpolated(_, value) => value.isFinite
      case _ => false
  .toVector

  def contributingSampleIndices: Vector[Int] = aggregation match
    case SurfaceSampleAggregation.Nearest => acceptedSampleIndices.take(1)
    case _ => acceptedSampleIndices

  def acceptedCount: Int = acceptedSampleIndices.size

  def hasNonFiniteSamples: Boolean = samples.exists: sample =>
    sample.outcome match
      case SurfaceSampleOutcome.NonFinite(_, _) => true
      case SurfaceSampleOutcome.Included(_, value) => !value.isFinite
      case SurfaceSampleOutcome.Interpolated(_, value) => !value.isFinite
      case _ => false

final case class VolumeSurfaceSampler(plan: VolumeSurfaceSamplingPlan):

  def sample(volume: SomeScalarVolume[Double], mask: Option[SomeMaskVolume] = None): SurfaceSampleResult =
    sampleSelected(volume, mask, None)

  private[surface] def sampleSelected(volume: SomeScalarVolume[Double], mask: Option[SomeMaskVolume],
      selected: Option[Array[Boolean]]): SurfaceSampleResult =
    mask.foreach(validateMask(volume, _))

    val vertexCount = plan.surfaces.white.vertexCount
    val values = Array.fill(vertexCount)(Double.NaN)
    val counts = Array.ofDim[Int](vertexCount)
    val nonFiniteCounts = Array.ofDim[Int](vertexCount)
    val tally = TallyBuilder()
    selected.foreach(flags => require(flags.length == vertexCount, "selected vertex count differs from anatomy"))

    var i = 0
    while i < vertexCount do
      if selected.forall(_(i)) then
        val vertex = VertexId.unsafe(i)
        val samples = sampleValues(volume, mask, samplePoints(vertex), tally)
        var finite = 0
        samples.foreach(value => if value.isFinite then finite += 1)
        counts(i) = finite
        nonFiniteCounts(i) = samples.length - finite
        values(i) = aggregate(samples)
      i += 1

    SurfaceSampleResult(
      values = SurfaceField.full(plan.surfaces.white, values.toVector, "surface-sample"),
      sampleCounts = SurfaceField.full(plan.surfaces.white, counts.toVector, "surface-sample-count"),
      nonFiniteCounts = SurfaceField.full(plan.surfaces.white, nonFiniteCounts.toVector, "surface-non-finite-sample-count"),
      tally = tally.result()
    )

  def inspectVertex(volume: SomeScalarVolume[Double], vertex: VertexId,
      mask: Option[SomeMaskVolume] = None): SurfaceVertexSample =
    require(vertex.index < plan.surfaces.white.vertexCount, "vertex id out of range")
    mask.foreach(validateMask(volume, _))
    val receipts = Vector.newBuilder[SurfacePointSample]
    val values = sampleValues(volume, mask, samplePoints(vertex), TallyBuilder(), Some(receipt => { receipts += receipt; () }))
    SurfaceVertexSample(vertex, plan.path, plan.aggregation, receipts.result(),
      aggregate(values))

  def inspectVertexEither(volume: SomeScalarVolume[Double], vertex: VertexId,
      mask: Option[SomeMaskVolume] = None): Either[SurfaceError, SurfaceVertexSample] =
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
    surface.surfaceToWorld(Vector(point.x, point.y, point.z)) match
      case Right(world) => world
      // A finite vertex whose world coordinate overflows has no finite location;
      // the point classifiers count it as outside the volume.
      case Left(GeometryError.NonFiniteCoordinate(_, _)) => Vector.fill(3)(Double.NaN)
      case Left(error) => throw new IllegalStateException(error.message)

  private def sampleValues(
    volume: SomeScalarVolume[Double],
    mask: Option[SomeMaskVolume],
    points: Vector[Vector[Double]],
    tally: TallyBuilder,
    observe: Option[SurfacePointSample => Unit] = None
  ): Vector[Double] =
    val out = Vector.newBuilder[Double]
    points.foreach { point =>
      tally.requested += 1L
      lazy val world = WorldPoint(point(0), point(1), point(2))
      plan.interpolation match
        case SurfaceVoxelInterpolation.Nearest =>
          nearestGrid(volume, point) match
            case None =>
              tally.outsideVolume += 1L
              observe.foreach(_(SurfacePointSample(world, SurfaceSampleOutcome.OutsideVolume)))
            case Some(grid) =>
              val lin = volume.gridToIndex(grid(0), grid(1), grid(2))
              val voxel = volume.indexToVoxel(lin)
              if !mask.forall(_.valueAtCanonicalOrdinal(lin)) then
                tally.masked += 1L
                observe.foreach(_(SurfacePointSample(world, SurfaceSampleOutcome.Masked(voxel))))
              else
                val value = volume.valueAtCanonicalOrdinal(lin)
                if value.isFinite then
                  tally.accepted += 1L
                  out += value
                  observe.foreach(_(SurfacePointSample(world, SurfaceSampleOutcome.Included(voxel, value))))
                else
                  tally.nonFinite += 1L
                  out += value
                  // Preserve the established nearest-voxel inspection shape.
                  observe.foreach(_(SurfacePointSample(world, SurfaceSampleOutcome.Included(voxel, value))))
        case SurfaceVoxelInterpolation.Linear =>
          linearStencil(volume, point) match
            case None =>
              tally.outsideVolume += 1L
              observe.foreach(_(SurfacePointSample(world, SurfaceSampleOutcome.OutsideVolume)))
            case Some(stencil) =>
              stencil.collectFirst { case (lin, _, _) if !mask.forall(_.valueAtCanonicalOrdinal(lin)) => lin } match
                case Some(lin) =>
                  tally.masked += 1L
                  observe.foreach(_(SurfacePointSample(world, SurfaceSampleOutcome.Masked(volume.indexToVoxel(lin)))))
                case None =>
                  stencil.collectFirst { case (lin, _, value) if !value.isFinite => lin -> value } match
                    case Some((lin, value)) =>
                      tally.nonFinite += 1L
                      out += value
                      observe.foreach(_(SurfacePointSample(world, SurfaceSampleOutcome.NonFinite(volume.indexToVoxel(lin), value))))
                    case None =>
                      providerLinear(volume, point) match
                        case Left(error) =>
                          throw new IllegalStateException(s"linear provider rejected an admitted source point: $error")
                        case Right(value) =>
                          // The provider owns interpolation arithmetic; the local stencil is only a typed receipt.
                          if value.isFinite then tally.accepted += 1L else tally.nonFinite += 1L
                          out += value
                          val contributors = stencil.map((lin, weight, _) => SurfaceVoxelContributor(volume.indexToVoxel(lin), weight))
                          observe.foreach(_(SurfacePointSample(world, SurfaceSampleOutcome.Interpolated(contributors, value))))
    }
    out.result()

  private final class TallyBuilder:
    var requested = 0L
    var outsideVolume = 0L
    var masked = 0L
    var nonFinite = 0L
    var accepted = 0L

    def result(): SurfaceSampleTally =
      SurfaceSampleTally(requested, outsideVolume, masked, nonFinite, accepted)

  private def nearestGrid(volume: SomeScalarVolume[Double], point: Vector[Double]): Option[Vector[Int]] =
    // Preserve the established spatial-terms-first sum at nearest-voxel ties.
    val matrix = volume.grid.indexToFrame.inverse.matrix
    val index = Vector.tabulate(3): row =>
      (((0.0 + matrix(row, 0) * point(0)) + matrix(row, 1) * point(1)) + matrix(row, 2) * point(2)) + matrix(row, 3) * 1.0
    val rounded = index.map(v => math.round(v))
    val dims = volume.space.grid.shape
    // A far-outside Long can wrap into the grid when narrowed to Int.
    if rounded.indices.forall(i => index(i).isFinite && rounded(i) >= 0L && rounded(i) < dims(i).toLong) then
      Some(rounded.map(_.toInt))
    else None

  /** Positive-weight trilinear stencil after enforcing the closed voxel-centre hull.
    * The provider performs the value interpolation; this function supplies only
    * domain validation, mask/non-finite admission, and receipt provenance.
    */
  private def linearStencil(volume: SomeScalarVolume[Double], point: Vector[Double]): Option[Vector[(Int, Double, Double)]] =
    // Use the provider's affine evaluation order for admission and receipts,
    // too: roundoff at a sheared boundary can change the contributing stencil.
    val index = providerContinuousIndex(volume, point) match
      case Left(_) => return None
      case Right(value) => value
    val dims = volume.space.grid.shape
    if !index.indices.forall(axis => index(axis).isFinite && index(axis) >= 0.0 && index(axis) <= dims(axis).toDouble - 1.0) then None
    else
      val lower = index.map(math.floor(_).toInt)
      val fraction = index.indices.map(axis => index(axis) - lower(axis)).toVector
      val stencil = Vector.newBuilder[(Int, Double, Double)]
      var corner = 0
      while corner < 8 do
        var weight = 1.0
        var axis = 0
        while axis < 3 do
          weight *= (if ((corner >> axis & 1) == 1) fraction(axis) else 1.0 - fraction(axis))
          axis += 1
        if weight > 0.0 then
          val grid = Vector.tabulate(3)(axis => lower(axis) + ((corner >> axis) & 1))
          val lin = volume.gridToIndex(grid(0), grid(1), grid(2))
          stencil += ((lin, weight, volume.valueAtCanonicalOrdinal(lin)))
        corner += 1
      Some(stencil.result())

  private def providerContinuousIndex(volume: SomeScalarVolume[Double], point: Vector[Double]): Either[ImageError, Vector[Double]] =
    val image = volume.sampled
    Point.fromVector[D3](image.frame, point).flatMap: sourcePoint =>
      Frame.alignOwners[D3, image.frame.type, image.sampleSpace.F](image.frame, image.frame).flatMap: alignment =>
        alignment.pointToRight(sourcePoint).flatMap(image.grid.continuousIndexOf)
    .left.map(ImageError.Geometry.apply).map(_.values)

  private def providerLinear(
    volume: SomeScalarVolume[Double],
    point: Vector[Double]
  ): Either[ImageError, Double] =
    val source = volume.sampled
    val space = source.sampleSpace
    // Re-admit the same array with its retained singleton sample-space owner.
    // This checks the space/data contract without copying voxel storage.
    Sampled.create[Double, Continuous, Rank[3]](space, source.data, source.metadata).flatMap: sampled =>
      Point.fromVector[D3](sampled.frame, point).left.map(ImageError.Geometry.apply).flatMap: providerPoint =>
        providerLinearAt[space.F, sampled.frame.type, space.type](sampled, providerPoint)

  private def providerLinearAt[
    IF <: Frame[D3],
    PF <: Frame[D3],
    S <: SampleSpace[IF, D3]
  ](
    image: Sampled[S, Double, Continuous, Rank[3]],
    point: Point[PF, D3]
  ): Either[ImageError, Double] =
    Frame.alignOwners[D3, IF, PF](image.frame, point.frame).left.map(ImageError.Geometry.apply).flatMap: alignment =>
      alignment.pointToLeft(point).left.map(ImageError.Geometry.apply).flatMap: imagePoint =>
        SampledInterpolator.at(image, imagePoint, Vector.empty, Interpolation.Linear, BoundaryPolicy.Reject).map(_.value)

  /** Reduces the finite samples only; NaN when there are none. */
  private def aggregate(samples: Vector[Double]): Double =
    val values = samples.filter(_.isFinite)
    if values.isEmpty then Double.NaN
    else plan.aggregation match
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
    mask: Option[SomeMaskVolume] = None,
    interpolation: SurfaceVoxelInterpolation = SurfaceVoxelInterpolation.Nearest
  ): SurfaceSampleResult =
    VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(surfaces, path, aggregation, interpolation)).sample(volume, mask)

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
