package scalafim.surface.reference

import image4s.{Axis, AxisKind, NonSpatialAxes, Sampled}
import image4s.geometry.{Affine, CoordinateConvention, D3, Frame, FrameMetadata, Grid, Point}
import reframe4s.core.{MapError, SpatialMap}
import reframe4s.field.{CoverageReportingMap, Displacement, SupportOutcome}
import reframe4s.lie.FramedAffine
import ravel.DType.given
import ravel.NDArray
import scalafim.image.WorldPoint

enum PointMapError:
  case InvalidField(reason: String)
  case InvalidStage(reason: String)
  case InvalidPolicy(reason: String)

  def message: String = this match
    case InvalidField(reason) => s"invalid displacement field: $reason"
    case InvalidStage(reason) => s"invalid point-map stage: $reason"
    case InvalidPolicy(reason) => s"invalid inverse policy: $reason"

/** Dense RAS component-planar displacement data. Runtime interpolation is
  * delegated to image4s/reframe4s; this type retains the decoded contract.
  */
final class DisplacementField private (
  val nx: Int,
  val ny: Int,
  val nz: Int,
  val voxelToRas: Affine[D3],
  private[reference] val values: Array[Double]
):
  private[reference] lazy val sharedProviderMap
      : Either[PointMapError, CoverageReportingMap[PointMap.rasFrame.type, PointMap.rasFrame.type, D3]] =
    PointMap.providerMap(this, PointMap.rasFrame)

  def dims: Vector[Int] = Vector(nx, ny, nz)

  def supports(x: Double, y: Double, z: Double): Boolean =
    voxelToRas.inverse(Vector(x, y, z)).toOption.exists: c =>
      inside(c(0), nx) && inside(c(1), ny) && inside(c(2), nz)

  /** Writes zero and returns false outside ITK support. */
  def displacementInto(x: Double, y: Double, z: Double, out: Array[Double]): Boolean =
    (sharedProviderMap.toOption.flatMap: map =>
      Point.fromVector(PointMap.rasFrame, Vector(x, y, z)).toOption.flatMap(map.applyWithCoverage(_).toOption)
    ) match
      case Some(mapped) if mapped.outcome == SupportOutcome.Covered =>
        val q = mapped.point.coordinates
        out(0) = q(0) - x
        out(1) = q(1) - y
        out(2) = q(2) - z
        true
      case _ =>
        out(0) = 0.0
        out(1) = 0.0
        out(2) = 0.0
        false

  private inline def inside(c: Double, n: Int): Boolean = c >= -0.5 && c < n - 0.5

object DisplacementField:
  def make(dims: Vector[Int], voxelToRas: Affine[D3], componentPlanar: Array[Double]): Either[PointMapError, DisplacementField] =
    validate(dims, componentPlanar).map(_ => DisplacementField(dims(0), dims(1), dims(2), voxelToRas, componentPlanar.clone()))

  private[reference] def owned(dims: Vector[Int], voxelToRas: Affine[D3], componentPlanar: Array[Double]): Either[PointMapError, DisplacementField] =
    validate(dims, componentPlanar).map(_ => DisplacementField(dims(0), dims(1), dims(2), voxelToRas, componentPlanar))

  private def validate(dims: Vector[Int], values: Array[Double]): Either[PointMapError, Unit] =
    if dims.length != 3 || dims.exists(_ <= 0) then Left(PointMapError.InvalidField(s"dims must be three positive extents; got $dims"))
    else if dims.map(_.toLong).product * 3L != values.length.toLong then Left(PointMapError.InvalidField(s"expected ${dims.map(_.toLong).product * 3L} values for dims $dims; got ${values.length}"))
    else
      val invalid = values.indexWhere(value => !value.isFinite)
      if invalid >= 0 then Left(PointMapError.InvalidField(s"value $invalid is not finite")) else Right(())

enum PointMapStage:
  case AffineStage(matrix: Affine[D3])
  case DisplacementStage(field: DisplacementField)

enum PointMapOutcome:
  case Mapped(point: WorldPoint)
  case OutsideSupport
  case NonFinite
  case Unavailable(reason: String)

  def placed: Option[WorldPoint] = this match
    case Mapped(point) => Some(point)
    case _ => None

/** Kept only to declare an intended inverse request. The pinned provider does
  * not expose pointwise inversion for finite displacement maps.
  */
final case class InversePolicy private (toleranceMm: Double, maxIterations: Int)

object InversePolicy:
  def make(toleranceMm: Double, maxIterations: Int): Either[PointMapError, InversePolicy] =
    if !(toleranceMm.isFinite && toleranceMm >= 0.0) then Left(PointMapError.InvalidPolicy("tolerance must be finite and non-negative"))
    else if maxIterations < 1 then Left(PointMapError.InvalidPolicy("at least one iteration is required"))
    else Right(InversePolicy(toleranceMm, maxIterations))

final class PointMap private (
  val stages: Vector[PointMapStage],
  private val forwardMap: CoverageReportingMap[PointMap.rasFrame.type, PointMap.rasFrame.type, D3]
):
  /** ITK raw semantics: outside a field, retain the coordinate and report false. */
  private[reference] def forwardInto(x: Double, y: Double, z: Double, out: Array[Double]): Boolean =
    Point.fromVector(PointMap.rasFrame, Vector(x, y, z)).toOption.flatMap(forwardMap.applyWithCoverage(_).toOption) match
      case Some(mapped) =>
        val q = mapped.point.coordinates
        out(0) = q(0)
        out(1) = q(1)
        out(2) = q(2)
        mapped.outcome == SupportOutcome.Covered
      case None =>
        out(0) = Double.NaN
        out(1) = Double.NaN
        out(2) = Double.NaN
        false

  def forward(point: WorldPoint): PointMapOutcome =
    val out = new Array[Double](3)
    val supported = forwardInto(point.x, point.y, point.z, out)
    if !out.forall(_.isFinite) then PointMapOutcome.NonFinite
    else if supported then PointMapOutcome.Mapped(WorldPoint(out(0), out(1), out(2)))
    else PointMapOutcome.OutsideSupport

  def inverse(point: WorldPoint, policy: InversePolicy): PointMapOutcome =
    PointMapOutcome.Unavailable("pointwise inverse is unavailable for finite displacement maps in reframe4s 9a450")

object PointMap:
  private[reference] val rasFrame: Frame[D3] =
    Frame.ephemeral(FrameMetadata.named("RAS displacement field").fold(error => throw new IllegalStateException(error.message), identity),
      convention = CoordinateConvention.RAS)

  def make(stages: Vector[PointMapStage]): Either[PointMapError, PointMap] =
    if stages.isEmpty then Left(PointMapError.InvalidStage("a point map needs at least one stage"))
    else sequence(stages.map(stageMap)).map: maps =>
      val composite = maps.tail.foldLeft(maps.head): (first, next) =>
        CoverageReportingMap.compose(first, next)
      PointMap(stages, composite)

  private[reference] def providerMap(field: DisplacementField, frame: Frame[D3])
      : Either[PointMapError, CoverageReportingMap[frame.type, frame.type, D3]] =
    for
      grid <- Grid.in(frame)(field.dims, field.voxelToRas).left.map(error => PointMapError.InvalidField(error.message))
      _ <- Either.cond((field.nx.toLong + 2) * (field.ny.toLong + 2) * (field.nz.toLong + 2) * 3 <= Int.MaxValue,
        (), PointMapError.InvalidField("edge-extended provider field exceeds the array size limit"))
      origin <- field.voxelToRas(Vector(-1.0, -1.0, -1.0)).left.map(error => PointMapError.InvalidField(error.message))
      paddedAffine <- Affine.fromRowMajor[D3](field.voxelToRas.rowMajor.updated(3, origin(0)).updated(7, origin(1)).updated(11, origin(2)))
        .left.map(error => PointMapError.InvalidField(error.message))
      paddedGrid <- Grid.in(frame)(field.dims.map(_ + 2), paddedAffine).left.map(error => PointMapError.InvalidField(error.message))
      direction <- Axis.create("displacement", 3, AxisKind.Direction).left.map(error => PointMapError.InvalidField(error.message))
      axes <- NonSpatialAxes.from(Vector(direction)).left.map(error => PointMapError.InvalidField(error.message))
      image <- Sampled.continuous(paddedGrid, axes,
        NDArray.tabulate[Double](field.nx + 2, field.ny + 2, field.nz + 2, 3)((i, j, k, c) =>
          val x = math.max(0, math.min(field.nx - 1, i - 1))
          val y = math.max(0, math.min(field.ny - 1, j - 1))
          val z = math.max(0, math.min(field.nz - 1, k - 1))
          field.values(x + field.nx * (y + field.ny * (z + field.nz * c)))))
        .left.map(error => PointMapError.InvalidField(error.message))
      displacement <- Displacement.from(image).left.map(error => PointMapError.InvalidField(error.message))
    // One replicated layer expresses ITK's constant edge displacement through
    // the provider's ordinary interpolator. Even oblique edge queries remain
    // inside that grid; support is still checked against the original field.
    yield new SupportedDisplacementMap(frame, grid, Displacement.asMap(displacement))

  private def stageMap(stage: PointMapStage)
      : Either[PointMapError, CoverageReportingMap[rasFrame.type, rasFrame.type, D3]] = stage match
    case PointMapStage.AffineStage(matrix) => Right(CoverageReportingMap.lift(FramedAffine.between(rasFrame, rasFrame)(matrix)))
    case PointMapStage.DisplacementStage(field) => field.sharedProviderMap

  private def sequence[A](values: Vector[Either[PointMapError, A]]): Either[PointMapError, Vector[A]] =
    values.foldLeft[Either[PointMapError, Vector[A]]](Right(Vector.empty)): (acc, next) =>
      for all <- acc; one <- next yield all :+ one

  private final class SupportedDisplacementMap[F <: Frame[D3]](
    val source: F,
    grid: Grid[F, D3],
    underlying: SpatialMap[F, F, D3]
  ) extends CoverageReportingMap[F, F, D3]:
    val target: F = source

    def applyWithCoverage(point: Point[F, D3]) =
      grid.continuousIndexOf(point).left.map(MapError.Geometry.apply).flatMap: index =>
        val c = index.values
        if !inside(c(0), grid.shape(0)) || !inside(c(1), grid.shape(1)) || !inside(c(2), grid.shape(2)) then
          Right(reframe4s.field.CoveredPoint(point, SupportOutcome.SourcePreserved))
        else
          underlying(point).map(reframe4s.field.CoveredPoint(_, SupportOutcome.Covered))

    private inline def inside(value: Double, extent: Int): Boolean = value >= -0.5 && value < extent - 0.5
