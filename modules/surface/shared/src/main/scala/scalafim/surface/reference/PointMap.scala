package scalafim.surface.reference

import image4s.geometry.{Affine, D3}
import scalafim.image.WorldPoint

enum PointMapError:
  case InvalidField(reason: String)
  case InvalidStage(reason: String)
  case InvalidPolicy(reason: String)

  def message: String =
    this match
      case InvalidField(reason) => s"invalid displacement field: $reason"
      case InvalidStage(reason) => s"invalid point-map stage: $reason"
      case InvalidPolicy(reason) => s"invalid inverse policy: $reason"

/** A dense displacement field `d` on a voxel grid, in RAS millimetres, used as
  * `y = x + d(x)`. Evaluation matches ITK's `DisplacementFieldTransform` with
  * linear interpolation: with `ci = voxelToRas⁻¹ · x`, `d(x) = 0` unless
  * `-0.5 <= ci < n - 0.5` on every axis; inside, `d` is trilinear between the
  * eight surrounding voxel centres with neighbour indices clamped to
  * `[0, n - 1]`, so the half-voxel border takes the edge value.
  *
  * Values are component-planar: component `c` of voxel `(i, j, k)` is element
  * `i + nx * (j + ny * (k + nz * c))`.
  */
final class DisplacementField private (
  val nx: Int,
  val ny: Int,
  val nz: Int,
  val voxelToRas: Affine[D3],
  private val values: Array[Double]
):
  private val inv: Array[Double] = rowMajor3x4(voxelToRas.inverse)
  private val plane = nx * ny * nz

  def dims: Vector[Int] = Vector(nx, ny, nz)

  /** Whether `x` lies in the field's support box. Nonfinite points never do. */
  def supports(x: Double, y: Double, z: Double): Boolean =
    val ci = inv(0) * x + inv(1) * y + inv(2) * z + inv(3)
    val cj = inv(4) * x + inv(5) * y + inv(6) * z + inv(7)
    val ck = inv(8) * x + inv(9) * y + inv(10) * z + inv(11)
    inside(ci, nx) && inside(cj, ny) && inside(ck, nz)

  /** Write `d(x)` into `out(0..2)`; zero outside the support. Returns whether `x` was inside. */
  def displacementInto(x: Double, y: Double, z: Double, out: Array[Double]): Boolean =
    val ci = inv(0) * x + inv(1) * y + inv(2) * z + inv(3)
    val cj = inv(4) * x + inv(5) * y + inv(6) * z + inv(7)
    val ck = inv(8) * x + inv(9) * y + inv(10) * z + inv(11)
    if !(inside(ci, nx) && inside(cj, ny) && inside(ck, nz)) then
      out(0) = 0.0
      out(1) = 0.0
      out(2) = 0.0
      false
    else
      val fi = math.floor(ci)
      val fj = math.floor(cj)
      val fk = math.floor(ck)
      val ti = ci - fi
      val tj = cj - fj
      val tk = ck - fk
      val i0 = clamp(fi.toInt, nx)
      val i1 = clamp(fi.toInt + 1, nx)
      val j0 = clamp(fj.toInt, ny)
      val j1 = clamp(fj.toInt + 1, ny)
      val k0 = clamp(fk.toInt, nz)
      val k1 = clamp(fk.toInt + 1, nz)
      var c = 0
      while c < 3 do
        val base = c * plane
        val v000 = values(base + i0 + nx * (j0 + ny * k0))
        val v100 = values(base + i1 + nx * (j0 + ny * k0))
        val v010 = values(base + i0 + nx * (j1 + ny * k0))
        val v110 = values(base + i1 + nx * (j1 + ny * k0))
        val v001 = values(base + i0 + nx * (j0 + ny * k1))
        val v101 = values(base + i1 + nx * (j0 + ny * k1))
        val v011 = values(base + i0 + nx * (j1 + ny * k1))
        val v111 = values(base + i1 + nx * (j1 + ny * k1))
        val v00 = v000 + ti * (v100 - v000)
        val v10 = v010 + ti * (v110 - v010)
        val v01 = v001 + ti * (v101 - v001)
        val v11 = v011 + ti * (v111 - v011)
        val v0 = v00 + tj * (v10 - v00)
        val v1 = v01 + tj * (v11 - v01)
        out(c) = v0 + tk * (v1 - v0)
        c += 1
      true

  private inline def inside(c: Double, n: Int): Boolean = c >= -0.5 && c < n - 0.5

  private inline def clamp(index: Int, n: Int): Int = if index < 0 then 0 else if index >= n then n - 1 else index

object DisplacementField:
  /** Copy a component-planar field; every value must be finite. */
  def make(dims: Vector[Int], voxelToRas: Affine[D3], componentPlanar: Array[Double]): Either[PointMapError, DisplacementField] =
    validate(dims, componentPlanar).map(_ => DisplacementField(dims(0), dims(1), dims(2), voxelToRas, componentPlanar.clone()))

  /** Take ownership of a freshly decoded array without copying (reader hot path). */
  private[reference] def owned(dims: Vector[Int], voxelToRas: Affine[D3], componentPlanar: Array[Double]): Either[PointMapError, DisplacementField] =
    validate(dims, componentPlanar).map(_ => DisplacementField(dims(0), dims(1), dims(2), voxelToRas, componentPlanar))

  private def validate(dims: Vector[Int], values: Array[Double]): Either[PointMapError, Unit] =
    if dims.length != 3 || dims.exists(_ <= 0) then Left(PointMapError.InvalidField(s"dims must be three positive extents; got $dims"))
    else if dims.map(_.toLong).product * 3L != values.length.toLong then
      Left(PointMapError.InvalidField(s"expected ${dims.map(_.toLong).product * 3L} values for dims $dims; got ${values.length}"))
    else
      var i = 0
      while i < values.length && values(i).isFinite do i += 1
      if i < values.length then Left(PointMapError.InvalidField(s"value $i is not finite")) else Right(())

private def rowMajor3x4(affine: Affine[D3]): Array[Double] =
  val m = affine.rowMajor
  Array.tabulate(12)(i => m(i))

/** One stage of a point map, applied as `y = stage(x)`. */
enum PointMapStage:
  /** `y = M · x + t` (row-major 4x4). */
  case AffineStage(matrix: Affine[D3])
  /** `y = x + d(x)`. */
  case DisplacementStage(field: DisplacementField)

/** How a point map was applied to a point. */
enum PointMapOutcome:
  /** Forward evaluation with every displacement stage inside its support. */
  case Mapped(point: WorldPoint)
  /** Inverse solve that reached the declared residual within the iteration budget. */
  case Converged(point: WorldPoint, residualMm: Double, iterations: Int)
  /** Inverse solve that did not reach the declared residual; `point` is the last iterate and is not admitted. */
  case NonConvergent(point: WorldPoint, residualMm: Double, iterations: Int)
  /** The input, or the solution, lies outside a displacement support; nothing is extrapolated. */
  case OutsideSupport

  /** The admitted location, if any. */
  def placed: Option[WorldPoint] =
    this match
      case Mapped(point) => Some(point)
      case Converged(point, _, _) => Some(point)
      case _ => None

/** Fixed-point inversion `y ← y + (x − T(y))` from `y = x`, stopping once
  * `‖x − T(y)‖ <= toleranceMm` or after `maxIterations` updates.
  */
final case class InversePolicy private (toleranceMm: Double, maxIterations: Int)

object InversePolicy:
  def make(toleranceMm: Double, maxIterations: Int): Either[PointMapError, InversePolicy] =
    if !(toleranceMm.isFinite && toleranceMm >= 0.0) then Left(PointMapError.InvalidPolicy("tolerance must be finite and non-negative"))
    else if maxIterations < 1 then Left(PointMapError.InvalidPolicy("at least one iteration is required"))
    else Right(InversePolicy(toleranceMm, maxIterations))

/** An ordered composite point map: `stages(0)` is applied first. */
final class PointMap private (val stages: Vector[PointMapStage]):
  private val count = stages.length
  private val affines: Array[Array[Double]] = stages.map {
    case PointMapStage.AffineStage(matrix) => rowMajor3x4(matrix)
    case _ => null
  }.toArray
  private val fields: Array[DisplacementField] = stages.map {
    case PointMapStage.DisplacementStage(field) => field
    case _ => null
  }.toArray

  /** ITK-exact composite `T(x)` written into `out(0..2)` (zero displacement
    * outside a field, as ITK does). `scratch` must hold at least 3 values.
    * Returns whether every displacement stage saw its point inside support.
    */
  def forwardInto(x: Double, y: Double, z: Double, out: Array[Double], scratch: Array[Double]): Boolean =
    var px = x
    var py = y
    var pz = z
    var supported = true
    var s = 0
    while s < count do
      val a = affines(s)
      if a != null then
        val nx = a(0) * px + a(1) * py + a(2) * pz + a(3)
        val ny = a(4) * px + a(5) * py + a(6) * pz + a(7)
        val nz = a(8) * px + a(9) * py + a(10) * pz + a(11)
        px = nx
        py = ny
        pz = nz
      else
        if !fields(s).displacementInto(px, py, pz, scratch) then supported = false
        px += scratch(0)
        py += scratch(1)
        pz += scratch(2)
      s += 1
    out(0) = px
    out(1) = py
    out(2) = pz
    supported

  /** `T(x)`, refusing points that leave any displacement support. */
  def forward(point: WorldPoint): PointMapOutcome =
    val out = new Array[Double](3)
    if forwardInto(point.x, point.y, point.z, out, new Array[Double](3)) && out.forall(_.isFinite) then
      PointMapOutcome.Mapped(WorldPoint(out(0), out(1), out(2)))
    else PointMapOutcome.OutsideSupport

  /** Solve `T(y) = x`. The solution must lie inside every displacement support. */
  def inverse(point: WorldPoint, policy: InversePolicy): PointMapOutcome =
    locally:
      val t = new Array[Double](3)
      val scratch = new Array[Double](3)
      val x = point.x
      val y = point.y
      val z = point.z
      var yx = x
      var yy = y
      var yz = z
      var iterations = 0
      forwardInto(yx, yy, yz, t, scratch)
      var rx = x - t(0)
      var ry = y - t(1)
      var rz = z - t(2)
      var residual = math.sqrt(rx * rx + ry * ry + rz * rz)
      while residual > policy.toleranceMm && iterations < policy.maxIterations && residual.isFinite do
        yx += rx
        yy += ry
        yz += rz
        iterations += 1
        forwardInto(yx, yy, yz, t, scratch)
        rx = x - t(0)
        ry = y - t(1)
        rz = z - t(2)
        residual = math.sqrt(rx * rx + ry * ry + rz * rz)
      val supported = residual.isFinite && forwardInto(yx, yy, yz, t, scratch)
      if !supported then PointMapOutcome.OutsideSupport
      else if residual <= policy.toleranceMm then PointMapOutcome.Converged(WorldPoint(yx, yy, yz), residual, iterations)
      else PointMapOutcome.NonConvergent(WorldPoint(yx, yy, yz), residual, iterations)

object PointMap:
  def make(stages: Vector[PointMapStage]): Either[PointMapError, PointMap] =
    if stages.isEmpty then Left(PointMapError.InvalidStage("a point map needs at least one stage"))
    else Right(PointMap(stages))
