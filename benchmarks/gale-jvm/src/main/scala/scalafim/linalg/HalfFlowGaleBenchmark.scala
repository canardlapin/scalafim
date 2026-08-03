package scalafim.linalg

import gale.linalg.*
import gale.solvers.{IterativeSolvers, SolverConfig, ToleranceMode}
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import scala.compiletime.uninitialized

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class HalfFlowGaleBenchmark:
  private val tinySystems = 262144
  private val denseSystems = 4096
  private val side = 32
  private val voxels = side * side * side

  private var a00: Array[Double] = uninitialized
  private var a01: Array[Double] = uninitialized
  private var a02: Array[Double] = uninitialized
  private var a11: Array[Double] = uninitialized
  private var a12: Array[Double] = uninitialized
  private var a22: Array[Double] = uninitialized
  private var b0: Array[Double] = uninitialized
  private var b1: Array[Double] = uninitialized
  private var b2: Array[Double] = uninitialized
  private var x0: Array[Double] = uninitialized
  private var x1: Array[Double] = uninitialized
  private var x2: Array[Double] = uninitialized

  private var helmholtz: PeriodicHelmholtz = uninitialized
  private var helmholtzInput: DVec = uninitialized
  private var helmholtzOutput: MutableDVec = uninitialized
  private var rhs: DVec = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    a00 = new Array[Double](tinySystems)
    a01 = new Array[Double](tinySystems)
    a02 = new Array[Double](tinySystems)
    a11 = new Array[Double](tinySystems)
    a12 = new Array[Double](tinySystems)
    a22 = new Array[Double](tinySystems)
    b0 = new Array[Double](tinySystems)
    b1 = new Array[Double](tinySystems)
    b2 = new Array[Double](tinySystems)
    x0 = new Array[Double](tinySystems)
    x1 = new Array[Double](tinySystems)
    x2 = new Array[Double](tinySystems)

    var i = 0
    while i < tinySystems do
      val t = i.toDouble
      val l00 = 1.0 + 0.20 * math.abs(math.sin(t * 0.011))
      val l10 = 0.08 * math.sin(t * 0.017)
      val l11 = 0.8 + 0.15 * math.abs(math.cos(t * 0.013))
      val l20 = 0.06 * math.cos(t * 0.019)
      val l21 = 0.05 * math.sin(t * 0.023)
      val l22 = 0.7 + 0.10 * math.abs(math.sin(t * 0.029))
      a00(i) = l00 * l00 + 1e-3
      a01(i) = l00 * l10
      a02(i) = l00 * l20
      a11(i) = l10 * l10 + l11 * l11 + 1e-3
      a12(i) = l10 * l20 + l11 * l21
      a22(i) = l20 * l20 + l21 * l21 + l22 * l22 + 1e-3
      b0(i) = math.sin(t * 0.007)
      b1(i) = math.cos(t * 0.005)
      b2(i) = math.sin(t * 0.003 + 0.4)
      i += 1

    val scalarChecksum = scalarCholeskyBatch()
    require(scalarChecksum.isFinite, "scalar 3x3 setup solve must be finite")
    require(maximumTinyResidual() <= 5e-13, "scalar 3x3 setup residual exceeded tolerance")
    val denseChecksum = galeDenseCholeskyBatch()
    require(denseChecksum.isFinite, "Gale dense setup solve must be finite")

    helmholtz = PeriodicHelmholtz(side, side, side, alpha = 0.6)
    helmholtzInput = DVec.tabulate(voxels): index =>
      val x = index % side
      val y = (index / side) % side
      val z = index / (side * side)
      math.cos(2.0 * math.Pi * x.toDouble / side.toDouble) +
        0.5 * math.sin(4.0 * math.Pi * y.toDouble / side.toDouble) +
        0.25 * math.cos(6.0 * math.Pi * z.toDouble / side.toDouble)
    helmholtzOutput = MutableDVec.zeros(voxels)
    helmholtz.applyTo(helmholtzInput, helmholtzOutput)
    require(maximumFourierApplyError() <= 2e-12, "Gale operator apply failed Fourier oracle")

    rhs = DVec.tabulate(voxels): index =>
      val x = index % side
      val y = (index / side) % side
      val z = index / (side * side)
      1.0 +
        0.2 * math.cos(2.0 * math.Pi * x.toDouble / side.toDouble) +
        0.1 * math.sin(4.0 * math.Pi * y.toDouble / side.toDouble) +
        0.05 * math.cos(6.0 * math.Pi * z.toDouble / side.toDouble)
    val galeResult = galeCgSolve()
    require(galeResult.isFinite, "Gale CG setup solve must be finite")

  @Benchmark
  @OperationsPerInvocation(262144)
  def scalarCholeskyBatch(): Double =
    var checksum = 0.0
    var i = 0
    while i < tinySystems do
      val l00 = math.sqrt(a00(i))
      val l10 = a01(i) / l00
      val l20 = a02(i) / l00
      val l11 = math.sqrt(a11(i) - l10 * l10)
      val l21 = (a12(i) - l20 * l10) / l11
      val l22 = math.sqrt(a22(i) - l20 * l20 - l21 * l21)

      val y0 = b0(i) / l00
      val y1 = (b1(i) - l10 * y0) / l11
      val y2 = (b2(i) - l20 * y0 - l21 * y1) / l22
      val s2 = y2 / l22
      val s1 = (y1 - l21 * s2) / l11
      val s0 = (y0 - l10 * s1 - l20 * s2) / l00
      x0(i) = s0
      x1(i) = s1
      x2(i) = s2
      checksum += s0 + 0.5 * s1 + 0.25 * s2
      i += 1
    checksum

  @Benchmark
  @OperationsPerInvocation(262144)
  def galeMat3AdjugateBatch(): Double =
    var checksum = 0.0
    var i = 0
    while i < tinySystems do
      val matrix = Mat3(
        a00(i), a01(i), a02(i),
        a01(i), a11(i), a12(i),
        a02(i), a12(i), a22(i)
      )
      val rhs = Vec3(b0(i), b1(i), b2(i))
      val c00 = matrix.a11 * matrix.a22 - matrix.a12 * matrix.a21
      val c01 = matrix.a02 * matrix.a21 - matrix.a01 * matrix.a22
      val c02 = matrix.a01 * matrix.a12 - matrix.a02 * matrix.a11
      val c11 = matrix.a00 * matrix.a22 - matrix.a02 * matrix.a20
      val c12 = matrix.a01 * matrix.a20 - matrix.a00 * matrix.a21
      val c22 = matrix.a00 * matrix.a11 - matrix.a01 * matrix.a10
      val inverseDeterminant = 1.0 / matrix.det
      val s0 = (c00 * rhs.x0 + c01 * rhs.x1 + c02 * rhs.x2) * inverseDeterminant
      val s1 = (c01 * rhs.x0 + c11 * rhs.x1 + c12 * rhs.x2) * inverseDeterminant
      val s2 = (c02 * rhs.x0 + c12 * rhs.x1 + c22 * rhs.x2) * inverseDeterminant
      x0(i) = s0
      x1(i) = s1
      x2(i) = s2
      checksum += s0 + 0.5 * s1 + 0.25 * s2
      i += 1
    checksum

  @Benchmark
  @OperationsPerInvocation(4096)
  def galeDenseCholeskyBatch(): Double =
    var checksum = 0.0
    var i = 0
    while i < denseSystems do
      val matrix = Matrix.dense(3, 3)(
        a00(i), a01(i), a02(i),
        a01(i), a11(i), a12(i),
        a02(i), a12(i), a22(i)
      )
      val solution = matrix.cholesky(CholeskyOptions()).flatMap(_.solve(Vec(b0(i), b1(i), b2(i)))) match
        case Right(value) => value
        case Left(error)  => throw error
      checksum += solution(0) + 0.5 * solution(1) + 0.25 * solution(2)
      i += 1
    checksum

  @Benchmark
  @OperationsPerInvocation(32)
  def helmholtzApplyInto(): Double =
    var checksum = 0.0
    var iteration = 0
    while iteration < 32 do
      helmholtz.applyTo(helmholtzInput, helmholtzOutput)
      checksum += helmholtzOutput((iteration * 997) % voxels)
      iteration += 1
    checksum

  @Benchmark
  def galeCgSolve(): Double =
    val result = IterativeSolvers.cg(
      helmholtz,
      rhs,
      SolverConfig(tolerance = 1e-10, maxIterations = 80),
      toleranceMode = ToleranceMode.RelativeToRhs
    )
    result.x(voxels / 2) + result.iterations.toDouble + result.residual

  private def maximumTinyResidual(): Double =
    var maximum = 0.0
    var i = 0
    while i < tinySystems do
      maximum = math.max(maximum, math.abs(a00(i) * x0(i) + a01(i) * x1(i) + a02(i) * x2(i) - b0(i)))
      maximum = math.max(maximum, math.abs(a01(i) * x0(i) + a11(i) * x1(i) + a12(i) * x2(i) - b1(i)))
      maximum = math.max(maximum, math.abs(a02(i) * x0(i) + a12(i) * x1(i) + a22(i) * x2(i) - b2(i)))
      i += 1
    maximum

  private def maximumFourierApplyError(): Double =
    var maximum = 0.0
    var index = 0
    while index < voxels do
      val x = index % side
      val y = (index / side) % side
      val z = index / (side * side)
      val expected =
        PeriodicHelmholtz.eigenvalue(side, 1, 0.6) * math.cos(2.0 * math.Pi * x.toDouble / side.toDouble) +
          0.5 * PeriodicHelmholtz.eigenvalue(side, 2, 0.6) * math.sin(4.0 * math.Pi * y.toDouble / side.toDouble) +
          0.25 * PeriodicHelmholtz.eigenvalue(side, 3, 0.6) * math.cos(6.0 * math.Pi * z.toDouble / side.toDouble)
      maximum = math.max(maximum, math.abs(helmholtzOutput(index) - expected))
      index += 1
    maximum

private final class PeriodicHelmholtz(
    val nx: Int,
    val ny: Int,
    val nz: Int,
    val alpha: Double
) extends DoubleLinearOperator:
  private val plane = nx * ny
  override val rows: Int = plane * nz
  override val cols: Int = rows

  override def applyTo(input: DVec, output: MutableDVec): Unit =
    if input.length != cols then throw LinAlgError.VectorLengthMismatch(cols, input.length)
    if output.length != rows then throw LinAlgError.VectorLengthMismatch(rows, output.length)
    var z = 0
    while z < nz do
      val zm = if z == 0 then nz - 1 else z - 1
      val zp = if z + 1 == nz then 0 else z + 1
      var y = 0
      while y < ny do
        val ym = if y == 0 then ny - 1 else y - 1
        val yp = if y + 1 == ny then 0 else y + 1
        var x = 0
        while x < nx do
          val xm = if x == 0 then nx - 1 else x - 1
          val xp = if x + 1 == nx then 0 else x + 1
          val i = x + nx * y + plane * z
          val neighbourSum =
            input(xm + nx * y + plane * z) + input(xp + nx * y + plane * z) +
              input(x + nx * ym + plane * z) + input(x + nx * yp + plane * z) +
              input(x + nx * y + plane * zm) + input(x + nx * y + plane * zp)
          output(i) = input(i) + alpha * (6.0 * input(i) - neighbourSum)
          x += 1
        y += 1
      z += 1

private object PeriodicHelmholtz:
  def apply(nx: Int, ny: Int, nz: Int, alpha: Double): PeriodicHelmholtz =
    new PeriodicHelmholtz(nx, ny, nz, alpha)

  def eigenvalue(side: Int, frequency: Int, alpha: Double): Double =
    1.0 + 2.0 * alpha * (1.0 - math.cos(2.0 * math.Pi * frequency.toDouble / side.toDouble))
