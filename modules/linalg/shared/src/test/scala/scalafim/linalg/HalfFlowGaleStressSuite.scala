package scalafim.linalg

import gale.linalg.*
import gale.solvers.{IterativeSolvers, SolverConfig, ToleranceMode}

class HalfFlowGaleStressSuite extends munit.FunSuite:
  import HalfFlowGaleStress.*

  test("scalar symmetric 3x3 Cholesky agrees with Gale and its original system"):
    val systems = Vector(
      System3(4.0, 0.3, -0.2, 3.0, 0.4, 2.0, 1.0, -2.0, 0.5),
      System3(1e-6, 0.0, 0.0, 1.0, 0.0, 1e6, 3e-7, -0.5, 2e5),
      fromLower(1e-4, 3e-5, 2e-3, -2e-5, 4e-4, 0.7, 0.2, -0.4, 1.5, damping = 1e-10),
      fromLower(1.2, -0.2, 0.9, 0.1, -0.15, 0.8, -1.0, 0.25, 2.0, damping = 1e-3)
    )

    systems.foreach: system =>
      val scalar = solveCholesky(system, minimumPivot = 1e-16).fold(error => fail(error), identity)
      val gale = galeSolve(system)
      assert(relativeResidual(system, scalar) <= 2e-13)
      assert(relativeDifference(scalar, gale) <= 2e-12)

  test("scalar symmetric 3x3 Cholesky rejects non-finite and non-positive pivots"):
    assert(solveCholesky(System3(1.0, 0.0, 0.0, -1.0, 0.0, 1.0, 1.0, 2.0, 3.0), 1e-12).isLeft)
    assert(solveCholesky(System3(Double.NaN, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 2.0, 3.0), 1e-12).isLeft)

  test("original-system residual rejects the faster adjugate on an ill-conditioned SPD system"):
    val system = System3(
      0.30005464219752837,
      -0.1725022068417165,
      0.4240296108250584,
      0.09917373488363729,
      -0.24380960590992307,
      0.6017726229188344,
      -1.623791913221198,
      0.8910947703670219,
      -0.05393636309838139
    )
    val cholesky = solveCholesky(system, minimumPivot = 1e-16).fold(error => fail(error), identity)
    val adjugate = solveAdjugate(system)
    val choleskyResidual = relativeResidual(system, cholesky)
    val adjugateResidual = relativeResidual(system, adjugate)

    assert(choleskyResidual <= 1e-11, s"Cholesky residual=$choleskyResidual")
    assert(adjugateResidual >= 1e-10, s"adjugate residual=$adjugateResidual")
    assert(adjugateResidual >= choleskyResidual * 100.0)

  test("Gale destination operator satisfies the periodic Helmholtz Fourier oracle"):
    val side = 12
    val alpha = 0.6
    val operator = PeriodicHelmholtz(side, side, side, alpha)
    val input = fourierMixture(side)
    val output = MutableDVec.zeros(input.length)
    operator.applyTo(input, output)

    val error = maximumFourierApplyError(side, alpha, output.asVec)
    assert(error <= 3e-12, s"maximum Fourier apply error=$error")

  test("Gale CG solves independent Helmholtz right-hand sides cleanly"):
    val side = 12
    val alpha = 0.6
    val operator = PeriodicHelmholtz(side, side, side, alpha)
    val expected = inverseFourierMixture(side, alpha)
    val rhs = fourierMixture(side)

    val result = IterativeSolvers.cg(
      operator,
      rhs,
      SolverConfig(tolerance = 1e-12, maxIterations = 40),
      toleranceMode = ToleranceMode.RelativeToRhs
    )
    assert(result.converged, s"iterations=${result.iterations} residual=${result.residual}")
    assert(maximumDifference(result.x, expected) <= 2e-11)

    val constant = DVec.fill(rhs.length)(2.5)
    val constantResult = IterativeSolvers.cg(
      operator,
      constant,
      SolverConfig(tolerance = 1e-13, maxIterations = 10),
      toleranceMode = ToleranceMode.RelativeToRhs
    )
    assert(constantResult.converged)
    assert(maximumConstantError(constantResult.x, 2.5) <= 2e-12)

private[scalafim] object HalfFlowGaleStress:
  final case class System3(
      a00: Double,
      a01: Double,
      a02: Double,
      a11: Double,
      a12: Double,
      a22: Double,
      b0: Double,
      b1: Double,
      b2: Double
  )

  def fromLower(
      l00: Double,
      l10: Double,
      l11: Double,
      l20: Double,
      l21: Double,
      l22: Double,
      b0: Double,
      b1: Double,
      b2: Double,
      damping: Double
  ): System3 =
    System3(
      l00 * l00 + damping,
      l00 * l10,
      l00 * l20,
      l10 * l10 + l11 * l11 + damping,
      l10 * l20 + l11 * l21,
      l20 * l20 + l21 * l21 + l22 * l22 + damping,
      b0,
      b1,
      b2
    )

  def solveCholesky(system: System3, minimumPivot: Double): Either[String, Vec3] =
    val finite =
      system.a00.isFinite && system.a01.isFinite && system.a02.isFinite &&
        system.a11.isFinite && system.a12.isFinite && system.a22.isFinite &&
        system.b0.isFinite && system.b1.isFinite && system.b2.isFinite &&
        minimumPivot.isFinite && minimumPivot >= 0.0
    if !finite then Left("symmetric 3x3 solve requires finite inputs and a non-negative minimum pivot")
    else
      val p0 = system.a00
      if p0 <= minimumPivot then Left("symmetric 3x3 first pivot is not positive")
      else
        val l00 = math.sqrt(p0)
        val l10 = system.a01 / l00
        val l20 = system.a02 / l00
        val p1 = system.a11 - l10 * l10
        if p1 <= minimumPivot then Left("symmetric 3x3 second pivot is not positive")
        else
          val l11 = math.sqrt(p1)
          val l21 = (system.a12 - l20 * l10) / l11
          val p2 = system.a22 - l20 * l20 - l21 * l21
          if p2 <= minimumPivot then Left("symmetric 3x3 third pivot is not positive")
          else
            val l22 = math.sqrt(p2)
            val y0 = system.b0 / l00
            val y1 = (system.b1 - l10 * y0) / l11
            val y2 = (system.b2 - l20 * y0 - l21 * y1) / l22
            val x2 = y2 / l22
            val x1 = (y1 - l21 * x2) / l11
            val x0 = (y0 - l10 * x1 - l20 * x2) / l00
            if x0.isFinite && x1.isFinite && x2.isFinite then Right(Vec3(x0, x1, x2))
            else Left("symmetric 3x3 solution is not finite")

  def galeSolve(system: System3): Vec3 =
    val matrix = Matrix.dense(3, 3)(
      system.a00, system.a01, system.a02,
      system.a01, system.a11, system.a12,
      system.a02, system.a12, system.a22
    )
    val solution = matrix.cholesky(CholeskyOptions()).flatMap(_.solve(Vec(system.b0, system.b1, system.b2))) match
      case Right(value) => value
      case Left(error)  => throw error
    Vec3(solution(0), solution(1), solution(2))

  def solveAdjugate(system: System3): Vec3 =
    val matrix = Mat3(
      system.a00, system.a01, system.a02,
      system.a01, system.a11, system.a12,
      system.a02, system.a12, system.a22
    )
    val c00 = matrix.a11 * matrix.a22 - matrix.a12 * matrix.a21
    val c01 = matrix.a02 * matrix.a21 - matrix.a01 * matrix.a22
    val c02 = matrix.a01 * matrix.a12 - matrix.a02 * matrix.a11
    val c11 = matrix.a00 * matrix.a22 - matrix.a02 * matrix.a20
    val c12 = matrix.a01 * matrix.a20 - matrix.a00 * matrix.a21
    val c22 = matrix.a00 * matrix.a11 - matrix.a01 * matrix.a10
    val inverseDeterminant = 1.0 / matrix.det
    Vec3(
      (c00 * system.b0 + c01 * system.b1 + c02 * system.b2) * inverseDeterminant,
      (c01 * system.b0 + c11 * system.b1 + c12 * system.b2) * inverseDeterminant,
      (c02 * system.b0 + c12 * system.b1 + c22 * system.b2) * inverseDeterminant
    )

  def relativeResidual(system: System3, solution: Vec3): Double =
    val r0 = system.a00 * solution.x0 + system.a01 * solution.x1 + system.a02 * solution.x2 - system.b0
    val r1 = system.a01 * solution.x0 + system.a11 * solution.x1 + system.a12 * solution.x2 - system.b1
    val r2 = system.a02 * solution.x0 + system.a12 * solution.x1 + system.a22 * solution.x2 - system.b2
    val residual = math.sqrt(r0 * r0 + r1 * r1 + r2 * r2)
    val rhs = math.sqrt(system.b0 * system.b0 + system.b1 * system.b1 + system.b2 * system.b2)
    residual / math.max(1.0, rhs)

  def relativeDifference(left: Vec3, right: Vec3): Double =
    val d0 = left.x0 - right.x0
    val d1 = left.x1 - right.x1
    val d2 = left.x2 - right.x2
    val difference = math.sqrt(d0 * d0 + d1 * d1 + d2 * d2)
    val scale = math.sqrt(right.x0 * right.x0 + right.x1 * right.x1 + right.x2 * right.x2)
    difference / math.max(1.0, scale)

  final class PeriodicHelmholtz private (
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
            val index = x + nx * y + plane * z
            val neighbours =
              input(xm + nx * y + plane * z) + input(xp + nx * y + plane * z) +
                input(x + nx * ym + plane * z) + input(x + nx * yp + plane * z) +
                input(x + nx * y + plane * zm) + input(x + nx * y + plane * zp)
            output(index) = input(index) + alpha * (6.0 * input(index) - neighbours)
            x += 1
          y += 1
        z += 1

  object PeriodicHelmholtz:
    def apply(nx: Int, ny: Int, nz: Int, alpha: Double): PeriodicHelmholtz =
      new PeriodicHelmholtz(nx, ny, nz, alpha)

  def eigenvalue(side: Int, frequency: Int, alpha: Double): Double =
    1.0 + 2.0 * alpha * (1.0 - math.cos(2.0 * math.Pi * frequency.toDouble / side.toDouble))

  def fourierMixture(side: Int): DVec =
    val plane = side * side
    DVec.tabulate(side * plane): index =>
      val x = index % side
      val y = (index / side) % side
      val z = index / plane
      1.0 +
        0.2 * math.cos(2.0 * math.Pi * x.toDouble / side.toDouble) +
        0.1 * math.sin(4.0 * math.Pi * y.toDouble / side.toDouble) +
        0.05 * math.cos(6.0 * math.Pi * z.toDouble / side.toDouble)

  def inverseFourierMixture(side: Int, alpha: Double): DVec =
    val plane = side * side
    DVec.tabulate(side * plane): index =>
      val x = index % side
      val y = (index / side) % side
      val z = index / plane
      1.0 +
        0.2 * math.cos(2.0 * math.Pi * x.toDouble / side.toDouble) / eigenvalue(side, 1, alpha) +
        0.1 * math.sin(4.0 * math.Pi * y.toDouble / side.toDouble) / eigenvalue(side, 2, alpha) +
        0.05 * math.cos(6.0 * math.Pi * z.toDouble / side.toDouble) / eigenvalue(side, 3, alpha)

  def maximumFourierApplyError(side: Int, alpha: Double, actual: DVec): Double =
    val plane = side * side
    var maximum = 0.0
    var index = 0
    while index < actual.length do
      val x = index % side
      val y = (index / side) % side
      val z = index / plane
      val expected =
        1.0 +
          0.2 * eigenvalue(side, 1, alpha) * math.cos(2.0 * math.Pi * x.toDouble / side.toDouble) +
          0.1 * eigenvalue(side, 2, alpha) * math.sin(4.0 * math.Pi * y.toDouble / side.toDouble) +
          0.05 * eigenvalue(side, 3, alpha) * math.cos(6.0 * math.Pi * z.toDouble / side.toDouble)
      maximum = math.max(maximum, math.abs(actual(index) - expected))
      index += 1
    maximum

  def maximumDifference(left: DVec, right: DVec): Double =
    var maximum = 0.0
    var i = 0
    while i < left.length do
      maximum = math.max(maximum, math.abs(left(i) - right(i)))
      i += 1
    maximum

  def maximumConstantError(values: DVec, expected: Double): Double =
    var maximum = 0.0
    var i = 0
    while i < values.length do
      maximum = math.max(maximum, math.abs(values(i) - expected))
      i += 1
    maximum
