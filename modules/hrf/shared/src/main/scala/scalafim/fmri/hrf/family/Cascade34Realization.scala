package scalafim.fmri.hrf.family

import scalafim.fmri.hrf.{Cascade34Error, Cascade34Params, NonNegativeSeconds}

/** Errors from constructing the exact state-space realization of [[Cascade34Family]]. */
enum Cascade34RealizationError:
  case Point(error: ShapeChartError)
  case Parameters(error: Cascade34Error)

  def message: String =
    this match
      case Point(error) => error.message
      case Parameters(error) => error.message

/** Exact point-observation / impulse realization of [[Cascade34Family]].
  *
  * The realization shares the trial amplitude between its positive and
  * undershoot cascades. Transition and vector jets use `JetLayout(3)` in
  * component order `value,a,b,r,aa,ab,ar,bb,br,rr`, with component `c` at
  * offset `c * MatrixSize` or `c * StateCount`. There is no horizon
  * truncation, box observation, covariance, scheduling, or backend API.
  */
final class Cascade34Realization private (
    val family: Cascade34Family,
    val point: ShapePoint,
    val kappaP: Double,
    val kappaU: Double,
    val rho: Double,
    private val q: Double,
    private val v: Double
):
  import Cascade34Realization.*

  /** Writes the row-major 7 by 7 exact transition for `delta`. */
  def transitionInto(delta: NonNegativeSeconds, out: Array[Double]): Unit =
    validateDuration(delta)
    requireCapacity(out, MatrixSize, "transition")
    zero(out, MatrixSize)
    val xp = kappaP * delta.value
    val xu = kappaU * delta.value
    writeValueBlock(out, 0, 3, xp)
    writeValueBlock(out, 3, 4, xu)

  /** Writes the component-major second-order transition jet. */
  def transitionJetInto(delta: NonNegativeSeconds, out: Array[Double]): Unit =
    validateDuration(delta)
    requireCapacity(out, JetComponents * MatrixSize, "transition jet")
    zero(out, JetComponents * MatrixSize)
    writePositiveJet(out, kappaP * delta.value)
    writeUndershootJet(out, kappaU * delta.value)

  /** Writes the impulse injection vector in state order `p1,p2,p3,z1,z2,z3,z4`. */
  def injectionInto(out: Array[Double]): Unit =
    requireCapacity(out, StateCount, "injection")
    zero(out, StateCount)
    out(0) = kappaP
    out(3) = kappaU

  /** Writes the component-major second-order impulse-injection jet. */
  def injectionJetInto(out: Array[Double]): Unit =
    requireCapacity(out, JetComponents * StateCount, "injection jet")
    zero(out, JetComponents * StateCount)
    out(0) = kappaP
    out(StateCount) = kappaP
    out(4 * StateCount) = kappaP
    out(3) = kappaU
    out(StateCount + 3) = kappaU
    out(2 * StateCount + 3) = kappaU * v
    out(4 * StateCount + 3) = kappaU
    out(5 * StateCount + 3) = kappaU * v
    out(7 * StateCount + 3) = kappaU * v * (v - q)

  /** Writes the point-observation row. */
  def observationInto(out: Array[Double]): Unit =
    requireCapacity(out, StateCount, "observation")
    zero(out, StateCount)
    out(2) = 1.0
    out(6) = -rho

  /** Writes the component-major second-order point-observation jet. */
  def observationJetInto(out: Array[Double]): Unit =
    requireCapacity(out, JetComponents * StateCount, "observation jet")
    zero(out, JetComponents * StateCount)
    out(2) = 1.0
    out(6) = -rho
    out(3 * StateCount + 6) = -1.0

  private def writeValueBlock(out: Array[Double], state: Int, size: Int, x: Double): Unit =
    val f0 = weight(0, x)
    val f1 = weight(1, x)
    val f2 = weight(2, x)
    val f3 = weight(3, x)
    var row = 0
    while row < size do
      var col = 0
      while col <= row do
        out((state + row) * StateCount + state + col) =
          row - col match
            case 0 => f0
            case 1 => f1
            case 2 => f2
            case _ => f3
        col += 1
      row += 1

  private def writePositiveJet(out: Array[Double], x: Double): Unit =
    val f0 = weight(0, x)
    val f1 = weight(1, x)
    val f2 = weight(2, x)
    val f3 = weight(3, x)
    val f4 = weight(4, x)
    val e0 = -f1 + 2.0 * f2
    val e1 = f1 - 6.0 * f2 + 6.0 * f3
    val e2 = 4.0 * f2 - 15.0 * f3 + 12.0 * f4
    writeJetBlock(out, 0, 3, f0, f1, f2, 0.0, -f1, f1 - 2.0 * f2, 2.0 * f2 - 3.0 * f3, 0.0, e0, e1, e2, 0.0)

  private def writeUndershootJet(out: Array[Double], x: Double): Unit =
    val f0 = weight(0, x)
    val f1 = weight(1, x)
    val f2 = weight(2, x)
    val f3 = weight(3, x)
    val f4 = weight(4, x)
    val f5 = weight(5, x)
    val d0 = -f1
    val d1 = f1 - 2.0 * f2
    val d2 = 2.0 * f2 - 3.0 * f3
    val d3 = 3.0 * f3 - 4.0 * f4
    val e0 = -f1 + 2.0 * f2
    val e1 = f1 - 6.0 * f2 + 6.0 * f3
    val e2 = 4.0 * f2 - 15.0 * f3 + 12.0 * f4
    val e3 = 9.0 * f3 - 28.0 * f4 + 20.0 * f5
    writeJetBlock(out, 3, 4, f0, f1, f2, f3, d0, d1, d2, d3, e0, e1, e2, e3)

  private def writeJetBlock(
      out: Array[Double], state: Int, size: Int,
      f0: Double, f1: Double, f2: Double, f3: Double,
      d0: Double, d1: Double, d2: Double, d3: Double,
      e0: Double, e1: Double, e2: Double, e3: Double
  ): Unit =
    var row = 0
    while row < size do
      var col = 0
      while col <= row do
        val offset = row - col
        val value = offset match
          case 0 => f0
          case 1 => f1
          case 2 => f2
          case _ => f3
        val a = offset match
          case 0 => d0
          case 1 => d1
          case 2 => d2
          case _ => d3
        val aa = offset match
          case 0 => e0
          case 1 => e1
          case 2 => e2
          case _ => e3
        val index = (state + row) * StateCount + state + col
        out(index) = value
        if state == 0 then
          out(MatrixSize + index) = a
          out(4 * MatrixSize + index) = aa
        else
          out(MatrixSize + index) = a
          out(2 * MatrixSize + index) = v * a
          out(4 * MatrixSize + index) = aa
          out(5 * MatrixSize + index) = v * aa
          out(7 * MatrixSize + index) = v * v * aa - q * v * a
        col += 1
      row += 1

  private def validateDuration(delta: NonNegativeSeconds): Unit =
    require(delta.value.isFinite && delta.value >= 0.0, s"transition duration must be finite and >= 0, got ${delta.value}")

object Cascade34Realization:
  val StateCount: Int = 7
  val MatrixSize: Int = StateCount * StateCount
  val JetComponents: Int = JetLayout.components(3)

  def make(family: Cascade34Family, point: ShapePoint): Either[Cascade34RealizationError, Cascade34Realization] =
    family.chart.point(point.coordinates) match
      case Left(error) => Left(Cascade34RealizationError.Point(error))
      case Right(validPoint) =>
        val p = math.exp(validPoint(0))
        val q = Cascade34Family.logistic(validPoint(1))
        val u = p * q
        val rho = validPoint(2)
        Cascade34Params.make(p, u, rho) match
          case Left(error) => Left(Cascade34RealizationError.Parameters(error))
          case Right(_) => Right(new Cascade34Realization(family, validPoint, p, u, rho, q, Cascade34Family.logistic(-validPoint(1))))

  private val LogFactorial0 = 0.0
  private val LogFactorial1 = 0.0
  private val LogFactorial2 = math.log(2.0)
  private val LogFactorial3 = math.log(6.0)
  private val LogFactorial4 = math.log(24.0)
  private val LogFactorial5 = math.log(120.0)

  private def weight(order: Int, x: Double): Double =
    if x >= 1024.0 then 0.0
    else if x < 700.0 then
      val f0 = math.exp(-x)
      order match
        case 0 => f0
        case 1 => f0 * x
        case 2 => f0 * x * x / 2.0
        case 3 => f0 * x * x * x / 6.0
        case 4 => f0 * x * x * x * x / 24.0
        case _ => f0 * x * x * x * x * x / 120.0
    else
      val logFactorial = order match
        case 0 => LogFactorial0
        case 1 => LogFactorial1
        case 2 => LogFactorial2
        case 3 => LogFactorial3
        case 4 => LogFactorial4
        case _ => LogFactorial5
      math.exp(-x + order * math.log(x) - logFactorial)

  private def zero(out: Array[Double], length: Int): Unit =
    var i = 0
    while i < length do
      out(i) = 0.0
      i += 1

  private def requireCapacity(out: Array[Double], required: Int, label: String): Unit =
    require(out.length >= required, s"$label output requires at least $required elements, got ${out.length}")
