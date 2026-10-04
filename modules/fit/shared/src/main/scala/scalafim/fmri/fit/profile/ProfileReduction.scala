package scalafim.fmri.fit.profile

import gale.linalg.{DenseCholeskyWorkspace, DensePositiveDefiniteness}
import scalafim.fmri.hrf.family.JetLayout

/** Curvature status of a profile jet at its evaluation point. */
enum CurvatureStatus:
  case PositiveDefinite
  case Indefinite
  case GramNotPositiveDefinite
  case NumericalFailure

/** Energy, gradient, symmetric Hessian and amplitudes of the profiled energy
  * at one shape, in a chart of dimension `d`. Hessian is row-major `d x d`.
  */
final case class ProfileJet(
    energy: Double,
    gradient: Vector[Double],
    hessian: Vector[Double],
    amplitudes: Vector[Double],
    curvature: CurvatureStatus):
  def dimension: Int = gradient.length

/** Reusable, allocation-free buffer for [[ProfileReduction]] output. */
final class ProfileJetBuffer(val dimension: Int, val amplitudeCount: Int):
  var energy: Double = Double.NaN
  val gradient: Array[Double] = new Array[Double](dimension)
  val hessian: Array[Double] = new Array[Double](dimension * dimension)
  val amplitudes: Array[Double] = new Array[Double](amplitudeCount)
  var curvature: CurvatureStatus = CurvatureStatus.GramNotPositiveDefinite

  def toJet: ProfileJet =
    ProfileJet(energy, gradient.toVector, hessian.toVector, amplitudes.toVector, curvature)

/** The common second-order reduction `E = s - b' G^-1 b`.
  *
  * With `w = G^-1 b` and `r_p = b_p - G_p w`:
  * `E_p = s_p - 2 b_p' w + w' G_p w`,
  * `E_pq = s_pq - 2 b_pq' w + w' G_pq w - 2 r_p' G^-1 r_q`.
  *
  * Inputs are component-major jets in the [[JetLayout]] order: `s` has
  * `comps` scalars, `b` has `comps x C` entries and `g` has `comps x C x C`
  * row-major blocks. `s` carries the response energy, which is shape-free in
  * the condition regime and shape-dependent under a shape-dependent temporal
  * filter; omitting its derivatives is a correctness bug, so it is an input
  * rather than an assumption. Successful reductions, including indefinite
  * curvature, do not allocate after construction. Refused inputs may allocate
  * a Gale error value.
  */
final class ProfileReduction(val dimension: Int, val amplitudeCount: Int):
  require(dimension >= 1 && dimension <= 3, s"dimension must be 1..3, got $dimension")
  require(amplitudeCount >= 1, s"amplitudeCount must be >= 1, got $amplitudeCount")

  val components: Int = JetLayout.components(dimension)
  private val d = dimension
  private val c = amplitudeCount
  private val solver = new DenseCholeskyWorkspace(math.max(c, d))
  private val factor = new Array[Double](c * c)
  private val w = new Array[Double](c)
  private val r = new Array[Double](d * c)
  private val t = new Array[Double](d * c)
  private val tmp = new Array[Double](c)

  /** Reduce one shape. Returns false and clears the buffer for a non-SPD Gram
    * matrix or non-finite numerical result.
    */
  def reduce(s: Array[Double], b: Array[Double], g: Array[Double], out: ProfileJetBuffer): Boolean =
    System.arraycopy(g, 0, factor, 0, c * c)
    if solver.factorLowerInPlace(c, factor).isLeft then
      return invalidate(out, CurvatureStatus.GramNotPositiveDefinite)
    System.arraycopy(b, 0, w, 0, c)
    if solver.solveLowerInPlace(c, factor, w).isLeft then
      return invalidate(out, CurvatureStatus.NumericalFailure)
    var bw = 0.0
    var i = 0
    while i < c do
      bw += b(i) * w(i)
      i += 1
    out.energy = s(JetLayout.Value) - bw
    System.arraycopy(w, 0, out.amplitudes, 0, c)
    var p = 0
    while p < d do
      val comp = JetLayout.first(p)
      var grad = s(comp)
      i = 0
      while i < c do
        grad -= 2.0 * b(comp * c + i) * w(i)
        i += 1
      grad += quadratic(g, comp * c * c, w)
      out.gradient(p) = grad
      i = 0
      while i < c do
        var acc = b(comp * c + i)
        var j = 0
        while j < c do
          acc -= g(comp * c * c + i * c + j) * w(j)
          j += 1
        r(p * c + i) = acc
        tmp(i) = acc
        i += 1
      if solver.solveLowerInPlace(c, factor, tmp).isLeft then
        return invalidate(out, CurvatureStatus.NumericalFailure)
      System.arraycopy(tmp, 0, t, p * c, c)
      p += 1
    p = 0
    while p < d do
      var q = p
      while q < d do
        val comp = JetLayout.second(d, p, q)
        var h = s(comp)
        i = 0
        while i < c do
          h -= 2.0 * b(comp * c + i) * w(i)
          h -= 2.0 * r(p * c + i) * t(q * c + i)
          i += 1
        h += quadratic(g, comp * c * c, w)
        out.hessian(p * d + q) = h
        out.hessian(q * d + p) = h
        q += 1
      p += 1
    if !out.energy.isFinite || !finite(out.gradient) || !finite(out.hessian) then
      return invalidate(out, CurvatureStatus.NumericalFailure)
    out.curvature =
      if solver.testPositiveDefinite(d, out.hessian) == DensePositiveDefiniteness.PositiveDefinite then CurvatureStatus.PositiveDefinite
      else CurvatureStatus.Indefinite
    true

  private def finite(values: Array[Double]): Boolean =
    var i = 0
    while i < values.length do
      if !values(i).isFinite then return false
      i += 1
    true

  private def invalidate(out: ProfileJetBuffer, status: CurvatureStatus): Boolean =
    out.energy = Double.PositiveInfinity
    java.util.Arrays.fill(out.gradient, Double.NaN)
    java.util.Arrays.fill(out.hessian, Double.NaN)
    java.util.Arrays.fill(out.amplitudes, Double.NaN)
    out.curvature = status
    false

  private def quadratic(matrix: Array[Double], offset: Int, x: Array[Double]): Double =
    var acc = 0.0
    var i = 0
    while i < c do
      var j = 0
      while j < c do
        acc += x(i) * matrix(offset + i * c + j) * x(j)
        j += 1
      i += 1
    acc
