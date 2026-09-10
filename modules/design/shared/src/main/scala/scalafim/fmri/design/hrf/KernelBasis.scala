package scalafim.fmri.design.hrf

import gale.linalg.DMat
import scalafim.fmri.hrf.{Hrf, HrfDescriptor, IntegrationPolicy, Lag, PositiveSeconds, ResponseBasis, Support}
import scalafim.fmri.hrf.family.{JetLayout, ParametricHrfFamily, ShapePoint}

enum KernelBasisError:
  case InvalidSpec(detail: String)
  case Spectral(detail: String)
  case BudgetExceeded(maxRank: Int, tolerance: Double, achieved: Double)

  def message: String =
    this match
      case InvalidSpec(detail) => s"invalid kernel basis specification: $detail"
      case Spectral(detail) => s"kernel basis SVD failed: $detail"
      case BudgetExceeded(maxRank, tolerance, achieved) =>
        s"no rank <= $maxRank reaches held-out relative value error $tolerance (best $achieved); refusing rather than relaxing"

/** How a kernel basis is compiled: the family, the fine lag grid, the shape
  * grid it is sampled on, the accuracy it must certify and its rank budget.
  */
final case class KernelBasisSpec(
    family: ParametricHrfFamily,
    fineStep: PositiveSeconds,
    nodesPerAxis: Vector[Int],
    tolerance: Double,
    maxRank: Int = 32,
    includeDerivatives: Boolean = true,
    heldOutPoints: Int = 300,
    seed: Long = 11L)

/** Held-out evidence for a compiled basis, per rank `1..maxRank`. Relative
  * errors are maxima over the held-out points; the tail bound is the maximum
  * relative squared-norm mass of the family kernel beyond the horizon.
  */
final case class KernelBasisCertificate(
    valueError: Vector[Double],
    firstDerivativeError: Vector[Double],
    secondDerivativeError: Vector[Double],
    tailEnergyBound: Double,
    heldOutPoints: Int,
    seed: Long):
  def rankFor(tolerance: Double): Option[Int] =
    val index = valueError.indexWhere(_ <= tolerance)
    if index < 0 then None else Some(index + 1)

final case class KernelBasisProvenance(
    family: String,
    chart: Vector[(String, Double, Double)],
    horizonSeconds: Double,
    fineStepSeconds: Double,
    nodesPerAxis: Vector[Int],
    includeDerivatives: Boolean,
    tolerance: Double,
    rank: Int,
    seed: Long):
  def canonical: String =
    val axes = chart.map { case (n, lo, hi) => s"$n:[$lo,$hi]" }.mkString(",")
    s"kernel-basis/v1|family=$family|chart=$axes|horizon=$horizonSeconds|step=$fineStepSeconds|nodes=${nodesPerAxis.mkString("x")}|derivatives=$includeDerivatives|tolerance=$tolerance|rank=$rank|seed=$seed"

/** A data-independent basis `Phi` for a parametric HRF family:
  * `h_theta ~= sum_j c_j(theta) phi_j`, with `c(theta)` and its parameter
  * jets obtained by projecting the family's analytic jets.
  *
  * The basis is the left singular subspace of the family (and, optionally, its
  * parameter derivatives) sampled over a shape grid, ordered by singular value
  * so truncations are nested. [[kernel]] exposes it as an `m`-column [[Hrf]]
  * (linear interpolation between fine samples, exact on the grid) and
  * [[responseBasis]] as a [[ResponseBasis]], so the ordinary design and fit
  * machinery can consume it unchanged. The certificate records what the basis
  * was checked against; it is not a guarantee about shapes outside the chart.
  */
final class HrfKernelBasis private (
    val spec: KernelBasisSpec,
    val lags: Array[Double],
    private val phi: Array[Double],
    val rank: Int,
    val singularValues: Vector[Double],
    val certificate: KernelBasisCertificate):

  def family: ParametricHrfFamily = spec.family
  def fineCount: Int = lags.length

  val provenance: KernelBasisProvenance =
    KernelBasisProvenance(
      family = family.name,
      chart = family.chart.names.indices.map(i => (family.chart.names(i), family.chart.lower(i), family.chart.upper(i))).toVector,
      horizonSeconds = family.horizon.value,
      fineStepSeconds = spec.fineStep.value,
      nodesPerAxis = spec.nodesPerAxis,
      includeDerivatives = spec.includeDerivatives,
      tolerance = spec.tolerance,
      rank = rank,
      seed = spec.seed
    )

  /** Basis value `phi_j(lags(i))`. */
  def value(j: Int, i: Int): Double = phi(j * lags.length + i)

  /** `c(theta)` into `out(0 until rank)`; `kernelScratch` has length `fineCount`. */
  def coefficientsInto(point: ShapePoint, kernelScratch: Array[Double], out: Array[Double]): Unit =
    family.evalInto(lags, point, kernelScratch)
    projectInto(kernelScratch, 0, out, 0)

  /** Coefficient jet, component-major `out(component * rank + j)`, for the first
    * `components` jet components (`1`, `1 + d`, or all); `kernelScratch` has
    * length `family.jetComponents * fineCount`.
    */
  def coefficientJetInto(point: ShapePoint, kernelScratch: Array[Double], out: Array[Double], components: Int): Unit =
    require(components >= 1 && components <= family.jetComponents, s"components $components outside 1..${family.jetComponents}")
    family.jetInto(lags, point, kernelScratch)
    var comp = 0
    while comp < components do
      projectInto(kernelScratch, comp * lags.length, out, comp * rank)
      comp += 1

  private def projectInto(source: Array[Double], sourceOffset: Int, out: Array[Double], outOffset: Int): Unit =
    val n = lags.length
    var j = 0
    while j < rank do
      val base = j * n
      var acc = 0.0
      var i = 0
      while i < n do
        acc += phi(base + i) * source(sourceOffset + i)
        i += 1
      out(outOffset + j) = acc
      j += 1

  /** `Phi' c` into `out(0 until fineCount)`. */
  def reconstructInto(coefficients: Array[Double], out: Array[Double]): Unit =
    val n = lags.length
    java.util.Arrays.fill(out, 0, n, 0.0)
    var j = 0
    while j < rank do
      val base = j * n
      val c = coefficients(j)
      var i = 0
      while i < n do
        out(i) += phi(base + i) * c
        i += 1
      j += 1

  /** The basis as an `m`-column causal kernel with compact support on the horizon. */
  val kernel: Hrf =
    val n = lags.length
    val dt = spec.fineStep.value
    val horizon = family.horizon.seconds
    val name = s"kernel-basis:${family.name}"
    val descriptor = HrfDescriptor.derived(name, rank, horizon, integration = IntegrationPolicy.Quadrature)
    Hrf.multi(name, rank, horizon, Some(descriptor), Support.Compact(horizon)) { lag =>
      val x = lag.value / dt
      val i0 = math.min(n - 1, math.max(0, math.floor(x).toInt))
      val i1 = math.min(n - 1, i0 + 1)
      val frac = math.min(1.0, math.max(0.0, x - i0))
      val out = new Array[Double](rank)
      var j = 0
      while j < rank do
        out(j) = (1.0 - frac) * phi(j * n + i0) + frac * phi(j * n + i1)
        j += 1
      out
    }

  lazy val responseBasis: ResponseBasis = ResponseBasis.of(kernel)

object HrfKernelBasis:

  def compile(spec: KernelBasisSpec): Either[KernelBasisError, HrfKernelBasis] =
    val family = spec.family
    val d = family.dimension
    if spec.nodesPerAxis.length != d then Left(KernelBasisError.InvalidSpec(s"nodesPerAxis has ${spec.nodesPerAxis.length} entries for a $d-dimensional chart"))
    else if spec.nodesPerAxis.exists(_ < 2) then Left(KernelBasisError.InvalidSpec("every axis needs at least two nodes"))
    else if !(spec.tolerance > 0.0 && spec.tolerance < 1.0) then Left(KernelBasisError.InvalidSpec(s"tolerance must lie in (0, 1), got ${spec.tolerance}"))
    else if spec.maxRank < 1 then Left(KernelBasisError.InvalidSpec(s"maxRank must be >= 1, got ${spec.maxRank}"))
    else if spec.fineStep.value >= family.horizon.value then Left(KernelBasisError.InvalidSpec("fineStep must be smaller than the family horizon"))
    else if spec.heldOutPoints < 1 then Left(KernelBasisError.InvalidSpec("heldOutPoints must be >= 1"))
    else
      val dt = spec.fineStep.value
      val n = math.floor(family.horizon.value / dt).toInt + 1
      val lags = Array.tabulate(n)(i => i * dt)
      val comps = if spec.includeDerivatives then family.jetComponents else 1
      val gridPoints = spec.nodesPerAxis.product
      val builder = DMat.newBuilder(n, gridPoints * comps)
      val scratch = new Array[Double](family.jetComponents * n)
      var col = 0
      var g = 0
      while g < gridPoints do
        // mixed-radix decode of the grid index into one node per axis
        var rest = g
        val coordinates = Vector.newBuilder[Double]
        var axis = 0
        while axis < d do
          val nodes = spec.nodesPerAxis(axis)
          val index = rest % nodes
          rest /= nodes
          coordinates += family.chart.lower(axis) + family.chart.width(axis) * index / (nodes - 1)
          axis += 1
        val point = ShapePoint.unsafe(coordinates.result())
        if spec.includeDerivatives then family.jetInto(lags, point, scratch) else family.evalInto(lags, point, scratch)
        var comp = 0
        while comp < comps do
          var norm = 0.0
          var i = 0
          while i < n do
            val x = scratch(comp * n + i)
            norm += x * x
            i += 1
          val scale = if norm > 0.0 then 1.0 / math.sqrt(norm) else 0.0
          i = 0
          while i < n do
            builder.update(i, col, scratch(comp * n + i) * scale)
            i += 1
          col += 1
          comp += 1
        g += 1
      builder.result().svd match
        case Left(err) => Left(KernelBasisError.Spectral(err.getMessage))
        case Right(svd) =>
          val available = math.min(spec.maxRank, math.min(svd.u.cols, svd.singularValues.length))
          val phiFull = new Array[Double](available * n)
          var j = 0
          while j < available do
            var i = 0
            while i < n do
              phiFull(j * n + i) = svd.u(i, j)
              i += 1
            j += 1
          val certificate = certify(family, lags, phiFull, available, spec)
          certificate.rankFor(spec.tolerance) match
            case None =>
              Left(KernelBasisError.BudgetExceeded(spec.maxRank, spec.tolerance, certificate.valueError.lastOption.getOrElse(1.0)))
            case Some(rank) =>
              val phi = java.util.Arrays.copyOf(phiFull, rank * n)
              val sv = Vector.tabulate(rank)(j => svd.singularValues(j))
              Right(new HrfKernelBasis(spec, lags, phi, rank, sv, certificate))

  private def certify(family: ParametricHrfFamily, lags: Array[Double], phi: Array[Double], ranks: Int, spec: KernelBasisSpec): KernelBasisCertificate =
    val rng = new scala.util.Random(spec.seed)
    val n = lags.length
    val d = family.dimension
    val comps = family.jetComponents
    val value = new Array[Double](ranks)
    val first = new Array[Double](ranks)
    val second = new Array[Double](ranks)
    val jet = new Array[Double](comps * n)
    val coeff = new Array[Double](ranks)
    var tail = 0.0
    var p = 0
    while p < spec.heldOutPoints do
      val coordinates = Vector.tabulate(d)(axis => family.chart.lower(axis) + rng.nextDouble() * family.chart.width(axis))
      val point = ShapePoint.unsafe(coordinates)
      tail = math.max(tail, family.tailRelativeEnergy(point, spec.fineStep))
      family.jetInto(lags, point, jet)
      var comp = 0
      while comp < comps do
        var norm = 0.0
        var i = 0
        while i < n do
          val x = jet(comp * n + i)
          norm += x * x
          i += 1
        var j = 0
        while j < ranks do
          var acc = 0.0
          i = 0
          while i < n do
            acc += phi(j * n + i) * jet(comp * n + i)
            i += 1
          coeff(j) = acc
          j += 1
        val target = if comp == JetLayout.Value then value else if comp <= d then first else second
        var residual = norm
        j = 0
        while j < ranks do
          residual -= coeff(j) * coeff(j)
          val rel = if norm > 0.0 then math.sqrt(math.max(0.0, residual) / norm) else 0.0
          if rel > target(j) then target(j) = rel
          j += 1
        comp += 1
      p += 1
    KernelBasisCertificate(value.toVector, first.toVector, second.toVector, tail, spec.heldOutPoints, spec.seed)
