package scalafim.fmri.fit.profile

import scalafim.fmri.hrf.family.{ShapeChart, ShapePoint}

/** A regular grid of reference nodes over a chart, `d <= 3`, axis 0 fastest. */
final case class NodeGrid(chart: ShapeChart, nodesPerAxis: Vector[Int]):
  require(nodesPerAxis.length == chart.dimension, s"${nodesPerAxis.length} axes for a ${chart.dimension}-dimensional chart")
  require(nodesPerAxis.forall(_ >= 2), "every axis needs at least two nodes")

  val dimension: Int = chart.dimension
  val count: Int = nodesPerAxis.product

  def step(axis: Int): Double = chart.width(axis) / (nodesPerAxis(axis) - 1)

  def indexOf(indices: Array[Int]): Int =
    var index = 0
    var stride = 1
    var axis = 0
    while axis < dimension do
      index += indices(axis) * stride
      stride *= nodesPerAxis(axis)
      axis += 1
    index

  def indicesInto(node: Int, out: Array[Int]): Unit =
    var rest = node
    var axis = 0
    while axis < dimension do
      out(axis) = rest % nodesPerAxis(axis)
      rest /= nodesPerAxis(axis)
      axis += 1

  def coordinateOf(nodeIndexOnAxis: Int, axis: Int): Double =
    chart.lower(axis) + step(axis) * nodeIndexOnAxis

  def coordinatesInto(node: Int, out: Array[Double]): Unit =
    var rest = node
    var axis = 0
    while axis < dimension do
      out(axis) = coordinateOf(rest % nodesPerAxis(axis), axis)
      rest /= nodesPerAxis(axis)
      axis += 1

  def point(node: Int): ShapePoint =
    val out = new Array[Double](dimension)
    coordinatesInto(node, out)
    ShapePoint.unsafe(out.toVector)

/** What a backend must provide for one voxel: exact energies at bank nodes,
  * jets at nodes (from a precomputed bank) and at continuous shapes, and exact
  * energies with amplitudes. The objective is pointed at a voxel by the
  * backend before decoding; the decoder holds no response data.
  */
trait ShapeObjective:
  def grid: NodeGrid
  def amplitudeCount: Int
  /** Exact energy at bank node `node` (value only). */
  def scoreNode(node: Int): Double
  /** Full jet at bank node `node`; false when the Gram is not positive definite. */
  def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean
  /** Full jet at a continuous shape. */
  def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean
  /** Exact energy at a continuous shape, amplitudes into `out`. */
  def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double

/** Gaussian prior on chart coordinates in energy units: the decoder minimises
  * `E(theta) + (theta - mean)' precision (theta - mean)`, so `precision` is
  * the prior precision times the frozen noise variance.
  */
final case class ShapePrior(mean: Vector[Double], precision: Vector[Double]):
  def dimension: Int = mean.length
  require(precision.length == dimension * dimension, "precision must be d x d row-major")

/** Per-voxel work caps; every cap is a counter with a reported actual. */
final case class DecodeBudget(
    coarseStride: Int = 2,
    maxNewtonSteps: Int = 2,
    maxJets: Int = 2,
    maxExactEvaluations: Int = 6,
    weakSdLimit: Vector[Double] = Vector.empty,
    ambiguityEnergy: Double = 0.0):
  require(coarseStride >= 1 && maxNewtonSteps >= 0 && maxJets >= 1 && maxExactEvaluations >= 0)

final class DecoderCounters:
  var voxels: Long = 0L
  var nodeScores: Long = 0L
  var jets: Long = 0L
  var exactEvaluations: Long = 0L
  var newtonSteps: Long = 0L
  var fallbacks: Long = 0L
  def perVoxel(value: Long): Double = if voxels == 0L then 0.0 else value.toDouble / voxels

enum DecodeStatus:
  case Accepted
  case WeaklyIdentified
  case Boundary
  case CurvatureNotPositive
  case AmbiguousCells
  case BudgetExceeded

final case class ShapeDecodeResult(
    coordinates: Vector[Double],
    energy: Double,
    amplitudes: Vector[Double],
    status: DecodeStatus,
    node: Int,
    newtonSteps: Int,
    dataHessian: Vector[Double],
    augmentedHessian: Vector[Double],
    conditionalSd: Vector[Double],
    ambiguityGap: Double):
  def point: ShapePoint = ShapePoint.unsafe(coordinates)

/** The shared bounded decoder: a hierarchical scan of the node bank (coarse
  * sub-grid, then the fine neighbourhood of the coarse best), a jet at the
  * best node from the bank, then projected Newton steps on the box with every
  * step verified by an exact evaluation, a derivative-free parabolic fallback
  * when the node curvature is not positive definite, and statuses that keep
  * identification, boundary and approximation separate. Data-only and
  * prior-augmented curvature are both reported.
  */
final class ShapeDecoder(objective: ShapeObjective, budget: DecodeBudget, prior: Option[ShapePrior], noiseVariance: Double):
  private val grid = objective.grid
  private val d = grid.dimension
  private val c = objective.amplitudeCount
  private val nodeEnergy = new Array[Double](grid.count)
  private val jet = new ProfileJetBuffer(d, c)
  private val x = new Array[Double](d)
  private val trial = new Array[Double](d)
  private val delta = new Array[Double](d)
  private val grad = new Array[Double](d)
  private val hess = new Array[Double](d * d)
  private val dataHess = new Array[Double](d * d)
  private val free = new Array[Boolean](d)
  private val reduced = new Array[Double](d * d)
  private val rhs = new Array[Double](d)
  private val betaAccepted = new Array[Double](c)
  private val indices = new Array[Int](d)
  private val neighbour = new Array[Int](d)
  prior.foreach(p => require(p.dimension == d, "prior dimension must match the chart"))

  private def priorEnergy(coords: Array[Double]): Double =
    prior match
      case None => 0.0
      case Some(p) =>
        var acc = 0.0
        var i = 0
        while i < d do
          var j = 0
          while j < d do
            acc += (coords(i) - p.mean(i)) * p.precision(i * d + j) * (coords(j) - p.mean(j))
            j += 1
          i += 1
        acc

  /** Add the prior's gradient and Hessian to `grad`/`hess` at `coords`. */
  private def augment(coords: Array[Double]): Unit =
    prior.foreach { p =>
      var i = 0
      while i < d do
        var g = 0.0
        var j = 0
        while j < d do
          g += 2.0 * p.precision(i * d + j) * (coords(j) - p.mean(j))
          hess(i * d + j) += 2.0 * p.precision(i * d + j)
          j += 1
        grad(i) += g
        i += 1
    }

  private def scan(counters: DecoderCounters): Int =
    java.util.Arrays.fill(nodeEnergy, Double.NaN)
    var best = -1
    var bestE = Double.PositiveInfinity
    val stride = budget.coarseStride
    var node = 0
    while node < grid.count do
      grid.indicesInto(node, indices)
      var coarse = true
      var axis = 0
      while axis < d do
        if indices(axis) % stride != 0 then coarse = false
        axis += 1
      if coarse then
        val e = objective.scoreNode(node)
        counters.nodeScores += 1
        nodeEnergy(node) = e
        if e < bestE then
          bestE = e
          best = node
      node += 1
    if stride > 1 then
      grid.indicesInto(best, indices)
      // enumerate the (2 stride - 1)^d neighbourhood
      val span = 2 * stride - 1
      val cells = math.pow(span.toDouble, d.toDouble).toInt
      var cell = 0
      while cell < cells do
        var rest = cell
        var inside = true
        var axis = 0
        while axis < d do
          val offset = rest % span - (stride - 1)
          rest /= span
          val idx = indices(axis) + offset
          if idx < 0 || idx >= grid.nodesPerAxis(axis) then inside = false
          neighbour(axis) = idx
          axis += 1
        if inside then
          val n = grid.indexOf(neighbour)
          if nodeEnergy(n).isNaN then
            val e = objective.scoreNode(n)
            counters.nodeScores += 1
            nodeEnergy(n) = e
            if e < bestE then
              bestE = e
              best = n
        cell += 1
    best

  /** Energy gap between the best node and the best node outside its immediate neighbourhood. */
  private def ambiguity(best: Int): Double =
    grid.indicesInto(best, indices)
    var second = Double.PositiveInfinity
    var node = 0
    while node < grid.count do
      val e = nodeEnergy(node)
      if !e.isNaN && node != best then
        grid.indicesInto(node, neighbour)
        var adjacent = true
        var axis = 0
        while axis < d do
          if math.abs(neighbour(axis) - indices(axis)) > 1 then adjacent = false
          axis += 1
        if !adjacent && e < second then second = e
      node += 1
    if second.isInfinite then Double.PositiveInfinity else second - nodeEnergy(best)

  private def onBoundary(coords: Array[Double]): Boolean =
    var i = 0
    while i < d do
      if coords(i) <= grid.chart.lower(i) + 1e-12 || coords(i) >= grid.chart.upper(i) - 1e-12 then return true
      i += 1
    false

  /** Projected Newton direction on the box; false when the free block is not positive definite. */
  private def newtonDirection(coords: Array[Double]): Boolean =
    var i = 0
    while i < d do
      val atLower = coords(i) <= grid.chart.lower(i) + 1e-12
      val atUpper = coords(i) >= grid.chart.upper(i) - 1e-12
      free(i) = !((atLower && grad(i) > 0.0) || (atUpper && grad(i) < 0.0))
      delta(i) = 0.0
      i += 1
    var nFree = 0
    i = 0
    while i < d do
      if free(i) then
        var j = 0
        var col = 0
        while j < d do
          if free(j) then
            reduced(nFree * d + col) = hess(i * d + j)
            col += 1
          j += 1
        rhs(nFree) = -grad(i)
        nFree += 1
      i += 1
    if nFree == 0 then return false
    // pack the reduced matrix as nFree x nFree
    val packed = new Array[Double](nFree * nFree)
    var r = 0
    while r < nFree do
      var col = 0
      while col < nFree do
        packed(r * nFree + col) = reduced(r * d + col)
        col += 1
      r += 1
    if !SmallCholesky.factorInPlace(nFree, packed) then return false
    SmallCholesky.solveInPlace(nFree, packed, rhs)
    var k = 0
    i = 0
    while i < d do
      if free(i) then
        delta(i) = rhs(k)
        k += 1
      i += 1
    true

  private def conditionalSd(out: Array[Double]): Boolean =
    val copy = java.util.Arrays.copyOf(dataHess, d * d)
    if !SmallCholesky.factorInPlace(d, copy) then
      java.util.Arrays.fill(out, Double.NaN)
      return false
    var i = 0
    while i < d do
      java.util.Arrays.fill(rhs, 0.0)
      rhs(i) = 1.0
      SmallCholesky.solveInPlace(d, copy, rhs)
      out(i) = math.sqrt(math.max(0.0, 2.0 * noiseVariance * rhs(i)))
      i += 1
    true

  /** Node energies from the last scan (`NaN` where unscanned); valid until the next decode. */
  def lastNodeEnergies: Array[Double] = nodeEnergy

  def decode(counters: DecoderCounters): ShapeDecodeResult =
    counters.voxels += 1
    val best = scan(counters)
    val gap = ambiguity(best)
    grid.coordinatesInto(best, x)
    var jetsUsed = 0
    var exactUsed = 0
    var steps = 0
    var fallback = false
    val nodeOk = objective.jetAtNode(best, jet)
    jetsUsed += 1
    counters.jets += 1
    var current = if nodeOk then jet.energy + priorEnergy(x) else Double.PositiveInfinity
    System.arraycopy(jet.amplitudes, 0, betaAccepted, 0, c)
    System.arraycopy(jet.gradient, 0, grad, 0, d)
    System.arraycopy(jet.hessian, 0, hess, 0, d * d)
    System.arraycopy(jet.hessian, 0, dataHess, 0, d * d)
    augment(x)
    val curvatureOk = nodeOk && newtonDirection(x)
    var continue = curvatureOk
    while continue && steps < budget.maxNewtonSteps do
      // clip to the box by scaling the step
      var alpha = 1.0
      var i = 0
      while i < d do
        if delta(i) > 0.0 then alpha = math.min(alpha, (grid.chart.upper(i) - x(i)) / delta(i))
        if delta(i) < 0.0 then alpha = math.min(alpha, (grid.chart.lower(i) - x(i)) / delta(i))
        i += 1
      var norm = 0.0
      i = 0
      while i < d do
        delta(i) *= alpha
        norm = math.max(norm, math.abs(delta(i)))
        i += 1
      if norm <= 1e-9 then continue = false
      else
        val lastStep = steps == budget.maxNewtonSteps - 1
        var accepted = false
        var scale = 1.0
        var tries = 0
        while !accepted && tries < 4 && continue do
          i = 0
          while i < d do
            trial(i) = grid.chart.clamp(i, x(i) + scale * delta(i))
            i += 1
          val useJet = !lastStep && jetsUsed < budget.maxJets
          if !useJet && exactUsed >= budget.maxExactEvaluations then continue = false
          else
            val value =
              if useJet then
                jetsUsed += 1
                counters.jets += 1
                if objective.jetAt(trial, jet) then jet.energy + priorEnergy(trial) else Double.PositiveInfinity
              else
                exactUsed += 1
                counters.exactEvaluations += 1
                objective.energyAt(trial, jet) + priorEnergy(trial)
            if value < current then
              accepted = true
              current = value
              System.arraycopy(trial, 0, x, 0, d)
              System.arraycopy(jet.amplitudes, 0, betaAccepted, 0, c)
              if useJet then
                System.arraycopy(jet.gradient, 0, grad, 0, d)
                System.arraycopy(jet.hessian, 0, hess, 0, d * d)
                System.arraycopy(jet.hessian, 0, dataHess, 0, d * d)
                augment(x)
                if !newtonDirection(x) then continue = false
              else continue = false
            else
              scale *= 0.5
              tries += 1
        if accepted then
          steps += 1
          counters.newtonSteps += 1
        else continue = false
    if !curvatureOk && nodeOk then
      // derivative-free fallback: parabolic interpolation along each axis of the node grid
      fallback = true
      counters.fallbacks += 1
      grid.indicesInto(best, indices)
      var moved = false
      var i = 0
      while i < d do
        val n = grid.nodesPerAxis(i)
        val idx = indices(i)
        if idx > 0 && idx < n - 1 then
          System.arraycopy(indices, 0, neighbour, 0, d)
          neighbour(i) = idx - 1
          val em = nodeEnergy(grid.indexOf(neighbour))
          neighbour(i) = idx + 1
          val ep = nodeEnergy(grid.indexOf(neighbour))
          val e0 = nodeEnergy(best)
          val den = em - 2.0 * e0 + ep
          if !em.isNaN && !ep.isNaN && den > 0.0 then
            val h = grid.step(i)
            trial(i) = grid.chart.clamp(i, x(i) + math.max(-h, math.min(h, 0.5 * h * (em - ep) / den)))
            moved = true
          else trial(i) = x(i)
        else trial(i) = x(i)
        i += 1
      if moved && exactUsed < budget.maxExactEvaluations then
        exactUsed += 1
        counters.exactEvaluations += 1
        val value = objective.energyAt(trial, jet) + priorEnergy(trial)
        if value < current then
          current = value
          System.arraycopy(trial, 0, x, 0, d)
          System.arraycopy(jet.amplitudes, 0, betaAccepted, 0, c)
    val sd = new Array[Double](d)
    val sdOk = conditionalSd(sd)
    val weak =
      sdOk && budget.weakSdLimit.nonEmpty && (0 until d).exists(i => !(sd(i) <= budget.weakSdLimit(i)))
    val status =
      if !nodeOk then DecodeStatus.CurvatureNotPositive
      else if fallback then DecodeStatus.CurvatureNotPositive
      else if gap <= budget.ambiguityEnergy then DecodeStatus.AmbiguousCells
      else if onBoundary(x) then DecodeStatus.Boundary
      else if !sdOk || weak then DecodeStatus.WeaklyIdentified
      else DecodeStatus.Accepted
    ShapeDecodeResult(
      coordinates = x.toVector,
      energy = current - priorEnergy(x),
      amplitudes = betaAccepted.toVector,
      status = status,
      node = best,
      newtonSteps = steps,
      dataHessian = dataHess.toVector,
      augmentedHessian = hess.toVector,
      conditionalSd = sd.toVector,
      ambiguityGap = gap
    )
