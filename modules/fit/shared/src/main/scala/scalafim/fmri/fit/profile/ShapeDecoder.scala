package scalafim.fmri.fit.profile

import gale.linalg.DenseCholeskyWorkspace

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

/** Per-voxel work caps; every cap is a counter with a reported actual.
  * `stationarityStepTolerance` is the largest raw free Newton correction (in
  * chart coordinates) that counts as a budget-qualified approximation rather
  * than requiring another candidate evaluation.
  */
final case class DecodeBudget(
    coarseStride: Int = 2,
    maxNewtonSteps: Int = 2,
    maxJets: Int = 2,
    maxExactEvaluations: Int = 6,
    weakSdLimit: Vector[Double] = Vector.empty,
    ambiguityEnergy: Double = 0.0,
    maxCandidateAttempts: Int = 4,
    stationarityStepTolerance: Double = 1e-9):
  require(
    coarseStride >= 1 && maxNewtonSteps >= 0 && maxJets >= 1 && maxExactEvaluations >= 0 &&
      maxCandidateAttempts >= 1 && stationarityStepTolerance > 0.0 && !stationarityStepTolerance.isInfinite
  )

final class DecoderCounters:
  var voxels: Long = 0L
  var nodeScores: Long = 0L
  var jets: Long = 0L
  var exactEvaluations: Long = 0L
  var candidateAttempts: Long = 0L
  var terminalVerifications: Long = 0L
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
  case NoAdmissibleNode
  /** Nonstationary correction has no finite representable feasible proposal. */
  case Stalled

enum DecodeBudgetExit:
  case CandidateAttemptCap
  case RemainingEvaluationQuota
  case AcceptedEnergyOnlyNonstationaryTerminal
  case AcceptedEnergyOnlyTerminalVerificationFailed
  case AcceptedEnergyOnlyTerminalJetQuota
  case NewtonStepCap
  case FallbackExactEvaluationQuota
  case FallbackTerminalJetQuota

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
    ambiguityGap: Double,
    budgetExit: Option[DecodeBudgetExit] = None):
  def point: ShapePoint = ShapePoint.unsafe(coordinates)

private enum NewtonDirectionStatus:
  case Stationary
  case Direction
  case CurvatureNotPositive
  case Stalled

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
  private val nodeObjectiveEnergy = new Array[Double](grid.count)
  private val jet = new ProfileJetBuffer(d, c)
  private val x = new Array[Double](d)
  private val trial = new Array[Double](d)
  private val delta = new Array[Double](d)
  private val grad = new Array[Double](d)
  private val hess = new Array[Double](d * d)
  private val candidateDelta = new Array[Double](d)
  private val candidateGrad = new Array[Double](d)
  private val candidateHess = new Array[Double](d * d)
  private val dataHess = new Array[Double](d * d)
  private val solver = new DenseCholeskyWorkspace(d)
  private val factor = new Array[Double](d * d)
  private val free = new Array[Boolean](d)
  private val rhs = new Array[Double](d)
  private val betaAccepted = new Array[Double](c)
  private val indices = new Array[Int](d)
  private val neighbour = new Array[Int](d)
  private val nodeScored = new Array[Boolean](grid.count)
  prior.foreach(p => require(p.dimension == d, "prior dimension must match the chart"))

  private def finite(value: Double): Boolean = !value.isNaN && !value.isInfinite

  private def finiteValues(values: Array[Double]): Boolean =
    var i = 0
    while i < values.length do
      if !finite(values(i)) then return false
      i += 1
    true

  private def clearJet(): Unit =
    jet.energy = Double.NaN
    java.util.Arrays.fill(jet.gradient, Double.NaN)
    java.util.Arrays.fill(jet.hessian, Double.NaN)
    java.util.Arrays.fill(jet.amplitudes, Double.NaN)
    jet.curvature = CurvatureStatus.GramNotPositiveDefinite

  private def finiteJet(): Boolean =
    finite(jet.energy) && finiteValues(jet.gradient) && finiteValues(jet.hessian) && finiteValues(jet.amplitudes)

  private def finiteEnergyEvaluation(value: Double): Boolean = finite(value) && finiteValues(jet.amplitudes)

  private def copyJetState(coords: Array[Double]): Unit =
    System.arraycopy(jet.amplitudes, 0, betaAccepted, 0, c)
    System.arraycopy(jet.gradient, 0, grad, 0, d)
    System.arraycopy(jet.hessian, 0, hess, 0, d * d)
    System.arraycopy(jet.hessian, 0, dataHess, 0, d * d)
    augment(coords, grad, hess)

  private def clearCurvature(): Unit =
    java.util.Arrays.fill(grad, Double.NaN)
    java.util.Arrays.fill(hess, Double.NaN)
    java.util.Arrays.fill(dataHess, Double.NaN)

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

  /** Add the prior's gradient and Hessian to the supplied jet at `coords`. */
  private def augment(coords: Array[Double], gradient: Array[Double], hessian: Array[Double]): Unit =
    prior.foreach { p =>
      var i = 0
      while i < d do
        var g = 0.0
        var j = 0
        while j < d do
          g += 2.0 * p.precision(i * d + j) * (coords(j) - p.mean(j))
          hessian(i * d + j) += 2.0 * p.precision(i * d + j)
          j += 1
        gradient(i) += g
        i += 1
    }

  private def scan(counters: DecoderCounters): Int =
    java.util.Arrays.fill(nodeEnergy, Double.NaN)
    java.util.Arrays.fill(nodeObjectiveEnergy, Double.NaN)
    java.util.Arrays.fill(nodeScored, false)
    var best = -1
    var bestE = Double.PositiveInfinity
    val stride = budget.coarseStride

    def evaluateNode(node: Int): Double =
      val data = objective.scoreNode(node)
      counters.nodeScores += 1
      nodeScored(node) = true
      nodeEnergy(node) = data
      grid.coordinatesInto(node, trial)
      val augmented = data + priorEnergy(trial)
      nodeObjectiveEnergy(node) = augmented
      augmented

    var node = 0
    while node < grid.count do
      grid.indicesInto(node, indices)
      var coarse = true
      var axis = 0
      while axis < d do
        if indices(axis) % stride != 0 then coarse = false
        axis += 1
      if coarse then
        val augmented = evaluateNode(node)
        if finite(augmented) && augmented < bestE then
          bestE = augmented
          best = node
      node += 1
    // A refused coarse sub-grid is not evidence that every bank node is
    // inadmissible. Complete the scan only on this failure path.
    if best < 0 && stride > 1 then
      node = 0
      while node < grid.count do
        if !nodeScored(node) then
          val augmented = evaluateNode(node)
          if finite(augmented) && augmented < bestE then
            bestE = augmented
            best = node
        node += 1
    if stride > 1 && best >= 0 then
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
          if !nodeScored(n) then
            val augmented = evaluateNode(n)
            if finite(augmented) && augmented < bestE then
              bestE = augmented
              best = n
        cell += 1
    best

  /** Energy gap between the best node and the best node outside its immediate neighbourhood. */
  private def ambiguity(best: Int): Double =
    grid.indicesInto(best, indices)
    var second = Double.PositiveInfinity
    var node = 0
    while node < grid.count do
      val e = nodeObjectiveEnergy(node)
      if finite(e) && node != best then
        grid.indicesInto(node, neighbour)
        var adjacent = true
        var axis = 0
        while axis < d do
          if math.abs(neighbour(axis) - indices(axis)) > 1 then adjacent = false
          axis += 1
        if !adjacent && e < second then second = e
      node += 1
    if second.isInfinite then Double.PositiveInfinity else second - nodeObjectiveEnergy(best)

  private def onBoundary(coords: Array[Double]): Boolean =
    var i = 0
    while i < d do
      if coords(i) <= grid.chart.lower(i) + 1e-12 || coords(i) >= grid.chart.upper(i) - 1e-12 then return true
      i += 1
    false

  /** Projected Newton direction on the box, separated from a constrained stationary point. */
  private def newtonDirection(
      coords: Array[Double], gradient: Array[Double], hessian: Array[Double], direction: Array[Double]
  ): NewtonDirectionStatus =
    var i = 0
    while i < d do
      val atLower = coords(i) <= grid.chart.lower(i) + 1e-12
      val atUpper = coords(i) >= grid.chart.upper(i) - 1e-12
      free(i) = !((atLower && gradient(i) > 0.0) || (atUpper && gradient(i) < 0.0))
      direction(i) = 0.0
      i += 1
    var nFree = 0
    i = 0
    while i < d do
      if free(i) then
        var j = 0
        var col = 0
        while j < d do
          if free(j) then
            factor(nFree * d + col) = hessian(i * d + j)
            col += 1
          j += 1
        rhs(nFree) = -gradient(i)
        nFree += 1
      i += 1
    if nFree == 0 then
      System.arraycopy(hessian, 0, factor, 0, d * d)
      return
        if solver.factorLowerInPlace(d, factor).isRight then NewtonDirectionStatus.Stationary
        else NewtonDirectionStatus.CurvatureNotPositive
    // Compact the active block in place; d <= 3.
    var r = 0
    while r < nFree do
      var col = 0
      while col < nFree do
        factor(r * nFree + col) = factor(r * d + col)
        col += 1
      r += 1
    if !solver.factorLowerInPlace(nFree, factor).isRight then return NewtonDirectionStatus.CurvatureNotPositive
    if solver.solveLowerInPlace(nFree, factor, rhs).isLeft then return NewtonDirectionStatus.Stalled
    var k = 0
    i = 0
    while i < d do
      if free(i) then
        direction(i) = rhs(k)
        k += 1
      i += 1
    var norm = 0.0
    i = 0
    while i < d do
      norm = math.max(norm, math.abs(direction(i)))
      i += 1
    if norm <= budget.stationarityStepTolerance then NewtonDirectionStatus.Stationary else NewtonDirectionStatus.Direction

  /** Probe a candidate without changing the accepted derivatives or search direction. */
  private def candidateStationary(coords: Array[Double]): Boolean =
    System.arraycopy(jet.gradient, 0, candidateGrad, 0, d)
    System.arraycopy(jet.hessian, 0, candidateHess, 0, d * d)
    augment(coords, candidateGrad, candidateHess)
    finiteValues(candidateGrad) && finiteValues(candidateHess) &&
      newtonDirection(coords, candidateGrad, candidateHess, candidateDelta) == NewtonDirectionStatus.Stationary

  private def conditionalSd(out: Array[Double]): Boolean =
    System.arraycopy(dataHess, 0, factor, 0, d * d)
    if !solver.factorLowerInPlace(d, factor).isRight then
      java.util.Arrays.fill(out, Double.NaN)
      return false
    var i = 0
    while i < d do
      java.util.Arrays.fill(rhs, 0.0)
      rhs(i) = 1.0
      if solver.solveLowerInPlace(d, factor, rhs).isLeft then
        java.util.Arrays.fill(out, Double.NaN)
        return false
      out(i) = math.sqrt(math.max(0.0, 2.0 * noiseVariance * rhs(i)))
      i += 1
    true

  /** Node energies from the last scan (`NaN` where unscanned); valid until the next decode. */
  def lastNodeEnergies: Array[Double] = nodeEnergy

  /** Data-plus-prior node energies used for selection and ambiguity. */
  def lastAugmentedNodeEnergies: Array[Double] = nodeObjectiveEnergy

  private def refused(best: Int, gap: Double): ShapeDecodeResult =
    if best >= 0 then grid.coordinatesInto(best, x)
    else java.util.Arrays.fill(x, Double.NaN)
    java.util.Arrays.fill(betaAccepted, Double.NaN)
    clearCurvature()
    ShapeDecodeResult(
      coordinates = x.toVector,
      energy = if best >= 0 && finite(nodeEnergy(best)) then nodeEnergy(best) else Double.PositiveInfinity,
      amplitudes = betaAccepted.toVector,
      status = DecodeStatus.NoAdmissibleNode,
      node = best,
      newtonSteps = 0,
      dataHessian = dataHess.toVector,
      augmentedHessian = hess.toVector,
      conditionalSd = Vector.fill(d)(Double.NaN),
      ambiguityGap = gap
    )

  def decode(counters: DecoderCounters): ShapeDecodeResult =
    counters.voxels += 1
    val best = scan(counters)
    if best < 0 then return refused(best, Double.NaN)
    val gap = ambiguity(best)
    grid.coordinatesInto(best, x)
    var jetsUsed = 0
    var exactUsed = 0
    var steps = 0
    var fallback = false
    var budgetExceeded = false
    var budgetExit: Option[DecodeBudgetExit] = None
    def exhaust(reason: DecodeBudgetExit): Unit =
      budgetExceeded = true
      if budgetExit.isEmpty then budgetExit = Some(reason)
    var terminalCurvature = true
    clearJet()
    val nodeOk = objective.jetAtNode(best, jet) && finiteJet()
    jetsUsed += 1
    counters.jets += 1
    if !nodeOk then return refused(best, gap)
    var current = jet.energy + priorEnergy(x)
    copyJetState(x)
    var directionStatus = newtonDirection(x, grad, hess, delta)
    val initialCurvatureNotPositive = directionStatus == NewtonDirectionStatus.CurvatureNotPositive
    var continue = directionStatus == NewtonDirectionStatus.Direction
    if continue && budget.maxNewtonSteps == 0 then exhaust(DecodeBudgetExit.NewtonStepCap)
    while continue && steps < budget.maxNewtonSteps do
      // Stationarity was checked on the raw free correction. Removing outward
      // bound components preserves descent for the SPD free Hessian; common
      // scaling alone can otherwise erase a nonstationary coupled direction.
      var usable = finiteValues(delta)
      var alpha = 1.0
      var i = 0
      while i < d do
        val atLower = x(i) <= grid.chart.lower(i) + 1e-12
        val atUpper = x(i) >= grid.chart.upper(i) - 1e-12
        if (atLower && delta(i) < 0.0) || (atUpper && delta(i) > 0.0) then delta(i) = 0.0
        if delta(i) > 0.0 then alpha = math.min(alpha, (grid.chart.upper(i) - x(i)) / delta(i))
        if delta(i) < 0.0 then alpha = math.min(alpha, (grid.chart.lower(i) - x(i)) / delta(i))
        i += 1
      usable = usable && finite(alpha) && alpha > 0.0
      i = 0
      while i < d do
        delta(i) *= alpha
        i += 1
      if !usable || !finiteValues(delta) then
        directionStatus = NewtonDirectionStatus.Stalled
        continue = false
      else
        var accepted = false
        var scale = 1.0
        var tries = 0
        while !accepted && tries < budget.maxCandidateAttempts && continue do
          i = 0
          while i < d do
            trial(i) = grid.chart.clamp(i, x(i) + scale * delta(i))
            i += 1
          var moved = false
          i = 0
          while i < d do
            if trial(i) != x(i) then moved = true
            i += 1
          if !moved || !finiteValues(trial) then
            // No objective call occurred, so this is not budget exhaustion.
            // Numeric equality also treats signed zeros as one coordinate.
            directionStatus = NewtonDirectionStatus.Stalled
            continue = false
          else
            // Keep one jet available to verify an accepted energy-only move. If
            // no energy-only evaluation remains, use that final jet directly for
            // this candidate; it then carries its own coherent terminal state.
            val useJet = jetsUsed < budget.maxJets - 1 ||
              (jetsUsed < budget.maxJets && exactUsed >= budget.maxExactEvaluations)
            if !useJet && exactUsed >= budget.maxExactEvaluations then
              exhaust(DecodeBudgetExit.RemainingEvaluationQuota)
              continue = false
            else
              counters.candidateAttempts += 1
              tries += 1
              val value =
                if useJet then
                  clearJet()
                  jetsUsed += 1
                  counters.jets += 1
                  if objective.jetAt(trial, jet) && finiteJet() then jet.energy + priorEnergy(trial) else Double.PositiveInfinity
                else
                  clearJet()
                  exactUsed += 1
                  counters.exactEvaluations += 1
                  val data = objective.energyAt(trial, jet)
                  if finiteEnergyEvaluation(data) then data + priorEnergy(trial) else Double.PositiveInfinity
              var equalStationary = false
              if finite(value) && value == current then
                if useJet then equalStationary = candidateStationary(trial)
                else if jetsUsed < budget.maxJets then
                  // An energy-only equality must be checked by its reserved full
                  // jet before committing any coordinates or accepted state.
                  clearJet()
                  jetsUsed += 1
                  counters.jets += 1
                  counters.terminalVerifications += 1
                  equalStationary = objective.jetAt(trial, jet) && finiteJet() &&
                    finite(jet.energy + priorEnergy(trial)) && jet.energy + priorEnergy(trial) == current &&
                    candidateStationary(trial)
              if finite(value) && (value < current || equalStationary) then
                accepted = true
                current = value
                System.arraycopy(trial, 0, x, 0, d)
                System.arraycopy(jet.amplitudes, 0, betaAccepted, 0, c)
                steps += 1
                counters.newtonSteps += 1
                if useJet || equalStationary then
                  copyJetState(x)
                  directionStatus = newtonDirection(x, grad, hess, delta)
                  continue = directionStatus == NewtonDirectionStatus.Direction
                else
                  terminalCurvature = false
                  continue = false
                  if jetsUsed < budget.maxJets then
                    clearJet()
                    jetsUsed += 1
                    counters.jets += 1
                    counters.terminalVerifications += 1
                    if objective.jetAt(x, jet) && finiteJet() then
                      current = jet.energy + priorEnergy(x)
                      copyJetState(x)
                      terminalCurvature = true
                      directionStatus = newtonDirection(x, grad, hess, delta)
                      if directionStatus == NewtonDirectionStatus.Direction then
                        exhaust(DecodeBudgetExit.AcceptedEnergyOnlyNonstationaryTerminal)
                    else exhaust(DecodeBudgetExit.AcceptedEnergyOnlyTerminalVerificationFailed)
                  else exhaust(DecodeBudgetExit.AcceptedEnergyOnlyTerminalJetQuota)
              else
                scale *= 0.5
        if !accepted && continue then
          exhaust(DecodeBudgetExit.CandidateAttemptCap)
          continue = false
    if directionStatus == NewtonDirectionStatus.Direction && steps >= budget.maxNewtonSteps then exhaust(DecodeBudgetExit.NewtonStepCap)
    if initialCurvatureNotPositive then
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
          val em = nodeObjectiveEnergy(grid.indexOf(neighbour))
          neighbour(i) = idx + 1
          val ep = nodeObjectiveEnergy(grid.indexOf(neighbour))
          val e0 = nodeObjectiveEnergy(best)
          val den = em - 2.0 * e0 + ep
          if finite(em) && finite(ep) && den > 0.0 then
            val h = grid.step(i)
            trial(i) = grid.chart.clamp(i, x(i) + math.max(-h, math.min(h, 0.5 * h * (em - ep) / den)))
            moved = true
          else trial(i) = x(i)
        else trial(i) = x(i)
        i += 1
      if moved && exactUsed < budget.maxExactEvaluations then
        counters.candidateAttempts += 1
        clearJet()
        exactUsed += 1
        counters.exactEvaluations += 1
        val data = objective.energyAt(trial, jet)
        val value = if finiteEnergyEvaluation(data) then data + priorEnergy(trial) else Double.PositiveInfinity
        if finite(value) && value < current then
          current = value
          System.arraycopy(trial, 0, x, 0, d)
          System.arraycopy(jet.amplitudes, 0, betaAccepted, 0, c)
          terminalCurvature = false
          if jetsUsed < budget.maxJets then
            clearJet()
            jetsUsed += 1
            counters.jets += 1
            counters.terminalVerifications += 1
            if objective.jetAt(x, jet) && finiteJet() then
              current = jet.energy + priorEnergy(x)
              copyJetState(x)
              terminalCurvature = true
            else clearCurvature()
          else exhaust(DecodeBudgetExit.FallbackTerminalJetQuota)
      else if moved then exhaust(DecodeBudgetExit.FallbackExactEvaluationQuota)
    if !terminalCurvature then clearCurvature()
    val sd = new Array[Double](d)
    val sdOk = terminalCurvature && conditionalSd(sd)
    if !sdOk then java.util.Arrays.fill(sd, Double.NaN)
    val weak =
      sdOk && budget.weakSdLimit.nonEmpty && (0 until d).exists(i => !(sd(i) <= budget.weakSdLimit(i)))
    val status =
      if budgetExceeded then DecodeStatus.BudgetExceeded
      else if directionStatus == NewtonDirectionStatus.Stalled then DecodeStatus.Stalled
      else if fallback || directionStatus == NewtonDirectionStatus.CurvatureNotPositive then DecodeStatus.CurvatureNotPositive
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
      ambiguityGap = gap,
      budgetExit = if status == DecodeStatus.BudgetExceeded then budgetExit else None
    )
