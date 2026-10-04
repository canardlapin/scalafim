package scalafim.fmri.laws.profile

import gale.linalg.DMat
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.fmri.design.event.{Event, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis, KernelBasisSpec}
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, NormalizationRule, ShapePoint}

object ConditionC0QualificationHarness:
  val Rows = 600
  val MaxBlock = 256
  val Baseline = DecodeBudget(2, 6, 8, 2, Vector(0.5, 1.0), stationarityStepTolerance = 1e-9, maxCandidateAttempts = 4)
  val Candidate = Baseline.copy(maxJets = 12, maxCandidateAttempts = 8)

  val BaselineLabel = "gaussian-d2-6n-8j-2e-4c"
  val CandidateLabel = "gaussian-d2-6n-12j-2e-8c"
  val FullCohort = 200
  val MainRefinementLevels = 14
  val DenseRefinementLevels = 16
  private val fixtureStarted = System.nanoTime()

  final case class PreparationTiming(basis: Long, expansion: Long, certification: Long, preparation: Long)
  private var preparationTiming = PreparationTiming(0L, 0L, 0L, 0L)
  private var preparationReady = false

  final case class Receipt(
      label: String, voxels: Int, blockSize: Int, preparationReused: Boolean, preparationTiming: PreparationTiming,
      fixtureNanos: Long, setupNanos: Long, allocationNanos: Long, inputNanos: Long, whitenNanos: Long,
      gatherNanos: Long, fitNanos: Long, sinkNanos: Long, totalNanos: Long,
      accepted: Long, statuses: Map[String, Long], budgetExits: Map[String, Long], counters: DecoderCounters,
      inputChecksum: Long, outputChecksum: Long, endHeapBytes: Option[Long], maxHeapBytes: Option[Long],
      rawBlockBytes: Long, whitenedBlockBytes: Long, gatherBytes: Long, emittedFloat32Bytes: Long,
      retainedFloat32Bytes: Long, float32Failures: Long, perVoxel: Option[Vector[VoxelReceipt]])

  final case class VoxelReceipt(
      sourceId: String, sampleId: Int, status: DecodeStatus, budgetExit: Option[DecodeBudgetExit],
      coordinates: Vector[Float], amplitudes: Vector[Float], work: DecoderWork, finite: Boolean, roundtrip: Boolean)

  private val frame = SamplingFrame(blockLens = Seq(Rows), tr = Seq(1.0))
  private val precision = Seconds(0.1)
  private val fineStep = PositiveSeconds(0.1).fold(e => throw IllegalArgumentException(e.message), identity)
  private val whitening = WhiteningPlan.global(ArmaCoefficients.ar(0.3), Vector(TimeSegment(0, Rows, 0)))
  private val nuisance = DMat.tabulate(Rows, 6): (t, j) =>
    val x = 2.0 * t / (Rows - 1.0) - 1.0
    j match
      case 0 => 1.0
      case 1 => x
      case 2 => x * x - 1.0 / 3.0
      case k => math.cos(math.Pi * (k - 2) * (t + 0.5) / Rows)
  private val schedule =
    val rng = new scala.util.Random(20260910L)
    EventTerm(
      events = Vector(Event.factor(Vector.tabulate(300)(i => Vector("A", "B", "C")(i % 3)), "cond")),
      onsets = Vector.fill(300)(rng.nextInt((Rows - 24) * 10) / 10.0).sorted.map(Seconds(_)),
      blockIds = Vector.fill(300)(0), termTag = Some("cond")
    )

  private val fixtureNanos = System.nanoTime() - fixtureStarted

  lazy val preparation: CompactConditionPreparation =
    val t0 = System.nanoTime()
    val basis = HrfKernelBasis.compile(KernelBasisSpec(GaussianFamily.Default, fineStep, Vector(26, 21), 1e-3, 48)).fold(e => throw IllegalArgumentException(e.message), identity)
    val t1 = System.nanoTime()
    val expanded = ExpandedConditionDesign.lower(schedule, frame, basis, precision).fold(e => throw IllegalArgumentException(e.message), identity)
    val t2 = System.nanoTime()
    val admission = ObservedFamilyCertification.admitForCompact(
      expanded, schedule, frame, precision, Some(whitening), Some(nuisance),
      Vector(ShapePoint.unsafe(Vector(5.25, math.log(1.55)))), ObservedFamilyRequirements(1e-2, 1e8, 1e-6)
    ).fold(e => throw IllegalArgumentException(e.message), identity)
    val t3 = System.nanoTime()
    val prep = CompactConditionPreparation.prepare(expanded, admission, Some(whitening), Some(nuisance), schedule, frame, precision)
      .fold(e => throw IllegalArgumentException(e.message), identity)
    preparationTiming = PreparationTiming(t1 - t0, t2 - t1, t3 - t2, System.nanoTime() - t3)
    preparationReady = true
    prep

  private def mix(hash: Long, value: Double): Long =
    java.lang.Long.rotateLeft(hash, 5) ^ java.lang.Double.doubleToRawLongBits(value)

  private def fill(
      raw: Array[Double], width: Int, start: Int, rng: scala.util.Random, snr: Double, expanded: ExpandedConditionDesign, initialChecksum: Long
  ): Long =
    val chart = GaussianFamily.Default.chart
    val nuisanceData = new Array[Double](Rows * 6)
    nuisance.copyRowMajorTo(nuisanceData)
    val signal = new Array[Double](Rows)
    var checksum = initialChecksum
    var local = 0
    while local < width do
      val point = ShapePoint.unsafe(Vector(
        chart.lower(0) + 0.5 + rng.nextDouble() * (chart.width(0) - 1.0),
        chart.lower(1) + 0.2 + rng.nextDouble() * (chart.width(1) - 0.4)
      ))
      val beta = Array.fill(3)((if rng.nextBoolean() then 1.0 else -1.0) * (0.5 + 1.5 * rng.nextDouble()))
      val design = expanded.designAt(point).data
      var sum = 0.0
      var sum2 = 0.0
      var row = 0
      while row < Rows do
        var value = 0.0
        var c = 0
        while c < 3 do
          value += design(row * 3 + c) * beta(c)
          c += 1
        signal(row) = value
        sum += value
        sum2 += value * value
        row += 1
      val sd = math.sqrt(math.max(1e-12, sum2 / Rows - (sum / Rows) * (sum / Rows)))
      val noiseSd = sd / snr
      val innovation = noiseSd * math.sqrt(1.0 - 0.09)
      var noise = rng.nextGaussian() * noiseSd
      row = 0
      while row < Rows do
        if row > 0 then noise = 0.3 * noise + rng.nextGaussian() * innovation
        var drift = 0.0
        var c = 0
        while c < 6 do
          drift += nuisanceData(row * 6 + c) * (if c == 0 then 10.0 else 0.3) * sd
          c += 1
        val value = signal(row) + noise + drift
        raw(row * width + local) = value
        checksum = mix(checksum ^ (start + local).toLong, value)
        row += 1
      local += 1
    checksum

  private def whitenColumns(cols: Int, rows: Array[Double]): Array[Double] =
    preparation.whiten(cols, rows).fold(e => throw IllegalArgumentException(e.message), identity)

  private def projectNuisance(cols: Int, whitened: Array[Double]): Array[Double] =
    val f = preparation.nuisanceRank
    val out = whitened.clone()
    var column = 0
    while column < cols do
      val qy = new Array[Double](f)
      var row = 0
      while row < Rows do
        var i = 0
        while i < f do
          qy(i) += preparation.qF(row * f + i) * whitened(row * cols + column)
          i += 1
        row += 1
      row = 0
      while row < Rows do
        var removed = 0.0
        var i = 0
        while i < f do
          removed += preparation.qF(row * f + i) * qy(i)
          i += 1
        out(row * cols + column) -= removed
        row += 1
      column += 1
    out

  private def directDesign(coords: Vector[Double]): Array[Double] =
    val point = ShapePoint.unsafe(coords)
    val scale = new Array[Double](GaussianFamily.Default.jetComponents)
    GaussianFamily.Default.scaleJetInto(GaussianFamily.Default.libraryNormalization, point, scale)
    val raw = schedule.convolve(GaussianFamily.Default.toHrf(point), frame, precision = precision).data
    projectNuisance(raw.cols, whitenColumns(raw.cols, raw.data.map(_ / scale(0))))

  private final case class DirectGeometry(design: Array[Double], factor: gale.linalg.Cholesky)

  private def directGeometry(coords: Vector[Double]): DirectGeometry =
    val design = directDesign(coords)
    val normal = DMat.tabulate(3, 3): (row, col) =>
      var value = 0.0
      var time = 0
      while time < Rows do
        value += design(time * 3 + row) * design(time * 3 + col)
        time += 1
      value
    DirectGeometry(design, normal.cholesky.fold(error => throw error, identity))

  private def leastSquares(geometry: DirectGeometry, y: Array[Double]): (Double, Array[Double]) =
    val rhs = DMat.tabulate(3, 1): (condition, _) =>
      var value = 0.0
      var time = 0
      while time < Rows do
        value += geometry.design(time * 3 + condition) * y(time)
        time += 1
      value
    val coefficients = geometry.factor.solve(rhs).fold(error => throw error, identity)
    val out = Array.tabulate(3)(coefficients(_, 0))
    var energy = 0.0
    var row = 0
    while row < Rows do
      var fitted = 0.0
      var c = 0
      while c < 3 do
        fitted += geometry.design(row * 3 + c) * out(c)
        c += 1
      val residual = y(row) - fitted
      energy += residual * residual
      row += 1
    (energy, out)

  enum RefinementTermination:
    case DepthComplete, SweepCap, Nonfinite

  final case class RefinementStart(
      gridNode: Int, coordinates: Vector[Double], amplitudes: Vector[Double], energy: Double,
      requestedLevels: Int, completedLevels: Int, sweeps: Int, evaluations: Int,
      finalTestedStep: Option[Vector[Double]], termination: RefinementTermination):
    def failure: Option[String] = termination match
      case RefinementTermination.DepthComplete => None
      case RefinementTermination.SweepCap => Some("refinement-sweep-cap")
      case RefinementTermination.Nonfinite => Some("nonfinite-reference")

  /** The same bounded compass loop serves direct geometry and an independent analytic stopping control. */
  private[profile] def refineCompass(
      gridNode: Int, start: Vector[Double], initialStep: Vector[Double], levels: Int, maxSweeps: Int,
      initial: (Double, Array[Double]), evaluate: Vector[Double] => (Double, Array[Double])
  ): RefinementStart =
    require(levels > 0 && maxSweeps >= 0 && start.length == 2 && initialStep.length == 2 && initialStep.forall(_ > 0.0))
    val coords = start.toArray
    val step = initialStep.toArray
    var current = initial._1
    var beta = initial._2
    var nonfinite = !current.isFinite || !beta.forall(_.isFinite)
    var completed = 0
    var sweeps = 0
    var evaluations = 0
    var tested: Option[Vector[Double]] = None
    while completed < levels && sweeps < maxSweeps && !nonfinite do
      sweeps += 1
      tested = Some(step.toVector) // record the mesh actually polled, before the final halving
      var improved = false
      var axis = 0
      while axis < 2 && !nonfinite do
        var sign = 1
        while sign >= -1 && !nonfinite do
          val trial = coords.clone()
          trial(axis) = GaussianFamily.Default.chart.clamp(axis, coords(axis) + sign * step(axis))
          if trial(axis) != coords(axis) then
            evaluations += 1
            val fit = evaluate(trial.toVector)
            if !fit._1.isFinite || !fit._2.forall(_.isFinite) then nonfinite = true
            else if fit._1 < current then
              current = fit._1
              coords(axis) = trial(axis)
              beta = fit._2
              improved = true
          sign -= 2
        axis += 1
      if !improved && !nonfinite then
        step(0) /= 2.0
        step(1) /= 2.0
        completed += 1
    val termination =
      if nonfinite then RefinementTermination.Nonfinite
      else if completed < levels then RefinementTermination.SweepCap
      else RefinementTermination.DepthComplete
    RefinementStart(gridNode, coords.toVector, beta.toVector, current, levels, completed, sweeps, evaluations, tested, termination)

  private final case class DirectReference(
      coordinates: Vector[Double], amplitudes: Array[Double], energy: Double,
      failure: Option[String], startReports: Vector[RefinementStart])

  private final class DirectOracle(
      val nodes: Vector[Int] = Vector(51, 21), val starts: Int = 4, val maxSweeps: Int = 256,
      val levels: Int = MainRefinementLevels):
    require(starts > 0 && maxSweeps >= 0)
    private val setupStarted = System.nanoTime()
    private val grid = NodeGrid(GaussianFamily.Default.chart, nodes)
    private val geometry = Array.tabulate(grid.count)(i => directGeometry(grid.point(i).coordinates))
    val setupNanos: Long = System.nanoTime() - setupStarted
    val gridGeometries: Int = grid.count
    val cachedNumericBytes: Long = geometry.iterator.map(g => 8L * (g.design.length + g.factor.lower.rows * g.factor.lower.cols)).sum
    var continuousEvaluations: Long = 0L
    var continuousHighWater: Int = 0
    var refinementSweeps: Long = 0L
    var refinementFailures: Int = 0

    /** One response-local geometry, never inserted into the grid cache. */
    private def at(coords: Vector[Double]): DirectGeometry =
      continuousEvaluations += 1L
      continuousHighWater = 1
      directGeometry(coords)

    def residualAt(coords: Vector[Double], y: Array[Double]): Double = leastSquares(at(coords), y)._1

    private def refine(start: Int, y: Array[Double]): DirectReference =
      val result = refineCompass(start, grid.point(start).coordinates, Vector(grid.step(0) / 2.0, grid.step(1) / 2.0),
        levels, maxSweeps, leastSquares(geometry(start), y), coords => leastSquares(at(coords), y))
      refinementSweeps += result.sweeps
      if result.failure.nonEmpty then refinementFailures += 1
      DirectReference(result.coordinates, result.amplitudes.toArray, result.energy, result.failure, Vector(result))

    def solve(y: Array[Double]): DirectReference =
      val ranked = Array.tabulate(grid.count)(i => (leastSquares(geometry(i), y)._1, i)).sortBy(_._1)
      val refined = Vector.tabulate(math.min(starts, ranked.length))(i => refine(ranked(i)._2, y))
      val best = refined.minBy(_.energy)
      // Even one unfinished start makes search adequacy unresolved.
      best.copy(failure = refined.flatMap(_.failure).headOption, startReports = refined.flatMap(_.startReports))

  private def directCohort(voxels: Int, snr: Double, seed: Long, shapes: Vector[Vector[Double]] = Vector.empty): Array[Double] =
    val rng = new scala.util.Random(seed)
    val chart = GaussianFamily.Default.chart
    val raw = new Array[Double](Rows * voxels)
    val nuisanceData = new Array[Double](Rows * 6)
    nuisance.copyRowMajorTo(nuisanceData)
    val scale = new Array[Double](GaussianFamily.Default.jetComponents)
    val signal = new Array[Double](Rows)
    var voxel = 0
    while voxel < voxels do
      val randomPoint = Vector(
        chart.lower(0) + 0.5 + rng.nextDouble() * (chart.width(0) - 1.0),
        chart.lower(1) + 0.2 + rng.nextDouble() * (chart.width(1) - 0.4)
      )
      val point = ShapePoint.unsafe(if shapes.isEmpty then randomPoint else shapes(voxel))
      val beta = Array.fill(3)((if rng.nextBoolean() then 1.0 else -1.0) * (0.5 + 1.5 * rng.nextDouble()))
      GaussianFamily.Default.scaleJetInto(GaussianFamily.Default.libraryNormalization, point, scale)
      val design = schedule.convolve(GaussianFamily.Default.toHrf(point), frame, precision = precision).data.data.map(_ / scale(0))
      var sum = 0.0
      var sum2 = 0.0
      var row = 0
      while row < Rows do
        var value = 0.0
        var c = 0
        while c < 3 do
          value += design(row * 3 + c) * beta(c)
          c += 1
        signal(row) = value
        sum += value
        sum2 += value * value
        row += 1
      val sd = math.sqrt(math.max(1e-12, sum2 / Rows - (sum / Rows) * (sum / Rows)))
      val noiseSd = sd / snr
      val innovation = noiseSd * math.sqrt(1.0 - 0.09)
      var noise = rng.nextGaussian() * noiseSd
      row = 0
      while row < Rows do
        if row > 0 then noise = 0.3 * noise + rng.nextGaussian() * innovation
        var drift = 0.0
        var c = 0
        while c < 6 do
          drift += nuisanceData(row * 6 + c) * (if c == 0 then 10.0 else 0.3) * sd
          c += 1
        raw(row * voxels + voxel) = signal(row) + noise + drift
        row += 1
      voxel += 1
    raw

  /** Fresh native, unnormalised, no-prior diagnostic objective for a literal column. */
  private[profile] def platformTraceSetup(column: Array[Double]): (CompactConditionObjective, Vector[Double], Double) =
    require(column.length == Rows && column.forall(java.lang.Double.isFinite))
    val prep = preparation
    val z = new Array[Double](prep.rank)
    val qy = new Array[Double](prep.nuisanceRank)
    val energy = prep.project(column, 0, z, qy)
    val objective = new CompactConditionObjective(prep, NodeGrid(GaussianFamily.Default.chart, Vector(15, 15)))
    objective.pointAt(z, energy)
    (objective, z.toVector, energy)

  private def percentile(values: Vector[Double]): Double =
    if values.isEmpty then Double.NaN
    else
      val sorted = values.sorted
      sorted(math.min(sorted.length - 1, math.ceil(.95 * sorted.length).toInt - 1).max(0))

  final case class StudyCell(
      label: String, snr: Double, seed: Long, voxels: Int, admitted: Int, latencyP95: Double, widthP95: Double, amplitudeP95: Double,
      statuses: Map[String, Long], exits: Map[String, Long], counters: DecoderCounters,
      admittedVoxels: Vector[Int], errors: Vector[AccuracyError], terminalAudits: Vector[TerminalAudit], referenceAudits: Vector[ReferenceAudit],
      terminalJetAudits: Int, terminalJetAuditFailures: Int, oracleUnresolved: Int, reference: ReferenceReceipt)

  final case class AccuracyError(voxel: Int, latency: Double, width: Double, relativeAmplitude: Double)

  final case class ReferenceAudit(
      voxel: Int, coordinates: Vector[Double], amplitudes: Vector[Double], energy: Double,
      decoderDirectEnergy: Option[Double], failures: Vector[String], starts: Vector[RefinementStart])

  final case class ReferenceReceipt(
      setupNanos: Long, responseNanos: Long, responses: Int, gridGeometries: Int, refinementStarts: Int,
      continuousEvaluations: Long, continuousHighWater: Int, cachedNumericBytes: Long,
      continuousNumericBytes: Long, refinementSweeps: Long, refinementFailures: Int, maxSweepsPerStart: Int, levels: Int)

  final case class AuditEvidence(
      jetEnergy: Double, gradient: Vector[Double], hessian: Vector[Double], amplitudes: Vector[Double],
      curvature: CurvatureStatus, conditionalSd: Option[Vector[Double]], sdFailure: Option[String],
      projectedNewtonCorrection: Option[Double])

  /** Recomputed evidence never substitutes the decoder's reported fields. */
  final case class TerminalAudit(
      voxel: Int, status: DecodeStatus, budgetExit: Option[DecodeBudgetExit], coordinates: Vector[Double],
      fitEnergy: Double, fitAmplitudes: Vector[Double], fitHessian: Vector[Double], fitSd: Vector[Double],
      evidence: Option[AuditEvidence], failure: Option[String], coherent: Boolean, work: DecoderWork)

  final case class DecoderWork(
      nodes: Long, jets: Long, exact: Long, terminal: Long, candidates: Long, steps: Long, fallbacks: Long)

  private def work(counters: DecoderCounters): DecoderWork =
    DecoderWork(
      counters.nodeScores, counters.jets, counters.exactEvaluations, counters.terminalVerifications,
      counters.candidateAttempts, counters.newtonSteps, counters.fallbacks
    )

  private def delta(after: DecoderWork, before: DecoderWork): DecoderWork =
    DecoderWork(
      after.nodes - before.nodes, after.jets - before.jets, after.exact - before.exact,
      after.terminal - before.terminal, after.candidates - before.candidates,
      after.steps - before.steps, after.fallbacks - before.fallbacks
    )

  private def closeEnough(actual: Double, expected: Double): Boolean =
    actual.isFinite && expected.isFinite && math.abs(actual - expected) <= 1e-9 * math.max(1.0, math.max(math.abs(actual), math.abs(expected)))

  private[profile] def rederiveConditionalSd(hessian: Array[Double]): Option[Vector[Double]] =
    if hessian.length != 4 || !hessian.forall(_.isFinite) then None
    else
      DMat.tabulate(2, 2)((row, col) => hessian(row * 2 + col)).cholesky.toOption.flatMap: factor =>
        factor.solve(DMat.tabulate(2, 2)((row, col) => if row == col then 1.0 else 0.0)).toOption.flatMap: inverse =>
          val diagonal = Vector(inverse(0, 0), inverse(1, 1))
          if diagonal.forall(value => value.isFinite && value > 0.0) then
            val sd = diagonal.map(value => math.sqrt(2.0 * value)) // frozen sigma2 = 1
            Option.when(sd.forall(value => value.isFinite && value > 0.0))(sd)
          else None

  /** ShapeDecoder's bound-active free-coordinate Newton rule, including the all-active SPD check. */
  private[profile] def projectedNewtonCorrection(coords: Vector[Double], gradient: Array[Double], hessian: Array[Double]): Option[Double] =
    if coords.length != 2 || !coords.forall(_.isFinite) || !gradient.forall(_.isFinite) || !hessian.forall(_.isFinite) then None
    else
      val free = Array.tabulate(2): axis =>
        val lower = coords(axis) <= GaussianFamily.Default.chart.lower(axis) + 1e-12
        val upper = coords(axis) >= GaussianFamily.Default.chart.upper(axis) - 1e-12
        !((lower && gradient(axis) > 0.0) || (upper && gradient(axis) < 0.0))
      val indices = free.indices.filter(free).toVector
      if indices.isEmpty then
        DMat.tabulate(2, 2)((r, c) => hessian(r * 2 + c)).cholesky.toOption.map(_ => 0.0)
      else
        DMat.tabulate(indices.length, indices.length)((r, c) => hessian(indices(r) * 2 + indices(c))).cholesky.toOption.flatMap: chol =>
          chol.solve(DMat.tabulate(indices.length, 1)((r, _) => -gradient(indices(r)))).toOption.flatMap: step =>
            val norm = indices.indices.map(i => math.abs(step(i, 0))).max
            Option.when(norm.isFinite)(norm)

  private[profile] def auditTerminal(
      voxel: Int, fit: CompactConditionFit, decoderWork: DecoderWork, evaluate: ProfileJetBuffer => Boolean
  ): TerminalAudit =
    val result = fit.decode
    val jet = new ProfileJetBuffer(2, 3) // fresh, never reused after failure
    val evaluated = result.coordinates.length == 2 && result.coordinates.forall(_.isFinite) && evaluate(jet)
    val finiteJet = evaluated && jet.energy.isFinite && jet.gradient.forall(_.isFinite) &&
      jet.hessian.forall(_.isFinite) && jet.amplitudes.forall(_.isFinite)
    val evidence = Option.when(finiteJet):
      val sd = rederiveConditionalSd(jet.hessian)
      AuditEvidence(jet.energy, jet.gradient.toVector, jet.hessian.toVector, jet.amplitudes.toVector,
        jet.curvature, sd, Option.when(sd.isEmpty)("curvature-or-inverse-not-finite-positive"),
        projectedNewtonCorrection(result.coordinates, jet.gradient, jet.hessian))
    def matches(a: Vector[Double], b: Vector[Double]): Boolean =
      a.length == b.length && a.zip(b).forall((x, y) => closeEnough(x, y))
    val coherent = evidence.exists: actual =>
      closeEnough(actual.jetEnergy, result.energy) && closeEnough(actual.jetEnergy, fit.residualEnergy) &&
        matches(actual.hessian, result.dataHessian) && matches(actual.hessian, result.augmentedHessian) &&
        matches(actual.amplitudes, result.amplitudes) && matches(actual.amplitudes, fit.amplitudes) &&
        actual.conditionalSd.fold(result.conditionalSd.forall(_.isNaN))(sd => matches(sd, result.conditionalSd)) &&
        (result.status != DecodeStatus.Accepted || (actual.curvature == CurvatureStatus.PositiveDefinite &&
          actual.conditionalSd.nonEmpty && actual.projectedNewtonCorrection.exists(_ <= 1e-9)))
    val failure =
      if !evaluated then Some("terminal-jet-unavailable")
      else if !finiteJet then Some("terminal-jet-nonfinite")
      else if !coherent then Some("returned-fit-mismatch-or-nonstationary-acceptance")
      else None
    TerminalAudit(voxel, result.status, result.budgetExit, result.coordinates, result.energy, fit.amplitudes,
      result.dataHessian, result.conditionalSd, evidence, failure, coherent, decoderWork)

  private def studyCell(
      label: String, snr: Double, seed: Long, whitened: Array[Double], references: Array[DirectReference],
      budget: DecodeBudget, referenceReceipt: ReferenceReceipt, oracle: DirectOracle, failAudit: Boolean
  ): StudyCell =
    val voxels = references.length
    val runtime = new CompactConditionRuntime(preparation, NodeGrid(GaussianFamily.Default.chart, Vector(15, 15)), budget, None, 1.0, NormalizationRule.Unnormalised)
    val counters = new DecoderCounters
    val statuses = scala.collection.mutable.Map.empty[String, Long].withDefaultValue(0L)
    val exits = scala.collection.mutable.Map.empty[String, Long].withDefaultValue(0L)
    val column = new Array[Double](Rows)
    val errors = Vector.newBuilder[AccuracyError]
    val terminalAudits = Vector.newBuilder[TerminalAudit]
    val referenceAudits = Vector.newBuilder[ReferenceAudit]
    var voxel = 0
    while voxel < voxels do
      var row = 0
      while row < Rows do
        column(row) = whitened(row * voxels + voxel)
        row += 1
      val before = work(counters)
      val fit = runtime.fit(column, 0, counters)
      val decoderWork = delta(work(counters), before)
      statuses(fit.decode.status.toString) += 1L
      fit.decode.budgetExit.foreach(exit => exits(exit.toString) += 1L)
      terminalAudits += auditTerminal(voxel, fit, decoderWork, jet => !failAudit && runtime.objective.jetAt(fit.decode.coordinates.toArray, jet))
      val reference = references(voxel)
      var referenceFailures = reference.failure.toVector
      var directEnergy: Option[Double] = None
      if fit.decode.status == DecodeStatus.Accepted then
        val decoderDirectEnergy = oracle.residualAt(fit.decode.coordinates, projectNuisance(1, column))
        directEnergy = Some(decoderDirectEnergy)
        if !decoderDirectEnergy.isFinite then referenceFailures :+= "nonfinite-decoder-direct-energy"
        else if decoderDirectEnergy < reference.energy - 1e-9 * math.max(1.0, math.abs(reference.energy)) then
          referenceFailures :+= "decoder-point-below-searched-reference"
        val theirs = GaussianFamily.Default.summaries(ShapePoint.unsafe(reference.coordinates))
        var numerator = 0.0
        var denominator = 0.0
        var c = 0
        while c < 3 do
          val difference = fit.amplitudes(c) - reference.amplitudes(c)
          numerator += difference * difference
          denominator += reference.amplitudes(c) * reference.amplitudes(c)
          c += 1
        errors += AccuracyError(voxel, math.abs(fit.summaries.peakLatency.value - theirs.peakLatency.value),
          math.abs(fit.summaries.fwhm.value - theirs.fwhm.value), math.sqrt(numerator / denominator))
      referenceAudits += ReferenceAudit(voxel, reference.coordinates, reference.amplitudes.toVector,
        reference.energy, directEnergy, referenceFailures, reference.startReports)
      voxel += 1
    val referenceEvidence = referenceAudits.result()
    val audits = terminalAudits.result()
    val accuracy = errors.result()
    val ids = audits.filter(_.status == DecodeStatus.Accepted).map(_.voxel)
    StudyCell(label, snr, seed, voxels, ids.length, percentile(accuracy.map(_.latency)), percentile(accuracy.map(_.width)),
      percentile(accuracy.map(_.relativeAmplitude)), statuses.toMap, exits.toMap, counters, ids, accuracy, audits, referenceEvidence,
      voxels, audits.count(_.failure.nonEmpty), referenceEvidence.count(_.failures.nonEmpty), referenceReceipt)

  private def referenceCohort(whitened: Array[Double], voxels: Int, oracle: DirectOracle): (Array[DirectReference], ReferenceReceipt) =
    val column = new Array[Double](Rows)
    val references = new Array[DirectReference](voxels)
    val t0 = System.nanoTime()
    val beforeEvaluations = oracle.continuousEvaluations
    val beforeSweeps = oracle.refinementSweeps
    val beforeFailures = oracle.refinementFailures
    var voxel = 0
    while voxel < voxels do
      var row = 0
      while row < Rows do
        column(row) = whitened(row * voxels + voxel)
        row += 1
      references(voxel) = oracle.solve(projectNuisance(1, column))
      voxel += 1
    (references, ReferenceReceipt(oracle.setupNanos, System.nanoTime() - t0, voxels, oracle.gridGeometries, oracle.starts,
      oracle.continuousEvaluations - beforeEvaluations, oracle.continuousHighWater, oracle.cachedNumericBytes, 8L * (Rows * 3 + 9),
      oracle.refinementSweeps - beforeSweeps, oracle.refinementFailures - beforeFailures, oracle.maxSweeps, oracle.levels))

  private def runStudy(
      cells: Vector[(Double, Long)], voxels: Int, baseline: DecodeBudget = Baseline,
      candidate: DecodeBudget = Candidate, failAudit: Boolean = false
  ): Vector[StudyCell] =
    require(voxels > 0 && voxels <= FullCohort)
    val oracle = new DirectOracle
    cells.flatMap: (snr, seed) =>
      val data = directCohort(voxels, snr, seed)
      val whitened = preparation.whiten(voxels, data).fold(e => throw IllegalArgumentException(e.message), identity)
      val (references, referenceReceipt) = referenceCohort(whitened, voxels, oracle)
      Vector(
        studyCell(BaselineLabel, snr, seed, whitened, references, baseline, referenceReceipt, oracle, failAudit),
        studyCell(CandidateLabel, snr, seed, whitened, references, candidate, referenceReceipt, oracle, failAudit)
      )

  def runProspectiveStudy(): Vector[StudyCell] =
    runStudy(Vector((1.0, 7000930201L), (0.5, 7000930102L), (0.25, 7000930103L)), FullCohort)

  /** Full frozen cohorts are explicit standalone work; the unit panel defaults to one response per SNR. */
  def runDevelopmentStudy(voxels: Int = 1, zeroBudget: Boolean = false, failAudit: Boolean = false): Vector[StudyCell] =
    val base = if zeroBudget then Baseline.copy(maxNewtonSteps = 0, maxJets = 1, maxExactEvaluations = 0) else Baseline
    val cand = if zeroBudget then Candidate.copy(maxNewtonSteps = 0, maxJets = 1, maxExactEvaluations = 0) else Candidate
    runStudy(Vector((1.0, 101L), (0.5, 102L)), voxels, base, cand, failAudit)

  final case class OracleComparison(
      voxel: Int, snr: Double, seed: Long, trueCoordinates: Vector[Double],
      coarse: ReferenceAudit, dense: ReferenceAudit, difference: AccuracyError)

  final case class OracleAdequacy(
      responses: Int, maxLatency: Double, maxWidth: Double, maxRelativeAmplitude: Double,
      unresolved: Int, coarse: Vector[ReferenceReceipt], dense: Vector[ReferenceReceipt], comparisons: Vector[OracleComparison]):
    def adequate: Boolean = unresolved == 0 && maxLatency <= .002 && maxWidth <= .005 && maxRelativeAmplitude <= 1e-4

  /** Predeclared three varied shapes at BOTH SNRs (six DEV responses), unchanged adequacy margins. */
  def oracleAdequacyDevelopment(): OracleAdequacy =
    val chart = GaussianFamily.Default.chart
    val shapes = Vector((.15, .25), (.5, .5), (.85, .75)).map: (a, b) =>
      Vector(chart.lower(0) + a * chart.width(0), chart.lower(1) + b * chart.width(1))
    val coarseOracle = new DirectOracle(Vector(51, 21), 4)
    val denseOracle = new DirectOracle(Vector(61, 25), 6, levels = DenseRefinementLevels)
    val coarseReceipts = Vector.newBuilder[ReferenceReceipt]
    val denseReceipts = Vector.newBuilder[ReferenceReceipt]
    val comparisons = Vector.newBuilder[OracleComparison]
    var maxLatency = 0.0
    var maxWidth = 0.0
    var maxAmplitude = 0.0
    var unresolved = 0
    for (snr, seed) <- Vector(1.0 -> 101L, .5 -> 102L) do
      val whitened = preparation.whiten(shapes.length, directCohort(shapes.length, snr, seed, shapes)).fold(e => throw IllegalArgumentException(e.message), identity)
      val (coarse, cr) = referenceCohort(whitened, shapes.length, coarseOracle)
      val (dense, dr) = referenceCohort(whitened, shapes.length, denseOracle)
      coarseReceipts += cr
      denseReceipts += dr
      var voxel = 0
      while voxel < shapes.length do
        if coarse(voxel).failure.nonEmpty || dense(voxel).failure.nonEmpty then unresolved += 1
        val cs = GaussianFamily.Default.summaries(ShapePoint.unsafe(coarse(voxel).coordinates))
        val ds = GaussianFamily.Default.summaries(ShapePoint.unsafe(dense(voxel).coordinates))
        maxLatency = math.max(maxLatency, math.abs(cs.peakLatency.value - ds.peakLatency.value))
        maxWidth = math.max(maxWidth, math.abs(cs.fwhm.value - ds.fwhm.value))
        val numerator = coarse(voxel).amplitudes.zip(dense(voxel).amplitudes).map((a, b) => (a - b) * (a - b)).sum
        val denominator = dense(voxel).amplitudes.map(a => a * a).sum
        val difference = AccuracyError(voxel, math.abs(cs.peakLatency.value - ds.peakLatency.value),
          math.abs(cs.fwhm.value - ds.fwhm.value), math.sqrt(numerator / denominator))
        maxAmplitude = math.max(maxAmplitude, difference.relativeAmplitude)
        def evidence(ref: DirectReference): ReferenceAudit = ReferenceAudit(voxel, ref.coordinates, ref.amplitudes.toVector,
          ref.energy, None, ref.failure.toVector, ref.startReports)
        comparisons += OracleComparison(voxel, snr, seed, shapes(voxel), evidence(coarse(voxel)), evidence(dense(voxel)), difference)
        voxel += 1
    OracleAdequacy(6, maxLatency, maxWidth, maxAmplitude, unresolved, coarseReceipts.result(), denseReceipts.result(), comparisons.result())

  private[profile] def oracleExhaustionControl(): Boolean =
    val oracle = new DirectOracle(Vector(3, 3), 1, maxSweeps = 0)
    oracle.solve(Array.fill(Rows)(0.0)).failure.contains("refinement-sweep-cap") && oracle.refinementFailures == 1

  /** Every candidate attempt charges a post-initial jet or exact evaluation; terminal jets only reduce this bound. */
  def totalCandidateBound(budget: DecodeBudget): Long = budget.maxJets.toLong - 1L + budget.maxExactEvaluations

  def withinCaps(actual: DecoderWork, budget: DecodeBudget): Boolean =
    Vector(actual.nodes, actual.jets, actual.exact, actual.terminal, actual.candidates, actual.steps, actual.fallbacks).forall(_ >= 0L) &&
      actual.nodes <= 225L && actual.jets <= budget.maxJets && actual.exact <= budget.maxExactEvaluations &&
      actual.candidates <= totalCandidateBound(budget) && actual.steps <= budget.maxNewtonSteps &&
      actual.terminal <= actual.jets && actual.fallbacks <= 1L

  /** Baseline is descriptive; a smaller DEV cell can never qualify. */
  def meetsGate(cell: StudyCell): Boolean =
    cell.label == CandidateLabel && (cell.snr == 1.0 || cell.snr == .5) && cell.voxels == FullCohort && cell.terminalAudits.length == FullCohort &&
      cell.terminalAudits.map(_.voxel) == (0 until FullCohort).toVector && cell.statuses.values.sum == FullCohort &&
      cell.errors.length == cell.admitted && cell.errors.map(_.voxel) == cell.admittedVoxels &&
      cell.referenceAudits.length == FullCohort && cell.referenceAudits.forall(_.failures.isEmpty) &&
      cell.errors.forall(e => e.latency.isFinite && e.width.isFinite && e.relativeAmplitude.isFinite) && cell.admitted >= 190 &&
      cell.latencyP95.isFinite && cell.widthP95.isFinite && cell.amplitudeP95.isFinite &&
      cell.latencyP95 <= .02 && cell.widthP95 <= .05 && cell.amplitudeP95 <= 1e-3 && cell.oracleUnresolved == 0 &&
      cell.terminalAudits.forall(audit => withinCaps(audit.work, Candidate) &&
        (audit.status != DecodeStatus.Accepted || audit.coherent))

  def runQualified(
      label: String, voxels: Int, blockSize: Int, snr: Double, seed: Long, budget: DecodeBudget,
      capturePerVoxel: Boolean = false, sink: VoxelReceipt => Unit = _ => ()
  ): Receipt =
    require(voxels > 0 && blockSize >= 1 && blockSize <= MaxBlock && snr.isFinite && snr > 0.0)
    require(!capturePerVoxel || voxels <= MaxBlock, "retained DEV output is bounded to one maximum block")
    val started = System.nanoTime()
    val reused = preparationReady
    val prep = preparation
    val setupStarted = System.nanoTime()
    val runtime = new CompactConditionRuntime(prep, NodeGrid(GaussianFamily.Default.chart, Vector(15, 15)), budget, None, 1.0, NormalizationRule.Unnormalised)
    val setupNanos = System.nanoTime() - setupStarted
    val rng = new scala.util.Random(seed)
    val counters = new DecoderCounters
    val statuses = scala.collection.mutable.Map.empty[String, Long].withDefaultValue(0L)
    val exits = scala.collection.mutable.Map.empty[String, Long].withDefaultValue(0L)
    var allocationNanos = 0L
    var inputNanos = 0L
    var whitenNanos = 0L
    var gatherNanos = 0L
    var fitNanos = 0L
    var sinkNanos = 0L
    var inputChecksum = 0L
    var outputChecksum = 0L
    var accepted = 0L
    var float32Failures = 0L
    val column = new Array[Double](Rows)
    val perVoxel = if capturePerVoxel then Some(Vector.newBuilder[VoxelReceipt]) else None
    val sourceId = ConditionC0QualificationPlatform.executionSourceId.getOrElse("unfrozen-dev") + ":basis-fixture:" + seed
    var start = 0
    while start < voxels do
      val width = math.min(blockSize, voxels - start)
      var t0 = System.nanoTime()
      val raw = new Array[Double](Rows * width)
      allocationNanos += System.nanoTime() - t0
      t0 = System.nanoTime()
      inputChecksum = fill(raw, width, start, rng, snr, prep.expanded, inputChecksum)
      inputNanos += System.nanoTime() - t0
      t0 = System.nanoTime()
      val whitened = prep.whiten(width, raw).fold(e => throw IllegalArgumentException(e.message), identity)
      whitenNanos += System.nanoTime() - t0
      var local = 0
      while local < width do
        t0 = System.nanoTime()
        var row = 0
        while row < Rows do
          column(row) = whitened(row * width + local)
          row += 1
        gatherNanos += System.nanoTime() - t0
        val before = work(counters)
        t0 = System.nanoTime()
        val fit = runtime.fit(column, 0, counters)
        fitNanos += System.nanoTime() - t0
        t0 = System.nanoTime()
        val result = fit.decode
        statuses(result.status.toString) += 1L
        result.budgetExit.foreach(exit => exits(exit.toString) += 1L)
        if result.status == DecodeStatus.Accepted then accepted += 1L
        val doubles = result.coordinates ++ fit.amplitudes
        val floats = doubles.map(_.toFloat)
        val finite = doubles.forall(_.isFinite) && floats.forall(_.isFinite)
        val roundtrip = finite && doubles.zip(floats).forall: (value, rounded) =>
          math.abs(value - rounded.toDouble) <= math.max(math.abs(value) * math.pow(2.0, -24.0), java.lang.Float.MIN_VALUE.toDouble)
        if !finite || !roundtrip then float32Failures += 1L
        val record = VoxelReceipt(sourceId, start + local, result.status, result.budgetExit,
          floats.take(2), floats.drop(2), delta(work(counters), before), finite, roundtrip)
        // Callback failure propagates immediately. No subsequent response is generated or emitted.
        sink(record)
        perVoxel.foreach(_ += record)
        outputChecksum = outputChecksum ^ record.sampleId.toLong ^ result.status.ordinal.toLong ^ result.budgetExit.fold(-1L)(_.ordinal.toLong)
        floats.foreach(value => outputChecksum = mix(outputChecksum, value.toDouble))
        sinkNanos += System.nanoTime() - t0
        local += 1
      start += width
    Receipt(label, voxels, blockSize, reused, if reused then PreparationTiming(0L, 0L, 0L, 0L) else preparationTiming,
      fixtureNanos, setupNanos, allocationNanos, inputNanos, whitenNanos, gatherNanos, fitNanos, sinkNanos,
      System.nanoTime() - started, accepted, statuses.toMap, exits.toMap, counters, inputChecksum, outputChecksum,
      ConditionC0QualificationPlatform.endHeapBytes, ConditionC0QualificationPlatform.maxHeapBytes,
      8L * Rows * math.min(voxels, blockSize), 8L * Rows * math.min(voxels, blockSize), 8L * Rows,
      20L * voxels, if capturePerVoxel then 20L * voxels else 0L, float32Failures, perVoxel.map(_.result()))

  // Tiny test-only JSON codec: finite numbers or null, explicit availability fields, escaped strings.
  private def quoted(value: String): String =
    "\"" + value.flatMap:
      case '"' => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case ch if ch < ' ' => f"\\u${ch.toInt}%04x"
      case ch => ch.toString
    + "\""

  private def number(value: Double): String = if value.isFinite then value.toString else "null"
  private def array(values: Iterable[String]): String = values.mkString("[", ",", "]")
  private def numbers(values: Iterable[Double]): String = array(values.map(number))
  private def obj(fields: (String, String)*): String = fields.map((key, value) => quoted(key) + ":" + value).mkString("{", ",", "}")
  private def optional(value: Option[String]): String = value.fold("null")(quoted)
  private def counts(values: Map[String, Long]): String = obj(values.toVector.sortBy(_._1).map((k, v) => k -> v.toString)*)
  private def workJson(w: DecoderWork): String = obj("nodes" -> w.nodes.toString, "jets" -> w.jets.toString,
    "exact" -> w.exact.toString, "terminal" -> w.terminal.toString, "candidates" -> w.candidates.toString,
    "steps" -> w.steps.toString, "fallbacks" -> w.fallbacks.toString)
  private def errorJson(error: AccuracyError): String = obj("voxel" -> error.voxel.toString,
    "latency" -> number(error.latency), "width" -> number(error.width), "relativeAmplitude" -> number(error.relativeAmplitude))
  private def referenceJson(r: ReferenceReceipt): String = obj("setupNanos" -> r.setupNanos.toString,
    "responseNanos" -> r.responseNanos.toString, "responses" -> r.responses.toString, "gridGeometries" -> r.gridGeometries.toString,
    "refinementStarts" -> r.refinementStarts.toString, "continuousEvaluations" -> r.continuousEvaluations.toString,
    "continuousHighWater" -> r.continuousHighWater.toString, "continuousCached" -> "0",
    "cachedNumericBytes" -> r.cachedNumericBytes.toString, "continuousNumericBytes" -> r.continuousNumericBytes.toString,
    "refinementSweeps" -> r.refinementSweeps.toString, "refinementFailures" -> r.refinementFailures.toString,
    "maxSweepsPerStart" -> r.maxSweepsPerStart.toString, "levels" -> r.levels.toString)
  private def startJson(r: RefinementStart): String = obj("gridNode" -> r.gridNode.toString,
    "coordinates" -> numbers(r.coordinates), "amplitudes" -> numbers(r.amplitudes), "energy" -> number(r.energy),
    "requestedLevels" -> r.requestedLevels.toString, "completedLevels" -> r.completedLevels.toString,
    "sweeps" -> r.sweeps.toString, "evaluations" -> r.evaluations.toString,
    "finalTestedStep" -> r.finalTestedStep.fold("null")(numbers), "termination" -> quoted(r.termination.toString))
  private def referenceAuditJson(r: ReferenceAudit): String = obj("coordinates" -> numbers(r.coordinates),
    "amplitudes" -> numbers(r.amplitudes), "energy" -> number(r.energy),
    "decoderDirectEnergy" -> r.decoderDirectEnergy.fold("null")(number), "failures" -> array(r.failures.map(quoted)),
    "starts" -> array(r.starts.map(startJson)))

  private def auditJson(a: TerminalAudit): String =
    val evidence = a.evidence.fold("null"): e =>
      obj("jetEnergy" -> number(e.jetEnergy), "gradient" -> numbers(e.gradient), "hessian" -> numbers(e.hessian),
        "amplitudes" -> numbers(e.amplitudes), "curvature" -> quoted(e.curvature.toString),
        "conditionalSd" -> e.conditionalSd.fold("null")(numbers), "sdFailure" -> optional(e.sdFailure),
        "projectedNewtonCorrection" -> e.projectedNewtonCorrection.fold("null")(number))
    obj("voxel" -> a.voxel.toString, "status" -> quoted(a.status.toString), "budgetExit" -> optional(a.budgetExit.map(_.toString)),
      "coordinates" -> numbers(a.coordinates), "fitEnergy" -> number(a.fitEnergy), "fitAmplitudes" -> numbers(a.fitAmplitudes),
      "fitHessian" -> numbers(a.fitHessian), "fitSd" -> numbers(a.fitSd), "auditAvailable" -> a.evidence.nonEmpty.toString,
      "auditFailure" -> optional(a.failure), "audit" -> evidence, "coherent" -> a.coherent.toString, "work" -> workJson(a.work))

  def studyJsonLines(cells: Vector[StudyCell], cohort: String = "development"): Vector[String] =
    val identity = ConditionC0QualificationPlatform.executionSourceId.getOrElse("unfrozen-dev")
    val lines = Vector.newBuilder[String]
    cells.foreach: cell =>
      lines += obj("kind" -> quoted("cell"), "platform" -> quoted(ConditionC0QualificationPlatform.name),
        "sourceId" -> quoted(identity), "cohort" -> quoted(cohort), "policy" -> quoted(cell.label), "snr" -> number(cell.snr), "seed" -> cell.seed.toString,
        "voxels" -> cell.voxels.toString, "admitted" -> cell.admitted.toString, "latencyP95" -> number(cell.latencyP95),
        "widthP95" -> number(cell.widthP95), "amplitudeP95" -> number(cell.amplitudeP95), "statuses" -> counts(cell.statuses),
        "exits" -> counts(cell.exits), "work" -> workJson(work(cell.counters)), "auditAttempts" -> cell.terminalJetAudits.toString,
        "auditFailures" -> cell.terminalJetAuditFailures.toString, "oracleUnresolved" -> cell.oracleUnresolved.toString,
        "reference" -> referenceJson(cell.reference), "candidateGate" -> meetsGate(cell).toString,
        "perIterationCandidateCap" -> (if cell.label == CandidateLabel then Candidate.maxCandidateAttempts else Baseline.maxCandidateAttempts).toString,
        "totalCandidateBound" -> totalCandidateBound(if cell.label == CandidateLabel then Candidate else Baseline).toString)
      cell.terminalAudits.foreach: audit =>
        lines += obj("kind" -> quoted("voxel"), "platform" -> quoted(ConditionC0QualificationPlatform.name),
          "sourceId" -> quoted(identity), "cohort" -> quoted(cohort), "policy" -> quoted(cell.label), "snr" -> number(cell.snr), "seed" -> cell.seed.toString,
          "terminal" -> auditJson(audit), "reference" -> cell.referenceAudits.find(_.voxel == audit.voxel).fold("null")(referenceAuditJson),
          "accuracy" -> cell.errors.find(_.voxel == audit.voxel).fold("null")(errorJson))
    cells.groupBy(cell => (cell.snr, cell.seed)).toVector.sortBy(_._1).foreach:
      case ((snr, seed), pair) =>
        val base = pair.find(_.label == BaselineLabel).get
        val cand = pair.find(_.label == CandidateLabel).get
        require(base.terminalAudits.map(_.voxel) == cand.terminalAudits.map(_.voxel), "paired IDs differ")
        val newly = cand.admittedVoxels.filterNot(base.admittedVoxels.contains)
        val newlyErrors = cand.errors.filter(e => newly.contains(e.voxel))
        lines += obj("kind" -> quoted("paired"), "platform" -> quoted(ConditionC0QualificationPlatform.name),
          "sourceId" -> quoted(identity), "cohort" -> quoted(cohort), "snr" -> number(snr), "seed" -> seed.toString,
          "transitions" -> array(base.terminalAudits.zip(cand.terminalAudits).map: (b, c) =>
            obj("voxel" -> b.voxel.toString, "baseline" -> quoted(b.status.toString), "candidate" -> quoted(c.status.toString))),
          "acceptedToRefused" -> array(base.admittedVoxels.filterNot(cand.admittedVoxels.contains).map(_.toString)),
          "newlyAdmitted" -> array(newly.map(_.toString)), "newlyAdmittedErrors" -> array(newlyErrors.map(errorJson)),
          "newlyLatencyP95" -> number(percentile(newlyErrors.map(_.latency))),
          "newlyWidthP95" -> number(percentile(newlyErrors.map(_.width))),
          "newlyAmplitudeP95" -> number(percentile(newlyErrors.map(_.relativeAmplitude))))
    lines.result()

  def adequacyJson(a: OracleAdequacy): String = obj("kind" -> quoted("oracle-adequacy"),
    "platform" -> quoted(ConditionC0QualificationPlatform.name), "sourceId" -> optional(ConditionC0QualificationPlatform.executionSourceId), "responses" -> a.responses.toString,
    "maxLatency" -> number(a.maxLatency), "maxWidth" -> number(a.maxWidth), "maxRelativeAmplitude" -> number(a.maxRelativeAmplitude),
    "unresolved" -> a.unresolved.toString, "adequate" -> a.adequate.toString,
    "coarse" -> array(a.coarse.map(referenceJson)), "dense" -> array(a.dense.map(referenceJson)),
    "comparisons" -> array(a.comparisons.map: c =>
      obj("voxel" -> c.voxel.toString, "snr" -> number(c.snr), "seed" -> c.seed.toString,
        "trueCoordinates" -> numbers(c.trueCoordinates), "coarse" -> referenceAuditJson(c.coarse),
        "dense" -> referenceAuditJson(c.dense), "difference" -> errorJson(c.difference))))

  def measurementJson(r: Receipt): String = obj("kind" -> quoted("measurement"),
    "platform" -> quoted(ConditionC0QualificationPlatform.name), "sourceId" -> optional(ConditionC0QualificationPlatform.executionSourceId),
    "policy" -> quoted(r.label), "voxels" -> r.voxels.toString, "blockSize" -> r.blockSize.toString,
    "preparationReused" -> r.preparationReused.toString, "basisNanos" -> r.preparationTiming.basis.toString,
    "expansionNanos" -> r.preparationTiming.expansion.toString, "certificationNanos" -> r.preparationTiming.certification.toString,
    "preparationNanos" -> r.preparationTiming.preparation.toString, "fixtureInitializationNanos" -> r.fixtureNanos.toString,
    "runtimeSetupNanos" -> r.setupNanos.toString, "allocationNanos" -> r.allocationNanos.toString,
    "inputNanos" -> r.inputNanos.toString, "whitenNanos" -> r.whitenNanos.toString, "gatherNanos" -> r.gatherNanos.toString,
    "projectDecodeReadoutNanos" -> r.fitNanos.toString, "sinkNanos" -> r.sinkNanos.toString,
    "endToEndNanos" -> r.totalNanos.toString, "endHeapBytes" -> r.endHeapBytes.fold("null")(_.toString),
    "maxHeapBytes" -> r.maxHeapBytes.fold("null")(_.toString), "rawBlockBytes" -> r.rawBlockBytes.toString,
    "whitenedBlockBytes" -> r.whitenedBlockBytes.toString, "gatherBytes" -> r.gatherBytes.toString,
    "emittedFloat32Bytes" -> r.emittedFloat32Bytes.toString, "retainedFloat32Bytes" -> r.retainedFloat32Bytes.toString,
    "float32Failures" -> r.float32Failures.toString, "accepted" -> r.accepted.toString, "statuses" -> counts(r.statuses),
    "exits" -> counts(r.budgetExits), "work" -> workJson(work(r.counters)),
    "inputChecksum" -> quoted(r.inputChecksum.toString), "outputChecksum" -> quoted(r.outputChecksum.toString))

/** One frozen Node entry can dispatch the same standalone workloads as the JVM. */
object ConditionC0QualificationMain:
  def main(args: Array[String]): Unit =
    val command = ConditionC0QualificationPlatform.arguments(args)
    command.headOption.getOrElse("development") match
      case "oracle" => ConditionC0QualificationOracleControlMain.main(command.drop(1))
      case "study" => ConditionC0QualificationStudyMain.main(command.drop(1))
      case "measurement" => ConditionC0QualificationMeasurementMain.main(command.drop(1))
      case "development" => ConditionC0QualificationDevelopmentStudyMain.main(command.drop(1))
      case other => throw IllegalArgumentException("unknown C0 command: " + other)

object ConditionC0QualificationStudyMain:
  def main(args: Array[String]): Unit =
    val mode = args.headOption.getOrElse("development")
    require(ConditionC0QualificationPlatform.executionSourceId.nonEmpty, "standalone work requires a source freeze ID")
    val cells = mode match
      case "development" => ConditionC0QualificationHarness.runDevelopmentStudy(ConditionC0QualificationHarness.FullCohort)
      case "fresh" =>
        require(args.drop(1).headOption.contains("--reviewed-freeze=" + ConditionC0QualificationPlatform.executionSourceId.get),
          "fresh work requires independent review of this exact freeze")
        ConditionC0QualificationHarness.runProspectiveStudy()
      case other => throw IllegalArgumentException("study mode must be development or fresh: " + other)
    ConditionC0QualificationHarness.studyJsonLines(cells, mode).foreach(println)
    cells.filter(cell => cell.snr >= .5 && cell.label == ConditionC0QualificationHarness.CandidateLabel).foreach: cell =>
      require(ConditionC0QualificationHarness.meetsGate(cell), "unmet " + mode + " candidate gate SNR " + cell.snr)

object ConditionC0QualificationDevelopmentStudyMain:
  def main(args: Array[String]): Unit =
    val voxels = args.headOption.fold(1)(_.toInt)
    val cells = ConditionC0QualificationHarness.runDevelopmentStudy(voxels)
    ConditionC0QualificationHarness.studyJsonLines(cells).foreach(println)

/** Manual performance entry; the CLI cannot default into the expensive workload. */
object ConditionC0QualificationMeasurementMain:
  def main(args: Array[String]): Unit =
    val policy = args.headOption.getOrElse("baseline")
    val voxels = args.drop(1).headOption.fold(256)(_.toInt)
    if voxels >= 100000 then
      require(ConditionC0QualificationPlatform.executionSourceId.nonEmpty && args.exists(_.startsWith("--science-gate=")),
        "100k requires a reviewed source freeze and the matching successful science-gate receipt")
    val budget = policy match
      case "baseline" => ConditionC0QualificationHarness.Baseline
      case "candidate" => ConditionC0QualificationHarness.Candidate
      case other => throw IllegalArgumentException("policy must be baseline or candidate, got " + other)
    val receipt = ConditionC0QualificationHarness.runQualified(
      if policy == "baseline" then ConditionC0QualificationHarness.BaselineLabel else ConditionC0QualificationHarness.CandidateLabel,
      voxels, ConditionC0QualificationHarness.MaxBlock, 1.0, 101L, budget)
    println(ConditionC0QualificationHarness.measurementJson(receipt))

object ConditionC0QualificationOracleControlMain:
  def main(args: Array[String]): Unit =
    val adequacy = ConditionC0QualificationHarness.oracleAdequacyDevelopment()
    println(ConditionC0QualificationHarness.adequacyJson(adequacy))
    require(adequacy.adequate, "direct oracle adequacy unresolved; no fresh or performance promotion")

class ConditionC0QualificationSuite extends munit.FunSuite:
  import ConditionC0QualificationHarness.*

  // Measured worst cases for the bounded DEV/control studies: 14.2 s JVM and 29.9 s JS on a quiet
  // host, 87 s under load average ~52. Ten minutes gives ~7x the loaded worst case; assertions unchanged.
  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(10, "min")

  test("public C0 block widths preserve all signed outputs, sample IDs and seven work fields"):
    val receipts = Vector(1, 2, 256).map: blockSize =>
      runQualified(BaselineLabel, 256, blockSize, 1.0, 101L, Baseline, capturePerVoxel = true)
    val expected = receipts.head
    receipts.foreach: receipt =>
      assertEquals(receipt.statuses.values.sum, 256L)
      assertEquals(receipt.counters.voxels, 256L)
      assertEquals(receipt.float32Failures, 0L)
      assertEquals(receipt.inputChecksum, expected.inputChecksum)
      assertEquals(receipt.outputChecksum, expected.outputChecksum)
      assertEquals(receipt.statuses, expected.statuses)
      assertEquals(receipt.budgetExits, expected.budgetExits)
      assertEquals(receipt.perVoxel, expected.perVoxel)
      assertEquals(receipt.perVoxel.get.map(_.sampleId), (0 until 256).toVector)
      assert(receipt.perVoxel.get.forall(r => r.amplitudes.length == 3 && r.finite && r.roundtrip && withinCaps(r.work, Baseline)))
      assertEquals(receipt.emittedFloat32Bytes, 20L * 256)
      assertEquals(receipt.retainedFloat32Bytes, 20L * 256)
    println(measurementJson(expected))

  test("a failed sink stops immediately and full-volume retention is refused"):
    var calls = 0
    val boom = new IllegalStateException("deliberate-sink-failure")
    val thrown = intercept[IllegalStateException]:
      runQualified(BaselineLabel, 10, 1, 1.0, 101L, Baseline, sink = record =>
        assertEquals(record.sampleId, calls)
        calls += 1
        if calls == 2 then throw boom)
    assert(thrown eq boom)
    assertEquals(calls, 2)
    intercept[IllegalArgumentException]:
      runQualified(BaselineLabel, 257, 256, 1.0, 101L, Baseline, capturePerVoxel = true)

  test("terminal stationarity uses the free block and requires curvature when every bound is active"):
    val interior = Vector(5.0, math.log(1.5))
    val gradient = Array(2.0, -3.0)
    val hessian = Array(4.0, 0.0, 0.0, 6.0)
    assertEqualsDouble(projectedNewtonCorrection(interior, gradient, hessian).get, .5, 1e-12)
    assertEqualsDouble(projectedNewtonCorrection(interior, gradient.map(_ * 7), hessian.map(_ * 7)).get, .5, 1e-12)
    val chart = GaussianFamily.Default.chart
    // Coupling to the active coordinate must not enter the free-coordinate solve.
    assertEqualsDouble(projectedNewtonCorrection(Vector(chart.lower(0), interior(1)), Array(10.0, -3.0), Array(4.0, 1.0, 1.0, 6.0)).get, .5, 1e-12)
    assertEquals(projectedNewtonCorrection(chart.lower, Array(1.0, 1.0), Array(-1.0, 0.0, 0.0, 1.0)), None)
    assertEquals(rederiveConditionalSd(Array(-1.0, 0.0, 0.0, 1.0)), None)
    assertEquals(rederiveConditionalSd(Array(Double.NaN, 0.0, 0.0, 1.0)), None)
    assertEqualsDouble(rederiveConditionalSd(Array(4.0, 0.0, 0.0, 2.0)).get.head, math.sqrt(.5), 1e-12)

  test("bounded DEV paired study exercises complete audits and parseable report without qualification"):
    val cells = runDevelopmentStudy()
    assertEquals(cells.length, 4)
    cells.foreach: cell =>
      assertEquals(cell.voxels, 1)
      assertEquals(cell.terminalAudits.length, 1)
      assertEquals(cell.terminalJetAudits, 1)
      assertEquals(cell.referenceAudits.length, 1)
      assertEquals(cell.statuses.values.sum, 1L)
      assertEquals(cell.errors.length, cell.admitted)
      assertEquals(cell.admittedVoxels.length, cell.admitted)
      assert(cell.terminalAudits.forall(_.evidence.nonEmpty))
      assert(!meetsGate(cell))
    val admittedCell = cells.find(c => c.label == CandidateLabel && c.snr == .5).get
    val healthy = admittedCell.copy(voxels = FullCohort, admitted = FullCohort, statuses = Map("Accepted" -> 200L),
      admittedVoxels = (0 until FullCohort).toVector,
      errors = Vector.tabulate(FullCohort)(i => admittedCell.errors.head.copy(voxel = i)),
      terminalAudits = Vector.tabulate(FullCohort)(i => admittedCell.terminalAudits.head.copy(voxel = i)),
      referenceAudits = Vector.tabulate(FullCohort)(i => admittedCell.referenceAudits.head.copy(voxel = i)))
    // A synthetic report control, never printed as a cohort or qualification receipt.
    assert(meetsGate(healthy))
    assert(!meetsGate(healthy.copy(snr = .25)))
    assert(!meetsGate(healthy.copy(label = BaselineLabel)))
    assert(!meetsGate(healthy.copy(errors = healthy.errors.updated(0, healthy.errors.head.copy(relativeAmplitude = Double.NaN)))))
    studyJsonLines(cells).foreach(println)

  test("zero-admission control retains every attempt and cannot qualify"):
    val zero = runDevelopmentStudy(zeroBudget = true)
    zero.foreach: cell =>
      assertEquals(cell.admitted, 0)
      assert(cell.errors.isEmpty)
      assert(cell.latencyP95.isNaN)
      assert(!meetsGate(cell))
    studyJsonLines(zero, "zero-budget-control").foreach(println)

  test("unavailable-audit control cannot reuse or substitute evidence"):
    val failure = runDevelopmentStudy(failAudit = true)
    failure.foreach: cell =>
      assertEquals(cell.terminalJetAuditFailures, 1)
      assert(cell.terminalAudits.forall(a => a.evidence.isEmpty && a.failure.contains("terminal-jet-unavailable") && !a.coherent))
      assert(!meetsGate(cell))
    studyJsonLines(failure, "audit-failure-control").foreach(println)

  test("candidate total quota and bounded oracle exhaustion are explicit"):
    assertEquals(totalCandidateBound(Candidate), 13L)
    assertEquals(totalCandidateBound(Baseline), 9L)
    assert(withinCaps(DecoderWork(225, 12, 2, 1, 13, 6, 0), Candidate))
    assert(!withinCaps(DecoderWork(225, 12, 2, 1, 14, 6, 0), Candidate))
    assert(oracleExhaustionControl())

  test("fixed compass resolution converges a coupled off-grid quadratic and reports tested mesh"):
    val target = Vector(5.12345678, math.log(1.37))
    def evaluate(x: Vector[Double]): (Double, Array[Double]) =
      val a = x(0) - target(0)
      val b = x(1) - target(1)
      (5.0 + 3.0 * a * a + 2.0 * a * b + 2.0 * b * b, Array(x(0), -x(1)))
    val start = Vector(5.0, math.log(1.5))
    val step = Vector(.05, GaussianFamily.Default.chart.width(1) / 40.0)
    val coarse = refineCompass(0, start, step, 7, 256, evaluate(start), evaluate)
    val fine = refineCompass(0, start, step, MainRefinementLevels, 256, evaluate(start), evaluate)
    def distance(r: RefinementStart): Double = r.coordinates.zip(target).map((a, b) => math.abs(a - b)).max
    assert(distance(coarse) > 2e-5)
    assert(distance(fine) < 1e-5)
    assert(fine.energy < coarse.energy)
    assertEquals(fine.termination, RefinementTermination.DepthComplete)
    assertEquals(fine.completedLevels, 14)
    assertEquals(fine.evaluations, fine.sweeps * 4)
    assertEqualsDouble(fine.finalTestedStep.get.head, step.head / math.pow(2.0, 13), 0.0)
    val exhausted = refineCompass(0, start, step, 14, 0, evaluate(start), evaluate)
    assertEquals(exhausted.termination, RefinementTermination.SweepCap)
    assertEquals(exhausted.evaluations, 0)
    assertEquals(exhausted.finalTestedStep, None)
    val invalid = refineCompass(0, start, step, 14, 256, evaluate(start), _ => (Double.NaN, Array(1.0)))
    assertEquals(invalid.termination, RefinementTermination.Nonfinite)
    var inside = true
    val chart = GaussianFamily.Default.chart
    def outsideMinimum(x: Vector[Double]): (Double, Array[Double]) =
      inside &&= x.indices.forall(i => x(i) >= chart.lower(i) && x(i) <= chart.upper(i))
      (5.0 + x.indices.map(i => math.pow(x(i) - chart.upper(i) - 1.0, 2)).sum, Array(1.0))
    val boundary = refineCompass(0, chart.upper, step, 14, 256, outsideMinimum(chart.upper), outsideMinimum)
    assert(inside)
    assertEquals(boundary.coordinates, chart.upper)
    assertEquals(boundary.termination, RefinementTermination.DepthComplete)
