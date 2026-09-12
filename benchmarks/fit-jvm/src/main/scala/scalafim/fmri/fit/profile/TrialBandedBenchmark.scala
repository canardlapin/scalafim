package scalafim.fmri.fit.profile

import gale.linalg.DMat
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import scala.compiletime.uninitialized
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.fmri.design.hrf.{ExpandedTrialDesign, HrfKernelBasis, KernelBasisSpec, TrialMembership}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{Cascade34Family, ShapePoint}

/** PHRF-07 trial-backend workload on the frozen B0 dimensions. Responses are
  * retained in a bounded 256-voxel input block and recycled; trial outputs are
  * converted to Float32 and consumed immediately, so increasing `voxels`
  * cannot create a `N x V` retained payload.
  *
  * `dense` uses seeded uniform 0.1-second onsets. `regular` spaces the same
  * trial count evenly and supplies the sparse/dense bandwidth comparison.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class TrialBandedBenchmark:

  @Param(Array("300", "1200"))
  var trials: Int = 0

  @Param(Array("256"))
  var voxels: Int = 0

  @Param(Array("dense", "regular"))
  var schedule: String = ""

  private val rows = 600
  private val conditions = 3
  private val nuisanceColumns = 6
  private val lambda = 1.0
  private val bankNodes = 8
  private var preparation: TrialBandedPreparation = uninitialized
  private var objective: TrialBandedObjective = uninitialized
  private var responseColumns: Array[Double] = uninitialized
  private var encoded: TrialBandedResponse = uninitialized
  private var trialAmplitudes: Array[Double] = uninitialized
  private var floatOutput: Array[Float] = uninitialized
  private var firstJet: ProfileJetBuffer = uninitialized
  private var secondJet: ProfileJetBuffer = uninitialized
  private var exactCoordinates: Vector[Double] = uninitialized
  private var setupElapsedMillis: Double = Double.NaN

  @Setup(Level.Trial)
  def setup(): Unit =
    require(trials >= conditions && trials % conditions == 0, s"trials must be a positive multiple of $conditions")
    require(voxels > 0, "voxels must be positive")
    require(schedule == "dense" || schedule == "regular", s"unknown schedule $schedule")
    val started = System.nanoTime()
    val family = Cascade34Family.Default
    val basis = TrialBandedBenchmark.checkpointBasis
    val onsetRng = new scala.util.Random(20260910L)
    val responseRng = new scala.util.Random(20260911L)
    val lastOnset = rows - family.horizon.value - 1.0
    val onsets =
      if schedule == "dense" then
        Vector.fill(trials)(math.floor(onsetRng.nextDouble() * lastOnset * 10.0) / 10.0).sorted.map(Seconds(_))
      else
        Vector.tabulate(trials)(i => Seconds(lastOnset * i / math.max(1, trials - 1)))
    val membership = TrialMembership.make(Vector.tabulate(trials)(i => i % conditions), conditions)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val frame = SamplingFrame(blockLens = Seq(rows), tr = Seq(1.0))
    val expanded = ExpandedTrialDesign
      .lower(onsets, Vector.fill(trials)(0), Vector.fill(trials)(Seconds(0.0)), membership, frame, basis, Seconds(0.1))
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val nuisance = DMat.tabulate(rows, nuisanceColumns): (t, j) =>
      val x = (t.toDouble / (rows - 1)) * 2.0 - 1.0
      j match
        case 0 => 1.0
        case 1 => x
        case 2 => x * x - 1.0 / 3.0
        case k => math.cos(math.Pi * (k - 2) * (t + 0.5) / rows)
    val whitening = WhiteningPlan.global(ArmaCoefficients.ar(0.3), Vector(TimeSegment(0, rows, 0)))
    preparation = TrialBandedPreparation.prepare(expanded, Some(whitening), Some(nuisance), lambda)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    objective = preparation.objective(NodeGrid(family.chart, Vector(2, 2, 2)))
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val blockVoxels = math.min(voxels, 256)
    val raw = new Array[Double](rows * blockVoxels)
    val trialDesign = designAt(expanded, ShapePoint.unsafe(Vector(math.log(0.5), 0.0, 0.3)))
    val conditionDesign = new Array[Double](rows * conditions)
    var t = 0
    while t < rows do
      var trial = 0
      while trial < trials do
        conditionDesign(t * conditions + membership.conditionOfTrial(trial)) += trialDesign(t * trials + trial)
        trial += 1
      t += 1
    var v = 0
    while v < blockVoxels do
      val beta = Array.tabulate(conditions)(_ => (if responseRng.nextBoolean() then 1.0 else -1.0) * (0.5 + 1.5 * responseRng.nextDouble()))
      var signal2 = 0.0
      t = 0
      while t < rows do
        var signal = 0.0
        var condition = 0
        while condition < conditions do
          signal += conditionDesign(t * conditions + condition) * beta(condition)
          condition += 1
        raw(t * blockVoxels + v) = signal
        signal2 += signal * signal
        t += 1
      val signalSd = math.sqrt(signal2 / rows)
      val innovationSd = 2.0 * signalSd * math.sqrt(1.0 - 0.3 * 0.3)
      var noise = responseRng.nextGaussian() * 2.0 * signalSd
      t = 0
      while t < rows do
        if t > 0 then noise = 0.3 * noise + responseRng.nextGaussian() * innovationSd
        raw(t * blockVoxels + v) += noise + 4.0 * signalSd
        t += 1
      v += 1
    val whitened = preparation.whitenResponses(blockVoxels, raw)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    responseColumns = new Array[Double](rows * blockVoxels)
    v = 0
    while v < blockVoxels do
      t = 0
      while t < rows do
        responseColumns(v * rows + t) = whitened(t * blockVoxels + v)
        t += 1
      v += 1
    encoded = preparation.newResponseBuffer
    trialAmplitudes = new Array[Double](trials)
    floatOutput = new Array[Float](trials)
    firstJet = new ProfileJetBuffer(family.dimension, conditions)
    secondJet = new ProfileJetBuffer(family.dimension, conditions)
    exactCoordinates = Vector(math.log(0.5), 0.0, 0.3)
    setupElapsedMillis = (System.nanoTime() - started) / 1e6

  /** Trial-basis response scores only. */
  @Benchmark
  def projection(): Double =
    var checksum = 0.0
    var v = 0
    while v < voxels do
      val offset = (v % inputBlockVoxels) * rows
      val value = preparation.encodeWhitenedInto(responseColumns, offset, encoded)
        .fold(error => throw new IllegalStateException(error.message), identity)
      checksum += value.responseEnergy
      v += 1
    checksum

  /** Eight prepared values, two prepared analytic jets, one amplitude
    * correction and streamed Float32 trial output per voxel.
    */
  @Benchmark
  def completeBackend(): Double = runComplete(voxels)

  /** Fixed-shape denominator: response projection, prepared readout and the
    * same streamed Float32 payload, without shape search or jets.
    */
  @Benchmark
  def fixedShape(): Double = runFixed(voxels)

  /** One-reference ratio cell: one value, one analytic jet, one prepared
    * amplitude correction and the same streamed Float32 payload.
    */
  @Benchmark
  def oneReference(): Double = runOneReference(voxels)

  /** Complete workload with the per-voxel exact-shape readout factor enabled. */
  @Benchmark
  def completeExperimentalExact(): Double = runCompleteExact(voxels)

  /** Explicit experimental mode: one exact-shape factor and readout per voxel. */
  @Benchmark
  def experimentalExactReadout(): Double = runExact(voxels)

  def preparationMillis: Double = setupElapsedMillis
  def basisRank: Int = preparation.basisRank
  def bandwidth: Int = preparation.bandwidth
  def gramBlocks: Int = preparation.gramBlockCount
  def engineBytes(workers: Int, exactFactor: Boolean): Long =
    val transientReference = if exactFactor then objective.estimatedValueBuildBytes else 0L
    objective.estimatedSharedBytes + workers.toLong * (objective.estimatedWorkerBytes + activeWorkspaceBytes + transientReference)
  def retainedInputBytes: Long = responseColumns.length.toLong * 8L
  def sourceDesignBytes: Long = rows.toLong * trials * preparation.basisRank * 8L
  def preparationPeakBytes: Long =
    2L * sourceDesignBytes + preparation.receipt.estimatedBytes + rows.toLong * nuisanceColumns * 8L
  def conservativeLiveBytes(workers: Int, exactFactor: Boolean): Long =
    engineBytes(workers, exactFactor) + retainedInputBytes + sourceDesignBytes
  def outputBytes(count: Int): Long = count.toLong * trials * 4L
  def snapshot: TrialBandedWorkSnapshot = objective.work.snapshot

  private def inputBlockVoxels: Int = math.min(voxels, 256)

  private def activeWorkspaceBytes: Long =
    8L * (trials.toLong * preparation.basisRank + nuisanceColumns + trials) + 4L * trials

  private def runComplete(count: Int): Double =
    runCompleteRange(objective, encoded, trialAmplitudes, floatOutput, firstJet, secondJet, 0, count, exactReadout = false)

  private def runCompleteExact(count: Int): Double =
    runCompleteRange(objective, encoded, trialAmplitudes, floatOutput, firstJet, secondJet, 0, count, exactReadout = true)

  private def runCompleteRange(
      worker: TrialBandedObjective,
      responseBuffer: TrialBandedResponse,
      amplitudes: Array[Double],
      output: Array[Float],
      jetA: ProfileJetBuffer,
      jetB: ProfileJetBuffer,
      fromVoxel: Int,
      untilVoxel: Int,
      exactReadout: Boolean
  ): Double =
    var checksum = 0.0
    var v = fromVoxel
    while v < untilVoxel do
      val offset = (v % inputBlockVoxels) * rows
      val value = preparation.encodeWhitenedInto(responseColumns, offset, responseBuffer)
        .fold(error => throw new IllegalStateException(error.message), identity)
      worker.pointAt(value)
      var bestNode = 0
      var secondNode = 1
      var best = Double.PositiveInfinity
      var second = Double.PositiveInfinity
      var node = 0
      while node < bankNodes do
        val energy = worker.scoreNode(node)
        if energy < best then
          second = best
          secondNode = bestNode
          best = energy
          bestNode = node
        else if energy < second then
          second = energy
          secondNode = node
        node += 1
      if !worker.jetAtNode(bestNode, jetA) then checksum += 1.0
      if !worker.jetAtNode(secondNode, jetB) then checksum += 1.0
      val readoutMode =
        if exactReadout then TrialReadoutFactorMode.ExactShape(exactCoordinates)
        else TrialReadoutFactorMode.PreparedNode(bestNode)
      checksum += worker.readoutInto(readoutMode, amplitudes)
        .fold(error => throw new IllegalStateException(error.message), identity)
      var trial = 0
      while trial < trials do
        val amplitude = amplitudes(trial).toFloat
        output(trial) = amplitude
        checksum += amplitude * 1e-12
        trial += 1
      checksum += jetA.energy * 1e-12 + jetB.energy * 1e-12
      v += 1
    checksum

  private def runExact(count: Int): Double =
    runExactRange(objective, encoded, trialAmplitudes, 0, count)

  private def runFixed(count: Int): Double =
    runFixedRange(objective, encoded, trialAmplitudes, floatOutput, 0, count)

  private def runOneReference(count: Int): Double =
    runOneReferenceRange(objective, encoded, trialAmplitudes, floatOutput, firstJet, 0, count)

  private def runOneReferenceRange(
      worker: TrialBandedObjective,
      responseBuffer: TrialBandedResponse,
      amplitudes: Array[Double],
      output: Array[Float],
      jet: ProfileJetBuffer,
      fromVoxel: Int,
      untilVoxel: Int
  ): Double =
    var checksum = 0.0
    var v = fromVoxel
    while v < untilVoxel do
      val offset = (v % inputBlockVoxels) * rows
      val value = preparation.encodeWhitenedInto(responseColumns, offset, responseBuffer)
        .fold(error => throw new IllegalStateException(error.message), identity)
      worker.pointAt(value)
      checksum += worker.scoreNode(0)
      if !worker.jetAtNode(0, jet) then checksum += 1.0
      checksum += worker.readoutInto(TrialReadoutFactorMode.PreparedNode(0), amplitudes)
        .fold(error => throw new IllegalStateException(error.message), identity)
      var trial = 0
      while trial < trials do
        val amplitude = amplitudes(trial).toFloat
        output(trial) = amplitude
        checksum += amplitude * 1e-12
        trial += 1
      v += 1
    checksum

  private def runFixedRange(
      worker: TrialBandedObjective,
      responseBuffer: TrialBandedResponse,
      amplitudes: Array[Double],
      output: Array[Float],
      fromVoxel: Int,
      untilVoxel: Int
  ): Double =
    var checksum = 0.0
    var v = fromVoxel
    while v < untilVoxel do
      val offset = (v % inputBlockVoxels) * rows
      val value = preparation.encodeWhitenedInto(responseColumns, offset, responseBuffer)
        .fold(error => throw new IllegalStateException(error.message), identity)
      worker.pointAt(value)
      checksum += worker.readoutInto(TrialReadoutFactorMode.PreparedNode(0), amplitudes)
        .fold(error => throw new IllegalStateException(error.message), identity)
      var trial = 0
      while trial < trials do
        val amplitude = amplitudes(trial).toFloat
        output(trial) = amplitude
        checksum += amplitude * 1e-12
        trial += 1
      v += 1
    checksum

  private def runExactRange(
      worker: TrialBandedObjective,
      responseBuffer: TrialBandedResponse,
      amplitudes: Array[Double],
      fromVoxel: Int,
      untilVoxel: Int
  ): Double =
    var checksum = 0.0
    var v = fromVoxel
    while v < untilVoxel do
      val offset = (v % inputBlockVoxels) * rows
      val value = preparation.encodeWhitenedInto(responseColumns, offset, responseBuffer)
        .fold(error => throw new IllegalStateException(error.message), identity)
      worker.pointAt(value)
      checksum += worker.readoutInto(TrialReadoutFactorMode.ExactShape(exactCoordinates), amplitudes)
        .fold(error => throw new IllegalStateException(error.message), identity)
      v += 1
    checksum

  private def runParallel(count: Int, parallelism: Int, mode: String): (Double, TrialBandedWorkSnapshot) =
    require(parallelism >= 1 && parallelism <= 8, s"workers must be 1..8, got $parallelism")
    val workerObjects = Array.tabulate(parallelism)(_ => objective.newWorker())
    val responseBuffers = Array.tabulate(parallelism)(_ => preparation.newResponseBuffer)
    val amplitudeBuffers = Array.fill(parallelism)(new Array[Double](trials))
    val floatBuffers = Array.fill(parallelism)(new Array[Float](trials))
    val firstJets = Array.fill(parallelism)(new ProfileJetBuffer(3, conditions))
    val secondJets = Array.fill(parallelism)(new ProfileJetBuffer(3, conditions))
    val checksums = new Array[Double](parallelism)
    val failures = new Array[Throwable | Null](parallelism)
    val threads = Array.tabulate(parallelism): workerIndex =>
      val from = count * workerIndex / parallelism
      val until = count * (workerIndex + 1) / parallelism
      new Thread(
        () =>
          try
            checksums(workerIndex) =
              if mode == "exact" then runExactRange(workerObjects(workerIndex), responseBuffers(workerIndex), amplitudeBuffers(workerIndex), from, until)
              else if mode == "fixed" then runFixedRange(workerObjects(workerIndex), responseBuffers(workerIndex), amplitudeBuffers(workerIndex),
                floatBuffers(workerIndex), from, until)
              else if mode == "one" then runOneReferenceRange(workerObjects(workerIndex), responseBuffers(workerIndex), amplitudeBuffers(workerIndex),
                floatBuffers(workerIndex), firstJets(workerIndex), from, until)
              else runCompleteRange(workerObjects(workerIndex), responseBuffers(workerIndex), amplitudeBuffers(workerIndex),
                floatBuffers(workerIndex), firstJets(workerIndex), secondJets(workerIndex), from, until,
                exactReadout = mode == "complete-exact")
          catch case error: Throwable => failures(workerIndex) = error,
        s"trial-banded-$workerIndex"
      )
    threads.foreach(_.start())
    threads.foreach(_.join())
    failures.collectFirst { case error: Throwable => error }.foreach(throw _)
    val work = workerObjects.iterator.map(_.work.snapshot).foldLeft(TrialBandedBenchmark.zeroWork)(TrialBandedBenchmark.addWork)
    (checksums.sum, work)

  private def designAt(expanded: ExpandedTrialDesign, point: ShapePoint): Array[Double] =
    val coefficients = new Array[Double](expanded.rank)
    expanded.basis.coefficientsInto(point, new Array[Double](expanded.basis.fineCount), coefficients)
    val out = new Array[Double](expanded.rows * expanded.trials)
    val source = expanded.term.data.data
    var t = 0
    while t < expanded.rows do
      var trial = 0
      while trial < expanded.trials do
        var sum = 0.0
        var p = 0
        while p < expanded.rank do
          sum += source(t * expanded.columns + p * expanded.trials + trial) * coefficients(p)
          p += 1
        out(t * expanded.trials + trial) = sum
        trial += 1
      t += 1
    out

object TrialBandedBenchmark:

  private lazy val checkpointBasis: HrfKernelBasis =
    val step = PositiveSeconds(0.1).fold(error => throw new IllegalArgumentException(error.message), identity)
    HrfKernelBasis
      .compile(KernelBasisSpec(Cascade34Family.Default, step, Vector(9, 9, 7), tolerance = 1e-3, maxRank = 32))
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private val zeroWork = TrialBandedWorkSnapshot(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)

  private def addWork(left: TrialBandedWorkSnapshot, right: TrialBandedWorkSnapshot): TrialBandedWorkSnapshot =
    TrialBandedWorkSnapshot(
      left.voxels + right.voxels,
      left.trialBasisScores + right.trialBasisScores,
      left.bankValueEvaluations + right.bankValueEvaluations,
      left.jetEvaluations + right.jetEvaluations,
      left.amplitudeCorrections + right.amplitudeCorrections,
      left.bandedSolveCalls + right.bandedSolveCalls,
      left.bandedRightHandSides + right.bandedRightHandSides,
      left.continuousFactors + right.continuousFactors,
      left.exactReadoutFactors + right.exactReadoutFactors
    )

  /** One-shot checkpoint runner. Args: trials, voxels, schedule, mode
    * (`complete` or `exact`). It deliberately reports setup separately.
    */
  def main(args: Array[String]): Unit =
    val state = new TrialBandedBenchmark
    state.trials = args.headOption.fold(300)(_.toInt)
    state.voxels = args.lift(1).fold(100000)(_.toInt)
    state.schedule = args.lift(2).getOrElse("dense")
    val mode = args.lift(3).getOrElse("complete")
    require(Set("complete", "complete-exact", "fixed", "one", "exact").contains(mode), s"unknown mode $mode")
    val workers = args.lift(4).fold(1)(_.toInt)
    require(workers >= 1 && workers <= 8, s"workers must be 1..8, got $workers")
    state.setup()
    val warmup = math.min(state.voxels, 32)
    if mode == "exact" then state.runExact(warmup)
    else if mode == "fixed" then state.runFixed(warmup)
    else if mode == "one" then state.runOneReference(warmup)
    else if mode == "complete-exact" then state.runCompleteExact(warmup)
    else state.runComplete(warmup)
    val started = System.nanoTime()
    val (checksum, work) = state.runParallel(state.voxels, workers, mode)
    val elapsedMillis = (System.nanoTime() - started) / 1e6
    println(
      s"trial-banded-checkpoint/v1 trials=${state.trials} voxels=${state.voxels} schedule=${state.schedule} mode=$mode workers=$workers " +
        f"basisRank=${state.basisRank}%d bandwidth=${state.bandwidth}%d gramBlocks=${state.gramBlocks}%d " +
        f"preparationMs=${state.preparationMillis}%.3f elapsedMs=$elapsedMillis%.3f " +
        s"engineBytes=${state.engineBytes(workers, mode == "exact" || mode == "complete-exact")} " +
        s"retainedInputBytes=${state.retainedInputBytes} sourceDesignBytes=${state.sourceDesignBytes} " +
        s"preparationPeakBytes=${state.preparationPeakBytes} " +
        s"conservativeLiveBytes=${state.conservativeLiveBytes(workers, mode == "exact" || mode == "complete-exact")} outputBytes=${state.outputBytes(state.voxels)} " +
        s"voxelsCounted=${work.voxels} trialBasisScores=${work.trialBasisScores} " +
        s"bankValues=${work.bankValueEvaluations} jets=${work.jetEvaluations} " +
        s"amplitudeCorrections=${work.amplitudeCorrections} " +
        s"solveCalls=${work.bandedSolveCalls} solveRhs=${work.bandedRightHandSides} " +
        s"continuousFactors=${work.continuousFactors} exactReadoutFactors=${work.exactReadoutFactors} " +
        f"checksum=$checksum%.9g"
    )
