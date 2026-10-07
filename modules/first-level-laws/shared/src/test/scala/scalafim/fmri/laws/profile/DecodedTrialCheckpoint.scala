package scalafim.fmri.laws.profile

import gale.linalg.{DMat, DVec}
import scalafim.dataset.*
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan, WhiteningTransform}
import scalafim.fmri.design.{ConditionId, TrialId}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept, NuisanceCheck}
import scalafim.fmri.design.event.EventSchedule
import scalafim.fmri.design.hrf.{
  TrialBasisDesign,
  HrfKernelBasis,
  KernelBasisCompilation,
  KernelBasisSpec,
  TrialMembership
}
import scalafim.fmri.fit.CanonicalTemporalWhitening
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{Cascade34Family, GaussianFamily, NormalizationRule, ShapePoint}
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig, ProfileCriterion, ProfileHrfPlan, ProfileTrialDrive}
import scalafim.image.SampleSpaces

/** Diagnostic public-path workload. It cannot qualify PHRF-33 while original-family certification is unavailable.
  * Fixture construction, preparation, execution and sink work remain separate measurements; no truth coordinates enter
  * the decoder.
  */
object DecodedTrialCheckpoint:
  val repairedBudget: DecodeBudget = DecodeBudget(
    maxNewtonSteps = 16,
    maxJets = 901,
    maxExactEvaluations = 40,
    maxCandidateAttempts = 30,
    stationarityStepTolerance = 1e-6,
    initialization = DecodeInitialization.BoundedMultistart
  )

  enum Geometry:
    case Tiny, B0Dense, B0Regular

  final case class Config(
      geometry: Geometry,
      voxels: Int,
      trials: Int = 300,
      blockSize: Int = 256,
      workers: Int = 1,
      gridAxisNodes: Int = 2,
      compilation: KernelBasisCompilation = KernelBasisCompilation.Dense,
      criterion: ProfileCriterion = ProfileCriterion.PenalizedProfile(1.0),
      mode: ProfileTrialReadoutMode = ProfileTrialReadoutMode.ExactShape,
      budget: DecodeBudget = DecodeBudget(),
      trialPreparation: TrialPreparationPolicy = TrialPreparationPolicy(),
      noiseRatio: Option[Double] = None
  ):
    require(voxels > 0 && trials >= 3 && trials % 3 == 0)
    require(gridAxisNodes >= 2)
    require(blockSize >= 1 && blockSize <= 256 && workers >= 1 && workers <= 8)
    require(noiseRatio.forall(x => x.isFinite && x >= 0.0))

  final case class Fixture(
      config: Config,
      dataset: FmriDataset,
      plan: ProfileHrfPlan,
      baseline: BaselineModel,
      expanded: TrialBasisDesign,
      whitening: WhiteningPlan,
      rawBlock: Array[Double],
      inputBlockVoxels: Int,
      basisNanos: Long,
      fixtureNanos: Long
  ):
    val rows: Int = dataset.shape.timepoints
    val trials: Int = expanded.trials
    val conditions: Int = expanded.membership.conditionCount
    val nuisance: Int = baseline.designMatrix.cols
    val inputBytes: Long = rawBlock.length.toLong * 8L

    /** Distinct counters and series allocation per reader; immutable input block shared. */
    final class Reader extends DatasetSeriesReader:
      val dataset: FmriDataset = Fixture.this.dataset
      var calls: Long = 0L
      var values: Long = 0L
      var nanos: Long = 0L
      var largestSeriesValues: Long = 0L
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        val started = System.nanoTime()
        try
          dataset
            .resolve(selection)
            .flatMap: selected =>
              calls += 1
              // These accessors materialize vectors. Resolve them once per
              // block, rather than allocating both again for every matrix cell.
              val selectedTimes = selected.timepoints
              val selectedVoxels = selected.voxels
              val count = selectedTimes.length.toLong * selectedVoxels.length
              values += count
              largestSeriesValues = math.max(largestSeriesValues, count)
              FmriSeries.make(
                DMat.tabulate(selectedTimes.length, selectedVoxels.length)((t, v) =>
                  rawBlock(selectedTimes(t) * inputBlockVoxels + selectedVoxels(v) % inputBlockVoxels)
                ),
                selected.voxelIndexValues,
                selected.timepointIndices,
                dataset.shape
              )
        finally nanos += System.nanoTime() - started

    def prepare: Either[ProfileFitError, PreparedProfileHrf] =
      ProfileHrfFit.prepare(
        plan,
        DataSelection.All,
        CanonicalTemporalWhitening.Shared(whitening),
        ProfileDecodePolicy(
          Vector.fill(plan.basis.family.dimension)(config.gridAxisNodes),
          config.budget,
          None,
          ExecutionBudget(config.blockSize, config.workers),
          trialPreparation = config.trialPreparation
        )
      )

    /** Independent augmented least-squares oracle for the prepared basis at the returned shape. This is empirical
      * same-model evidence, not a certificate against the original HRF family. The penalty is sqrt(lambda) (I - P_M).
      */
    def oracle(voxel: Int, coordinates: Vector[Double]): Vector[Double] =
      oracleDesign(voxel, designAt(expanded, coordinates))

    def oracleDesign(voxel: Int, x: Array[Double]): Vector[Double] =
      require(x.length == rows * trials)
      val design = DMat.tabulate(rows, trials + nuisance)((t, j) =>
        if j < trials then x(t * trials + j) else baseline.designMatrix(t, j - trials)
      )
      val wx =
        WhiteningTransform.matrix(whitening, design).fold(e => throw new IllegalArgumentException(e.toString), identity)
      val y = DMat.tabulate(rows, 1)((t, _) => rawBlock(t * inputBlockVoxels + voxel % inputBlockVoxels))
      val wy =
        WhiteningTransform.matrix(whitening, y).fold(e => throw new IllegalArgumentException(e.toString), identity)
      val augmented = DMat.tabulate(rows + trials, trials + nuisance)((t, j) =>
        if t < rows then wx(t, j)
        else if j >= trials then 0.0
        else
          val trial = t - rows
          val condition = expanded.membership.conditionOfTrial(trial)
          (if trial == j then 1.0 else 0.0) -
            (if expanded.membership.conditionOfTrial(j) == condition then
               1.0 / expanded.membership.trialsOf(condition).length
             else 0.0)
      )
      val rhs = DVec.fromSeq(Vector.tabulate(rows + trials)(t => if t < rows then wy(t, 0) else 0.0))
      val solved = augmented.leastSquares(rhs).fold(throw _, identity)
      Vector.tabulate(trials + nuisance)(solved(_))

  /** Sink consumes every delivered status; only accepted amplitudes are converted. No amplitude or decoded-coordinate
    * collection grows with voxel count.
    */
  final class Sink(val trials: Int) extends BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
    var attempted: Long = 0L
    var emitted: Long = 0L
    var outputValues: Long = 0L
    var outputChecksum: Double = 0.0
    var maxFloatError: Double = 0.0
    var maxPreparedResidual: Double = 0.0
    var offNode: Long = 0L
    var nanos: Long = 0L
    var statuses: Map[DecodeStatus, Long] = Map.empty
    var exits: Map[DecodeBudgetExit, Long] = Map.empty
    private val buffer = new Array[Float](trials)
    val retainedOutputBytes: Long = trials.toLong * 4L

    def accept(block: VoxelBlock, payload: ProfileTrialOutputBlock): Either[String, ProfileFitReceipt] =
      val started = System.nanoTime()
      try
        if block.index != payload.ordinal || payload.voxelIds != payload.results.map(_.voxelId) then
          Left("output block identity mismatch")
        else
          var error: Option[String] = None
          payload.results.foreach: voxel =>
            attempted += 1
            val selected = voxel.selection
            statuses = statuses.updated(selected.status, statuses.getOrElse(selected.status, 0L) + 1L)
            selected.budgetExit.foreach(exit => exits = exits.updated(exit, exits.getOrElse(exit, 0L) + 1L))
            voxel.output match
              case ProfileTrialOutputOutcome.DecodeRefused(status) =>
                if status == DecodeStatus.Accepted || status != selected.status then error = Some("incoherent refusal")
              case ProfileTrialOutputOutcome.Emitted(reference, value) =>
                if selected.status != DecodeStatus.Accepted || value.actualCoordinates != selected.coordinates then
                  error = Some("readout does not belong to accepted decoded coordinates")
                if reference.coordinates != selected.coordinates then offNode += 1
                value.trialAmplitudes match
                  case None             => error = Some("missing requested trial amplitudes")
                  case Some(amplitudes) =>
                    if amplitudes.length != trials then error = Some("wrong trial amplitude axis")
                    else
                      emitted += 1
                      var i = 0
                      while i < trials do
                        val amplitude = amplitudes(i)
                        val converted = amplitude.toFloat
                        if !amplitude.isFinite || !converted.isFinite then error = Some("nonfinite output")
                        buffer(i) = converted
                        maxFloatError = math.max(maxFloatError, math.abs(amplitude - converted.toDouble))
                        outputChecksum += converted.toDouble * (1.0 + (i % 7).toDouble / 8.0)
                        outputValues += 1
                        i += 1
                val residual = value.evidence.preparedBasisNormalResidualNorm
                if !residual.isFinite then error = Some("nonfinite prepared-basis residual")
                maxPreparedResidual = math.max(maxPreparedResidual, residual)
          error.toLeft(ProfileFitReceipt(payload.ordinal, payload.voxelIds))
      finally nanos += System.nanoTime() - started

  def request: OutputRequest = OutputRequest.TrialAmplitudes(NormalizationRule.Unnormalised)

  def fixture(config: Config): Fixture =
    val started = System.nanoTime()
    val tiny = config.geometry == Geometry.Tiny
    val rows = if tiny then 120 else 600
    val trials = if tiny then 12 else config.trials
    val family = if tiny then GaussianFamily.Default else Cascade34Family.Default
    val frame = SamplingFrame(blockLens = Seq(rows), tr = Seq(1.0))
    val step = PositiveSeconds.unsafe(Seconds(0.1))
    val basisStarted = System.nanoTime()
    val basis = HrfKernelBasis
      .compile(
        KernelBasisSpec(
          family,
          step,
          if tiny then Vector(26, 21) else Vector(9, 9, 7),
          tolerance = 1e-3,
          maxRank = 32,
          compilation = config.compilation
        )
      )
      .fold(e => throw new IllegalArgumentException(e.message), identity)
    val basisNanos = System.nanoTime() - basisStarted
    val onsetRng = new scala.util.Random(20260910L)
    val lastOnset = rows - family.horizon.value - 1.0
    val onsets =
      if config.geometry == Geometry.B0Dense then
        Vector.fill(trials)(math.floor(onsetRng.nextDouble() * lastOnset * 10.0) / 10.0).sorted.map(Seconds(_))
      else Vector.tabulate(trials)(i => Seconds(lastOnset * i / (trials - 1)))
    val membership = TrialMembership
      .make(Vector.tabulate(trials)(_ % 3), 3)
      .fold(e => throw new IllegalArgumentException(e.message), identity)
    val schedule = EventSchedule
      .fromParts(onsets, Vector.fill(trials)(Seconds(0.0)), Vector.fill(trials)(0))
      .fold(e => throw new IllegalArgumentException(e.message), identity)
    val labels = Vector("A", "B", "C").map(ConditionId.unsafe)
    val drive = ProfileTrialDrive
      .make(
        schedule,
        membership,
        labels,
        Vector.tabulate(trials)(i => TrialId.unsafe(s"trial-$i")),
        membership.conditionOfTrial.map(labels)
      )
      .fold(e => throw new IllegalArgumentException(e.message), identity)
    val expanded = TrialBasisDesign
      .lower(onsets, schedule.blockIds, schedule.durations, membership, frame, basis, Seconds(0.1), config.trialPreparation.lowering)
      .fold(e => throw new IllegalArgumentException(e.message), identity)
    val nuisanceValues = Array.tabulate(rows * 5): index =>
      val t = index / 5
      val j = index % 5 + 1
      val x = t.toDouble / (rows - 1) * 2.0 - 1.0
      j match
        case 1 => x
        case 2 => x * x - 1.0 / 3.0
        case k => math.cos(math.Pi * (k - 2) * (t + 0.5) / rows)
    val baseline = BaselineModel.build(
      frame,
      BaselineBasis.Constant,
      intercept = Intercept.Global,
      nuisanceList = Some(Seq(Mat.unsafe(rows, 5, nuisanceValues))),
      nuisanceCheck = NuisanceCheck.Error,
      nuisanceNames = Some(Seq(Vector("linear", "quadratic", "cos1", "cos2", "cos3")))
    )
    require(baseline.designMatrix.cols == 6, "frozen workload has six nuisance columns")
    val shape = DatasetShape.unsafe(SampleSpaces(Vector(config.voxels, 1, 1)), rows)
    val dataset = FmriDataset
      .describe(
        DatasetId("decoded-trial-checkpoint"),
        shape,
        VoxelDomain.fullUnsafe(shape),
        DatasetMetadata.Empty,
        frame,
        RunId("run-1")
      )
      .fold(e => throw new IllegalArgumentException(e.message), identity)
    val ar = WhiteningPlan.global(ArmaCoefficients.ar(0.3), Vector(TimeSegment(0, rows, 0)))
    val fitConfig =
      FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), global = true, rho = Some(0.3)))
    val plan = ProfileHrfPlan
      .fromTrialEvents(dataset, drive, baseline, fitConfig, basis, 1.0, config.criterion)
      .fold(e => throw new IllegalArgumentException(e.message), identity)
    val coordinates = if tiny then Vector(5.3, math.log(1.55)) else Vector(math.log(0.5), 0.0, 0.3)
    val design = designAt(expanded, coordinates)
    val conditionDesign = new Array[Double](rows * 3)
    var row = 0
    while row < rows do
      var trial = 0
      while trial < trials do
        conditionDesign(row * 3 + trial % 3) += design(row * trials + trial)
        trial += 1
      row += 1
    val blockVoxels = math.min(config.voxels, 256)
    val raw = new Array[Double](rows * blockVoxels)
    val rng = new scala.util.Random(20260911L)
    var v = 0
    while v < blockVoxels do
      val means = Array.tabulate(3)(_ => (if rng.nextBoolean() then 1.0 else -1.0) * (0.5 + 1.5 * rng.nextDouble()))
      var signal2 = 0.0
      var t = 0
      while t < rows do
        var signal = 0.0
        var i = 0
        while i < 3 do
          signal += conditionDesign(t * 3 + i) * means(i)
          i += 1
        raw(t * blockVoxels + v) = signal
        signal2 += signal * signal
        t += 1
      val signalSd = math.sqrt(signal2 / rows)
      val noiseSd = config.noiseRatio.getOrElse(if tiny then 0.01 else 2.0) * signalSd
      val innovationSd = noiseSd * math.sqrt(1.0 - 0.3 * 0.3)
      var noise = rng.nextGaussian() * noiseSd
      t = 0
      while t < rows do
        if t > 0 then noise = 0.3 * noise + rng.nextGaussian() * innovationSd
        raw(t * blockVoxels + v) += noise + 4.0 * signalSd
        t += 1
      v += 1
    Fixture(config, dataset, plan, baseline, expanded, ar, raw, blockVoxels, basisNanos, System.nanoTime() - started)

  private[profile] def designAt(expanded: TrialBasisDesign, coordinates: Vector[Double]): Array[Double] =
    val coefficients = new Array[Double](expanded.rank)
    expanded.basis.coefficientsInto(
      ShapePoint.unsafe(coordinates),
      new Array[Double](expanded.basis.fineCount),
      coefficients
    )
    val out = new Array[Double](expanded.rows * expanded.trials)
    var block = 0
    while block < expanded.blockCount do
      val source = expanded.block(block).fold(e => throw new IllegalArgumentException(e.message), identity)
      val count = expanded.trialsInBlock(block)
      var t = 0
      while t < expanded.rows do
        var local = 0
        while local < count do
          var value = 0.0
          var p = 0
          while p < expanded.rank do
            value += source(t, p * count + local) * coefficients(p)
            p += 1
          out(t * expanded.trials + block * expanded.trialsPerBlock + local) = value
          local += 1
        t += 1
      block += 1
    out
