package scalafim.phrfcmp.run

import scalafim.fmri.design.{ColumnId, ScanIndex}
import scalafim.fmri.design.hrf.HrfKernelBasis
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.family.NormalizationRule
import scalafim.fmri.model.PositiveAlpha
import scalafim.phrfcmp.ingest.{FitInputs, Matrix}
import scalafim.phrfcmp.prep.CommonPrep

/** PHRF-can timing-probe result: not scored in the pilot (design 2.2), reported for its LOROCV selection and stage seconds. */
final case class PhrfCanProbeResult(
    shape: PhrfCanonicalShape,
    tuning: PhrfTuningRecord,
    selectedAlpha: Double,
    finalAmplitudes: Matrix,
    finalETrial: Vector[Vector[Double]],
    timings: PhrfTimings
)

/**
  * PHRF-can (ablation, "shrinkage at the canonical shape"): the trial-banded PHRF readout with the shape FIXED at the
  * canonical point ([[PhrfCanonicalShape]]) and the same alpha grid, folds, fold score and tie rule as PHRF and rLSS. It
  * decodes no shape: per alpha it prepares the banded problem, freezes `ProfileTrialReadout` (ExactShape) at the
  * canonical coordinates and reads every voxel's trial amplitudes conditionally on that shape. Two datasets only, as a
  * timing probe; nothing here is scored.
  */
object PhrfCanProbe:

  /** Index of the bank node nearest to `coordinates` (strict comparison keeps the lower node at a half step, as PHRF's own reference selection). */
  private[run] def nearestNode(grid: NodeGrid, coordinates: Vector[Double]): Int =
    val indices = new Array[Int](grid.dimension)
    var d = 0
    while d < indices.length do
      val position = (coordinates(d) - grid.chart.lower(d)) / grid.step(d)
      val lower = math.max(0, math.min(grid.nodesPerAxis(d) - 1, math.floor(position).toInt))
      val upper = math.min(grid.nodesPerAxis(d) - 1, lower + 1)
      indices(d) = if position - lower > upper - position then upper else lower
      d += 1
    grid.indexOf(indices)

  /** One fixed-shape fit of `problem` at `alpha`: amplitudes are raw coefficients of the unnormalised kernel (as [[PhrfTrainFit]]). */
  def fixedShapeFit(
      problem: PhrfTrialProblem,
      basis: HrfKernelBasis,
      shape: PhrfCanonicalShape,
      alpha: Double,
      config: PhrfTrialConfig,
      clock: PhrfClock,
      y: Matrix
  ): Either[PhrfTrialRefusal, (PhrfTrainFit, PhrfStageSeconds)] =
    def dataset(stage: String)(e: Any) = PhrfTrialRefusal.Dataset(stage, PhrfTrialAssembly.fmt(e))
    for
      positive <- PositiveAlpha(alpha).left.map(_ => PhrfTrialRefusal.AlphaNotPositive(alpha))
      expanded <- PhrfTrialAssembly.expandedOf(problem, basis)
      t0 = clock.nanos()
      tb <- TrialBandedPreparation
        .prepare(expanded, Some(problem.spec.plan), Some(PhrfTrialAssembly.baselineMat(problem)), positive.lambda)
        .left.map(dataset("prepare"))
      grid = NodeGrid(basis.family.chart, Vector.fill(basis.family.dimension)(config.fixedShapeNodes))
      bank <- tb.objective(grid).left.map(dataset("bank"))
      axis <- ProfileTrialAxis
        .make(
          expanded,
          tb,
          problem.drive.trialLabels,
          problem.drive.conditionLabels,
          problem.drive.conditionForTrial,
          Vector.tabulate(tb.nuisanceColumns)(j => ColumnId.unsafe(s"nuisance$j")),
          Vector.tabulate(tb.rows)(i => ScanIndex.unsafeOneBased(i + 1))
        )
        .left.map(dataset("axis"))
      readout <- ProfileTrialReadout
        .freeze(bank, axis, shape.coordinates, nearestNode(grid, shape.coordinates), NormalizationRule.Density, ProfileTrialReadoutMode.ExactShape)
        .left.map(dataset("freeze"))
      prepSeconds = (clock.nanos() - t0) / 1e9
      t1 = clock.nanos()
      worker = readout.newWorker()
      n = problem.trials
      v = problem.voxels
      data = new Array[Double](n * v)
      _ <- readVoxels(problem, axis, worker, y, data)
      amplitudes <- Matrix.of(n, v, data).left.map(e => PhrfTrialRefusal.Inconsistent(e.message))
    yield
      val receipt = PhrfWorkReceipt("can-fit", "trial-banded-fixed-shape", v, v, Vector(("Fixed", v.toLong)), ProfileDecoderWork(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), None, None, 0, false, v.toLong, v.toLong, 0L, 0L)
      (
        new PhrfTrainFit(alpha, Vector.fill(v)(DecodeStatus.Accepted), Vector.fill(v)(shape.coordinates), amplitudes, receipt),
        PhrfStageSeconds(prepSeconds, (clock.nanos() - t1) / 1e9)
      )

  private def readVoxels(
      problem: PhrfTrialProblem,
      axis: ProfileTrialAxis,
      worker: ProfileTrialReadout#Worker,
      y: Matrix,
      data: Array[Double]
  ): Either[PhrfTrialRefusal, Unit] =
    val n = problem.trials
    val v = problem.voxels
    val request = OutputRequest.TrialAmplitudes(NormalizationRule.Density)
    var failure: Option[PhrfTrialRefusal] = None
    var j = 0
    while failure.isEmpty && j < v do
      val values = Array.tabulate(problem.timepoints)(r => y(j, problem.rows(r)))
      val evaluated = for
        response <- ProfileTrialResponse
          .make(axis, axis.selectedResponseRows, ProfileTrialResponseDomain.Original, values)
          .left.map(e => PhrfTrialRefusal.Dataset("response", PhrfTrialAssembly.fmt(e)))
        result <- worker.evaluate(response, request).left.map(e => PhrfTrialRefusal.Dataset("readout", PhrfTrialAssembly.fmt(e)))
        amps <- result.trialAmplitudes.filter(_.length == n).toRight(PhrfTrialRefusal.Setup("readout", "NoTrialAmplitudes"))
      yield amps.map(_ * result.normalizationScale)
      evaluated match
        case Left(e) => failure = Some(e)
        case Right(raw) =>
          var i = 0
          while i < n do
            data(i * v + j) = raw(i)
            i += 1
      j += 1
    failure.toLeft(())

  /** The fold engine of the probe (fixed shape, same grid and shell as PHRF). `y` is the raw `V x T` response matrix. */
  def engine(basis: HrfKernelBasis, shape: PhrfCanonicalShape, config: PhrfTrialConfig, clock: PhrfClock, y: Matrix): PhrfFoldEngine =
    new PhrfFoldEngine:
      def fit(problem: PhrfTrialProblem, alphaIndex: Int, alpha: Double): Either[PhrfTrialRefusal, (PhrfTrainFit, PhrfStageSeconds)] =
        val _ = alphaIndex
        fixedShapeFit(problem, basis, shape, alpha, config, clock, y)

  /** LOROCV over `grid` at the canonical shape, then the all-runs fixed-shape readout at the selected alpha. */
  def run(
      inputs: FitInputs,
      prep: CommonPrep,
      grid: AlphaGrid = AlphaGrid.Pilot,
      config: PhrfTrialConfig = PhrfTrialConfig(),
      clock: PhrfClock = PhrfClock.Wall
  ): Either[PhrfTrialRefusal, PhrfCanProbeResult] =
    for
      _ <- PhrfTrialRunner.checked(inputs, prep)
      basis <- PhrfTrialRunner.basisOf(config)
      shape <- PhrfCanonicalShape.derive(basis.family).left.map(m => PhrfTrialRefusal.Setup("canonical_shape", m.take(40).filter(_.isLetterOrDigit)))
      native <- TrialNativeInputs.from(inputs, prep).left.map(e => PhrfTrialRefusal.Inconsistent(e.code))
      eng = engine(basis, shape, config, clock, inputs.y)
      tuned <- PhrfTrialTuning.tune(inputs, prep, native, grid, config, clock, eng)
      problem <- PhrfTrialAssembly.problem(inputs, prep, prep.segments.map(_.runIndex), prep.sigma2All.sigma2, config)
      fin <- eng.fit(problem, tuned.record.selection.selected, tuned.record.selection.selectedAlpha)
      peak <- PhrfETrial.peakHeight(basis.family, shape.coordinates, config.peakHorizonSeconds, config.peakStepSeconds).left.map(_ => PhrfTrialRefusal.Setup("peak", "Unavailable"))
    yield
      val (fit, secs) = fin
      val eTrial = Vector.tabulate(fit.voxels)(j => Vector.tabulate(problem.trials)(i => fit.amplitudes(i, j) * peak))
      PhrfCanProbeResult(
        shape,
        tuned.record,
        tuned.record.selection.selectedAlpha,
        fit.amplitudes,
        eTrial,
        tuned.timings.withFinal(PhrfTrialRunner.FinalSeconds(secs.prepare, secs.run, 0.0))
      )
