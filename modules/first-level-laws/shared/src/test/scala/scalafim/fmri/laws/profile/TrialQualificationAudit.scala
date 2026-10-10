package scalafim.fmri.laws.profile

import gale.linalg.DMat
import gale.spectral.{MatrixEnclosure, ValidatedSvd}
import scalafim.fmri.ar.WhiteningTransform
import scalafim.fmri.design.event.{CategoricalEvent, EventTerm}
import scalafim.fmri.design.hrf.{KernelBasisCompilation, TrialDesignLowering}
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.{Hrf, Seconds}
import scalafim.fmri.hrf.family.ShapePoint
import scalafim.fmri.model.ProfileHrfSource
import scalafim.scenarios.*

/** Exploratory original-family checks, deliberately separate from scientific admission. The observation oracle
  * evaluates the unnormalised family through the entire acquisition window on the declared 0.1-second convolution grid.
  * This removes the 48-second tail cutoff, but does not certify continuous-time integration.
  */
object TrialQualificationAudit:
  final case class Cell(
      label: String,
      unitShape: Vector[Double],
      noiseRatio: Double,
      trialVariation: Double,
      originalFamily: Boolean
  )
  val shapes: Vector[Vector[Double]] =
    Vector(Vector(0.2, 0.25, 0.15), Vector(0.4, 0.75, 0.7), Vector(0.75, 0.3, 0.45), Vector(0.9, 0.85, 0.9))
  val cells: Vector[Cell] = shapes.zipWithIndex.flatMap: (shape, i) =>
    Vector(
      Cell(s"shape-$i-matched", shape, 0.0, 0.0, false),
      Cell(s"shape-$i-original", shape, 0.0, 0.25, true),
      Cell(s"shape-$i-original-noisy", shape, 0.5, 0.25, true)
    )

  final case class Record(
      cell: String,
      voxel: Int,
      mode: ProfileTrialReadoutMode,
      status: DecodeStatus,
      truth: Vector[Double],
      coordinates: Vector[Double],
      energy: Double,
      shapeErrorInChartUnits: Double,
      emitted: Boolean,
      preparedQrMaxError: Option[Double],
      originalQrMaxError: Option[Double],
      signedQueryAbsoluteError: Option[Double],
      amplitudeTruthRelativeError: Option[Double],
      preparedResidual: Option[Double],
      originalResidual: Option[Double],
      preparedTrialQrMaxError: Option[Double],
      originalTrialQrMaxError: Option[Double],
      originalNuisanceQrMaxError: Option[Double]
  )

  final case class Geometry(
      cell: String,
      relativeBasisErrorWithinHorizon: Double,
      relativeTailDesignError: Double,
      relativeWhitenedDesignError: Double,
      finiteOriginalAugmentedSigmaLower: Double
  )

  final case class Result(records: Vector[Record], geometry: Vector[Geometry], scenarios: Vector[ScenarioResult])

  def fixture(): DecodedTrialCheckpoint.Fixture = DecodedTrialCheckpoint.fixture(
    DecodedTrialCheckpoint.Config(
      DecodedTrialCheckpoint.Geometry.B0Dense,
      voxels = 2,
      trials = 60,
      blockSize = 2,
      compilation = KernelBasisCompilation.BlockedPartial(96),
      budget = DecodedTrialCheckpoint.repairedBudget,
      trialPreparation = TrialPreparationPolicy(TrialDesignLowering.Blocked(16))
    )
  )

  def directDesign(f: DecodedTrialCheckpoint.Fixture, coordinates: Vector[Double], fullWindow: Boolean): Array[Double] =
    val drive = f.plan.source match
      case ProfileHrfSource.TrialEvents(_, value, _, _) => value
      case _                                            => throw new IllegalArgumentException("trial fixture required")
    val family = f.plan.basis.family
    val point = ShapePoint.unsafe(coordinates)
    val lag = new Array[Double](1)
    val value = new Array[Double](1)
    val kernel = Hrf.scalar(
      "original-family-audit",
      span = if fullWindow then Seconds(f.rows.toDouble) else family.horizon.seconds
    ): t =>
      lag(0) = t.value
      family.evalInto(lag, point, value)
      value(0)
    val term = EventTerm
      .validated(
        events = Vector(
          CategoricalEvent(
            "trial",
            Vector.tabulate(f.trials)(identity),
            Vector.tabulate(f.trials)(i => f"trial_${i + 1}%04d")
          )
        ),
        onsets = drive.schedule.onsets,
        blockIds = drive.schedule.blockIds,
        durations = drive.schedule.durations,
        termTag = Some("trial")
      )
      .fold(e => throw new IllegalArgumentException(e.message), identity)
    term.convolve(kernel, f.dataset.samplingFrame, precision = Seconds(0.1), dropEmpty = false).data.data

  private def whiten(f: DecodedTrialCheckpoint.Fixture, values: Array[Double], columns: Int): DMat =
    WhiteningTransform
      .matrix(f.whitening, DMat.tabulate(f.rows, columns)((t, j) => values(t * columns + j)))
      .fold(e => throw new IllegalArgumentException(e.toString), identity)

  private def relative(a: Array[Double], b: Array[Double]): Double =
    math.sqrt(a.indices.map(i => math.pow(a(i) - b(i), 2)).sum / b.iterator.map(x => x * x).sum)

  def run(
      f: DecodedTrialCheckpoint.Fixture,
      selected: Vector[Cell] = cells,
      completed: String => Unit = _ => ()
  ): Result =
    require(selected.nonEmpty && selected.forall(cells.contains), "choose declared audit cells")
    val outputs = f.prepare.flatMap(_.trialOutputs).fold(e => throw new IllegalArgumentException(e.message), identity)
    val records = Vector.newBuilder[Record]
    val geometry = Vector.newBuilder[Geometry]
    selected.foreach: cell =>
      val chart = f.plan.basis.family.chart
      val truth = cell.unitShape.indices.map(i => chart.lower(i) + cell.unitShape(i) * chart.width(i)).toVector
      val basisDesign = DecodedTrialCheckpoint.designAt(f.expanded, truth)
      val truncated = directDesign(f, truth, false)
      val original = directDesign(f, truth, true)
      val wb = whiten(f, basisDesign, f.trials)
      val wo = whiten(f, original, f.trials)
      val nuisance = whiten(f, f.baseline.designMatrix.data, f.nuisance)
      // This enclosure certifies only this finite, rounded augmented array.
      // It does not enclose family evaluation, convolution or whitening errors.
      val augmented = DMat.tabulate(f.rows + f.trials, f.trials + f.nuisance): (t, j) =>
        if t < f.rows then if j < f.trials then wo(t, j) else nuisance(t, j - f.trials)
        else if j >= f.trials then 0.0
        else
          (if t - f.rows == j then 1.0 else 0.0) -
            (if (t - f.rows) % 3 == j % 3 then 3.0 / f.trials else 0.0)
      val spectral = (for
        input <- MatrixEnclosure.exact(augmented)
        candidate <- augmented.svd
        bounds <- ValidatedSvd.enclosure(input, candidate)
      yield bounds.lower(augmented.cols - 1)).fold(throw _, identity)
      geometry += Geometry(
        cell.label,
        relative(basisDesign, truncated),
        relative(truncated, original),
        math.sqrt(
          (0 until f.rows).map(t => (0 until f.trials).map(j => math.pow(wb(t, j) - wo(t, j), 2)).sum).sum /
            (0 until f.rows).map(t => (0 until f.trials).map(j => math.pow(wo(t, j), 2)).sum).sum
        ),
        spectral
      )
      val design = if cell.originalFamily then original else basisDesign
      val amplitudes = Array.tabulate(f.inputBlockVoxels, f.trials): (voxel, trial) =>
        val mean = Vector(1.2, -0.8, 0.4)(trial % 3) * (if voxel == 0 then 1.0 else -1.0)
        val group = f.expanded.membership.trialsOf(trial % 3)
        val center = group.map(i => math.sin(0.7 * i + voxel)).sum / group.length
        mean + cell.trialVariation * (math.sin(0.7 * trial + voxel) - center)
      val raw = new Array[Double](f.rows * f.inputBlockVoxels)
      val rng = new scala.util.Random(20261007L + cells.indexOf(cell))
      var voxel = 0
      while voxel < f.inputBlockVoxels do
        val signal = Array.tabulate(f.rows): t =>
          (0 until f.trials).map(j => design(t * f.trials + j) * amplitudes(voxel)(j)).sum
        val rms = math.sqrt(signal.iterator.map(x => x * x).sum / f.rows)
        var noise = rng.nextGaussian() * cell.noiseRatio * rms
        var t = 0
        while t < f.rows do
          if t > 0 then noise = 0.3 * noise + rng.nextGaussian() * cell.noiseRatio * rms * math.sqrt(0.91)
          raw(t * f.inputBlockVoxels + voxel) =
            signal(t) + noise + 4.0 * rms + 0.1 * rms * f.baseline.designMatrix(t, 1)
          t += 1
        voxel += 1
      val current = f.copy(rawBlock = raw)
      Vector(ProfileTrialReadoutMode.ExactShape, ProfileTrialReadoutMode.CorrectedReference).foreach: mode =>
        val sink = new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
          def accept(block: VoxelBlock, payload: ProfileTrialOutputBlock): Either[String, ProfileFitReceipt] =
            payload.results.foreach: voxel =>
              val s = voxel.selection
              val shapeError = truth.indices.map(i => math.abs(s.coordinates(i) - truth(i)) / chart.width(i)).max
              voxel.output match
                case ProfileTrialOutputOutcome.ReadoutRefused(error) =>
                  throw new IllegalStateException(s"unguarded audit unexpectedly refused: ${error.message}")
                case ProfileTrialOutputOutcome.DecodeRefused(_) =>
                  records += Record(
                    cell.label,
                    voxel.voxelId,
                    mode,
                    s.status,
                    truth,
                    s.coordinates,
                    s.energy,
                    shapeError,
                    false,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None,
                    None
                  )
                case ProfileTrialOutputOutcome.Emitted(_, output) =>
                  val candidate = output.trialAmplitudes.get ++ output.nuisanceCoefficients
                  val preparedOracle = current.oracle(voxel.voxelId, s.coordinates)
                  val direct = directDesign(f, s.coordinates, true)
                  val originalOracle = current.oracleDesign(voxel.voxelId, direct)
                  val originalMatrix = whiten(f, direct, f.trials)
                  val nuisance = whiten(f, f.baseline.designMatrix.data, f.nuisance)
                  val response = whiten(f, raw, f.inputBlockVoxels)
                  val residual = Array.tabulate(f.rows): t =>
                    response(t, voxel.voxelId) - (0 until f.trials).map(j => originalMatrix(t, j) * candidate(j)).sum -
                      (0 until f.nuisance).map(j => nuisance(t, j) * candidate(f.trials + j)).sum
                  val normalResidual = Vector.tabulate(f.trials + f.nuisance): j =>
                    val score = (0 until f.rows)
                      .map(t =>
                        (if j < f.trials then originalMatrix(t, j) else nuisance(t, j - f.trials)) * residual(t)
                      )
                      .sum
                    if j >= f.trials then score
                    else
                      score - (candidate(j) - f.expanded.membership
                        .trialsOf(j % 3)
                        .map(candidate(_))
                        .sum / (f.trials / 3))
                  val queryError = math.abs(
                    (0 until f.trials)
                      .map(j => (if j % 2 == 0 then 1.0 else -1.0) * (candidate(j) - originalOracle(j)))
                      .sum
                  )
                  records += Record(
                    cell.label,
                    voxel.voxelId,
                    mode,
                    s.status,
                    truth,
                    s.coordinates,
                    s.energy,
                    shapeError,
                    true,
                    Some(candidate.zip(preparedOracle).map((a, b) => math.abs(a - b)).max),
                    Some(candidate.zip(originalOracle).map((a, b) => math.abs(a - b)).max),
                    Some(queryError),
                    Some(relative(candidate.take(f.trials).toArray, amplitudes(voxel.voxelId))),
                    Some(output.evidence.preparedBasisNormalResidualNorm),
                    Some(math.sqrt(normalResidual.map(x => x * x).sum)),
                    Some(candidate.take(f.trials).zip(preparedOracle).map((a, b) => math.abs(a - b)).max),
                    Some(candidate.take(f.trials).zip(originalOracle).map((a, b) => math.abs(a - b)).max),
                    Some(candidate.drop(f.trials).zip(originalOracle.drop(f.trials)).map((a, b) => math.abs(a - b)).max)
                  )
            Right(ProfileFitReceipt(block.index, payload.voxelIds))
        outputs
          .run(new current.Reader, DecodedTrialCheckpoint.request, mode, sink)
          .fold(e => throw new IllegalArgumentException(e.message), identity)
      completed(cell.label)
    val values = records.result()
    val caveats = Vector(
      ScenarioCaveat(
        "phrf-original-equation-certificate",
        CaveatKind.PublicApiGap,
        CaveatSeverity.Blocking,
        "fit/profile",
        Some("PHRF-11"),
        "original-family equation certification is unavailable"
      ),
      ScenarioCaveat(
        "phrf-exploratory-scientific-scope",
        CaveatKind.DiagnosticsGap,
        CaveatSeverity.Blocking,
        "first-level-laws",
        Some("PHRF-14"),
        "exploratory Cascade34 cells lack independently searched optima, other families and adaptive coverage calibration"
      )
    )
    val scenarios = values.map: r =>
      val numerical = r.preparedQrMaxError.toVector.map: error =>
        // This comparison is a numerical oracle check, not a new scientific threshold.
        if r.mode == ProfileTrialReadoutMode.ExactShape then
          ScenarioHarness.scalar("exact prepared QR agreement", error, 0.0, ScenarioTolerance.absolute(1e-7))
        else ScenarioHarness.finite("measured corrected QR error", Vector(error))
      val observations = Vector(
        ScenarioHarness.finite("selected energy and chart error", Vector(r.energy, r.shapeErrorInChartUnits)),
        ScenarioHarness.fact(
          "decoded output available",
          r.emitted && r.status == DecodeStatus.Accepted,
          s"status=${r.status}; emitted=${r.emitted}"
        )
      ) ++ numerical
      ScenarioResult(s"phrf-audit-${r.cell}-${r.voxel}-${r.mode}", observations, caveats)
    Result(values, geometry.result(), scenarios)
