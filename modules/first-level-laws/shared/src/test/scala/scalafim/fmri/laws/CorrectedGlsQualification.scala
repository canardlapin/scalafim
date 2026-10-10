package scalafim.fmri.laws

import gale.linalg.{DMat, Matrix}
import scalafim.dataset.{DatasetId, FmriDataset, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.ar.*
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.fit.*
import scalafim.fmri.hrf.{Hrfs, Lag}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{
  ArBiasCorrection,
  ArOptions,
  ArStructure,
  FitConfig,
  FitEngine,
  FitPlan,
  FmriModel,
  ArInitialization,
  ArCensorTreatment
}
import scalafim.image.SampleSpaces
import scalafim.scenarios.{ScenarioObservation, ScenarioResult}
import ujson.{Arr, Bool, Num, Obj, Str}

private[laws] enum GlsStudyProfile(val label: String, val replicates: Int, val domain: Int):
  case Pilot extends GlsStudyProfile("pilot", 16, 1)
  case Screen extends GlsStudyProfile("screen", 64, 2)
  case Confirmation extends GlsStudyProfile("confirmation", 2048, 3)

  def counts: (Int, Int) = this match
    case Pilot        => CorrectedGlsBounds.PilotCounts
    case Screen       => CorrectedGlsBounds.ScreenCounts
    case Confirmation => CorrectedGlsBounds.ConfirmationCounts

  def meanCritical: Double = this match
    case Pilot        => CorrectedGlsBounds.PilotMeanCritical
    case Screen       => CorrectedGlsBounds.ScreenMeanCritical
    case Confirmation => CorrectedGlsBounds.ConfirmationMeanCritical

private[laws] object GlsStudyProfile:
  val current: GlsStudyProfile = LawEnvironment.get("SCALAFIM_GLS_STUDY_PROFILE") match
    case Some("pilot")        => Pilot
    case Some("screen")       => Screen
    case Some("confirmation") => Confirmation
    case None                 => if LawRunProfile.current == LawRunProfile.Calibration then Confirmation else Screen
    case Some(other)          => throw new IllegalArgumentException(s"unknown corrected GLS study profile: $other")

private[laws] enum GlsStudyStage:
  case Fit, Read, Schema, Contrast, Correction, Whitening, Acf

private[laws] enum GlsStudyError:
  case Pipeline(stage: GlsStudyStage, detail: String)
  def message: String = this match
    case Pipeline(stage, detail) => s"$stage: $detail"

private[laws] enum StudyPooling:
  case Global, Run, Voxelwise

private[laws] enum GlsStudyDuration(val runLengths: Vector[Int]):
  case Standard extends GlsStudyDuration(Vector(96, 144))
  case Extended extends GlsStudyDuration(Vector(192, 288))

private[laws] enum GlsStudyEngine(val label: String):
  case KnownPhi extends GlsStudyEngine("known-phi")
  case Raw extends GlsStudyEngine("raw")
  case Corrected extends GlsStudyEngine("corrected")

private[laws] final case class GlsStudyCell(
    id: String,
    phi: Vector[Double],
    nuisance: Int,
    censored: Boolean,
    pooling: StudyPooling,
    duration: GlsStudyDuration = GlsStudyDuration.Standard,
    initialization: Option[ArInitialization] = None,
    correction: Option[ArBiasCorrection] = None,
    censorTreatment: ArCensorTreatment = ArCensorTreatment.RestartWhitening
):
  require(phi.nonEmpty && phi.length <= 2 && ArmaCoefficients.ar(phi*).arOrder == phi.length)
  require(nuisance == 0 || nuisance == 12)
  val runLengths: Vector[Int] = duration.runLengths
  val rows: Int = runLengths.sum
  val frame: SamplingFrame = SamplingFrame(runLengths, Vector(1.0, 1.0))
  val censorRows: Vector[Int] =
    if !censored then Vector.empty
    else
      runLengths.zipWithIndex.flatMap { (length, run) =>
        val start = runLengths.take(run).sum
        (Vector(23, 24, 60) ++ (97 until length by 37)).filter(_ < length).map(_ + start)
      }
  val segments: Vector[TimeSegment] =
    if censorTreatment == ArCensorTreatment.EstimateOnly then TimeSegments.fromRunLengths(runLengths)
    else TimeSegments.withCensorResets(TimeSegments.fromRunLengths(runLengths), censorRows.toSet)
  val estimationLayout: NoiseEstimationLayout = NoiseEstimationLayout
    .excludingRows(segments, rows, censorRows.toSet)
    .fold(error => throw new IllegalArgumentException(error.message), identity)
  val eventRows: Vector[Vector[Double]] =
    val kernel = Vector.tabulate(33)(i => Hrfs.SPMG1(Lag(i.toDouble)).data(0))
    Vector.tabulate(rows) { row =>
      val run = if row < runLengths.head then 0 else 1
      val local = row - (if run == 0 then 0 else runLengths.head)
      val tasks = Vector.tabulate(2) { task =>
        var total = 0.0
        var onset = 6 + task * 32
        while onset <= local do
          var duration = 0
          while duration < 4 do
            val lag = local - onset - duration
            if lag >= 0 && lag < kernel.length then total += kernel(lag)
            duration += 1
          onset += 64
        total
      }
      val nuisanceValues = Vector.tabulate(nuisance) { col =>
        val owner = col / 6
        if owner != run then 0.0
        else math.cos(math.Pi * (2.0 * local + 1.0) * (col % 6 + 1) / (2.0 * runLengths(run)))
      }
      tasks ++ nuisanceValues
    }
  val event: EventModel = EventModel(
    Vector.empty,
    frame,
    Mat.fromRows(eventRows),
    Vector("task_a", "task_b") ++ Vector.tabulate(nuisance)(i => s"drift_$i"),
    Vector(0 -> (2 + nuisance)),
    Map("tasks" -> Vector(0, 1))
  )
  val baseline: BaselineModel =
    BaselineModel.build(frame, basis = BaselineBasis.Constant, intercept = Intercept.Runwise)
  val design: DMat = Matrix.tabulate(rows, 4 + nuisance) { (row, col) =>
    if col < 2 + nuisance then eventRows(row)(col)
    else if (row < runLengths.head) == (col == 2 + nuisance) then 1.0
    else 0.0
  }
  val df: Int = rows - design.cols
  val tCritical: Double = duration match
    case GlsStudyDuration.Standard => if nuisance == 0 then CorrectedGlsBounds.T236 else CorrectedGlsBounds.T224
    case GlsStudyDuration.Extended => if nuisance == 0 then GlsFactorBounds.T476 else GlsFactorBounds.T464
  val fCritical: Double = duration match
    case GlsStudyDuration.Standard => if nuisance == 0 then CorrectedGlsBounds.F236 else CorrectedGlsBounds.F224
    case GlsStudyDuration.Extended => if nuisance == 0 then GlsFactorBounds.F476 else GlsFactorBounds.F464

  def coefficient(predictor: Int, voxel: Int): Double =
    if predictor == 0 then if voxel == 0 then 0.0 else 0.75
    else if predictor == 1 then if voxel == 0 then 0.0 else -0.25
    else 0.1 * (predictor + 1) * (voxel + 1)

  def options(engine: GlsStudyEngine): ArOptions = engine match
    case GlsStudyEngine.KnownPhi =>
      ArOptions(
        ArStructure.Ar(phi.length),
        global = true,
        phi = Some(phi),
        censoredTimepoints = censorRows,
        initialization = initialization,
        censorTreatment = censorTreatment
      )
    case other =>
      ArOptions(
        ArStructure.Ar(phi.length),
        global = pooling == StudyPooling.Global,
        voxelwise = pooling == StudyPooling.Voxelwise,
        censoredTimepoints = censorRows,
        biasCorrection = if other == GlsStudyEngine.Corrected then correction.getOrElse(ArBiasCorrection.Ols)
        else ArBiasCorrection.Raw,
        initialization = initialization,
        censorTreatment = censorTreatment
      )

/** A portable, named Monte Carlo stream. Gaussian draws consume an independent run's own burn-in; censoring never
  * changes generator covariance or seeds.
  */
private[laws] final class GlsStudyRng(seed: Int):
  require(seed > 0 && seed < 2147483647)
  private var state = seed
  def uniform(): Double =
    state = ((state.toDouble * 48271.0) % 2147483647.0).toInt
    state.toDouble / 2147483647.0
  def gaussian(): Double =
    math.sqrt(-2.0 * math.log(uniform())) * math.cos(2.0 * math.Pi * uniform())

private[laws] final case class GlsStudyTrial(
    engine: GlsStudyEngine,
    replicate: Int,
    phiMean: Vector[Double],
    phiSquaredError: Double,
    whiteness: Double,
    nullEstimate: Double,
    nullVariance: Double,
    signalEstimate: Double,
    signalVariance: Double,
    tRejected: Boolean,
    fRejected: Boolean,
    covered: Boolean,
    milliseconds: Double
):
  def json(cell: GlsStudyCell, profile: GlsStudyProfile, root: Int): Obj = Obj(
    "cell" -> Str(cell.id),
    "profile" -> Str(profile.label),
    "engine" -> Str(engine.label),
    "root" -> Num(root),
    "replicate" -> Num(replicate),
    "phi" -> Arr.from(phiMean.map(Num(_))),
    "phiSquaredError" -> Num(phiSquaredError),
    "whiteness" -> Num(whiteness),
    "nullEstimate" -> Num(nullEstimate),
    "nullVariance" -> Num(nullVariance),
    "signalEstimate" -> Num(signalEstimate),
    "signalVariance" -> Num(signalVariance),
    "tRejected" -> Bool(tRejected),
    "fRejected" -> Bool(fRejected),
    "covered" -> Bool(covered),
    "milliseconds" -> Num(milliseconds)
  )

private[laws] object CorrectedGlsQualification:
  val cells: Vector[GlsStudyCell] = Vector(
    GlsStudyCell("ar1-low-global", Vector(0.5), 0, false, StudyPooling.Global),
    GlsStudyCell("ar1-high-global", Vector(0.5), 12, false, StudyPooling.Global),
    GlsStudyCell("ar1-high-censored-run", Vector(0.5), 12, true, StudyPooling.Run),
    GlsStudyCell("ar2-low-global", Vector(0.45, -0.10), 0, false, StudyPooling.Global),
    GlsStudyCell("ar2-high-global", Vector(0.45, -0.10), 12, false, StudyPooling.Global),
    GlsStudyCell("ar2-high-censored-voxelwise", Vector(0.45, -0.10), 12, true, StudyPooling.Voxelwise)
  )

  val rootSeed: Int = LawEnvironment
    .get("SCALAFIM_GLS_STUDY_SEED")
    .orElse(LawEnvironment.get("SCALAFIM_LAW_SEED_LONG"))
    .fold(CorrectedGlsBounds.DefaultSeed) { raw =>
      val value = raw.toLongOption.getOrElse(throw new IllegalArgumentException("study seed must be a signed integer"))
      val normalized = (value % 2147483646L + 2147483646L) % 2147483646L
      (if normalized == 0L then 2147483646L else normalized).toInt
    }

  def seed(domain: Int, cell: Int, replicate: Int): Int =
    ((rootSeed.toLong + domain.toLong * 10000019L + (cell + 1L) * 1000003L + replicate.toLong * 7919L) % 2147483646L + 1L).toInt

  def noise(cell: GlsStudyCell, seed: Int, columns: Int = 4): DMat =
    val rng = new GlsStudyRng(seed)
    val data = Matrix.newBuilder(cell.rows, columns)
    var column = 0
    while column < columns do
      var start = 0
      cell.runLengths.foreach { length =>
        var previous = 0.0
        var previous2 = 0.0
        var step = -256
        while step < length do
          val value = rng.gaussian() + cell.phi(0) * previous + cell.phi.lift(1).getOrElse(0.0) * previous2
          if step >= 0 then data(start + step, column) = value
          previous2 = previous
          previous = value
          step += 1
        start += length
      }
      column += 1
    data.result()

  def model(cell: GlsStudyCell, errors: DMat, id: String): FmriModel =
    val response = Matrix.tabulate(cell.rows, errors.cols) { (row, voxel) =>
      var signal = 0.0
      var col = 0
      while col < cell.design.cols do
        signal += cell.design(row, col) * cell.coefficient(col, voxel)
        col += 1
      signal + errors(row, voxel)
    }
    val dataset = FmriDataset.unsafe(
      InMemoryDatasetBackend(DatasetId(id), response, SampleSpaces(Vector(errors.cols, 1, 1))),
      cell.frame
    )
    FmriModel(cell.event, cell.baseline, dataset)

  private val contrastCache =
    scala.collection.mutable.Map.empty[String, Either[GlsStudyError, (CompiledTContrast, CompiledFContrast)]]

  private def contrasts(cell: GlsStudyCell): Either[GlsStudyError, (CompiledTContrast, CompiledFContrast)] =
    contrastCache.getOrElseUpdate(
      cell.id, {
        val reference = model(cell, Matrix.zeros(cell.rows, 4), s"${cell.id}-contrast-design")
        for
          schema <- reference.designSchema.toRight(
            GlsStudyError.Pipeline(GlsStudyStage.Schema, "study model lacks a structural schema")
          )
          t <- StructuralTContrast
            .fromColumn(ContrastId.unsafe("task-a"), "task_a", schema.columns(0).id)
            .compile(schema)
            .left
            .map(error => GlsStudyError.Pipeline(GlsStudyStage.Contrast, error.message))
          f <- StructuralFContrast
            .fromRows(
              ContrastId.unsafe("tasks"),
              "joint null tasks",
              Vector(Map(schema.columns(0).id -> 1.0), Map(schema.columns(1).id -> 1.0))
            )
            .compile(schema)
            .left
            .map(error => GlsStudyError.Pipeline(GlsStudyStage.Contrast, error.message))
        yield (t, f)
      }
    )

  def measure(
      cell: GlsStudyCell,
      model: FmriModel,
      engine: GlsStudyEngine,
      replicate: Int,
      inspect: DenseFmriFitResult => Unit = _ => ()
  ): Either[GlsStudyError, GlsStudyTrial] =
    val started = System.nanoTime()
    val plan = FitPlan(
      model,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(autocorrelation = cell.options(engine))
    )
    for
      fitted <- FitPlanExecutor
        .fitDense(plan)
        .left
        .map(error => GlsStudyError.Pipeline(GlsStudyStage.Fit, error.message))
      _ = inspect(fitted)
      reader <- SynchronousFmriDataset
        .readerFor(model.dataset)
        .left
        .map(error => GlsStudyError.Pipeline(GlsStudyStage.Read, error.message))
      input <- reader
        .seriesEither(scalafim.dataset.DataSelection.All)
        .left
        .map(error => GlsStudyError.Pipeline(GlsStudyStage.Read, error.message))
      compiled <- contrasts(cell)
      (t, f) = compiled
      tResult <- t.evaluate(fitted).left.map(error => GlsStudyError.Pipeline(GlsStudyStage.Contrast, error.message))
      fResult <- f.evaluate(fitted).left.map(error => GlsStudyError.Pipeline(GlsStudyStage.Contrast, error.message))
      diagnostic <- fitted.autocorrelation.toRight(
        GlsStudyError.Pipeline(GlsStudyStage.Correction, "study fit has no AR diagnostics")
      )
      _ <-
        val applied = if cell.pooling == StudyPooling.Voxelwise then
          diagnostic.runs.forall(_.voxelwiseCorrections.length == 4) && diagnostic.runs.forall(
            _.voxelwiseCorrections.forall(_.wasApplied)
          )
        else diagnostic.runs.forall(_.correction.exists(_.wasApplied))
        if engine != GlsStudyEngine.Corrected || applied then Right(())
        else
          val statuses = diagnostic.runs.zipWithIndex.flatMap: (run, index) =>
            val values =
              if cell.pooling == StudyPooling.Voxelwise then run.voxelwiseCorrections else run.correction.toVector
            values.zipWithIndex.collect:
              case (value, voxel) if !value.wasApplied => s"run=$index voxel=$voxel $value"
          Left(
            GlsStudyError.Pipeline(
              GlsStudyStage.Correction,
              s"corrected fit returned a non-Applied outcome: ${statuses.mkString("; ")}"
            )
          )
      residuals = Matrix.tabulate(cell.rows, 4) { (row, voxel) =>
        var predicted = 0.0
        var col = 0
        while col < cell.design.cols do
          predicted += cell.design(row, col) * fitted.coefficients(col, voxel)
          col += 1
        input.data(row, voxel) - predicted
      }
      whitened <- whiten(cell, diagnostic, residuals).left.map(error =>
        GlsStudyError.Pipeline(GlsStudyStage.Whitening, error.message)
      )
      whiteness <- meanRetainedAcf(cell, whitened).left.map(error =>
        GlsStudyError.Pipeline(GlsStudyStage.Acf, error.message)
      )
    yield
      val weights =
        cell.runLengths.indices.map(r => cell.estimationLayout.segmentsForRun(r).map(_.length).sum.toDouble).toVector
      val total = weights.sum
      val phis = diagnostic.runs.zipWithIndex.flatMap { (run, r) =>
        val values = if run.voxelwiseCoefficients.nonEmpty then run.voxelwiseCoefficients else Vector(run.phi)
        values.map(_ -> (weights(r) / total / values.length))
      }
      val phiMean = Vector.tabulate(cell.phi.length)(lag => phis.map((p, w) => p(lag) * w).sum)
      val mse = phis.map((p, w) => p.zip(cell.phi).map((a, b) => (a - b) * (a - b)).sum * w / cell.phi.length).sum
      val signalEstimate = tResult.estimates(1)
      val signalVariance = tResult.standardErrors(1) * tResult.standardErrors(1)
      GlsStudyTrial(
        engine,
        replicate,
        phiMean,
        mse,
        whiteness,
        tResult.estimates(0),
        tResult.standardErrors(0) * tResult.standardErrors(0),
        signalEstimate,
        signalVariance,
        math.abs(tResult.statistics(0)) > cell.tCritical,
        fResult.statistics(0) > cell.fCritical,
        math.abs(signalEstimate - 0.75) <= cell.tCritical * math.sqrt(signalVariance),
        (System.nanoTime() - started).toDouble / 1e6
      )

  private def meanRetainedAcf(cell: GlsStudyCell, residuals: DMat): Either[ArError, Double] =
    var total = 0.0
    var count = 0
    var column = 0
    while column < residuals.cols do
      NoiseAcvf.estimate(
        residuals.slice(0, residuals.rows, column, column + 1),
        cell.estimationLayout,
        4,
        NoisePooling.Run
      ) match
        case Left(error)  => return Left(error)
        case Right(value) =>
          value.units.foreach { unit =>
            unit.acvf.tail.foreach { covariance =>
              total += math.abs(covariance / unit.acvf.head)
              count += 1
            }
          }
      column += 1
    Right(total / count)

  private def whiten(cell: GlsStudyCell, diagnostics: ArDiagnostics, residuals: DMat): Either[ArError, DMat] =
    if diagnostics.sharedNormalizedCovariance then
      val coefficients = if diagnostics.whitening.pooling == NoisePooling.Global then
        Vector(ArmaCoefficients.ar(diagnostics.runs.head.phi*))
      else diagnostics.runs.map(run => ArmaCoefficients.ar(run.phi*))
      val scope = if diagnostics.whitening.pooling == NoisePooling.Global then
        CoefficientScope.Global(coefficients.head)
      else CoefficientScope.ByRun(coefficients)
      WhiteningPlan
        .withScope(scope, cell.segments, diagnostics.whitening.initialCondition)
        .flatMap(plan => WhiteningTransform.matrix(plan, residuals))
    else
      val output = Matrix.newBuilder(residuals.rows, residuals.cols)
      var voxel = 0
      while voxel < residuals.cols do
        val coefficients = diagnostics.runs.map(run => ArmaCoefficients.ar(run.voxelwiseCoefficients(voxel)*))
        val transformed = WhiteningPlan
          .withScope(CoefficientScope.ByRun(coefficients), cell.segments, diagnostics.whitening.initialCondition)
          .flatMap(plan => WhiteningTransform.matrix(plan, residuals.slice(0, residuals.rows, voxel, voxel + 1)))
        transformed match
          case Left(error)  => return Left(error)
          case Right(value) =>
            var row = 0
            while row < residuals.rows do
              output(row, voxel) = value(row, 0)
              row += 1
        voxel += 1
      Right(output.result())

  def mean(values: Vector[Double]): Double = values.sum / values.length
  def variance(values: Vector[Double]): Double =
    val m = mean(values)
    values.map(value => (value - m) * (value - m)).sum / (values.length - 1)

  def summary(
      cell: GlsStudyCell,
      profile: GlsStudyProfile,
      trials: Vector[GlsStudyTrial],
      failures: Vector[String],
      engines: Vector[GlsStudyEngine] = GlsStudyEngine.values.toVector
  ): ScenarioResult =
    val observations = Vector.newBuilder[ScenarioObservation]
    def fact(name: String, ok: Boolean, detail: String): Unit =
      observations += ScenarioObservation.Fact(name, ok, detail)
    fact("no-refusals", failures.isEmpty, failures.mkString("; "))
    engines.foreach { engine =>
      val rows = trials.filter(_.engine == engine)
      fact(s"${engine.label}-replicates", rows.length == profile.replicates, s"${rows.length}/${profile.replicates}")
      if rows.length == profile.replicates then
        val estimates = rows.map(_.nullEstimate)
        val samplingVariance = variance(estimates)
        val ratio = mean(rows.map(_.nullVariance)) / samplingVariance
        val tReject = rows.count(_.tRejected)
        val fReject = rows.count(_.fRejected)
        val covered = rows.count(_.covered)
        val whiteness = mean(rows.map(_.whiteness))
        val meanPhi = cell.phi.indices.map(lag => mean(rows.map(_.phiMean(lag)))).toVector
        GlsStudyTrace.emit(
          "GLS_STUDY_SUMMARY " + ujson.write(
            Obj(
              "cell" -> Str(cell.id),
              "profile" -> Str(profile.label),
              "root" -> Num(rootSeed),
              "engine" -> Str(engine.label),
              "replicates" -> Num(rows.length),
              "tReject" -> Num(tReject),
              "fReject" -> Num(fReject),
              "covered" -> Num(covered),
              "varianceRatio" -> Num(ratio),
              "whiteness" -> Num(whiteness),
              "phi" -> Arr.from(meanPhi.map(Num(_))),
              "milliseconds" -> Num(mean(rows.map(_.milliseconds)))
            )
          )
        )
        fact(
          s"${engine.label}-finite",
          rows.forall(row =>
            Vector(
              row.nullEstimate,
              row.nullVariance,
              row.signalEstimate,
              row.signalVariance,
              row.whiteness,
              row.phiSquaredError
            ).forall(_.isFinite)
          ) && samplingVariance > 0.0,
          s"varianceRatio=$ratio whiteness=$whiteness"
        )
        if engine != GlsStudyEngine.Raw && profile != GlsStudyProfile.Pilot then
          val (low, high) = profile.counts
          fact(s"${engine.label}-null-t", tReject >= low && tReject <= high, s"rejections=$tReject in [$low,$high]")
          fact(s"${engine.label}-null-f", fReject >= low && fReject <= high, s"rejections=$fReject in [$low,$high]")
          fact(
            s"${engine.label}-coverage",
            covered >= profile.replicates - high && covered <= profile.replicates - low,
            s"covered=$covered/${profile.replicates}"
          )
        if engine == GlsStudyEngine.Corrected && profile != GlsStudyProfile.Pilot then
          fact(
            "AR-recovery-rmse",
            math.sqrt(mean(rows.map(_.phiSquaredError))) <= 0.10,
            s"RMSE=${math.sqrt(mean(rows.map(_.phiSquaredError)))}"
          )
          cell.phi.indices.foreach { lag =>
            val errors = rows.map(_.phiMean(lag) - cell.phi(lag))
            val bias = mean(errors)
            val half = if profile == GlsStudyProfile.Confirmation then
              profile.meanCritical * math.sqrt(variance(errors) / rows.length)
            else 0.0
            fact(s"AR-recovery-bias-$lag", math.abs(bias) + half <= 0.04, s"bias=$bias half=$half")
          }
          val oracleWhiteness = mean(trials.filter(_.engine == GlsStudyEngine.KnownPhi).map(_.whiteness))
          fact(
            "residual-whiteness",
            whiteness <= 0.10 && whiteness - oracleWhiteness <= 0.025,
            s"corrected=$whiteness knownPhi=$oracleWhiteness"
          )
          if profile == GlsStudyProfile.Confirmation then
            fact(
              "variance-ratio-point",
              ratio >= 0.80 && ratio <= 1.20,
              s"ratio=$ratio; bootstrap equivalence adjudicated in retained analysis"
            )
            val standardized = mean(estimates) / math.sqrt(samplingVariance)
            val half = profile.meanCritical / math.sqrt(rows.length)
            fact(
              "effect-bias-equivalence",
              math.abs(standardized) + half <= 0.10,
              s"standardizedBias=$standardized half=$half"
            )
    }
    ScenarioResult(s"corrected-gls.${profile.label}.${cell.id}", observations.result())
