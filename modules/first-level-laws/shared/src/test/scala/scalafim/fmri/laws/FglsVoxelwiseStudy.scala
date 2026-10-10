package scalafim.fmri.laws

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.ar.{NoiseAcvf, NoisePooling, TimeSegments}
import scalafim.fmri.fit.{
  ArFglsTest,
  CensorContinuity,
  CovarianceUncertainty,
  VoxelArCoefficients,
  VoxelArScope,
  VoxelwiseArFgls,
  VoxelwiseArSpec
}
import ujson.{Arr, Bool, Null, Num, Obj, Str, Value}

private[laws] enum FglsStudyProfile(val label: String, val replicates: Int, val domain: Int):
  case Pilot extends FglsStudyProfile("pilot", 8, 10)
  case Development extends FglsStudyProfile("development", 1024, 11)
  case Confirmation extends FglsStudyProfile("confirmation", 2048, 12)

private[laws] object FglsStudyProfile:
  val current: FglsStudyProfile = LawEnvironment.get("SCALAFIM_FGLS_STUDY_PROFILE") match
    case None | Some("pilot") => Pilot
    case Some("development")  => Development
    case Some("confirmation") => Confirmation
    case Some(other)          => throw new IllegalArgumentException(s"unknown FGLS study profile: $other")

/** AR(2) noise truth for one voxel and run. */
private[laws] enum FglsNoiseTruth:
  case Homogeneous(phi: Vector[Double])
  case ByVoxel(phi: Vector[Vector[Double]])
  case ByRun(phi: Vector[Vector[Double]])

  def phi(voxel: Int, run: Int): Vector[Double] = this match
    case Homogeneous(value) => value
    case ByVoxel(values)    => values(voxel)
    case ByRun(values)      => values(run)

private[laws] final case class FglsStudyCell(id: String, group: Int, truth: FglsNoiseTruth, base: GlsStudyCell):
  def homogeneous: Boolean = truth.isInstanceOf[FglsNoiseTruth.Homogeneous]

private[laws] enum FglsEngine(val label: String):
  case ExistingKnown extends FglsEngine("existing-known-restart")
  case ExistingVoxelwise extends FglsEngine("existing-voxelwise")
  case KnownContinuous extends FglsEngine("known-continuous")
  case RunContinuousConditional extends FglsEngine("run-continuous-conditional")
  case RunContinuousKr extends FglsEngine("run-continuous-kr")
  case PooledContinuousConditional extends FglsEngine("pooled-continuous-conditional")
  case PooledContinuousKr extends FglsEngine("pooled-continuous-kr")
  case PooledRestartConditional extends FglsEngine("pooled-restart-conditional")

  def existing: Boolean = this == ExistingKnown || this == ExistingVoxelwise

private[laws] final case class FglsTrial(
    engine: FglsEngine,
    replicate: Int,
    phiError: Vector[Double],
    phiSquaredError: Double,
    whiteness: Double,
    nullEstimate: Double,
    nullVariance: Double,
    tPValue: Double,
    tDf: Double,
    fPValue: Double,
    fDf: Double,
    signalEstimate: Double,
    signalVariance: Double,
    coverPValue: Double,
    tRejected: Boolean,
    fRejected: Boolean,
    covered: Boolean
):
  private def finite(value: Double): Value = if value.isFinite then Num(value) else Null

  def json(cell: FglsStudyCell, profile: FglsStudyProfile): Obj = Obj(
    "cell" -> Str(cell.id),
    "profile" -> Str(profile.label),
    "engine" -> Str(engine.label),
    "replicate" -> Num(replicate),
    "phiError" -> Arr.from(phiError.map(Num(_))),
    "phiSquaredError" -> Num(phiSquaredError),
    "whiteness" -> Num(whiteness),
    "nullEstimate" -> Num(nullEstimate),
    "nullVariance" -> Num(nullVariance),
    "tPValue" -> finite(tPValue),
    "tDf" -> Num(tDf),
    "fPValue" -> finite(fPValue),
    "fDf" -> Num(fDf),
    "signalEstimate" -> Num(signalEstimate),
    "signalVariance" -> Num(signalVariance),
    "coverPValue" -> finite(coverPValue),
    "tRejected" -> Bool(tRejected),
    "fRejected" -> Bool(fRejected),
    "covered" -> Bool(covered)
  )

/** Portable F upper tail through the regularized incomplete beta (Lentz continued fraction). */
private[laws] object FglsDistribution:
  private val LanczosC = Array(
    0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313, -176.61502916214059,
    12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7
  )

  def logGamma(x: Double): Double =
    if x < 0.5 then math.log(math.Pi / math.abs(math.sin(math.Pi * x))) - logGamma(1.0 - x)
    else
      val shifted = x - 1.0
      var sum = LanczosC(0)
      val t = shifted + 7.5
      var i = 1
      while i < 9 do
        sum += LanczosC(i) / (shifted + i)
        i += 1
      0.5 * math.log(2.0 * math.Pi) + (shifted + 0.5) * math.log(t) - t + math.log(sum)

  private def continuedFraction(x: Double, a: Double, b: Double): Double =
    val tiny = 1e-300
    var c = 1.0
    var d = 1.0 - (a + b) * x / (a + 1.0)
    if math.abs(d) < tiny then d = tiny
    d = 1.0 / d
    var h = d
    var m = 1
    var done = false
    while !done && m <= 20000 do
      val m2 = 2 * m
      var aa = m * (b - m) * x / ((a - 1.0 + m2) * (a + m2))
      d = 1.0 + aa * d
      if math.abs(d) < tiny then d = tiny
      c = 1.0 + aa / c
      if math.abs(c) < tiny then c = tiny
      d = 1.0 / d
      h *= d * c
      aa = -(a + m) * (a + b + m) * x / ((a + m2) * (a + 1.0 + m2))
      d = 1.0 + aa * d
      if math.abs(d) < tiny then d = tiny
      c = 1.0 + aa / c
      if math.abs(c) < tiny then c = tiny
      d = 1.0 / d
      val delta = d * c
      h *= delta
      if math.abs(delta - 1.0) < 1e-16 then done = true
      m += 1
    h

  def regularizedBeta(x: Double, a: Double, b: Double): Double =
    if x <= 0.0 then 0.0
    else if x >= 1.0 then 1.0
    else
      val front = math.exp(logGamma(a + b) - logGamma(a) - logGamma(b) + a * math.log(x) + b * math.log1p(-x))
      if x < (a + 1.0) / (a + b + 2.0) then front * continuedFraction(x, a, b) / a
      else 1.0 - front * continuedFraction(1.0 - x, b, a) / b

  /** `P(F(d1, d2) > f)`. */
  def fUpper(f: Double, d1: Double, d2: Double): Double =
    if f <= 0.0 then 1.0 else regularizedBeta(d2 / (d2 + d1 * f), d2 / 2.0, d1 / 2.0)

private[laws] object FglsVoxelwiseStudy:
  val Phi: Vector[Double] = Vector(0.45, -0.10)
  val Alpha: Double = 0.05

  private def base(duration: GlsStudyDuration, censored: Boolean): GlsStudyCell =
    GlsStudyCell(
      s"fgls-base-${duration.toString.toLowerCase}-${if censored then "censored" else "complete"}",
      Phi,
      12,
      censored,
      StudyPooling.Voxelwise,
      duration
    )

  val cells: Vector[FglsStudyCell] = Vector(
    FglsStudyCell(
      "ar2-high-censored-short",
      0,
      FglsNoiseTruth.Homogeneous(Phi),
      base(GlsStudyDuration.Standard, true)
    ),
    FglsStudyCell(
      "ar2-high-censored-long",
      1,
      FglsNoiseTruth.Homogeneous(Phi),
      base(GlsStudyDuration.Extended, true)
    ),
    FglsStudyCell(
      "ar2-high-complete-short",
      2,
      FglsNoiseTruth.Homogeneous(Phi),
      base(GlsStudyDuration.Standard, false)
    ),
    FglsStudyCell(
      "ar2-high-censored-short-voxel-heterogeneous",
      3,
      FglsNoiseTruth.ByVoxel(Vector(Vector(0.60, -0.20), Vector(0.20, 0.05), Phi, Vector(0.30, 0.10))),
      base(GlsStudyDuration.Standard, true)
    ),
    FglsStudyCell(
      "ar2-high-censored-short-run-heterogeneous",
      4,
      FglsNoiseTruth.ByRun(Vector(Vector(0.30, 0.05), Vector(0.55, -0.15))),
      base(GlsStudyDuration.Standard, true)
    )
  )

  def engines(cell: FglsStudyCell): Vector[FglsEngine] =
    FglsEngine.values.toVector.filter(engine => cell.homogeneous || !engine.existing)

  val rootSeed: Int = LawEnvironment
    .get("SCALAFIM_FGLS_STUDY_SEED")
    .map(_.toInt)
    .getOrElse(1310161010)

  def seed(profile: FglsStudyProfile, group: Int, replicate: Int): Int =
    ((rootSeed.toLong + profile.domain.toLong * 10000019L + (group + 1L) * 1000003L + replicate.toLong * 7919L) %
      2147483646L + 1L).toInt

  /** Continuous AR(2) noise per run with 256 burn-in draws, columns generated in voxel order. */
  def noise(cell: FglsStudyCell, seed: Int): DMat =
    val rng = new GlsStudyRng(seed)
    val lengths = cell.base.runLengths
    val data = Matrix.newBuilder(cell.base.rows, 4)
    var voxel = 0
    while voxel < 4 do
      var start = 0
      lengths.zipWithIndex.foreach { (length, run) =>
        val phi = cell.truth.phi(voxel, run)
        var previous = 0.0
        var previous2 = 0.0
        var step = -256
        while step < length do
          val value = rng.gaussian() + phi(0) * previous + phi(1) * previous2
          if step >= 0 then data(start + step, voxel) = value
          previous2 = previous
          previous = value
          step += 1
        start += length
      }
      voxel += 1
    data.result()

  def response(cell: FglsStudyCell, errors: DMat): DMat =
    val design = cell.base.design
    Matrix.tabulate(cell.base.rows, 4) { (row, voxel) =>
      var signal = 0.0
      var col = 0
      while col < design.cols do
        signal += design(row, col) * cell.base.coefficient(col, voxel)
        col += 1
      signal + errors(row, voxel)
    }

  private def spec(cell: FglsStudyCell, engine: FglsEngine, voxel: Int): VoxelwiseArSpec =
    val runs = cell.base.runLengths.indices.toVector
    engine match
      case FglsEngine.KnownContinuous =>
        VoxelwiseArSpec(
          2,
          VoxelArCoefficients.Known(runs.map(run => cell.truth.phi(voxel, run))),
          CensorContinuity.ContinuousMissing,
          CovarianceUncertainty.Conditional
        )
      case FglsEngine.RunContinuousConditional =>
        estimated(VoxelArScope.PerRun, CensorContinuity.ContinuousMissing, CovarianceUncertainty.Conditional)
      case FglsEngine.RunContinuousKr =>
        estimated(VoxelArScope.PerRun, CensorContinuity.ContinuousMissing, CovarianceUncertainty.KenwardRoger)
      case FglsEngine.PooledContinuousConditional =>
        estimated(VoxelArScope.AcrossRuns, CensorContinuity.ContinuousMissing, CovarianceUncertainty.Conditional)
      case FglsEngine.PooledContinuousKr =>
        estimated(VoxelArScope.AcrossRuns, CensorContinuity.ContinuousMissing, CovarianceUncertainty.KenwardRoger)
      case FglsEngine.PooledRestartConditional =>
        estimated(VoxelArScope.AcrossRuns, CensorContinuity.RestartAfterCensor, CovarianceUncertainty.Conditional)
      case other => throw new IllegalArgumentException(s"${other.label} is measured by the existing executor")

  private def estimated(scope: VoxelArScope, censor: CensorContinuity, uncertainty: CovarianceUncertainty) =
    VoxelwiseArSpec(2, VoxelArCoefficients.Estimated(scope, 25), censor, uncertainty)

  private val tContrast: DMat = Matrix.tabulate(1, 16)((_, col) => if col == 0 then 1.0 else 0.0)
  private val fContrast: DMat = Matrix.tabulate(2, 16)((row, col) => if row == col then 1.0 else 0.0)

  private def pValue(test: ArFglsTest): Double =
    FglsDistribution.fUpper(test.statistic, test.numeratorDf.toDouble, test.denominatorDf)

  /** Run-weighted AR recovery over every voxel, against the voxel/run truth. */
  private def recovery(cell: FglsStudyCell, phiByVoxel: Vector[Vector[Vector[Double]]]): (Vector[Double], Double) =
    val layout = cell.base.estimationLayout
    val weights =
      cell.base.runLengths.indices.map(run => layout.segmentsForRun(run).map(_.length).sum.toDouble).toVector
    val total = weights.sum
    val errors = Array.fill(2)(0.0)
    var squared = 0.0
    phiByVoxel.zipWithIndex.foreach { (byRun, voxel) =>
      byRun.zipWithIndex.foreach { (phi, run) =>
        val weight = weights(run) / total / phiByVoxel.length
        val truth = cell.truth.phi(voxel, run)
        var lag = 0
        while lag < 2 do
          val error = phi(lag) - truth(lag)
          errors(lag) += weight * error
          squared += weight * error * error / 2.0
          lag += 1
      }
    }
    errors.toVector -> squared

  private def whiteness(cell: FglsStudyCell, residuals: Vector[Vector[Double]]): Either[String, Double] =
    var total = 0.0
    var count = 0
    var failure: Option[String] = None
    residuals.foreach { column =>
      if failure.isEmpty then
        NoiseAcvf.estimate(
          Matrix.tabulate(column.length, 1)((row, _) => column(row)),
          cell.base.estimationLayout,
          4,
          NoisePooling.Run
        ) match
          case Left(error)  => failure = Some(error.message)
          case Right(value) =>
            value.units.foreach { unit =>
              unit.acvf.tail.foreach { covariance =>
                total += math.abs(covariance / unit.acvf.head)
                count += 1
              }
            }
    }
    failure.toLeft(total / count)

  def measureCandidate(
      cell: FglsStudyCell,
      data: DMat,
      engine: FglsEngine,
      replicate: Int
  ): Either[String, FglsTrial] =
    val runs = TimeSegments.fromRunLengths(cell.base.runLengths)
    val fits = (0 until 4).toVector.map { voxel =>
      val column = Matrix.tabulate(data.rows, 1)((row, _) => data(row, voxel))
      VoxelwiseArFgls
        .fit(cell.base.design, column, runs, cell.base.censorRows, spec(cell, engine, voxel))
        .left
        .map(error => s"voxel $voxel: ${error.message}")
        .map(_.voxels.head)
    }
    fits.collectFirst { case Left(error) => error } match
      case Some(error) => Left(error)
      case None        =>
        val voxels = fits.collect { case Right(value) => value }
        for
          t <- voxels(0).test(tContrast, Vector(0.0)).left.map(_.message)
          f <- voxels(0).test(fContrast, Vector(0.0, 0.0)).left.map(_.message)
          cover <- voxels(1).test(tContrast, Vector(0.75)).left.map(_.message)
          white <- whiteness(cell, voxels.map(_.whitenedResiduals))
        yield
          val (phiError, squared) = recovery(cell, voxels.map(_.phiByRun))
          val tp = pValue(t)
          val fp = pValue(f)
          val cp = pValue(cover)
          FglsTrial(
            engine,
            replicate,
            phiError,
            squared,
            white,
            voxels(0).coefficients(0),
            voxels(0).adjustedCovariance(0, 0) / t.scale,
            tp,
            t.denominatorDf,
            fp,
            f.denominatorDf,
            voxels(1).coefficients(0),
            voxels(1).adjustedCovariance(0, 0) / cover.scale,
            cp,
            tp < Alpha,
            fp < Alpha,
            cp >= Alpha
          )

  def measureExisting(
      cell: FglsStudyCell,
      errors: DMat,
      engine: FglsEngine,
      replicate: Int
  ): Either[String, FglsTrial] =
    val study = CorrectedGlsQualification
    val existing = if engine == FglsEngine.ExistingKnown then GlsStudyEngine.KnownPhi else GlsStudyEngine.Corrected
    val model = study.model(cell.base, errors, s"fgls-${cell.id}-$replicate")
    study.measure(cell.base, model, existing, replicate).left.map(_.message).map { trial =>
      val df = cell.base.df.toDouble
      FglsTrial(
        engine,
        replicate,
        trial.phiMean.zip(Phi).map(_ - _),
        trial.phiSquaredError,
        trial.whiteness,
        trial.nullEstimate,
        trial.nullVariance,
        Double.NaN,
        df,
        Double.NaN,
        df,
        trial.signalEstimate,
        trial.signalVariance,
        Double.NaN,
        trial.tRejected,
        trial.fRejected,
        trial.covered
      )
    }
