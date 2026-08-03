package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix, QR, QROptions, QRPivoting}

final case class LssTrialDesign private (value: DMat, trialNames: Vector[String]):
  require(trialNames.length == value.cols, "trial names must match trial-design columns")
  require(trialNames.forall(_.nonEmpty), "trial names must be non-empty")
  require(trialNames.distinct.length == trialNames.length, "trial names must be unique")

  def timepoints: Int = value.rows
  def trials: Int = value.cols

object LssTrialDesign:
  def fromMatrix(value: DMat, trialNames: Vector[String] = Vector.empty): Either[FitError, LssTrialDesign] =
    if value.rows == 0 || value.cols == 0 then Left(FitError.EmptyDesign)
    else
      val names =
        if trialNames.isEmpty then Vector.tabulate(value.cols)(i => s"Trial_${i + 1}")
        else trialNames
      if names.length != value.cols then
        Left(FitError.UnsupportedLssDesign(s"trial names length ${names.length} must match trial-design columns ${value.cols}"))
      else if names.exists(_.trim.isEmpty) then
        Left(FitError.UnsupportedLssDesign("trial names must be non-empty"))
      else if names.map(_.trim).distinct.length != names.length then
        Left(FitError.UnsupportedLssDesign("trial names must be unique"))
      else Right(new LssTrialDesign(value, names.map(_.trim)))

  def unsafe(value: DMat, trialNames: Vector[String] = Vector.empty): LssTrialDesign =
    fromMatrix(value, trialNames).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class LssFixedDesign private (value: DMat, columnNames: Vector[String]):
  require(columnNames.length == value.cols, "fixed-design column names must match columns")

  def timepoints: Int = value.rows
  def predictors: Int = value.cols
  def isEmpty: Boolean = value.cols == 0

object LssFixedDesign:
  def empty(timepoints: Int): LssFixedDesign =
    require(timepoints > 0, "timepoints must be positive")
    new LssFixedDesign(DMat.zeros(timepoints, 0), Vector.empty)

  def fromMatrix(value: DMat, columnNames: Vector[String] = Vector.empty): Either[FitError, LssFixedDesign] =
    if value.rows == 0 then Left(FitError.EmptyDesign)
    else
      val names =
        if columnNames.isEmpty then Vector.tabulate(value.cols)(i => s"fixed_${i + 1}")
        else columnNames
      if names.length != value.cols then
        Left(FitError.UnsupportedLssDesign(s"fixed-design column names length ${names.length} must match columns ${value.cols}"))
      else Right(new LssFixedDesign(value, names))

  def unsafe(value: DMat, columnNames: Vector[String] = Vector.empty): LssFixedDesign =
    fromMatrix(value, columnNames).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class LssOptions(
    eps: Double = 1e-12,
    rankTol: Double = 1e-7
):
  require(eps > 0.0 && eps.isFinite, "eps must be positive and finite")
  require(rankTol >= 0.0 && rankTol.isFinite, "rankTol must be non-negative and finite")

final case class LssDiagnostics(
    fixedRank: Int,
    zeroTrialRegressors: Vector[String],
    degenerateOtherRegressors: Vector[String],
    nonEstimableTrials: Vector[String] = Vector.empty
):
  require(fixedRank >= 0, "fixed rank must be non-negative")

enum LssTrialStatus:
  case Active
  case ZeroTrial
  case TargetOnly

final case class LssTrialWorkspace private[fit] (
    trialName: String,
    trialIndex: Int,
    trialNorm2: Double,
    otherNorm2: Double,
    trialOtherDot: Double,
    alpha: Double,
    denominator: Double,
    status: LssTrialStatus
):
  require(trialName.nonEmpty, "trial workspace name must be non-empty")
  require(trialIndex >= 0, "trial workspace index must be non-negative")
  require(trialNorm2 >= 0.0 && trialNorm2.isFinite, "trial norm must be non-negative and finite")
  require(otherNorm2 >= 0.0 && otherNorm2.isFinite, "other-trial norm must be non-negative and finite")
  require(trialOtherDot.isFinite, "trial/other dot product must be finite")
  require(alpha.isFinite, "trial workspace alpha must be finite")
  require(denominator >= 0.0 && denominator.isFinite, "trial denominator must be non-negative and finite")

private[fit] enum LssFixedProjection:
  case Identity(timepoints: Int)
  case Qr(qr: QR)

  def rank: Int =
    this match
      case Identity(_) => 0
      case Qr(qr)     => qr.diagnostics.rank.getOrElse(qr.r.rows)

  def residualize(data: DMat): Either[FitError, DMat] =
    this match
      case Identity(_) =>
        Right(Matrix.tabulate(data.rows, data.cols)(data.apply))
      case Qr(qr) =>
        qr.residualize(data).left.map(FitError.SingularDesign.apply)

object LssFixedProjection:
  def fromFixed(fixed: LssFixedDesign, rankTol: Double): LssFixedProjection =
    if fixed.isEmpty then LssFixedProjection.Identity(fixed.timepoints)
    else LssFixedProjection.Qr(fixed.value.qr(QROptions(QRPivoting.Column, Some(rankTol))))

final case class LssPreparedDesign private[fit] (
    residualizedTrials: DMat,
    private[fit] val totalTrialSignal: Array[Double],
    private[fit] val fixedProjection: LssFixedProjection,
    workspaces: Vector[LssTrialWorkspace],
    diagnostics: LssDiagnostics,
    options: LssOptions
):
  require(residualizedTrials.cols == workspaces.length, "workspace count must match trial columns")
  require(totalTrialSignal.length == residualizedTrials.rows, "total trial signal must match design rows")

  def timepoints: Int = residualizedTrials.rows
  def trials: Int = residualizedTrials.cols
  def fixedRank: Int = diagnostics.fixedRank
  def trialNames: Vector[String] = workspaces.map(_.trialName)

  def trialWorkspace(trialName: String): Option[LssTrialWorkspace] =
    workspaces.find(_.trialName == trialName)

  private lazy val cachedTrialReadout: Either[FitError, TrialReadout] =
    LssTrialReadout.fromPrepared(this)

  def trialReadout: Either[FitError, TrialReadout] =
    cachedTrialReadout

  def fit(response: ResponseBlock): Either[FitError, LssFit] =
    LeastSquaresSeparate.fit(this, response)

final case class LssFit(
    coefficients: CoefficientBlock,
    trialNames: Vector[String],
    diagnostics: LssDiagnostics
):
  require(trialNames.length == coefficients.predictors, "trial names must match coefficient rows")

  def trials: Int = coefficients.predictors
  def voxels: Int = coefficients.voxels

  def coefficient(trialName: String, voxel: Int): Option[Double] =
    val row = trialNames.indexOf(trialName)
    if row < 0 || voxel < 0 || voxel >= voxels then None else Some(coefficients(row, voxel))

object LeastSquaresSeparate:
  def prepare(
      trials: LssTrialDesign
  ): Either[FitError, LssPreparedDesign] =
    prepare(trials, LssFixedDesign.empty(trials.timepoints), LssOptions())

  def prepare(
      trials: LssTrialDesign,
      options: LssOptions
  ): Either[FitError, LssPreparedDesign] =
    prepare(trials, LssFixedDesign.empty(trials.timepoints), options)

  def prepare(
      trials: LssTrialDesign,
      fixed: LssFixedDesign,
      options: LssOptions = LssOptions()
  ): Either[FitError, LssPreparedDesign] =
    if fixed.timepoints != trials.timepoints then
      Left(FitError.RowMismatch(trials.timepoints, fixed.timepoints))
    else if containsNonFinite(trials.value) then Left(FitError.NonFiniteInput("LSS trial design"))
    else if containsNonFinite(fixed.value) then Left(FitError.NonFiniteInput("LSS fixed design"))
    else
      val projection = LssFixedProjection.fromFixed(fixed, options.rankTol)
      projection.residualize(trials.value).flatMap { residualizedTrials =>
        buildPreparedDesign(
          residualizedTrials = residualizedTrials,
          trialNames = trials.trialNames,
          fixedRank = projection.rank,
          fixedProjection = projection,
          options = options
        )
      }

  def unsafePrepare(
      trials: LssTrialDesign
  ): LssPreparedDesign =
    unsafePrepare(trials, LssFixedDesign.empty(trials.timepoints), LssOptions())

  def unsafePrepare(
      trials: LssTrialDesign,
      options: LssOptions
  ): LssPreparedDesign =
    unsafePrepare(trials, LssFixedDesign.empty(trials.timepoints), options)

  def unsafePrepare(
      trials: LssTrialDesign,
      fixed: LssFixedDesign,
      options: LssOptions = LssOptions()
  ): LssPreparedDesign =
    prepare(trials, fixed, options).fold(error => throw new IllegalArgumentException(error.message), identity)

  def fit(
      trials: LssTrialDesign,
      response: ResponseBlock
  ): Either[FitError, LssFit] =
    fit(trials, response, LssFixedDesign.empty(trials.timepoints), LssOptions())

  def fit(
      trials: LssTrialDesign,
      response: ResponseBlock,
      options: LssOptions
  ): Either[FitError, LssFit] =
    fit(trials, response, LssFixedDesign.empty(trials.timepoints), options)

  def fit(
      trials: LssTrialDesign,
      response: ResponseBlock,
      fixed: LssFixedDesign,
      options: LssOptions = LssOptions()
  ): Either[FitError, LssFit] =
    prepare(trials, fixed, options).flatMap(fit(_, response))

  def fit(
      prepared: LssPreparedDesign,
      response: ResponseBlock
  ): Either[FitError, LssFit] =
    prepared.trialReadout.flatMap: readout =>
      readout.forward(response).map: coefficients =>
        LssFit(
          coefficients = coefficients,
          trialNames = prepared.trialNames,
          diagnostics = prepared.diagnostics
        )

  def unsafeFit(
      trials: LssTrialDesign,
      response: ResponseBlock
  ): LssFit =
    unsafeFit(trials, response, LssFixedDesign.empty(trials.timepoints), LssOptions())

  def unsafeFit(
      trials: LssTrialDesign,
      response: ResponseBlock,
      options: LssOptions
  ): LssFit =
    unsafeFit(trials, response, LssFixedDesign.empty(trials.timepoints), options)

  def unsafeFit(
      trials: LssTrialDesign,
      response: ResponseBlock,
      fixed: LssFixedDesign,
      options: LssOptions = LssOptions()
  ): LssFit =
    fit(trials, response, fixed, options).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def containsNonFinite(matrix: DMat): Boolean =
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        if !matrix(row, col).isFinite then return true
        col += 1
      row += 1
    false

  private def buildPreparedDesign(
      residualizedTrials: DMat,
      trialNames: Vector[String],
      fixedRank: Int,
      fixedProjection: LssFixedProjection,
      options: LssOptions
  ): Either[FitError, LssPreparedDesign] =
    val zero = Vector.newBuilder[String]
    val degenerateOther = Vector.newBuilder[String]
    val nonEstimable = Vector.newBuilder[String]
    val workspaces = Vector.newBuilder[LssTrialWorkspace]
    val eps = options.eps

    val total = new Array[Double](residualizedTrials.rows)
    var row = 0
    while row < residualizedTrials.rows do
      var trial = 0
      while trial < residualizedTrials.cols do
        total(row) += residualizedTrials(row, trial)
        trial += 1
      row += 1

    var trial = 0
    while trial < residualizedTrials.cols do
      var norm2 = 0.0
      var otherNorm2 = 0.0
      var crossOther = 0.0
      row = 0
      while row < residualizedTrials.rows do
        val c = residualizedTrials(row, trial)
        val other = total(row) - c
        norm2 += c * c
        otherNorm2 += other * other
        crossOther += c * other
        row += 1
      if norm2 <= eps then zero += trialNames(trial)
      if residualizedTrials.cols > 1 && otherNorm2 <= eps then degenerateOther += trialNames(trial)
      val workspace =
        if norm2 <= eps then
          LssTrialWorkspace(
            trialName = trialNames(trial),
            trialIndex = trial,
            trialNorm2 = norm2,
            otherNorm2 = otherNorm2,
            trialOtherDot = crossOther,
            alpha = 0.0,
            denominator = 0.0,
            status = LssTrialStatus.ZeroTrial
          )
        else if residualizedTrials.cols > 1 && otherNorm2 > eps then
          val residualizedTargetNorm2 = norm2 - (crossOther * crossOther) / otherNorm2
          if residualizedTargetNorm2 <= eps then nonEstimable += trialNames(trial)
          LssTrialWorkspace(
            trialName = trialNames(trial),
            trialIndex = trial,
            trialNorm2 = norm2,
            otherNorm2 = otherNorm2,
            trialOtherDot = crossOther,
            alpha = crossOther / otherNorm2,
            denominator = math.max(residualizedTargetNorm2, 0.0),
            status = LssTrialStatus.Active
          )
        else
          LssTrialWorkspace(
            trialName = trialNames(trial),
            trialIndex = trial,
            trialNorm2 = norm2,
            otherNorm2 = otherNorm2,
            trialOtherDot = crossOther,
            alpha = 0.0,
            denominator = norm2,
            status = LssTrialStatus.TargetOnly
          )

      workspaces += workspace
      trial += 1

    val nonEstimableTrials = nonEstimable.result()
    if nonEstimableTrials.nonEmpty then Left(FitError.NonEstimableLssTrials(nonEstimableTrials))
    else
      Right(
        LssPreparedDesign(
          residualizedTrials = residualizedTrials,
          totalTrialSignal = total,
          fixedProjection = fixedProjection,
          workspaces = workspaces.result(),
          diagnostics = LssDiagnostics(
            fixedRank = fixedRank,
            zeroTrialRegressors = zero.result(),
            degenerateOtherRegressors = degenerateOther.result()
          ),
          options = options
        )
      )
