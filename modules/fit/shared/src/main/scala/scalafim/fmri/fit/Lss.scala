package scalafim.fmri.fit

import scalafim.linalg.{DoubleMatrix, QrDecomposition}

final case class LssTrialDesign private (value: DoubleMatrix, trialNames: Vector[String]):
  require(trialNames.length == value.cols, "trial names must match trial-design columns")

  def timepoints: Int = value.rows
  def trials: Int = value.cols

object LssTrialDesign:
  def fromMatrix(value: DoubleMatrix, trialNames: Vector[String] = Vector.empty): Either[FitError, LssTrialDesign] =
    if value.rows == 0 || value.cols == 0 then Left(FitError.EmptyDesign)
    else
      val names =
        if trialNames.isEmpty then Vector.tabulate(value.cols)(i => s"Trial_${i + 1}")
        else trialNames
      if names.length != value.cols then
        Left(FitError.UnsupportedLssDesign(s"trial names length ${names.length} must match trial-design columns ${value.cols}"))
      else Right(new LssTrialDesign(value, names))

  def unsafe(value: DoubleMatrix, trialNames: Vector[String] = Vector.empty): LssTrialDesign =
    fromMatrix(value, trialNames).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class LssFixedDesign private (value: DoubleMatrix, columnNames: Vector[String]):
  require(columnNames.length == value.cols, "fixed-design column names must match columns")

  def timepoints: Int = value.rows
  def predictors: Int = value.cols
  def isEmpty: Boolean = value.cols == 0

object LssFixedDesign:
  def empty(timepoints: Int): LssFixedDesign =
    require(timepoints > 0, "timepoints must be positive")
    new LssFixedDesign(DoubleMatrix.zeros(timepoints, 0), Vector.empty)

  def fromMatrix(value: DoubleMatrix, columnNames: Vector[String] = Vector.empty): Either[FitError, LssFixedDesign] =
    if value.rows == 0 then Left(FitError.EmptyDesign)
    else
      val names =
        if columnNames.isEmpty then Vector.tabulate(value.cols)(i => s"fixed_${i + 1}")
        else columnNames
      if names.length != value.cols then
        Left(FitError.UnsupportedLssDesign(s"fixed-design column names length ${names.length} must match columns ${value.cols}"))
      else Right(new LssFixedDesign(value, names))

  def unsafe(value: DoubleMatrix, columnNames: Vector[String] = Vector.empty): LssFixedDesign =
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
  case Qr(qr: QrDecomposition)

  def rank: Int =
    this match
      case Identity(_) => 0
      case Qr(qr)     => qr.rank

  def residualize(data: DoubleMatrix): DoubleMatrix =
    this match
      case Identity(_) =>
        DoubleMatrix.unsafe(data.rows, data.cols, data.copyData)
      case Qr(qr) =>
        qr.residualize(data)

object LssFixedProjection:
  def fromFixed(fixed: LssFixedDesign, rankTol: Double): LssFixedProjection =
    if fixed.isEmpty then LssFixedProjection.Identity(fixed.timepoints)
    else LssFixedProjection.Qr(QrDecomposition.decompose(fixed.value, pivoting = true, tol = rankTol))

final case class LssPreparedDesign private[fit] (
    residualizedTrials: DoubleMatrix,
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
      val residualizedTrials = projection.residualize(trials.value)
      buildPreparedDesign(
        residualizedTrials = residualizedTrials,
        trialNames = trials.trialNames,
        fixedRank = projection.rank,
        fixedProjection = projection,
        options = options
      )

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
    if response.timepoints != prepared.timepoints then
      Left(FitError.RowMismatch(prepared.timepoints, response.timepoints))
    else if containsNonFinite(response.value) then Left(FitError.NonFiniteInput("LSS response block"))
    else
      val residualizedY = prepared.fixedProjection.residualize(response.value)
      val beta = computeBetas(prepared, residualizedY)
      Right(
        LssFit(
          coefficients = CoefficientBlock(beta),
          trialNames = prepared.trialNames,
          diagnostics = prepared.diagnostics
        )
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

  private def containsNonFinite(matrix: DoubleMatrix): Boolean =
    var i = 0
    while i < matrix.dataArray.length do
      if !matrix.dataArray(i).isFinite then return true
      i += 1
    false

  private[fit] def computeBetas(prepared: LssPreparedDesign, response: DoubleMatrix): DoubleMatrix =
    require(prepared.timepoints == response.rows, s"prepared design rows ${prepared.timepoints} != response rows ${response.rows}")
    require(response.cols > 0, "response block must have at least one column")

    val trials = prepared.residualizedTrials
    val n = prepared.timepoints
    val nTrials = prepared.trials
    val nVoxels = response.cols
    val totalY = new Array[Double](nVoxels)
    val ctY = new Array[Double](nTrials * nVoxels)

    var row = 0
    while row < n do
      val total = prepared.totalTrialSignal(row)
      var voxel = 0
      while voxel < nVoxels do
        totalY(voxel) += total * response.dataArray(row * nVoxels + voxel)
        voxel += 1

      var trial = 0
      while trial < nTrials do
        val c = trials.dataArray(row * nTrials + trial)
        voxel = 0
        while voxel < nVoxels do
          ctY(trial * nVoxels + voxel) += c * response.dataArray(row * nVoxels + voxel)
          voxel += 1
        trial += 1

      row += 1

    val out = new Array[Double](nTrials * nVoxels)
    var trial = 0
    while trial < nTrials do
      val workspace = prepared.workspaces(trial)
      workspace.status match
        case LssTrialStatus.ZeroTrial =>
          ()
        case LssTrialStatus.Active | LssTrialStatus.TargetOnly =>
          var voxel = 0
          while voxel < nVoxels do
            val cty = ctY(trial * nVoxels + voxel)
            val num = (1.0 + workspace.alpha) * cty - workspace.alpha * totalY(voxel)
            out(trial * nVoxels + voxel) = num / workspace.denominator
            voxel += 1

      trial += 1

    DoubleMatrix.unsafe(nTrials, nVoxels, out)

  private def buildPreparedDesign(
      residualizedTrials: DoubleMatrix,
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
        total(row) += residualizedTrials.dataArray(row * residualizedTrials.cols + trial)
        trial += 1
      row += 1

    var trial = 0
    while trial < residualizedTrials.cols do
      var norm2 = 0.0
      var otherNorm2 = 0.0
      var crossOther = 0.0
      row = 0
      while row < residualizedTrials.rows do
        val c = residualizedTrials.dataArray(row * residualizedTrials.cols + trial)
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
