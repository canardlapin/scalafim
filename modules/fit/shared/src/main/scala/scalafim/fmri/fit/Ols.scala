package scalafim.fmri.fit

import gale.linalg.{Cholesky, CholeskyOptions, DMat, DVec, Matrix, QR, QROptions, QRPivoting, Vec}

enum OlsSolveMethod:
  case QrRankRevealing
  case CholeskyNormalEquations

enum OlsRankPolicy:
  case StrictFullRank
  case RidgeRegularized(ridge: Double)
  case MinimumNorm

  def supported: Boolean =
    this match
      case StrictFullRank => true
      case _              => false

  def label: String =
    this match
      case StrictFullRank          => "strict full rank"
      case RidgeRegularized(ridge) => s"ridge regularized (ridge=$ridge)"
      case MinimumNorm             => "minimum norm"

final case class OlsSolvePolicy(
    method: OlsSolveMethod = OlsSolveMethod.QrRankRevealing,
    rankPolicy: OlsRankPolicy = OlsRankPolicy.StrictFullRank,
    rankTolerance: Double = 1e-7,
    choleskyTolerance: Double = 1e-12
):
  require(rankTolerance >= 0.0 && rankTolerance.isFinite, "OLS rank tolerance must be finite and non-negative")
  require(choleskyTolerance >= 0.0 && choleskyTolerance.isFinite, "OLS Cholesky tolerance must be finite and non-negative")

object OlsSolvePolicy:
  val Default: OlsSolvePolicy =
    OlsSolvePolicy()

  val NormalEquations: OlsSolvePolicy =
    OlsSolvePolicy(method = OlsSolveMethod.CholeskyNormalEquations)

final case class OlsDiagnostics(
    solveMethod: OlsSolveMethod,
    predictors: Int,
    rank: Int,
    policy: OlsSolvePolicy
):
  require(predictors > 0, "OLS diagnostics require at least one predictor")
  require(rank >= 0 && rank <= predictors, "OLS rank must be between 0 and predictor count")
  def fullRank: Boolean = rank == predictors

final class OlsPrepared private[fit] (
    val design: DesignMatrix,
    val crossproduct: DMat,
    private val solver: OlsPreparedSolver,
    val normalizedCovariance: DMat,
    val diagnostics: OlsDiagnostics
):
  def policy: OlsSolvePolicy = diagnostics.policy

  def fit(response: ResponseBlock): Either[FitError, OlsFit] =
    if response.timepoints != design.timepoints then
      Left(FitError.RowMismatch(design.timepoints, response.timepoints))
    else
      for
        residualDf <- ResidualDegreesOfFreedom(design.timepoints - design.predictors)
        coefficientMatrix <- solver.coefficients(design, response)
      yield
        val coefficients = CoefficientBlock(coefficientMatrix)
        val residualVariance =
          Ols.residualVariance(design.value, response.value, coefficients.value, residualDf)
        OlsFit(
          coefficients = coefficients,
          residualVariance = residualVariance,
          residualDegreesOfFreedom = residualDf,
          normalizedCovariance = normalizedCovariance,
          standardErrors = StandardErrorBlock(
            Ols.standardErrors(normalizedCovariance, residualVariance, response.voxels)
          ),
          diagnostics = diagnostics,
          coefficientCovariance = CoefficientCovariance.unsafeShared(normalizedCovariance)
        )

  def unsafeFit(response: ResponseBlock): OlsFit =
    fit(response).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class OlsFit(
    coefficients: CoefficientBlock,
    residualVariance: DVec,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    normalizedCovariance: DMat,
    standardErrors: StandardErrorBlock,
    diagnostics: OlsDiagnostics,
    coefficientCovariance: CoefficientCovariance
):
  require(coefficientCovariance.predictors == coefficients.predictors, "OLS coefficient covariance must match coefficient rows")
  require(coefficientCovariance.validateVoxelCount(coefficients.voxels).isRight, "OLS coefficient covariance must be shared or match voxel count")
  def predictors: Int = coefficients.predictors
  def voxels: Int = coefficients.voxels

object Ols:
  def prepare(
      design: DesignMatrix,
      policy: OlsSolvePolicy = OlsSolvePolicy.Default
  ): Either[FitError, OlsPrepared] =
    val xtx = design.value.t * design.value
    if !policy.rankPolicy.supported then
      Left(FitError.UnsupportedLeastSquaresPolicy(s"OLS currently supports ${OlsRankPolicy.StrictFullRank.label}; got ${policy.rankPolicy.label}"))
    else policy.method match
      case OlsSolveMethod.QrRankRevealing =>
        val qr = design.value.qr(QROptions(QRPivoting.Column, Some(policy.rankTolerance)))
        qr.normalizedCovariance
          .left
          .map(FitError.SingularDesign.apply)
          .map { covariance =>
            new OlsPrepared(
              design = design,
              crossproduct = xtx,
              solver = OlsPreparedSolver.Qr(qr),
              normalizedCovariance = covariance,
              diagnostics = OlsDiagnostics(
                solveMethod = OlsSolveMethod.QrRankRevealing,
                predictors = design.predictors,
                rank = qr.diagnostics.rank.getOrElse(design.predictors),
                policy = policy
              )
            )
          }
      case OlsSolveMethod.CholeskyNormalEquations =>
        for
          cholesky <- xtx
            .cholesky(CholeskyOptions(policy.choleskyTolerance))
            .left
            .map(FitError.SingularDesign.apply)
          covariance <- cholesky
            .solve(Matrix.eye(design.predictors))
            .left
            .map(FitError.SingularDesign.apply)
        yield new OlsPrepared(
          design = design,
          crossproduct = xtx,
          solver = OlsPreparedSolver.NormalEquations(cholesky),
          normalizedCovariance = covariance,
          diagnostics = OlsDiagnostics(
            solveMethod = OlsSolveMethod.CholeskyNormalEquations,
            predictors = design.predictors,
            rank = design.predictors,
            policy = policy
          )
        )

  def unsafePrepare(
      design: DesignMatrix,
      policy: OlsSolvePolicy = OlsSolvePolicy.Default
  ): OlsPrepared =
    prepare(design, policy).fold(error => throw new IllegalArgumentException(error.message), identity)

  def fit(
      design: DesignMatrix,
      response: ResponseBlock,
      policy: OlsSolvePolicy = OlsSolvePolicy.Default
  ): Either[FitError, OlsFit] =
    prepare(design, policy).flatMap(_.fit(response))

  def unsafeFit(
      design: DesignMatrix,
      response: ResponseBlock,
      policy: OlsSolvePolicy = OlsSolvePolicy.Default
  ): OlsFit =
    fit(design, response, policy).fold(error => throw new IllegalArgumentException(error.message), identity)

  private[fit] def residualVariance(
      design: DMat,
      response: DMat,
      coefficients: DMat,
      residualDegreesOfFreedom: ResidualDegreesOfFreedom
  ): DVec =
    val df = residualDegreesOfFreedom.value
    val out = Vec.newBuilder(response.cols)

    var voxel = 0
    while voxel < response.cols do
      var sse = 0.0
      var row = 0
      while row < response.rows do
        var fitted = 0.0
        var predictor = 0
        while predictor < design.cols do
          fitted += design(row, predictor) * coefficients(predictor, voxel)
          predictor += 1
        val residual = response(row, voxel) - fitted
        sse += residual * residual
        row += 1
      out(voxel) = sse / df
      voxel += 1

    out.result()

  private[fit] def standardErrors(
      normalizedCovariance: DMat,
      residualVariance: DVec,
      voxels: Int
  ): DMat =
    require(normalizedCovariance.rows == normalizedCovariance.cols, "normalized covariance must be square")
    require(residualVariance.length == voxels, "residual variance length must match voxel count")
    val predictors = normalizedCovariance.rows
    val out = Matrix.newBuilder(predictors, voxels)
    var predictor = 0
    while predictor < predictors do
      val normalizedVariance = normalizedCovariance(predictor, predictor)
      var voxel = 0
      while voxel < voxels do
        val variance = normalizedVariance * residualVariance(voxel)
        out(predictor, voxel) =
          if variance < 0.0 && variance > -1e-12 then 0.0 else math.sqrt(variance)
        voxel += 1
      predictor += 1
    out.result()

private enum OlsPreparedSolver:
  case Qr(qr: QR)
  case NormalEquations(factor: Cholesky)

  def coefficients(design: DesignMatrix, response: ResponseBlock): Either[FitError, DMat] =
    this match
      case Qr(qr) =>
        qr.solveLeastSquares(response.value).left.map(FitError.SingularDesign.apply)
      case NormalEquations(factor) =>
        val xty = design.value.t * response.value
        factor.solve(xty).left.map(FitError.SingularDesign.apply)
