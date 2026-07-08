package scalafim.fmri.fit

import scalafim.linalg.{Cholesky, DoubleMatrix, DoubleVector, Pivoting, QrDecomposition, Ridge, Tolerance}

enum OlsSolveMethod:
  case QrRankRevealing
  case CholeskyNormalEquations

enum OlsRankPolicy:
  case StrictFullRank
  case RidgeRegularized(ridge: Ridge)
  case MinimumNorm

  def supported: Boolean =
    this match
      case StrictFullRank => true
      case _              => false

  def label: String =
    this match
      case StrictFullRank          => "strict full rank"
      case RidgeRegularized(ridge) => s"ridge regularized (ridge=${ridge.value})"
      case MinimumNorm             => "minimum norm"

final case class OlsSolvePolicy(
    method: OlsSolveMethod = OlsSolveMethod.QrRankRevealing,
    rankPolicy: OlsRankPolicy = OlsRankPolicy.StrictFullRank,
    rankTolerance: Tolerance = Tolerance.DefaultQr,
    choleskyTolerance: Tolerance = Tolerance.DefaultCholesky
)

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
    val crossproduct: DoubleMatrix,
    private val solver: OlsPreparedSolver,
    val normalizedCovariance: DoubleMatrix,
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
          diagnostics = diagnostics
        )

  def unsafeFit(response: ResponseBlock): OlsFit =
    fit(response).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class OlsFit(
    coefficients: CoefficientBlock,
    residualVariance: DoubleVector,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    normalizedCovariance: DoubleMatrix,
    standardErrors: StandardErrorBlock,
    diagnostics: OlsDiagnostics
):
  def predictors: Int = coefficients.predictors
  def voxels: Int = coefficients.voxels

object Ols:
  def prepare(
      design: DesignMatrix,
      policy: OlsSolvePolicy = OlsSolvePolicy.Default
  ): Either[FitError, OlsPrepared] =
    val xtx = DoubleMatrix.transposeMultiply(design.value, design.value)
    if !policy.rankPolicy.supported then
      Left(FitError.UnsupportedLeastSquaresPolicy(s"OLS currently supports ${OlsRankPolicy.StrictFullRank.label}; got ${policy.rankPolicy.label}"))
    else policy.method match
      case OlsSolveMethod.QrRankRevealing =>
        val qr = QrDecomposition.decompose(design.value, Pivoting.Enabled, policy.rankTolerance)
        qr.normalizedCovarianceFullRank
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
                rank = qr.rank,
                policy = policy
              )
            )
          }
      case OlsSolveMethod.CholeskyNormalEquations =>
        Cholesky
          .decompose(xtx, policy.choleskyTolerance.value)
          .left
          .map(FitError.SingularDesign.apply)
          .map { cholesky =>
            new OlsPrepared(
              design = design,
              crossproduct = xtx,
              solver = OlsPreparedSolver.NormalEquations(cholesky),
              normalizedCovariance = cholesky.solve(DoubleMatrix.eye(design.predictors)),
              diagnostics = OlsDiagnostics(
                solveMethod = OlsSolveMethod.CholeskyNormalEquations,
                predictors = design.predictors,
                rank = design.predictors,
                policy = policy
              )
            )
          }

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
      design: DoubleMatrix,
      response: DoubleMatrix,
      coefficients: DoubleMatrix,
      residualDegreesOfFreedom: ResidualDegreesOfFreedom
  ): DoubleVector =
    val df = residualDegreesOfFreedom.value
    val out = new Array[Double](response.cols)

    var voxel = 0
    while voxel < response.cols do
      var sse = 0.0
      var row = 0
      while row < response.rows do
        var fitted = 0.0
        var predictor = 0
        while predictor < design.cols do
          fitted += design.dataArray(row * design.cols + predictor) *
            coefficients.dataArray(predictor * coefficients.cols + voxel)
          predictor += 1
        val residual = response.dataArray(row * response.cols + voxel) - fitted
        sse += residual * residual
        row += 1
      out(voxel) = sse / df
      voxel += 1

    DoubleVector.unsafe(out)

  private[fit] def standardErrors(
      normalizedCovariance: DoubleMatrix,
      residualVariance: DoubleVector,
      voxels: Int
  ): DoubleMatrix =
    require(normalizedCovariance.rows == normalizedCovariance.cols, "normalized covariance must be square")
    require(residualVariance.length == voxels, "residual variance length must match voxel count")
    val predictors = normalizedCovariance.rows
    val out = new Array[Double](predictors * voxels)
    var predictor = 0
    while predictor < predictors do
      val normalizedVariance = normalizedCovariance(predictor, predictor)
      var voxel = 0
      while voxel < voxels do
        val variance = normalizedVariance * residualVariance(voxel)
        out(predictor * voxels + voxel) =
          if variance < 0.0 && variance > -1e-12 then 0.0 else math.sqrt(variance)
        voxel += 1
      predictor += 1
    DoubleMatrix.unsafe(predictors, voxels, out)

private enum OlsPreparedSolver:
  case Qr(qr: QrDecomposition)
  case NormalEquations(factor: Cholesky)

  def coefficients(design: DesignMatrix, response: ResponseBlock): Either[FitError, DoubleMatrix] =
    this match
      case Qr(qr) =>
        qr.solveFullRank(response.value).left.map(FitError.SingularDesign.apply).map(_.coefficients)
      case NormalEquations(factor) =>
        val xty = DoubleMatrix.transposeMultiply(design.value, response.value)
        Right(factor.solve(xty))
