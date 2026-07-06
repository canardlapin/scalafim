package scalafim.fmri.fit

import scalafim.linalg.{Cholesky, DoubleMatrix, DoubleVector}

final case class OlsPrepared(
    design: DesignMatrix,
    crossproduct: DoubleMatrix,
    factor: Cholesky
):
  def fit(response: ResponseBlock): Either[FitError, OlsFit] =
    if response.timepoints != design.timepoints then
      Left(FitError.RowMismatch(design.timepoints, response.timepoints))
    else
      val xty = DoubleMatrix.transposeMultiply(design.value, response.value)
      val coefficients = CoefficientBlock(factor.solve(xty))
      val residualVariance =
        Ols.residualVariance(design.value, response.value, coefficients.value)
      val normalizedCovariance = factor.solve(DoubleMatrix.eye(design.predictors))
      Right(
        OlsFit(
          coefficients = coefficients,
          residualVariance = residualVariance,
          residualDegreesOfFreedom = design.timepoints - design.predictors,
          normalizedCovariance = normalizedCovariance,
          standardErrors = StandardErrorBlock(
            Ols.standardErrors(normalizedCovariance, residualVariance, response.voxels)
          )
        )
      )

  def unsafeFit(response: ResponseBlock): OlsFit =
    fit(response).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class OlsFit(
    coefficients: CoefficientBlock,
    residualVariance: DoubleVector,
    residualDegreesOfFreedom: Int,
    normalizedCovariance: DoubleMatrix,
    standardErrors: StandardErrorBlock
):
  def predictors: Int = coefficients.predictors
  def voxels: Int = coefficients.voxels

object Ols:
  def prepare(design: DesignMatrix): Either[FitError, OlsPrepared] =
    val xtx = DoubleMatrix.transposeMultiply(design.value, design.value)
    Cholesky
      .decompose(xtx)
      .left
      .map(FitError.SingularDesign.apply)
      .map(cholesky => OlsPrepared(design, xtx, cholesky))

  def unsafePrepare(design: DesignMatrix): OlsPrepared =
    prepare(design).fold(error => throw new IllegalArgumentException(error.message), identity)

  def fit(design: DesignMatrix, response: ResponseBlock): Either[FitError, OlsFit] =
    prepare(design).flatMap(_.fit(response))

  def unsafeFit(design: DesignMatrix, response: ResponseBlock): OlsFit =
    fit(design, response).fold(error => throw new IllegalArgumentException(error.message), identity)

  private[fit] def residualVariance(
      design: DoubleMatrix,
      response: DoubleMatrix,
      coefficients: DoubleMatrix
  ): DoubleVector =
    val df = design.rows - design.cols
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
      out(voxel) = if df > 0 then sse / df else Double.NaN
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
