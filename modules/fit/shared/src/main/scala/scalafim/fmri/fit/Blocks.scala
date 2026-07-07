package scalafim.fmri.fit

import scalafim.linalg.DoubleMatrix

final case class DesignMatrix private (value: DoubleMatrix):
  def timepoints: Int = value.rows
  def predictors: Int = value.cols

object DesignMatrix:
  def fromMatrix(value: DoubleMatrix): Either[FitError, DesignMatrix] =
    if value.rows == 0 || value.cols == 0 then Left(FitError.EmptyDesign)
    else if containsNonFinite(value) then Left(FitError.NonFiniteInput("design matrix"))
    else Right(new DesignMatrix(value))

  def unsafe(value: DoubleMatrix): DesignMatrix =
    fromMatrix(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class ResponseBlock private (value: DoubleMatrix):
  def timepoints: Int = value.rows
  def voxels: Int = value.cols

object ResponseBlock:
  def fromMatrix(value: DoubleMatrix): Either[FitError, ResponseBlock] =
    if value.rows == 0 || value.cols == 0 then Left(FitError.EmptyResponse)
    else if containsNonFinite(value) then Left(FitError.NonFiniteInput("response block"))
    else Right(new ResponseBlock(value))

  def unsafe(value: DoubleMatrix): ResponseBlock =
    fromMatrix(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class CoefficientBlock(value: DoubleMatrix):
  def predictors: Int = value.rows
  def voxels: Int = value.cols
  def apply(predictor: Int, voxel: Int): Double = value(predictor, voxel)

final case class StandardErrorBlock(value: DoubleMatrix):
  def predictors: Int = value.rows
  def voxels: Int = value.cols
  def apply(predictor: Int, voxel: Int): Double = value(predictor, voxel)

private def containsNonFinite(value: DoubleMatrix): Boolean =
  var i = 0
  var found = false
  while i < value.dataArray.length && !found do
    if !value.dataArray(i).isFinite then found = true
    i += 1
  found
