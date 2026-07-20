package scalafim.fmri.fit

import gale.linalg.DMat

final case class DesignMatrix private (value: DMat):
  def timepoints: Int = value.rows
  def predictors: Int = value.cols

object DesignMatrix:
  def fromMatrix(value: DMat): Either[FitError, DesignMatrix] =
    if value.rows == 0 || value.cols == 0 then Left(FitError.EmptyDesign)
    else if containsNonFinite(value) then Left(FitError.NonFiniteInput("design matrix"))
    else Right(new DesignMatrix(value))

  def unsafe(value: DMat): DesignMatrix =
    fromMatrix(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class ResponseBlock private (value: DMat):
  def timepoints: Int = value.rows
  def voxels: Int = value.cols

object ResponseBlock:
  def fromMatrix(value: DMat): Either[FitError, ResponseBlock] =
    if value.rows == 0 || value.cols == 0 then Left(FitError.EmptyResponse)
    else if containsNonFinite(value) then Left(FitError.NonFiniteInput("response block"))
    else Right(new ResponseBlock(value))

  def unsafe(value: DMat): ResponseBlock =
    fromMatrix(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class CoefficientBlock(value: DMat):
  def predictors: Int = value.rows
  def voxels: Int = value.cols
  def apply(predictor: Int, voxel: Int): Double = value(predictor, voxel)

final case class StandardErrorBlock(value: DMat):
  def predictors: Int = value.rows
  def voxels: Int = value.cols
  def apply(predictor: Int, voxel: Int): Double = value(predictor, voxel)

private def containsNonFinite(value: DMat): Boolean =
  var row = 0
  var found = false
  while row < value.rows && !found do
    var col = 0
    while col < value.cols && !found do
      if !value(row, col).isFinite then found = true
      col += 1
    row += 1
  found
