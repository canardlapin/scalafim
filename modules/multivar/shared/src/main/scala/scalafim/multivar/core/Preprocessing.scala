package scalafim.multivar
package core

import gale.linalg.DVec

enum PreprocessSpec:
  case Pass
  case Center
  case Scale(weights: DVec)

  /** Standardize columns to zero mean and unit sample variance.
    *
    * Degenerate (numerically constant) columns — those whose sample standard deviation
    * is at most `MatrixView.DegenerateScaleEpsilon` relative to the magnitude of the
    * column mean — are centered but not rescaled (their scale weight is 1), matching
    * scikit-learn's `StandardScaler` convention for zero-variance features. The test
    * is relative to the column's own magnitude, so genuine variation in tiny-valued
    * columns is still standardized while rounding noise on huge-valued constant
    * columns is not mistaken for signal.
    */
  case Standardize

  def fit(input: MatrixView): Either[MultivarError, FittedPreprocessor] =
    if input.cols <= 0 then Left(MultivarError.InvalidDimension("preprocessing input columns", input.cols))
    else
      this match
        case Pass =>
          Right(FittedColumnAffine(input.cols, MatrixView.ones(input.cols), MatrixView.zeros(input.cols)))
        case Center =>
          input.columnStats.flatMap(_.means).map { means =>
            FittedColumnAffine(input.cols, MatrixView.ones(input.cols), MatrixView.negate(means))
          }
        case Scale(weights) =>
          for
            _ <- MatrixView.requireVectorLength("preprocessing weights", weights, input.cols)
            _ <- MatrixView.requireFinite("preprocessing weights", weights)
          yield FittedColumnAffine(input.cols, weights, MatrixView.zeros(input.cols))
        case Standardize =>
          for
            stats <- input.columnStats
            means <- stats.means
            sds <- stats.sampleStandardDeviations
            scale <- MatrixView.invert(PreprocessSpec.safeStandardizeScale(means, sds))
          yield FittedColumnAffine(input.cols, scale, MatrixView.multiply(MatrixView.negate(means), scale))

object PreprocessSpec:
  def scale(weights: Seq[Double]): Either[MultivarError, PreprocessSpec] =
    val vector = DVec.fromSeq(weights)
    MatrixView.requireFinite("preprocessing weights", vector).map(_ => PreprocessSpec.Scale(vector))

  /** Column scale for standardization: the sample standard deviation, or 1.0 for
    * degenerate columns whose spread is negligible relative to their mean magnitude
    * (see the `Standardize` case documentation).
    */
  private def safeStandardizeScale(means: DVec, sds: DVec): DVec =
    val out = new Array[Double](sds.length)
    var col = 0
    while col < sds.length do
      val sd = sds(col)
      out(col) =
        if sd > MatrixView.DegenerateScaleEpsilon * Math.abs(means(col)) then sd
        else 1.0
      col += 1
    GaleNumerics.vectorFromArray(out)

trait FittedPreprocessor:
  def inputCols: Int

  def transform(
      input: MatrixView,
      columns: Option[IndexSet] = None,
      policy: StoragePolicy = StoragePolicy.Operator
  ): Either[MultivarError, MatrixView]

  def inverseTransform(
      input: MatrixView,
      columns: Option[IndexSet] = None,
      policy: StoragePolicy = StoragePolicy.Operator
  ): Either[MultivarError, MatrixView]

  def restrict(columns: IndexSet): Either[MultivarError, FittedPreprocessor]

final case class FittedColumnAffine(
    inputCols: Int,
    scale: DVec,
    shift: DVec
) extends FittedPreprocessor:
  require(inputCols > 0, "fitted preprocessor input columns must be positive")
  require(scale.length == inputCols, "scale length must match input columns")
  require(shift.length == inputCols, "shift length must match input columns")

  override def transform(
      input: MatrixView,
      columns: Option[IndexSet],
      policy: StoragePolicy
  ): Either[MultivarError, MatrixView] =
    parametersFor(input, columns).flatMap { case (selectedScale, selectedShift) =>
      MatrixView.affine(input, selectedScale, selectedShift, policy, "preprocessing transform")
    }

  override def inverseTransform(
      input: MatrixView,
      columns: Option[IndexSet],
      policy: StoragePolicy
  ): Either[MultivarError, MatrixView] =
    parametersFor(input, columns).flatMap { case (selectedScale, selectedShift) =>
      MatrixView.invert(selectedScale).flatMap { inverseScale =>
        val inverseShift = MatrixView.multiply(MatrixView.negate(selectedShift), inverseScale)
        MatrixView.affine(input, inverseScale, inverseShift, policy, "preprocessing inverse transform")
      }
    }

  override def restrict(columns: IndexSet): Either[MultivarError, FittedPreprocessor] =
    MatrixView.requireColumnIndexSet(columns, inputCols).map { checked =>
      FittedColumnAffine(
        inputCols = checked.length,
        scale = MatrixView.selectVector(scale, checked),
        shift = MatrixView.selectVector(shift, checked)
      )
    }

  private def parametersFor(
      input: MatrixView,
      columns: Option[IndexSet]
  ): Either[MultivarError, (DVec, DVec)] =
    columns match
      case None =>
        if input.cols != inputCols then
          Left(MultivarError.MatrixShapeMismatch(s"input has ${input.cols} columns but preprocessor expects $inputCols"))
        else Right((scale, shift))
      case Some(indices) =>
        MatrixView.requireColumnIndexSet(indices, inputCols).flatMap { checked =>
          if input.cols != checked.length then
            Left(
              MultivarError.MatrixShapeMismatch(
                s"input has ${input.cols} columns but column selection has ${checked.length}"
              )
            )
          else Right((MatrixView.selectVector(scale, checked), MatrixView.selectVector(shift, checked)))
        }

