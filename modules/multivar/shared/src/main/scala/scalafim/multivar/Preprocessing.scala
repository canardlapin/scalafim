package scalafim.multivar

import scalafim.linalg.DoubleVector

enum PreprocessSpec:
  case Pass
  case Center
  case Scale(weights: DoubleVector)
  case Standardize

  def fit(input: MatrixView): Either[MultivarError, FittedPreprocessor] =
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
        yield
          val safeSds = new Array[Double](sds.length)
          var col = 0
          while col < sds.length do
            val sd = sds(col)
            safeSds(col) =
              if sd.isFinite && sd > 1e-12 then sd
              else 1.0
            col += 1
          val scale = MatrixView.invert(DoubleVector.unsafe(safeSds)).toOption.get
          FittedColumnAffine(input.cols, scale, MatrixView.multiply(MatrixView.negate(means), scale))

object PreprocessSpec:
  def scale(weights: Seq[Double]): Either[MultivarError, PreprocessSpec] =
    val vector = DoubleVector.fromSeq(weights)
    MatrixView.requireFinite("preprocessing weights", vector).map(_ => PreprocessSpec.Scale(vector))

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
    scale: DoubleVector,
    shift: DoubleVector
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
  ): Either[MultivarError, (DoubleVector, DoubleVector)] =
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

