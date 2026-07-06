package scalafim.linalg

object GramProjection:
  final case class Projection(
      coefficients: DoubleMatrix,
      fitted: DoubleMatrix
  )

  def solveGram(
      gram: DoubleMatrix,
      rhs: DoubleMatrix,
      ridge: Double = 0.0,
      tol: Double = 1e-12
  ): Either[LinearAlgebraError, DoubleMatrix] =
    if gram.rows != gram.cols then Left(LinearAlgebraError.NonSquareMatrix(gram.rows, gram.cols))
    else if rhs.rows != gram.rows then Left(LinearAlgebraError.DimensionMismatch("right-hand side rows", gram.rows, rhs.rows))
    else if ridge < 0.0 || !ridge.isFinite then Left(LinearAlgebraError.InvalidParameter("ridge", ridge))
    else
      firstNonFinite("gram", gram)
        .orElse(firstNonFinite("rhs", rhs)) match
        case Some(error) =>
          Left(error)
        case None =>
          val regularized =
            if ridge == 0.0 then gram
            else gram.addToDiagonal(ridge)
          Cholesky.decompose(regularized, tol = tol).map(_.solve(rhs))

  def coefficients(
      data: DoubleMatrix,
      basis: DoubleMatrix,
      ridge: Double = 0.0,
      tol: Double = 1e-12
  ): Either[LinearAlgebraError, DoubleMatrix] =
    if data.rows != basis.rows then Left(LinearAlgebraError.DimensionMismatch("data rows", basis.rows, data.rows))
    else if ridge < 0.0 || !ridge.isFinite then Left(LinearAlgebraError.InvalidParameter("ridge", ridge))
    else
      firstNonFinite("data", data)
        .orElse(firstNonFinite("basis", basis)) match
        case Some(error) =>
          Left(error)
        case None =>
          val gram = DoubleMatrix.crossProduct(basis)
          val rhs = DoubleMatrix.transposeMultiply(basis, data)
          solveGram(gram, rhs, ridge = ridge, tol = tol)

  def project(
      data: DoubleMatrix,
      basis: DoubleMatrix,
      ridge: Double = 0.0,
      tol: Double = 1e-12
  ): Either[LinearAlgebraError, Projection] =
    coefficients(data, basis, ridge = ridge, tol = tol).map { coef =>
      Projection(
        coefficients = coef,
        fitted = DoubleMatrix.multiply(basis, coef)
      )
    }

  private def firstNonFinite(label: String, matrix: DoubleMatrix): Option[LinearAlgebraError] =
    var i = 0
    var error = Option.empty[LinearAlgebraError]
    while i < matrix.dataArray.length && error.isEmpty do
      val value = matrix.dataArray(i)
      if !value.isFinite then error = Some(LinearAlgebraError.NonFiniteValue(label, i, value))
      i += 1
    error
