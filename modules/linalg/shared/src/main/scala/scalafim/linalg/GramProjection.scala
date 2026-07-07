package scalafim.linalg

import scala.annotation.targetName

object GramProjection:
  final case class Projection(
      coefficients: DoubleMatrix,
      fitted: DoubleMatrix
  )

  @targetName("solveGramTyped")
  def solveGram(
      gram: DoubleMatrix,
      rhs: DoubleMatrix,
      ridge: Double = 0.0,
      tol: Double = 1e-12
  ): Either[LinearAlgebraError, DoubleMatrix] =
    for
      ridgeValue <- Ridge(ridge)
      tolerance <- Tolerance(tol)
      coefficients <- solveGram(gram, rhs, ridgeValue, tolerance)
    yield coefficients

  def solveGram(
      gram: DoubleMatrix,
      rhs: DoubleMatrix,
      ridge: Ridge,
      tolerance: Tolerance
  ): Either[LinearAlgebraError, DoubleMatrix] =
    if gram.rows != gram.cols then Left(LinearAlgebraError.NonSquareMatrix(gram.rows, gram.cols))
    else if rhs.rows != gram.rows then Left(LinearAlgebraError.DimensionMismatch(DimensionRole.RightHandSideRows, gram.rows, rhs.rows))
    else
      firstNonFinite(MatrixValueRole.Gram, gram)
        .orElse(firstNonFinite(MatrixValueRole.RightHandSide, rhs)) match
        case Some(error) =>
          Left(error)
        case None =>
          val regularized =
            if ridge.value == 0.0 then gram
            else gram.addToDiagonal(ridge.value)
          for
            square <- SquareMatrix.from(regularized)
            factor <- Cholesky.decompose(square, tolerance)
          yield factor.solve(rhs)

  @targetName("coefficientsTyped")
  def coefficients(
      data: DoubleMatrix,
      basis: DoubleMatrix,
      ridge: Double = 0.0,
      tol: Double = 1e-12
  ): Either[LinearAlgebraError, DoubleMatrix] =
    for
      ridgeValue <- Ridge(ridge)
      tolerance <- Tolerance(tol)
      coefficients <- coefficients(data, basis, ridgeValue, tolerance)
    yield coefficients

  def coefficients(
      data: DoubleMatrix,
      basis: DoubleMatrix,
      ridge: Ridge,
      tolerance: Tolerance
  ): Either[LinearAlgebraError, DoubleMatrix] =
    if data.rows != basis.rows then Left(LinearAlgebraError.DimensionMismatch(DimensionRole.DataRows, basis.rows, data.rows))
    else
      firstNonFinite(MatrixValueRole.Data, data)
        .orElse(firstNonFinite(MatrixValueRole.Basis, basis)) match
        case Some(error) =>
          Left(error)
        case None =>
          val gram = DoubleMatrix.crossProduct(basis)
          val rhs = DoubleMatrix.transposeMultiply(basis, data)
          solveGram(gram, rhs, ridge, tolerance)

  @targetName("projectTyped")
  def project(
      data: DoubleMatrix,
      basis: DoubleMatrix,
      ridge: Double = 0.0,
      tol: Double = 1e-12
  ): Either[LinearAlgebraError, Projection] =
    for
      ridgeValue <- Ridge(ridge)
      tolerance <- Tolerance(tol)
      projection <- project(data, basis, ridgeValue, tolerance)
    yield projection

  def project(
      data: DoubleMatrix,
      basis: DoubleMatrix,
      ridge: Ridge,
      tolerance: Tolerance
  ): Either[LinearAlgebraError, Projection] =
    coefficients(data, basis, ridge, tolerance).map { coef =>
      Projection(
        coefficients = coef,
        fitted = DoubleMatrix.multiply(basis, coef)
      )
    }

  private def firstNonFinite(role: MatrixValueRole, matrix: DoubleMatrix): Option[LinearAlgebraError] =
    var i = 0
    var error = Option.empty[LinearAlgebraError]
    while i < matrix.dataArray.length && error.isEmpty do
      val value = matrix.dataArray(i)
      if !value.isFinite then error = Some(LinearAlgebraError.NonFiniteValue(role, i, value))
      i += 1
    error
