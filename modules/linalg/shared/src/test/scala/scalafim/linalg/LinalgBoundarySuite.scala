package scalafim.linalg

class LinalgBoundarySuite extends munit.FunSuite:

  private def value[A](result: Either[LinearAlgebraError, A]): A =
    result match
      case Right(value) => value
      case Left(error)  => fail(error.message)

  test("MatrixShape validates dimensions and row-major storage length"):
    val shape = value(MatrixShape.from(2, 3))
    assertEquals(shape.entries, 6)
    assertEquals(shape.isSquare, false)

    assertEquals(MatrixShape.from(-1, 3).left.toOption, Some(LinearAlgebraError.InvalidMatrixShape(-1, 3)))
    assertEquals(MatrixShape.from(Int.MaxValue, 2).left.toOption, Some(LinearAlgebraError.MatrixTooLarge(Int.MaxValue, 2)))

    val data = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    val matrix = value(DoubleMatrix.fromRowMajor(shape, data))
    data(0) = 99.0

    assertEquals(matrix.shape, shape)
    assertEquals(matrix(0, 0), 1.0)
    assertEquals(
      DoubleMatrix.fromRowMajor(shape, Array(1.0)).left.toOption,
      Some(LinearAlgebraError.MatrixStorageLengthMismatch(shape, 1))
    )

  test("typed row and column indices validate against a matrix shape"):
    val matrix = DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0), Vector(3.0, 4.0)))
    val row = value(RowIndex.in(matrix.shape, 1))
    val col = value(ColIndex.in(matrix.shape, 0))

    assertEquals(matrix(row, col), 3.0)
    assertEquals(matrix.row(row).toVector, Vector(3.0, 4.0))
    assertEquals(matrix.col(col).toVector, Vector(1.0, 3.0))
    assertEquals(RowIndex.in(matrix.shape, 2).left.toOption, Some(LinearAlgebraError.IndexOutOfBounds(MatrixAxis.Row, 2, 2)))
    assertEquals(ColIndex.in(matrix.shape, -1).left.toOption, Some(LinearAlgebraError.IndexOutOfBounds(MatrixAxis.Col, -1, 2)))

  test("SquareMatrix and Tolerance drive Cholesky validation"):
    val a = DoubleMatrix.fromRows(Vector(Vector(4.0, 2.0), Vector(2.0, 3.0)))
    val b = DoubleMatrix.fromRows(Vector(Vector(6.0), Vector(5.0)))
    val square = value(SquareMatrix.from(a))
    val tolerance = value(Tolerance(1e-12))

    val solved = Cholesky.decompose(square, tolerance).map(_.solve(b))
    assert(solved.isRight)
    assertEqualsDouble(solved.toOption.get(0, 0), 1.0, 1e-10)

    val nonSquare = DoubleMatrix.zeros(2, 3)
    assertEquals(SquareMatrix.from(nonSquare).left.toOption, Some(LinearAlgebraError.NonSquareMatrix(2, 3)))
    assertEquals(
      Cholesky.decompose(a, tol = -1.0).left.toOption,
      Some(LinearAlgebraError.InvalidParameter(NumericParameter.Tolerance, -1.0))
    )

  test("Ridge and Tolerance provide typed GramProjection options"):
    val basis =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 1.0),
          Vector(2.0, 2.0),
          Vector(3.0, 3.0)
        )
      )
    val data =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0),
          Vector(2.0),
          Vector(3.0)
        )
      )

    val ridge = value(Ridge(1e-6))
    val tolerance = value(Tolerance(1e-12))
    val coefficients = value(GramProjection.coefficients(data, basis, ridge, tolerance))

    assertEquals(coefficients.rows, 2)
    assertEquals(coefficients.cols, 1)
    assert(coefficients.toRows.flatten.forall(_.isFinite))
    assertEquals(Ridge(-1.0).left.toOption, Some(LinearAlgebraError.InvalidParameter(NumericParameter.Ridge, -1.0)))

  test("QR decomposition accepts typed pivoting and tolerance"):
    val design =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(1.0, 2.0),
          Vector(1.0, 3.0)
        )
      )
    val data = DoubleMatrix.fromRows(Vector(Vector(0.0), Vector(1.0), Vector(4.0), Vector(9.0)))
    val tolerance = value(Tolerance(1e-7))

    val qr = QrDecomposition.decompose(design, Pivoting.Disabled, tolerance)
    val residualized = MatrixResidualizer.residualize(design, data, tolerance, Pivoting.Disabled)

    assertEquals(qr.shape, design.shape)
    assertEquals(qr.rank, 2)
    assertEquals(residualized.designRank, 2)
    intercept[IllegalArgumentException] {
      QrDecomposition.decompose(design, tol = Double.NaN)
    }
