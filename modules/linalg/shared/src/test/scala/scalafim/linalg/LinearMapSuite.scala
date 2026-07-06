package scalafim.linalg

class LinearMapSuite extends munit.FunSuite:

  private def value[A](result: Either[LinearMapError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def dot(left: DoubleMatrix, right: DoubleMatrix): Double =
    assertEquals(left.rows, right.rows)
    assertEquals(left.cols, right.cols)
    var sum = 0.0
    var i = 0
    val leftData = left.copyData
    val rightData = right.copyData
    while i < leftData.length do
      sum += leftData(i) * rightData(i)
      i += 1
    sum

  test("SparseTriplets validates dimensions and zero-based bounds"):
    val badLength =
      SparseTriplets(
        rows = 2,
        cols = 2,
        rowIndices = Array(0, 1),
        colIndices = Array(0),
        values = Array(1.0, 2.0)
      )
    assertEquals(badLength.left.toOption, Some(LinearMapError.MismatchedTripletLengths(2, 1, 2)))

    val badIndex =
      SparseTriplets(
        rows = 2,
        cols = 2,
        rowIndices = Array(2),
        colIndices = Array(0),
        values = Array(1.0)
      )
    assertEquals(badIndex.left.toOption, Some(LinearMapError.IndexOutOfBounds("row", 2, 2)))

  test("CsrMatrix applies sparse rows to dense matrices and vectors"):
    val map =
      value(
        CsrMatrix.fromTriplets(
          rows = 3,
          cols = 4,
          rowIndices = Array(0, 0, 1, 2),
          colIndices = Array(0, 2, 1, 3),
          values = Array(2.0, 1.0, -1.0, 0.5)
        )
      )

    val input =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 10.0),
          Vector(2.0, 20.0),
          Vector(3.0, 30.0),
          Vector(4.0, 40.0)
        )
      )
    val out = value(map.forward(input))
    assertEquals(out.rows, 3)
    assertEquals(out.cols, 2)
    assertEqualsDouble(out(0, 0), 5.0, 1e-12)
    assertEqualsDouble(out(0, 1), 50.0, 1e-12)
    assertEqualsDouble(out(1, 0), -2.0, 1e-12)
    assertEqualsDouble(out(1, 1), -20.0, 1e-12)
    assertEqualsDouble(out(2, 0), 2.0, 1e-12)
    assertEqualsDouble(out(2, 1), 20.0, 1e-12)

    val vector = value(map.forward(DoubleVector.fromSeq(Vector(1.0, 2.0, 3.0, 4.0))))
    assertEquals(vector.toVector, Vector(5.0, -2.0, 2.0))

  test("CsrMatrix canonicalizes duplicate triplets and drops zero entries"):
    val map =
      value(
        CsrMatrix.fromTriplets(
          rows = 2,
          cols = 2,
          rowIndices = Array(0, 0, 0, 1),
          colIndices = Array(0, 0, 1, 1),
          values = Array(1.0, 2.0, 0.0, 4.0)
        )
      )
    val triplets = map.toTriplets
    assertEquals(triplets.nnz, 2)
    assertEquals(triplets.rowIndices.toVector, Vector(0, 1))
    assertEquals(triplets.colIndices.toVector, Vector(0, 1))
    assertEquals(triplets.values.toVector, Vector(3.0, 4.0))

  test("adjoint satisfies the matrix inner-product identity"):
    val map =
      value(
        CsrMatrix.fromTriplets(
          rows = 3,
          cols = 4,
          rowIndices = Array(0, 0, 1, 2, 2),
          colIndices = Array(0, 2, 1, 2, 3),
          values = Array(2.0, 1.0, -1.0, 0.25, 0.5)
        )
      )
    val x =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, -1.0),
          Vector(2.0, -2.0),
          Vector(3.0, -3.0),
          Vector(4.0, -4.0)
        )
      )
    val y =
      DoubleMatrix.fromRows(
        Vector(
          Vector(0.5, 2.0),
          Vector(1.5, -1.0),
          Vector(-2.0, 0.25)
        )
      )

    val px = value(map.forward(x))
    val pty = value(map.adjoint.forward(y))
    assertEqualsDouble(dot(px, y), dot(x, pty), 1e-12)

  test("compose applies operators left-to-right in data flow"):
    val first =
      value(
        CsrMatrix.fromTriplets(
          rows = 2,
          cols = 3,
          rowIndices = Array(0, 0, 1),
          colIndices = Array(0, 1, 2),
          values = Array(1.0, 2.0, 3.0)
        )
      )
    val second =
      value(
        CsrMatrix.fromTriplets(
          rows = 2,
          cols = 2,
          rowIndices = Array(0, 1, 1),
          colIndices = Array(0, 0, 1),
          values = Array(4.0, 1.0, -1.0)
        )
      )
    val composed = value(LinearMap.compose(first, second))
    val out = value(composed.forward(DoubleVector.fromSeq(Vector(1.0, 2.0, 3.0))))
    assertEquals(out.toVector, Vector(20.0, -4.0))

    val bad = LinearMap.compose(second, first)
    assertEquals(bad.left.toOption, Some(LinearMapError.NonComposable(2, 3)))

  test("restrict preserves selected target row and source column order"):
    val base =
      value(
        CsrMatrix.fromTriplets(
          rows = 3,
          cols = 4,
          rowIndices = Array(0, 0, 1, 2),
          colIndices = Array(0, 2, 1, 3),
          values = Array(2.0, 1.0, -1.0, 0.5)
        )
      )
    val restricted =
      value(
        LinearMap.restrict(
          base,
          targetRows = Some(Vector(2, 0)),
          sourceCols = Some(Vector(3, 0, 2))
        )
      )

    assertEquals(restricted.rows, 2)
    assertEquals(restricted.cols, 3)
    val out = value(restricted.forward(DoubleVector.fromSeq(Vector(8.0, 5.0, 7.0))))
    assertEquals(out.toVector, Vector(4.0, 17.0))

    val duplicate = LinearMap.restrict(base, targetRows = Some(Vector(1, 1)))
    assertEquals(duplicate.left.toOption, Some(LinearMapError.DuplicateSelection("target", 1)))

  test("blockDiag applies independent maps along source and target offsets"):
    val left =
      value(
        CsrMatrix.fromTriplets(
          rows = 1,
          cols = 2,
          rowIndices = Array(0),
          colIndices = Array(1),
          values = Array(2.0)
        )
      )
    val right =
      value(
        CsrMatrix.fromTriplets(
          rows = 2,
          cols = 1,
          rowIndices = Array(0, 1),
          colIndices = Array(0, 0),
          values = Array(3.0, 4.0)
        )
      )
    val block = value(LinearMap.blockDiag(Vector(left, right)))
    val out = value(block.forward(DoubleVector.fromSeq(Vector(5.0, 7.0, 11.0))))
    assertEquals(out.toVector, Vector(14.0, 33.0, 44.0))

    val adjointOut = value(block.adjoint.forward(DoubleVector.fromSeq(Vector(2.0, 3.0, 5.0))))
    assertEquals(adjointOut.toVector, Vector(0.0, 4.0, 29.0))
