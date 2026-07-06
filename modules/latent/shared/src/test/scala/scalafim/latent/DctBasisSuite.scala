package scalafim.latent

import scalafim.linalg.DoubleMatrix

class DctBasisSuite extends munit.FunSuite:

  test("DCT basis matches fmrilatent R fixture values") {
    val ortho =
      DctBasis.build(timepoints = 5, components = 4, norm = DctNorm.Ortho)
        .fold(err => fail(err.message), identity)
    val raw =
      DctBasis.build(timepoints = 5, components = 4, norm = DctNorm.None)
        .fold(err => fail(err.message), identity)

    assertMatrixEquals(
      ortho,
      Vector(
        Vector(0.447213595499958, 0.601500955007546, 0.511667273601693, 0.371748034460185),
        Vector(0.447213595499958, 0.371748034460185, -0.195439507584855, -0.601500955007546),
        Vector(0.447213595499958, 0.0, -0.632455532033676, 0.0),
        Vector(0.447213595499958, -0.371748034460184, -0.195439507584855, 0.601500955007546),
        Vector(0.447213595499958, -0.601500955007546, 0.511667273601693, -0.371748034460184)
      ),
      1e-12
    )
    assertMatrixEquals(
      raw,
      Vector(
        Vector(1.0, 0.951056516295154, 0.809016994374947, 0.587785252292473),
        Vector(1.0, 0.587785252292473, -0.309016994374947, -0.951056516295154),
        Vector(1.0, 0.0, -1.0, 0.0),
        Vector(1.0, -0.587785252292473, -0.309016994374948, 0.951056516295154),
        Vector(1.0, -0.951056516295154, 0.809016994374947, -0.587785252292473)
      ),
      1e-12
    )
  }

  test("orthonormal DCT columns have identity Gram matrix") {
    val basis =
      DctBasis.build(timepoints = 8, components = 5, norm = DctNorm.Ortho)
        .fold(err => fail(err.message), identity)
    val gram = DoubleMatrix.crossProduct(basis)

    var row = 0
    while row < gram.rows do
      var col = 0
      while col < gram.cols do
        val expected = if row == col then 1.0 else 0.0
        assertEqualsDouble(gram(row, col), expected, 1e-12)
        col += 1
      row += 1
  }

  test("full-rank DCT encoder roundtrips dense time by sample data") {
    val data =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 2.0, 3.0),
          Vector(2.0, 3.0, 5.0),
          Vector(3.0, 5.0, 8.0),
          Vector(5.0, 8.0, 13.0)
        )
      )

    val encoded =
      TemporalBasisEncoder
        .encodeDct(data, components = data.rows, norm = DctNorm.Ortho)
        .fold(err => fail(err.message), identity)
    val reconstructed = encoded.reconstruct().fold(err => fail(err.message), identity)

    assertEquals(encoded.metadata("family"), "time_dct")
    assertEquals(encoded.metadata("basis"), "dct")
    assertEquals(encoded.metadata("components"), "4")
    assertEquals(encoded.metadata("norm"), "ortho")
    assertMatrixEquals(reconstructed, data.toRows, 1e-12)
  }

  test("raw DCT encoder uses Gram projection and roundtrips at full rank") {
    val data =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, -1.0),
          Vector(0.0, 2.0),
          Vector(3.0, 4.0),
          Vector(8.0, 5.0),
          Vector(13.0, 9.0)
        )
      )

    val encoded =
      TemporalBasisEncoder
        .encodeDct(data, components = data.rows, norm = DctNorm.None)
        .fold(err => fail(err.message), identity)
    val reconstructed = encoded.reconstruct().fold(err => fail(err.message), identity)

    assertEquals(encoded.metadata("norm"), "none")
    assertMatrixEquals(reconstructed, data.toRows, 1e-10)
  }

  test("centered DCT encoder stores column offsets") {
    val data =
      DoubleMatrix.fromRows(
        Vector(
          Vector(2.0, 4.0),
          Vector(4.0, 8.0),
          Vector(6.0, 12.0)
        )
      )

    val encoded =
      TemporalBasisEncoder
        .encodeFullDct(data, center = true)
        .fold(err => fail(err.message), identity)
    val reconstructed = encoded.reconstruct().fold(err => fail(err.message), identity)

    assertEquals(encoded.offset.map(_.toVector), Some(Vector(4.0, 8.0)))
    assertMatrixEquals(reconstructed, data.toRows, 1e-12)
  }

  test("DCT basis rejects invalid component counts") {
    val tooMany = DctBasis.build(timepoints = 4, components = 5)
    assert(tooMany.swap.toOption.exists(_.message.contains("DCT components")))

    val zero = DctBasis.build(timepoints = 4, components = 0)
    assert(zero.swap.toOption.exists(_.message.contains("positive")))
  }

  private def assertMatrixEquals(
      actual: DoubleMatrix,
      expected: Vector[Vector[Double]],
      tol: Double
  ): Unit =
    assertEquals(actual.rows, expected.length)
    assertEquals(actual.cols, if expected.isEmpty then 0 else expected.head.length)
    actual.toRows.zip(expected).foreach { case (actualRow, expectedRow) =>
      actualRow.zip(expectedRow).foreach { case (actualValue, expectedValue) =>
        assertEqualsDouble(actualValue, expectedValue, tol)
      }
    }
