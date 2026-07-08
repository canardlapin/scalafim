package scalafim.latent

import scalafim.linalg.DoubleMatrix

class HaarBasisSuite extends munit.FunSuite:

  test("Haar basis uses scaling then coarse-to-fine orthonormal wavelets") {
    val basis =
      HaarBasis.full(timepoints = 4).fold(err => fail(err.message), identity)
    val invSqrt2 = 1.0 / math.sqrt(2.0)

    assertMatrixEquals(
      basis,
      Vector(
        Vector(0.5, 0.5, invSqrt2, 0.0),
        Vector(0.5, 0.5, -invSqrt2, 0.0),
        Vector(0.5, -0.5, 0.0, invSqrt2),
        Vector(0.5, -0.5, 0.0, -invSqrt2)
      ),
      1e-12
    )
  }

  test("Haar basis columns have identity Gram matrix for full and truncated bases") {
    assertIdentityGram(HaarBasis.full(timepoints = 8).fold(err => fail(err.message), identity), 1e-12)
    assertIdentityGram(HaarBasis.build(timepoints = 8, components = 6).fold(err => fail(err.message), identity), 1e-12)
  }

  test("full-rank Haar encoder roundtrips dense time by sample data") {
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
        .encodeFullHaar(data)
        .fold(err => fail(err.message), identity)
    val reconstructed = encoded.reconstruct().fold(err => fail(err.message), identity)

    assertEquals(encoded.metadata("family"), "time_haar")
    assertEquals(encoded.metadata("basis"), "haar")
    assertEquals(encoded.metadata("components"), "4")
    assertEquals(encoded.metadata("levels"), "2")
    assertMatrixEquals(reconstructed, data.toRows, 1e-12)
  }

  test("truncated Haar encoder exactly reconstructs data in the retained span") {
    val basis =
      HaarBasis.build(timepoints = 4, components = 2).fold(err => fail(err.message), identity)
    val expectedLoadings =
      Vector(
        Vector(4.0, -2.0),
        Vector(-1.0, 3.0),
        Vector(0.5, 0.25)
      )
    val data = temporalDataFrom(basis, expectedLoadings)

    val encoded =
      TemporalBasisEncoder
        .encodeHaar(data, components = 2)
        .fold(err => fail(err.message), identity)
    val reconstructed = encoded.reconstruct().fold(err => fail(err.message), identity)

    assertRowsEqual(encoded.loadings.toRows, expectedLoadings, 1e-12)
    assertMatrixEquals(reconstructed, data.toRows, 1e-12)
  }

  test("centered Haar encoder stores column offsets") {
    val data =
      DoubleMatrix.fromRows(
        Vector(
          Vector(2.0, 4.0),
          Vector(4.0, 8.0),
          Vector(6.0, 12.0),
          Vector(8.0, 16.0)
        )
      )

    val encoded =
      TemporalBasisEncoder
        .encodeFullHaar(data, center = true)
        .fold(err => fail(err.message), identity)
    val reconstructed = encoded.reconstruct().fold(err => fail(err.message), identity)

    assertEquals(encoded.offset.map(_.toVector), Some(Vector(5.0, 10.0)))
    assertMatrixEquals(reconstructed, data.toRows, 1e-12)
  }

  test("HaarSpec validates power-of-two timepoints and component counts") {
    val spec =
      HaarSpec(timepoints = 8, components = 6).fold(err => fail(err.message), identity)
    assertEquals(spec.timepoints, 8)
    assertEquals(spec.components, 6)
    assertEquals(spec.levels, 3)

    val nonPowerOfTwo = HaarSpec(timepoints = 6, components = 2)
    assert(nonPowerOfTwo.left.toOption.exists {
      case LatentError.NonPowerOfTwoDimension("Haar timepoints", 6) => true
      case _ => false
    })

    val zeroComponents = HaarSpec(timepoints = 4, components = 0)
    assert(zeroComponents.left.toOption.exists(_.message.contains("positive")))

    val tooManyComponents = HaarSpec(timepoints = 4, components = 5)
    assert(tooManyComponents.left.toOption.exists(_.message.contains("Haar components")))
  }

  private def assertIdentityGram(
      basis: DoubleMatrix,
      tol: Double
  ): Unit =
    val gram = DoubleMatrix.crossProduct(basis)
    var row = 0
    while row < gram.rows do
      var col = 0
      while col < gram.cols do
        val expected = if row == col then 1.0 else 0.0
        assertEqualsDouble(gram(row, col), expected, tol)
        col += 1
      row += 1

  private def temporalDataFrom(
      basis: DoubleMatrix,
      loadings: Vector[Vector[Double]]
  ): DoubleMatrix =
    DoubleMatrix.fromRows(
      Vector.tabulate(basis.rows) { time =>
        Vector.tabulate(loadings.length) { sample =>
          var sum = 0.0
          var component = 0
          while component < basis.cols do
            sum += basis(time, component) * loadings(sample)(component)
            component += 1
          sum
        }
      }
    )

  private def assertMatrixEquals(
      actual: DoubleMatrix,
      expected: Vector[Vector[Double]],
      tol: Double
  ): Unit =
    assertRowsEqual(actual.toRows, expected, tol)

  private def assertRowsEqual(
      actual: Vector[Vector[Double]],
      expected: Vector[Vector[Double]],
      tol: Double
  ): Unit =
    assertEquals(actual.length, expected.length)
    assertEquals(if actual.isEmpty then 0 else actual.head.length, if expected.isEmpty then 0 else expected.head.length)
    actual.zip(expected).foreach { case (actualRow, expectedRow) =>
      actualRow.zip(expectedRow).foreach { case (actualValue, expectedValue) =>
        assertEqualsDouble(actualValue, expectedValue, tol)
      }
    }
