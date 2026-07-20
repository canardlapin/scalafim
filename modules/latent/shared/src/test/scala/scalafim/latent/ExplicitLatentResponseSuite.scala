package scalafim.latent

import scalafim.linalg.{DoubleMatrix, DoubleVector}

class ExplicitLatentResponseSuite extends munit.FunSuite:

  private val basis =
    DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(1.0, 1.0)
      )
    )

  private val loadings =
    DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 10.0),
        Vector(2.0, 20.0),
        Vector(3.0, 30.0),
        Vector(4.0, 40.0)
      )
    )

  private val offset =
    DoubleVector.fromSeq(Vector(100.0, 200.0, 300.0, 400.0))

  private def latent: ExplicitLatentResponse =
    ExplicitLatentResponse(basis, loadings, offset = Some(offset))
      .fold(err => fail(err.message), identity)

  test("explicit latent response reconstructs basis times loadings plus offset") {
    val reconstructed =
      latent.reconstruct().fold(err => fail(err.message), identity)

    assertEquals(reconstructed.rows, 3)
    assertEquals(reconstructed.cols, 4)
    val expected =
      Vector(
        Vector(101.0, 202.0, 303.0, 404.0),
        Vector(110.0, 220.0, 330.0, 440.0),
        Vector(111.0, 222.0, 333.0, 444.0)
      )

    reconstructed.toRows.zip(expected).foreach { case (actualRow, expectedRow) =>
      actualRow.zip(expectedRow).foreach { case (actual, expected) =>
        assertEqualsDouble(actual, expected, 1e-12)
      }
    }
  }

  test("selection preserves requested timepoint and sample order") {
    val selected =
      latent
        .reconstruct(LatentSelection(timepoints = Some(Vector(2, 0)), samples = Some(Vector(3, 1))))
        .fold(err => fail(err.message), identity)

    assertEquals(selected.toRows, Vector(Vector(444.0, 222.0), Vector(404.0, 202.0)))
  }

  test("coefficient decoder projects coefficient columns without offset") {
    val gamma =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 2.0),
          Vector(3.0, 4.0)
        )
      )

    val decoded =
      latent.decodeCoefficients(gamma).fold(err => fail(err.message), identity)

    val expected =
      Vector(
        Vector(31.0, 42.0),
        Vector(62.0, 84.0),
        Vector(93.0, 126.0),
        Vector(124.0, 168.0)
    )
    assertEquals(decoded.toRows, expected)
    assert(latent.decodeSemantics.coefficientDecodeIsLinearOnly)
    assert(!latent.decodeSemantics.coefficientDecodeIncludes(LatentMaterializationTerm.SampleOffset))
    assert(latent.decodeSemantics.reconstructionIncludes(LatentMaterializationTerm.SampleOffset))
  }

  test("constructor rejects inconsistent and non-finite explicit factors") {
    val badLoadings = DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0, 3.0)))
    val mismatch = ExplicitLatentResponse(basis, badLoadings)
    assert(mismatch.swap.toOption.exists(_.message.contains("loading columns")))

    val badBasis = DoubleMatrix.fromRows(Vector(Vector(1.0, Double.NaN)))
    val bad = ExplicitLatentResponse(badBasis, DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0))))
    assert(bad.swap.toOption.exists(_.message.contains("basis value")))
  }

  test("constructor stores validated typed annotations") {
    val response =
      ExplicitLatentResponse(
        basis = basis,
        loadings = loadings,
        label = " demo-latent ",
        metadata = Map("subject" -> "sub-01")
      ).fold(err => fail(err.message), identity)

    assertEquals(response.latentLabel.value, "demo-latent")
    assertEquals(response.label, "demo-latent")
    assertEquals(response.typedMetadata.get("subject"), Some("sub-01"))
    assertEquals(response.metadata, Map("subject" -> "sub-01"))
    assert(ExplicitLatentResponse(basis, loadings, metadata = Map(" " -> "bad")).isLeft)
  }

  test("selection validation rejects duplicates and out-of-range indices") {
    val duplicate =
      latent.reconstruct(LatentSelection(timepoints = Some(Vector(0, 0))))
    assert(duplicate.swap.toOption.exists(_.message.contains("duplicate")))

    val outOfRange =
      latent.reconstruct(LatentSelection(samples = Some(Vector(4))))
    assert(outOfRange.swap.toOption.exists(_.message.contains("out of bounds")))
  }

  test("temporal basis encoder uses Gram-solve projection for non-orthonormal bases") {
    val nonOrthonormalBasis =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(0.0, 1.0)
        )
      )
    val data =
      DoubleMatrix.fromRows(
        Vector(
          Vector(2.0, 10.0),
          Vector(6.0, 30.0),
          Vector(4.0, 20.0)
        )
      )

    val encoded =
      TemporalBasisEncoder
        .encodeProvided(data, nonOrthonormalBasis)
        .fold(err => fail(err.message), identity)

    encoded.loadings.toRows
      .zip(Vector(Vector(2.0, 4.0), Vector(10.0, 20.0)))
      .foreach { case (actualRow, expectedRow) =>
        actualRow.zip(expectedRow).foreach { case (actual, expected) =>
          assertEqualsDouble(actual, expected, 1e-12)
        }
      }

    val reconstructed =
      encoded.reconstruct().fold(err => fail(err.message), identity)
    reconstructed.toRows.zip(data.toRows).foreach { case (actualRow, expectedRow) =>
      actualRow.zip(expectedRow).foreach { case (actual, expected) =>
        assertEqualsDouble(actual, expected, 1e-12)
      }
    }
  }

  test("temporal basis encoder stores column means as offsets when centered") {
    val identityBasis =
      DoubleMatrix.eye(3)
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
        .encode(data, identityBasis, center = true)
        .fold(err => fail(err.message), identity)

    assertEquals(encoded.offset.map(_.toVector), Some(Vector(4.0, 8.0)))
    val reconstructed =
      encoded.reconstruct().fold(err => fail(err.message), identity)
    assertEquals(reconstructed.toRows, data.toRows)
  }
