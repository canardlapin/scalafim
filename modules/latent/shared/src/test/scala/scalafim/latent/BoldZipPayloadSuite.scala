package scalafim.latent

import scalafim.linalg.{DoubleMatrix, DoubleVector}

class BoldZipPayloadSuite extends munit.FunSuite:

  private def value[A](result: Either[LatentError, A]): A =
    result match
      case Right(value) => value
      case Left(error)  => fail(error.message)

  private def payload: BoldZipPayload =
    value(
      BoldZipPayload(
        temporalBasis = DoubleMatrix.eye(4),
        carrierTheta = DoubleMatrix.fromRows(
          Vector(
            Vector(1.0, 2.0, 3.0, 4.0),
            Vector(10.0, 20.0, 30.0, 40.0)
          )
        ),
        carrierLoadings = DoubleMatrix.fromRows(Vector(Vector(2.0, 1.0))),
        spatialBasis = BoldZipSpatialBasis(
          sampleCount = 3,
          phiCoarse = Some(DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(0.0), Vector(1.0)))),
          phiDetail = None,
          label = "identity-detail"
        ),
        texture = Vector(
          BoldZipTextureEntry(atom = 0, carrier = 0, amplitude = 0.5, lag = 0),
          BoldZipTextureEntry(atom = 1, carrier = 1, amplitude = 1.0, lag = 1)
        ),
        events = Vector(BoldZipResidualEvent(atom = 2, time = 2, amplitude = 3.0)),
        offset = Some(DoubleVector.fromSeq(Vector(10.0, 20.0, 30.0))),
        label = "fixture"
      )
    )

  test("BOLDZip payload reconstructs independent fixture as time by samples") {
    val reconstructed = value(payload.reconstruct())

    assertMatrixEquals(
      reconstructed,
      Vector(
        Vector(22.5, 20.0, 42.0),
        Vector(35.0, 30.0, 54.0),
        Vector(47.5, 40.0, 69.0),
        Vector(60.0, 50.0, 78.0)
      ),
      1e-12
    )
    assertEquals(payload.metadata("family"), "boldzip_sr")
    assertEquals(payload.metadata("orientation"), "time_x_samples")
  }

  test("BOLDZip payload supports time and ROI selection without changing order") {
    val selected =
      value(payload.reconstruct(LatentSelection(timepoints = Some(Vector(2, 0)), samples = Some(Vector(2, 0)))))

    assertEquals(selected.toRows, Vector(Vector(69.0, 47.5), Vector(42.0, 22.5)))
  }

  test("BOLDZip carrier decoder returns samples by coefficient columns without events or offsets") {
    val decoded = value(payload.decodeCoefficients(payload.coefTime.transpose))

    assertMatrixEquals(
      decoded,
      Vector(
        Vector(12.5, 25.0, 37.5, 50.0),
        Vector(0.0, 10.0, 20.0, 30.0),
        Vector(12.0, 24.0, 36.0, 48.0)
      ),
      1e-12
    )
  }

  test("BOLDZip payload validates texture and offset invariants") {
    val badTexture =
      BoldZipPayload(
        temporalBasis = DoubleMatrix.eye(2),
        carrierTheta = DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0))),
        carrierLoadings = DoubleMatrix.zeros(0, 1),
        spatialBasis = BoldZipSpatialBasis(sampleCount = 2),
        texture = Vector(BoldZipTextureEntry(atom = 2, carrier = 0, amplitude = 1.0))
      )
    assert(badTexture.swap.toOption.exists(_.message.contains("texture atom")))

    val badOffset =
      BoldZipPayload(
        temporalBasis = DoubleMatrix.eye(2),
        carrierTheta = DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0))),
        carrierLoadings = DoubleMatrix.zeros(0, 1),
        spatialBasis = BoldZipSpatialBasis(sampleCount = 2),
        offset = Some(DoubleVector.fromSeq(Vector(1.0)))
      )
    assert(badOffset.swap.toOption.exists(_.message.contains("offset length")))
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
