package scalafim.latent

import gale.linalg.{DMat, DVec}

class BoldZipPayloadSuite extends munit.FunSuite:

  private def value[A](result: Either[LatentError, A]): A =
    result match
      case Right(value) => value
      case Left(error)  => fail(error.message)

  private def payload: BoldZipPayload =
    value(
      BoldZipPayload(
        temporalBasis = DMat.eye(4),
        carrierTheta = LatentNumerics.matrixFromRows(
          Vector(
            Vector(1.0, 2.0, 3.0, 4.0),
            Vector(10.0, 20.0, 30.0, 40.0)
          )
        ),
        carrierLoadings = LatentNumerics.matrixFromRows(Vector(Vector(2.0, 1.0))),
        spatialBasis = value(BoldZipSpatialBasis(
          sampleCount = 3,
          coarse = BoldZipCoarseBasis.MatrixBasis(LatentNumerics.matrixFromRows(Vector(Vector(1.0), Vector(0.0), Vector(1.0)))),
          detail = BoldZipDetailBasis.IdentitySamples,
          label = "identity-detail"
        )),
        texture = Vector(
          BoldZipTextureEntry.unsafe(atom = 0, carrier = 0, amplitude = 0.5, lag = 0),
          BoldZipTextureEntry.unsafe(atom = 1, carrier = 1, amplitude = 1.0, lag = 1)
        ),
        events = Vector(BoldZipResidualEvent.unsafe(atom = 2, frame = 2, amplitude = 3.0)),
        offset = Some(DVec.fromSeq(Vector(10.0, 20.0, 30.0))),
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
    assertEquals(payload.typedMetadata.get("family"), Some("boldzip_sr"))
    assertEquals(payload.latentLabel.value, "fixture")
    assert(payload.decodeSemantics.coefficientDecodeIsLinearOnly)
    assert(!payload.decodeSemantics.coefficientDecodeIncludes(LatentMaterializationTerm.SampleOffset))
    assert(!payload.decodeSemantics.coefficientDecodeIncludes(LatentMaterializationTerm.ResidualEvents))
    assert(payload.decodeSemantics.reconstructionIncludes(LatentMaterializationTerm.SampleOffset))
    assert(payload.decodeSemantics.reconstructionIncludes(LatentMaterializationTerm.ResidualEvents))
    assertEquals(payload.texture.head.amplitudeValue, 0.5)
  }

  test("BOLDZip typed amplitude and lag constructors reject invalid values") {
    assertEquals(BoldZipAmplitude(1.25).map(_.value), Right(1.25))
    assert(BoldZipAmplitude(Double.NaN).isLeft)
    assert(BoldZipLag(Int.MinValue).isLeft)
    assert(BoldZipTextureEntry.checked(atom = 0, carrier = 0, amplitude = Double.NaN).isLeft)
    assert(BoldZipResidualEvent.checked(atom = 0, frame = 0, amplitude = Double.PositiveInfinity).isLeft)
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
        temporalBasis = DMat.eye(2),
        carrierTheta = LatentNumerics.matrixFromRows(Vector(Vector(1.0, 2.0))),
        carrierLoadings = DMat.zeros(0, 1),
        spatialBasis = value(BoldZipSpatialBasis(sampleCount = 2)),
        texture = Vector(BoldZipTextureEntry.unsafe(atom = 2, carrier = 0, amplitude = 1.0))
      )
    assert(badTexture.swap.toOption.exists(_.message.contains("texture atom")))

    val badOffset =
      BoldZipPayload(
        temporalBasis = DMat.eye(2),
        carrierTheta = LatentNumerics.matrixFromRows(Vector(Vector(1.0, 2.0))),
        carrierLoadings = DMat.zeros(0, 1),
        spatialBasis = value(BoldZipSpatialBasis(sampleCount = 2)),
        offset = Some(DVec.fromSeq(Vector(1.0)))
      )
    assert(badOffset.swap.toOption.exists(_.message.contains("offset length")))

    val badMetadata =
      BoldZipPayload(
        temporalBasis = DMat.eye(2),
        carrierTheta = LatentNumerics.matrixFromRows(Vector(Vector(1.0, 2.0))),
        carrierLoadings = DMat.zeros(0, 1),
        spatialBasis = value(BoldZipSpatialBasis(sampleCount = 2)),
        metadata = Map("" -> "bad")
      )
    assert(badMetadata.isLeft)
  }

  test("BOLDZip spatial basis variants make absent coarse and identity detail explicit") {
    val spatial =
      value(BoldZipSpatialBasis(sampleCount = 4))

    assertEquals(spatial.coarse, BoldZipCoarseBasis.Absent)
    assertEquals(spatial.detail, BoldZipDetailBasis.IdentitySamples)
    assertEquals(spatial.coarseAtoms, 0)
    assertEquals(spatial.detailAtoms, 4)
    val negative =
      BoldZipTextureEntry.checked(atom = -1, carrier = 0, amplitude = 1.0).left.toOption.map(_.payload)
    assertEquals(negative, Some(LatentError.Payload.Index("BOLDZip atom", -1, None)))
  }

  private def assertMatrixEquals(
      actual: DMat,
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
