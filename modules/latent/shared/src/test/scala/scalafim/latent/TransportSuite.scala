package scalafim.latent

import gale.linalg.{DMat, DVec, LinAlgError}

class TransportSuite extends munit.FunSuite:

  private def mapValue[A](result: Either[LinAlgError, A]): A =
    result match
      case Right(value) => value
      case Left(error)  => fail(error.message)

  private def latentValue[A](result: Either[LatentError, A]): A =
    result match
      case Right(value) => value
      case Left(error)  => fail(error.message)

  test("identity transport response reconstructs selected timepoints and samples") {
    val decoder = mapValue(LatentOperators.identity(2))
    val response =
      latentValue(
        TransportLatentResponse.withIdentityTransform(
          coefficientsAnalysis = LatentNumerics.matrixFromRows(
            Vector(
              Vector(1.0, 2.0),
              Vector(3.0, 4.0)
            )
          ),
          nativeDecoder = decoder,
          offset = Some(DVec.fromSeq(Vector(10.0, 20.0)))
        )
      )

    val reconstructed =
      latentValue(response.reconstruct(LatentSelection(timepoints = Some(Vector(1, 0)), samples = Some(Vector(1, 0)))))

    assertEquals(reconstructed.toRows, Vector(Vector(24.0, 13.0), Vector(22.0, 11.0)))
    assertEquals(response.metadata("family"), "transport")
    assertEquals(response.typedMetadata.get("family"), Some("transport"))
    assertEquals(response.decoders, TransportDecoders.NativeOnly(decoder))
    assertEquals(response.adjointConvention, TransportAdjointConvention.EuclideanDiscrete)
    assert(response.decodeSemantics.coefficientDecodeIsLinearOnly)
    assert(!response.decodeSemantics.coefficientDecodeIncludes(LatentMaterializationTerm.SampleOffset))
    assert(response.decodeSemantics.reconstructionIncludes(LatentMaterializationTerm.SampleOffset))

    val invalidMetadata =
      TransportLatentResponse.withIdentityTransform(
        coefficientsAnalysis = LatentNumerics.matrixFromRows(Vector(Vector(1.0, 2.0))),
        nativeDecoder = decoder,
        metadata = Map(" " -> "bad")
      )
    assert(invalidMetadata.isLeft)
  }

  test("rectangular transport decoder supports analysis and raw coefficient handoff") {
    val decoder =
      mapValue(
        LatentOperators.csrFromTriplets(
          rows = 4,
          cols = 2,
          rowIndices = Array(0, 1, 2, 2, 3, 3),
          colIndices = Array(0, 1, 0, 1, 0, 1),
          values = Array(1.0, 1.0, 1.0, 1.0, 2.0, -1.0)
        )
      )
    val toAnalysis =
      mapValue(
        LatentOperators.csrFromTriplets(
          rows = 2,
          cols = 2,
          rowIndices = Array(0, 1),
          colIndices = Array(0, 1),
          values = Array(2.0, 0.5)
        )
      )
    val toRaw =
      mapValue(
        LatentOperators.csrFromTriplets(
          rows = 2,
          cols = 2,
          rowIndices = Array(0, 1),
          colIndices = Array(0, 1),
          values = Array(0.5, 2.0)
        )
      )
    val transform = latentValue(CoefficientTransform(toAnalysis, toRaw))
    val response =
      latentValue(
        TransportLatentResponse(
          coefficientsAnalysis = LatentNumerics.matrixFromRows(Vector(Vector(2.0, 2.0))),
          nativeDecoder = decoder,
          transform = transform
        )
      )

    val analysisGamma = LatentNumerics.matrixFromRows(Vector(Vector(2.0), Vector(2.0)))
    val rawGamma = LatentNumerics.matrixFromRows(Vector(Vector(1.0), Vector(4.0)))
    val analysisProjection =
      latentValue(response.decodeCoefficientBlock(CoefficientBlock.Analysis(analysisGamma), TransportSpace.Native))
    val rawProjection =
      latentValue(response.decodeCoefficientBlock(CoefficientBlock.Raw(rawGamma), TransportSpace.Native))
    val rawMetric = latentValue(transform.rawMetric)

    assertEquals(analysisProjection.toRows, Vector(Vector(2.0), Vector(2.0), Vector(4.0), Vector(2.0)))
    assertEquals(rawProjection.toRows, analysisProjection.toRows)
    assertEquals(rawMetric.toRows, Vector(Vector(4.0, 0.0), Vector(0.0, 0.25)))
  }

  test("transport covariance diagonal uses raw-coordinate Euclidean decoder") {
    val decoder =
      mapValue(
        LatentOperators.csrFromTriplets(
          rows = 4,
          cols = 2,
          rowIndices = Array(0, 1, 2, 2, 3, 3),
          colIndices = Array(0, 1, 0, 1, 0, 1),
          values = Array(1.0, 1.0, 1.0, 1.0, 2.0, -1.0)
        )
      )
    val toAnalysis =
      mapValue(
        LatentOperators.csrFromTriplets(
          rows = 2,
          cols = 2,
          rowIndices = Array(0, 1),
          colIndices = Array(0, 1),
          values = Array(2.0, 0.5)
        )
      )
    val toRaw =
      mapValue(
        LatentOperators.csrFromTriplets(
          rows = 2,
          cols = 2,
          rowIndices = Array(0, 1),
          colIndices = Array(0, 1),
          values = Array(0.5, 2.0)
        )
      )
    val response =
      latentValue(
        TransportLatentResponse(
          coefficientsAnalysis = LatentNumerics.matrixFromRows(Vector(Vector(2.0, 2.0))),
          nativeDecoder = decoder,
          transform = latentValue(CoefficientTransform(toAnalysis, toRaw))
        )
      )

    val sigmaRaw =
      LatentNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.0, 4.0)
        )
      )
    val diag =
      latentValue(response.covarianceDiagonal(CoefficientCovariance.Raw(sigmaRaw), TransportSpace.Native))

    diag.toVector.zip(Vector(4.0, 1.0, 5.0, 17.0)).foreach { case (actual, expected) =>
      assertEqualsDouble(actual, expected, 1e-12)
    }
  }

  test("template decoder absence is represented as a typed latent error") {
    val decoder = mapValue(LatentOperators.identity(2))
    val response =
      latentValue(
        TransportLatentResponse.withIdentityTransform(
          coefficientsAnalysis = LatentNumerics.matrixFromRows(Vector(Vector(1.0, 2.0))),
          nativeDecoder = decoder
        )
      )

    assertEquals(
      response.decoder(TransportSpace.Template).left.toOption,
      Some(LatentError.MissingComponent("template decoder"))
    )
  }

  test("template-capable transport response exposes decoder capability as an ADT") {
    val native = mapValue(LatentOperators.identity(2))
    val template =
      mapValue(
        LatentOperators.csrFromTriplets(
          rows = 3,
          cols = 2,
          rowIndices = Array(0, 1, 2),
          colIndices = Array(0, 1, 1),
          values = Array(1.0, 1.0, 2.0)
        )
      )
    val response =
      latentValue(
        TransportLatentResponse.fromDecoders(
          coefficientsAnalysis = LatentNumerics.matrixFromRows(Vector(Vector(1.0, 2.0))),
          decoders = TransportDecoders.TemplateCapable(native, template),
          transform = latentValue(CoefficientTransform.identity(2))
        )
      )

    assertEquals(response.decoders.templateCapable, true)
    assert(response.templateDecoder.nonEmpty)
    assertEquals(response.metadata("transport_decoders"), "template_capable")
  }

  test("transport projection solves closed-form ridge and roughness fixture") {
    val decoder =
      mapValue(
        LatentOperators.csrFromTriplets(
          rows = 3,
          cols = 2,
          rowIndices = Array(0, 1, 2, 2),
          colIndices = Array(0, 1, 0, 1),
          values = Array(1.0, 1.0, 1.0, 1.0)
        )
      )
    val target =
      LatentNumerics.matrixFromRows(
        Vector(
          Vector(1.0),
          Vector(2.0),
          Vector(4.0)
        )
      )
    val roughness =
      LatentNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.0, 2.0)
        )
      )

    val coefficients =
      latentValue(
        TransportProjection.coefficientsWithPenalty(
          targetData = target,
          decoder = decoder,
          ridge = RidgePenalty.unsafe(0.5),
          roughness = Some(roughness)
        )
      )

    assertEquals(coefficients.rows, 2)
    assertEquals(coefficients.cols, 1)
    assertEqualsDouble(coefficients(0, 0), 16.5 / 14.75, 1e-12)
    assertEqualsDouble(coefficients(1, 0), 16.0 / 14.75, 1e-12)
  }
