package scalafim.fmri.model

class ArBiasCorrectionSuite extends munit.FunSuite:
  test("OLS correction has a checked positive ceiling and survives typed/legacy round trips") {
    assert(ArBiasCorrection.olsDesign(0).isLeft)
    assert(ArBiasCorrection.olsDesign(-1).isLeft)
    val policy = ArBiasCorrection.olsDesign(12).toOption.get
    val typed = AutocorrelationConfig.unsafe(order = 2, global = true, biasCorrection = policy)
    assertEquals(AutocorrelationConfig.fromLegacy(typed.toLegacy).toOption.get, typed)
    assertEquals(typed.toLegacy.biasCorrection, policy)
    assertNotEquals(typed, AutocorrelationConfig.unsafe(order = 2, global = true))
  }

  test("OLS correction refuses non-OLS estimation recipes before execution") {
    assert(AutocorrelationConfig(iterations = 2, biasCorrection = ArBiasCorrection.Ols).isLeft)
    assert(AutocorrelationConfig(coefficients = ArCoefficientSpec.Rho(0.3), biasCorrection = ArBiasCorrection.Ols).isLeft)
    val corrected = AutocorrelationConfig.unsafe(biasCorrection = ArBiasCorrection.Ols)
    assert(RobustAutocorrelation.validateReestimate(corrected).isLeft)
    assert(AutocorrelationConfig(iterations = 3).isRight)
  }

  test("portable GLS recipes retain correction policy and reject invalid serialized ceilings") {
    val spec = ModelBuildSpec("onset ~ trialwise()",
      strategy = FitStrategy.GeneralizedLeastSquares(AutocorrelationConfig.unsafe(biasCorrection = ArBiasCorrection.Ols)))
    val encoded = ModelBuildSpecJsonCodec.encode(spec).toOption.get
    assertEquals(ModelBuildSpecJsonCodec.decode(encoded), Right(spec))
    assert(ModelBuildSpecJsonCodec.decode(encoded.replace("\"ceiling\":25", "\"ceiling\":0")).isLeft)
  }
