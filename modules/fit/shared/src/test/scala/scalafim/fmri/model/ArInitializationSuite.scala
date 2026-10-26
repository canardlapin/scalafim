package scalafim.fmri.model

class ArInitializationSuite extends munit.FunSuite:
  test("typed initialization survives legacy and portable JSON round trips"):
    ArInitialization.values.foreach: initialization =>
      val ar = AutocorrelationConfig.withInitialization(initialization, order = 2, voxelwise = true).toOption.get
      assertEquals(AutocorrelationConfig.fromLegacy(ar.toLegacy), Right(ar))
      assertEquals(ar.initialization, initialization)
      val spec = ModelBuildSpec("onset ~ trialwise()", strategy = FitStrategy.GeneralizedLeastSquares(ar))
      val encoded = ModelBuildSpecJsonCodec.encode(spec).toOption.get
      assertEquals(ModelBuildSpecJsonCodec.decode(encoded), Right(spec))
      if initialization == ArInitialization.Stationary then
        assert(encoded.contains("Stationary"))
        assert(ModelBuildSpecJsonCodec.decode(encoded.replace("Stationary", "UnknownInitialization")).isLeft)
        assert(ModelBuildSpecJsonCodec.decode(encoded.replace("\"exactFirst\":true", "\"exactFirst\":false")).isLeft)

  test("inconsistent compatibility flags are refused and stationary identity is distinct"):
    assert(AutocorrelationConfig(exactFirst = false, initialization = Some(ArInitialization.Stationary)).isLeft)
    val stationary = AutocorrelationConfig.withInitialization(ArInitialization.Stationary, order = 2).toOption.get
    val legacy = AutocorrelationConfig.unsafe(order = 2)
    assertNotEquals(stationary, legacy)
    assertEquals(stationary.exactFirst, legacy.exactFirst)
    intercept[IllegalArgumentException](ArOptions(ArStructure.Ar(2), exactFirst = false, initialization = Some(ArInitialization.Stationary)))

  test("tail correction and estimation-only continuity survive complete recipe round trips"):
    val policy = ArBiasCorrection.olsTailAnchored(25).toOption.get
    val ar = AutocorrelationConfig.withInitialization(ArInitialization.Stationary, order = 2, voxelwise = true,
      censoredTimepoints = Vector(3,4), biasCorrection = policy, censorTreatment = ArCensorTreatment.EstimateOnly).toOption.get
    assertEquals(AutocorrelationConfig.fromLegacy(ar.toLegacy), Right(ar))
    val spec = ModelBuildSpec("onset ~ trialwise()", strategy = FitStrategy.GeneralizedLeastSquares(ar))
    val encoded = ModelBuildSpecJsonCodec.encode(spec).toOption.get
    assertEquals(ModelBuildSpecJsonCodec.decode(encoded), Right(spec))
    assert(ArBiasCorrection.olsTailAnchored(0).isLeft)
    assert(AutocorrelationConfig(iterations=2,biasCorrection=policy).isLeft)
    assert(ModelBuildSpecJsonCodec.decode(encoded.replace("EstimateOnly","UnknownTreatment")).isLeft)
    val document = ModelDocument(spec)
    val text = ModelDocumentText.render(document).toOption.get
    assertEquals(ModelDocumentText.parse(text, document), Right(document))
