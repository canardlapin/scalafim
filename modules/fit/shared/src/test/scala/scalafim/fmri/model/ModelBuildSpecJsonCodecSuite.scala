package scalafim.fmri.model

import scalafim.fmri.design.*
import scalafim.fmri.design.baseline.*
import scalafim.fmri.design.contrast.*
import scalafim.fmri.hrf.*

class ModelBuildSpecJsonCodecSuite extends munit.FunSuite:
  test("portable build fields round trip without omission"):
    val orth = ModulatorOrthogonalization.ordered("slopes", "gain", "loss").toOption.get
    val spec = ModelBuildSpec(
      formula = "onset ~ hrf(cond, modulators(gain, loss), id = slopes)",
      blockColumn = Some("run"), durationColumn = Some("duration"),
      baselineBasis = BaselineBasis.Dct(DctCutoffPeriod.unsafeSeconds(128)),
      baselineIntercept = Intercept.Runwise, strategy = FitStrategy.RunwiseLeastSquares(),
      factorLevels = FactorLevelRegistry.of("cond" -> Seq("A", "B")).toOption.get,
      emptyCellPolicy = EmptyCellPolicy.RetainZero, missingValuePolicy = MissingValuePolicy.Reject,
      orthogonalization = ModulatorOrthogonalizationPlan.one(orth),
      contrastSets = Map("effects" -> ContrastSpec.ContrastSet(Vector(ContrastSpec.Typed(ContrastExpr.UnitContrast(ContrastId.unsafe("unit"))))))
    )
    val json = ModelBuildSpecJsonCodec.encode(spec).toOption.get
    assertEquals(ModelBuildSpecJsonCodec.decode(json), Right(spec))
    assertEquals(ModelBuildSpecJsonCodec.encode(ModelBuildSpecJsonCodec.decode(json).toOption.get), Right(json))

  test("runtime-only fields and unknown JSON fields fail explicitly"):
    val spec = ModelBuildSpec("onset ~ trialwise()")
    assert(ModelBuildSpecJsonCodec.encode(spec.copy(defaultHrf = Hrfs.fir(4))).isLeft)
    assert(ModelBuildSpecJsonCodec.encode(spec.copy(strategy = FitStrategy.RobustLeastSquares(RobustConfig.unsafe(RobustOptions(psi = RobustPsi.Huber()))))).isLeft)
    assert(ModelBuildSpecJsonCodec.encode(spec.copy(strategy = FitStrategy.GeneralizedLeastSquares(controls = FitControls(missingData = MissingDataPolicy.ExcludeVoxel)))).isLeft)
    val json = ModelBuildSpecJsonCodec.encode(spec).toOption.get
    assert(ModelBuildSpecJsonCodec.decode(json.dropRight(1) + ",\"unknown\":1}").isLeft)
    assert(ModelBuildSpecJsonCodec.decode(json.replace("\"version\":1", "\"version\":99")).isLeft)

  test("portable AR(1) and AR(2) GLS strategies preserve every declared parameter"):
    val ar1 = AutocorrelationConfig.unsafe(
      order = 1, iterations = 0, global = true, exactFirst = false,
      censoredTimepoints = Vector(3, 8), coefficients = ArCoefficientSpec.Rho(0.35)
    )
    val ar2 = AutocorrelationConfig.unsafe(
      order = 2, iterations = 1, voxelwise = false, exactFirst = false,
      censoredTimepoints = Vector(1), coefficients = ArCoefficientSpec.Phi(Vector(0.4, -0.15))
    )
    val estimated = AutocorrelationConfig.unsafe(order = 2, iterations = 2, voxelwise = true)
    val strategies = Vector(FitStrategy.GeneralizedLeastSquares(ar1), FitStrategy.RunwiseGeneralizedLeastSquares(ar2), FitStrategy.RunwiseGeneralizedLeastSquares(estimated))
    val goldens = Vector(
      "{\"schema\":\"scalafim.model-build\",\"version\":1,\"formula\":\"onset ~ trialwise()\",\"blockColumn\":null,\"durationColumn\":null,\"baseline\":{\"kind\":\"constant\"},\"baselineDegree\":1,\"intercept\":\"Global\",\"strategy\":{\"kind\":\"gls\",\"ar\":{\"order\":1,\"iterations\":0,\"global\":true,\"voxelwise\":false,\"exactFirst\":false,\"censoredTimepoints\":[3,8],\"coefficients\":{\"kind\":\"rho\",\"value\":0.35}}},\"defaultHrf\":\"SPMG1\",\"precision\":0.3,\"dropEmpty\":true,\"summate\":true,\"strict\":false,\"contrastSets\":{},\"nuisance\":null,\"factorLevels\":[],\"emptyCellPolicy\":\"UseDropEmptyFlag\",\"missingValuePolicy\":{\"kind\":\"zero-contribution\"},\"degenerateModulatorPolicy\":\"RetainAndReport\",\"orthogonalization\":[]}",
      "{\"schema\":\"scalafim.model-build\",\"version\":1,\"formula\":\"onset ~ trialwise()\",\"blockColumn\":null,\"durationColumn\":null,\"baseline\":{\"kind\":\"constant\"},\"baselineDegree\":1,\"intercept\":\"Global\",\"strategy\":{\"kind\":\"runwise-gls\",\"ar\":{\"order\":2,\"iterations\":1,\"global\":false,\"voxelwise\":false,\"exactFirst\":false,\"censoredTimepoints\":[1],\"coefficients\":{\"kind\":\"phi\",\"values\":[0.4,-0.15]}}},\"defaultHrf\":\"SPMG1\",\"precision\":0.3,\"dropEmpty\":true,\"summate\":true,\"strict\":false,\"contrastSets\":{},\"nuisance\":null,\"factorLevels\":[],\"emptyCellPolicy\":\"UseDropEmptyFlag\",\"missingValuePolicy\":{\"kind\":\"zero-contribution\"},\"degenerateModulatorPolicy\":\"RetainAndReport\",\"orthogonalization\":[]}",
      "{\"schema\":\"scalafim.model-build\",\"version\":1,\"formula\":\"onset ~ trialwise()\",\"blockColumn\":null,\"durationColumn\":null,\"baseline\":{\"kind\":\"constant\"},\"baselineDegree\":1,\"intercept\":\"Global\",\"strategy\":{\"kind\":\"runwise-gls\",\"ar\":{\"order\":2,\"iterations\":2,\"global\":false,\"voxelwise\":true,\"exactFirst\":true,\"censoredTimepoints\":[],\"coefficients\":{\"kind\":\"estimate\"}}},\"defaultHrf\":\"SPMG1\",\"precision\":0.3,\"dropEmpty\":true,\"summate\":true,\"strict\":false,\"contrastSets\":{},\"nuisance\":null,\"factorLevels\":[],\"emptyCellPolicy\":\"UseDropEmptyFlag\",\"missingValuePolicy\":{\"kind\":\"zero-contribution\"},\"degenerateModulatorPolicy\":\"RetainAndReport\",\"orthogonalization\":[]}"
    )
    strategies.zip(goldens).foreach: (strategy, golden) =>
      val spec = ModelBuildSpec("onset ~ trialwise()", strategy = strategy)
      val json = ModelBuildSpecJsonCodec.encode(spec).toOption.getOrElse(fail("portable GLS encoding"))
      assertEquals(json, golden)
      assertEquals(ModelBuildSpecJsonCodec.decode(json), Right(spec))
      assertEquals(ModelBuildSpecJsonCodec.encode(ModelBuildSpecJsonCodec.decode(json).toOption.getOrElse(fail("portable GLS decoding"))), Right(json))

  test("portable LSA formula with OLS and typed LSS configuration round trip"):
    val lsa = ModelBuildSpec("onset ~ trialwise()", strategy = FitStrategy.OrdinaryLeastSquares())
    val lss = ModelBuildSpec(
      "onset ~ trialwise()",
      strategy = FitStrategy.LeastSquaresSeparate(LssStrategyConfig.unsafe(trialTerm = Some("trials"), eps = 1e-10, rankTol = 1e-5))
    )
    Vector(lsa, lss).foreach: spec =>
      val json = ModelBuildSpecJsonCodec.encode(spec).toOption.getOrElse(fail("portable single-trial encoding"))
      assertEquals(ModelBuildSpecJsonCodec.decode(json), Right(spec))
    val lssJson = ModelBuildSpecJsonCodec.encode(lss).toOption.getOrElse(fail("portable LSS JSON"))
    assert(ModelBuildSpecJsonCodec.decode(lssJson.replace("\"lss\":{\"trialTerm\"", "\"lss\":{\"unknown\":1,\"trialTerm\"")).isLeft)

  private def roundTrip(spec: ModelBuildSpec): String =
    val json = ModelBuildSpecJsonCodec.encode(spec).fold(error => fail(error.message), identity)
    val decoded = ModelBuildSpecJsonCodec.decode(json).fold(error => fail(error.message), identity)
    assertEquals(ModelBuildSpecJsonCodec.encode(decoded), Right(json))
    json

  test("nuisance, imputation, within-cell scope, polynomial and spline baselines round trip"):
    val nuisance = NuisanceRegressors(
      Vector(scalafim.fmri.hrf.linalg.Mat.unsafe(2, 2, Array(1.0, 0.1, 1e-7, 4.0))),
      names = Some(Vector(Vector("motion_x", "motion_y"))),
      check = NuisanceCheck.Drop, naAction = NaAction.Median, tol = 1e-6, duplicateThreshold = 0.95
    )
    val cells = ModulatorOrthogonalization.ordered(
      TermId.unsafe("slopes"), Vector(ModulatorId.unsafe("gain"), ModulatorId.unsafe("loss")),
      OrthogonalizationScope.WithinCells(Vector(FactorId.unsafe("cond"))), DegenerateModulatorPolicy.Reject, 1e-8
    ).toOption.get
    Vector(BaselineBasis.Poly, BaselineBasis.Bs, BaselineBasis.Ns, BaselineBasis.Constant).foreach: basis =>
      val spec = ModelBuildSpec(
        "onset ~ hrf(cond, modulators(gain, loss), id = slopes)", baselineBasis = basis, baselineDegree = 3,
        nuisance = Some(nuisance), missingValuePolicy = MissingValuePolicy.ImputeConstant(0.25),
        orthogonalization = ModulatorOrthogonalizationPlan.one(cells)
      )
      val json = roundTrip(spec)
      val decoded = ModelBuildSpecJsonCodec.decode(json).toOption.get
      assertEquals(decoded.baselineBasis, basis)
      assertEquals(decoded.missingValuePolicy, MissingValuePolicy.ImputeConstant(0.25))
      assertEquals(decoded.orthogonalization, spec.orthogonalization)
      val n = decoded.nuisance.get
      assertEquals((n.names, n.check, n.naAction, n.tol, n.duplicateThreshold), (nuisance.names, nuisance.check, nuisance.naAction, nuisance.tol, nuisance.duplicateThreshold))
      assertEquals(n.matrices.map(m => (m.rows, m.cols, m.data.toVector)), nuisance.matrices.map(m => (m.rows, m.cols, m.data.toVector)))
      assert(json.contains(""""values":[1,0.1,1e-7,4]"""), json)

  test("LSS without a trial term round trips"):
    val spec = ModelBuildSpec("onset ~ trialwise()", strategy = FitStrategy.LeastSquaresSeparate(LssStrategyConfig.unsafe(trialTerm = None)))
    val json = roundTrip(spec)
    assert(json.contains(""""trialTerm":null"""), json)
    assertEquals(ModelBuildSpecJsonCodec.decode(json), Right(spec))

  test("decode errors report the real JSON path"):
    val nuisance = NuisanceRegressors(Vector(scalafim.fmri.hrf.linalg.Mat.unsafe(1, 1, Array(1.0))))
    val spec = ModelBuildSpec("onset ~ trialwise()", nuisance = Some(nuisance), missingValuePolicy = MissingValuePolicy.ImputeConstant(1))
    val json = ModelBuildSpecJsonCodec.encode(spec).toOption.get
    def pathOf(text: String): String =
      ModelBuildSpecJsonCodec.decode(text).swap.toOption.getOrElse(fail(s"expected a decode error for $text")).path
    def swap(from: String, to: String): String =
      assert(json.contains(from), s"$from not in $json")
      json.replace(from, to)
    assertEquals(pathOf(swap(""""intercept":"Global"""", """"intercept":"global"""")), "$.intercept")
    assertEquals(pathOf(swap(""""emptyCellPolicy":"UseDropEmptyFlag"""", """"emptyCellPolicy":"Bogus"""")), "$.emptyCellPolicy")
    assertEquals(pathOf(swap(""""degenerateModulatorPolicy":"RetainAndReport"""", """"degenerateModulatorPolicy":1""")), "$.degenerateModulatorPolicy")
    assertEquals(pathOf(swap(""""check":"Warn"""", """"check":"warn"""")), "$.nuisance.check")
    assertEquals(pathOf(swap(""""naAction":"Drop"""", """"naAction":null""")), "$.nuisance.naAction")
    val tol = """"tol":""" + json.split(""""tol":""")(1).takeWhile(_ != ',')
    assertEquals(pathOf(swap(tol, """"tol":1e400""")), "$.nuisance.tol")
    assertEquals(pathOf(swap(tol, """"tol":-1""")), "$.nuisance.tol")
    val threshold = """"duplicateThreshold":""" + json.split(""""duplicateThreshold":""")(1).takeWhile(_ != '}')
    assertEquals(pathOf(swap(threshold, """"duplicateThreshold":1.5""")), "$.nuisance.duplicateThreshold")
    assertEquals(pathOf(swap(threshold, """"duplicateThreshold":-1e999""")), "$.nuisance.duplicateThreshold")
    assertEquals(pathOf(swap(""""values":[1]""", """"values":[1,2]""")), "$.nuisance.matrices[0]")
    assertEquals(pathOf(swap(""""kind":"impute","value":1""", """"kind":"impute","value":"x"""")), "$.missingValuePolicy.value")
    assertEquals(pathOf(swap(""""kind":"impute","value":1""", """"kind":"impute"""")), "$.missingValuePolicy")
    assertEquals(pathOf(swap(""""baseline":{"kind":"constant"}""", """"baseline":{"kind":"Constant"}""")), "$.baseline.kind")
    assertEquals(pathOf(swap(""""baselineDegree":1""", """"baselineDegree":0""")), "$.baselineDegree")
    assertEquals(pathOf(swap(""""baselineDegree":1""", """"baselineDegree":1.5""")), "$.baselineDegree")
    assertEquals(pathOf(swap(""""version":1""", """"version":1.5""")), "$.version")
    assertEquals(pathOf(swap(""""blockColumn":null""", """"blockColumn":7""")), "$.blockColumn")
    assertEquals(pathOf(swap(""""strategy":"ols"""", """"strategy":"bogus"""")), "$.strategy")
    assertEquals(pathOf(json.dropRight(1) + ""","strict":true}"""), "$")
    assertEquals(ModelBuildSpecJsonCodec.decode(json.dropRight(1) + ""","strict":true}""").swap.toOption.get.detail, "duplicate key 'strict'")
    assert(ModelBuildSpecJsonCodec.encode(spec.copy(missingValuePolicy = MissingValuePolicy.ImputeConstant(Double.NaN))).swap.toOption.exists(_.path == "$.missingValuePolicy.value"))

  test("nested contrast-set errors keep their document path"):
    val spec = ModelBuildSpec("onset ~ trialwise()", contrastSets = Map("effects" -> ContrastSpec.ContrastSet(Vector(ContrastSpec.Mask("m", Vector(true))))))
    val json = ModelBuildSpecJsonCodec.encode(spec).toOption.get
    assertEquals(ModelBuildSpecJsonCodec.decode(json.replace(""""id":"m"""", """"id":" m"""")).swap.toOption.map(_.path), Some("$.contrastSets.effects[0].value.id"))
    assertEquals(ModelBuildSpecJsonCodec.encode(spec.copy(contrastSets = Map("effects" -> ContrastSpec.ContrastSet(Vector(ContrastSpec.Mask("", Vector(true))))))).swap.toOption.map(_.path),
      Some("$.contrastSets.effects[0].value.id"))

  test("voxelwise FGLS has a versioned golden policy and refuses downgrade or ambiguous censoring"):
    val ar = AutocorrelationConfig.unsafe(
      order = 2, voxelwise = true, initialization = Some(ArInitialization.Stationary),
      censoredTimepoints = Vector(5, 33), biasCorrection = ArBiasCorrection.olsDesign(25).toOption.get,
      censorTreatment = ArCensorTreatment.ContinuousMissing,
      fgls = Some(VoxelwiseFglsOptions(VoxelArScope.AcrossRuns, CovarianceUncertainty.KenwardRoger))
    )
    val golden = """{"order":2,"iterations":1,"global":false,"voxelwise":true,"exactFirst":true,"censoredTimepoints":[5,33],"coefficients":{"kind":"estimate"},"censorTreatment":"ContinuousMissing","initialization":"Stationary","fgls":{"scope":"AcrossRuns","uncertainty":"KenwardRoger"},"biasCorrection":{"kind":"ols-design","ceiling":25}}"""
    assertEquals(ModelBuildSpecJsonCodec.autocorrelation(ar, "$.ar").map(ujson.write(_)), Right(golden))
    val spec = ModelBuildSpec("onset ~ trialwise()", strategy = FitStrategy.GeneralizedLeastSquares(ar))
    val json = roundTrip(spec)
    assertEquals(ujson.read(json)("version").num, 2.0)
    assert(ModelBuildSpecJsonCodec.decode(json.replace("\"version\":2", "\"version\":1")).isLeft)
    assert(ModelBuildSpecJsonCodec.decode(json.replace("KenwardRoger", "unknown")).isLeft)
    assert(ModelBuildSpecJsonCodec.decode(json.replace("ContinuousMissing", "EstimateOnly")).isLeft)
    val document = ModelDocument(spec)
    val encoded = ModelDocumentJsonCodec.encode(document).toOption.get
    assertEquals(ModelDocumentJsonCodec.decode(encoded), Right(document))
    assert(ModelDocumentJsonCodec.decode(encoded.replace("\"version\":2", "\"version\":1")).isLeft)
    assertEquals(AutocorrelationConfig.fromLegacy(ar.toLegacy), Right(ar))
    assert(AutocorrelationConfig.fromLegacy(ar.toLegacy.copy(voxelwise = false)).isLeft)
    assert(AutocorrelationConfig.fromLegacy(ar.toLegacy.copy(fgls = None)).isLeft)

  test("voxelwise FGLS restart initialization cannot silently claim a stationary factor"):
    val policy = Some(VoxelwiseFglsOptions(VoxelArScope.AcrossRuns, CovarianceUncertainty.Conditional))
    val correction = ArBiasCorrection.olsDesign(25).toOption.get
    assert(AutocorrelationConfig(order = 2, voxelwise = true, biasCorrection = correction,
      initialization = Some(ArInitialization.Stationary), fgls = policy).isLeft)
    assert(AutocorrelationConfig(order = 2, voxelwise = true, biasCorrection = correction,
      initialization = Some(ArInitialization.ExactAr1), fgls = policy).isRight)

  test("response support is preserved in version 3 and cannot be stripped or assigned an older version"):
    val request = ResponseSupportRequest(Vector(1, 3, 4).map(ScanIndex.unsafeOneBased),
      EventSupportPolicy.ExcludeUnobservedAllowPartial, 1e-8)
    val spec = ModelBuildSpec("onset ~ hrf(condition)", responseSupport = Some(request))
    val encoded = ModelBuildSpecJsonCodec.encode(spec).fold(error => fail(error.message), identity)
    assertEquals(ModelBuildSpecJsonCodec.decode(encoded), Right(spec))
    assertEqualsDouble(ujson.read(encoded)("version").num, 3.0, 0.0)
    assert(encoded.contains("\"responseSupport\":{\"retainedScans\":[1,3,4],\"policy\":\"ExcludeUnobservedAllowPartial\",\"absoluteTolerance\":1e-8}"))
    val missing = ujson.read(encoded)
    missing.obj.remove("responseSupport")
    assert(ModelBuildSpecJsonCodec.decode(ujson.write(missing)).isLeft)
    val old = ujson.read(encoded)
    old("version") = ujson.Num(2)
    assert(ModelBuildSpecJsonCodec.decode(ujson.write(old)).isLeft)
    for invalid <- Vector(Vector(1, 1), Vector(2, 1), Vector(0, 1)) do
      val changed = ujson.read(encoded)
      changed("responseSupport")("retainedScans") = ujson.Arr.from(invalid)
      assert(ModelBuildSpecJsonCodec.decode(ujson.write(changed)).left.exists(_.path.contains("retainedScans")))
    for tolerance <- Vector(-1.0); policy <- Vector("Unknown") do
      val changed = ujson.read(encoded)
      changed("responseSupport")("absoluteTolerance") = ujson.Num(tolerance)
      assert(ModelBuildSpecJsonCodec.decode(ujson.write(changed)).left.exists(_.path.endsWith("absoluteTolerance")))
      changed("responseSupport")("absoluteTolerance") = ujson.Num(0.0)
      changed("responseSupport")("policy") = ujson.Str(policy)
      assert(ModelBuildSpecJsonCodec.decode(ujson.write(changed)).left.exists(_.path.endsWith("policy")))
