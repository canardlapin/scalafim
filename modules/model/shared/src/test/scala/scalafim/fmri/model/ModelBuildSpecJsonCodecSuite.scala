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
    assert(ModelBuildSpecJsonCodec.decode(json.replace("\"version\":1", "\"version\":2")).isLeft)

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
    strategies.foreach: strategy =>
      val spec = ModelBuildSpec("onset ~ trialwise()", strategy = strategy)
      val json = ModelBuildSpecJsonCodec.encode(spec).toOption.getOrElse(fail("portable GLS encoding"))
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
