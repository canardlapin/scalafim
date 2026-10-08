package scalafim.fmri.model

import gale.linalg.Matrix
import scalafim.fmri.design.*
import scalafim.fmri.design.baseline.*
import scalafim.fmri.design.contrast.*
import scalafim.fmri.design.formula.{DerivedEventPlan, DerivedMissingRows, ModelJsonCodec}

class ModelDocumentJsonCodecSuite extends munit.FunSuite:
  private val derived = DerivedEventPlan.parse(
    Vector("reward: number = gain - loss", "level: text = cut(reward, c(-Inf, 0, Inf), c(\"loss\", \"gain\"))"),
    DerivedMissingRows.Drop
  ).fold(error => fail(error.message), identity)

  private val controls = FitControls(
    volumeWeighting = ModelVolumeWeighting.Estimated(DvarsWeightEstimator(
      DvarsWeightFunction.SoftThreshold(VolumeWeightThreshold.unsafe(2.0), SoftThresholdSteepness.unsafe(3.0)),
      DvarsWeightScope.AcrossSelection
    )),
    nuisanceProjection = ModelNuisanceProjection.MatrixProjection(NuisanceMatrix.unsafe(Matrix.dense(2, 2, Vector(1.0, 0.0, 0.5, 2.0))), Regularization.Fixed(4.0)),
    missingData = MissingDataPolicy.ExcludeVoxel
  )

  private val full = ModelDocument(
    build = ModelBuildSpec(
      formula = "onset ~ hrf(level, id = bins) + hrf(reward, id = slope)",
      blockColumn = Some("run"),
      baselineBasis = BaselineBasis.Dct(DctCutoffPeriod.unsafeSeconds(128)),
      baselineDegree = 2,
      baselineIntercept = Intercept.Runwise,
      strategy = FitStrategy.RunwiseGeneralizedLeastSquares(AutocorrelationConfig.unsafe(order = 1, iterations = 1), controls),
      contrastSets = Map("effects" -> ContrastSpec.ContrastSet(Vector(ContrastSpec.Typed(ContrastExpr.UnitContrast(ContrastId.unsafe("unit")))))),
      derived = derived
    ),
    confounds = Some(ConfoundSpec(MotionExpansion.Friston24, acompcorComponents = 5, includeCsf = true,
      censor = Some(FdCensorPolicy(threshold = 0.5, before = 1, after = 2, minimumRetainedSegment = 5, maximumCensoredFraction = 0.25)))),
    runCombination = RunContrastCombination.Concatenated(Vector(ColumnId.unsafe("bins_level.gain")))
  )

  private def encoded(document: ModelDocument): String =
    ModelDocumentJsonCodec.encode(document).fold(error => fail(error.message), identity)

  private def decoded(text: String): ModelDocument =
    ModelDocumentJsonCodec.decode(text).fold(error => fail(error.message), identity)

  test("every portable piece round trips and re-encodes byte for byte"):
    val json = encoded(full)
    assertEquals(decoded(json), full)
    assertEquals(encoded(decoded(json)), json)
    Vector(
      FitStrategy.OrdinaryLeastSquares(FitControls(volumeWeighting = ModelVolumeWeighting.fixed(Vector(1.0, 0.5, 0.0), FixedWeightAlignment.SelectedRows).toOption.get)),
      FitStrategy.RunwiseLeastSquares(),
      FitStrategy.SeparateRunsThenFixedEffects(FitControls(volumeWeighting = ModelVolumeWeighting.estimatedDvars(DvarsWeightFunction.TukeyBisquare()))),
      FitStrategy.GeneralizedLeastSquares(controls = FitControls(nuisanceProjection = ModelNuisanceProjection.MatrixProjection(NuisanceMatrix.unsafe(Matrix.dense(1, 1, Vector(1.0))), Regularization.Gcv))),
      FitStrategy.LeastSquaresSeparate(LssStrategyConfig.unsafe(trialTerm = Some("trials")), FitControls(missingData = MissingDataPolicy.OmitRowsPerVoxel))
    ).foreach: strategy =>
      val document = ModelDocument(ModelBuildSpec("onset ~ trialwise()", strategy = strategy))
      val text = encoded(document)
      assertEquals(decoded(text), document)
      assertEquals(encoded(decoded(text)), text)

  test("canonical text is pinned, so the JVM and Scala.js render identical bytes"):
    val document = ModelDocument(
      ModelBuildSpec("onset ~ trialwise()", derived = DerivedEventPlan.parse(Vector("x: number = 1 + 2"), DerivedMissingRows.PreserveNumeric).toOption.get),
      confounds = Some(ConfoundSpec(censor = Some(FdCensorPolicy(1.0))))
    )
    val expected =
      """{"schema":"scalafim.model-document","version":1,"formula":"onset ~ trialwise()",""" +
      """"derived":{"missingRows":"preserve-numeric","columns":["x: number = (1 + 2)"]},""" +
      """"build":{"blockColumn":null,"durationColumn":null,"baseline":{"kind":"constant"},"baselineDegree":1,"intercept":"Global",""" +
      """"strategy":{"kind":"ols","controls":{"volumeWeighting":{"kind":"disabled"},"nuisanceProjection":{"kind":"disabled"},"missingData":"Error"}},""" +
      """"defaultHrf":"SPMG1","precision":0.3,"dropEmpty":true,"summate":true,"strict":false,"contrastSets":{},"nuisance":null,"factorLevels":[],""" +
      """"emptyCellPolicy":"UseDropEmptyFlag","missingValuePolicy":{"kind":"zero-contribution"},"degenerateModulatorPolicy":"RetainAndReport","orthogonalization":[]},""" +
      """"confounds":{"motion":"Raw6","acompcorComponents":0,"includeWhiteMatter":false,"includeCsf":false,"includeGlobalSignal":false,""" +
      """"censor":{"threshold":1,"before":0,"after":0,"minimumRetainedSegment":1,"maximumCensoredFraction":1}},""" +
      """"runCombination":{"type":"fixed-effects"}}"""
    assertEquals(encoded(document), expected)
    assertEquals(decoded(expected), document)

  test("unknown and missing fields fail with the JSON path of the offending value"):
    val json = encoded(full)
    def pathOf(text: String): String =
      ModelDocumentJsonCodec.decode(text).swap.toOption.getOrElse(fail(s"expected a decode error for $text")).path
    def swap(from: String, to: String): String =
      assert(json.contains(from), s"$from not in $json")
      json.replace(from, to)
    assertEquals(pathOf(json.dropRight(1) + ""","extra":1}"""), "$")
    assertEquals(pathOf(swap(""""runCombination":""", """"runCombo":""")), "$")
    assertEquals(pathOf(swap(""""blockColumn":"run",""", """"blockColumn":"run","bogus":true,""")), "$.build")
    assertEquals(pathOf(swap(""""missingData":"ExcludeVoxel"""", """"missingData":"exclude"""")), "$.build.strategy.controls.missingData")
    assertEquals(pathOf(swap(""""scope":"AcrossSelection"""", """"scope":"AcrossSelection","extra":0""")), "$.build.strategy.controls.volumeWeighting")
    assertEquals(pathOf(swap(""""steepness":3""", """"steepness":-3""")), "$.build.strategy.controls.volumeWeighting.function.steepness")
    assertEquals(pathOf(swap(""""rows":2,"cols":2""", """"rows":3,"cols":2""")), "$.build.strategy.controls.nuisanceProjection")
    assertEquals(pathOf(swap(""""kind":"fixed","value":4""", """"kind":"fixed","value":-4""")), "$.build.strategy.controls.nuisanceProjection.lambda.value")
    assertEquals(pathOf(swap(""","minimumRetainedSegment":5""", "")), "$.confounds.censor")
    assertEquals(pathOf(swap(""""motion":"Friston24"""", """"motion":"friston24"""")), "$.confounds.motion")
    assertEquals(pathOf(swap(""""missingRows":"drop"""", """"missingRows":"keep"""")), "$.derived.missingRows")
    assertEquals(pathOf(swap(""""version":1""", """"version":2""")), "$.version")
    assertEquals(pathOf(swap("""scalafim.model-document""", """scalafim.model-other""")), "$.schema")
    assertEquals(pathOf(swap(""""type":"concatenated"""", """"type":"pooled"""")), "$.runCombination.type")
    assertEquals(pathOf(swap(""""formula":"onset""", """"formula":"onset ~~""")), "$.formula")

  test("strategies and HRFs without a portable form are refused at their path"):
    val robust = full.copy(build = full.build.copy(strategy = FitStrategy.RobustLeastSquares(RobustConfig.unsafe(RobustOptions(psi = RobustPsi.Huber())))))
    assertEquals(ModelDocumentJsonCodec.encode(robust).swap.toOption.map(_.path), Some("$.build.strategy"))
    val fir = full.copy(build = full.build.copy(defaultHrf = scalafim.fmri.hrf.Hrfs.fir(4)))
    assertEquals(ModelDocumentJsonCodec.encode(fir).swap.toOption.map(_.path), Some("$.build.defaultHrf"))

  test("existing model-build and model-spec v1 documents still decode"):
    val v1 =
      """{"schema":"scalafim.model-build","version":1,"formula":"onset ~ trialwise()","blockColumn":"run","durationColumn":null,""" +
      """"baseline":{"kind":"dct","cutoff":128},"baselineDegree":1,"intercept":"Runwise","strategy":"runwise-ols","defaultHrf":"SPMG1",""" +
      """"precision":0.3,"dropEmpty":true,"summate":true,"strict":false,"contrastSets":{},"nuisance":null,"factorLevels":[],""" +
      """"emptyCellPolicy":"UseDropEmptyFlag","missingValuePolicy":{"kind":"zero-contribution"},"degenerateModulatorPolicy":"RetainAndReport","orthogonalization":[]}"""
    val spec = ModelBuildSpec("onset ~ trialwise()", blockColumn = Some("run"), baselineBasis = BaselineBasis.Dct(DctCutoffPeriod.unsafeSeconds(128)),
      baselineIntercept = Intercept.Runwise, strategy = FitStrategy.RunwiseLeastSquares())
    assertEquals(ModelBuildSpecJsonCodec.decode(v1), Right(spec))
    assertEquals(ModelBuildSpecJsonCodec.encode(spec), Right(v1))
    assertEquals(ModelDocumentJsonCodec.decode(v1), Right(ModelDocument(spec)))
    val runCombination = """{"schema":"scalafim.model-spec","version":1,"kind":"run-combination","value":{"type":"concatenated","taskColumns":["a","b"]}}"""
    assertEquals(ModelJsonCodec.decodeRunCombination(runCombination), Right(RunContrastCombination.Concatenated(Vector(ColumnId.unsafe("a"), ColumnId.unsafe("b")))))
    val columns = """{"schema":"scalafim.model-spec","version":1,"kind":"derived-columns","value":["reward: number = gain - loss"]}"""
    assertEquals(ModelJsonCodec.decodeDerivedColumns(columns).map(_.map(_.text)), Right(Vector("reward: number = (gain - loss)")))
    val confounds = """{"version":1,"motion":"Raw6","acompcorComponents":0,"includeWhiteMatter":false,"includeCsf":false,"includeGlobalSignal":false,"censor":null}"""
    assertEquals(ConfoundSpecJsonCodec.decode(confounds), Right(ConfoundSpec()))
