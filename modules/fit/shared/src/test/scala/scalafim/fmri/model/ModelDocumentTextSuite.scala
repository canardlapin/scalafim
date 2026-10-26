package scalafim.fmri.model

import gale.linalg.Matrix
import scalafim.fmri.design.*
import scalafim.fmri.design.baseline.*
import scalafim.fmri.design.contrast.*
import scalafim.fmri.design.formula.{DerivedEventPlan, DerivedMissingRows}

class ModelDocumentTextSuite extends munit.FunSuite:
  private val derived = DerivedEventPlan.parse(Vector("reward: number = gain - loss"), DerivedMissingRows.Drop)
    .fold(error => fail(error.message), identity)
  private val contrasts = Map("effects" -> ContrastSpec.ContrastSet(Vector(ContrastSpec.Typed(ContrastExpr.UnitContrast(ContrastId.unsafe("unit"))))))

  private val ar = AutocorrelationConfig.unsafe(order = 2, iterations = 1, global = true, exactFirst = false,
    censoredTimepoints = Vector(3, 8), coefficients = ArCoefficientSpec.Phi(Vector(0.4, -0.15)))
  private val controls = FitControls(
    ModelVolumeWeighting.Estimated(DvarsWeightEstimator(DvarsWeightFunction.SoftThreshold(VolumeWeightThreshold.unsafe(2.0), SoftThresholdSteepness.unsafe(3.0)), DvarsWeightScope.AcrossSelection)),
    ModelNuisanceProjection.MatrixProjection(NuisanceMatrix.unsafe(Matrix.dense(2, 1, Vector(1.0, 0.5))), Regularization.Fixed(4.0)),
    MissingDataPolicy.OmitRowsPerVoxel
  )

  private val document = ModelDocument(
    ModelBuildSpec("onset ~ hrf(reward, id = slope)", blockColumn = Some("run"), contrastSets = contrasts,
      baselineBasis = BaselineBasis.Dct(DctCutoffPeriod.unsafeSeconds(128)), baselineDegree = 2, baselineIntercept = Intercept.Runwise,
      strategy = FitStrategy.RunwiseGeneralizedLeastSquares(ar, controls), derived = derived),
    Some(ConfoundSpec(MotionExpansion.RawAndDerivative12, acompcorComponents = 5, includeCsf = true,
      censor = Some(FdCensorPolicy(0.5, before = 1, after = 2, minimumRetainedSegment = 5, maximumCensoredFraction = 0.25)))),
    RunContrastCombination.Concatenated(Vector(ColumnId.unsafe("slope_reward")))
  )

  private def render(value: ModelDocument): Vector[String] =
    ModelDocumentText.render(value).fold(error => fail(error.message), identity)

  test("canonical lines are pinned and re-parse to the same envelope"):
    val lines = render(document)
    assertEquals(lines, Vector(
      "formula: onset ~ hrf(reward, id = slope)",
      "missing_rows: drop",
      "derive: reward: number = (gain - loss)",
      "baseline: dct(cutoff = 128, degree = 2, intercept = \"runwise\")",
      "confounds: confounds(motion = \"raw_and_derivative12\", acompcor = 5, white_matter = FALSE, csf = TRUE, global_signal = FALSE, " +
        "censor = fd(threshold = 0.5, before = 1, after = 2, min_segment = 5, max_fraction = 0.25))",
      "estimation: runwise_gls(ar = ar(order = 2, iterations = 1, global = TRUE, voxelwise = FALSE, exact_first = FALSE, censored = c(3, 8), " +
        "coefficients = phi(values = c(0.4, -0.15)), bias = raw()), weights = dvars(function = soft_threshold(threshold = 2, steepness = 3), " +
        "scope = \"across_selection\"), projection = matrix(rows = 2, cols = 1, values = c(1, 0.5), lambda = fixed(value = 4)), missing = \"omit_rows_per_voxel\")",
      "runs: concatenated(columns = c(\"slope_reward\"))",
      "base: fnv64-84ec4c09d96dced2"
    ))
    // Pieces without a text form come from the base; every text piece is replaced.
    val base = ModelDocument(ModelBuildSpec("onset ~ trialwise()", blockColumn = Some("run"), contrastSets = contrasts))
    val parsed = ModelDocumentText.parse(lines, base).fold(error => fail(error.message), identity)
    assertEquals(parsed, document)
    assertEquals(ModelDocumentJsonCodec.encode(parsed), ModelDocumentJsonCodec.encode(document))
    assertEquals(render(parsed), lines)

  test("every portable strategy and the empty pieces round trip through text"):
    Vector(
      FitStrategy.OrdinaryLeastSquares(),
      FitStrategy.RunwiseLeastSquares(FitControls(volumeWeighting = ModelVolumeWeighting.fixed(Vector(1.0, 0.0), FixedWeightAlignment.SelectedRows).toOption.get)),
      FitStrategy.SeparateRunsThenFixedEffects(FitControls(volumeWeighting = ModelVolumeWeighting.estimatedDvars(DvarsWeightFunction.TukeyBisquare()))),
      FitStrategy.GeneralizedLeastSquares(AutocorrelationConfig.unsafe(order = 1, iterations = 1, biasCorrection = ArBiasCorrection.olsDesign(3).toOption.get)),
      FitStrategy.GeneralizedLeastSquares(AutocorrelationConfig.unsafe(order = 1, iterations = 0, coefficients = ArCoefficientSpec.Rho(0.35))),
      FitStrategy.GeneralizedLeastSquares(AutocorrelationConfig.withInitialization(ArInitialization.Stationary, order = 2).toOption.get),
      FitStrategy.LeastSquaresSeparate(LssStrategyConfig.unsafe(trialTerm = Some("trials"))),
      FitStrategy.LeastSquaresSeparate(LssStrategyConfig.unsafe(), FitControls(nuisanceProjection = ModelNuisanceProjection.MatrixProjection(NuisanceMatrix.unsafe(Matrix.dense(1, 1, Vector(2.0))), Regularization.Gcv)))
    ).foreach: strategy =>
      val value = ModelDocument(ModelBuildSpec("onset ~ trialwise()", strategy = strategy, baselineBasis = BaselineBasis.Poly))
      val lines = render(value)
      assertEquals(ModelDocumentText.parse(lines, ModelDocument(ModelBuildSpec("onset ~ trialwise()"))), Right(value))
    assert(render(ModelDocument(ModelBuildSpec("onset ~ trialwise()"))).contains("confounds: none()"))

  test("malformed lines fail with their line number"):
    val lines = render(document)
    val base = ModelDocument(ModelBuildSpec("onset ~ trialwise()", blockColumn = Some("run"), contrastSets = contrasts))
    def errorOf(text: Vector[String]): ModelTextError =
      ModelDocumentText.parse(text, base).swap.toOption.getOrElse(fail(s"expected a parse error for $text"))
    assertEquals(errorOf(lines :+ "colour: blue").line, Some(9))
    assertEquals(errorOf(lines :+ lines(3)).line, Some(9))
    assertEquals(errorOf(lines.patch(3, Vector("derive: reward: number = gain"), 0)).line, Some(4))
    assertEquals(errorOf(lines.filterNot(_.startsWith("runs:"))), ModelTextError(None, "missing 'runs' line"))
    assertEquals(errorOf(lines.updated(3, "baseline: dct(cutoff = 128, degree = 2)")).line, Some(4))
    assertEquals(errorOf(lines.updated(3, "baseline: dct(cutoff = 128, degree = 2, intercept = \"runwise\", extra = 1)")).line, Some(4))
    assertEquals(errorOf(lines.updated(1, "missing_rows: keep")).line, Some(2))
    assertEquals(errorOf(lines.updated(6, "runs: pooled()")).line, Some(7))
    assertEquals(errorOf(lines.updated(4, "confounds: confounds(motion = \"friston\")")).line, Some(5))
    assert(ModelDocumentText.render(document.copy(build = document.build.copy(strategy = FitStrategy.RobustLeastSquares(RobustConfig.unsafe(RobustOptions(psi = RobustPsi.Huber())))))).isLeft)

  test("text cannot be combined with a base that differs in fields without a text form"):
    val lines = render(document)
    val good = ModelDocument(ModelBuildSpec("onset ~ trialwise()", blockColumn = Some("run"), contrastSets = contrasts))
    assertEquals(ModelDocumentText.parse(lines, good), Right(document))
    Vector(
      good.build.copy(blockColumn = None),
      good.build.copy(contrastSets = Map.empty),
      good.build.copy(precision = scalafim.fmri.hrf.Seconds(0.1)),
      good.build.copy(missingValuePolicy = MissingValuePolicy.Reject),
      good.build.copy(factorLevels = FactorLevelRegistry.of("level" -> Seq("a", "b")).toOption.get)
    ).foreach: other =>
      val error = ModelDocumentText.parse(lines, ModelDocument(other)).swap.toOption.getOrElse(fail(s"expected a digest mismatch for $other"))
      assertEquals(error.line, Some(lines.length))
    // Fields that do have a text form may differ freely in the base.
    val textual = ModelDocument(good.build.copy(formula = "onset ~ hrf(x)", strategy = FitStrategy.RunwiseLeastSquares(), baselineDegree = 4))
    assertEquals(ModelDocumentText.parse(lines, textual), Right(document))

  test("formula text with surrounding whitespace is rejected rather than trimmed"):
    assert(ModelDocumentText.render(ModelDocument(ModelBuildSpec(" onset ~ trialwise()"))).isLeft)
