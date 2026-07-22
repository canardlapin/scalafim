package scalafim.multivar

import gale.linalg.DMat

class RecoveryValidationSuite extends munit.FunSuite:

  test("the fold-safety manifest binds every learned component to ModelSpec"):
    val spec = modelSpec("fold-manifest")
    val manifest = accepted(FoldSafetyManifest.from(spec, bindings))

    assertEquals(manifest.bindings.map(_.component).toSet, FoldFittedComponent.all)
    assertEquals(manifest.binding(FoldFittedComponent.Offsets).stage, LifecycleStage.Preprocessing)
    assertEquals(manifest.binding(FoldFittedComponent.Graph).artifact, "fit-graph")
    assertEquals(manifest.binding(FoldFittedComponent.Rank).hyperparameters, Set("rank"))

    val missing = FoldSafetyManifest.from(spec, bindings.filterNot(_.component == FoldFittedComponent.Encoding))
    assert(missing.left.exists(_ == RecoveryValidationError.MissingFoldComponent(FoldFittedComponent.Encoding)))

    val wrongArtifact = bindings.map: binding =>
      if binding.component == FoldFittedComponent.Graph then
        accepted(FoldComponentBinding.from(binding.component, binding.stage, "foreign-graph"))
      else binding
    assert(FoldSafetyManifest.from(spec, wrongArtifact).left.exists(_.message.contains("foreign-graph")))

  test("offset and scaling estimation require fold-fitted standardization"):
    val centered = modelSpec("centered-manifest", preprocessing = PreprocessSpec.Center)
    val result = FoldSafetyManifest.from(centered, bindings)

    assert(result.left.exists:
      case RecoveryValidationError.LifecycleMismatch(FoldFittedComponent.Scaling, detail) =>
        detail.contains("does not estimate scaling")
      case _ => false
    )

  test("row, column, entry, group, site, and error resampling are distinct explicit designs"):
    val row = accepted(ResamplingDesign.from(ResamplingUnit.Rows, None, "resample independent subjects"))
    val column = accepted(ResamplingDesign.from(ResamplingUnit.Columns, None, "resample measured features"))
    val entry = accepted(ResamplingDesign.from(ResamplingUnit.Entries, None, "hold out matrix cells"))
    val group = accepted(
      ResamplingDesign.from(ResamplingUnit.Groups, Some(id("group-membership")), "cluster bootstrap")
    )
    val site = accepted(
      ResamplingDesign.from(ResamplingUnit.Sites, Some(id("site-membership")), "leave one site out")
    )
    val errors = accepted(
      ResamplingDesign.from(ResamplingUnit.Errors, Some(id("error-exchangeability")), "wild error bootstrap")
    )

    assertEquals(
      Vector(row, column, entry, group, site, errors).map(_.unit),
      Vector(
        ResamplingUnit.Rows,
        ResamplingUnit.Columns,
        ResamplingUnit.Entries,
        ResamplingUnit.Groups,
        ResamplingUnit.Sites,
        ResamplingUnit.Errors
      )
    )
    assert(ResamplingDesign.from(ResamplingUnit.Groups, None, "unidentified groups").isLeft)
    assert(ResamplingDesign.from(ResamplingUnit.Rows, Some(id("accidental-groups")), "rows").isLeft)

  test("fixed-mask, MCAR, MAR, and MNAR targets do not coerce into one another"):
    val fixed = ValidationMissingnessTarget.FixedMaskPrediction(id("fixed-mask"))
    val mcar = accepted(ValidationMissingnessTarget.mcar(id("mcar-generator"), 0.2))
    val mar = ValidationMissingnessTarget.MarSensitivity(
      id("mar-mechanism"),
      ObservationReason.unsafe("deletion depends on observed acquisition covariates")
    )
    val mnar = ValidationMissingnessTarget.MnarSensitivity(
      id("mnar-selection"),
      ObservationReason.unsafe("selection depends on the latent unobserved value")
    )

    assertEquals(Vector(fixed, mcar, mar, mnar).distinct.length, 4)
    assert(Vector(fixed, mcar, mar, mnar).forall(!_.inferentialClaimGranted))
    assert(ValidationMissingnessTarget.mcar(id("invalid-mcar"), 1.0).isLeft)

  test("warm-start paths are deterministic, within-fold, and provenance complete"):
    val spec = modelSpec("warm-path")
    val split = SplitIdentity.unsafe("inner-warm-a")
    val first = accepted(DeterministicWarmStartPath.from(spec, split, spec.candidates.map(_.id)))
    val second = accepted(DeterministicWarmStartPath.from(spec, split, spec.candidates.map(_.id)))

    assertEquals(first.resetPolicy, WarmStartResetPolicy.ResetAtEveryFold)
    assertEquals(first.edges, second.edges)
    assertEquals(first.valueIdentity, second.valueIdentity)
    assertEquals(first.edges.length, spec.candidates.length - 1)
    assert(first.edges.forall(_.split == split))
    assert(DeterministicWarmStartPath.from(spec, split, spec.candidates.map(_.id).reverse).isLeft)

  test("simulation coverage requires every adversarial statistical regime"):
    val complete = accepted(RecoverySimulationCoverage.from(simulationCases))
    val missing = RecoverySimulationCoverage.from(
      simulationCases.filterNot(_.scenario == RecoverySimulationScenario.MisspecifiedGraph)
    )

    assertEquals(complete.cases.map(_.scenario).toSet, RecoverySimulationScenario.all)
    assert(missing.left.exists:
      case RecoveryValidationError.MissingSimulationScenarios(values) =>
        values == Set(RecoverySimulationScenario.MisspecifiedGraph)
      case _ => false
    )
    assert(
      RecoverySimulationCase
        .from(
          RecoverySimulationDesign.PMuchGreaterThanN(sampleCount = 20, featureCount = 50),
          DeterministicSeed(9),
          id("invalid-high-dimensional-generator"),
          "not actually high dimensional"
        )
        .isLeft
    )

  test("recovery metrics match independent projector, sign, support, roughness, and calibration oracles"):
    val projector = matrix(Vector(Vector(1.0, 0.0), Vector(0.0, 0.0)))
    val truthFactors = matrix(Vector(Vector(1.0), Vector(-1.0)))
    val estimatedFactors = matrix(Vector(Vector(-1.0), Vector(1.0)))
    val roughness = matrix(Vector(Vector(-1.0, 1.0)))
    val metrics = accepted(
      RecoveryMetrics.from(
        projector,
        projector,
        estimatedFactors,
        truthFactors,
        estimatedSupport = Set(1, 2),
        truthSupport = Set(2, 3),
        heldOutLosses = Vector(0.5, 1.5),
        roughness,
        selectedSupports = Vector(Set(1, 2), Set(2, 3)),
        estimatedBlockContributions = Vector(1.0, 3.0),
        truthBlockContributions = Vector(2.0, 2.0)
      )
    )

    assertEqualsDouble(metrics.subspaceProjectorError, 0.0, 1e-14)
    assertEqualsDouble(metrics.alignedFactorError, 0.0, 1e-14)
    assertEquals(metrics.support.truePositive, 1)
    assertEqualsDouble(metrics.support.precision, 0.5, 1e-14)
    assertEqualsDouble(metrics.support.recall, 0.5, 1e-14)
    assertEqualsDouble(metrics.heldOutRisk, 1.0, 1e-14)
    assertEqualsDouble(metrics.roughness, 2.0, 1e-14)
    assertEqualsDouble(metrics.stability, 1.0 / 3.0, 1e-14)
    assertEqualsDouble(metrics.blockCalibrationError, 0.5, 1e-14)

  test("support-recovery and inferential claims require a theorem while empirical claims stay labeled"):
    val empirical = ValidationClaimEvidence.EmpiricalOnly(id("empirical-design"))
    val predictive = accepted(
      ValidationClaim.from(
        ValidationClaimKind.PredictiveRisk,
        "held-out risk improved in the declared resampling design",
        empirical
      )
    )
    val theorem = ValidationClaimEvidence.TheoremBacked(
      ContractReference.theorem("support-recovery-theorem").toOption.get,
      Set(ContractReference.assumption("beta-min").toOption.get),
      id("support-recovery-witness")
    )
    val support = accepted(
      ValidationClaim.from(
        ValidationClaimKind.SupportRecovery,
        "support is recovered under beta-min and incoherence assumptions",
        theorem
      )
    )

    assertEquals(predictive.evidence, empirical)
    assert(support.evidence.isInstanceOf[ValidationClaimEvidence.TheoremBacked])
    assert(
      ValidationClaim
        .from(ValidationClaimKind.SupportRecovery, "empirical support looked stable", empirical)
        .left
        .exists(_ == RecoveryValidationError.ClaimRequiresTheorem(ValidationClaimKind.SupportRecovery))
    )
    assert(
      ValidationClaim
        .from(ValidationClaimKind.Inferential, "latent factor is associated with outcome", empirical)
        .isLeft
    )

  test("a complete report binds fold safety, missingness, warm starts, simulations, metrics, and claim level"):
    val spec = modelSpec("complete-report")
    val manifest = accepted(FoldSafetyManifest.from(spec, bindings))
    val coverage = accepted(RecoverySimulationCoverage.from(simulationCases))
    val warm = accepted(
      DeterministicWarmStartPath.from(
        spec,
        SplitIdentity.unsafe("complete-report-inner"),
        spec.candidates.map(_.id)
      )
    )
    val metrics = zeroMetrics
    val results = simulationCases.map: current =>
      ScenarioRecoveryResult(
        current.scenario,
        current.seed,
        metrics,
        ValueIdentity.derived(s"result-${current.scenario.toString}", current.generatorIdentity)
      )
    val claim = accepted(
      ValidationClaim.from(
        ValidationClaimKind.DescriptiveStability,
        "selection stability is descriptive under the declared simulation suite",
        ValidationClaimEvidence.EmpiricalOnly(coverage.valueIdentity)
      )
    )
    val report = accepted(
      RecoveryValidationReport.from(
        accepted(ResamplingDesign.from(ResamplingUnit.Sites, Some(id("report-sites")), "leave-site-out")),
        ValidationMissingnessTarget.FixedMaskPrediction(id("report-mask")),
        manifest,
        Vector(warm),
        coverage,
        results,
        Vector(claim)
      )
    )

    assertEquals(report.results.map(_.scenario).toSet, RecoverySimulationScenario.all)
    assertEquals(report.claims.map(_.kind), Vector(ValidationClaimKind.DescriptiveStability))
    assert(!report.missingness.inferentialClaimGranted)
    assert(report.valueIdentity.stableKey.nonEmpty)

  private lazy val bindings: Vector[FoldComponentBinding] = Vector(
    accepted(FoldComponentBinding.from(FoldFittedComponent.Offsets, LifecycleStage.Preprocessing, "standardize-offset")),
    accepted(FoldComponentBinding.from(FoldFittedComponent.Scaling, LifecycleStage.Preprocessing, "standardize-scale")),
    accepted(
      FoldComponentBinding.from(
        FoldFittedComponent.LossBalancing,
        LifecycleStage.StatisticalEstimation,
        "fit-loss-balance",
        Set("lossWeight")
      )
    ),
    accepted(FoldComponentBinding.from(FoldFittedComponent.Graph, LifecycleStage.GraphEstimation, "fit-graph")),
    accepted(FoldComponentBinding.from(FoldFittedComponent.Encoding, LifecycleStage.ChartEstimation, "fit-encoding")),
    accepted(
      FoldComponentBinding.from(
        FoldFittedComponent.Rank,
        LifecycleStage.ProgramBuild,
        "select-rank",
        Set("rank")
      )
    ),
    accepted(
      FoldComponentBinding.from(
        FoldFittedComponent.Penalties,
        LifecycleStage.Lowering,
        "select-penalty",
        Set("penalty")
      )
    )
  )

  private lazy val simulationCases: Vector[RecoverySimulationCase] =
    RecoverySimulationScenario.all.toVector.sortBy(_.toString).zipWithIndex.map: (scenario, index) =>
      accepted(
        RecoverySimulationCase.from(
          simulationDesign(scenario),
          DeterministicSeed(1000 + index),
          id(s"generator-${scenario.toString.toLowerCase}"),
          s"deterministic $scenario recovery simulation"
        )
      )

  private def simulationDesign(scenario: RecoverySimulationScenario): RecoverySimulationDesign =
    scenario match
      case RecoverySimulationScenario.SparseSmoothSignal =>
        RecoverySimulationDesign.SparseSmoothSignal(80, 200, 10, 0.25)
      case RecoverySimulationScenario.DisconnectedGraph =>
        RecoverySimulationDesign.DisconnectedGraph(200, 4)
      case RecoverySimulationScenario.PMuchGreaterThanN =>
        RecoverySimulationDesign.PMuchGreaterThanN(40, 800)
      case RecoverySimulationScenario.CorrelatedNoise =>
        RecoverySimulationDesign.CorrelatedNoise(0.7)
      case RecoverySimulationScenario.WeakEigengap =>
        RecoverySimulationDesign.WeakEigengap(1.0, 0.01)
      case RecoverySimulationScenario.BlockImbalance =>
        RecoverySimulationDesign.BlockImbalance(20, 400)
      case RecoverySimulationScenario.MisspecifiedGraph =>
        RecoverySimulationDesign.MisspecifiedGraph(100, 120, 30)

  private lazy val zeroMetrics: RecoveryMetrics =
    accepted(
      RecoveryMetrics.from(
        matrix(Vector(Vector(1.0))),
        matrix(Vector(Vector(1.0))),
        matrix(Vector(Vector(1.0))),
        matrix(Vector(Vector(1.0))),
        Set(0),
        Set(0),
        Vector(0.0),
        matrix(Vector(Vector(0.0))),
        Vector(Set(0), Set(0)),
        Vector(1.0),
        Vector(1.0)
      )
    )

  private def modelSpec(
      name: String,
      preprocessing: PreprocessSpec = PreprocessSpec.Standardize
  ): ModelSpec =
    val candidates = Vector(
      candidate(s"$name-candidate-a", 1.0, 0.1, 0.5),
      candidate(s"$name-candidate-b", 2.0, 0.2, 1.0)
    )
    val plans = Vector(
      LifecyclePlan.unsafe(LifecycleStage.StatisticalEstimation, "fit-loss-balance"),
      LifecyclePlan.unsafe(LifecycleStage.GraphEstimation, "fit-graph"),
      LifecyclePlan.unsafe(LifecycleStage.ChartEstimation, "fit-encoding"),
      LifecyclePlan.unsafe(LifecycleStage.ProgramBuild, "select-rank"),
      LifecyclePlan.unsafe(LifecycleStage.Lowering, "select-penalty"),
      LifecyclePlan.unsafe(LifecycleStage.Solve, "validation-solver")
    )
    val inner = acceptedModel(
      NestedFoldPlan.from(
        Vector(
          acceptedModel(
            FoldSplit.from(
              SplitIdentity.unsafe(s"$name-inner"),
              IndexSet.unsafe(Vector(0, 1)),
              IndexSet.unsafe(Vector(2)),
              totalRows = 3
            )
          )
        )
      )
    )
    acceptedModel(
      ModelSpec.from(
        ModelSpecId.unsafe(name),
        preprocessing,
        MissingnessPolicy.RejectNonFinite,
        plans,
        candidates,
        SelectionDirection.Minimize,
        inner,
        unusedPipeline,
        unusedScorer,
        unusedTransformer,
        ModelSolverPolicy.unsafe(
          "validation-solver",
          Set.empty,
          Set(OptimizationClaimClass.CoordinatewiseStationary)
        ),
        DeterministicSeed(20260721)
      )
    )

  private def candidate(name: String, rank: Double, penalty: Double, lossWeight: Double): HyperparameterCandidate =
    acceptedModel(
      HyperparameterCandidate.from(
        CandidateId.unsafe(name),
        Vector("rank" -> rank, "penalty" -> penalty, "lossWeight" -> lossWeight)
      )
    )

  private val unusedPipeline: FoldPipeline = new FoldPipeline:
    def fit(
        context: TrainingContext,
        training: ProcessedStudy,
        candidate: HyperparameterCandidate
    ): Either[ModelSpecError, FoldPipelineFit] =
      Left(ModelSpecError.InvalidDefinition("validation-plan fixture does not execute its pipeline"))

  private val unusedScorer: ValidationScorer = new ValidationScorer:
    def score(
        fitted: FoldPipelineFit,
        validation: ProcessedStudy,
        candidate: HyperparameterCandidate
    ): Either[ModelSpecError, Double] =
      Left(ModelSpecError.InvalidDefinition("validation-plan fixture does not execute its scorer"))

  private val unusedTransformer: ModelTransformer = new ModelTransformer:
    def transform(
        fitted: FoldPipelineFit,
        study: ProcessedStudy
    ): Either[ModelSpecError, PipelineTransformation] =
      Left(ModelSpecError.InvalidDefinition("validation-plan fixture does not execute its transformer"))

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    GaleNumerics.matrixFromRows(rows)

  private def id(value: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(value))

  private def accepted[A](result: Either[RecoveryValidationError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def acceptedModel[A](result: Either[ModelSpecError, A]): A =
    result.fold(error => fail(error.message), identity)
