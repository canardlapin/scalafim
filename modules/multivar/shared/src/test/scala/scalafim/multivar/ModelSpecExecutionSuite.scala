package scalafim.multivar

import gale.linalg.DMat

class ModelSpecExecutionSuite extends munit.FunSuite:

  test("exact quadratic GPCA tunes requested and lowered programs entirely inside each fold"):
    val fixture = fixtureFor(
      pipeline = new ExactQuadraticGpcaFoldPipeline,
      candidates = Vector(
        candidate("quadratic-one", Vector("components" -> 1.0, "quadraticWeight" -> 0.05)),
        candidate("quadratic-two", Vector("components" -> 2.0, "quadraticWeight" -> 0.05))
      ),
      plans = Vector(
        LifecyclePlan.unsafe(LifecycleStage.StatisticalEstimation, "quadratic-gpca-statistics"),
        LifecyclePlan.unsafe(LifecycleStage.ProgramBuild, "quadratic-gpca-program"),
        LifecyclePlan.unsafe(LifecycleStage.Lowering, "exact-quadratic-rewrite"),
        LifecyclePlan.unsafe(LifecycleStage.Solve, "gale-exact-quadratic-eigen")
      ),
      solver = ModelSolverPolicy.unsafe(
        "gale-exact-quadratic-eigen",
        Set.empty,
        Set(SolverGuarantee.GlobalSpectralOptimum)
      )
    )
    val fit = accepted(fixture.spec.fit(fixture.study, fixture.outer))
    val heldOut = accepted(fixture.study.subset(fixture.outer.validation))
    val transformed = accepted(fit.transform(heldOut))

    assertEquals(fit.selection.candidates.length, 2)
    assertEquals(fit.selection.selected.id, CandidateId.unsafe("quadratic-two"))
    assert(fit.requestedProgram ne fit.loweredProgram)
    assertEquals(fit.requestedProgram.penalties.length, 1)
    assertEquals(fit.loweredProgram.penalties, Vector.empty)
    assertEquals(fit.guarantee, SolverGuarantee.GlobalSpectralOptimum)
    assertEquals(fit.solverExecution.settings.toMap.get("rewriteExact"), Some("true"))
    assertEquals(fit.solverExecution.settings.toMap.get("components"), Some("2"))
    assertEquals(
      fit.effectiveOperators.map(_.label),
      Vector("effective-numerator", "effective-denominator")
    )
    assert(fit.pipeline.fitBundle.diagnostics.exists(_.name == "exact-rewrite-residual"))
    assertFoldLocal(fit, heldOut.rowIds.toSet)
    assertEquals(transformed.rowIds, heldOut.rowIds)
    assertEquals(transformed.inputFeatureIdentity, heldOut.featureIdentity)
    assertEquals(transformed.outputCoordinates.dimension, 2)

  test("nonnegative constrained canonical tuning retains the cone and stationary-point guarantee"):
    val fixture = fixtureFor(
      pipeline = new NonnegativeCanonicalFoldPipeline,
      candidates = Vector(
        candidate("small-ridge", Vector("ridge" -> 0.05)),
        candidate("large-ridge", Vector("ridge" -> 0.5))
      ),
      plans = Vector(
        LifecyclePlan.unsafe(LifecycleStage.StatisticalEstimation, "constrained-canonical-moments"),
        LifecyclePlan.unsafe(LifecycleStage.ProgramBuild, "nonnegative-canonical-program"),
        LifecyclePlan.unsafe(LifecycleStage.Lowering, "projected-rayleigh-lowering"),
        LifecyclePlan.unsafe(LifecycleStage.Solve, "gale-projected-rayleigh")
      ),
      solver = ModelSolverPolicy.unsafe(
        "gale-projected-rayleigh",
        Set.empty,
        Set(SolverGuarantee.StationaryPoint)
      )
    )
    val fit = accepted(fixture.spec.fit(fixture.study, fixture.outer))
    val heldOut = accepted(fixture.study.subset(fixture.outer.validation))
    val transformed = accepted(fit.transform(heldOut))
    val weights = fit.pipeline.fitBundle.parameterFrames.head.values

    assertEquals(fit.selection.candidates.length, 2)
    assert(fit.requestedProgram eq fit.loweredProgram)
    assertEquals(fit.requestedProgram.constraints.map(_.feasibleSet), Vector(FeasibleSetKind.NonnegativeOrthant))
    assertEquals(fit.guarantee, SolverGuarantee.StationaryPoint)
    assertEquals(fit.solverExecution.settings.toMap.get("constraint"), Some("Nonnegative"))
    assert(fit.pipeline.fitBundle.diagnostics.exists(_.name == "projected-stationarity"))
    assert(fit.pipeline.fitBundle.diagnostics.exists(_.name == "constraint-violation"))
    assert(matrixMinimum(weights) >= -1e-10)
    assertFoldLocal(fit, heldOut.rowIds.toSet)
    assertEquals(transformed.rowIds, heldOut.rowIds)
    assertEquals(transformed.inputFeatureSpace, heldOut.featureSpace)
    assertEquals(transformed.outputCoordinates.dimension, 1)

  private final case class ExecutionFixture(
      study: ModelStudy,
      outer: FoldSplit,
      spec: ModelSpec
  )

  private def fixtureFor(
      pipeline: FoldPipeline,
      candidates: Vector[HyperparameterCandidate],
      plans: Vector[LifecyclePlan],
      solver: ModelSolverPolicy
  ): ExecutionFixture =
    val values = matrix(
      Vector(
        Vector(-2.0, 0.0, 1.0),
        Vector(-1.0, 1.0, 0.0),
        Vector(0.0, -1.0, 2.0),
        Vector(1.0, 0.5, -1.0),
        Vector(2.0, -0.5, 1.5),
        Vector(3.0, 1.5, -0.5),
        Vector(4.0, 0.25, 0.75),
        Vector(5.0, -1.25, 2.5)
      )
    )
    val features = acceptedMultivar(MvSpace.of("modelspec-execution-features", SpaceRole.Observed, values.cols))
    val study = accepted(
      ModelStudy.from(
        MatrixView.dense(values),
        Vector.tabulate(values.rows)(index => ModelRowId.unsafe(s"execution-row-$index")),
        features,
        identity("modelspec-execution-feature-order"),
        identity("modelspec-execution-source"),
        SemanticProvenance.source("modelspec-execution-fixture")
      )
    )
    val outer = accepted(
      FoldSplit.from(
        SplitIdentity.unsafe("execution-outer"),
        IndexSet.unsafe(Vector(0, 1, 2, 3, 4, 5)),
        IndexSet.unsafe(Vector(6, 7)),
        study.rows
      )
    )
    val inner = accepted(
      NestedFoldPlan.from(
        Vector(
          accepted(
            FoldSplit.from(
              SplitIdentity.unsafe("execution-inner-a"),
              IndexSet.unsafe(Vector(0, 1, 2, 3)),
              IndexSet.unsafe(Vector(4, 5)),
              study.rows
            )
          ),
          accepted(
            FoldSplit.from(
              SplitIdentity.unsafe("execution-inner-b"),
              IndexSet.unsafe(Vector(2, 3, 4, 5)),
              IndexSet.unsafe(Vector(0, 1)),
              study.rows
            )
          )
        )
      )
    )
    val spec = accepted(
      ModelSpec.from(
        ModelSpecId.unsafe(s"execution-${solver.artifact}"),
        PreprocessSpec.Standardize,
        MissingnessPolicy.RejectNonFinite,
        plans,
        candidates,
        SelectionDirection.Maximize,
        inner,
        pipeline,
        FittedFrameCapturedVariance,
        FittedFrameTransformer,
        solver,
        DeterministicSeed(20260721)
      )
    )
    ExecutionFixture(study, outer, spec)

  private def assertFoldLocal(fit: ModelFit, outerHeldOut: Set[ModelRowId]): Unit =
    val fittedStages = fit.lifecycleEvents.filter(_.action == LifecycleAction.Fit)
    assert(fittedStages.nonEmpty)
    assert(fittedStages.forall: event =>
      event.appliedRows == event.trainingRows && event.trainingRows.toSet.intersect(outerHeldOut).isEmpty
    )
    assert(fit.selection.candidates.flatMap(_.folds).forall(_.audit.valid))

  private def candidate(
      name: String,
      values: Vector[(String, Double)]
  ): HyperparameterCandidate =
    accepted(HyperparameterCandidate.from(CandidateId.unsafe(name), values))

  private def matrixMinimum(value: DMat): Double =
    var result = Double.PositiveInfinity
    var row = 0
    while row < value.rows do
      var column = 0
      while column < value.cols do
        result = Math.min(result, value(row, column))
        column += 1
      row += 1
    result

  private def accepted[A](value: Either[ModelSpecError, A]): A =
    value.fold(error => fail(error.message), current => current)

  private def acceptedMultivar[A](value: Either[MultivarError, A]): A =
    value.fold(error => fail(error.message), current => current)

  private def identity(name: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(name))

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    GaleNumerics.matrixFromRows(rows)
