package scalafim.multivar
package workflow

import scalafim.multivar.core.*
import scalafim.multivar.contract.*
import scalafim.multivar.optimization.*
import scalafim.multivar.solver.*
import scalafim.multivar.workflow.*

import gale.linalg.DMat

class ModelSpecSuite extends munit.FunSuite:

  test("nested ModelSpec tuning matches a hand-executed fold pipeline and selects deterministically"):
    val fixture = modelFixture()
    val fit = accepted(fixture.spec.fit(fixture.study, fixture.outer))

    fit.selection.candidates.foreach: evaluation =>
      val expected = fixture.inner.folds.map: fold =>
        handScore(fixture.study, fold, evaluation.candidate)
      assertEquals(evaluation.folds.length, expected.length)
      evaluation.folds.zip(expected).foreach: (actual, reference) =>
        assertEqualsDouble(actual.score, reference, 1e-8)
      assertEqualsDouble(evaluation.meanScore, expected.sum / expected.length.toDouble, 1e-8)

    val handSelected = fit.selection.candidates.maxBy(_.meanScore).candidate
    assertEquals(fit.selection.selected, handSelected)
    assertEquals(fit.selection.selected.id, CandidateId.unsafe("two-components"))

  test("every learned stage is fitted only on its declared training rows"):
    val fixture = modelFixture()
    val fit = accepted(fixture.spec.fit(fixture.study, fixture.outer))
    val heldOut = fixture.outer.validation.indices.map(fixture.study.rowIds).toSet
    val learnedStages = Set(
      LifecycleStage.Preprocessing,
      LifecycleStage.AlignmentEstimation,
      LifecycleStage.ChartEstimation,
      LifecycleStage.GraphEstimation,
      LifecycleStage.StatisticalEstimation,
      LifecycleStage.OperatorPolicy,
      LifecycleStage.ProgramBuild,
      LifecycleStage.Lowering,
      LifecycleStage.Solve
    )

    assert(fit.lifecycleEvents.nonEmpty)
    assert(fit.lifecycleEvents.filter(event => learnedStages.contains(event.stage)).forall: event =>
      event.action == LifecycleAction.Fit && event.appliedRows == event.trainingRows &&
        event.trainingRows.toSet.intersect(heldOut).isEmpty
    )
    assert(fit.lifecycleEvents.exists(_.stage == LifecycleStage.AlignmentEstimation))
    assert(fit.lifecycleEvents.exists(_.stage == LifecycleStage.ChartEstimation))
    assert(fit.lifecycleEvents.exists(_.stage == LifecycleStage.GraphEstimation))
    assert(fit.selection.candidates.flatMap(_.folds).forall(_.audit.valid))

  test("split identities, derived seeds, scores, and selected candidate are reproducible"):
    val fixture = modelFixture()
    val first = accepted(fixture.spec.fit(fixture.study, fixture.outer))
    val second = accepted(fixture.spec.fit(fixture.study, fixture.outer))

    assertEquals(first.selection.selected, second.selection.selected)
    assertEquals(
      first.selection.candidates.flatMap(_.folds.map(fold => (fold.split, fold.seed, fold.score))),
      second.selection.candidates.flatMap(_.folds.map(fold => (fold.split, fold.seed, fold.score)))
    )
    assertEquals(first.lifecycleEvents.map(event => (event.split, event.seed)), second.lifecycleEvents.map(event => (event.split, event.seed)))

  test("fitted transformation rejects foreign nominal spaces and feature identities"):
    val fixture = modelFixture()
    val fit = accepted(fixture.spec.fit(fixture.study, fixture.outer))
    val heldOut = accepted(fixture.study.subset(fixture.outer.validation))
    val scores = accepted(fit.transform(heldOut))
    val foreignIdentity = accepted(
      ModelStudy.from(
        heldOut.values,
        heldOut.rowIds,
        heldOut.featureSpace,
        id("foreign-feature-order"),
        id("foreign-source"),
        SemanticProvenance.source("foreign-identity")
      )
    )
    val foreignSpace = acceptedMultivar(MvSpace.of("foreign-features", SpaceRole.Observed, heldOut.cols))
    val foreignNominal = accepted(
      ModelStudy.from(
        heldOut.values,
        heldOut.rowIds,
        foreignSpace,
        heldOut.featureIdentity,
        id("foreign-nominal-source"),
        SemanticProvenance.source("foreign-space")
      )
    )

    assertEquals(scores.rows, heldOut.rows)
    assertEquals(scores.cols, 2)
    assertEquals(scores.rowIds, heldOut.rowIds)
    assertEquals(scores.inputFeatureSpace, heldOut.featureSpace)
    assertEquals(scores.inputFeatureIdentity, heldOut.featureIdentity)
    assertEquals(scores.outputCoordinates.dimension, 2)
    assert(scores.provenance.events.nonEmpty)
    assertEquals(
      fit.transform(foreignIdentity).left.toOption,
      Some(ModelSpecError.FeatureIdentityMismatch(fit.featureIdentity, foreignIdentity.featureIdentity))
    )
    assertEquals(
      fit.transform(foreignNominal).left.toOption,
      Some(ModelSpecError.IncompatibleFeatureSpace(fit.featureSpace, foreignSpace))
    )

  test("ModelStudy row subsets preserve sparse storage and row order"):
    val rows = Vector(
      Vector(1.0, 0.0, 2.0),
      Vector(0.0, 3.0, 0.0),
      Vector(4.0, 0.0, 5.0)
    )
    val sparse = acceptedMultivar(SparseMatrixView.fromRows(rows))
    val features = acceptedMultivar(MvSpace.of("sparse-modelspec-features", SpaceRole.Observed, 3))
    val study = accepted(
      ModelStudy.from(
        sparse,
        valuesRowIds(rows.length),
        features,
        id("sparse-modelspec-feature-order"),
        id("sparse-modelspec-source"),
        SemanticProvenance.source("sparse-modelspec")
      )
    )
    val subset = accepted(study.subset(IndexSet.from(Vector(2, 0), IndexAxis.Row).toOption.get))

    assertEquals(subset.values.storage, StorageKind.Sparse)
    assertEquals(subset.rowIds, Vector(study.rowIds(2), study.rowIds(0)))
    assertEquals(
      acceptedMultivar(subset.values.toDense(StoragePolicy.AllowDense)).toRows,
      Vector(rows(2), rows(0))
    )

  test("certified ModelFit exposes requested and lowered programs, operators, terms, auxiliaries, achieved guarantee, and provenance"):
    val fixture = modelFixture()
    val fit = accepted(fixture.spec.fit(fixture.study, fixture.outer))

    assert(fit.requestedProgram eq fit.loweredProgram)
    assert(fit.pipeline.fitBundle.programFit.program eq fit.loweredProgram)
    assertEquals(fit.pipeline.activePenalties, fit.requestedProgram.penalties)
    assertEquals(fit.pipeline.activeConstraints, fit.requestedProgram.constraints)
    assertEquals(fit.auxiliaryVariables, Vector.empty)
    assertEquals(fit.pipeline.splitMethod, None)
    assert(fit.effectiveOperators.map(_.label).contains("covariance"))
    assertEquals(fit.achievedGuarantee.claimClass, OptimizationClaimClass.ExactGlobal)
    assertEquals(fit.missingness, MissingnessPolicy.RejectNonFinite)
    assertEquals(fit.lifecyclePlans, lifecyclePlans)
    assertEquals(fit.solverPolicy.artifact, "gale-generalized-eigen")
    assertEquals(fit.solverExecution.artifact, fit.solverPolicy.artifact)
    assertEquals(fit.solverExecution.attestation, fit.pipeline.fitBundle.programFit.solverAttestation)
    assertEquals(fit.solverExecution.settings.toMap.get("components"), Some("2"))
    assert(fit.solverPolicy.accepts(fit.achievedGuarantee))
    assert(fit.provenance.events.nonEmpty)
    assert(fit.pipeline.events.exists(_.stage == LifecycleStage.Solve))

  test("missingness policy rejects non-finite study values before nested fitting"):
    val fixture = modelFixture()
    val dense = acceptedMultivar(fixture.study.values.toDense(StoragePolicy.AllowDense))
    val nonFiniteValues = matrix(
      Vector.tabulate(dense.rows): row =>
        Vector.tabulate(dense.cols): column =>
          if row == 7 && column == 2 then Double.NaN else dense(row, column)
    )
    val nonFinite = accepted(
      ModelStudy.from(
        MatrixView.dense(nonFiniteValues),
        fixture.study.rowIds,
        fixture.study.featureSpace,
        fixture.study.featureIdentity,
        id("modelspec-nonfinite-source"),
        SemanticProvenance.source("modelspec-nonfinite")
      )
    )

    fixture.spec.fit(nonFinite, fixture.outer).left.toOption match
      case Some(ModelSpecError.Multivar(MultivarError.NonFiniteValue("model study", _, value))) =>
        assert(value.isNaN)
      case other => fail(s"expected explicit non-finite rejection, got $other")

  test("solver policy rejects an achieved claim outside the declared acceptance contract"):
    val fixture = modelFixture(acceptedClaims = Set(OptimizationClaimClass.Feasible))

    fixture.spec.fit(fixture.study, fixture.outer).left.toOption match
      case Some(ModelSpecError.InvalidDefinition(reason)) =>
        assert(reason.contains("not accepted by policy gale-generalized-eigen"))
      case other => fail(s"expected solver-policy rejection, got $other")

  test("declared fold-fitted stages must be realized by the pipeline"):
    val fixture = modelFixture(pipeline = new GpcaFoldPipeline)

    fixture.spec.fit(fixture.study, fixture.outer).left.toOption match
      case Some(ModelSpecError.InvalidDefinition(reason)) =>
        assert(reason.contains("declared AlignmentEstimation artifact 'identity-alignment' was not fitted"))
      case other => fail(s"expected lifecycle-plan rejection, got $other")

  test("operator-policy lifecycle events require matching certified policy records"):
    val policyPlan = LifecyclePlan.unsafe(LifecycleStage.OperatorPolicy, "unrecorded-policy")
    val fixture = modelFixture(
      pipeline = new UnrecordedPolicyPipeline,
      plans = lifecyclePlans :+ policyPlan
    )

    fixture.spec.fit(fixture.study, fixture.outer).left.toOption match
      case Some(ModelSpecError.InvalidDefinition(reason)) =>
        assert(reason.contains("declared operator policy 'unrecorded-policy' has no certified policy record"))
      case other => fail(s"expected missing policy-record rejection, got $other")

  test("a pipeline that lies about training rows is rejected before selection"):
    val fixture = modelFixture(pipeline = new LeakyPipeline)
    val result = fixture.spec.fit(fixture.study, fixture.outer)

    result.left.toOption match
      case Some(ModelSpecError.LeakageDetected(violations)) =>
        assert(violations.exists(_.contains("different rows")))
      case other => fail(s"expected leakage rejection, got $other")

  test("a fitted artifact cannot be reused across ModelSpec-minted training scopes"):
    val fixture = modelFixture(pipeline = new ReusedFoldFitPipeline)

    fixture.spec.fit(fixture.study, fixture.outer).left.toOption match
      case Some(ModelSpecError.LeakageDetected(violations)) =>
        assert(violations.exists(_.contains("different ModelSpec-minted training scope")))
      case other => fail(s"expected scoped-provenance rejection, got $other")

  test("an effective operator not derived from the scoped training study is rejected"):
    val fixture = modelFixture(pipeline = new UnboundOperatorPipeline)

    fixture.spec.fit(fixture.study, fixture.outer).left.toOption match
      case Some(ModelSpecError.LeakageDetected(violations)) =>
        assert(violations.exists(_.contains("effective operator provenance")))
      case other => fail(s"expected effective-operator provenance rejection, got $other")

  private final class AuditedGpcaPipeline extends FoldPipeline:
    private val delegate = new GpcaFoldPipeline

    def fit(
        context: TrainingContext,
        training: ProcessedStudy,
        candidate: HyperparameterCandidate
    ): Either[ModelSpecError, FoldPipelineFit] =
      delegate.fit(context, training, candidate).flatMap: base =>
        val learned = Vector(
          LifecycleEvent.fit(context, LifecycleStage.AlignmentEstimation, "identity-alignment", base.provenance),
          LifecycleEvent.fit(context, LifecycleStage.ChartEstimation, "declared-feature-chart", base.provenance),
          LifecycleEvent.fit(context, LifecycleStage.GraphEstimation, "fixed-neighborhood-graph", base.provenance)
        )
        FoldPipelineFit.from(
          context,
          training,
          base.requestedProgram,
          base.loweredProgram,
          base.fitBundle,
          base.operatorPolicies,
          base.effectiveOperators,
          base.auxiliaryVariables,
          base.splitMethod,
          base.solverExecution,
          learned ++ base.events,
          base.provenance
        )

  private final class LeakyPipeline extends FoldPipeline:
    private val delegate = new GpcaFoldPipeline

    def fit(
        context: TrainingContext,
        training: ProcessedStudy,
        candidate: HyperparameterCandidate
    ): Either[ModelSpecError, FoldPipelineFit] =
      delegate.fit(context, training, candidate).flatMap: base =>
        val foreign = ModelRowId.unsafe("validation-row-smuggled")
        val lie = LifecycleEvent(
          LifecycleStage.GraphEstimation,
          LifecycleAction.Fit,
          context.split,
          context.seed,
          context.trainingRows :+ foreign,
          context.trainingRows :+ foreign,
          "leaky-graph",
          base.provenance
        )
        FoldPipelineFit.from(
          context,
          training,
          base.requestedProgram,
          base.loweredProgram,
          base.fitBundle,
          base.operatorPolicies,
          base.effectiveOperators,
          base.auxiliaryVariables,
          base.splitMethod,
          base.solverExecution,
          base.events :+ lie,
          base.provenance
        )

  private final class UnrecordedPolicyPipeline extends FoldPipeline:
    private val delegate = new AuditedGpcaPipeline

    def fit(
        context: TrainingContext,
        training: ProcessedStudy,
        candidate: HyperparameterCandidate
    ): Either[ModelSpecError, FoldPipelineFit] =
      delegate.fit(context, training, candidate).flatMap: base =>
        val event = LifecycleEvent.fit(
          context,
          LifecycleStage.OperatorPolicy,
          "unrecorded-policy",
          base.provenance
        )
        FoldPipelineFit.from(
          context,
          training,
          base.requestedProgram,
          base.loweredProgram,
          base.fitBundle,
          base.operatorPolicies,
          base.effectiveOperators,
          base.auxiliaryVariables,
          base.splitMethod,
          base.solverExecution,
          base.events :+ event,
          base.provenance
        )

  private final class ReusedFoldFitPipeline extends FoldPipeline:
    private val delegate = new AuditedGpcaPipeline
    private var cached = Option.empty[FoldPipelineFit]

    def fit(
        context: TrainingContext,
        training: ProcessedStudy,
        candidate: HyperparameterCandidate
    ): Either[ModelSpecError, FoldPipelineFit] =
      cached match
        case Some(value) => Right(value)
        case None =>
          delegate.fit(context, training, candidate).map: value =>
            cached = Some(value)
            value

  private final class UnboundOperatorPipeline extends FoldPipeline:
    private val delegate = new AuditedGpcaPipeline

    def fit(
        context: TrainingContext,
        training: ProcessedStudy,
        candidate: HyperparameterCandidate
    ): Either[ModelSpecError, FoldPipelineFit] =
      delegate.fit(context, training, candidate).flatMap: base =>
        val feature = SpaceRef(training.featureSpace)
        val foreign = Op
          .fromDense(
            DMat.eye(training.values.cols),
            CoordinateEvidence.dual(feature.evidence),
            CoordinateEvidence.primal(feature.evidence),
            OperatorRoleWitness.covariance,
            id("foreign-effective-operator")
          )
          .toOption
          .get
        val snapshot = OperatorSnapshot
          .from("foreign-effective", DerivedOperatorKind.SecondOrder, foreign)
          .toOption
          .get
        FoldPipelineFit.from(
          context,
          training,
          base.requestedProgram,
          base.loweredProgram,
          base.fitBundle,
          base.operatorPolicies,
          base.effectiveOperators :+ snapshot,
          base.auxiliaryVariables,
          base.splitMethod,
          base.solverExecution,
          base.events,
          base.provenance
        )

  private final case class ModelFixture(
      study: ModelStudy,
      outer: FoldSplit,
      inner: NestedFoldPlan,
      spec: ModelSpec
  )

  private def modelFixture(
      pipeline: FoldPipeline = new AuditedGpcaPipeline,
      acceptedClaims: Set[OptimizationClaimClass] = Set(OptimizationClaimClass.ExactGlobal),
      plans: Vector[LifecyclePlan] = lifecyclePlans
  ): ModelFixture =
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
    val features = acceptedMultivar(MvSpace.of("modelspec-features", SpaceRole.Observed, 3))
    val study = accepted(
      ModelStudy.from(
        MatrixView.dense(values),
        valuesRowIds(values.rows),
        features,
        id("modelspec-feature-order"),
        id("modelspec-source"),
        SemanticProvenance.source("modelspec-fixture")
      )
    )
    val outer = accepted(
      FoldSplit.from(
        SplitIdentity.unsafe("outer-0"),
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
              SplitIdentity.unsafe("inner-a"),
              IndexSet.unsafe(Vector(0, 1, 2, 3)),
              IndexSet.unsafe(Vector(4, 5)),
              study.rows
            )
          ),
          accepted(
            FoldSplit.from(
              SplitIdentity.unsafe("inner-b"),
              IndexSet.unsafe(Vector(2, 3, 4, 5)),
              IndexSet.unsafe(Vector(0, 1)),
              study.rows
            )
          )
        )
      )
    )
    val candidates = Vector(
      accepted(
        HyperparameterCandidate.from(
          CandidateId.unsafe("one-component"),
          Vector("components" -> 1.0)
        )
      ),
      accepted(
        HyperparameterCandidate.from(
          CandidateId.unsafe("two-components"),
          Vector("components" -> 2.0)
        )
      )
    )
    val spec = accepted(
      ModelSpec.from(
        ModelSpecId.unsafe("gpca-nested"),
        PreprocessSpec.Standardize,
        MissingnessPolicy.RejectNonFinite,
        plans,
        candidates,
        SelectionDirection.Maximize,
        inner,
        pipeline,
        GpcaCapturedVariance,
        GpcaFrameTransformer,
        ModelSolverPolicy.unsafe("gale-generalized-eigen", Set.empty, acceptedClaims),
        DeterministicSeed(20260720)
      )
    )
    ModelFixture(study, outer, inner, spec)

  private def handScore(
      study: ModelStudy,
      fold: FoldSplit,
      candidate: HyperparameterCandidate
  ): Double =
    val training = accepted(study.subset(fold.training))
    val validation = accepted(study.subset(fold.validation))
    val preprocessor = acceptedMultivar(PreprocessSpec.Standardize.fit(training.values))
    val processedTraining = acceptedMultivar(preprocessor.transform(training.values))
    val processedValidation = acceptedMultivar(preprocessor.transform(validation.values))
    val denseTraining = acceptedMultivar(processedTraining.toDense(StoragePolicy.AllowDense))
    val covariance = denseTraining.t * denseTraining
    val components = ComponentCount.unsafe(accepted(candidate.value("components")).toInt)
    val rayleigh = acceptedMultivar(
      GeneralizedRayleighRitz.solve(covariance, DMat.eye(covariance.rows), components)
    )
    val scores = acceptedMultivar(processedValidation.rightMultiply(rayleigh.vectors))
    squared(scores) / validation.rows.toDouble

  private def squared(value: DMat): Double =
    var result = 0.0
    var row = 0
    while row < value.rows do
      var column = 0
      while column < value.cols do
        result += value(row, column) * value(row, column)
        column += 1
      row += 1
    result

  private def valuesRowIds(count: Int): Vector[ModelRowId] =
    Vector.tabulate(count)(index => ModelRowId.unsafe(s"row-$index"))

  private def lifecyclePlans: Vector[LifecyclePlan] =
    Vector(
      LifecyclePlan.unsafe(LifecycleStage.AlignmentEstimation, "identity-alignment"),
      LifecyclePlan.unsafe(LifecycleStage.ChartEstimation, "declared-feature-chart"),
      LifecyclePlan.unsafe(LifecycleStage.GraphEstimation, "fixed-neighborhood-graph"),
      LifecyclePlan.unsafe(LifecycleStage.StatisticalEstimation, "gpca-operator-diagram"),
      LifecyclePlan.unsafe(LifecycleStage.ProgramBuild, "gpca-operator-program"),
      LifecyclePlan.unsafe(LifecycleStage.Lowering, "generalized-rayleigh-ritz"),
      LifecyclePlan.unsafe(LifecycleStage.Solve, "gale-generalized-eigen")
    )

  private def accepted[A](value: Either[ModelSpecError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def acceptedMultivar[A](value: Either[MultivarError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def id(value: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(value))
  private def matrix(rows: Vector[Vector[Double]]): DMat = GaleNumerics.matrixFromRows(rows)
