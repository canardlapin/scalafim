package scalafim.fmri.mvpa

import multivar.family.canonical.{ResidualRegularization, TraceRidgeFraction}
import multivar.core.{ValueId, ValueIdentity}

import gale.backend.Backend.given
import gale.linalg.DMat
import gale.linalg.Matrix
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegments, WhiteningMethod, WhiteningPlan}
import scalafim.fmri.fit.{
  DesignMatrix,
  PreparedContrastGeometry,
  ResponseBlock,
  ResponsePreparationPlan,
  RunPartition,
  SelectedTimepointIndices,
  TContrast,
  TemporalNuisanceRank,
  TemporalPreparationScope,
  TrainingRunScope
}
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig}
import resample4s.core.{IndexSpace, Injection}
import scalafim.fmri.mvpa.CanonicalAnalysis.given

class CanonicalEffectMvpaSuite extends munit.FunSuite:

  test("runwise streaming moments agree with batch products and explicit dense projectors"):
    val geometry = iidGeometry()
    val response = responseBlock(runResponses.head)
    val positions = Array(3, 0, 2)
    val streamed = CanonicalMoments.accumulate(response, geometry, positions).toOption.get
    val selected = selectColumns(response.value, positions)
    val design = geometry.preparedDesign.value
    val cross = selected.t * design
    val effectScore = cross * geometry.effectBasis
    val expectedEffect = effectScore * effectScore.t
    val expectedResidual = subtract(selected.t * selected, cross * geometry.inverseXtX * cross.t)
    val xProjector = design * geometry.inverseXtX * design.t
    val effectProjector = design * geometry.effectBasis * geometry.effectBasis.t * design.t
    val explicitEffect = selected.t * effectProjector * selected
    val explicitResidual = selected.t * subtract(DMat.eye(design.rows), xProjector) * selected

    assertMatrixClose(streamed.total, selected.t * selected, 1e-11)
    assertMatrixClose(streamed.responseDesign, cross, 1e-11)
    assertMatrixClose(streamed.effect, expectedEffect, 1e-11)
    assertMatrixClose(streamed.residual, expectedResidual, 1e-11)
    assertMatrixClose(streamed.effect, explicitEffect, 1e-11)
    assertMatrixClose(streamed.residual, explicitResidual, 1e-11)

  test("canonical analysis uses the ordinary typed measurement result and execution receipt"):
    val dataset = canonicalDataset(runResponses)
    val ridge = ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(0.05))
    val result = CanonicalAnalysisTestSupport.canonical(
      dataset,
      Vector(0, 1, 2, 3),
      ridge,
      revision = "canonical-result-shell"
    )
    val estimate = CanonicalAnalysisTestSupport.success(result)

    assertEquals(result.counts.succeeded, 1)
    assertEquals(result.values.length, 1)
    assertEquals(result.receipt.work.succeeded, 1)
    assertEquals(estimate.folds.length, 3)
    assert(estimate.meanHeldOutRoot >= 0.0)
    assert(estimate.canonicalCorrelation >= 0.0 && estimate.canonicalCorrelation <= 1.0)
    assertEquals(estimate.computation.folds, estimate.folds.map(_.receipt))
    assert(estimate.computation.operatorApplications > 0L)

  test("held-out response perturbations change the held-out score but cannot alter its frozen training frame"):
    val regularization = ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(0.05))
    val ordinary = CanonicalAnalysisTestSupport.success(
      CanonicalAnalysisTestSupport.canonical(
        canonicalDataset(runResponses),
        Vector(0, 1, 2, 3),
        regularization,
        revision = "canonical-leakage-ordinary"
      )
    )
    val perturbedRows = runResponses.updated(0, addHeldOutTaskSignal(runResponses.head, 25.0))
    val perturbed = CanonicalAnalysisTestSupport.success(
      CanonicalAnalysisTestSupport.canonical(
        canonicalDataset(perturbedRows),
        Vector(0, 1, 2, 3),
        regularization,
        revision = "canonical-leakage-perturbed"
      )
    )
    val ordinaryFold = foldFor(ordinary, "run-0")
    val perturbedFold = foldFor(perturbed, "run-0")
    val ordinaryFrame = ordinaryFold.trainingFit.functionalFrame.weights.toDense.toOption.get
    val perturbedFrame = perturbedFold.trainingFit.functionalFrame.weights.toDense.toOption.get

    assertMatrixClose(ordinaryFrame, perturbedFrame, 0.0)
    assertEqualsDouble(
      ordinaryFold.trainingFit.regularization.ridgeAmount,
      perturbedFold.trainingFit.regularization.ridgeAmount,
      0.0
    )
    assertNotEquals(ordinaryFold.heldOutRoot, perturbedFold.heldOutRoot)

  test("dataset and geometry schedules reject incomplete or mismatched run structure"):
    val geometry = iidGeometry()
    val schedule = CanonicalGeometrySchedule.stable(geometry).toOption.get
    val response = responseBlock(runResponses.head)
    val neural = neuralAxis(response.voxels, "canonical-structure")
    val first = CanonicalRunInput(
      PartitionId.unsafe("run-0"),
      response,
      schedule,
      neural
    ).toOption.get
    val duplicate = CanonicalEffectDataset(Vector(first, first))
    val smallerNeural = neuralAxis(response.voxels - 1, "canonical-wrong-size")
    val wrongSize = CanonicalRunInput(
      PartitionId.unsafe("run-1"),
      response,
      schedule,
      smallerNeural
    )

    assertEquals(
      CanonicalEffectDataset(Vector(first)).left.toOption,
      Some(FmriEvidenceError.InsufficientPartitions(1))
    )
    duplicate.left.toOption match
      case Some(FmriEvidenceError.DuplicatePartition(partition)) =>
        assertEquals(partition, PartitionId.unsafe("run-0"))
      case other => fail(s"expected duplicate-run failure, got $other")
    assertEquals(
      wrongSize.left.toOption,
      Some(FmriEvidenceError.NeuralCountMismatch(PartitionId.unsafe("run-1"), 3, 4))
    )

  test("response-learned temporal geometry must resolve the exact training-run scope"):
    val geometries = trainingFoldGeometries()
    val complete = CanonicalGeometrySchedule.trainingFolds(geometries).toOption.get
    val incomplete = CanonicalGeometrySchedule.trainingFolds(geometries.take(1)).toOption.get
    val neural = neuralAxis(runResponses.head.head.length, "canonical-fold-scope")
    val completeRuns = runResponses.zipWithIndex.map: (rows, index) =>
      CanonicalRunInput(PartitionId.unsafe(s"run-$index"), responseBlock(rows), complete, neural).toOption.get
    val incompleteRuns = runResponses.zipWithIndex.map: (rows, index) =>
      CanonicalRunInput(PartitionId.unsafe(s"run-$index"), responseBlock(rows), incomplete, neural).toOption.get

    val dataset = CanonicalEffectDataset(completeRuns).toOption.get
    val result = CanonicalAnalysisTestSupport.canonical(
      dataset,
      Vector(0, 1),
      ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(0.05)),
      revision = "canonical-fold-scoped"
    )

    assertEquals(result.counts.failed, 0)
    CanonicalAnalysisTestSupport
      .success(result)
      .folds
      .zipWithIndex
      .foreach: (fold, heldOut) =>
        val expected = (0 until 3).filter(_ != heldOut).toVector
        assertEquals(fold.receipt.training.map(_.value), expected.map(index => s"run-$index"))
    assert(
      CanonicalEffectDataset(incompleteRuns).left.toOption.exists(_.isInstanceOf[FmriEvidenceError.MissingFoldGeometry])
    )

  test("fold-scoped fMRI geometry lowers to the ordinary relation compiler without numerical drift"):
    val geometries = trainingFoldGeometries()
    val schedule = CanonicalGeometrySchedule.trainingFolds(geometries).toOption.get
    val neural = neuralAxis(runResponses.head.head.length, "canonical-migration")
    val runs = runResponses.zipWithIndex.map: (rows, index) =>
      CanonicalRunInput(PartitionId.unsafe(s"run-$index"), responseBlock(rows), schedule, neural).toOption.get
    val dataset = CanonicalEffectDataset(runs).toOption.get
    val partitionAxisRef = AxisRef
      .create(
        AxisId.unsafe("canonical-migration-runs"),
        AxisPurpose.Partitions,
        runs.map(_.partition),
        CoordinateBasis.unsafe("run-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("canonical-migration-suite", "v1")
      )
      .toOption
      .get
    val partitionAxis = PartitionAxis(
      ScientificAxisName.unsafe("runs"),
      partitionAxisRef
    ).toOption.get
    val effects = AxisRef
      .create(
        AxisId.unsafe("canonical-migration-effect"),
        AxisPurpose.Effects,
        Vector(AxisKey.unsafe("task")),
        CoordinateBasis.unsafe("normalized-contrast"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("canonical-migration-suite", "v1")
      )
      .toOption
      .get
    val revisions = partitionAxisRef.keys
      .map: partition =>
        partition -> ValueIdentity.source(ValueId.unsafe(s"response-${partition.value}"))
      .toMap
    val source = RunwiseCanonicalRelations
      .scalar(dataset, partitionAxis, effects, revisions)
      .toOption
      .get
    val validation = RelationValidationDesign
      .leaveOnePartitionOut(
        partitionAxis,
        GeneralizationAxis(ScientificAxisName.unsafe("runs"), partitionAxisRef.identity)
      )
      .toOption
      .get
    val injection = Injection
      .from(
        IArray(0, 1),
        IndexSpace.of(neural.size).toOption.get
      )
      .toOption
      .get
    val measurement = Measurement
      .hardSelection(neural, MeasurementId.unsafe("first-two"), injection)
      .toOption
      .get
    val frame = MeasurementFrame(neural)(
      Vector(MeasurementEntry(measurement, NoRendition))
    ).toOption.get
    val strategy = ExecutionStrategy(
      BackendId.unsafe("canonical-portable"),
      ExecutionRepresentation.SufficientStatistics,
      NumericPrecision.Binary64,
      SolverChoice.NotApplicable,
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Reject,
      FallbackPolicy.forbidden,
      ResultDelivery.Collected
    ).toOption.get
    val regularization = ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(0.05))
    val migrated = Mvpa
      .run(source)(
        validation,
        frame,
        CanonicalAnalysis.canonicalEffect(source, regularization),
        strategy
      )
      .toOption
      .get
    val estimate = migrated.values.head.outcome match
      case MeasurementOutcome.Success(value, _) => value
      case other                                => fail(s"expected canonical relation success, obtained $other")
    assertEquals(estimate.folds.map(_.receipt.heldOut.value), Vector("run-0", "run-1", "run-2"))
    assert(estimate.folds.forall(_.heldOutRoot >= 0.0))
    assert(estimate.folds.forall(_.trainingFit.programFit.program.objective.label == "generalized-rayleigh"))
    assert(estimate.meanHeldOutRoot >= 0.0)
    val preparationIdentities = validation.folds.map: fold =>
      val relations = source.relationsFor(fold.heldOut).toOption.get
      relations.relation(relations.partitionKeys.head).toOption.get.receipt.preparation.fingerprint
    assertEquals(preparationIdentities.distinct.length, 3)

  test("the canonical analysis surface requires no trialwise beta or TrialReadout artifact"):
    val payload = CanonicalAnalysisTestSupport.success(
      CanonicalAnalysisTestSupport.canonical(
        canonicalDataset(runResponses),
        Vector(0, 1),
        ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(0.05)),
        revision = "canonical-no-trial-beta"
      )
    )

    assert(payload.computation.operatorApplications > 0L)
    assert(payload.folds.forall(_.trainingFit.programFit.program.objective.label == "generalized-rayleigh"))

  private def canonicalDataset(
      responses: Vector[Vector[Vector[Double]]]
  ): CanonicalEffectDataset[? <: multivar.core.SemanticSpace, FeatureId] =
    val geometry = iidGeometry()
    val schedule = CanonicalGeometrySchedule.stable(geometry).toOption.get
    val neural = neuralAxis(responses.head.head.length, "canonical-dataset")
    val runs = responses.zipWithIndex.map: (rows, index) =>
      CanonicalRunInput(
        PartitionId.unsafe(s"run-$index"),
        responseBlock(rows),
        schedule,
        neural
      ).toOption.get
    CanonicalEffectDataset(runs).toOption.get

  private def neuralAxis(size: Int, revision: String): AxisRef[FeatureId] =
    AxisRef
      .create(
        AxisId.unsafe(s"$revision-neural"),
        AxisPurpose.NeuralFeatures,
        Vector.tabulate(size)(index => FeatureId.unsafe(s"voxel-$index")),
        CoordinateBasis.unsafe("voxel-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("canonical-effect-suite", revision)
      )
      .toOption
      .get

  private def iidGeometry(): PreparedContrastGeometry =
    val design = DesignMatrix.unsafe(fromRows(designRows))
    ResponsePreparationPlan
      .fromConfig(FitConfig())
      .prepareContrast(
        design = design,
        columnNames = Vector("intercept", "task", "drift"),
        contrast = TContrast("task", Map("task" -> 1.0)),
        selectedTimepoints = SelectedTimepointIndices.unsafe(designRows.indices.toVector),
        partitions = Vector(RunPartition(0, designRows.indices.toVector, designRows.indices.toVector)),
        nuisanceRank = TemporalNuisanceRank.unsafe(2)
      )
      .toOption
      .get

  private def trainingFoldGeometries(): Vector[PreparedContrastGeometry] =
    val design = DesignMatrix.unsafe(fromRows(designRows))
    val partitions = Vector(RunPartition(0, designRows.indices.toVector, designRows.indices.toVector))
    val options = ArOptions(structure = ArStructure.Ar(1), global = true)
    val segments = TimeSegments.continuous(design.timepoints)
    val whitening = WhiteningPlan.global(
      ArmaCoefficients(Vector(0.2)),
      segments,
      exactFirstAr1 = true,
      method = WhiteningMethod.Estimated
    )
    Vector(Vector(1, 2), Vector(0, 2), Vector(0, 1)).map: training =>
      val scope = TrainingRunScope.fromInts(training).toOption.get
      ResponsePreparationPlan
        .fromConfig(FitConfig(autocorrelation = options))
        .prepareContrast(
          design = design,
          columnNames = Vector("intercept", "task", "drift"),
          contrast = TContrast("task", Map("task" -> 1.0)),
          selectedTimepoints = SelectedTimepointIndices.unsafe(designRows.indices.toVector),
          partitions = partitions,
          nuisanceRank = TemporalNuisanceRank.unsafe(2),
          scope = TemporalPreparationScope.TrainingFold(scope),
          whitening = scalafim.fmri.fit.CanonicalTemporalWhitening.Shared(whitening)
        )
        .toOption
        .get

  private def foldFor(payload: CanonicalEffectEstimate, heldOut: String): CanonicalEffectFoldEstimate =
    payload.folds.find(_.receipt.heldOut.value == heldOut).getOrElse(fail(s"missing fold for $heldOut"))

  private def addHeldOutTaskSignal(rows: Vector[Vector[Double]], amount: Double): Vector[Vector[Double]] =
    rows
      .zip(designRows)
      .map: (response, design) =>
        response.updated(0, response.head + amount * design(1))

  private val designRows = Vector(
    Vector(1.0, -1.0, -1.0),
    Vector(1.0, -1.0, -0.7),
    Vector(1.0, 1.0, -0.4),
    Vector(1.0, 1.0, -0.1),
    Vector(1.0, -1.0, 0.1),
    Vector(1.0, -1.0, 0.4),
    Vector(1.0, 1.0, 0.7),
    Vector(1.0, 1.0, 1.0)
  )

  private val baseResponse = Vector(
    Vector(-0.9, 0.1, -0.15, -0.2),
    Vector(-1.14, 0.5, -0.09, -0.5),
    Vector(0.91, -0.85, 0.21, 0.5),
    Vector(0.67, -0.55, 0.47, 0.2),
    Vector(-1.02, 0.75, -0.47, -0.1),
    Vector(-0.56, 0.65, -0.56, -0.4),
    Vector(0.99, -0.2, 0.49, 0.6),
    Vector(1.05, -0.4, 0.1, 0.3)
  )

  private val runResponses: Vector[Vector[Vector[Double]]] =
    Vector.tabulate(3): run =>
      baseResponse.zipWithIndex.map: (row, time) =>
        row.zipWithIndex.map: (value, feature) =>
          value + 0.03 * run.toDouble * ((time + feature) % 3 - 1).toDouble

  private def responseBlock(rows: Vector[Vector[Double]]): ResponseBlock =
    ResponseBlock.unsafe(fromRows(rows))

  private def selectColumns(matrix: DMat, positions: Array[Int]): DMat =
    val out = Matrix.newBuilder(matrix.rows, positions.length)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < positions.length do
        out(row, col) = matrix(row, positions(col))
        col += 1
      row += 1
    out.result()

  private def subtract(left: DMat, right: DMat): DMat =
    val out = Matrix.newBuilder(left.rows, left.cols)
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        out(row, col) = left(row, col) - right(row, col)
        col += 1
      row += 1
    out.result()

  private def fromRows(rows: Seq[Seq[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((row, col) => rows(row)(col))

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1
