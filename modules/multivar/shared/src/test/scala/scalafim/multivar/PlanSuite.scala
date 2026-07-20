package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

class PlanSuite extends munit.FunSuite:

  private def inputRef(samples: Int, features: Int): SampleByFeatureInput =
    inputRef("patterns", samples, features)

  private def inputRef(id: String, samples: Int, features: Int): SampleByFeatureInput =
    SampleByFeatureInput.of(
      id,
      samples,
      features,
      MultivarSourceRef.MvpaPatternSource("subject-01/run-1")
    ).toOption.get

  private def roi(id: String, columns: Int*): RoiPlan =
    RoiPlan.of(id, columns).toOption.get

  private def data: MatrixView =
    MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 1.0, 0.0),
          Vector(2.0, 2.0, 1.0),
          Vector(3.0, 3.0, 0.0),
          Vector(4.0, 4.0, 1.0)
        )
      )
    )

  test("MultivarPlan is a pure inspectable ROI/sample-by-feature plan") {
    val rois = RoiPlanSet.of("roi-plan", Vector(roi("left", 0, 1), roi("right", 2)), featureCount = 3).toOption.get
    val plan = MultivarPlan.of(
      "pca-roi",
      inputRef(samples = 4, features = 3),
      rois,
      MultivarEstimator.Pca(ComponentCount(1).toOption.get),
      MultivarExecutionPlan.sparkReadyRoi
    ).toOption.get

    assertEquals(plan.roiCount, 2)
    assert(plan.inspectableSummary.contains("pca"))
    assertEquals(plan.execution.mode, MultivarExecutionMode.DistributedReady)
    assertEquals(plan.execution.partitionAxis, MultivarPartitionAxis.Roi)
    assert(plan.execution.broadcastSmallFits)
  }

  test("PairedMultivarPlan is a pure whole-input paired latent plan") {
    val x = inputRef("x-patterns", samples = 6, features = 3)
    val y = inputRef("y-patterns", samples = 6, features = 2)
    val paired = PairedSampleByFeatureInput.of(x, y).toOption.get
    val estimator = PairedMultivarEstimator.ReducedRankRegression(ComponentCount(2).toOption.get)
    val plan = PairedMultivarPlan.of("rrr-pair", paired, estimator).toOption.get

    assertEquals(plan.input.sampleCount, 6)
    assertEquals(plan.featureScope, PairedFeatureScope.WholeInputPair)
    assertEquals(plan.execution.partitionAxis, MultivarPartitionAxis.WholeInput)
    assert(plan.inspectableSummary.contains("rrr"))
    assert(plan.inspectableSummary.contains("whole-input-pair"))
    assertEquals(plan.estimator.method, PairedLatentMethod.ReducedRankRegression(RegressionDirection.XToY, RegressionRegularization.Ols))
  }

  test("PairedMultivarPlan validates samples, component rank, and execution scope") {
    val x = inputRef("x-patterns", samples = 6, features = 3)
    val y = inputRef("y-patterns", samples = 6, features = 2)
    val shortY = inputRef("short-y", samples = 5, features = 2)
    val paired = PairedSampleByFeatureInput.of(x, y).toOption.get

    PairedSampleByFeatureInput.of(x, shortY) match
      case Left(MultivarError.MatrixShapeMismatch(detail)) =>
        assert(detail.contains("equal samples"), detail)
      case other =>
        fail(s"expected paired sample mismatch, got $other")

    PairedMultivarPlan.of(
      "bad-paired-rank",
      paired,
      PairedMultivarEstimator.Plsc(ComponentCount(3).toOption.get)
    ) match
      case Left(MultivarError.InvalidComponentRequest(requested, limit)) =>
        assertEquals(requested, 3)
        assertEquals(limit, 2)
      case other =>
        fail(s"expected paired rank rejection, got $other")

    PairedMultivarPlan.of(
      "bad-paired-execution",
      paired,
      PairedMultivarEstimator.Plsc(ComponentCount(1).toOption.get),
      execution = MultivarExecutionPlan.roiLocal
    ) match
      case Left(MultivarError.InvalidRowGeometry(detail)) =>
        assert(detail.contains("whole-input"), detail)
      case other =>
        fail(s"expected paired execution rejection, got $other")
  }

  test("PairedMultivarPlan carries typed CCA regularization") {
    val paired = PairedSampleByFeatureInput.of(inputRef("x-patterns", 6, 3), inputRef("y-patterns", 6, 2)).toOption.get
    val regularization = CcaRegularization.asymmetric(1e-4, 1e-3).toOption.get
    val plan = PairedMultivarPlan
      .of("cca-pair", paired, PairedMultivarEstimator.Cca(ComponentCount(2).toOption.get, regularization))
      .toOption
      .get

    plan.estimator.method match
      case PairedLatentMethod.Cca(value) =>
        assertEqualsDouble(value.x.value, 1e-4, 0.0)
        assertEqualsDouble(value.y.value, 1e-3, 0.0)
      case other =>
        fail(s"expected CCA paired method, got $other")
  }

  test("ROI plans validate feature bounds and duplicate ROI ids") {
    val badBounds = RoiPlanSet.of("bad", Vector(roi("x", 0, 3)), featureCount = 3)
    val duplicate = RoiPlanSet.of("dup", Vector(roi("x", 0), roi("x", 1)), featureCount = 3)

    assert(badBounds.swap.toOption.exists {
      case MultivarError.IndexOutOfBounds(IndexAxis.Feature, 3, 3) => true
      case _                                                       => false
    })
    assert(duplicate.swap.toOption.exists {
      case MultivarError.DuplicateBlock(_) => true
      case _                               => false
    })
  }

  test("MultivarPlan re-validates ROI column bounds against its own input feature count") {
    val rois = RoiPlanSet.of("wide", Vector(roi("tail", 5)), featureCount = 6).toOption.get
    val plan = MultivarPlan.of(
      "stale-roi-bounds",
      inputRef(samples = 4, features = 4),
      rois,
      MultivarEstimator.Pca(ComponentCount(1).toOption.get)
    )

    plan match
      case Left(MultivarError.IndexOutOfBounds(IndexAxis.Feature, 5, 4)) => ()
      case other => fail(s"expected ROI bound rejection at plan construction, got $other")
  }

  test("local executor interprets ROI PCA plans without dataset or scheduler dependencies") {
    val rois = RoiPlanSet.of("roi-plan", Vector(roi("pair", 0, 1), roi("single", 2)), featureCount = 3).toOption.get
    val plan = MultivarPlan.of(
      "local-pca",
      inputRef(samples = 4, features = 3),
      rois,
      MultivarEstimator.Pca(ComponentCount(1).toOption.get),
      MultivarExecutionPlan.roiLocal
    ).toOption.get

    val result = LocalMultivarExecutor.run(plan, data).toOption.get

    assertEquals(result.artifacts.length, 2)
    assertEquals(result.artifacts.head.shape.roiId.value, "pair")
    assertEquals(result.artifacts.head.shape.kind, FitArtifactKind.Pca)
    assertEquals(result.artifacts.head.shape.features, 2)
    assertEquals(result.artifacts.head.shape.components, 1)
    assertEquals(result.artifacts.head.shape.source.label, "mvpa:subject-01/run-1")
  }

  test("local executor interprets ROI Nyström plans and preserves kernel artifact shapes") {
    val rois = RoiPlanSet.of("roi-plan", Vector(roi("all", 0, 1, 2)), featureCount = 3).toOption.get
    val plan = MultivarPlan.of(
      "nystrom-roi",
      inputRef(samples = 4, features = 3),
      rois,
      MultivarEstimator.Nystrom(
        ComponentCount(2).toOption.get,
        landmarks = Vector(0, 1, 2, 3),
        kernel = KernelSpec("rbf", Map("gamma" -> 0.25))
      )
    ).toOption.get

    val result = LocalMultivarExecutor.run(plan, data).toOption.get
    val artifact = result.artifacts.head

    assertEquals(artifact.shape.kind, FitArtifactKind.Nystrom)
    assertEquals(artifact.shape.samples, 4)
    assertEquals(artifact.shape.features, 3)
    assertEquals(artifact.shape.components, 2)
    artifact match
      case FitArtifact.KernelArtifact(_, fit) =>
        assertEquals(fit.kernel.name, "rbf")
        assertEquals(fit.landmarks.indices, Vector(0, 1, 2, 3))
      case _ =>
        fail("expected kernel artifact")
  }

  test("local executor interprets ROI GenPCA plans as duality-diagram fits") {
    val rois = RoiPlanSet.of("roi-plan", Vector(roi("pair", 0, 1)), featureCount = 3).toOption.get
    val plan = MultivarPlan.of(
      "local-genpca",
      inputRef(samples = 4, features = 3),
      rois,
      MultivarEstimator.GenPca(
        ComponentCount(1).toOption.get,
        preprocessing = PreprocessSpec.Pass,
        rowMetric = Some(MvMetric.diagonal(DVec.fromSeq(Vector(1.0, 2.0, 1.0, 0.5))).toOption.get),
        columnMetric = Some(MvMetric.diagonal(DVec.fromSeq(Vector(1.0, 0.25))).toOption.get),
        backend = GmdBackend.Eigen(),
        storagePolicy = StoragePolicy.AllowDense
      )
    ).toOption.get

    val result = LocalMultivarExecutor.run(plan, data).toOption.get
    val artifact = result.artifacts.head

    assertEquals(artifact.shape.kind, FitArtifactKind.GenPca)
    artifact match
      case FitArtifact.GenPcaArtifact(_, fit) =>
        assertEquals(fit.projection.map.domain.id.value, "patterns.pair")
        assertEquals(fit.componentCount, 1)
        assertEquals(fit.projection.diagnostics.flatMap(_.backend), Some("eigen"))
        assertEquals(fit.projection.diagnostics.flatMap(_.storagePolicy), Some(StoragePolicy.AllowDense))
      case _ =>
        fail("expected GenPCA artifact")
  }

  test("local executor interprets ROI CPCA plans with ROI-local column constraints and reused row basis") {
    val rois = RoiPlanSet.of("roi-plan", Vector(roi("pair", 0, 1), roi("shifted", 1, 2)), featureCount = 3).toOption.get
    val rowDesign = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 1.0)
      )
    )
    val columnDesign = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0),
        Vector(1.0)
      )
    )
    val spec = CpcaEstimatorSpec(
      blocks = Vector(CpcaBlock.GxH),
      defaultComponents = Some(ComponentCount(1).toOption.get),
      rowConstraint = CpcaConstraint.Basis(rowDesign),
      columnConstraint = CpcaConstraint.Basis(columnDesign)
    )
    val plan = MultivarPlan.of(
      "local-cpca",
      inputRef(samples = 4, features = 3),
      rois,
      MultivarEstimator.Cpca(spec)
    ).toOption.get

    val result = LocalMultivarExecutor.run(plan, data).toOption.get

    assertEquals(result.artifacts.length, 2)
    assertEquals(plan.estimator.kind, FitArtifactKind.Cpca)
    assertEquals(plan.estimator.componentRequestSummary, "GxH:1")

    def assertCpcaArtifact(artifact: FitArtifact, roiId: String, expectedTotalSS: Double): Unit =
      assertEquals(artifact.shape.kind, FitArtifactKind.Cpca)
      assertEquals(artifact.shape.roiId.value, roiId)
      assertEquals(artifact.shape.features, 2)
      assertEquals(artifact.shape.components, 1)
      artifact match
        case FitArtifact.CpcaArtifact(_, fit) =>
          assertEquals(fit.problem.diagram.columnSpace.id.value, s"patterns.$roiId")
          assertEquals(fit.problem.rowConstraint.space.id.value, "patterns")
          assertEquals(fit.problem.rowConstraint.rank, 2)
          assertEquals(fit.problem.columnConstraint.rank, 1)
          assertEquals(fit.block(CpcaBlock.GxH).map(_.rank), Some(1))
          assertEqualsDouble(fit.partition.totalSS, expectedTotalSS, 1e-10)
        case _ =>
          fail("expected CPCA artifact")

    assertCpcaArtifact(result.artifacts(0), "pair", 60.0)
    assertCpcaArtifact(result.artifacts(1), "shifted", 32.0)
  }

  test("estimator component counts are honest options for full or invalid CPCA requests") {
    assertEquals(
      MultivarEstimator.Pca(ComponentCount(2).toOption.get).componentCount.map(_.value),
      Some(2)
    )
    assertEquals(MultivarEstimator.Cpca(CpcaEstimatorSpec()).componentCount, None)
    assertEquals(
      MultivarEstimator.Cpca(CpcaEstimatorSpec(blocks = Vector(CpcaBlock.GxH, CpcaBlock.GxH))).componentCount,
      None
    )
    assertEquals(
      MultivarEstimator.Cpca(
        CpcaEstimatorSpec(defaultComponents = Some(ComponentCount(2).toOption.get))
      ).componentCount.map(_.value),
      Some(2)
    )
  }

  test("plan validation rejects impossible component requests before execution") {
    val rois = RoiPlanSet.of("roi-plan", Vector(roi("single", 2)), featureCount = 3).toOption.get
    val plan = MultivarPlan.of(
      "bad-pca",
      inputRef(samples = 4, features = 3),
      rois,
      MultivarEstimator.Pca(ComponentCount(2).toOption.get)
    )

    assert(plan.swap.toOption.exists {
      case MultivarError.InvalidComponentRequest(2, 1) => true
      case _                                           => false
    })
  }

  test("plan validation rejects impossible CPCA component requests before execution") {
    val rois = RoiPlanSet.of("roi-plan", Vector(roi("pair", 0, 1)), featureCount = 3).toOption.get
    val plan = MultivarPlan.of(
      "bad-cpca-rank",
      inputRef(samples = 4, features = 3),
      rois,
      MultivarEstimator.Cpca(
        CpcaEstimatorSpec(
          defaultComponents = Some(ComponentCount(3).toOption.get)
        )
      )
    )

    assert(plan.swap.toOption.exists {
      case MultivarError.InvalidComponentRequest(3, 2) => true
      case _                                           => false
    })
  }

  test("plan validation rejects empty and structurally impossible CPCA block requests") {
    val rois = RoiPlanSet.of("roi-plan", Vector(roi("pair", 0, 1)), featureCount = 3).toOption.get
    val empty = MultivarPlan.of(
      "bad-cpca-empty",
      inputRef(samples = 4, features = 3),
      rois,
      MultivarEstimator.Cpca(
        CpcaEstimatorSpec(blocks = Vector.empty)
      )
    )
    assert(empty.swap.toOption.exists {
      case MultivarError.InvalidBlockPartition(detail) =>
        detail.contains("at least one block")
      case _ =>
        false
    })

    val zeroRank = MultivarPlan.of(
      "bad-cpca-zero-rank",
      inputRef(samples = 4, features = 3),
      rois,
      MultivarEstimator.Cpca(
        CpcaEstimatorSpec(
          blocks = Vector(CpcaBlock.G0xH),
          defaultComponents = Some(ComponentCount(1).toOption.get)
        )
      )
    )
    assert(zeroRank.swap.toOption.contains(MultivarError.InvalidComponentRequest(1, 0)))
  }

  test("plan validation rejects GenPCA metrics that do not match the duality axes") {
    val rois = RoiPlanSet.of("roi-plan", Vector(roi("pair", 0, 1)), featureCount = 3).toOption.get
    val plan = MultivarPlan.of(
      "bad-genpca",
      inputRef(samples = 4, features = 3),
      rois,
      MultivarEstimator.GenPca(
        ComponentCount(1).toOption.get,
        columnMetric = Some(MvMetric.identity(3).toOption.get)
      )
    )

    assert(plan.swap.toOption.exists {
      case MultivarError.MetricShapeMismatch(IndexAxis.Feature, expected, actual) =>
        expected == 2 && actual == 3
      case _ =>
        false
    })
  }

  test("plan validation rejects CPCA metric, constraint, and block-rank mismatches") {
    val rois = RoiPlanSet.of("roi-plan", Vector(roi("pair", 0, 1)), featureCount = 3).toOption.get
    val rowDesign = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0),
        Vector(1.0),
        Vector(0.0),
        Vector(0.0)
      )
    )
    val wholeFeatureDesign = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0),
        Vector(0.0),
        Vector(1.0)
      )
    )

    val badMetric = MultivarPlan.of(
      "bad-cpca-metric",
      inputRef(samples = 4, features = 3),
      rois,
      MultivarEstimator.Cpca(
        CpcaEstimatorSpec(
          columnMetric = Some(MvMetric.identity(3).toOption.get),
          rowConstraint = CpcaConstraint.Basis(rowDesign)
        )
      )
    )
    assert(badMetric.swap.toOption.exists {
      case MultivarError.MetricShapeMismatch(IndexAxis.Feature, expected, actual) =>
        expected == 2 && actual == 3
      case _ =>
        false
    })

    val badConstraint = MultivarPlan.of(
      "bad-cpca-constraint",
      inputRef(samples = 4, features = 3),
      rois,
      MultivarEstimator.Cpca(
        CpcaEstimatorSpec(
          rowConstraint = CpcaConstraint.Basis(rowDesign),
          columnConstraint = CpcaConstraint.Basis(wholeFeatureDesign)
        )
      )
    )
    assert(badConstraint.swap.toOption.exists {
      case MultivarError.MatrixShapeMismatch(detail) =>
        detail.contains("constraint design has 3 rows") && detail.contains("expected 2")
      case _ =>
        false
    })

    val badBlockRequest = MultivarPlan.of(
      "bad-cpca-block",
      inputRef(samples = 4, features = 3),
      rois,
      MultivarEstimator.Cpca(
        CpcaEstimatorSpec(
          blocks = Vector(CpcaBlock.GxH, CpcaBlock.GxH),
          rowConstraint = CpcaConstraint.Basis(rowDesign)
        )
      )
    )
    assert(badBlockRequest.swap.toOption.exists {
      case MultivarError.InvalidBlockPartition(detail) =>
        detail.contains("GxH")
      case _ =>
        false
    })
  }
