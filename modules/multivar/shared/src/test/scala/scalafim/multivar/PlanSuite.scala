package scalafim.multivar

import scalafim.linalg.DoubleMatrix

class PlanSuite extends munit.FunSuite:

  private def inputRef(samples: Int, features: Int): SampleByFeatureInput =
    SampleByFeatureInput.of(
      "patterns",
      samples,
      features,
      MultivarSourceRef.MvpaPatternSource("subject-01/run-1")
    ).toOption.get

  private def roi(id: String, columns: Int*): RoiPlan =
    RoiPlan.of(id, columns).toOption.get

  private def data: MatrixView =
    MatrixView.dense(
      DoubleMatrix.fromRows(
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
    assertEquals(plan.execution.mode, MultivarExecutionMode.SparkReady)
    assertEquals(plan.execution.partitionAxis, MultivarPartitionAxis.Roi)
    assert(plan.execution.broadcastSmallFits)
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

  test("local executor interprets ROI PCA plans without dataset or Spark dependencies") {
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
