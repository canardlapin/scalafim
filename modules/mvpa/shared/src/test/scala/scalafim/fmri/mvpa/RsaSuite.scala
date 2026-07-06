package scalafim.fmri.mvpa

class RsaSuite extends munit.FunSuite:

  private val threeClassData: PatternMatrix =
    PatternMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 0.0),
        Vector(0.0, 2.0),
        Vector(1.0, 0.0),
        Vector(0.0, 0.0),
        Vector(0.0, 2.0)
      )
    )

  private val threeClassResponse: Response =
    Response.categorical(Vector("b", "a", "c", "b", "a", "c")).toOption.get

  private val allFeatures: FeatureSetPlan =
    FeatureSetPlan.regional("all", Vector(FeatureSet.unsafe(RoiId(1), Vector(0, 1)))).toOption.get

  private val crossnobisData: PatternMatrix =
    PatternMatrix.fromRows(
      Vector(
        Vector(1.0),
        Vector(3.0),
        Vector(1.0),
        Vector(5.0)
      )
    )

  private val crossnobisResponse: Response =
    Response.categorical(Vector("a", "b", "a", "b")).toOption.get

  private val crossnobisFolds: FoldPlan =
    FoldPlan.unsafe(
      Vector(
        Fold.unsafe("p1", Seq(2, 3), Seq(0, 1)),
        Fold.unsafe("p2", Seq(0, 1), Seq(2, 3))
      ),
      samples = 4
    )

  test("RDM analysis can use class-mean rows and store the observed RDM payload") {
    val analysis = RdmAnalysis(RdmMethod.SquaredEuclidean(), rows = RdmRows.ClassMeans)
    val result = MvpaEngine.run(threeClassData, allFeatures, threeClassResponse, analysis, folds = None).toOption.get
    val success = result.successes.head

    assertEquals(success.metrics("Items"), Some(3.0))
    assertEquals(success.metrics("Pairs"), Some(3.0))
    assertEqualsDouble(success.metrics("MeanDistance").get, 10.0 / 3.0, 1e-12)
    success.payload match
      case Some(RoiPayload.Rdm(items, rdm)) =>
        assertEquals(items, Vector("b", "a", "c"))
        assertEquals(rdm.values, Vector(1.0, 5.0, 4.0))
      case other =>
        fail(s"unexpected payload: $other")
  }

  test("crossnobis analysis computes crossvalidated class distances") {
    val plan = FeatureSetPlan.regional("all", Vector(FeatureSet.unsafe(RoiId(1), Vector(0)))).toOption.get
    val analysis = CrossnobisAnalysis(normalizeByFeatures = true, storeRdm = true)
    val result = MvpaEngine.run(crossnobisData, plan, crossnobisResponse, analysis, Some(crossnobisFolds)).toOption.get
    val success = result.successes.head

    assertEqualsDouble(success.metrics("MeanDistance").get, 8.0, 1e-12)
    assertEquals(success.metrics("Folds"), Some(2.0))
    success.payload match
      case Some(RoiPayload.Rdm(items, rdm)) =>
        assertEquals(items, Vector("a", "b"))
        assertEqualsDouble(rdm.values.head, 8.0, 1e-12)
      case other =>
        fail(s"unexpected payload: $other")
  }

  test("Spearman RDM scorer uses average ranks for ties") {
    val observed = RdmVector.unsafe(4, Vector(1.0, 1.0, 2.0, 3.0, 3.0, 4.0))
    val model = RdmVector.unsafe(4, Vector(1.0, 2.0, 2.0, 3.0, 4.0, 4.0))
    val score = RdmScorer.Spearman.score(observed, model).toOption.get

    assertEqualsDouble(score, 10.0 / 11.0, 1e-12)
  }

  test("Spearman RDM scorer reports typed finite and zero-variance errors") {
    val nonFinite = RdmScorer.Spearman.score(
      RdmVector.unsafe(3, Vector(1.0, Double.NaN, 2.0)),
      RdmVector.unsafe(3, Vector(1.0, 2.0, 3.0))
    )
    val zeroVariance = RdmScorer.Spearman.score(
      RdmVector.unsafe(3, Vector(1.0, 1.0, 1.0)),
      RdmVector.unsafe(3, Vector(1.0, 2.0, 3.0))
    )

    assert(nonFinite.swap.toOption.get.message.contains("finite distances"))
    assert(zeroVariance.swap.toOption.get.message.contains("zero-variance"))
  }

  test("partial Pearson RDM scorer aligns labeled controls and residualizes them") {
    val items = Vector("a", "b", "c", "d")
    val observed = RdmVector.unsafe(4, Vector(3.0, 3.0, 6.0, 8.0, 9.0, 13.0))
    val model = RdmVector.unsafe(4, Vector(1.0, -10.0, -9.0, -12.0, -19.0, -14.0))
    val reversedControl = RdmModel.unsafe(
      "trend",
      Vector("d", "c", "b", "a"),
      RdmVector.unsafe(4, Vector(6.0, 5.0, 3.0, 4.0, 2.0, 1.0))
    )
    val scorer = RdmScorer.PartialPearson.unsafe(Vector(reversedControl))
    val score = scorer.score(items, observed, model).toOption.get

    assertEqualsDouble(score, 1.0, 1e-12)
  }

  test("partial Pearson RDM scorer reports typed zero-residual errors") {
    val items = Vector("a", "b", "c", "d")
    val observed = RdmVector.unsafe(4, Vector(3.0, 3.0, 6.0, 8.0, 9.0, 13.0))
    val modelExplainedByControl = RdmVector.unsafe(4, Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0))
    val control = RdmModel.unsafe("trend", items, modelExplainedByControl)
    val scorer = RdmScorer.PartialPearson.unsafe(Vector(control))
    val result = scorer.score(items, observed, modelExplainedByControl)

    assert(result.swap.toOption.get.message.contains("zero-variance residual"))
  }

  test("RSA analysis aligns labeled model RDMs to observed class order") {
    val model = RdmModel
      .unsafe(
        "geometry",
        Vector("a", "b", "c"),
        RdmVector.unsafe(3, Vector(1.0, 4.0, 5.0))
      )
    val analysis = RsaAnalysis(
      method = RdmMethod.SquaredEuclidean(),
      models = Vector(model),
      rows = RdmRows.ClassMeans,
      storeObservedRdm = true
    )
    val result = MvpaEngine.run(threeClassData, allFeatures, threeClassResponse, analysis, folds = None).toOption.get
    val success = result.successes.head

    assertEqualsDouble(success.metrics("geometry.Pearson").get, 1.0, 1e-12)
    success.payload match
      case Some(RoiPayload.Rsa(items, Some(observed), scores)) =>
        assertEquals(items, Vector("b", "a", "c"))
        assertEquals(observed.values, Vector(1.0, 5.0, 4.0))
        assertEquals(scores.map(_.modelName), Vector("geometry"))
        assertEqualsDouble(scores.head.value, 1.0, 1e-12)
      case other =>
        fail(s"unexpected payload: $other")
  }

  test("RSA analysis can use a Spearman scorer") {
    val model = RdmModel
      .unsafe(
        "geometry",
        Vector("a", "b", "c"),
        RdmVector.unsafe(3, Vector(1.0, 4.0, 5.0))
      )
    val analysis = RsaAnalysis(
      method = RdmMethod.SquaredEuclidean(),
      models = Vector(model),
      rows = RdmRows.ClassMeans,
      scorer = RdmScorer.Spearman
    )
    val result = MvpaEngine.run(threeClassData, allFeatures, threeClassResponse, analysis, folds = None).toOption.get
    val success = result.successes.head

    assertEqualsDouble(success.metrics("geometry.Spearman").get, 1.0, 1e-12)
  }

  test("RSA analysis reports a typed error for model item mismatches") {
    val badModel = RdmModel
      .unsafe(
        "wrong",
        Vector("a", "b", "d"),
        RdmVector.unsafe(3, Vector(1.0, 4.0, 5.0))
      )
    val analysis = RsaAnalysis(
      method = RdmMethod.SquaredEuclidean(),
      models = Vector(badModel),
      rows = RdmRows.ClassMeans
    )
    val result = MvpaEngine.run(threeClassData, allFeatures, threeClassResponse, analysis, folds = None).toOption.get

    assertEquals(result.successes.length, 0)
    assertEquals(result.failures.length, 1)
    assert(result.failures.head.error.message.contains("item labels"))
  }
