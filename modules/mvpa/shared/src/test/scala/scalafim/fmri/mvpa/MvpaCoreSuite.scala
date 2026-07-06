package scalafim.fmri.mvpa

class MvpaCoreSuite extends munit.FunSuite:

  private def toyData: PatternMatrix =
    PatternMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0, 3.0),
        Vector(2.0, 3.0, 4.0),
        Vector(3.0, 4.0, 5.0),
        Vector(4.0, 5.0, 6.0)
      )
    )

  private def toyResponse: Response =
    Response.categorical(Vector("a", "a", "b", "b")).toOption.get

  private def meanAnalysis: RoiAnalysis =
    new RoiAnalysis:
      override val name: String = "mean-signal"

      override def evaluate(roi: PatternMatrix, context: RoiContext): Either[MvpaError, RoiAnalysisResult] =
        var sum = 0.0
        var row = 0
        while row < roi.value.rows do
          var col = 0
          while col < roi.value.cols do
            sum += roi.value(row, col)
            col += 1
          row += 1
        Right(RoiAnalysisResult(MetricVector("mean" -> (sum / (roi.value.rows * roi.value.cols)))))

  private final class CountingSource(data: PatternMatrix) extends PatternSource:
    var selections: Int = 0

    override def samples: Int =
      data.samples

    override def featureIndices: Vector[FeatureIndex] =
      data.featureIndices

    override def selectFeatures(featureSet: FeatureSet): Either[MvpaError, PatternMatrix] =
      selections += 1
      data.selectFeatures(featureSet)

  test("leave-one-block-out fold plan preserves held-out blocks") {
    val plan = FoldPlan.leaveOneBlockOut(Vector(1, 1, 2, 2, 3)).toOption.get

    assertEquals(plan.folds.length, 3)
    assertEquals(plan.folds.head.test.map(_.value), Vector(0, 1))
    assertEquals(plan.folds.head.train.map(_.value), Vector(2, 3, 4))
  }

  test("feature sets reject empty and duplicate indices") {
    assert(FeatureSet(RoiId(1), Vector.empty).isLeft)
    assert(FeatureSet(RoiId(1), Vector(1, 1)).isLeft)
    assertEquals(FeatureSet(RoiId(1), Vector(1, 3)).toOption.get.size, 2)
  }

  test("feature set plans build regional and searchlight feature streams") {
    val regional = FeatureSetPlan.fromLabelVector("atlas", Vector(0, 1, 1, 2, 2, 0)).toOption.get
    assertEquals(regional.kind, FeatureSetKind.Region)
    assertEquals(regional.featureSets.map(_.id.value), Vector(1, 2))
    assertEquals(regional.featureSets.head.featureIndices.map(_.value), Vector(1, 2))
    assertEquals(regional.featureSets.head.label, Some("1"))

    val searchlights = FeatureSetPlan.fromNeighborhoods(
      "search",
      Vector(
        1 -> Vector(0, 1, 2),
        4 -> Vector(3, 4, 5)
      )
    ).toOption.get
    assertEquals(searchlights.kind, FeatureSetKind.Searchlight)
    assertEquals(searchlights.featureSets.map(_.center.map(_.value)), Vector(Some(1), Some(4)))
  }

  test("engine runs ROI analysis and records per-ROI feature failures") {
    val good = FeatureSet.unsafe(RoiId(10), Vector(0, 2))
    val missing = FeatureSet.unsafe(RoiId(11), Vector(0, 99))

    val result = MvpaEngine.run(toyData, Vector(good, missing), toyResponse, meanAnalysis).toOption.get

    assertEquals(result.successes.length, 1)
    assertEquals(result.failures.length, 1)
    assertEqualsDouble(result.successes.head.metrics("mean").get, 3.5, 1e-12)
    assertEquals(result.failures.head.id.value, 11)
  }

  test("single ROI task evaluates through a pattern source") {
    val source = PatternSource.fromMatrix(toyData)
    val featureSet = FeatureSet.unsafe(RoiId(10), Vector(0, 2))

    val outcome = MvpaTask.evaluate(source, featureSet, toyResponse, meanAnalysis)

    outcome match
      case RoiOutcome.Success(roiId, features, metrics, payload) =>
        assertEquals(roiId.value, 10)
        assertEquals(features.map(_.value), Vector(0, 2))
        assertEquals(payload, None)
        assertEqualsDouble(metrics("mean").get, 3.5, 1e-12)
      case failure: RoiOutcome.Failure =>
        fail(s"unexpected failure: ${failure.error.message}")
  }

  test("source runner matches the in-memory engine collector") {
    val plan = FeatureSetPlan.regional(
      "two-regions",
      Vector(
        FeatureSet.unsafe(RoiId(1), Vector(0, 1)),
        FeatureSet.unsafe(RoiId(2), Vector(1, 2))
      )
    ).toOption.get
    val source = PatternSource.fromMatrix(toyData)

    val fromMatrix = MvpaEngine.run(toyData, plan, toyResponse, meanAnalysis).toOption.get
    val fromSource = MvpaEngine.runSource(source, plan, toyResponse, meanAnalysis).toOption.get

    assertEquals(fromSource.featureSetPlan.map(_.name), Some("two-regions"))
    assertEquals(fromSource.outcomes, fromMatrix.outcomes)
  }

  test("streaming outcomes match the local engine collector") {
    val plan = FeatureSetPlan.regional(
      "two-regions",
      Vector(
        FeatureSet.unsafe(RoiId(1), Vector(0, 1)),
        FeatureSet.unsafe(RoiId(2), Vector(1, 2))
      )
    ).toOption.get
    val source = PatternSource.fromMatrix(toyData)

    val streamed = MvpaStream.outcomes(source, plan, toyResponse, meanAnalysis).toOption.get.toVector
    val collected = MvpaEngine.runSource(source, plan, toyResponse, meanAnalysis).toOption.get.outcomes

    assertEquals(streamed, collected)
  }

  test("streaming visitor can stop before materializing later feature sets") {
    val plan = FeatureSetPlan.regional(
      "three-regions",
      Vector(
        FeatureSet.unsafe(RoiId(1), Vector(0, 1)),
        FeatureSet.unsafe(RoiId(2), Vector(1, 2)),
        FeatureSet.unsafe(RoiId(3), Vector(0, 2))
      )
    ).toOption.get
    val source = new CountingSource(toyData)
    val seen = Vector.newBuilder[RoiId]

    val result = MvpaStream.foreach(source, plan, toyResponse, meanAnalysis) { outcome =>
      seen += outcome.id
      MvpaStreamControl.Stop
    }

    assert(result.isRight)
    assertEquals(source.selections, 1)
    assertEquals(seen.result().map(_.value), Vector(1))
  }

  test("streaming foldLeft can stop with accumulated state") {
    val plan = FeatureSetPlan.regional(
      "three-regions",
      Vector(
        FeatureSet.unsafe(RoiId(1), Vector(0, 1)),
        FeatureSet.unsafe(RoiId(2), Vector(1, 2)),
        FeatureSet.unsafe(RoiId(3), Vector(0, 2))
      )
    ).toOption.get
    val source = new CountingSource(toyData)

    val result =
      MvpaStream.foldLeft(source, plan, toyResponse, meanAnalysis)(Vector.empty[Int]) { (ids, outcome) =>
        val next = ids :+ outcome.id.value
        val control =
          if next.length == 2 then MvpaStreamControl.Stop
          else MvpaStreamControl.Continue
        (next, control)
      }

    assertEquals(result.toOption.get, Vector(1, 2))
    assertEquals(source.selections, 2)
  }

  test("streaming validates response and folds before visiting feature sets") {
    val plan = FeatureSetPlan.regional("two-regions", Vector(FeatureSet.unsafe(RoiId(1), Vector(0, 1)))).toOption.get
    val source = new CountingSource(toyData)
    val badResponse = Response.categorical(Vector("a", "b")).toOption.get
    var visited = false

    val result = MvpaStream.foreach(source, plan, badResponse, meanAnalysis) { _ =>
      visited = true
      MvpaStreamControl.Continue
    }

    assert(result.swap.toOption.get.message.contains("response length mismatch"))
    assertEquals(visited, false)
    assertEquals(source.selections, 0)
  }

  test("streaming emits ROI failures without stopping the stream") {
    val good = FeatureSet.unsafe(RoiId(10), Vector(0, 2))
    val missing = FeatureSet.unsafe(RoiId(11), Vector(0, 99))
    val source = PatternSource.fromMatrix(toyData)

    val outcomes = MvpaStream.outcomes(source, Vector(good, missing, good), toyResponse, meanAnalysis, None).toOption.get.toVector

    assertEquals(outcomes.map(_.id.value), Vector(10, 11, 10))
    assertEquals(outcomes.count(_.isSuccess), 2)
    assertEquals(outcomes.count(outcome => !outcome.isSuccess), 1)
  }

  test("engine records the named feature set plan used for an analysis") {
    val plan = FeatureSetPlan.regional("two-regions", Vector(FeatureSet.unsafe(RoiId(1), Vector(0, 1)))).toOption.get
    val analysis = RdmAnalysis(RdmMethod.SquaredEuclidean())

    val result = MvpaEngine.run(toyData, plan, toyResponse, analysis, folds = None).toOption.get

    assertEquals(result.featureSetPlan.map(_.name), Some("two-regions"))
    assertEquals(result.successes.length, 1)
  }
