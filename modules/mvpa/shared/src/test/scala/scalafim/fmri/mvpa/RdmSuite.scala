package scalafim.fmri.mvpa

import gale.linalg.DMat

class RdmSuite extends munit.FunSuite:

  test("squared Euclidean RDM uses lower-triangle order") {
    val matrix = GaleTestMatrix.fromRows(
      Vector(
        Vector(0.0, 0.0),
        Vector(3.0, 4.0),
        Vector(1.0, 1.0)
      )
    )

    val rdm = Rdm.squaredEuclidean(matrix).toOption.get
    assertEquals(rdm.values.length, 3)
    assertEqualsDouble(rdm.values(0), 25.0, 1e-12)
    assertEqualsDouble(rdm.values(1), 2.0, 1e-12)
    assertEqualsDouble(rdm.values(2), 13.0, 1e-12)

    val normalized = Rdm.squaredEuclidean(matrix, normalizeByFeatures = true).toOption.get
    assertEqualsDouble(normalized.values(0), 12.5, 1e-12)
  }

  test("correlation RDM computes one minus row correlation") {
    val matrix = GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0, 3.0),
        Vector(1.0, 2.0, 3.0),
        Vector(3.0, 2.0, 1.0)
      )
    )

    val rdm = Rdm.correlation(matrix).toOption.get
    assertEqualsDouble(rdm.values(0), 0.0, 1e-12)
    assertEqualsDouble(rdm.values(1), 2.0, 1e-12)
    assertEqualsDouble(rdm.values(2), 2.0, 1e-12)
  }

  test("RDM kernels reject non-finite pattern values") {
    val matrix = GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0),
        Vector(Double.NaN, 3.0)
      )
    )

    val result = Rdm.squaredEuclidean(matrix)
    assert(result.isLeft)
    assert(result.left.toOption.get.message.contains("non-finite"))
  }

  test("partition means builder computes fold-wise class means") {
    val data = PatternMatrix.fromRows(
      Vector(
        Vector(1.0),
        Vector(3.0),
        Vector(1.0),
        Vector(5.0)
      )
    )
    val response = Response.categorical(Vector("a", "b", "a", "b")).toOption.get
    val folds = FoldPlan.unsafe(
      Vector(
        Fold.unsafe("p1", Seq(2, 3), Seq(0, 1)),
        Fold.unsafe("p2", Seq(0, 1), Seq(2, 3))
      ),
      samples = 4
    )

    val partitioned = PartitionMeansBuilder.fromPatterns(data, response, folds).toOption.get
    assertEquals(partitioned.classes.map(_.value), Vector("a", "b"))
    assertEqualsDouble(partitioned.means(0, 0, 0), 1.0, 1e-12)
    assertEqualsDouble(partitioned.means(1, 0, 0), 3.0, 1e-12)
    assertEqualsDouble(partitioned.means(0, 0, 1), 1.0, 1e-12)
    assertEqualsDouble(partitioned.means(1, 0, 1), 5.0, 1e-12)
  }

  test("partition means builder requires every fold to contain every class") {
    val data = PatternMatrix.fromRows(
      Vector(
        Vector(1.0),
        Vector(3.0),
        Vector(1.0),
        Vector(5.0)
      )
    )
    val response = Response.categorical(Vector("a", "b", "a", "b")).toOption.get
    val folds = FoldPlan.unsafe(
      Vector(
        Fold.unsafe("missing-b", Seq(1, 2, 3), Seq(0)),
        Fold.unsafe("complete", Seq(0, 1), Seq(2, 3))
      ),
      samples = 4
    )

    val result = PartitionMeansBuilder.fromPatterns(data, response, folds)
    assert(result.isLeft)
    assert(result.left.toOption.get.message.contains("no samples for class 'b'"))
  }

  test("crossnobis distances use cross-fold second moments") {
    val means = PartitionMeans
      .unsafe(
        conditions = 2,
        features = 1,
        folds = 2,
        data = Array(
          1.0, // fold 0, condition 0
          3.0, // fold 0, condition 1
          1.0, // fold 1, condition 0
          5.0  // fold 1, condition 1
        )
      )

    val rdm = Rdm.crossnobisDistances(means)
    assertEquals(rdm.values.length, 1)
    assertEqualsDouble(rdm.values.head, 8.0, 1e-12)
  }

  test("crossnobis distances keep feature normalization explicit") {
    val means = PartitionMeans
      .unsafe(
        conditions = 2,
        features = 2,
        folds = 2,
        data = Array(
          1.0, 0.0,
          3.0, 0.0,
          1.0, 0.0,
          5.0, 0.0
        )
      )

    val normalized = Rdm.crossnobisDistances(means, normalizeByFeatures = true)
    val raw = Rdm.crossnobisDistances(means, normalizeByFeatures = false)
    assertEqualsDouble(normalized.values.head, 4.0, 1e-12)
    assertEqualsDouble(raw.values.head, 8.0, 1e-12)
  }
