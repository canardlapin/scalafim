package scalafim.multivar
package capability

import scalafim.multivar.core.*
import scalafim.multivar.capability.*
import scalafim.multivar.family.spectral.*
import scalafim.multivar.family.paired.*

import gale.linalg.DMat
import gale.linalg.DVec

class FittedProjectionSuite extends munit.FunSuite:

  import ProjectionParityReferenceFixtures as R

  private val featureIds: Vector[FeatureId] =
    Vector("motion", "age", "task", "site").map(FeatureId.unsafe)

  private def fittedPreprocessor: FittedColumnAffine =
    val scale = R.scale.map(value => 1.0 / value)
    val shift = R.center.zip(scale).map((center, weight) => -center * weight)
    FittedColumnAffine(
      R.center.length,
      DVec.fromSeq(scale),
      DVec.fromSeq(shift)
    )

  private def fittedTransform: FittedFrameTransform =
    FittedFrameTransform
      .fromTraining(
        MatrixView.dense(R.newRaw),
        R.weights,
        fittedPreprocessor,
        "projection-fixture",
        ComponentCount.unsafe(2),
        featureIds = Some(featureIds)
      )
      .toOption
      .get

  private def restriction(indices: Vector[Int]): RestrictedFrameTransform[?, ?] =
    fittedTransform
      .restrictFeatures(IndexSet.from(indices, IndexAxis.Feature).toOption.get)
      .toOption
      .get

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double = 1e-12): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1

  private def assertFinite(input: DMat): Unit =
    var row = 0
    while row < input.rows do
      var col = 0
      while col < input.cols do
        assert(input(row, col).isFinite)
        col += 1
      row += 1

  test("all-feature contribution equals full fitted projection"):
    val transform = fittedTransform
    val restricted = restriction(Vector(0, 1, 2, 3))
    val input = restricted.restriction
      .bind(MatrixView.dense(R.newRaw), restricted.restriction.restrictedSchema)
      .toOption
      .get
    val contribution = restricted.contribution(input).toOption.get

    assertMatrixClose(contribution.values, transform.project(MatrixView.dense(R.newRaw)).toOption.get)
    assertEquals(contribution.projectionProvenance.selectedFeatures, featureIds)
    assertEquals(contribution.projectionProvenance.sourceFrame, transform.frame.weights.valueIdentity)

  test("partial contribution and Euclidean recovery match independent base-R fixtures"):
    val restricted = restriction(R.subset)
    val input = restricted.restriction
      .bind(MatrixView.dense(R.partialRaw), restricted.restriction.restrictedSchema)
      .toOption
      .get
    val contribution = restricted.contribution(input).toOption.get
    val recovered = restricted.recoverEuclidean(input, Ridge(R.ridge).toOption.get).toOption.get

    assertMatrixClose(contribution.values, R.partialContribution)
    assertMatrixClose(recovered.values, R.partialLeastSquares)
    assert(contribution.isInstanceOf[PartialFeatureContribution])
    assert(recovered.isInstanceOf[PartialLeastSquaresScores])
    assertEquals(contribution.projectionProvenance.policy, PartialScorePolicy.Contribution)
    recovered.projectionProvenance.policy match
      case PartialScorePolicy.LeastSquares(metric, ridge) =>
        assertEquals(metric.dim, 2)
        assertEqualsDouble(ridge.value, R.ridge, 0.0)
      case other => fail(s"expected least-squares policy, got $other")

  test("generalized feature metric recovery matches the independent dense oracle"):
    val restricted = restriction(R.subset)
    val input = restricted.restriction
      .bind(MatrixView.dense(R.partialRaw), restricted.restriction.restrictedSchema)
      .toOption
      .get
    val metric = MetricSpec
      .denseSymmetric(R.partialMetric, MetricValidation.StrictPsd())
      .toOption
      .get
    val recovered = restricted.recover(input, metric, Ridge(R.ridge).toOption.get).toOption.get

    assertMatrixClose(recovered.values, R.partialMetricLeastSquares)
    assert(
      restricted
        .recover(input, MetricSpec.identity(1).toOption.get, Ridge(R.ridge).toOption.get)
        .isLeft
    )

  test("restriction commutes with fitted preprocessing and preserves declared order"):
    val transform = fittedTransform
    val columns = IndexSet.from(Vector(2, 0), IndexAxis.Feature).toOption.get
    val restricted = transform.restrictFeatures(columns).toOption.get
    val partialRaw = MatrixView.dense(R.newRaw.selectColumns(Vector(2, 0)))
    val fullRestricted = transform.preprocessor.transform(partialRaw, columns = Some(columns)).toOption.get
    val local = restricted.restriction.fittedPreprocessor.transform(partialRaw).toOption.get
    val input = restricted.restriction
      .bind(partialRaw, restricted.restriction.restrictedSchema)
      .toOption
      .get
    val expected = local.rightMultiply(R.weights.selectRows(Vector(2, 0))).toOption.get

    assertMatrixClose(local.toDense(StoragePolicy.AllowDense).toOption.get, fullRestricted.toDense(StoragePolicy.AllowDense).toOption.get)
    assertMatrixClose(restricted.contribution(input).toOption.get.values, expected)
    assertEquals(restricted.restriction.restrictedSchema.identities, Vector(featureIds(2), featureIds(0)))

  test("reordered and foreign feature schemas fail before numerical projection"):
    val restricted = restriction(R.subset)
    val expected = restricted.restriction.restrictedSchema
    val reordered = FeatureSchema
      .from(expected.space, expected.identities.reverse, expected.valueIdentity)
      .toOption
      .get
    val foreign = FeatureSchema
      .from(
        expected.space,
        expected.identities,
        ValueIdentity.source(ValueId.unsafe("foreign-feature-schema"))
      )
      .toOption
      .get
    val reorderedInput = restricted.restriction.bind(MatrixView.dense(R.partialRaw), reordered).toOption.get
    val foreignInput = restricted.restriction.bind(MatrixView.dense(R.partialRaw), foreign).toOption.get

    assert(restricted.restriction.bind(MatrixView.dense(R.partialRaw)).isLeft)
    assert(restricted.contribution(reorderedInput).isLeft)
    assert(restricted.contribution(foreignInput).isLeft)

  test("single-feature rank deficiency requires an explicit positive ridge"):
    val restricted = restriction(Vector(0))
    val raw = MatrixView.dense(R.newRaw.selectColumns(Vector(0)))
    val input = restricted.restriction
      .bind(raw, restricted.restriction.restrictedSchema)
      .toOption
      .get

    assert(restricted.recoverEuclidean(input, Ridge(0.0).toOption.get).isLeft)
    val recovered = restricted.recoverEuclidean(input, Ridge(0.2).toOption.get).toOption.get
    assertEquals(recovered.values.rows, R.newRaw.rows)
    assertEquals(recovered.values.cols, R.weights.cols)
    assertFinite(recovered.values)

  test("sparse partial input stays lazy-affine through fitted preprocessing"):
    val restricted = restriction(R.subset)
    val sparse = SparseMatrixView.fromRows(R.partialRaw.toRows).toOption.get
    val processed = restricted.restriction.fittedPreprocessor.transform(sparse).toOption.get
    val input = restricted.restriction
      .bind(sparse, restricted.restriction.restrictedSchema)
      .toOption
      .get

    assertEquals(processed.storage, StorageKind.LazyAffine)
    assertMatrixClose(restricted.contribution(input).toOption.get.values, R.partialContribution)

  test("policy dispatch retains distinct contribution and least-squares result types"):
    val restricted = restriction(R.subset)
    val input = restricted.restriction
      .bind(MatrixView.dense(R.partialRaw), restricted.restriction.restrictedSchema)
      .toOption
      .get
    val leastSquares = PartialScorePolicy
      .euclideanLeastSquares(R.subset.length, R.ridge)
      .toOption
      .get

    restricted.score(input, PartialScorePolicy.Contribution).toOption.get match
      case _: PartialFeatureContribution => ()
      case other                         => fail(s"expected contribution result, got $other")
    restricted.score(input, leastSquares).toOption.get match
      case _: PartialLeastSquaresScores => ()
      case other                        => fail(s"expected recovered-score result, got $other")

  test("PCA, PLSC, CCA, and RRR expose the same restricted-frame capability"):
    val x = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 0.2, -0.4),
          Vector(0.5, 1.3, 0.7),
          Vector(-0.8, 0.4, 1.1),
          Vector(1.7, -1.0, 0.3),
          Vector(-1.4, -0.6, -0.9),
          Vector(0.2, 0.8, -1.2)
        )
      )
    )
    val y = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(1.58, -0.32),
          Vector(0.82, 1.44),
          Vector(-1.56, 1.07),
          Vector(1.20, -1.70),
          Vector(-1.32, -0.38),
          Vector(1.65, 0.42)
        )
      )
    )
    val componentCount = ComponentCount.unsafe(1)
    val pca = Pca.fit(x, componentCount).toOption.get.transform
    val plsc = Plsc.fit(x, y, componentCount).toOption.get.sourceTransform
    val cca = Cca.fit(x, y, componentCount, ridge = 0.1).toOption.get.sourceTransform
    val rrr = ReducedRankRegression.fit(x, y, componentCount).toOption.get.sourceTransform
    val transforms = Vector(pca, plsc, cca, rrr)
    val column = IndexSet.from(Vector(1), IndexAxis.Feature).toOption.get
    val partial = x.selectColumns(column).toOption.get

    transforms.foreach: transform =>
      val restricted = transform.restrictFeatures(column).toOption.get
      val input = restricted.restriction.bind(partial).toOption.get
      val contribution = restricted.contribution(input).toOption.get
      assertEquals(contribution.values.rows, x.rows)
      assertEquals(contribution.values.cols, 1)
      assertFinite(contribution.values)
