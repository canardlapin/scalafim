package scalafim.multivar
package family.spectral

import scalafim.multivar.core.*
import scalafim.multivar.optimization.*

import gale.linalg.DMat
import gale.linalg.DVec

class GpcaProblemSuite extends munit.FunSuite:

  import GpcaRReferenceFixtures as R

  private def accepted[A](result: Either[DiagramError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def acceptedMv[A](result: Either[MultivarError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def acceptedSemantic[A](result: Either[SemanticError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def ref(id: String, role: SpaceRole, dimension: Int): SpaceRef =
    SpaceRef(MvSpace(SpaceId.unsafe(id), role, Dimension.unsafe(dimension)))

  private def value(id: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(id))

  private def metric[S <: SemanticSpace](
      space: SpaceEvidence[S],
      legacy: MetricSpec,
      id: String
  ): MetricForm[S, CertifiedSpd] =
    val operator = acceptedSemantic(FormOperator.primal(legacy, space, value(id)))
    val certificate = acceptedSemantic(FormCertificates.spd(operator))
    acceptedSemantic(Form.metric(operator, space, certificate))

  private def semiMetric[S <: SemanticSpace](
      space: SpaceEvidence[S],
      legacy: MetricSpec,
      id: String
  ): SemiMetric[S, CertifiedPsd] =
    val operator = acceptedSemantic(FormOperator.primal(legacy, space, value(id)))
    val certificate = acceptedSemantic(FormCertificates.psd(operator))
    acceptedSemantic(Form.semiMetric(operator, space, certificate))

  private def diagram(
      matrix: DMat,
      rowMetric: MetricSpec,
      featureMetric: MetricSpec,
      id: String
  ): SemanticDualityDiagram[? <: SemanticSpace, ? <: SemanticSpace, CompleteCells] =
    val rows = ref(s"$id.rows", SpaceRole.Samples, matrix.rows)
    val features = ref(s"$id.features", SpaceRole.Observed, matrix.cols)
    val table = acceptedSemantic(
      Table.fromMatrixView(DenseMatrixView(matrix), rows.evidence, features.evidence, value(s"$id.table"))
    )
    val core = accepted(
      DiagramCore.from(
        table,
        DiagramGeometry.metric(metric(rows.evidence, rowMetric, s"$id.row-metric")),
        DiagramGeometry.metric(metric(features.evidence, featureMetric, s"$id.feature-metric"))
      )
    )
    accepted(
      SemanticDualityDiagram.from(
        core,
        DiagramPreparation.noCentering(None, GeometryPolicies.reject),
        CellDataSemantics.complete
      )
    )

  test("semantic GPCA executes the operator program and matches the R generalized spectrum"):
    val x = GaleNumerics.matrixFromRows(R.g3X)
    val rowMetric = acceptedMv(MetricSpec.diagonal(DVec.fromSeq(R.g3RowWeights)))
    val featureMetric = acceptedMv(MetricSpec.diagonal(DVec.fromSeq(R.g3ColWeights)))
    val fit = accepted(
      SemanticGpca.fit(
        diagram(x, rowMetric, featureMetric, "gpca-r-g3"),
        ComponentCount.unsafe(3)
      )
    )

    assertVector(fit.operatorResult.singularValues, R.g3Sdev, 1e-8)
    assertMatrix(
      canonicalColumns(fit.operatorResult.axes.get.toDense.toOption.get),
      GaleNumerics.matrixFromRows(R.g3Ov),
      1e-8
    )
    assertEquals(fit.operatorResult.programFit.program.objective.label, "maximize-trace")
    assertEquals(fit.operatorResult.programFit.frames.length, 1)
    assertEquals(fit.operatorResult.diagnostics.retainedRank, 3)
    assert(fit.operatorResult.diagnostics.generalizedResidual <= 1e-8)
    assert(fit.operatorResult.diagnostics.normalizationResidual <= 1e-8)

  test("secondOrder covariance and functional axes have direct dense oracles"):
    val x = GaleNumerics.matrixFromRows(
      Seq(Seq(1.0, 2.0), Seq(3.0, -1.0), Seq(0.5, 4.0), Seq(-2.0, 1.5))
    )
    val rowDense = GaleNumerics.matrixFromRows(
      Seq(
        Seq(2.0, 0.0, 0.0, 0.0),
        Seq(0.0, 1.0, 0.0, 0.0),
        Seq(0.0, 0.0, 3.0, 0.0),
        Seq(0.0, 0.0, 0.0, 0.5)
      )
    )
    val featureDense = GaleNumerics.matrixFromRows(Seq(Seq(2.0, 0.4), Seq(0.4, 1.5)))
    val fit = accepted(
      SemanticGpca.fit(
        diagram(
          x,
          acceptedMv(MetricSpec.denseSymmetric(rowDense)),
          acceptedMv(MetricSpec.denseSymmetric(featureDense)),
          "gpca-oracle"
        ),
        ComponentCount.unsafe(2)
      )
    )

    val covariance = fit.operatorResult.covariance.toDense.toOption.get
    val expectedCovariance = GaleNumerics.multiply(x.t, GaleNumerics.multiply(rowDense, x))
    assertMatrix(covariance, expectedCovariance, 1e-10)

    val expectedAxes = fit.operatorResult.axes.get.toDense.toOption.get
    val derivedAxes = fit.operatorResult.functionalFrame.axes.get.toDense.toOption.get
    assertMatrix(derivedAxes, expectedAxes, 1e-9)

  test("rank loss and repeated roots are reported as numerical identifiability, not invented directions"):
    val repeated = GaleNumerics.matrixFromRows(
      Seq(Seq(2.0, 0.0, 0.0), Seq(0.0, 2.0, 0.0), Seq(0.0, 0.0, 1.0))
    )
    val identityMetric = acceptedMv(MetricSpec.identity(3))
    val repeatedFit = accepted(
      SemanticGpca.fit(
        diagram(repeated, identityMetric, identityMetric, "gpca-repeated"),
        ComponentCount.unsafe(3)
      )
    )

    assertEquals(repeatedFit.operatorResult.diagnostics.spectralClusters.head, Vector(0, 1))
    assertEquals(
      repeatedFit.operatorResult.programFit.program.resultSemantics.equivalence.isInstanceOf[ResultEquivalence.SubspaceEquivalent],
      true
    )

    val rankTwo = GaleNumerics.matrixFromRows(
      Seq(Seq(1.0, 0.0, 1.0), Seq(0.0, 1.0, 1.0), Seq(1.0, 1.0, 2.0), Seq(2.0, -1.0, 1.0))
    )
    val rankRows = acceptedMv(MetricSpec.identity(4))
    val rankFeatures = acceptedMv(MetricSpec.identity(3))
    val rankFit = accepted(
      SemanticGpca.fit(
        diagram(rankTwo, rankRows, rankFeatures, "gpca-rank-two"),
        ComponentCount.unsafe(3)
      )
    )

    assertEquals(rankFit.operatorResult.diagnostics.retainedRank, 2)
    assertEquals(rankFit.operatorResult.singularValues.length, 2)
    assertEquals(rankFit.operatorResult.programFit.identifiability.retainedRank, 2)

  test("singular feature geometry fits only on its explicitly declared support"):
    val x = GaleNumerics.matrixFromRows(
      Seq(Seq(1.0, 2.0, 5.0), Seq(2.0, -1.0, 4.0), Seq(-1.0, 3.0, 3.0), Seq(0.5, 1.5, 2.0))
    )
    val rows = ref("gpca-support.rows", SpaceRole.Samples, 4)
    val features = ref("gpca-support.features", SpaceRole.Observed, 3)
    val table = acceptedSemantic(
      Table.fromMatrixView(DenseMatrixView(x), rows.evidence, features.evidence, value("gpca-support.table"))
    )
    val rowMetric = metric(rows.evidence, acceptedMv(MetricSpec.identity(4)), "gpca-support.row-metric")
    val singular = acceptedMv(MetricSpec.diagonal(DVec.fromSeq(Seq(2.0, 1.0, 0.0))))
    val featureSemiMetric = semiMetric(features.evidence, singular, "gpca-support.feature-semimetric")
    val core = accepted(
      DiagramCore.from(
        table,
        DiagramGeometry.metric(rowMetric),
        DiagramGeometry.semiMetric(featureSemiMetric)
      )
    )
    val preparation = DiagramPreparation.noCentering[rows.Id](
      None,
      GeometryPolicies(
        SingularGeometryPolicy.RejectSingularGeometry,
        SingularGeometryPolicy.RestrictToSupport(SupportThreshold.default)
      )
    )
    val semantic = accepted(SemanticDualityDiagram.from(core, preparation, CellDataSemantics.complete))
    val fit = accepted(SemanticGpca.fit(semantic, ComponentCount.unsafe(2)))

    assertEquals(fit.preparedDiagram.columnResolution.kind, GeometryResolutionKind.Restricted)
    assertEquals(fit.preparedDiagram.columnSpace.size, 2)
    assertEquals(fit.operatorResult.featureMetric.domain.dimension, 2)
    assertEquals(fit.operatorResult.diagnostics.retainedRank, 2)

  private def canonicalColumns(matrix: DMat): DMat =
    val out = matrix.copyData
    var col = 0
    while col < matrix.cols do
      var anchor = 0
      var row = 1
      while row < matrix.rows do
        if Math.abs(out(row * matrix.cols + col)) > Math.abs(out(anchor * matrix.cols + col)) then anchor = row
        row += 1
      if out(anchor * matrix.cols + col) < 0.0 then
        row = 0
        while row < matrix.rows do
          out(row * matrix.cols + col) = -out(row * matrix.cols + col)
          row += 1
      col += 1
    GaleNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)

  private def assertVector(actual: DVec, expected: Vector[Double], tolerance: Double): Unit =
    assertEquals(actual.length, expected.length)
    var index = 0
    while index < actual.length do
      assertEqualsDouble(actual(index), expected(index), tolerance)
      index += 1

  private def assertMatrix(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1
