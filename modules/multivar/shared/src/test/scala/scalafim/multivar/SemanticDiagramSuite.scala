package scalafim.multivar

import scala.compiletime.testing.typeCheckErrors

import gale.linalg.DMat
import gale.linalg.DVec

class SemanticDiagramSuite extends munit.FunSuite:
  private def accepted[A](result: Either[DiagramError, A]): A =
    result.fold(error => fail(error.message), value => value)

  private def acceptedSemantic[A](result: Either[SemanticError, A]): A =
    result.fold(error => fail(error.message), value => value)

  private def acceptedMv[A](result: Either[MultivarError, A]): A =
    result.fold(error => fail(error.message), value => value)

  private def ref(id: String, role: SpaceRole, dimension: Int): SpaceRef =
    SpaceRef(MvSpace(SpaceId.unsafe(id), role, Dimension.unsafe(dimension)))

  private def identity(id: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(id))

  private def table[Rows <: SemanticSpace, Columns <: SemanticSpace](
      matrix: DMat,
      rows: SpaceEvidence[Rows],
      columns: SpaceEvidence[Columns],
      id: String
  ): Table[Rows, Columns] =
    acceptedSemantic(
      Table.fromMatrixView(DenseMatrixView(matrix), rows, columns, identity(id))
    )

  private def metric[S <: SemanticSpace](
      space: SpaceEvidence[S],
      legacy: MetricSpec,
      id: String
  ): MetricForm[S, CertifiedSpd] =
    val operator = acceptedSemantic(FormOperator.primal(legacy, space, identity(id)))
    val certificate = acceptedSemantic(FormCertificates.spd(operator))
    acceptedSemantic(Form.metric(operator, space, certificate))

  private def semiMetric[S <: SemanticSpace](
      space: SpaceEvidence[S],
      legacy: MetricSpec,
      id: String
  ): SemiMetric[S, CertifiedPsd] =
    val operator = acceptedSemantic(FormOperator.primal(legacy, space, identity(id)))
    val certificate = acceptedSemantic(FormCertificates.psd(operator))
    acceptedSemantic(Form.semiMetric(operator, space, certificate))

  private def identityMetric[S <: SemanticSpace](
      space: SpaceEvidence[S],
      id: String
  ): MetricForm[S, CertifiedSpd] =
    metric(space, acceptedMv(MetricSpec.identity(space.dimension, Some(space.descriptor))), id)

  private def assertMatrix(actual: DMat, expected: DMat, tolerance: Double = 1e-9): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1

  private def weightedMeans(matrix: DMat, weights: DVec): Vector[Double] =
    Vector.tabulate(matrix.cols) { col =>
      var value = 0.0
      var row = 0
      while row < matrix.rows do
        value += weights(row) * matrix(row, col)
        row += 1
      value
    }

  test("row measures normalize probability mass and remain distinct from row geometry") {
    val rows = ref("measure.rows", SpaceRole.Samples, 3)
    val measure = accepted(
      RowMeasure.fromWeights(
        rows.evidence,
        DVec.fromSeq(Seq(2.0, 3.0, 5.0)),
        identity("measure.weights")
      )
    )

    assertEquals(measure.weights.toVector, Vector(0.2, 0.3, 0.5))
    assertEquals(measure.descriptor.space, rows.descriptor)
    assertEquals(measure.descriptor.normalization, RowMeasureNormalization.UnitMass(10.0))
    assert(
      RowMeasure
        .fromWeights(rows.evidence, DVec.fromSeq(Seq(1.0, -1.0, 1.0)), identity("bad.measure"))
        .isLeft
    )

    val geometry = identityMetric(rows.evidence, "measure.geometry")
    assertNotEquals(measure.descriptor.valueIdentity, geometry.operator.valueIdentity)
  }

  test("measure centering is a certified projection and annihilates weighted means") {
    val rows = ref("center.rows", SpaceRole.Samples, 3)
    val measure = accepted(
      RowMeasure.fromWeights(
        rows.evidence,
        DVec.fromSeq(Seq(1.0, 2.0, 1.0)),
        identity("center.measure")
      )
    )
    val projection = accepted(CenteringProjection.byMeasure(measure))
    val h = projection.matrix

    assertMatrix(GaleNumerics.multiply(h, h), h)
    assert(projection.lawCertificate.idempotenceResidual <= projection.lawCertificate.context.tolerance.threshold(1.0))
    assert(projection.lawCertificate.annihilatesOneResidual <= 1e-10)
    assert(projection.lawCertificate.leftAnnihilationResidual <= 1e-10)

    val raw = GaleNumerics.matrixFromRows(Seq(Seq(1.0, 5.0), Seq(4.0, 2.0), Seq(7.0, -1.0)))
    val centered = accepted(projection.applyTo(DenseMatrixView(raw))).toDense().toOption.get
    weightedMeans(centered, measure.weights).foreach(value => assertEqualsDouble(value, 0.0, 1e-10))
  }

  test("metric-orthogonal centering is distinct from probability centering") {
    val rows = ref("orthogonal.rows", SpaceRole.Samples, 2)
    val dense = GaleNumerics.matrixFromRows(Seq(Seq(2.0, -3.0), Seq(-3.0, 5.0)))
    val rowMetric = metric(
      rows.evidence,
      acceptedMv(MetricSpec.denseSymmetric(dense, MetricValidation.Structural, Some(rows.descriptor))),
      "orthogonal.metric"
    )
    val projection = accepted(CenteringProjection.orthogonal(rowMetric))

    assertEquals(projection.functional.descriptor.kind, RowFunctionalKind.MetricDerived)
    assert(projection.functional.weights.toVector.exists(_ < 0.0))
    assert(projection.lawCertificate.metricSelfAdjointResidual.exists(_ <= 1e-10))
    val h = projection.matrix
    assertMatrix(GaleNumerics.multiply(h.transpose, dense), GaleNumerics.multiply(dense, h))
  }

  test("already-centered evidence is bound to both table and functional") {
    val rows = ref("already.rows", SpaceRole.Samples, 3)
    val columns = ref("already.columns", SpaceRole.Observed, 2)
    type Rows = rows.Id
    type Columns = columns.Id
    val measure = RowMeasure.uniform(rows.evidence, identity("already.measure"))
    val projection = accepted(CenteringProjection.byMeasure(measure))
    val centeredView = accepted(
      projection.applyTo(
        DenseMatrixView(GaleNumerics.matrixFromRows(Seq(Seq(1.0, 2.0), Seq(3.0, 0.0), Seq(5.0, 4.0))))
      )
    )
    val centered: Table[Rows, Columns] = acceptedSemantic(
      Table.fromMatrixView(centeredView, rows.evidence, columns.evidence, identity("already.table"))
    )
    val functional = NormalizedRowFunctional.fromMeasure(measure)
    val certificate = accepted(CenteredDataCertificate.certify(centered, functional))
    val preparation = accepted(
      DiagramPreparation.alreadyCenteredBy(
        functional,
        certificate,
        Some(measure),
        GeometryPolicies.reject
      )
    )
    val core = accepted(
      DiagramCore.from(
        centered,
        DiagramGeometry.metric(identityMetric(rows.evidence, "already.row.metric")),
        DiagramGeometry.metric(identityMetric(columns.evidence, "already.column.metric"))
      )
    )
    val diagram = accepted(
      SemanticDualityDiagram.from(core, preparation, CellDataSemantics.complete)
    )
    val prepared = accepted(PreparedSemanticDiagram.prepare(diagram))

    assert(prepared.evidence.contains(DiagramCertificate.AlreadyCentered(certificate)))
    assertMatrix(prepared.table.toDense().toOption.get, centeredView.toDense().toOption.get)
  }

  test("column transformations and their explicitly induced forms remain separate objects") {
    val columns = ref("transform.columns", SpaceRole.Observed, 2)
    type Columns = columns.Id
    val fullOperator = acceptedSemantic(
      Lin.fromDenseMatrix[Primal[Columns], Primal[Columns]](
        GaleNumerics.matrixFromRows(Seq(Seq(1.0, 1.0), Seq(0.0, 1.0))),
        CoordinateEvidence.primal(columns.evidence),
        CoordinateEvidence.primal(columns.evidence),
        identity("transform.full")
      )
    )
    val full = accepted(ColumnTransformation.fromLin(columns.evidence, fullOperator))
    val inducedFull = accepted(inducedGeometry(full))
    assert(inducedFull.isInstanceOf[InducedColumnGeometry.FullRank[?]])
    assertEquals(inducedFull.relation.formula, "R = M M*")
    assertNotEquals(full.descriptor.valueIdentity, inducedFull.diagramGeometry.operator.valueIdentity)

    val singularOperator = acceptedSemantic(
      Lin.fromDenseMatrix[Primal[Columns], Primal[Columns]](
        GaleNumerics.matrixFromRows(Seq(Seq(1.0, 0.0), Seq(0.0, 0.0))),
        CoordinateEvidence.primal(columns.evidence),
        CoordinateEvidence.primal(columns.evidence),
        identity("transform.singular")
      )
    )
    val singular = accepted(ColumnTransformation.fromLin(columns.evidence, singularOperator))
    assert(accepted(inducedGeometry(singular)).isInstanceOf[InducedColumnGeometry.RankDeficient[?]])

    val errors = typeCheckErrors("""
      import scalafim.multivar.*
      def geometry[S <: SemanticSpace](value: DiagramGeometry[S]): Unit = ()
      val transform: ColumnTransformation[Nothing] = ???
      geometry(transform)
    """)
    assert(errors.nonEmpty)
  }

  test("diagram composition is immutable and records certificate invalidation") {
    val rows = ref("diagram.rows", SpaceRole.Samples, 3)
    val columns = ref("diagram.columns", SpaceRole.Observed, 2)
    val x = table(
      GaleNumerics.matrixFromRows(Seq(Seq(1.0, 2.0), Seq(2.0, 0.0), Seq(3.0, 1.0))),
      rows.evidence,
      columns.evidence,
      "diagram.table"
    )
    val rowGeometry = DiagramGeometry.metric(identityMetric(rows.evidence, "diagram.row.metric"))
    val columnGeometry = DiagramGeometry.metric(identityMetric(columns.evidence, "diagram.column.metric"))
    val core = accepted(DiagramCore.from(x, rowGeometry, columnGeometry))
    val preparation = DiagramPreparation.noCentering[rows.Id](None, GeometryPolicies.reject)
    val original = accepted(SemanticDualityDiagram.from(core, preparation, CellDataSemantics.complete))

    val replacement = DiagramGeometry.metric(
      metric(
        columns.evidence,
        acceptedMv(
          MetricSpec.diagonal(DVec.fromSeq(Seq(2.0, 1.0)), Some(columns.descriptor))
        ),
        "diagram.column.replacement"
      )
    )
    val updated = accepted(original.withColumnGeometry(replacement))

    assertEquals(original.audit.records, Vector.empty)
    assertEquals(updated.audit.records.length, 1)
    assertEquals(
      updated.audit.records.head.certificateEffects.head.validity,
      CertificateValidity.Invalidated
    )
    assertNotEquals(original.core.columnGeometry.operator.valueIdentity, updated.core.columnGeometry.operator.valueIdentity)
  }

  test("singular geometry requires an explicit reject, support, quotient, or ridge policy") {
    val columns = ref("singular.columns", SpaceRole.Observed, 3)
    val legacy = acceptedMv(
      MetricSpec.diagonal(DVec.fromSeq(Seq(4.0, 0.0, 1.0)), Some(columns.descriptor))
    )
    val geometry = DiagramGeometry.semiMetric(semiMetric(columns.evidence, legacy, "singular.metric"))

    val rejected = GeometryResolution.resolve(
      geometry,
      SingularGeometryPolicy.RejectSingularGeometry,
      IndexAxis.Column
    )
    assert(rejected.left.toOption.exists(_.isInstanceOf[DiagramError.SingularGeometryRejected]))

    val threshold = accepted(SupportThreshold(1e-10))
    val restricted = accepted(
      GeometryResolution.resolve(
        geometry,
        SingularGeometryPolicy.RestrictToSupport(threshold),
        IndexAxis.Column
      )
    )
    val support = restricted.support.getOrElse(fail("expected support restriction"))
    assertEquals(restricted.kind, GeometryResolutionKind.Restricted)
    assertEquals(support.discardedNullity, 1)
    assertEquals(support.effectiveSpace.descriptor.size, 2)
    assertEquals(support.reducedMetric.descriptor.positivity, PositivityStatus.Spd)
    assertEquals(support.rankCertificate.runtime.context.tolerance.relative, threshold.toDouble)
    assertMatrix(
      GaleNumerics.multiply(support.restrictionMatrix, support.embeddingMatrix),
      DMat.eye(2)
    )

    val quotient = accepted(
      GeometryResolution.resolve(
        geometry,
        SingularGeometryPolicy.WorkInQuotientSpace(threshold),
        IndexAxis.Column
      )
    )
    assertEquals(quotient.kind, GeometryResolutionKind.Quotient)

    val ridge = accepted(GeometryRidge(1e-3))
    val regularized = accepted(
      GeometryResolution.resolve(
        geometry,
        SingularGeometryPolicy.RegularizeWith(ridge),
        IndexAxis.Column
      )
    )
    assertEquals(regularized.kind, GeometryResolutionKind.Regularized(ridge))
    assert(regularized.certificates.exists(_.context.regularization.contains("ridge=0.001")))
  }

  test("support preparation returns an effective table and requires explicit densification") {
    val rows = ref("support.rows", SpaceRole.Samples, 3)
    val columns = ref("support.columns", SpaceRole.Observed, 3)
    val x = table(
      GaleNumerics.matrixFromRows(Seq(Seq(1.0, 2.0, 3.0), Seq(2.0, 1.0, 0.0), Seq(3.0, 4.0, 1.0))),
      rows.evidence,
      columns.evidence,
      "support.table"
    )
    val columnSemi = semiMetric(
      columns.evidence,
      acceptedMv(
        MetricSpec.diagonal(DVec.fromSeq(Seq(2.0, 0.0, 1.0)), Some(columns.descriptor))
      ),
      "support.column.metric"
    )
    val core = accepted(
      DiagramCore.from(
        x,
        DiagramGeometry.metric(identityMetric(rows.evidence, "support.row.metric")),
        DiagramGeometry.semiMetric(columnSemi)
      )
    )
    val threshold = accepted(SupportThreshold(1e-10))
    val policies = GeometryPolicies(
      SingularGeometryPolicy.RejectSingularGeometry,
      SingularGeometryPolicy.RestrictToSupport(threshold)
    )
    val diagram = accepted(
      SemanticDualityDiagram.from(
        core,
        DiagramPreparation.noCentering[rows.Id](None, policies),
        CellDataSemantics.complete
      )
    )

    assert(PreparedSemanticDiagram.prepare(diagram, StoragePolicy.Operator).isLeft)
    val prepared = accepted(PreparedSemanticDiagram.prepare(diagram, StoragePolicy.AllowDense))
    assertEquals(prepared.table.rows, 3)
    assertEquals(prepared.table.cols, 2)
    assert(prepared.columnSpace.id.value.startsWith("support.columns.support.support.column.metric.r2"))
  }

  test("uniform arithmetic centering exists only as an explicit measure policy") {
    val rows = ref("compat.rows", SpaceRole.Samples, 3)
    val columns = ref("compat.columns", SpaceRole.Observed, 2)
    val raw = GaleNumerics.matrixFromRows(Seq(Seq(1.0, 4.0), Seq(2.0, 1.0), Seq(6.0, 7.0)))
    val x = table(raw, rows.evidence, columns.evidence, "compat.table")
    val measure = RowMeasure.uniform(rows.evidence, identity("compat.uniform"))
    val core = accepted(
      DiagramCore.from(
        x,
        DiagramGeometry.metric(identityMetric(rows.evidence, "compat.row.metric")),
        DiagramGeometry.metric(identityMetric(columns.evidence, "compat.column.metric"))
      )
    )
    val diagram = accepted(
      SemanticDualityDiagram.from(
        core,
        DiagramPreparation.centerByMeasure(measure, GeometryPolicies.reject),
        CellDataSemantics.complete
      )
    )
    val prepared = accepted(PreparedSemanticDiagram.prepare(diagram))
    val legacyCentered = acceptedMv(PreprocessSpec.Center.fit(DenseMatrixView(raw))).transform(DenseMatrixView(raw)).toOption.get

    assertMatrix(prepared.table.toDense().toOption.get, legacyCentered.toDense().toOption.get)
  }

  test("semantic GenPCA consumes the complete prepared diagram and exposes its evidence") {
    val rows = ref("gpca.rows", SpaceRole.Samples, 4)
    val columns = ref("gpca.columns", SpaceRole.Observed, 2)
    val raw = GaleNumerics.matrixFromRows(
      Seq(Seq(1.0, 4.0), Seq(2.0, 1.0), Seq(5.0, 3.0), Seq(7.0, -1.0))
    )
    val x = table(raw, rows.evidence, columns.evidence, "gpca.semantic.table")
    val measure = accepted(
      RowMeasure.fromWeights(
        rows.evidence,
        DVec.fromSeq(Seq(1.0, 2.0, 3.0, 4.0)),
        identity("gpca.measure")
      )
    )
    val core = accepted(
      DiagramCore.from(
        x,
        DiagramGeometry.metric(identityMetric(rows.evidence, "gpca.row.metric")),
        DiagramGeometry.metric(identityMetric(columns.evidence, "gpca.column.metric"))
      )
    )
    val diagram = accepted(
      SemanticDualityDiagram.from(
        core,
        DiagramPreparation.centerByMeasure(measure, GeometryPolicies.reject),
        CellDataSemantics.complete
      )
    )
    val fit = accepted(SemanticGenPca.fit(diagram, ComponentCount.unsafe(2)))
    val preparedDense = fit.preparedDiagram.table.toDense().toOption.get

    weightedMeans(preparedDense, measure.weights).foreach(value => assertEqualsDouble(value, 0.0, 1e-9))
    assert(fit.lawDiagnostics.satisfied)
    assertEquals(fit.result.columnAxes.values.cols, 2)
    assert(fit.evidence.exists(_.isInstanceOf[DiagramCertificate.CenteringLaws]))
    assertEquals(fit.preparedDiagram.audit.records.last.operation, "prepare")

    val errors = typeCheckErrors("""
      import scalafim.multivar.*
      val missing: SemanticDualityDiagram[Nothing, Nothing, MissingCells] = ???
      SemanticGenPca.fit(missing, ComponentCount.unsafe(1))
    """)
    assert(errors.nonEmpty)
  }
