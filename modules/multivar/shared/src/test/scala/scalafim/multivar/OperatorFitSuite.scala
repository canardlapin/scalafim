package scalafim.multivar

import gale.linalg.DMat

class OperatorFitSuite extends munit.FunSuite:
  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.cols do
        assertEqualsDouble(actual(row, column), expected(row, column), tolerance)
        column += 1
      row += 1

  test("operator snapshots round-trip action only through matching typed ports and role") {
    val features = SpaceRef(MvSpace.of("snapshot-features", SpaceRole.Observed, 2).toOption.get)
    val components = SpaceRef(MvSpace.of("snapshot-components", SpaceRole.Latent, 1).toOption.get)
    val values = GaleNumerics.matrixFromRows(Vector(Vector(2.0), Vector(-1.0)))
    val frame = Op.fromDense(
      values,
      CoordinateEvidence.primal(components.evidence),
      CoordinateEvidence.dual(features.evidence),
      OperatorRoleWitness.frame,
      ValueIdentity.source(ValueId.unsafe("snapshot-frame"))
    ).toOption.get
    val snapshot = OperatorSnapshot
      .from("frame", DerivedOperatorKind.FunctionalFrame, frame)
      .toOption
      .get
    val lifted = snapshot
      .lift(
        CoordinateEvidence.primal(components.evidence),
        CoordinateEvidence.dual(features.evidence),
        OperatorRoleWitness.frame
      )
      .toOption
      .get

    assertEquals(snapshot.domain, frame.domain.descriptor)
    assertEquals(snapshot.codomain, frame.codomain.descriptor)
    assertEquals(snapshot.role, OperatorRole.Frame)
    assertEquals(snapshot.evidence, EvidenceStatus.Unchecked)
    assertMatrixClose(lifted.toDense.toOption.get, values, 0.0)
    assert(snapshot.lift(
      CoordinateEvidence.primal(components.evidence),
      CoordinateEvidence.dual(features.evidence),
      OperatorRoleWitness.axis
    ).isLeft)
  }

  test("operator fit bundle exposes parameter assignments, result semantics, diagnostics, and provenance") {
    val x = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.0, 1.0),
          Vector(-1.0, 0.0),
          Vector(0.0, -1.0)
        )
      )
    )
    val rows = MvSpace.of("bundle-rows", SpaceRole.Samples, x.rows).toOption.get
    val features = MvSpace.of("bundle-features", SpaceRole.Observed, x.cols).toOption.get
    val preprocessor = PreprocessSpec.Pass.fit(x).toOption.get
    val problem = DynamicGpcaProblem.from(
      x,
      x,
      preprocessor,
      rows,
      features,
      MvMetric.identity(x.rows, Some(rows)).toOption.get,
      MvMetric.identity(x.cols, Some(features)).toOption.get,
      ValueIdentity.source(ValueId.unsafe("bundle-source")),
      SemanticProvenance.source("bundle-fixture")
    ).toOption.get
    val fit = problem.fit(ComponentCount.unsafe(1)).toOption.get
    val axes = OperatorSnapshot
      .from("axes", DerivedOperatorKind.Axes, fit.axes.get)
      .toOption
      .get
    val residual = FitDiagnostic.from("generalized-residual", fit.diagnostics.generalizedResidual, Some(1e-8)).toOption.get
    val bundle = OperatorFitBundle
      .from(fit.programFit, Vector(axes), Vector(residual), fit.provenance)
      .toOption
      .get

    assertEquals(bundle.parameterFrames.length, 1)
    assertEquals(bundle.parameterFrames.head.role, OperatorRole.Frame)
    assertEquals(bundle.operator("axes").map(_.role), Some(OperatorRole.Axis))
    assertEquals(bundle.resultSemantics, fit.programFit.program.resultSemantics)
    assertEquals(bundle.diagnostics.map(_.name), Vector("generalized-residual"))
    assertEquals(bundle.provenance, fit.provenance)
  }
