package scalafim.multivar

import scala.compiletime.testing.typeCheckErrors

import gale.linalg.DMat
import gale.linalg.DVec

class PairedOperatorProblemSuite extends munit.FunSuite:

  test("partial row relationships construct the exact typed cross operator"):
    val sourceRows = space("partial-source-rows", SpaceRole.Samples, 3)
    val targetRows = space("partial-target-rows", SpaceRole.Samples, 2)
    val sourceFeatures = space("partial-source-features", SpaceRole.Observed, 2)
    val targetFeatures = space("partial-target-features", SpaceRole.Observed, 1)
    val sourceTable = table(
      sourceRows.evidence,
      sourceFeatures.evidence,
      matrix(Vector(Vector(1.0, 2.0), Vector(3.0, 4.0), Vector(5.0, 6.0))),
      "partial-source-table"
    )
    val targetTable = table(
      targetRows.evidence,
      targetFeatures.evidence,
      matrix(Vector(Vector(7.0), Vector(8.0))),
      "partial-target-table"
    )
    val sourceGeometry = certifiedRowLink(sourceRows.evidence, DMat.eye(3), "partial-source-geometry")
    val targetGeometry = certifiedRowLink(targetRows.evidence, DMat.eye(2), "partial-target-geometry")
    val relationship = rowLink(
      sourceRows.evidence,
      targetRows.evidence,
      matrix(Vector(Vector(1.0, 0.0), Vector(0.0, 0.5), Vector(0.0, 1.0))),
      "partial-relationship"
    )
    val problem = PairedOperatorProblem.fromTables(
      sourceRows.evidence,
      targetRows.evidence,
      sourceFeatures.evidence,
      targetFeatures.evidence,
      sourceTable,
      targetTable,
      sourceGeometry,
      targetGeometry,
      relationship
    )

    assertMatrix(acceptedSemantic(problem.cross.toDense), matrix(Vector(Vector(59.0), Vector(78.0))), 1e-12)
    val fit = accepted(problem.fitPlsc(ComponentCount.unsafe(1), crossScale = 1.0))
    assertEquals(fit.programFit.program.objective.label, "maximize-cross-trace")
    assertEquals(fit.programFit.frames.length, 2)
    assert(fit.diagnostics.crossResidual < 1e-10)
    assert(fit.diagnostics.normalizationResidual < 1e-10)

  test("named paired fits expose one generic program fit and the RRR coefficient role"):
    val plscReference = PairedLatentRReferenceFixtures.plsc
    val plsc = accepted(
      Plsc.fit(
        MatrixView.dense(plscReference.x),
        MatrixView.dense(plscReference.y),
        ComponentCount.unsafe(plscReference.components)
      )
    )
    assertEquals(plsc.operator.programFit.program.objective.label, "maximize-cross-trace")
    assertEquals(plsc.operator.programFit.program.resultSemantics.guarantee, SolverGuarantee.GlobalSpectralOptimum)
    assertEquals(
      plsc.operator.programFit.program.resultSemantics.equivalence,
      ResultEquivalence.FrameEquivalent(FrameSymmetry.Orthogonal, CertificateTolerance.strict)
    )
    assertEquals(plsc.operator.coefficient, None)

    val ccaReference = PairedLatentRReferenceFixtures.cca
    val cca = accepted(
      Cca.fitRegularized(
        MatrixView.dense(ccaReference.x),
        MatrixView.dense(ccaReference.y),
        ComponentCount.unsafe(ccaReference.components),
        CcaRegularization.asymmetric(ccaReference.xRidge, ccaReference.yRidge).toOption.get
      )
    )
    assertEquals(cca.operator.programFit.program.objective.label, "maximize-cross-trace")
    assert(cca.operator.diagnostics.crossResidual < 1e-8)
    assert(cca.operator.diagnostics.normalizationResidual < 1e-8)

    val rrrReference = PairedLatentRReferenceFixtures.rrr
    val rrr = accepted(
      ReducedRankRegression.fit(
        MatrixView.dense(rrrReference.x),
        MatrixView.dense(rrrReference.y),
        ComponentCount.unsafe(rrrReference.components)
      )
    )
    assertEquals(rrr.operator.programFit.program.objective.label, "sequential-cross-regression")
    assert(rrr.operator.programFit.program.resultSemantics.equivalence.isInstanceOf[ResultEquivalence.PredictionEquivalent])
    val coefficient = acceptedSemantic(rrr.operator.coefficient.get.toDense)
    assertEquals(rrr.operator.coefficient.get.role.value, OperatorRole.Coefficient)
    assertMatrix(coefficient, rrr.fullCoefficient, 1e-10)

  test("simultaneous row permutation preserves paired spectra"):
    val reference = PairedLatentRReferenceFixtures.plsc
    val permutation = Vector(4, 0, 3, 1, 2)
    val original = accepted(
      Plsc.fit(
        MatrixView.dense(reference.x),
        MatrixView.dense(reference.y),
        ComponentCount.unsafe(reference.components)
      )
    )
    val permuted = accepted(
      Plsc.fit(
        MatrixView.dense(reference.x.selectRows(permutation)),
        MatrixView.dense(reference.y.selectRows(permutation)),
        ComponentCount.unsafe(reference.components)
      )
    )
    assertVector(original.result.singularValues, permuted.result.singularValues, 1e-10)

  test("PLSC obeys common scale and feature-sign transport laws"):
    val reference = PairedLatentRReferenceFixtures.plsc
    val original = accepted(
      Plsc.fit(
        MatrixView.dense(reference.x),
        MatrixView.dense(reference.y),
        ComponentCount.unsafe(reference.components)
      )
    )
    val transformed = accepted(
      Plsc.fit(
        MatrixView.dense(MatrixOps.scale(flipFirstColumn(reference.x), 2.0)),
        MatrixView.dense(MatrixOps.scale(reference.y, -3.0)),
        ComponentCount.unsafe(reference.components)
      )
    )
    var component = 0
    while component < original.result.singularValues.length do
      assertEqualsDouble(
        transformed.result.singularValues(component),
        6.0 * original.result.singularValues(component),
        1e-9
      )
      component += 1

  test("coefficient orientation is nominal and cannot be substituted for the reverse map"):
    val errors = typeCheckErrors("""
      import scalafim.multivar.*
      val source = SpaceRef(MvSpace(SpaceId.unsafe("source"), SpaceRole.Observed, Dimension.unsafe(2)))
      val target = SpaceRef(MvSpace(SpaceId.unsafe("target"), SpaceRole.Observed, Dimension.unsafe(2)))
      val forward: OpCoefficient[source.Id, target.Id, UncheckedEvidence] = ???
      val reverse: OpCoefficient[target.Id, source.Id, UncheckedEvidence] = forward
    """)
    assert(errors.nonEmpty)

  test("paired lowering refuses implicit lazy or sparse materialization"):
    val reference = PairedLatentRReferenceFixtures.plsc
    val preservedDense = Plsc.fit(
      MatrixView.dense(reference.x),
      MatrixView.dense(reference.y),
      ComponentCount.unsafe(1),
      xPreproc = PreprocessSpec.Pass,
      yPreproc = PreprocessSpec.Pass,
      policy = StoragePolicy.PreserveSparse
    )
    assert(preservedDense.isRight)

    val sparseX = SparseMatrixView.fromRows(reference.x.toRows).toOption.get
    val rejectedSparse = Plsc.fit(
      MatrixView.sparse(sparseX),
      MatrixView.dense(reference.y),
      ComponentCount.unsafe(1),
      xPreproc = PreprocessSpec.Pass,
      yPreproc = PreprocessSpec.Pass,
      policy = StoragePolicy.PreserveSparse
    )
    assert(rejectedSparse.left.exists:
      case MultivarError.DensificationRejected("paired operator sufficient-statistic lowering", StorageKind.Sparse) => true
      case _ => false
    )

  private def space(id: String, role: SpaceRole, dimension: Int): SpaceRef =
    SpaceRef.of(id, role, dimension).toOption.get

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    GaleNumerics.matrixFromRows(rows)

  private def flipFirstColumn(value: DMat): DMat =
    val out = value.copyData
    var row = 0
    while row < value.rows do
      out(row * value.cols) = -out(row * value.cols)
      row += 1
    GaleNumerics.matrixFromRowMajor(value.rows, value.cols, out)

  private def table[Rows <: SemanticSpace, Features <: SemanticSpace](
      rows: SpaceEvidence[Rows],
      features: SpaceEvidence[Features],
      values: DMat,
      id: String
  ): OpTable[Rows, Features, UncheckedEvidence] =
    acceptedSemantic(
      Op.fromDense(
        values,
        CoordinateEvidence.dual(features),
        CoordinateEvidence.primal(rows),
        OperatorRoleWitness.table,
        ValueIdentity.source(ValueId.unsafe(id))
      )
    )

  private def certifiedRowLink[Rows <: SemanticSpace](
      rows: SpaceEvidence[Rows],
      values: DMat,
      id: String
  ): OpRowLink[Rows, Rows, CertifiedSpd] =
    val identity = ValueIdentity.source(ValueId.unsafe(id))
    val linear = acceptedSemantic(
      Lin.fromDenseMatrix(
        values,
        CoordinateEvidence.primal(rows),
        CoordinateEvidence.dual(rows),
        identity
      )
    )
    val certificate = acceptedSemantic(FormCertificates.spd(linear))
    acceptedSemantic(Op.certifiedSpd(Op.fromLin(linear, OperatorRoleWitness.rowLink), certificate))

  private def rowLink[SourceRows <: SemanticSpace, TargetRows <: SemanticSpace](
      source: SpaceEvidence[SourceRows],
      target: SpaceEvidence[TargetRows],
      values: DMat,
      id: String
  ): OpRowLink[SourceRows, TargetRows, UncheckedEvidence] =
    acceptedSemantic(
      Op.fromDense(
        values,
        CoordinateEvidence.primal(target),
        CoordinateEvidence.dual(source),
        OperatorRoleWitness.rowLink,
        ValueIdentity.source(ValueId.unsafe(id))
      )
    )

  private def accepted[A](result: Either[MultivarError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def acceptedSemantic[A](result: Either[SemanticError, A]): A =
    result.fold(error => fail(error.message), identity)

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

  private def assertVector(actual: DVec, expected: DVec, tolerance: Double): Unit =
    assertEquals(actual.length, expected.length)
    var index = 0
    while index < actual.length do
      assertEqualsDouble(actual(index), expected(index), tolerance)
      index += 1
