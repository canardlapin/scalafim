package scalafim.multivar

import scala.compiletime.testing.typeCheckErrors

import gale.linalg.DMat

class DirectSumStudySuite extends munit.FunSuite:
  private def accepted[A](result: Either[DirectSumError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def acceptedAlignment[A](result: Either[AlignmentError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def acceptedDiagram[A](result: Either[DiagramError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def acceptedSemantic[A](result: Either[SemanticError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def acceptedMv[A](result: Either[MultivarError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def ref(id: String, role: SpaceRole, dimension: Int): SpaceRef =
    SpaceRef(MvSpace(SpaceId.unsafe(id), role, Dimension.unsafe(dimension)))

  private def value(id: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(id))

  private def metric[S <: SemanticSpace](space: SpaceEvidence[S], id: String): MetricForm[S, CertifiedSpd] =
    val legacy = acceptedMv(MetricSpec.identity(space.dimension, Some(space.descriptor)))
    val operator = acceptedSemantic(FormOperator.primal(legacy, space, value(id)))
    val certificate = acceptedSemantic(FormCertificates.spd(operator))
    acceptedSemantic(Form.metric(operator, space, certificate))

  private def geometry[S <: SemanticSpace](space: SpaceEvidence[S], id: String): DiagramGeometry[S] =
    DiagramGeometry.metric(metric(space, id))

  private def diagram[R <: SemanticSpace, C <: SemanticSpace](
      rows: SpaceEvidence[R],
      columns: SpaceEvidence[C],
      matrix: DMat,
      id: String
  ): SemanticDualityDiagram[R, C, CompleteCells] =
    val table = acceptedSemantic(Table.fromMatrixView(DenseMatrixView(matrix), rows, columns, value(s"$id.table")))
    val core = acceptedDiagram(DiagramCore.from(table, geometry(rows, s"$id.rows"), geometry(columns, s"$id.columns")))
    acceptedDiagram(
      SemanticDualityDiagram.from(
        core,
        DiagramPreparation.noCentering(None, GeometryPolicies.reject),
        CellDataSemantics.complete
      )
    )

  private final class Fixture(val entities: SpaceRef)(
      val study: DirectSumStudy,
      val alignment: EntityAlignedStudy[entities.Id],
      val leftEntry: EntityMapEntry[entities.Id],
      val rightEntry: EntityMapEntry[entities.Id]
  )

  private def fixture(leftOrder: Vector[Int] = Vector(0, 1, 2)): Fixture =
    val leftRows = ref("direct.left.rows", SpaceRole.Samples, 3)
    val rightRows = ref("direct.right.rows", SpaceRole.Samples, 2)
    val leftFeatures = ref("direct.left.features", SpaceRole.Observed, 1)
    val rightFeatures = ref("direct.right.features", SpaceRole.Observed, 1)
    val entities = ref("direct.entities", SpaceRole.Samples, 4)
    val leftId = BlockId.unsafe("left")
    val rightId = BlockId.unsafe("right")
    val leftBase = GaleNumerics.matrixFromRows(Vector(Vector(0.0), Vector(1.0), Vector(2.0)))
    val leftDiagram = diagram(
      leftRows.evidence,
      leftFeatures.evidence,
      leftBase.selectRows(leftOrder),
      "direct.left"
    )
    val rightDiagram = diagram(
      rightRows.evidence,
      rightFeatures.evidence,
      GaleNumerics.matrixFromRows(Vector(Vector(3.0), Vector(4.0))),
      "direct.right"
    )
    val study = accepted(
      DirectSumStudy.from(
        ValueId.unsafe("direct.study"),
        Vector(CompleteStudyView(leftId, leftDiagram), CompleteStudyView(rightId, rightDiagram))
      )
    )
    val leftMap = acceptedAlignment(
      IncidenceMap
        .fromEdges(
          leftRows.evidence,
          entities.evidence,
          leftOrder.zipWithIndex.map { case (entity, row) => (row, entity) },
          value("direct.left-map")
        )
        .map(_.rowMap)
    )
    val rightMap = acceptedAlignment(
      IncidenceMap
        .fromEdges(
          rightRows.evidence,
          entities.evidence,
          Vector((0, 1), (1, 2)),
          value("direct.right-map")
        )
        .map(_.rowMap)
    )
    val leftEntry = EntityMapEntry(leftId, leftMap)
    val rightEntry = EntityMapEntry(rightId, rightMap)
    val alignment = acceptedAlignment(
      EntityAlignedStudy.from(
        entities.evidence,
        geometry(entities.evidence, "direct.entity-geometry"),
        Vector(leftEntry, rightEntry)
      )
    )
    new Fixture(entities)(study, alignment, leftEntry, rightEntry)

  private def materialize[
      From <: Coordinate,
      To <: Coordinate,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](operator: Op[From, To, R, E]): DMat =
    acceptedSemantic(operator.toDense)

  private def assertMatrix(actual: DMat, expected: DMat): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), 1e-12)
        col += 1
      row += 1

  private def design(study: DirectSumStudy): BlockDesign =
    accepted(
      BlockDesign.from(
        study.blocks.map(_.id),
        Vector(BlockDesignEdge(BlockId.unsafe("left"), BlockId.unsafe("right"), 1.0))
      )
    )

  private def associationProblem(
      data: Fixture
  ): MaximizeAssociation[data.study.rowSpace.Id] =
    val objective = accepted(DirectSumRowForms.hubAssociation(data.study, data.alignment, design(data.study)))
    MaximizeAssociation(
      objective,
      ObjectiveDefinition(
        ObjectiveFormula.PairwiseAssociation,
        "maximize t* L t = sum_{s != t} gamma_st t_s* L_st t_t",
        Vector("W* Q W = I"),
        ObjectiveNormalization.DirectSumMetricOrthonormal,
        SolverFormulation.SymmetricFeatureEigen,
        SolvedToReportedRelationship.Identical
      )
    )

  test("direct-sum study compiles tables and column forms as block-diagonal operators") {
    val data = fixture()
    assertEquals(data.study.rowSpace.evidence.dimension, 5)
    assertEquals(data.study.featureSpace.evidence.dimension, 2)
    assertEquals(data.study.table.representation, OperatorRepresentation.Block)
    assertEquals(data.study.columnGeometry.operator.representation, OperatorRepresentation.Block)
    assert(data.study.columnGeometry.isSpd)
    assertMatrix(
      materialize(data.study.table),
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(0.0, 0.0),
          Vector(1.0, 0.0),
          Vector(2.0, 0.0),
          Vector(0.0, 3.0),
          Vector(0.0, 4.0)
        )
      )
    )

    val independent = accepted(DirectSumRowForms.independent(data.study))
    assertMatrix(materialize(independent.operator), DMat.eye(5))
    assertEquals(independent.psdCertificate.construction, "block-diagonal")
  }

  test("block design is independent of correspondence and rejects malformed graphs") {
    val data = fixture()
    val blockDesign = design(data.study)
    assertEqualsDouble(blockDesign.coefficient(BlockId.unsafe("right"), BlockId.unsafe("left")), 1.0, 0.0)
    assertEqualsDouble(blockDesign.coefficient(BlockId.unsafe("left"), BlockId.unsafe("absent")), 0.0, 0.0)
    assert(
      BlockDesign
        .from(
          data.study.blocks.map(_.id),
          Vector(BlockDesignEdge(BlockId.unsafe("left"), BlockId.unsafe("left"), 1.0))
        )
        .isLeft
    )
  }

  test("hub-factorized direct-sum row geometry is PSD and retains its global proof") {
    val data = fixture()
    val rowGeometry = accepted(DirectSumRowForms.hubGeometry(data.study, data.alignment))
    val matrix = materialize(rowGeometry.operator)
    val eigen = DenseSolvers.symmetricEigen.decompose(matrix).toOption.get
    assert(eigen.values.toVector.forall(_ >= -1e-10))
    assert(rowGeometry.psdCertificate.proof.contains("PSD"))
    assertEquals(matrix.rows, 5)
    assertEqualsDouble(matrix(0, 3), 0.0, 0.0)
    assertEqualsDouble(matrix(1, 3), 1.0, 0.0)
    assertEqualsDouble(matrix(2, 4), 1.0, 0.0)
  }

  test("hub association compiles an explicitly symmetric but potentially indefinite objective") {
    val data = fixture()
    val objective = accepted(DirectSumRowForms.hubAssociation(data.study, data.alignment, design(data.study)))
    val matrix = materialize(objective.operator)
    assert(objective.potentiallyIndefinite)
    assertEquals(objective.adjointCertificates.length, 1)
    assertEqualsDouble(matrix(1, 3), 1.0, 0.0)
    assertEqualsDouble(matrix(3, 1), 1.0, 0.0)
    assertEqualsDouble(matrix(2, 4), 1.0, 0.0)
    assertEqualsDouble(matrix(4, 2), 1.0, 0.0)
    assertEqualsDouble(matrix(0, 0), 0.0, 0.0)
  }

  test("a real partially aligned multiset association fit uses the compiled operator") {
    val data = fixture()
    val problem = associationProblem(data)
    val fit = accepted(
      MultisetAssociation.fit(
        data.study,
        problem,
        ComponentCount.unsafe(1),
        StoragePolicy.AllowDense
      )
    )

    assertEqualsDouble(fit.eigenvalues(0), 11.0, 1e-8)
    assertEquals(fit.featureAxes.rows, 2)
    assertEquals(fit.directSumScores.rows, 5)
    assertEquals(fit.viewScores.map(_.values.rows), Vector(3, 2))
    assertEquals(fit.objective, problem.definition)
    assertEquals(fit.diagnostics.rowOperatorRepresentation, OperatorRepresentation.Block)
    assertEquals(fit.diagnostics.featureOperatorRepresentation, OperatorRepresentation.Block)
    assert(fit.diagnostics.formulation.contains("OperatorProgram"))
    assertEquals(fit.programFit.program.objective.label, "maximize-trace")
    assert(fit.programFit.program.resultSemantics.equivalence.isInstanceOf[ResultEquivalence.SubspaceEquivalent])
    assertEquals(fit.operatorBundle.resultSemantics, fit.programFit.program.resultSemantics)
    assertEquals(fit.componentAssociation.role.value, OperatorRole.Component)
    assertEqualsDouble(materialize(fit.componentAssociation)(0, 0), fit.eigenvalues(0), 1e-8)
    assertEquals(
      fit.programFit.frames.head.parameter.componentSpace.descriptor,
      fit.functionalFrame.weights.domain.descriptor.space
    )
  }

  test("pairwise second-order blocks equal the direct-sum operator and an independent dense oracle") {
    val data = fixture()
    val problem = associationProblem(data)
    val association = accepted(DirectSumFeatureOperators.association(data.study, problem.objective))
    val table = materialize(data.study.table)
    val rowRelation = materialize(problem.objective.operator)
    val expected = GaleNumerics.multiply(table.t, GaleNumerics.multiply(rowRelation, table))

    assertEquals(association.blocks.length, 2)
    assert(association.blocks.forall(_.operator.role.value == OperatorRole.Cross))
    assertEquals(association.operator.representation, OperatorRepresentation.Block)
    assertMatrix(materialize(association.operator), expected)
    assertMatrix(materialize(association.direct), expected)
    assert(association.certificate.proof.contains("secondOrder"))

    val leftToRight = association.blocks.find(block =>
      block.sourceId == BlockId.unsafe("left") && block.targetId == BlockId.unsafe("right")
    ).get
    assertEqualsDouble(materialize(leftToRight.operator)(0, 0), 11.0, 1e-12)
  }

  test("hub-factorized association is invariant to a consistent within-view row permutation") {
    val original = fixture()
    val permuted = fixture(Vector(2, 0, 1))
    val originalFit = accepted(
      MultisetAssociation.fit(
        original.study,
        associationProblem(original),
        ComponentCount.unsafe(1),
        StoragePolicy.AllowDense
      )
    )
    val permutedFit = accepted(
      MultisetAssociation.fit(
        permuted.study,
        associationProblem(permuted),
        ComponentCount.unsafe(1),
        StoragePolicy.AllowDense
      )
    )

    assertEqualsDouble(permutedFit.eigenvalues(0), originalFit.eigenvalues(0), 1e-10)
    assertEquals(permutedFit.association.blocks.map(block => block.sourceId -> block.targetId),
      originalFit.association.blocks.map(block => block.sourceId -> block.targetId))
  }

  test("agreement constraints have distinct hard, bounded, and PSD penalty semantics") {
    val data = fixture()
    val constraint = accepted(
      LinearConstraint.pairwiseHubAgreement(data.study, data.leftEntry, data.rightEntry)
    )
    val agreeingScores = GaleNumerics.matrixFromRows(
      Vector(Vector(0.0), Vector(5.0), Vector(7.0), Vector(5.0), Vector(7.0))
    )
    assertEqualsDouble(acceptedSemantic(constraint.residual(agreeingScores)), 0.0, 1e-12)
    assertEquals(constraint.hard.semantics, ConstraintSemantics.Hard)
    val bounded = accepted(constraint.bounded(data.alignment.entityForm, 0.25))
    assertEqualsDouble(bounded.epsilon, 0.25, 0.0)

    val penalty = accepted(constraint.quadraticPenalty(data.alignment.entityForm))
    val eigen = DenseSolvers.symmetricEigen.decompose(materialize(penalty.operator)).toOption.get
    assert(eigen.values.toVector.forall(_ >= -1e-10))
    assertEquals(penalty.psdCertificate.construction, "B-star-W-B")
  }

  test("identity hub alignment reduces to the classical same-row block objective") {
    val leftRows = ref("direct.identity.left", SpaceRole.Samples, 2)
    val rightRows = ref("direct.identity.right", SpaceRole.Samples, 2)
    val leftFeatures = ref("direct.identity.x", SpaceRole.Observed, 1)
    val rightFeatures = ref("direct.identity.y", SpaceRole.Observed, 1)
    val entities = ref("direct.identity.entities", SpaceRole.Samples, 2)
    val leftId = BlockId.unsafe("left")
    val rightId = BlockId.unsafe("right")
    val study = accepted(
      DirectSumStudy.from(
        ValueId.unsafe("direct.identity.study"),
        Vector(
          CompleteStudyView(
            leftId,
            diagram(leftRows.evidence, leftFeatures.evidence, GaleNumerics.matrixFromRows(Vector(Vector(1.0), Vector(2.0))), "identity.left")
          ),
          CompleteStudyView(
            rightId,
            diagram(rightRows.evidence, rightFeatures.evidence, GaleNumerics.matrixFromRows(Vector(Vector(3.0), Vector(4.0))), "identity.right")
          )
        )
      )
    )
    val leftEvidence = acceptedAlignment(
      SameEntityEvidence.fromVerifiedIdentity(
        leftRows.evidence,
        entities.evidence,
        entities.descriptor,
        value("identity.left-keys")
      )
    )
    val rightEvidence = acceptedAlignment(
      SameEntityEvidence.fromVerifiedIdentity(
        rightRows.evidence,
        entities.evidence,
        entities.descriptor,
        value("identity.right-keys")
      )
    )
    val sameRows = accepted(
      SameRowStudy.from(
        study,
        entities.evidence,
        geometry(entities.evidence, "identity.entity-geometry"),
        Vector(SameRowViewEvidence(leftId, leftEvidence), SameRowViewEvidence(rightId, rightEvidence))
      )
    )
    val objective = accepted(DirectSumRowForms.sameRowAssociation(sameRows, design(study)))
    assertMatrix(
      materialize(objective.operator),
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(0.0, 0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0),
          Vector(1.0, 0.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0, 0.0)
        )
      )
    )
  }

  test("pairwise links retain only justified coherence certificates") {
    val data = fixture()
    val pair = acceptedAlignment(
      HubAlignment.inducedPair(data.leftEntry.map, data.alignment.entityForm, data.rightEntry.map)
    )
    val linked = accepted(
      PairwiseLinkedStudy.from(
        data.study,
        Vector(UndirectedRowRelationship(data.leftEntry.viewId, data.rightEntry.viewId, pair))
      )
    )
    assertEquals(linked.certificates.adjoint.length, 1)
    assertEquals(linked.certificates.hub, None)
    assertEquals(linked.certificates.globalBlockPsd, None)
    assertEquals(linked.certificates.cycle, None)

    val pairwiseObjective = accepted(DirectSumRowForms.pairwiseAssociation(linked, design(data.study)))
    val hubObjective = accepted(DirectSumRowForms.hubAssociation(data.study, data.alignment, design(data.study)))
    assertMatrix(materialize(pairwiseObjective.operator), materialize(hubObjective.operator))
  }

  test("objective, geometry, penalty, and relation kinds do not silently coerce") {
    val errors = typeCheckErrors("""
      import scalafim.multivar.*
      def metricFromObjective[S <: SemanticSpace](value: SymmetricObjectiveForm[S]): RowGeometry[S] = value
      def objectiveFromPenalty[S <: SemanticSpace](value: ConstraintPenalty[S]): SymmetricObjectiveForm[S] = value
      def undirectedFromCoupling[A <: SemanticSpace, B <: SemanticSpace](
          left: BlockId,
          right: BlockId,
          value: NonnegativeCoupling[A, B]
      ): UndirectedRowRelationship = UndirectedRowRelationship(left, right, value)
    """)
    assert(errors.length >= 3)
  }
