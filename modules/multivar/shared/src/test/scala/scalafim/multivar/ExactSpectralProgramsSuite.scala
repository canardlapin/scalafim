package scalafim.multivar

import gale.linalg.DMat

class ExactSpectralProgramsSuite extends munit.FunSuite:

  private val gpcaPrepared = DynamicGpcaProblem
    .from(
      MatrixView.dense(
        GaleNumerics.matrixFromRows(
          Vector(
            Vector(2.0, 0.0, 1.0),
            Vector(0.0, 2.0, 1.0),
            Vector(-2.0, 0.0, -1.0),
            Vector(0.0, -2.0, -1.0)
          )
        )
      ),
      MvSpace.of("exact-gpca-rows", SpaceRole.Samples, 4).toOption.get,
      MvSpace.of("exact-gpca-features", SpaceRole.Observed, 3).toOption.get,
      MetricSpec.identity(4).toOption.get,
      MetricSpec.identity(3).toOption.get,
      ValueIdentity.source(ValueId.unsafe("exact-gpca-source")),
      SemanticProvenance.source("exact-gpca")
    )
    .toOption
    .get
  private val gpca = gpcaPrepared.value

  test("every quadratic family executes as an exact global spectral rewrite"):
    QuadraticFamily.values.foreach: family =>
      val lowering = gpcaLowering(family, QuadraticPlacement.ObjectiveRidge)
      val fit = accepted(
        ExactSpectralPrograms.gpcaQuadratic(gpca, lowering, ComponentCount.unsafe(2))
      )

      assertEquals(fit.proof.kind, ExactSpectralRewriteKind.ObjectiveQuadratic(family))
      assert(fit.proof.exact)
      assertEquals(fit.requestedProgram.penalties, Vector(lowering.original))
      assertEquals(fit.loweredProgram.penalties, Vector.empty)
      assertEquals(fit.loweredProgram.resultSemantics.requestedClaim, RequestedOptimizationClaim.ExactGlobal)
      assertEquals(fit.programFit.achievedGuarantee.claimClass, OptimizationClaimClass.ExactGlobal)
      assert(fit.programFit.achievedGuarantee.isInstanceOf[AchievedOptimizationGuarantee.ExactGlobal])

  test("direct and lowered quadratic objectives agree on the fitted frame"):
    val lowering = gpcaLowering(QuadraticFamily.GraphSmoothness, QuadraticPlacement.ObjectiveRidge)
    val fit = accepted(
      ExactSpectralPrograms.gpcaQuadratic(gpca, lowering, ComponentCount.unsafe(2))
    )
    val weights = fit.functionalFrame.weights.toDense.toOption.get
    val covariance = gpca.covariance.toDense.toOption.get
    val penalty = lowering.pulledBack.toDense.toOption.get
    val direct = trace(weights.t * covariance * weights) -
      lowering.original.weight.value * trace(weights.t * penalty * weights)
    val lowered = trace(weights.t * fit.solvedNumerator * weights)

    assertEqualsDouble(direct, lowered, 1e-9)
    assertEqualsDouble(lowered, fit.eigenvalues.toVector.sum, 1e-8)

  test("objective penalties and denominator loadings remain distinct executable programs"):
    val objectiveLowering = gpcaLowering(QuadraticFamily.Ridge, QuadraticPlacement.ObjectiveRidge)
    val denominatorLowering = gpcaLowering(QuadraticFamily.Ridge, QuadraticPlacement.DenominatorLoading)
    val objective = accepted(
      ExactSpectralPrograms.gpcaQuadratic(gpca, objectiveLowering, ComponentCount.unsafe(2))
    )
    val denominator = accepted(
      ExactSpectralPrograms.gpcaQuadratic(gpca, denominatorLowering, ComponentCount.unsafe(2))
    )
    val baseNumerator = gpca.covariance.toDense.toOption.get
    val baseDenominator = gpca.featureCometric.toDense.toOption.get
    val pulledBack = objectiveLowering.pulledBack.toDense.toOption.get

    assertMatrixClose(
      objective.solvedNumerator,
      MatrixOps.subtract(baseNumerator, MatrixOps.scale(pulledBack, objectiveLowering.original.weight.value)),
      1e-12
    )
    assertMatrixClose(objective.solvedDenominator, baseDenominator, 1e-12)
    assertMatrixClose(denominator.solvedNumerator, baseNumerator, 1e-12)
    assertMatrixClose(
      denominator.solvedDenominator,
      MatrixOps.subtract(baseDenominator, MatrixOps.scale(pulledBack, -denominatorLowering.original.weight.value)),
      1e-12
    )
    assert(objective.loweredProgram.objective != denominator.loweredProgram.objective)
    assert(objective.proof.kind != denominator.proof.kind)

  test("multiset quadratic disagreement executes at the explicit feature-space boundary"):
    val data = multisetFixture()
    val constraint = acceptedDirect(
      LinearConstraint.pairwiseHubAgreement(data.study, data.leftEntry, data.rightEntry)
    )
    val penalty = acceptedDirect(constraint.quadraticPenalty(data.entityGeometry))
    val association = associationProblem(data)
    val fit = accepted(
      ExactSpectralPrograms.multisetQuadratic(
        data.study,
        association,
        penalty,
        PenaltyWeight.unsafe(0.2),
        ComponentCount.unsafe(1)
      )
    )
    val featureAssociation = acceptedDirect(DirectSumFeatureOperators.association(data.study, association.objective))
    val base = featureAssociation.operator.toDense.toOption.get
    val table = data.study.table.toDense.toOption.get
    val rowPenalty = penalty.operator.toDense.toOption.get
    val expected = MatrixOps.subtract(base, MatrixOps.scale(table.t * rowPenalty * table, 0.2))

    assertMatrixClose(fit.solvedNumerator, expected, 1e-10)
    assertEquals(fit.proof.kind, ExactSpectralRewriteKind.ObjectiveQuadratic(QuadraticFamily.MultisetDisagreement))
    assertEquals(data.study.table.representation, OperatorRepresentation.Block)
    assertEquals(featureAssociation.operator.representation, OperatorRepresentation.Block)

  test("hard multiset agreement lowers through a verified null space and remains distinct from bounded and quadratic forms"):
    val data = multisetFixture()
    val constraint = acceptedDirect(
      LinearConstraint.pairwiseHubAgreement(data.study, data.leftEntry, data.rightEntry)
    )
    val hard = constraint.hard
    val bounded = acceptedDirect(constraint.bounded(data.entityGeometry, 0.25))
    val quadratic = acceptedDirect(constraint.quadraticPenalty(data.entityGeometry))
    val fit = accepted(
      ExactSpectralPrograms.multisetHardEquality(
        data.study,
        associationProblem(data),
        hard,
        ComponentCount.unsafe(1)
      )
    )
    val weights = fit.functionalFrame.weights.toDense.toOption.get
    val scores = data.study.table.toDense.toOption.get * weights
    val residual = constraint.residual(scores).toOption.get

    assertEqualsDouble(residual, fit.proof.residual, 1e-12)
    assert(residual <= CertificateTolerance.strict.threshold(1.0))
    assert(fit.proof.nullSpace.exists(_.numericalCertificate.residual <= fit.proof.tolerance.threshold(1.0)))
    assertEquals(fit.requestedProgram.constraints.length, 1)
    assertEquals(fit.loweredProgram.constraints, Vector.empty)
    assert(fit.loweredProgram.parameters.head.kind.isInstanceOf[ParameterizationKind.NullSpace])
    assertEquals(fit.programFit.achievedGuarantee.claimClass, OptimizationClaimClass.ExactGlobal)
    assertEquals(hard.semantics, ConstraintSemantics.Hard)
    assert(bounded.semantics.isInstanceOf[ConstraintSemantics.Bounded])
    assert(quadratic.operator.valueIdentity != hard.constraint.operator.valueIdentity)

  private def gpcaLowering(
      family: QuadraticFamily,
      placement: QuadraticPlacement
  ): QuadraticLowering[gpcaPrepared.features.Id] =
    val identity = Op
      .fromDense(
        DMat.eye(gpca.featureSpace.dimension),
        CoordinateEvidence.dual(gpca.featureSpace),
        CoordinateEvidence.primal(gpca.featureSpace),
        OperatorRoleWitness.cross,
        ValueIdentity.source(ValueId.unsafe(s"${family.toString}-${placement.toString}-target"))
      )
      .toOption
      .get
    val parameter = ParameterId.unsafe(s"${gpca.featureSpace.id.value}.quadratic-gpca-frame")
    val target = TargetExpression.linear(parameter, s"${family.toString}-target", identity).toOption.get
    val term = PenaltyTerm(
      target,
      FunctionalKind.SquaredNorm(gpca.featureMetric.valueIdentity),
      PenaltyWeight.unsafe(0.1)
    )
    QuadraticPullback
      .lower(term, identity, gpca.featureMetric, family, placement)
      .toOption
      .get

  private final class MultisetFixture(val entities: SpaceRef)(
      val study: DirectSumStudy,
      val alignment: EntityAlignedStudy[entities.Id],
      val leftEntry: EntityMapEntry[entities.Id],
      val rightEntry: EntityMapEntry[entities.Id],
      val entityGeometry: DiagramGeometry[entities.Id]
  )

  private def multisetFixture(): MultisetFixture =
    val leftRows = ref("exact.left.rows", SpaceRole.Samples, 3)
    val rightRows = ref("exact.right.rows", SpaceRole.Samples, 2)
    val leftFeatures = ref("exact.left.features", SpaceRole.Observed, 1)
    val rightFeatures = ref("exact.right.features", SpaceRole.Observed, 1)
    val entities = ref("exact.entities", SpaceRole.Samples, 4)
    val leftId = BlockId.unsafe("left")
    val rightId = BlockId.unsafe("right")
    val study = acceptedDirect(
      DirectSumStudy.from(
        ValueId.unsafe("exact-multiset-study"),
        Vector(
          CompleteStudyView(
            leftId,
            diagram(
              leftRows.evidence,
              leftFeatures.evidence,
              GaleNumerics.matrixFromRows(Vector(Vector(0.0), Vector(1.0), Vector(2.0))),
              "exact-left"
            )
          ),
          CompleteStudyView(
            rightId,
            diagram(
              rightRows.evidence,
              rightFeatures.evidence,
              GaleNumerics.matrixFromRows(Vector(Vector(1.0), Vector(2.0))),
              "exact-right"
            )
          )
        )
      )
    )
    val leftMap = IncidenceMap
      .fromEdges(
        leftRows.evidence,
        entities.evidence,
        Vector((0, 0), (1, 1), (2, 2)),
        identity("exact-left-map")
      )
      .toOption
      .get
      .rowMap
    val rightMap = IncidenceMap
      .fromEdges(
        rightRows.evidence,
        entities.evidence,
        Vector((0, 1), (1, 2)),
        identity("exact-right-map")
      )
      .toOption
      .get
      .rowMap
    val leftEntry = EntityMapEntry(leftId, leftMap)
    val rightEntry = EntityMapEntry(rightId, rightMap)
    val entityGeometry = geometry(entities.evidence, "exact-entity-geometry")
    val alignment = EntityAlignedStudy
      .from(entities.evidence, entityGeometry, Vector(leftEntry, rightEntry))
      .toOption
      .get
    new MultisetFixture(entities)(study, alignment, leftEntry, rightEntry, entityGeometry)

  private def associationProblem(data: MultisetFixture): MaximizeAssociation[data.study.rowSpace.Id] =
    val design = BlockDesign
      .from(
        data.study.blocks.map(_.id),
        Vector(BlockDesignEdge(BlockId.unsafe("left"), BlockId.unsafe("right"), 1.0))
      )
      .toOption
      .get
    val objective = DirectSumRowForms
      .hubAssociation(data.study, data.alignment, design)
      .toOption
      .get
    MaximizeAssociation(
      objective,
      ObjectiveDefinition(
        ObjectiveFormula.PairwiseAssociation,
        "exact multiset association",
        Vector("W* Q W = I"),
        ObjectiveNormalization.DirectSumMetricOrthonormal,
        SolverFormulation.SymmetricFeatureEigen,
        SolvedToReportedRelationship.Identical
      )
    )

  private def diagram[R <: SemanticSpace, C <: SemanticSpace](
      rows: SpaceEvidence[R],
      columns: SpaceEvidence[C],
      values: DMat,
      name: String
  ): SemanticDualityDiagram[R, C, CompleteCells] =
    val table = Table
      .fromMatrixView(DenseMatrixView(values), rows, columns, identity(s"$name-table"))
      .toOption
      .get
    val core = DiagramCore
      .from(table, geometry(rows, s"$name-rows"), geometry(columns, s"$name-columns"))
      .toOption
      .get
    SemanticDualityDiagram
      .from(
        core,
        DiagramPreparation.noCentering(None, GeometryPolicies.reject),
        CellDataSemantics.complete
      )
      .toOption
      .get

  private def geometry[S <: SemanticSpace](
      space: SpaceEvidence[S],
      name: String
  ): DiagramGeometry[S] =
    val spec = MetricSpec.identity(space.dimension, Some(space.descriptor)).toOption.get
    val operator = FormOperator.primal(spec, space, identity(name)).toOption.get
    val certificate = FormCertificates.spd(operator).toOption.get
    DiagramGeometry.metric(Form.metric(operator, space, certificate).toOption.get)

  private def ref(name: String, role: SpaceRole, size: Int): SpaceRef =
    SpaceRef(MvSpace.of(name, role, size).toOption.get)

  private def identity(name: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(name))

  private def trace(values: DMat): Double =
    var result = 0.0
    var index = 0
    while index < Math.min(values.rows, values.cols) do
      result += values(index, index)
      index += 1
    result

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

  private def accepted[A](value: Either[ExactSpectralError, A]): A =
    value.fold(error => fail(error.message), current => current)

  private def acceptedDirect[A](value: Either[DirectSumError, A]): A =
    value.fold(error => fail(error.message), current => current)
