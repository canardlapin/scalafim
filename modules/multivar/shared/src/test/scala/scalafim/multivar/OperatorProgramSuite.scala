package scalafim.multivar

import scala.compiletime.testing.typeCheckErrors

import gale.linalg.DMat

class OperatorProgramSuite extends munit.FunSuite:

  test("every closed base objective retains its typed dense operators"):
    val fixture = programFixture()
    val objectives = Vector[BaseObjective](
      BaseObjective.MaximizeTrace(fixture.source.variable.id, fixture.sourceValue),
      BaseObjective.MaximizeCrossTrace(fixture.source.variable.id, fixture.target.variable.id, fixture.cross),
      BaseObjective.GeneralizedRayleigh(fixture.source.variable.id, fixture.sourceValue, fixture.sourceDenominator),
      BaseObjective.TraceRatio(fixture.source.variable.id, fixture.sourceValue, fixture.sourceDenominator),
      BaseObjective.RatioTrace(fixture.source.variable.id, fixture.sourceValue, fixture.sourceDenominator),
      BaseObjective.MinimizeDisagreement(fixture.source.variable.id, fixture.sourceValue),
      BaseObjective.SequentialCrossRegression(
        fixture.source.variable.id,
        fixture.target.variable.id,
        fixture.cross,
        fixture.sourceDenominator
      )
    )

    assertEquals(
      objectives.map(_.label),
      Vector(
        "maximize-trace",
        "maximize-cross-trace",
        "generalized-rayleigh",
        "trace-ratio",
        "ratio-trace",
        "minimize-disagreement",
        "sequential-cross-regression"
      )
    )
    objectives.foreach:
      case BaseObjective.MaximizeTrace(_, operator) => assertScalarOperator(operator, 2.0)
      case BaseObjective.MaximizeCrossTrace(_, _, operator) => assertScalarOperator(operator, 0.75)
      case BaseObjective.GeneralizedRayleigh(_, numerator, denominator) =>
        assertScalarOperator(numerator, 2.0)
        assertScalarOperator(denominator, 4.0)
      case BaseObjective.TraceRatio(_, numerator, denominator) =>
        assertScalarOperator(numerator, 2.0)
        assertScalarOperator(denominator, 4.0)
      case BaseObjective.RatioTrace(_, numerator, denominator) =>
        assertScalarOperator(numerator, 2.0)
        assertScalarOperator(denominator, 4.0)
      case BaseObjective.MinimizeDisagreement(_, operator) => assertScalarOperator(operator, 2.0)
      case BaseObjective.SequentialCrossRegression(_, _, cross, predictor) =>
        assertScalarOperator(cross, 0.75)
        assertScalarOperator(predictor, 4.0)

  test("GPCA, LDA, CCA, PLSC, and multiset builders compile to one program type"):
    val fixture = programFixture()
    val gpca = accepted(
      OperatorPrograms.gpca(fixture.source, fixture.sourceValue, fixture.sourceNormalization)
    )
    val lda = accepted(
      OperatorPrograms.ldaRayleigh(
        fixture.source,
        fixture.sourceValue,
        fixture.sourceDenominator,
        fixture.sourceNormalization
      )
    )
    val traceRatio = accepted(
      OperatorPrograms.ldaTraceRatio(
        fixture.source,
        fixture.sourceValue,
        fixture.sourceDenominator,
        fixture.sourceNormalization
      )
    )
    val cca = accepted(
      OperatorPrograms.cca(
        fixture.source,
        fixture.target,
        fixture.cross,
        fixture.sourceNormalization,
        fixture.targetNormalization
      )
    )
    val plsc = accepted(
      OperatorPrograms.plsc(
        fixture.source,
        fixture.target,
        fixture.cross,
        fixture.sourceNormalization,
        fixture.targetNormalization
      )
    )
    val multiset = accepted(
      OperatorPrograms.multiset(fixture.source, fixture.sourceValue, fixture.sourceNormalization)
    )

    assertEquals(
      Vector(gpca, lda, traceRatio, cca, plsc, multiset).map(_.objective.label),
      Vector(
        "maximize-trace",
        "generalized-rayleigh",
        "trace-ratio",
        "maximize-cross-trace",
        "maximize-cross-trace",
        "maximize-trace"
      )
    )
    assert(Vector(gpca, lda, traceRatio, cca, plsc, multiset).forall(_.isInstanceOf[OperatorProgram]))

  test("parameterization variants are inspectable and exact linear reductions retain operator identity"):
    val feature = space("parameterization-feature", SpaceRole.Observed, 3)
    val free = space("parameterization-free", SpaceRole.Observed, 2)
    val component = space("parameterization-component", SpaceRole.Latent, 1)
    type F = feature.Id
    type Z = free.Id
    type K = component.Id
    val variable = accepted(FrameVariable.from(ParameterId.unsafe("w"), feature.evidence, component.evidence))
    val embedding = acceptedSemantic(
      Op.fromDense(
        matrix(Vector(Vector(1.0, 0.0), Vector(0.0, 1.0), Vector(0.0, 0.0))),
        CoordinateEvidence.dual(free.evidence),
        CoordinateEvidence.dual(feature.evidence),
        OperatorRoleWitness.frame,
        id("support-embedding")
      )
    )
    val identity = FrameParameterization.identity(variable)
    val support = FrameParameterization.knownSupport(variable, free.evidence, embedding, injective = true)
    val nullSpace = FrameParameterization.nullSpace(variable, free.evidence, embedding, CertificateTolerance.strict)
    val fixedRank = accepted(FrameParameterization.fixedRank(variable, ComponentCount.unsafe(1)))
    val blocked = accepted(FrameParameterization.blockDiagonal(variable, Vector(ParameterId.unsafe("left"), ParameterId.unsafe("right"))))

    assertEquals(identity.kind, ParameterizationKind.Identity)
    assertEquals(support.kind, ParameterizationKind.KnownSupport(embedding.valueIdentity, injective = true))
    assertEquals(nullSpace.kind, ParameterizationKind.NullSpace(embedding.valueIdentity, CertificateTolerance.strict))
    assertEquals(fixedRank.kind, ParameterizationKind.FixedRank(ComponentCount.unsafe(1), ParameterizationGauge.GeneralLinear))
    assert(blocked.kind.isInstanceOf[ParameterizationKind.BlockDiagonal])
    assert(FrameParameterization.fixedRank(variable, ComponentCount.unsafe(2)).isLeft)
    assert(FrameParameterization.blockDiagonal(variable, Vector.empty).isLeft)

  test("program construction rejects unknown, duplicated, and unnormalized parameters"):
    val fixture = programFixture()
    val objective = BaseObjective.MaximizeTrace(fixture.source.variable.id, fixture.sourceValue)
    val duplicate = OperatorProgram.from(
      Vector(fixture.source, fixture.source),
      objective,
      Vector(fixture.sourceNormalization)
    )
    val missing = OperatorProgram.from(Vector(fixture.source), objective, Vector.empty)
    val duplicateNormalization = OperatorProgram.from(
      Vector(fixture.source),
      objective,
      Vector(fixture.sourceNormalization, fixture.sourceNormalization)
    )
    val unknownTarget = PenaltyTerm(
      TargetExpression.frame(ParameterId.unsafe("missing")),
      FunctionalKind.L1,
      PenaltyWeight.unsafe(1.0)
    )
    val unknown = OperatorProgram.from(
      Vector(fixture.source),
      objective,
      Vector(fixture.sourceNormalization),
      penalties = Vector(unknownTarget)
    )
    val collapsedPair = OperatorProgram.from(
      Vector(fixture.source),
      BaseObjective.MaximizeCrossTrace(
        fixture.source.variable.id,
        fixture.source.variable.id,
        fixture.cross
      ),
      Vector(fixture.sourceNormalization)
    )

    assert(duplicate.left.exists(_.isInstanceOf[ProgramError.DuplicateParameter]))
    assert(missing.left.exists(_.isInstanceOf[ProgramError.MissingNormalization]))
    assert(duplicateNormalization.left.exists(_.isInstanceOf[ProgramError.DuplicateNormalization]))
    assert(unknown.left.exists(_.isInstanceOf[ProgramError.UnknownParameter]))
    assert(collapsedPair.left.exists(_.isInstanceOf[ProgramError.InvalidParameterization]))

  test("whole-program symmetry determines subspace, frame, and prediction semantics"):
    val fixture = programFixture()
    val smooth = accepted(
      OperatorPrograms.gpca(fixture.source, fixture.sourceValue, fixture.sourceNormalization)
    )
    val l1 = PenaltyTerm(
      TargetExpression.frame(fixture.source.variable.id),
      FunctionalKind.L1,
      PenaltyWeight.unsafe(0.5)
    )
    val sparse = accepted(
      OperatorProgram.from(
        Vector(fixture.source),
        BaseObjective.MaximizeTrace(fixture.source.variable.id, fixture.sourceValue),
        Vector(fixture.sourceNormalization),
        penalties = Vector(l1)
      )
    )
    val regression = accepted(
      OperatorProgram.from(
        Vector(fixture.source, fixture.target),
        BaseObjective.SequentialCrossRegression(
          fixture.source.variable.id,
          fixture.target.variable.id,
          fixture.cross,
          fixture.sourceDenominator
        ),
        Vector(fixture.sourceNormalization, fixture.targetNormalization)
      )
    )

    assert(smooth.resultSemantics.equivalence.isInstanceOf[ResultEquivalence.SubspaceEquivalent])
    assertEquals(
      sparse.resultSemantics.equivalence,
      ResultEquivalence.FrameEquivalent(FrameSymmetry.SignedPermutation, CertificateTolerance.strict)
    )
    assert(regression.resultSemantics.equivalence.isInstanceOf[ResultEquivalence.PredictionEquivalent])
    assertEquals(smooth.resultSemantics.guarantee, SolverGuarantee.GlobalSpectralOptimum)
    assertEquals(sparse.resultSemantics.guarantee, SolverGuarantee.StationaryPoint)

    val variants = Vector[ResultEquivalence](
      ResultEquivalence.ValueEquivalent(CertificateTolerance.strict),
      ResultEquivalence.OperatorEquivalent(
        fixture.sourceValue.domain.descriptor,
        fixture.sourceValue.codomain.descriptor,
        CertificateTolerance.strict
      ),
      ResultEquivalence.SubspaceEquivalent(CertificateTolerance.strict, CertificateTolerance.strict),
      ResultEquivalence.FrameEquivalent(FrameSymmetry.Orthogonal, CertificateTolerance.strict),
      ResultEquivalence.PredictionEquivalent(PredictionMetric.Correlation, CertificateTolerance.strict),
      ResultEquivalence.ObjectiveEquivalent(CertificateTolerance.strict)
    )
    assertEquals(variants.map(resultLabel), Vector("value", "operator", "subspace", "frame", "prediction", "objective"))

  test("program fits retain one functional frame and derive transformations from it"):
    val rows = space("fit-rows", SpaceRole.Samples, 2)
    val feature = space("fit-feature", SpaceRole.Observed, 2)
    val componentSpace = space("fit-component", SpaceRole.Latent, 1)
    type O = rows.Id
    type F = feature.Id
    type K = componentSpace.Id
    val variable = accepted(FrameVariable.from(ParameterId.unsafe("fit-frame"), feature.evidence, componentSpace.evidence))
    val parameterization = FrameParameterization.identity(variable)
    val program = accepted(
      OperatorPrograms.gpca(
        parameterization,
        component(componentSpace.evidence, 2.0, "fit-value"),
        FrameNormalization(variable, certifiedMetric(feature.evidence, DMat.eye(2), "fit-normalization"))
      )
    )
    val weights: OpFrame[F, K, UncheckedEvidence] = acceptedSemantic(
      Op.fromDense(
        matrix(Vector(Vector(1.0), Vector(-0.5))),
        CoordinateEvidence.primal(componentSpace.evidence),
        CoordinateEvidence.dual(feature.evidence),
        OperatorRoleWitness.frame,
        id("fit-weights")
      )
    )
    val fitted = FittedFrame(variable, FunctionalFrame(weights))
    val identifiability = NumericalIdentifiability(1, Vector(Vector(0)), 0.0, CertificateContext.portableFloat64)
    val result = accepted(
      OperatorProgramFit.from(
        program,
        Vector(fitted),
        2.0,
        identifiability,
        SemanticProvenance.source("fit-result")
      )
    )
    val table: OpTable[O, F, UncheckedEvidence] = acceptedSemantic(
      Op.fromDense(
        matrix(Vector(Vector(2.0, 0.0), Vector(1.0, 4.0))),
        CoordinateEvidence.dual(feature.evidence),
        CoordinateEvidence.primal(rows.evidence),
        OperatorRoleWitness.table,
        id("fit-table")
      )
    )

    assertEquals(result.program.resultSemantics, program.resultSemantics)
    assertMatrix(
      acceptedSemantic(fitted.frame.scores(table).toDense),
      matrix(Vector(Vector(2.0), Vector(-1.0)))
    )
    assert(
      OperatorProgramFit.from(
        program,
        Vector(fitted),
        Double.NaN,
        identifiability,
        SemanticProvenance.source("bad-fit-result")
      ).isLeft
    )

  test("nominal feature spaces prevent invalid normalization at compile time"):
    val errors = typeCheckErrors("""
      import scalafim.multivar.*
      val f = SpaceRef(MvSpace(SpaceId.unsafe("f"), SpaceRole.Observed, Dimension.unsafe(2)))
      val g = SpaceRef(MvSpace(SpaceId.unsafe("g"), SpaceRole.Observed, Dimension.unsafe(2)))
      val k = SpaceRef(MvSpace(SpaceId.unsafe("k"), SpaceRole.Latent, Dimension.unsafe(1)))
      type F = f.Id
      type G = g.Id
      type K = k.Id
      val variable: FrameVariable[F, K] = ???
      val metric: OpMetric[G, CertifiedSpd] = ???
      FrameNormalization(variable, metric)
    """)
    assert(errors.nonEmpty)

  private final case class Fixture[
      SourceFeature <: SemanticSpace,
      TargetFeature <: SemanticSpace,
      SourceComponent <: SemanticSpace,
      TargetComponent <: SemanticSpace
  ](
      source: FrameParameterization[SourceFeature, SourceComponent],
      target: FrameParameterization[TargetFeature, TargetComponent],
      sourceValue: Op[Primal[SourceComponent], Dual[SourceComponent], ComponentOperatorRole, UncheckedEvidence],
      sourceDenominator: Op[Primal[SourceComponent], Dual[SourceComponent], ComponentOperatorRole, CertifiedSpd],
      cross: Op[Primal[TargetComponent], Dual[SourceComponent], ComponentOperatorRole, UncheckedEvidence],
      sourceNormalization: FrameNormalization[SourceFeature, SourceComponent, CertifiedSpd],
      targetNormalization: FrameNormalization[TargetFeature, TargetComponent, CertifiedSpd]
  )

  private def programFixture(): Fixture[?, ?, ?, ?] =
    val sourceFeature = space("program-source-feature", SpaceRole.Observed, 2)
    val targetFeature = space("program-target-feature", SpaceRole.Observed, 3)
    val sourceComponent = space("program-source-component", SpaceRole.Latent, 1)
    val targetComponent = space("program-target-component", SpaceRole.Latent, 1)
    type SF = sourceFeature.Id
    type TF = targetFeature.Id
    type SK = sourceComponent.Id
    type TK = targetComponent.Id
    val sourceVariable = accepted(FrameVariable.from(ParameterId.unsafe("source"), sourceFeature.evidence, sourceComponent.evidence))
    val targetVariable = accepted(FrameVariable.from(ParameterId.unsafe("target"), targetFeature.evidence, targetComponent.evidence))
    Fixture[SF, TF, SK, TK](
      FrameParameterization.identity(sourceVariable),
      FrameParameterization.identity(targetVariable),
      component(sourceComponent.evidence, 2.0, "program-source-value"),
      certifiedComponent(sourceComponent.evidence, 4.0, "program-source-denominator"),
      crossComponent(sourceComponent.evidence, targetComponent.evidence, 0.75, "program-cross"),
      FrameNormalization(sourceVariable, certifiedMetric(sourceFeature.evidence, DMat.eye(2), "program-source-normalization")),
      FrameNormalization(targetVariable, certifiedMetric(targetFeature.evidence, DMat.eye(3), "program-target-normalization"))
    )

  private def component[K <: SemanticSpace](
      space: SpaceEvidence[K],
      value: Double,
      name: String
  ): Op[Primal[K], Dual[K], ComponentOperatorRole, UncheckedEvidence] =
    acceptedSemantic(
      Op.fromDense(
        matrix(Vector(Vector(value))),
        CoordinateEvidence.primal(space),
        CoordinateEvidence.dual(space),
        OperatorRoleWitness.component,
        id(name)
      )
    )

  private def certifiedComponent[K <: SemanticSpace](
      space: SpaceEvidence[K],
      value: Double,
      name: String
  ): Op[Primal[K], Dual[K], ComponentOperatorRole, CertifiedSpd] =
    val linear = acceptedSemantic(
      Lin.fromDenseMatrix(
        matrix(Vector(Vector(value))),
        CoordinateEvidence.primal(space),
        CoordinateEvidence.dual(space),
        id(name)
      )
    )
    acceptedSemantic(Op.certifiedSpd(Op.fromLin(linear, OperatorRoleWitness.component), acceptedSemantic(FormCertificates.spd(linear))))

  private def crossComponent[Source <: SemanticSpace, Target <: SemanticSpace](
      source: SpaceEvidence[Source],
      target: SpaceEvidence[Target],
      value: Double,
      name: String
  ): Op[Primal[Target], Dual[Source], ComponentOperatorRole, UncheckedEvidence] =
    acceptedSemantic(
      Op.fromDense(
        matrix(Vector(Vector(value))),
        CoordinateEvidence.primal(target),
        CoordinateEvidence.dual(source),
        OperatorRoleWitness.component,
        id(name)
      )
    )

  private def certifiedMetric[S <: SemanticSpace](
      space: SpaceEvidence[S],
      value: DMat,
      name: String
  ): OpMetric[S, CertifiedSpd] =
    val linear = acceptedSemantic(
      Lin.fromDenseMatrix(value, CoordinateEvidence.primal(space), CoordinateEvidence.dual(space), id(name))
    )
    acceptedSemantic(Op.certifiedSpd(Op.fromLin(linear, OperatorRoleWitness.metric), acceptedSemantic(FormCertificates.spd(linear))))

  private def assertScalarOperator(
      operator: Op[? <: Coordinate, ? <: Coordinate, ? <: OperatorRoleTag, ? <: OperatorEvidence],
      expected: Double
  ): Unit =
    val dense = acceptedSemantic(operator.toDense)
    assertEquals((dense.rows, dense.cols), (1, 1))
    assertEqualsDouble(dense(0, 0), expected, 1e-12)

  private def resultLabel(value: ResultEquivalence): String =
    value match
      case ResultEquivalence.ValueEquivalent(_) => "value"
      case ResultEquivalence.OperatorEquivalent(_, _, _) => "operator"
      case ResultEquivalence.SubspaceEquivalent(_, _) => "subspace"
      case ResultEquivalence.FrameEquivalent(_, _) => "frame"
      case ResultEquivalence.PredictionEquivalent(_, _) => "prediction"
      case ResultEquivalence.ObjectiveEquivalent(_) => "objective"

  private def accepted[A](result: Either[ProgramError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def acceptedSemantic[A](result: Either[SemanticError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def space(name: String, role: SpaceRole, dimension: Int): SpaceRef =
    SpaceRef(MvSpace(SpaceId.unsafe(name), role, Dimension.unsafe(dimension)))

  private def id(name: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(name))

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    GaleNumerics.matrixFromRows(rows)

  private def assertMatrix(actual: DMat, expected: DMat, tolerance: Double = 1e-12): Unit =
    assertEquals((actual.rows, actual.cols), (expected.rows, expected.cols))
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1
