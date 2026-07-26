package scalafim.multivar
package optimization

import scalafim.multivar.core.*
import scalafim.multivar.contract.*
import scalafim.multivar.optimization.*

import scala.compiletime.testing.typeCheckErrors

import gale.linalg.DMat

class OperatorProgramSuite extends munit.FunSuite:

  test("sparse-group is correctly classified as a degree-one convex penalty"):
    val functional = FunctionalKind.SparseGroup(
      UnitFraction.unsafe(0.4),
      ValueIdentity.source(ValueId.unsafe("sparse-group-homogeneity"))
    )

    assertEquals(functional.traits.convexity, ConvexityTrait.Convex)
    assertEquals(functional.traits.homogeneity, HomogeneityTrait.DegreeOne)

  test("every closed base objective retains its typed dense operators"):
    val fixture = programFixture()
    val source = SelfCompressionExpression(fixture.source.variable, fixture.sourceValue)
    val denominator = SelfCompressionExpression(fixture.source.variable, fixture.sourceDenominator)
    val cross = CrossCompressionExpression(fixture.source.variable, fixture.target.variable, fixture.cross)
    val objectives = Vector[BaseObjective](
      BaseObjective.MaximizeTrace(source),
      BaseObjective.MaximizeCrossTrace(cross),
      BaseObjective.GeneralizedRayleigh(source, denominator),
      BaseObjective.TraceRatio(source, denominator),
      BaseObjective.RatioTrace(source, denominator),
      BaseObjective.MinimizeDisagreement(source),
      BaseObjective.SequentialCrossRegression(cross, denominator)
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
    assertScalarOperator(source.evaluate(fixture.sourceFrame), 1.75)
    assertScalarOperator(denominator.evaluate(fixture.sourceFrame), 4.5)
    assertScalarOperator(cross.evaluate(fixture.sourceFrame, fixture.targetFrame), 0.95)
    objectives.foreach:
      case BaseObjective.MaximizeTrace(_) => ()
      case BaseObjective.MaximizeCrossTrace(_) => ()
      case BaseObjective.GeneralizedRayleigh(_, _) => ()
      case BaseObjective.TraceRatio(_, _) => ()
      case BaseObjective.RatioTrace(_, _) => ()
      case BaseObjective.MinimizeDisagreement(_) => ()
      case BaseObjective.SequentialCrossRegression(_, _) => ()

  test("GPCA, LDA, CCA, PLSC, RRR, and multiset builders compile to one program type"):
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
    val rrr = accepted(
      OperatorPrograms.reducedRankRegression(
        fixture.source,
        fixture.target,
        fixture.cross,
        fixture.sourceDenominator,
        fixture.sourceNormalization,
        fixture.targetNormalization
      )
    )
    val multiset = accepted(
      OperatorPrograms.multiset(fixture.source, fixture.sourceValue, fixture.sourceNormalization)
    )

    assertEquals(
      Vector(gpca, lda, traceRatio, cca, plsc, rrr, multiset).map(_.objective.label),
      Vector(
        "maximize-trace",
        "generalized-rayleigh",
        "trace-ratio",
        "maximize-cross-trace",
        "maximize-cross-trace",
        "sequential-cross-regression",
        "maximize-trace"
      )
    )
    assert(Vector(gpca, lda, traceRatio, cca, plsc, rrr, multiset).forall(_.isInstanceOf[OperatorProgram]))
    assert(rrr.resultSemantics.equivalence.isInstanceOf[ResultEquivalence.PredictionEquivalent])

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
    val objective = BaseObjective.MaximizeTrace(
      SelfCompressionExpression(fixture.source.variable, fixture.sourceValue)
    )
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
        CrossCompressionExpression(fixture.source.variable, fixture.source.variable, fixture.sourceValue)
      ),
      Vector(fixture.sourceNormalization)
    )

    assert(duplicate.left.exists(_.isInstanceOf[ProgramError.DuplicateParameter]))
    assert(missing.left.exists(_.isInstanceOf[ProgramError.MissingNormalization]))
    assert(duplicateNormalization.left.exists(_.isInstanceOf[ProgramError.DuplicateNormalization]))
    assert(unknown.left.exists(_.isInstanceOf[ProgramError.UnknownParameter]))
    assert(collapsedPair.left.exists(_.isInstanceOf[ProgramError.InvalidParameterization]))

  test("every objective operand must bind the declared parameter identity"):
    val fixture = programFixture()
    val foreignVariable = accepted(
      FrameVariable.from(
        ParameterId.unsafe("foreign-source"),
        fixture.source.variable.featureSpace,
        fixture.source.variable.componentSpace
      )
    )
    val numerator = SelfCompressionExpression(fixture.source.variable, fixture.sourceValue)
    val foreignDenominator = SelfCompressionExpression(foreignVariable, fixture.sourceDenominator)
    val cross = CrossCompressionExpression(fixture.source.variable, fixture.target.variable, fixture.cross)
    val ratio = OperatorProgram.from(
      Vector(fixture.source),
      BaseObjective.GeneralizedRayleigh(numerator, foreignDenominator),
      Vector(fixture.sourceNormalization)
    )
    val regression = OperatorProgram.from(
      Vector(fixture.source, fixture.target),
      BaseObjective.SequentialCrossRegression(cross, foreignDenominator),
      Vector(fixture.sourceNormalization, fixture.targetNormalization)
    )

    assert(ratio.left.exists:
      case ProgramError.InvalidParameterization(reason) => reason.contains("same frame parameter")
      case _ => false
    )
    assert(regression.left.exists:
      case ProgramError.InvalidParameterization(reason) => reason.contains("predictor")
      case _ => false
    )

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
        BaseObjective.MaximizeTrace(SelfCompressionExpression(fixture.source.variable, fixture.sourceValue)),
        Vector(fixture.sourceNormalization),
        penalties = Vector(l1)
      )
    )
    val regression = accepted(
      OperatorProgram.from(
        Vector(fixture.source, fixture.target),
        BaseObjective.SequentialCrossRegression(
          CrossCompressionExpression(fixture.source.variable, fixture.target.variable, fixture.cross),
          SelfCompressionExpression(fixture.source.variable, fixture.sourceDenominator)
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
    assertEquals(smooth.resultSemantics.requestedClaim, RequestedOptimizationClaim.ExactGlobal)
    assertEquals(sparse.resultSemantics.requestedClaim, RequestedOptimizationClaim.Stationary)

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
        featureOperator(feature.evidence, matrix(Vector(Vector(2.0, 0.0), Vector(0.0, 1.0))), "fit-value"),
        FrameNormalization(variable, certifiedCovariance(feature.evidence, DMat.eye(2), "fit-normalization"))
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
      OperatorProgramFit.exactSpectral(
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
    assertEquals(result.achievedGuarantee.claimClass, OptimizationClaimClass.ExactGlobal)
    assert(result.achievedGuarantee.isInstanceOf[AchievedOptimizationGuarantee.ExactGlobal])
    assertEquals(result.solverAttestation.certificate.claim.property, "converged")
    assertMatrix(
      acceptedSemantic(fitted.frame.scores(table).toDense),
      matrix(Vector(Vector(2.0), Vector(-1.0)))
    )
    assert(
      OperatorProgramFit.exactSpectral(
        program,
        Vector(fitted),
        Double.NaN,
        identifiability,
        SemanticProvenance.source("bad-fit-result")
      ).isLeft
    )
    assert(
      OperatorProgramFit.exactSpectral(
        program,
        Vector(fitted),
        2.0,
        identifiability.copy(residual = 1.0),
        SemanticProvenance.source("uncertified-fit-result")
      ).left.exists:
        case ProgramError.InvalidResult(reason) => reason.contains("solver convergence was not certified")
        case _ => false
    )

  test("nominal feature spaces prevent invalid normalization at compile time"):
    val errors = typeCheckErrors("""
      import scalafim.multivar.core.*
      import scalafim.multivar.contract.*
      import scalafim.multivar.optimization.*
      import scalafim.multivar.solver.*
      import scalafim.multivar.lifecycle.*
      import scalafim.multivar.capability.*
      import scalafim.multivar.family.spectral.*
      import scalafim.multivar.family.paired.*
      import scalafim.multivar.family.canonical.*
      import scalafim.multivar.family.cpca.*
      import scalafim.multivar.family.sparse.*
      import scalafim.multivar.family.glrm.*
      import scalafim.multivar.family.multiblock.*
      import scalafim.multivar.family.kernel.*
      import scalafim.multivar.workflow.*
      val f = SpaceRef(MvSpace(SpaceId.unsafe("f"), SpaceRole.Observed, Dimension.unsafe(2)))
      val g = SpaceRef(MvSpace(SpaceId.unsafe("g"), SpaceRole.Observed, Dimension.unsafe(2)))
      val k = SpaceRef(MvSpace(SpaceId.unsafe("k"), SpaceRole.Latent, Dimension.unsafe(1)))
      type F = f.Id
      type G = g.Id
      type K = k.Id
      val variable: FrameVariable[F, K] = ???
      val metric: OpCovariance[G, CertifiedSpd] = ???
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
      sourceValue: Op[Dual[SourceFeature], Primal[SourceFeature], CovarianceOperatorRole, UncheckedEvidence],
      sourceDenominator: Op[Dual[SourceFeature], Primal[SourceFeature], CovarianceOperatorRole, CertifiedSpd],
      cross: Op[Dual[TargetFeature], Primal[SourceFeature], CrossOperatorRole, UncheckedEvidence],
      sourceNormalization: FrameNormalization[SourceFeature, SourceComponent, CertifiedSpd],
      targetNormalization: FrameNormalization[TargetFeature, TargetComponent, CertifiedSpd],
      sourceFrame: OpFrame[SourceFeature, SourceComponent, UncheckedEvidence],
      targetFrame: OpFrame[TargetFeature, TargetComponent, UncheckedEvidence]
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
      featureOperator(
        sourceFeature.evidence,
        matrix(Vector(Vector(2.0, 0.5), Vector(0.5, 1.0))),
        "program-source-value"
      ),
      certifiedCovariance(
        sourceFeature.evidence,
        matrix(Vector(Vector(4.0, 0.0), Vector(0.0, 2.0))),
        "program-source-denominator"
      ),
      crossFeatureOperator(
        sourceFeature.evidence,
        targetFeature.evidence,
        matrix(Vector(Vector(0.75, 0.2, -0.1), Vector(0.5, -0.4, 0.3))),
        "program-cross"
      ),
      FrameNormalization(sourceVariable, certifiedCovariance(sourceFeature.evidence, DMat.eye(2), "program-source-normalization")),
      FrameNormalization(targetVariable, certifiedCovariance(targetFeature.evidence, DMat.eye(3), "program-target-normalization")),
      frame(
        sourceFeature.evidence,
        sourceComponent.evidence,
        matrix(Vector(Vector(1.0), Vector(-0.5))),
        "program-source-frame"
      ),
      frame(
        targetFeature.evidence,
        targetComponent.evidence,
        matrix(Vector(Vector(1.0), Vector(0.5), Vector(-1.0))),
        "program-target-frame"
      )
    )

  private def featureOperator[F <: SemanticSpace](
      space: SpaceEvidence[F],
      value: DMat,
      name: String
  ): Op[Dual[F], Primal[F], CovarianceOperatorRole, UncheckedEvidence] =
    acceptedSemantic(
      Op.fromDense(
        value,
        CoordinateEvidence.dual(space),
        CoordinateEvidence.primal(space),
        OperatorRoleWitness.covariance,
        id(name)
      )
    )

  private def certifiedCovariance[F <: SemanticSpace](
      space: SpaceEvidence[F],
      value: DMat,
      name: String
  ): Op[Dual[F], Primal[F], CovarianceOperatorRole, CertifiedSpd] =
    val linear = acceptedSemantic(
      Lin.fromDenseMatrix(
        value,
        CoordinateEvidence.dual(space),
        CoordinateEvidence.primal(space),
        id(name)
      )
    )
    acceptedSemantic(Op.certifiedSpd(Op.fromLin(linear, OperatorRoleWitness.covariance), acceptedSemantic(FormCertificates.spd(linear))))

  private def crossFeatureOperator[Source <: SemanticSpace, Target <: SemanticSpace](
      source: SpaceEvidence[Source],
      target: SpaceEvidence[Target],
      value: DMat,
      name: String
  ): Op[Dual[Target], Primal[Source], CrossOperatorRole, UncheckedEvidence] =
    acceptedSemantic(
      Op.fromDense(
        value,
        CoordinateEvidence.dual(target),
        CoordinateEvidence.primal(source),
        OperatorRoleWitness.cross,
        id(name)
      )
    )

  private def frame[F <: SemanticSpace, K <: SemanticSpace](
      feature: SpaceEvidence[F],
      component: SpaceEvidence[K],
      value: DMat,
      name: String
  ): OpFrame[F, K, UncheckedEvidence] =
    acceptedSemantic(
      Op.fromDense(
        value,
        CoordinateEvidence.primal(component),
        CoordinateEvidence.dual(feature),
        OperatorRoleWitness.frame,
        id(name)
      )
    )

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
