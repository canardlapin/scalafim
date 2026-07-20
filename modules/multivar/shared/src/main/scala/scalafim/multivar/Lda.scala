package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

/** Class-incidence matrix over one sample space. Rows are simplex weights, so
  * hard labels and soft membership induce the same row-relation algebra.
  */
final class ClassIncidence private (
    val weights: DMat,
    val classMasses: DVec
):
  val samples: Int = weights.rows
  val classes: Int = weights.cols

object ClassIncidence:
  def hard(labels: Seq[Int]): Either[MultivarError, ClassIncidence] =
    val values = labels.toVector
    if values.isEmpty then Left(MultivarError.MatrixShapeMismatch("class labels must be non-empty"))
    else
      val classes = values.distinct.sorted
      if classes.length < 2 then Left(MultivarError.MatrixShapeMismatch("LDA requires at least two classes"))
      else
        val index = classes.zipWithIndex.toMap
        val out = new Array[Double](values.length * classes.length)
        var row = 0
        while row < values.length do
          out(row * classes.length + index(values(row))) = 1.0
          row += 1
        fromSimplex(GaleNumerics.matrixFromRowMajor(values.length, classes.length, out))

  def fromSimplex(weights: DMat, tolerance: Double = 1e-10): Either[MultivarError, ClassIncidence] =
    if weights.rows == 0 || weights.cols < 2 then
      Left(MultivarError.MatrixShapeMismatch("class incidence requires samples and at least two classes"))
    else if !tolerance.isFinite || tolerance < 0.0 then Left(MultivarError.InvalidTolerance("class incidence", tolerance))
    else
      val masses = new Array[Double](weights.cols)
      var row = 0
      var failure = Option.empty[MultivarError]
      while row < weights.rows && failure.isEmpty do
        var total = 0.0
        var col = 0
        while col < weights.cols && failure.isEmpty do
          val value = weights(row, col)
          if !value.isFinite || value < 0.0 then
            failure = Some(MultivarError.NonFiniteValue("class incidence", row * weights.cols + col, value))
          else
            total += value
            masses(col) += value
          col += 1
        if failure.isEmpty && Math.abs(total - 1.0) > tolerance then
          failure = Some(MultivarError.MatrixShapeMismatch(s"class-incidence row $row sums to $total instead of one"))
        row += 1
      failure match
        case Some(error) => Left(error)
        case None if masses.exists(_ <= tolerance) =>
          Left(MultivarError.MatrixShapeMismatch("every declared class must have positive incidence mass"))
        case None => Right(new ClassIncidence(weights, GaleNumerics.vectorFromArray(masses)))

final case class LdaRowRelations[Rows <: SemanticSpace](
    between: OpRowLink[Rows, Rows, CertifiedPsd],
    within: OpRowLink[Rows, Rows, CertifiedPsd]
)

object LdaRowRelations:
  def fromIncidence[Rows <: SemanticSpace](
      rows: SpaceEvidence[Rows],
      incidence: ClassIncidence,
      provenance: SemanticProvenance = SemanticProvenance.source("lda-class-relations")
  ): Either[MultivarError, LdaRowRelations[Rows]] =
    if incidence.samples != rows.dimension then
      Left(MultivarError.MatrixShapeMismatch(s"class incidence has ${incidence.samples} rows, expected ${rows.dimension}"))
    else
      val classProjection = classMeanProjection(incidence)
      val grandProjection = constantProjection(rows.dimension)
      val betweenDense = MatrixOps.subtract(classProjection, grandProjection)
      val withinDense = MatrixOps.subtract(DMat.eye(rows.dimension), classProjection)
      for
        between <- certifiedRelation(rows, betweenDense, "lda-between-class-relation", provenance)
        within <- certifiedRelation(rows, withinDense, "lda-within-class-relation", provenance)
      yield LdaRowRelations(between, within)

  private def classMeanProjection(incidence: ClassIncidence): DMat =
    val out = new Array[Double](incidence.samples * incidence.samples)
    var left = 0
    while left < incidence.samples do
      var right = 0
      while right < incidence.samples do
        var value = 0.0
        var klass = 0
        while klass < incidence.classes do
          value += incidence.weights(left, klass) * incidence.weights(right, klass) / incidence.classMasses(klass)
          klass += 1
        out(left * incidence.samples + right) = value
        right += 1
      left += 1
    GaleNumerics.matrixFromRowMajor(incidence.samples, incidence.samples, out)

  private def constantProjection(samples: Int): DMat =
    GaleNumerics.matrixFromRowMajor(samples, samples, Array.fill(samples * samples)(1.0 / samples.toDouble))

  private def certifiedRelation[Rows <: SemanticSpace](
      rows: SpaceEvidence[Rows],
      dense: DMat,
      label: String,
      provenance: SemanticProvenance
  ): Either[MultivarError, OpRowLink[Rows, Rows, CertifiedPsd]] =
    val identity = ValueIdentity.source(ValueId.unsafe(s"${rows.id.value}.$label"))
    for
      linear <- ldaSemantic(
        Lin.fromDenseMatrix(
          dense,
          CoordinateEvidence.primal(rows),
          CoordinateEvidence.dual(rows),
          identity,
          provenance.append(SemanticProvenanceEvent.Derived(label, Vector.empty))
        )
      )
      certificate <- ldaSemantic(FormCertificates.psd(linear))
      relation <- ldaSemantic(Op.certifiedPsd(Op.fromLin(linear, OperatorRoleWitness.rowLink), certificate))
    yield relation

enum WithinScatterPolicy:
  case RequirePositiveDefinite
  case FixedTraceScaledRidge(fraction: TraceRidgeFraction)

enum LdaObjective:
  case FisherRayleigh
  case TraceRatio

final case class LdaShrinkageFit(
    policy: WithinScatterPolicy,
    traceScale: Double,
    ridgeAmount: Double
):
  require(traceScale.isFinite && traceScale >= 0.0, "within-scatter scale must be finite and non-negative")
  require(ridgeAmount.isFinite && ridgeAmount >= 0.0, "within-scatter ridge must be finite and non-negative")

final case class LdaDiagnostics(
    objective: LdaObjective,
    retainedRank: Int,
    criterionValue: Double,
    residual: Double,
    spectralClusters: Vector[Vector[Int]],
    solver: String
):
  require(retainedRank > 0, "LDA must retain at least one component")
  require(criterionValue.isFinite, "LDA criterion must be finite")
  require(residual.isFinite && residual >= 0.0, "LDA residual must be finite and non-negative")

final case class LdaOperatorFit[
    Rows <: SemanticSpace,
    Feature <: SemanticSpace,
    Component <: SemanticSpace
](
    functionalFrame: FunctionalFrame[Feature, Component, UncheckedEvidence],
    programFit: OperatorProgramFit,
    between: OpScatter[Feature, CertifiedPsd],
    within: OpScatter[Feature, CertifiedPsd],
    realizedWithin: OpCovariance[Feature, CertifiedSpd],
    criterionValues: DVec,
    shrinkage: LdaShrinkageFit,
    diagnostics: LdaDiagnostics,
    provenance: SemanticProvenance
):
  def scores(table: OpTable[Rows, Feature, ? <: OperatorEvidence]):
      Op[Primal[Component], Primal[Rows], ScoreOperatorRole, UncheckedEvidence] =
    functionalFrame.scores(table)

/** LDA is a typed assembly of class relations and two second-order operators.
  * It owns no eigensolver: Fisher delegates to [[GeneralizedRayleighRitz]] and
  * trace-ratio delegates to [[TraceRatioOptimization]].
  */
final class LdaProblem[Rows <: SemanticSpace, Feature <: SemanticSpace] private (
    val rowSpace: SpaceEvidence[Rows],
    val featureSpace: SpaceEvidence[Feature],
    val table: OpTable[Rows, Feature, UncheckedEvidence],
    val incidence: ClassIncidence,
    val relations: LdaRowRelations[Rows],
    val between: OpScatter[Feature, CertifiedPsd],
    val within: OpScatter[Feature, CertifiedPsd],
    val realizedWithin: OpCovariance[Feature, CertifiedSpd],
    val euclideanGeometry: OpCometric[Feature, CertifiedSpd],
    val shrinkage: LdaShrinkageFit,
    val provenance: SemanticProvenance
):
  val maximumComponents: Int = Math.min(featureSpace.dimension, incidence.classes - 1)

  def fit(
      components: ComponentCount,
      objective: LdaObjective = LdaObjective.FisherRayleigh,
      rankTolerance: SpectralRankTolerance = SpectralRankTolerance.default,
      generalizedSolver: GeneralizedEigenSolver = DenseSolvers.generalizedEigen,
      symmetricSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen
  ): Either[MultivarError, LdaOperatorFit[Rows, Feature, ? <: SemanticSpace]] =
    if components.value > maximumComponents then Left(MultivarError.InvalidComponentRequest(components.value, maximumComponents))
    else
      for
        betweenDense <- ldaSemantic(between.toDense)
        withinDense <- ldaSemantic(realizedWithin.toDense)
        solved <- objective match
          case LdaObjective.FisherRayleigh =>
            GeneralizedRayleighRitz
              .solve(betweenDense, withinDense, components, rankTolerance, solver = generalizedSolver)
              .map: result =>
                LdaSolved(
                  result.vectors,
                  result.values,
                  sum(result.values),
                  Math.max(result.diagnostics.generalizedResidual, result.diagnostics.normalizationResidual),
                  result.diagnostics.spectralClusters,
                  result.diagnostics.solver
                )
          case LdaObjective.TraceRatio =>
            TraceRatioOptimization.solve(betweenDense, withinDense, components, solver = symmetricSolver).map: result =>
              LdaSolved(
                result.vectors,
                componentRatios(betweenDense, withinDense, result.vectors),
                result.value,
                result.stationarityResidual,
                result.spectralClusters,
                "multivar.trace-ratio[gale.symmetric-eigen]"
              )
        component <- SpaceRef.of(s"${featureSpace.id.value}.lda", SpaceRole.Latent, solved.vectors.cols)
        fit <- assemble(component, objective, solved)
      yield fit

  private def assemble(
      component: SpaceRef,
      objective: LdaObjective,
      solved: LdaSolved
  ): Either[MultivarError, LdaOperatorFit[Rows, Feature, component.Id]] =
    val frameIdentity = ValueIdentity.derived("lda-frame", between.valueIdentity, realizedWithin.valueIdentity)
    val fitProvenance = provenance.append(
      SemanticProvenanceEvent.Derived("lda-operator-program-fit", Vector(between.valueIdentity, realizedWithin.valueIdentity))
    )
    for
      variable <- ldaProgram(FrameVariable.from(ParameterId.unsafe(s"${featureSpace.id.value}.lda-frame"), featureSpace, component.evidence))
      frame <- ldaSemantic(
        Op.fromDense(
          solved.vectors,
          CoordinateEvidence.primal(component.evidence),
          CoordinateEvidence.dual(featureSpace),
          OperatorRoleWitness.frame,
          frameIdentity,
          fitProvenance
        )
      )
      functionalFrame = FunctionalFrame(frame, Some(euclideanGeometry))
      parameterization = FrameParameterization.identity(variable)
      operatorProgram <- objective match
        case LdaObjective.FisherRayleigh =>
          ldaProgram(
            OperatorPrograms.ldaRayleigh(
              parameterization,
              between,
              realizedWithin,
              FrameNormalization(variable, realizedWithin)
            )
          )
        case LdaObjective.TraceRatio =>
          ldaProgram(
            OperatorPrograms.ldaTraceRatio(
              parameterization,
              between,
              realizedWithin,
              FrameNormalization(variable, euclideanGeometry)
            )
          )
      context <- ldaSemantic(
        CertificateContext.from(
          CertificateTolerance.strict,
          CertificateNorm.Frobenius,
          "lda-fit",
          "gale",
          NumericalPrecision.Float64,
          Some(shrinkageLabel)
        )
      )
      identifiability = NumericalIdentifiability(
        solved.vectors.cols,
        solved.clusters,
        solved.residual,
        context
      )
      genericFit <- ldaProgram(
        OperatorProgramFit.from(
          operatorProgram,
          Vector(FittedFrame(variable, functionalFrame)),
          solved.objectiveValue,
          identifiability,
          fitProvenance
        )
      )
    yield
      LdaOperatorFit(
        functionalFrame,
        genericFit,
        between,
        within,
        realizedWithin,
        solved.values,
        shrinkage,
        LdaDiagnostics(
          objective,
          solved.vectors.cols,
          solved.objectiveValue,
          solved.residual,
          solved.clusters,
          solved.solver
        ),
        fitProvenance
      )

  private def shrinkageLabel: String =
    shrinkage.policy match
      case WithinScatterPolicy.RequirePositiveDefinite => "require-positive-definite"
      case WithinScatterPolicy.FixedTraceScaledRidge(fraction) => s"fixed-trace-scaled-${fraction.value}"

object LdaProblem:
  def fromTable[Rows <: SemanticSpace, Feature <: SemanticSpace](
      rowSpace: SpaceEvidence[Rows],
      featureSpace: SpaceEvidence[Feature],
      table: OpTable[Rows, Feature, UncheckedEvidence],
      incidence: ClassIncidence,
      withinPolicy: WithinScatterPolicy,
      provenance: SemanticProvenance = SemanticProvenance.source("lda-problem")
  ): Either[MultivarError, LdaProblem[Rows, Feature]] =
    if table.codomain.descriptor.space != rowSpace.descriptor || table.domain.descriptor.space != featureSpace.descriptor then
      Left(MultivarError.MatrixShapeMismatch("LDA table endpoints do not match its declared spaces"))
    else
      for
        relations <- LdaRowRelations.fromIncidence(rowSpace, incidence, provenance)
        betweenUnchecked = OperatorAlgebra
          .secondOrder(table, relations.between, table)
          .retag(OperatorRoleWitness.scatter, "lda-between-scatter")
        withinUnchecked = OperatorAlgebra
          .secondOrder(table, relations.within, table)
          .retag(OperatorRoleWitness.scatter, "lda-within-scatter")
        between <- certifyPsd(featureSpace, betweenUnchecked)
        within <- certifyPsd(featureSpace, withinUnchecked)
        realized <- realizeWithin(featureSpace, within, withinPolicy, provenance)
        (realizedWithin, shrinkage) = realized
        euclidean <- identityCometric(featureSpace, provenance)
      yield
        new LdaProblem(
          rowSpace,
          featureSpace,
          table,
          incidence,
          relations,
          between,
          within,
          realizedWithin,
          euclidean,
          shrinkage,
          provenance
        )

  def fromMatrix(
      matrix: DMat,
      incidence: ClassIncidence,
      withinPolicy: WithinScatterPolicy,
      id: String = "lda"
  ): Either[MultivarError, PreparedLdaProblem] =
    for
      rows <- SpaceRef.of(s"$id.rows", SpaceRole.Samples, matrix.rows)
      features <- SpaceRef.of(s"$id.features", SpaceRole.Observed, matrix.cols)
      table <- ldaSemantic(
        Op.fromDense(
          matrix,
          CoordinateEvidence.dual(features.evidence),
          CoordinateEvidence.primal(rows.evidence),
          OperatorRoleWitness.table,
          ValueIdentity.source(ValueId.unsafe(s"$id.table"))
        )
      )
      problem <- fromTable(rows.evidence, features.evidence, table, incidence, withinPolicy)
    yield new PreparedLdaProblem(rows, features)(problem)

  private def certifyPsd[Feature <: SemanticSpace, R <: OperatorRoleTag](
      feature: SpaceEvidence[Feature],
      value: Op[Dual[Feature], Primal[Feature], R, UncheckedEvidence]
  ): Either[MultivarError, Op[Dual[Feature], Primal[Feature], R, CertifiedPsd]] =
    for
      dense <- ldaSemantic(value.toDense)
      linear <- ldaSemantic(
        Lin.fromDenseMatrix(dense, CoordinateEvidence.dual(feature), CoordinateEvidence.primal(feature), value.valueIdentity, value.provenance)
      )
      certificate <- ldaSemantic(FormCertificates.psd(linear))
      certified <- ldaSemantic(Op.certifiedPsd(value, certificate))
    yield certified

  private def realizeWithin[Feature <: SemanticSpace](
      feature: SpaceEvidence[Feature],
      within: OpScatter[Feature, CertifiedPsd],
      policy: WithinScatterPolicy,
      provenance: SemanticProvenance
  ): Either[MultivarError, (OpCovariance[Feature, CertifiedSpd], LdaShrinkageFit)] =
    for
      dense <- ldaSemantic(within.toDense)
      current <- policy match
        case WithinScatterPolicy.RequirePositiveDefinite =>
          certifySpd(feature, dense, within.valueIdentity, provenance).map(_ -> LdaShrinkageFit(policy, trace(dense) / feature.dimension.toDouble, 0.0))
        case WithinScatterPolicy.FixedTraceScaledRidge(fraction) =>
          val scale = trace(dense) / feature.dimension.toDouble
          if !scale.isFinite || scale <= 0.0 then Left(MultivarError.NonInvertibleValue("within-scatter trace", 0, scale))
          else
            val ridge = fraction.value * scale
            certifySpd(feature, MatrixOps.addRidge(dense, ridge), within.valueIdentity, provenance)
              .map(_ -> LdaShrinkageFit(policy, scale, ridge))
    yield current

  private def certifySpd[Feature <: SemanticSpace](
      feature: SpaceEvidence[Feature],
      dense: DMat,
      source: ValueIdentity,
      provenance: SemanticProvenance
  ): Either[MultivarError, OpCovariance[Feature, CertifiedSpd]] =
    val identity = ValueIdentity.derived("lda-realized-within", source)
    for
      linear <- ldaSemantic(
        Lin.fromDenseMatrix(
          dense,
          CoordinateEvidence.dual(feature),
          CoordinateEvidence.primal(feature),
          identity,
          provenance.append(SemanticProvenanceEvent.Derived("within-scatter-policy", Vector(source)))
        )
      )
      certificate <- ldaSemantic(FormCertificates.spd(linear))
      certified <- ldaSemantic(Op.certifiedSpd(Op.fromLin(linear, OperatorRoleWitness.covariance), certificate))
    yield certified

  private def identityCometric[Feature <: SemanticSpace](
      feature: SpaceEvidence[Feature],
      provenance: SemanticProvenance
  ): Either[MultivarError, OpCometric[Feature, CertifiedSpd]] =
    val identity = ValueIdentity.source(ValueId.unsafe(s"${feature.id.value}.lda-euclidean-cometric"))
    for
      linear <- ldaSemantic(
        Lin.fromDenseMatrix(
          DMat.eye(feature.dimension),
          CoordinateEvidence.dual(feature),
          CoordinateEvidence.primal(feature),
          identity,
          provenance
        )
      )
      certificate <- ldaSemantic(FormCertificates.spd(linear))
      certified <- ldaSemantic(Op.certifiedSpd(Op.fromLin(linear, OperatorRoleWitness.cometric), certificate))
    yield certified

final class PreparedLdaProblem private[multivar] (
    val rows: SpaceRef,
    val features: SpaceRef
)(
    val value: LdaProblem[rows.Id, features.Id]
):
  def fit(
      components: ComponentCount,
      objective: LdaObjective = LdaObjective.FisherRayleigh
  ): Either[MultivarError, LdaOperatorFit[rows.Id, features.Id, ? <: SemanticSpace]] =
    value.fit(components, objective)

private final case class LdaSolved(
    vectors: DMat,
    values: DVec,
    objectiveValue: Double,
    residual: Double,
    clusters: Vector[Vector[Int]],
    solver: String
)

private def componentRatios(between: DMat, within: DMat, vectors: DMat): DVec =
  val out = new Array[Double](vectors.cols)
  var col = 0
  while col < vectors.cols do
    var numerator = 0.0
    var denominator = 0.0
    var left = 0
    while left < vectors.rows do
      var right = 0
      while right < vectors.rows do
        numerator += vectors(left, col) * between(left, right) * vectors(right, col)
        denominator += vectors(left, col) * within(left, right) * vectors(right, col)
        right += 1
      left += 1
    out(col) = numerator / denominator
    col += 1
  GaleNumerics.vectorFromArray(out)

private def sum(values: DVec): Double =
  var total = 0.0
  var index = 0
  while index < values.length do
    total += values(index)
    index += 1
  total

private def ldaSemantic[A](result: Either[SemanticError, A]): Either[MultivarError, A] =
  result.left.map:
    case SemanticError.MultivarFailure(error) => error
    case SemanticError.LinearMapFailure(error) => LinalgErrorAdapter.toMultivarError(error)
    case error => MultivarError.SolverFailed(error.message)

private def ldaProgram[A](result: Either[ProgramError, A]): Either[MultivarError, A] =
  result.left.map(error => MultivarError.SolverFailed(error.message))
