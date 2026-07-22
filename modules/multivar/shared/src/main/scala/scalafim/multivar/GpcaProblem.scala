package scalafim.multivar

import gale.backend.Backend.given
import gale.linalg.CholeskyOptions
import gale.linalg.DMat
import gale.linalg.DVec

/** Relative cutoff used to separate the fitted GPCA range from numerical null space. */
opaque type GpcaRankTolerance = Double

object GpcaRankTolerance:
  def from(value: Double): Either[MultivarError, GpcaRankTolerance] =
    if value.isFinite && value >= 0.0 then Right(value)
    else Left(MultivarError.InvalidTolerance("GPCA rank", value))

  val default: GpcaRankTolerance =
    1e-12

  private[multivar] def fromBackend(backend: GpcaBackend): Either[MultivarError, GpcaRankTolerance] =
    backend match
      case GpcaBackend.Eigen(value) => from(value)
      case GpcaBackend.Auto         => Right(default)

  extension (tolerance: GpcaRankTolerance)
    inline def value: Double = tolerance

final case class GpcaNumericalDiagnostics(
    retainedRank: Int,
    generalizedResidual: Double,
    normalizationResidual: Double,
    spectralClusters: Vector[Vector[Int]],
    solver: String
):
  require(retainedRank > 0, "GPCA retained rank must be positive")
  require(generalizedResidual.isFinite && generalizedResidual >= 0.0, "GPCA residual must be finite and non-negative")
  require(normalizationResidual.isFinite && normalizationResidual >= 0.0, "GPCA normalization residual must be finite and non-negative")
  require(spectralClusters.flatten.length == retainedRank, "GPCA spectral clusters must partition the retained range")
  require(solver.nonEmpty, "GPCA solver label must be non-empty")

enum GpcaBackend:
  case Eigen(rankTolerance: Double = GpcaRankTolerance.default.value)
  case Auto

/** Executable result of the GPCA operator program.
  *
  * `functionalFrame` is the fitted covector frame `W = Q V`. Scores and axes
  * remain derived views of that one fitted value; no parallel numerical fit
  * record is assembled.
  */
final case class GpcaOperatorFit[
    Rows <: SemanticSpace,
    Feature <: SemanticSpace,
    Component <: SemanticSpace
](
    functionalFrame: FunctionalFrame[Feature, Component, UncheckedEvidence],
    programFit: OperatorProgramFit,
    covariance: OpCovariance[Feature, CertifiedPsd],
    featureMetric: OpMetric[Feature, CertifiedSpd],
    featureCometric: OpCometric[Feature, CertifiedSpd],
    generalizedEigenvalues: DVec,
    singularValues: DVec,
    diagnostics: GpcaNumericalDiagnostics,
    provenance: SemanticProvenance
):
  def scores(table: OpTable[Rows, Feature, ? <: OperatorEvidence]):
      Op[Primal[Component], Primal[Rows], ScoreOperatorRole, UncheckedEvidence] =
    functionalFrame.scores(table)

  def axes: Option[Op[Primal[Component], Primal[Feature], AxisOperatorRole, UncheckedEvidence]] =
    functionalFrame.axes

  def toBundle(
      table: OpTable[Rows, Feature, ? <: OperatorEvidence]
  ): Either[MultivarError, OperatorFitBundle] =
    for
      covarianceSnapshot <- OperatorSnapshot.from("covariance", DerivedOperatorKind.SecondOrder, covariance)
      scoreSnapshot <- OperatorSnapshot.from("scores", DerivedOperatorKind.Scores, scores(table))
      axisSnapshots <- axes match
        case Some(value) => OperatorSnapshot.from("axes", DerivedOperatorKind.Axes, value).map(Vector(_))
        case None        => Right(Vector.empty)
      generalized <- FitDiagnostic.from("generalized-residual", diagnostics.generalizedResidual)
      normalization <- FitDiagnostic.from("normalization-residual", diagnostics.normalizationResidual)
      bundle <- OperatorFitBundle.from(
        programFit,
        Vector(covarianceSnapshot, scoreSnapshot) ++ axisSnapshots,
        Vector(generalized, normalization),
        provenance
      )
    yield bundle

/** A GPCA problem over one typed table and its two declared geometries.
  *
  * The sufficient statistic is constructed only as `secondOrder(X, M, X)`.
  * The optimization variable is a functional frame normalized by the feature
  * cometric `Q^-1`; the Gale capability solves `S W = Q^-1 W Lambda`.
  */
final class GpcaProblem[Rows <: SemanticSpace, Feature <: SemanticSpace] private (
    val rowSpace: SpaceEvidence[Rows],
    val featureSpace: SpaceEvidence[Feature],
    val table: OpTable[Rows, Feature, UncheckedEvidence],
    val rowMetric: OpMetric[Rows, CertifiedSpd],
    val rowRelationship: OpRowLink[Rows, Rows, CertifiedSpd],
    val featureMetric: OpMetric[Feature, CertifiedSpd],
    val featureCometric: OpCometric[Feature, CertifiedSpd],
    val covariance: OpCovariance[Feature, CertifiedPsd],
    val provenance: SemanticProvenance
):
  def fit(
      components: ComponentCount,
      rankTolerance: GpcaRankTolerance = GpcaRankTolerance.default,
      solver: GeneralizedEigenSolver = DenseSolvers.generalizedEigen
  ): Either[MultivarError, GpcaOperatorFit[Rows, Feature, ? <: SemanticSpace]] =
    val limit = Math.min(table.rows, table.cols)
    if components.value > limit then Left(MultivarError.InvalidComponentRequest(components.value, limit))
    else
      for
        covarianceDense <- gpcaSemantic(covariance.toDense)
        normalizationDense <- gpcaSemantic(featureCometric.toDense)
        rayleigh <- GeneralizedRayleighRitz.solve(
          covarianceDense,
          normalizationDense,
          components,
          SpectralRankTolerance.unsafe(rankTolerance.value),
          solver = solver
        )
        component <- SpaceRef.of(s"${featureSpace.id.value}.gpca", SpaceRole.Latent, rayleigh.values.length)
        fit <- assemble(
          component,
          rayleigh,
          rankTolerance
        )
      yield fit

  private def assemble(
      component: SpaceRef,
      rayleigh: RayleighRitzResult,
      rankTolerance: GpcaRankTolerance
  ): Either[MultivarError, GpcaOperatorFit[Rows, Feature, component.Id]] =
    val eigenvalues = rayleigh.values
    val weights = rayleigh.vectors
    val frameIdentity = ValueIdentity.derived("gpca-frame", covariance.valueIdentity, featureCometric.valueIdentity)
    val fitProvenance = provenance.append(
      SemanticProvenanceEvent.Derived(
        "gale-generalized-rayleigh-ritz",
        Vector(covariance.valueIdentity, featureCometric.valueIdentity)
      )
    )
    for
      variable <- program(
        FrameVariable.from(ParameterId.unsafe(s"${featureSpace.id.value}.gpca-frame"), featureSpace, component.evidence)
      )
      frameOperator <- gpcaSemantic(
        Op.fromDense(
          weights,
          CoordinateEvidence.primal(component.evidence),
          CoordinateEvidence.dual(featureSpace),
          OperatorRoleWitness.frame,
          frameIdentity,
          fitProvenance
        )
      )
      functionalFrame = FunctionalFrame(frameOperator, Some(featureCometric))
      parameterization = FrameParameterization.identity(variable)
      normalization = FrameNormalization(variable, featureCometric)
      operatorProgram <- program(OperatorPrograms.gpca(parameterization, covariance, normalization))
      singularValues = squareRoots(eigenvalues)
      generalizedResidual = rayleigh.diagnostics.generalizedResidual
      normalizationResidual = rayleigh.diagnostics.normalizationResidual
      tolerance = CertificateTolerance.strict
      context <- gpcaSemantic(
        CertificateContext.from(
          tolerance,
          CertificateNorm.Frobenius,
          "gpca-generalized-eigenfit",
          "gale",
          NumericalPrecision.Float64,
          Some(s"rank-tolerance=${rankTolerance.value}")
        )
      )
      clusters = rayleigh.diagnostics.spectralClusters
      identifiability = NumericalIdentifiability(
        eigenvalues.length,
        clusters,
        Math.max(generalizedResidual, normalizationResidual),
        context
      )
      operatorFit <- program(
        OperatorProgramFit.exactSpectral(
          operatorProgram,
          Vector(FittedFrame(variable, functionalFrame)),
          sum(eigenvalues),
          identifiability,
          fitProvenance
        )
      )
    yield
      GpcaOperatorFit(
        functionalFrame,
        operatorFit,
        covariance,
        featureMetric,
        featureCometric,
        eigenvalues,
        singularValues,
        GpcaNumericalDiagnostics(
          eigenvalues.length,
          generalizedResidual,
          normalizationResidual,
          clusters,
          "gale.spectral.Eigen.eigSymmetricGeneralized"
        ),
        fitProvenance
      )

object GpcaProblem:
  private[multivar] def fromPrepared[Rows <: SemanticSpace, Feature <: SemanticSpace](
      rowSpace: SpaceEvidence[Rows],
      featureSpace: SpaceEvidence[Feature],
      tableView: MatrixView,
      rowMetricValue: MetricSpec,
      featureMetricValue: MetricSpec,
      sourceIdentity: ValueIdentity,
      provenance: SemanticProvenance
  ): Either[DiagramError, GpcaProblem[Rows, Feature]] =
    val tableIdentity = ValueIdentity.derived("prepared-gpca-table", sourceIdentity)
    val rowIdentity = ValueIdentity.derived("prepared-gpca-row-metric", sourceIdentity)
    val featureIdentity = ValueIdentity.derived("prepared-gpca-feature-metric", sourceIdentity)
    for
      table <- semanticDiagram(
        Op.fromMatrixView(
          tableView,
          CoordinateEvidence.dual(featureSpace),
          CoordinateEvidence.primal(rowSpace),
          OperatorRoleWitness.table,
          tableIdentity,
          provenance
        )
      )
      rowDense <- multivarDiagram(rowMetricValue.toDense())
      rowMetric <- certifiedMetric(rowSpace, rowDense, rowIdentity, provenance)
      featureDense <- multivarDiagram(featureMetricValue.toDense())
      featureMetric <- certifiedMetric(featureSpace, featureDense, featureIdentity, provenance)
      featureCometric <- certifiedCometric(featureSpace, featureDense, featureIdentity, provenance)
      rowRelationship = rowMetric.retag(OperatorRoleWitness.rowLink, "gpca-row-relationship")
      covarianceUnchecked = OperatorAlgebra
        .secondOrder(table, rowRelationship, table)
        .retag(OperatorRoleWitness.covariance, "gpca-covariance")
      covariance <- certifiedCovariance(covarianceUnchecked, featureSpace)
    yield
      new GpcaProblem(
        rowSpace,
        featureSpace,
        table,
        rowMetric,
        rowRelationship,
        featureMetric,
        featureCometric,
        covariance,
        provenance
      )

  private def certifiedMetric[S <: SemanticSpace](
      space: SpaceEvidence[S],
      dense: DMat,
      identity: ValueIdentity,
      provenance: SemanticProvenance
  ): Either[DiagramError, OpMetric[S, CertifiedSpd]] =
    for
      linear <- semanticDiagram(
        Lin.fromDenseMatrix(
          dense,
          CoordinateEvidence.primal(space),
          CoordinateEvidence.dual(space),
          identity,
          provenance
        )
      )
      certificate <- semanticDiagram(FormCertificates.spd(linear))
      metric <- semanticDiagram(Op.certifiedSpd(Op.fromLin(linear, OperatorRoleWitness.metric), certificate))
    yield metric

  private def certifiedCometric[S <: SemanticSpace](
      space: SpaceEvidence[S],
      metric: DMat,
      metricIdentity: ValueIdentity,
      provenance: SemanticProvenance
  ): Either[DiagramError, OpCometric[S, CertifiedSpd]] =
    val identity = ValueIdentity.derived("inverse", metricIdentity)
    for
      factor <- multivarDiagram(
        metric
          .cholesky(CholeskyOptions())
          .left
          .map(LinalgErrorAdapter.toMultivarError)
      )
      inverse <- multivarDiagram(
        factor
          .solve(DMat.eye(metric.rows))
          .left
          .map(LinalgErrorAdapter.toMultivarError)
      )
      linear <- semanticDiagram(
        Lin.fromDenseMatrix(
          MatrixOps.symmetrize(inverse),
          CoordinateEvidence.dual(space),
          CoordinateEvidence.primal(space),
          identity,
          provenance.append(SemanticProvenanceEvent.Derived("feature-cometric", Vector(metricIdentity)))
        )
      )
      certificate <- semanticDiagram(FormCertificates.spd(linear))
      cometric <- semanticDiagram(Op.certifiedSpd(Op.fromLin(linear, OperatorRoleWitness.cometric), certificate))
    yield cometric

  private def certifiedCovariance[S <: SemanticSpace](
      covariance: Op[Dual[S], Primal[S], CovarianceOperatorRole, UncheckedEvidence],
      space: SpaceEvidence[S]
  ): Either[DiagramError, OpCovariance[S, CertifiedPsd]] =
    for
      dense <- semanticDiagram(covariance.toDense)
      linear <- semanticDiagram(
        Lin.fromDenseMatrix(
          dense,
          CoordinateEvidence.dual(space),
          CoordinateEvidence.primal(space),
          covariance.valueIdentity,
          covariance.provenance
        )
      )
      certificate <- semanticDiagram(FormCertificates.psd(linear))
      certified <- semanticDiagram(Op.certifiedPsd(covariance, certificate))
    yield certified

/** Stable existential wrapper for support-restricted prepared diagrams. */
final class PreparedGpcaProblem private[multivar] (
    val rows: SpaceRef,
    val features: SpaceRef
)(
    val value: GpcaProblem[rows.Id, features.Id]
):
  def fit(
      components: ComponentCount,
      rankTolerance: GpcaRankTolerance = GpcaRankTolerance.default,
      solver: GeneralizedEigenSolver = DenseSolvers.generalizedEigen
  ): Either[MultivarError, GpcaOperatorFit[rows.Id, features.Id, ? <: SemanticSpace]] =
    value.fit(components, rankTolerance, solver)

object PreparedGpcaProblem:
  def from[Rows <: SemanticSpace, Columns <: SemanticSpace](
      prepared: PreparedSemanticDiagram[Rows, Columns]
  ): Either[DiagramError, PreparedGpcaProblem] =
    val rows = SpaceRef(prepared.rowSpace)
    val features = SpaceRef(prepared.columnSpace)
    for
      problem <- GpcaProblem.fromPrepared(
        rows.evidence,
        features.evidence,
        prepared.table,
        prepared.rowMetric,
        prepared.columnMetric,
        prepared.source.core.table.valueIdentity,
        prepared.provenance
      )
    yield new PreparedGpcaProblem(rows, features)(problem)

/** Lifecycle boundary for a dynamically described, already transformed GPCA table. */
private[multivar] object DynamicGpcaProblem:
  def from(
      table: MatrixView,
      rowSpace: MvSpace,
      featureSpace: MvSpace,
      rowMetric: MetricSpec,
      featureMetric: MetricSpec,
      sourceIdentity: ValueIdentity,
      provenance: SemanticProvenance
  ): Either[MultivarError, PreparedGpcaProblem] =
    val rows = SpaceRef(rowSpace)
    val features = SpaceRef(featureSpace)
    GpcaProblem
      .fromPrepared(
        rows.evidence,
        features.evidence,
        table,
        rowMetric,
        featureMetric,
        sourceIdentity,
        provenance
      )
      .left
      .map(diagramToMultivar)
      .map(problem => new PreparedGpcaProblem(rows, features)(problem))

private def squareRoots(values: DVec): DVec =
  val out = values.copyData
  var index = 0
  while index < out.length do
    out(index) = Math.sqrt(Math.max(out(index), 0.0))
    index += 1
  GaleNumerics.vectorFromArray(out)

private def sum(values: DVec): Double =
  var total = 0.0
  var index = 0
  while index < values.length do
    total += values(index)
    index += 1
  total

private def gpcaSemantic[A](result: Either[SemanticError, A]): Either[MultivarError, A] =
  result.left.map:
    case SemanticError.MultivarFailure(error) => error
    case SemanticError.LinearMapFailure(error) => LinalgErrorAdapter.toMultivarError(error)
    case error => MultivarError.SolverFailed(error.message)

private def semanticDiagram[A](result: Either[SemanticError, A]): Either[DiagramError, A] =
  result.left.map(DiagramError.Semantic.apply)

private def multivarDiagram[A](result: Either[MultivarError, A]): Either[DiagramError, A] =
  result.left.map(DiagramError.Multivar.apply)

private def program[A](result: Either[ProgramError, A]): Either[MultivarError, A] =
  result.left.map(error => MultivarError.SolverFailed(error.message))

private def diagramToMultivar(error: DiagramError): MultivarError =
  error match
    case DiagramError.Multivar(value) => value
    case other                        => MultivarError.SolverFailed(other.message)
