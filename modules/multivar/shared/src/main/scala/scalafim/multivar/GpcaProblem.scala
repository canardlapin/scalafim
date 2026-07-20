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

  private[multivar] def fromBackend(backend: GmdBackend): Either[MultivarError, GpcaRankTolerance] =
    backend match
      case GmdBackend.Eigen(value) => from(value)
      case GmdBackend.Auto         => Right(default)
      case GmdBackend.Deflation(_, _, _, _) =>
        Left(
          MultivarError.UnsupportedEstimator(
            "the typed GPCA operator path supports Gale generalized Rayleigh-Ritz; " +
              "the legacy deflation selector is confined to the explicit unsafe compatibility boundary"
          )
        )

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

/** Executable result of the GPCA operator program.
  *
  * `functionalFrame` is the fitted covector frame `W = Q V`. Scores and axes
  * remain derived views of that one fitted value. `compatibility` contains no
  * independent numerical result: it is assembled from the same frame and
  * spectrum for consumers that have not yet migrated from [[GenPcaFit]].
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
    compatibility: GenPcaFit,
    diagnostics: GpcaNumericalDiagnostics,
    provenance: SemanticProvenance
):
  def scores(table: OpTable[Rows, Feature, ? <: OperatorEvidence]):
      Op[Primal[Component], Primal[Rows], ScoreOperatorRole, UncheckedEvidence] =
    functionalFrame.scores(table)

  def axes: Option[Op[Primal[Component], Primal[Feature], AxisOperatorRole, UncheckedEvidence]] =
    functionalFrame.axes

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
    private val tableView: MatrixView,
    private val sourceView: MatrixView,
    private val preprocessor: FittedPreprocessor,
    private val rowMetricValue: MvMetric,
    private val featureMetricValue: MvMetric,
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
        covarianceDense <- semantic(covariance.toDense)
        normalizationDense <- semantic(featureCometric.toDense)
        eigen <- LinalgErrorAdapter.adapt(solver.decompose(covarianceDense, normalizationDense, components))
        retained <- retainedSpectrum(eigen, rankTolerance)
        (eigenvalues, rawWeights) = retained
        weights = orientColumns(rawWeights)
        component <- SpaceRef.of(s"${featureSpace.id.value}.gpca", SpaceRole.Latent, eigenvalues.length)
        fit <- assemble(
          component,
          components,
          eigenvalues,
          weights,
          covarianceDense,
          normalizationDense,
          rankTolerance
        )
      yield fit

  private def assemble(
      component: SpaceRef,
      requested: ComponentCount,
      eigenvalues: DVec,
      weights: DMat,
      covarianceDense: DMat,
      normalizationDense: DMat,
      rankTolerance: GpcaRankTolerance
  ): Either[MultivarError, GpcaOperatorFit[Rows, Feature, component.Id]] =
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
      frameOperator <- semantic(
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
      scoreValues <- semantic(functionalFrame.scores(table).toDense)
      axisValues <- semantic(functionalFrame.axes.get.toDense)
      singularValues = squareRoots(eigenvalues)
      rowAxes = scaleColumnsByInverse(scoreValues, singularValues)
      totalVariance <- featureMetricValue.contract(covarianceDense)
      compatibility <- GenPca.assembleCompatibility(
        sourceView,
        preprocessor,
        rowMetricValue,
        featureMetricValue,
        GmdDecomposition(GmdResult(rowAxes, singularValues, axisValues), totalVariance),
        requested,
        featureSpace.descriptor,
        backend = Some("operator-gale-generalized-eigen"),
        storagePolicy = Some(StoragePolicy.AllowDense),
        tolerance = Some(rankTolerance.value),
        method = "semantic-gpca",
        latentId = component.descriptor.id.value
      )
      generalizedResidual = generalizedEquationResidual(
        covarianceDense,
        normalizationDense,
        weights,
        eigenvalues
      )
      normalizationResidual = orthonormalityResidual(normalizationDense, weights)
      tolerance = CertificateTolerance.strict
      equationScale = frobenius(covarianceDense) + maxAbs(eigenvalues) * frobenius(normalizationDense)
      _ <-
        if generalizedResidual <= tolerance.threshold(equationScale) then Right(())
        else
          Left(
            MultivarError.NumericalResidualExceeded(
              "GPCA generalized eigen equation",
              generalizedResidual,
              tolerance.threshold(equationScale)
            )
          )
      _ <-
        if normalizationResidual <= tolerance.threshold(eigenvalues.length.toDouble) then Right(())
        else
          Left(
            MultivarError.NumericalResidualExceeded(
              "GPCA frame normalization",
              normalizationResidual,
              tolerance.threshold(eigenvalues.length.toDouble)
            )
          )
      context <- semantic(
        CertificateContext.from(
          tolerance,
          CertificateNorm.Frobenius,
          "gpca-generalized-eigenfit",
          "gale",
          NumericalPrecision.Float64,
          Some(s"rank-tolerance=${rankTolerance.value}")
        )
      )
      clusters = spectralClusters(eigenvalues)
      identifiability = NumericalIdentifiability(
        eigenvalues.length,
        clusters,
        Math.max(generalizedResidual, normalizationResidual),
        context
      )
      operatorFit <- program(
        OperatorProgramFit.from(
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
        compatibility,
        GpcaNumericalDiagnostics(
          eigenvalues.length,
          generalizedResidual,
          normalizationResidual,
          clusters,
          "gale.spectral.Eigen.eigSymmetricGeneralized"
        ),
        fitProvenance
      )

  private def retainedSpectrum(
      eigen: SymmetricEigenResult,
      tolerance: GpcaRankTolerance
  ): Either[MultivarError, (DVec, DMat)] =
    if eigen.values.length == 0 then Left(MultivarError.SolverFailed("GPCA eigensolver returned an empty spectrum"))
    else
      val leading = eigen.values(0)
      if !leading.isFinite then Left(MultivarError.NonFiniteValue("GPCA generalized eigenvalue", 0, leading))
      else
        val cutoff = tolerance.value * Math.max(leading, 0.0)
        var retained = 0
        var error = Option.empty[MultivarError]
        while retained < eigen.values.length && error.isEmpty && eigen.values(retained) > cutoff do
          val value = eigen.values(retained)
          if !value.isFinite then error = Some(MultivarError.NonFiniteValue("GPCA generalized eigenvalue", retained, value))
          else retained += 1
        error match
          case Some(value) => Left(value)
          case None if retained == 0 => Left(MultivarError.SolverFailed("no GPCA components survived the rank tolerance"))
          case None => Right((MatrixOps.takeVector(eigen.values, retained), MatrixOps.takeColumns(eigen.vectors, retained)))

object GpcaProblem:
  private[multivar] def fromPrepared[Rows <: SemanticSpace, Feature <: SemanticSpace](
      rowSpace: SpaceEvidence[Rows],
      featureSpace: SpaceEvidence[Feature],
      tableView: MatrixView,
      sourceView: MatrixView,
      preprocessor: FittedPreprocessor,
      rowMetricValue: MvMetric,
      featureMetricValue: MvMetric,
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
        tableView,
        sourceView,
        preprocessor,
        rowMetricValue,
        featureMetricValue,
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
          DualityKernels.symmetrize(inverse),
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
      preprocessor <- PreprocessSpec.Pass.fit(prepared.table).left.map(DiagramError.Multivar.apply)
      problem <- GpcaProblem.fromPrepared(
        rows.evidence,
        features.evidence,
        prepared.table,
        prepared.table,
        preprocessor,
        prepared.rowMetric,
        prepared.columnMetric,
        prepared.source.core.table.valueIdentity,
        prepared.provenance
      )
    yield new PreparedGpcaProblem(rows, features)(problem)

/** Compatibility adapter for a dynamically described, already transformed GPCA table. */
private[multivar] object DynamicGpcaProblem:
  def from(
      table: MatrixView,
      source: MatrixView,
      preprocessor: FittedPreprocessor,
      rowSpace: MvSpace,
      featureSpace: MvSpace,
      rowMetric: MvMetric,
      featureMetric: MvMetric,
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
        source,
        preprocessor,
        rowMetric,
        featureMetric,
        sourceIdentity,
        provenance
      )
      .left
      .map(diagramToMultivar)
      .map(problem => new PreparedGpcaProblem(rows, features)(problem))

private def orientColumns(matrix: DMat): DMat =
  val out = matrix.copyData
  var col = 0
  while col < matrix.cols do
    var anchor = 0
    var row = 1
    while row < matrix.rows do
      if Math.abs(matrix(row, col)) > Math.abs(matrix(anchor, col)) then anchor = row
      row += 1
    if matrix(anchor, col) < 0.0 then
      row = 0
      while row < matrix.rows do
        out(row * matrix.cols + col) = -out(row * matrix.cols + col)
        row += 1
    col += 1
  GaleNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)

private def squareRoots(values: DVec): DVec =
  val out = values.copyData
  var index = 0
  while index < out.length do
    out(index) = Math.sqrt(Math.max(out(index), 0.0))
    index += 1
  GaleNumerics.vectorFromArray(out)

private def scaleColumnsByInverse(matrix: DMat, scale: DVec): DMat =
  val out = matrix.copyData
  var row = 0
  while row < matrix.rows do
    var col = 0
    while col < matrix.cols do
      out(row * matrix.cols + col) /= scale(col)
      col += 1
    row += 1
  GaleNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)

private def generalizedEquationResidual(
    numerator: DMat,
    denominator: DMat,
    vectors: DMat,
    values: DVec
): Double =
  val left = GaleNumerics.multiply(numerator, vectors)
  val right = MatrixOps.scaleColumns(GaleNumerics.multiply(denominator, vectors), values)
  frobenius(MatrixOps.subtract(left, right))

private def orthonormalityResidual(metric: DMat, vectors: DMat): Double =
  val gram = GaleNumerics.multiply(vectors.t, GaleNumerics.multiply(metric, vectors))
  frobenius(MatrixOps.subtract(gram, DMat.eye(gram.rows)))

private def frobenius(matrix: DMat): Double =
  var squared = 0.0
  var row = 0
  while row < matrix.rows do
    var col = 0
    while col < matrix.cols do
      val value = matrix(row, col)
      squared += value * value
      col += 1
    row += 1
  Math.sqrt(squared)

private def spectralClusters(values: DVec): Vector[Vector[Int]] =
  GenPcaSpectrum(squareRoots(values), values)
    .clusters()
    .map(cluster => (cluster.firstComponent until cluster.lastComponentExclusive).toVector)

private def sum(values: DVec): Double =
  var total = 0.0
  var index = 0
  while index < values.length do
    total += values(index)
    index += 1
  total

private def maxAbs(values: DVec): Double =
  var maximum = 0.0
  var index = 0
  while index < values.length do
    maximum = Math.max(maximum, Math.abs(values(index)))
    index += 1
  maximum

private def semantic[A](result: Either[SemanticError, A]): Either[MultivarError, A] =
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
