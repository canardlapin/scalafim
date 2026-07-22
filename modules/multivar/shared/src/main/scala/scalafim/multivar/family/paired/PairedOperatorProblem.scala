package scalafim.multivar
package family.paired

import scalafim.multivar.core.*
import scalafim.multivar.optimization.*
import scalafim.multivar.solver.*

import gale.linalg.DMat
import gale.linalg.DVec

enum PairedProgramKind:
  case Plsc
  case Cca(regularization: CcaRegularization)
  case ReducedRankRegression(regularization: RegressionRegularization)

  def label: String =
    this match
      case Plsc                         => "plsc"
      case Cca(_)                       => "cca"
      case ReducedRankRegression(_)     => "rrr"

final case class PairedOperatorDiagnostics(
    kind: PairedProgramKind,
    retainedRank: Int,
    crossResidual: Double,
    normalizationResidual: Double,
    spectralClusters: Vector[Vector[Int]],
    solver: String
):
  require(retainedRank > 0, "paired fit must retain at least one component")
  require(crossResidual.isFinite && crossResidual >= 0.0, "paired cross residual must be finite and non-negative")
  require(
    normalizationResidual.isFinite && normalizationResidual >= 0.0,
    "paired normalization residual must be finite and non-negative"
  )
  require(spectralClusters.flatten.length == retainedRank, "paired spectral clusters must partition the retained range")
  require(solver.nonEmpty, "paired solver label must be non-empty")

final case class PairedOperatorFit[
    SourceFeature <: SemanticSpace,
    TargetFeature <: SemanticSpace,
    Component <: SemanticSpace
](
    sourceFrame: FunctionalFrame[SourceFeature, Component, UncheckedEvidence],
    targetFrame: FunctionalFrame[TargetFeature, Component, UncheckedEvidence],
    programFit: OperatorProgramFit,
    cross: Op[Dual[TargetFeature], Primal[SourceFeature], CrossOperatorRole, UncheckedEvidence],
    coefficient: Option[OpCoefficient[SourceFeature, TargetFeature, UncheckedEvidence]],
    result: SvdResult,
    diagnostics: PairedOperatorDiagnostics,
    provenance: SemanticProvenance
):
  def sourceWeights: Either[MultivarError, DMat] =
    pairedSemantic(sourceFrame.weights.toDense)

  def targetWeights: Either[MultivarError, DMat] =
    pairedSemantic(targetFrame.weights.toDense)

  def toBundle[SourceRows <: SemanticSpace, TargetRows <: SemanticSpace](
      sourceTable: OpTable[SourceRows, SourceFeature, ? <: OperatorEvidence],
      targetTable: OpTable[TargetRows, TargetFeature, ? <: OperatorEvidence]
  ): Either[MultivarError, OperatorFitBundle] =
    for
      crossSnapshot <- OperatorSnapshot.from("cross", DerivedOperatorKind.SecondOrder, cross)
      sourceScores <- OperatorSnapshot.from("source-scores", DerivedOperatorKind.Scores, sourceFrame.scores(sourceTable))
      targetScores <- OperatorSnapshot.from("target-scores", DerivedOperatorKind.Scores, targetFrame.scores(targetTable))
      sourceAxes <- sourceFrame.axes match
        case Some(value) => OperatorSnapshot.from("source-axes", DerivedOperatorKind.Axes, value).map(Vector(_))
        case None        => Right(Vector.empty)
      targetAxes <- targetFrame.axes match
        case Some(value) => OperatorSnapshot.from("target-axes", DerivedOperatorKind.Axes, value).map(Vector(_))
        case None        => Right(Vector.empty)
      coefficientSnapshot <- coefficient match
        case Some(value) => OperatorSnapshot.from("coefficient", DerivedOperatorKind.Coefficient, value).map(Vector(_))
        case None        => Right(Vector.empty)
      crossResidual <- FitDiagnostic.from("cross-residual", diagnostics.crossResidual)
      normalizationResidual <- FitDiagnostic.from("normalization-residual", diagnostics.normalizationResidual)
      bundle <- OperatorFitBundle.from(
        programFit,
        Vector(crossSnapshot, sourceScores, targetScores) ++ sourceAxes ++ targetAxes ++ coefficientSnapshot,
        Vector(crossResidual, normalizationResidual),
        provenance
      )
    yield bundle

/** Typed paired sufficient statistics and their common generalized cross-SVD
  * lowering. Cross-view matching is entirely represented by `relationship`;
  * neither the objective nor the solver assumes positional row equality.
  */
final class PairedOperatorProblem[
    SourceRows <: SemanticSpace,
    TargetRows <: SemanticSpace,
    SourceFeature <: SemanticSpace,
    TargetFeature <: SemanticSpace
] private (
    val sourceRows: SpaceEvidence[SourceRows],
    val targetRows: SpaceEvidence[TargetRows],
    val sourceFeatures: SpaceEvidence[SourceFeature],
    val targetFeatures: SpaceEvidence[TargetFeature],
    val sourceTable: OpTable[SourceRows, SourceFeature, UncheckedEvidence],
    val targetTable: OpTable[TargetRows, TargetFeature, UncheckedEvidence],
    val sourceMarginal: Op[Dual[SourceFeature], Primal[SourceFeature], CovarianceOperatorRole, UncheckedEvidence],
    val targetMarginal: Op[Dual[TargetFeature], Primal[TargetFeature], CovarianceOperatorRole, UncheckedEvidence],
    val cross: Op[Dual[TargetFeature], Primal[SourceFeature], CrossOperatorRole, UncheckedEvidence],
    val provenance: SemanticProvenance
):
  def fitPlsc(
      components: ComponentCount,
      crossScale: Double,
      solver: SvdSolver = DenseSolvers.svd,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen
  ): Either[MultivarError, PairedOperatorFit[SourceFeature, TargetFeature, ? <: SemanticSpace]] =
    for
      _ <- requireScale(crossScale)
      sourceNormalization <- identityCometric(sourceFeatures, "plsc-source-normalization")
      targetNormalization <- identityCometric(targetFeatures, "plsc-target-normalization")
      fit <- solve(
        PairedProgramKind.Plsc,
        components,
        crossScale,
        sourceNormalization,
        targetNormalization,
        solver,
        eigenSolver
      )
    yield fit

  def fitCca(
      components: ComponentCount,
      regularization: CcaRegularization,
      covarianceScale: Double,
      solver: SvdSolver = DenseSolvers.svd,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen
  ): Either[MultivarError, PairedOperatorFit[SourceFeature, TargetFeature, ? <: SemanticSpace]] =
    for
      _ <- requireScale(covarianceScale)
      sourceNormalization <- covarianceNormalization(
        sourceFeatures,
        sourceMarginal,
        covarianceScale,
        regularization.x.value,
        "cca-source-normalization"
      )
      targetNormalization <- covarianceNormalization(
        targetFeatures,
        targetMarginal,
        covarianceScale,
        regularization.y.value,
        "cca-target-normalization"
      )
      fit <- solve(
        PairedProgramKind.Cca(regularization),
        components,
        covarianceScale,
        sourceNormalization,
        targetNormalization,
        solver,
        eigenSolver
      )
    yield fit

  def fitReducedRankRegression(
      components: ComponentCount,
      regularization: RegressionRegularization,
      ridgeScale: Double,
      solver: SvdSolver = DenseSolvers.svd,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen
  ): Either[MultivarError, PairedOperatorFit[SourceFeature, TargetFeature, ? <: SemanticSpace]] =
    for
      _ <- requireScale(ridgeScale)
      sourceNormalization <- covarianceNormalization(
        sourceFeatures,
        sourceMarginal,
        1.0,
        pairedRidgeValue(regularization) * ridgeScale,
        "rrr-source-normalization"
      )
      targetNormalization <- identityCometric(targetFeatures, "rrr-target-normalization")
      fit <- solve(
        PairedProgramKind.ReducedRankRegression(regularization),
        components,
        1.0,
        sourceNormalization,
        targetNormalization,
        solver,
        eigenSolver
      )
    yield fit

  private def solve[
      RS <: OperatorRoleTag,
      RT <: OperatorRoleTag
  ](
      kind: PairedProgramKind,
      components: ComponentCount,
      crossScale: Double,
      sourceNormalization: Op[Dual[SourceFeature], Primal[SourceFeature], RS, CertifiedSpd],
      targetNormalization: Op[Dual[TargetFeature], Primal[TargetFeature], RT, CertifiedSpd],
      solver: SvdSolver,
      eigenSolver: SymmetricEigenSolver
  ): Either[MultivarError, PairedOperatorFit[SourceFeature, TargetFeature, ? <: SemanticSpace]] =
    val limit = Math.min(sourceFeatures.dimension, targetFeatures.dimension)
    if components.value > limit then Left(MultivarError.InvalidComponentRequest(components.value, limit))
    else
      for
        crossDense <- pairedSemantic(cross.toDense)
        sourceDense <- pairedSemantic(sourceNormalization.toDense)
        targetDense <- pairedSemantic(targetNormalization.toDense)
        sourceInverseHalf <- MatrixOps.inverseSquareRoot(sourceDense, eigenSolver, 1e-12)
        targetInverseHalf <- MatrixOps.inverseSquareRoot(targetDense, eigenSolver, 1e-12)
        scaledCrossDense = MatrixOps.scale(crossDense, crossScale)
        whitened = GaleNumerics.multiply(sourceInverseHalf, GaleNumerics.multiply(scaledCrossDense, targetInverseHalf))
        svd <- solver.decompose(MatrixView.dense(whitened), components)
        _ <- requirePairedComponents(svd)
        component <- SpaceRef.of(
          s"${sourceFeatures.id.value}.${targetFeatures.id.value}.${kind.label}",
          SpaceRole.Latent,
          svd.singularValues.length
        )
        fit <- assemble(
          component,
          kind,
          scaledCrossDense,
          sourceDense,
          targetDense,
          sourceInverseHalf,
          targetInverseHalf,
          sourceNormalization,
          targetNormalization,
          svd
        )
      yield fit

  private def assemble[
      RS <: OperatorRoleTag,
      RT <: OperatorRoleTag
  ](
      component: SpaceRef,
      kind: PairedProgramKind,
      crossDense: DMat,
      sourceNormalizationDense: DMat,
      targetNormalizationDense: DMat,
      sourceInverseHalf: DMat,
      targetInverseHalf: DMat,
      sourceNormalization: Op[Dual[SourceFeature], Primal[SourceFeature], RS, CertifiedSpd],
      targetNormalization: Op[Dual[TargetFeature], Primal[TargetFeature], RT, CertifiedSpd],
      svd: SvdResult
  ): Either[MultivarError, PairedOperatorFit[SourceFeature, TargetFeature, component.Id]] =
    val sourceWeights = GaleNumerics.multiply(sourceInverseHalf, svd.u)
    val targetWeights = GaleNumerics.multiply(targetInverseHalf, svd.v)
    val fitProvenance = provenance.append(
      SemanticProvenanceEvent.Derived(
        s"${kind.label}-generalized-cross-svd",
        Vector(cross.valueIdentity, sourceNormalization.valueIdentity, targetNormalization.valueIdentity)
      )
    )
    for
      scaledCross <- pairedSemantic(
        Op.fromDense(
          crossDense,
          CoordinateEvidence.dual(targetFeatures),
          CoordinateEvidence.primal(sourceFeatures),
          OperatorRoleWitness.cross,
          ValueIdentity.derived(s"${kind.label}-scaled-cross", cross.valueIdentity),
          fitProvenance
        )
      )
      sourceVariable <- pairedProgram(
        FrameVariable.from(
          ParameterId.unsafe(s"${sourceFeatures.id.value}.${kind.label}-frame"),
          sourceFeatures,
          component.evidence
        )
      )
      targetVariable <- pairedProgram(
        FrameVariable.from(
          ParameterId.unsafe(s"${targetFeatures.id.value}.${kind.label}-frame"),
          targetFeatures,
          component.evidence
        )
      )
      sourceFrameOperator <- pairedSemantic(
        Op.fromDense(
          sourceWeights,
          CoordinateEvidence.primal(component.evidence),
          CoordinateEvidence.dual(sourceFeatures),
          OperatorRoleWitness.frame,
          ValueIdentity.derived(s"${kind.label}-source-frame", scaledCross.valueIdentity),
          fitProvenance
        )
      )
      targetFrameOperator <- pairedSemantic(
        Op.fromDense(
          targetWeights,
          CoordinateEvidence.primal(component.evidence),
          CoordinateEvidence.dual(targetFeatures),
          OperatorRoleWitness.frame,
          ValueIdentity.derived(s"${kind.label}-target-frame", scaledCross.valueIdentity),
          fitProvenance
        )
      )
      sourceAxisGeometry <- identityCometric(sourceFeatures, s"${kind.label}-source-axis-geometry")
      targetAxisGeometry <- identityCometric(targetFeatures, s"${kind.label}-target-axis-geometry")
      sourceFrame = FunctionalFrame(sourceFrameOperator, Some(sourceAxisGeometry))
      targetFrame = FunctionalFrame(targetFrameOperator, Some(targetAxisGeometry))
      sourceParameterization = FrameParameterization.identity(sourceVariable)
      targetParameterization = FrameParameterization.identity(targetVariable)
      sourceConstraint = FrameNormalization(sourceVariable, sourceNormalization)
      targetConstraint = FrameNormalization(targetVariable, targetNormalization)
      operatorProgram <- kind match
        case PairedProgramKind.Plsc =>
          pairedProgram(
            OperatorPrograms.plsc(
              sourceParameterization,
              targetParameterization,
              scaledCross,
              sourceConstraint,
              targetConstraint
            )
          )
        case PairedProgramKind.Cca(_) =>
          pairedProgram(
            OperatorPrograms.cca(
              sourceParameterization,
              targetParameterization,
              scaledCross,
              sourceConstraint,
              targetConstraint
            )
          )
        case PairedProgramKind.ReducedRankRegression(_) =>
          pairedProgram(
            OperatorPrograms.reducedRankRegression(
              sourceParameterization,
              targetParameterization,
              scaledCross,
              sourceNormalization,
              sourceConstraint,
              targetConstraint
            )
          )
      context <- pairedSemantic(
        CertificateContext.from(
          CertificateTolerance.strict,
          CertificateNorm.Frobenius,
          s"${kind.label}-generalized-cross-svd",
          "gale",
          NumericalPrecision.Float64,
          None
        )
      )
      crossResidual = generalizedCrossResidual(
        crossDense,
        sourceNormalizationDense,
        targetNormalizationDense,
        sourceWeights,
        targetWeights,
        svd.singularValues
      )
      normalizationResidual = Math.max(
        gramResidual(sourceWeights, sourceNormalizationDense),
        gramResidual(targetWeights, targetNormalizationDense)
      )
      clusters = spectralClusters(svd.singularValues, CertificateTolerance.strict)
      genericFit <- pairedProgram(
        OperatorProgramFit.exactSpectral(
          operatorProgram,
          Vector(FittedFrame(sourceVariable, sourceFrame), FittedFrame(targetVariable, targetFrame)),
          pairedSum(svd.singularValues),
          NumericalIdentifiability(
            svd.singularValues.length,
            clusters,
            Math.max(crossResidual, normalizationResidual),
            context
          ),
          fitProvenance
        )
      )
      coefficient <- kind match
        case PairedProgramKind.ReducedRankRegression(_) =>
          val inverse = GaleNumerics.multiply(sourceInverseHalf, sourceInverseHalf)
          pairedSemantic(
            Op.fromDense(
              GaleNumerics.multiply(inverse, crossDense),
              CoordinateEvidence.dual(targetFeatures),
              CoordinateEvidence.dual(sourceFeatures),
              OperatorRoleWitness.coefficient,
              ValueIdentity.derived("rrr-coefficient", scaledCross.valueIdentity, sourceNormalization.valueIdentity),
              fitProvenance
            )
          ).map(Some(_))
        case _ => Right(None)
    yield
      PairedOperatorFit(
        sourceFrame,
        targetFrame,
        genericFit,
        scaledCross,
        coefficient,
        svd,
        PairedOperatorDiagnostics(
          kind,
          svd.singularValues.length,
          crossResidual,
          normalizationResidual,
          clusters,
          "gale.spectral.Svd"
        ),
        fitProvenance
      )

  private def covarianceNormalization[
      Feature <: SemanticSpace
  ](
      feature: SpaceEvidence[Feature],
      marginal: Op[Dual[Feature], Primal[Feature], CovarianceOperatorRole, UncheckedEvidence],
      scale: Double,
      ridge: Double,
      label: String
  ): Either[MultivarError, Op[Dual[Feature], Primal[Feature], CovarianceOperatorRole, CertifiedSpd]] =
    for
      dense <- pairedSemantic(marginal.toDense)
      realized = MatrixOps.addRidge(MatrixOps.scale(dense, scale), ridge)
      certified <- certifySpd(feature, realized, OperatorRoleWitness.covariance, label, marginal.valueIdentity)
    yield certified

  private def identityCometric[
      Feature <: SemanticSpace
  ](
      feature: SpaceEvidence[Feature],
      label: String
  ): Either[MultivarError, OpCometric[Feature, CertifiedSpd]] =
    certifySpd(
      feature,
      DMat.eye(feature.dimension),
      OperatorRoleWitness.cometric,
      label,
      ValueIdentity.source(ValueId.unsafe(s"${feature.id.value}.$label-source"))
    )

  private def certifySpd[
      Feature <: SemanticSpace,
      R <: OperatorRoleTag
  ](
      feature: SpaceEvidence[Feature],
      dense: DMat,
      role: OperatorRoleWitness[R],
      label: String,
      source: ValueIdentity
  ): Either[MultivarError, Op[Dual[Feature], Primal[Feature], R, CertifiedSpd]] =
    val identity = ValueIdentity.derived(label, source)
    for
      tolerance <- pairedSemantic(CertificateTolerance.from(1e-12, 1e-12))
      context <- pairedSemantic(
        CertificateContext.from(
          tolerance,
          CertificateNorm.Frobenius,
          "paired-spd-normalization",
          "gale",
          NumericalPrecision.Float64
        )
      )
      linear <- pairedSemantic(
        Lin.fromDenseMatrix(
          dense,
          CoordinateEvidence.dual(feature),
          CoordinateEvidence.primal(feature),
          identity,
          provenance.append(SemanticProvenanceEvent.Derived(label, Vector(source)))
        )
      )
      certificate <- pairedSemantic(FormCertificates.spd(linear, context))
      certified <- pairedSemantic(Op.certifiedSpd(Op.fromLin(linear, role), certificate))
    yield certified

  private def requireScale(value: Double): Either[MultivarError, Unit] =
    if value.isFinite && value > 0.0 then Right(())
    else Left(MultivarError.InvalidTolerance("paired operator scale", value))

object PairedOperatorProblem:
  def fromTables[
      SourceRows <: SemanticSpace,
      TargetRows <: SemanticSpace,
      SourceFeature <: SemanticSpace,
      TargetFeature <: SemanticSpace,
      ES <: OperatorEvidence,
      ET <: OperatorEvidence,
      EL <: OperatorEvidence
  ](
      sourceRows: SpaceEvidence[SourceRows],
      targetRows: SpaceEvidence[TargetRows],
      sourceFeatures: SpaceEvidence[SourceFeature],
      targetFeatures: SpaceEvidence[TargetFeature],
      sourceTable: OpTable[SourceRows, SourceFeature, UncheckedEvidence],
      targetTable: OpTable[TargetRows, TargetFeature, UncheckedEvidence],
      sourceGeometry: OpRowLink[SourceRows, SourceRows, ES],
      targetGeometry: OpRowLink[TargetRows, TargetRows, ET],
      relationship: OpRowLink[SourceRows, TargetRows, EL],
      provenance: SemanticProvenance = SemanticProvenance.source("paired-operator-problem")
  ): PairedOperatorProblem[SourceRows, TargetRows, SourceFeature, TargetFeature] =
    val sourceMarginal = OperatorAlgebra
      .secondOrder(sourceTable, sourceGeometry, sourceTable)
      .retag(OperatorRoleWitness.covariance, "paired-source-marginal")
    val targetMarginal = OperatorAlgebra
      .secondOrder(targetTable, targetGeometry, targetTable)
      .retag(OperatorRoleWitness.covariance, "paired-target-marginal")
    val cross = OperatorAlgebra.secondOrder(sourceTable, relationship, targetTable)
    new PairedOperatorProblem(
      sourceRows,
      targetRows,
      sourceFeatures,
      targetFeatures,
      sourceTable,
      targetTable,
      sourceMarginal,
      targetMarginal,
      cross,
      provenance
    )

  private[multivar] def fromMatrices(
      source: MatrixView,
      target: MatrixView,
      rowMetric: Option[MetricSpec],
      id: String,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, PreparedPairedOperatorProblem] =
    if source.rows != target.rows then
      Left(MultivarError.MatrixShapeMismatch(s"paired tables require equal rows, got ${source.rows} and ${target.rows}"))
    else
      for
        _ <- requirePairedMaterialization(source, target, policy)
        rowDescriptor <- rowMetric.flatMap(_.space) match
          case Some(space) => Right(space)
          case None        => MvSpace.of(s"$id.rows", SpaceRole.Samples, source.rows)
        sourceDescriptor <- MvSpace.of(s"$id.source", SpaceRole.Observed, source.cols)
        targetDescriptor <- MvSpace.of(s"$id.target", SpaceRole.Observed, target.cols)
        metricValue <- rowMetric match
          case Some(value) => Right(value)
          case None        => MetricSpec.identity(source.rows, Some(rowDescriptor))
        prepared <- fromDynamic(source, target, metricValue, rowDescriptor, sourceDescriptor, targetDescriptor, id)
      yield prepared

  private def fromDynamic(
      source: MatrixView,
      target: MatrixView,
      rowMetric: MetricSpec,
      rowDescriptor: MvSpace,
      sourceDescriptor: MvSpace,
      targetDescriptor: MvSpace,
      id: String
  ): Either[MultivarError, PreparedPairedOperatorProblem] =
    val rows = SpaceRef(rowDescriptor)
    val sourceFeatures = SpaceRef(sourceDescriptor)
    val targetFeatures = SpaceRef(targetDescriptor)
    val provenance = SemanticProvenance.source(s"$id-paired-operator-problem")
    val metricIdentity = ValueIdentity.source(ValueId.unsafe(s"$id.row-metric"))
    for
      sourceTable <- pairedSemantic(
        Op.fromMatrixView(
          source,
          CoordinateEvidence.dual(sourceFeatures.evidence),
          CoordinateEvidence.primal(rows.evidence),
          OperatorRoleWitness.table,
          ValueIdentity.source(ValueId.unsafe(s"$id.source-table")),
          provenance
        )
      )
      targetTable <- pairedSemantic(
        Op.fromMatrixView(
          target,
          CoordinateEvidence.dual(targetFeatures.evidence),
          CoordinateEvidence.primal(rows.evidence),
          OperatorRoleWitness.table,
          ValueIdentity.source(ValueId.unsafe(s"$id.target-table")),
          provenance
        )
      )
      metricLinear <- pairedSemantic(FormOperator.primal(rowMetric, rows.evidence, metricIdentity, provenance))
      metricCertificate <- pairedSemantic(FormCertificates.psd(metricLinear))
      metric <- pairedSemantic(Op.certifiedPsd(Op.fromLin(metricLinear, OperatorRoleWitness.metric), metricCertificate))
      relation = metric.retag(OperatorRoleWitness.rowLink, "paired-row-relationship")
      problem = fromTables(
        rows.evidence,
        rows.evidence,
        sourceFeatures.evidence,
        targetFeatures.evidence,
        sourceTable,
        targetTable,
        relation,
        relation,
        relation,
        provenance
      )
    yield new PreparedPairedOperatorProblem(rows, sourceFeatures, targetFeatures)(problem)

final class PreparedPairedOperatorProblem private[multivar] (
    val rows: SpaceRef,
    val sourceFeatures: SpaceRef,
    val targetFeatures: SpaceRef
)(
    val value: PairedOperatorProblem[rows.Id, rows.Id, sourceFeatures.Id, targetFeatures.Id]
):
  def fitPlsc(
      components: ComponentCount,
      crossScale: Double,
      solver: SvdSolver,
      eigenSolver: SymmetricEigenSolver
  ): Either[MultivarError, PairedOperatorFit[sourceFeatures.Id, targetFeatures.Id, ? <: SemanticSpace]] =
    value.fitPlsc(components, crossScale, solver, eigenSolver)

  def fitCca(
      components: ComponentCount,
      regularization: CcaRegularization,
      covarianceScale: Double,
      solver: SvdSolver,
      eigenSolver: SymmetricEigenSolver
  ): Either[MultivarError, PairedOperatorFit[sourceFeatures.Id, targetFeatures.Id, ? <: SemanticSpace]] =
    value.fitCca(components, regularization, covarianceScale, solver, eigenSolver)

  def fitReducedRankRegression(
      components: ComponentCount,
      regularization: RegressionRegularization,
      ridgeScale: Double,
      solver: SvdSolver,
      eigenSolver: SymmetricEigenSolver
  ): Either[MultivarError, PairedOperatorFit[sourceFeatures.Id, targetFeatures.Id, ? <: SemanticSpace]] =
    value.fitReducedRankRegression(components, regularization, ridgeScale, solver, eigenSolver)

private def requirePairedComponents(svd: SvdResult): Either[MultivarError, Unit] =
  if svd.singularValues.length == 0 then Left(MultivarError.SolverFailed("no singular values above tolerance"))
  else Right(())

private def pairedRidgeValue(regularization: RegressionRegularization): Double =
  regularization match
    case RegressionRegularization.Ols          => 0.0
    case RegressionRegularization.Ridge(value) => value.value

private def requirePairedMaterialization(
    source: MatrixView,
    target: MatrixView,
    policy: StoragePolicy
): Either[MultivarError, Unit] =
  if policy == StoragePolicy.AllowDense || (source.storage == StorageKind.Dense && target.storage == StorageKind.Dense) then Right(())
  else
    val storage = if source.storage != StorageKind.Dense then source.storage else target.storage
    Left(MultivarError.DensificationRejected("paired operator sufficient-statistic lowering", storage))

private def gramResidual(weights: DMat, geometry: DMat): Double =
  val gram = GaleNumerics.multiply(weights.t, GaleNumerics.multiply(geometry, weights))
  frobeniusResidual(gram, DMat.eye(gram.rows))

private def generalizedCrossResidual(
    cross: DMat,
    sourceGeometry: DMat,
    targetGeometry: DMat,
    sourceWeights: DMat,
    targetWeights: DMat,
    singularValues: DVec
): Double =
  val scaledSource = MatrixOps.scaleColumns(sourceWeights, singularValues)
  val scaledTarget = MatrixOps.scaleColumns(targetWeights, singularValues)
  val left = GaleNumerics.multiply(cross, targetWeights)
  val leftExpected = GaleNumerics.multiply(sourceGeometry, scaledSource)
  val right = GaleNumerics.multiply(cross.t, sourceWeights)
  val rightExpected = GaleNumerics.multiply(targetGeometry, scaledTarget)
  Math.max(frobeniusResidual(left, leftExpected), frobeniusResidual(right, rightExpected))

private def frobeniusResidual(actual: DMat, expected: DMat): Double =
  var numerator = 0.0
  var denominator = 0.0
  var row = 0
  while row < actual.rows do
    var col = 0
    while col < actual.cols do
      val difference = actual(row, col) - expected(row, col)
      numerator += difference * difference
      denominator += expected(row, col) * expected(row, col)
      col += 1
    row += 1
  Math.sqrt(numerator) / Math.max(1.0, Math.sqrt(denominator))

private def pairedSum(values: DVec): Double =
  var total = 0.0
  var index = 0
  while index < values.length do
    total += values(index)
    index += 1
  total

private def pairedSemantic[A](result: Either[SemanticError, A]): Either[MultivarError, A] =
  result.left.map:
    case SemanticError.MultivarFailure(error)   => error
    case SemanticError.LinearMapFailure(error)  => LinalgErrorAdapter.toMultivarError(error)
    case error                                  => MultivarError.SolverFailed(error.message)

private def pairedProgram[A](result: Either[ProgramError, A]): Either[MultivarError, A] =
  result.left.map(error => MultivarError.SolverFailed(error.message))
