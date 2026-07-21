package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

/** A CPCA projector resolved in one nominal space.
  *
  * Unlike [[ResolvedCpcaConstraint]], the canonical representation contains
  * only typed operators. `CpcaConstraint` remains the inspectable request;
  * the fitted projector and optional coordinate extractor carry their domain
  * and codomain in the Scala type and at runtime.
  */
final class CpcaOperatorConstraint[S <: SemanticSpace] private[multivar] (
    val constraint: CpcaConstraint,
    val axis: IndexAxis,
    val space: SpaceEvidence[S],
    val rank: Int,
    val basis: Option[DMat],
    val originalDesign: Option[DMat],
    val projector: OpConstraint[S, UncheckedEvidence],
    val coordinateMap: Option[CpcaCoordinateOperator[S]],
    val provenance: SemanticProvenance
):
  require(rank >= 0 && rank <= space.dimension, "resolved constraint rank must lie within its space")
  require(basis.forall(value => value.rows == space.dimension && value.cols == rank), "constraint basis must match its space and rank")

  def project(input: DMat): Either[MultivarError, DMat] =
    cpcaSemantic(projector.apply(input))

  def residual(input: DMat): Either[MultivarError, DMat] =
    project(input).map(projected => MatrixOps.subtract(input, projected))

  def coordinates(input: DMat): Either[MultivarError, DMat] =
    coordinateMap match
      case Some(value) => cpcaSemantic(value.operator.apply(input))
      case None =>
        if input.rows != space.dimension then
          Left(
            MultivarError.MatrixShapeMismatch(
              s"${axis.label} constraint expected ${space.dimension} rows, got ${input.rows}"
            )
          )
        else Right(DMat.zeros(rank, input.cols))

final class CpcaCoordinateOperator[S <: SemanticSpace] private[multivar] (
    val coordinates: SpaceRef
)(
    val operator: Op[Primal[S], Primal[coordinates.Id], ConstraintOperatorRole, UncheckedEvidence]
)

object CpcaOperatorConstraint:
  def resolve[S <: SemanticSpace](
      constraint: CpcaConstraint,
      axis: IndexAxis,
      space: SpaceEvidence[S],
      metric: Op[Primal[S], Dual[S], MetricOperatorRole, CertifiedPsd],
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      tolerance: Double = 1e-12
  ): Either[MultivarError, CpcaOperatorConstraint[S]] =
    for
      _ <- constraint.validate(axis, space.dimension)
      _ <- RowGeometryOps.requireTolerance("CPCA constraint tolerance", tolerance)
      resolved <- constraint match
        case CpcaConstraint.Identity => identity(axis, space, metric.provenance)
        case CpcaConstraint.Zero     => zero(axis, space, metric.provenance)
        case CpcaConstraint.Basis(design, keepDesign) =>
          basis(axis, space, design, metric, eigenSolver, tolerance, keepDesign)
    yield resolved

  private def identity[S <: SemanticSpace](
      axis: IndexAxis,
      space: SpaceEvidence[S],
      provenance: SemanticProvenance
  ): Either[MultivarError, CpcaOperatorConstraint[S]] =
    val identity = ValueIdentity.source(ValueId.unsafe(s"${space.id.value}.${axis.label}.identity-constraint"))
    cpcaSemantic(
      Op.fromDense(
        DMat.eye(space.dimension),
        CoordinateEvidence.primal(space),
        CoordinateEvidence.primal(space),
        OperatorRoleWitness.constraint,
        identity,
        provenance.append(SemanticProvenanceEvent.Derived("identity-constraint", Vector.empty))
      )
    ).map: projector =>
      new CpcaOperatorConstraint(
        CpcaConstraint.Identity,
        axis,
        space,
        space.dimension,
        None,
        None,
        projector,
        None,
        projector.provenance
      )

  private def zero[S <: SemanticSpace](
      axis: IndexAxis,
      space: SpaceEvidence[S],
      provenance: SemanticProvenance
  ): Either[MultivarError, CpcaOperatorConstraint[S]] =
    val identity = ValueIdentity.source(ValueId.unsafe(s"${space.id.value}.${axis.label}.zero-constraint"))
    cpcaSemantic(
      Op.fromDense(
        DMat.zeros(space.dimension, space.dimension),
        CoordinateEvidence.primal(space),
        CoordinateEvidence.primal(space),
        OperatorRoleWitness.constraint,
        identity,
        provenance.append(SemanticProvenanceEvent.Derived("zero-constraint", Vector.empty))
      )
    ).map: projector =>
      new CpcaOperatorConstraint(
        CpcaConstraint.Zero,
        axis,
        space,
        0,
        None,
        None,
        projector,
        None,
        projector.provenance
      )

  private def basis[S <: SemanticSpace](
      axis: IndexAxis,
      space: SpaceEvidence[S],
      design: DMat,
      metric: Op[Primal[S], Dual[S], MetricOperatorRole, CertifiedPsd],
      eigenSolver: SymmetricEigenSolver,
      tolerance: Double,
      keepDesign: Boolean
  ): Either[MultivarError, CpcaOperatorConstraint[S]] =
    for
      metricDense <- cpcaSemantic(metric.toDense)
      roots <- MetricSqrt.factorDense(metricDense, eigenSolver, tolerance, s"${axis.label} constraint metric")
      whitened = roots.half.applyLeft(design)
      orthogonal <- RowProjector.orthogonal(whitened, eigenSolver, tolerance)
      resolved <-
        if orthogonal.rank == 0 then zero(axis, space, metric.provenance)
        else
          orthogonal.basis match
            case None =>
              Left(
                MultivarError.SolverFailed(
                  s"${axis.label} constraint projector of rank ${orthogonal.rank} did not expose an orthonormal basis"
                )
              )
            case Some(q) =>
              assembleBasis(axis, space, design, keepDesign, q, metric)
    yield resolved

  private def assembleBasis[S <: SemanticSpace](
      axis: IndexAxis,
      space: SpaceEvidence[S],
      design: DMat,
      keepDesign: Boolean,
      q: DMat,
      metric: Op[Primal[S], Dual[S], MetricOperatorRole, CertifiedPsd]
  ): Either[MultivarError, CpcaOperatorConstraint[S]] =
    val rank = q.cols
    val projectorIdentity = ValueIdentity.derived(s"${axis.label}-constraint-projector", metric.valueIdentity)
    val provenance = metric.provenance.append(
      SemanticProvenanceEvent.Derived("cpca-constraint-projector", Vector(metric.valueIdentity))
    )
    for
      projector <- cpcaSemantic(
        Op.fromDense(
          GaleNumerics.multiply(q, q.transpose),
          CoordinateEvidence.primal(space),
          CoordinateEvidence.primal(space),
          OperatorRoleWitness.constraint,
          projectorIdentity,
          provenance
        )
      )
      coordinates <- SpaceRef.of(
        s"${space.id.value}.${axis.label}.constraint",
        SpaceRole.Latent,
        rank
      )
      coordinateOperator <- cpcaSemantic(
        Op.fromDense(
          q.transpose,
          CoordinateEvidence.primal(space),
          CoordinateEvidence.primal(coordinates.evidence),
          OperatorRoleWitness.constraint,
          ValueIdentity.derived("constraint-coordinates", projectorIdentity),
          provenance
        )
      )
    yield
      new CpcaOperatorConstraint(
        CpcaConstraint.Basis(design, keepDesign),
        axis,
        space,
        rank,
        Some(q),
        if keepDesign then Some(design) else None,
        projector,
        Some(new CpcaCoordinateOperator(coordinates)(coordinateOperator)),
        provenance
      )

final case class CpcaOperatorDiagnostics(
    retainedRank: Int,
    crossResidual: Double,
    normalizationResidual: Double,
    spectralClusters: Vector[Vector[Int]],
    solver: String
):
  require(retainedRank > 0, "a fitted CPCA operator block must have positive rank")
  require(crossResidual.isFinite && crossResidual >= 0.0, "CPCA cross residual must be finite and non-negative")
  require(normalizationResidual.isFinite && normalizationResidual >= 0.0, "CPCA normalization residual must be finite and non-negative")
  require(spectralClusters.flatten.length == retainedRank, "CPCA spectral clusters must partition the retained range")
  require(solver.nonEmpty, "CPCA solver label must be non-empty")

final case class CpcaOperatorBlockFit[
    Rows <: SemanticSpace,
    Feature <: SemanticSpace,
    Component <: SemanticSpace
](
    block: CpcaBlock,
    featureFrame: FunctionalFrame[Feature, Component, UncheckedEvidence],
    rowScores: Op[Primal[Component], Primal[Rows], ScoreOperatorRole, UncheckedEvidence],
    blockTable: OpTable[Rows, Feature, UncheckedEvidence],
    featureOperator: Op[Dual[Feature], Primal[Feature], CovarianceOperatorRole, UncheckedEvidence],
    programFit: OperatorProgramFit,
    compatibility: CpcaBlockFit,
    diagnostics: CpcaOperatorDiagnostics,
    provenance: SemanticProvenance
):
  def toBundle: Either[MultivarError, OperatorFitBundle] =
    for
      scores <- OperatorSnapshot.from("scores", DerivedOperatorKind.Scores, rowScores)
      table <- OperatorSnapshot.from("block-table", DerivedOperatorKind.Projection, blockTable)
      feature <- OperatorSnapshot.from("feature-operator", DerivedOperatorKind.SecondOrder, featureOperator)
      axes <- featureFrame.axes match
        case Some(value) => OperatorSnapshot.from("axes", DerivedOperatorKind.Axes, value).map(Vector(_))
        case None        => Right(Vector.empty)
      crossResidual <- FitDiagnostic.from("cross-residual", diagnostics.crossResidual)
      normalizationResidual <- FitDiagnostic.from("normalization-residual", diagnostics.normalizationResidual)
      bundle <- OperatorFitBundle.from(
        programFit,
        Vector(scores, table, feature) ++ axes,
        Vector(crossResidual, normalizationResidual),
        provenance
      )
    yield bundle

/** Certified CPCA fit. Structural-zero blocks remain present in `blocks` but
  * intentionally have no parameter frames or variational program.
  */
final case class CpcaOperatorFit[Rows <: SemanticSpace, Feature <: SemanticSpace](
    rowSpace: SpaceEvidence[Rows],
    featureSpace: SpaceEvidence[Feature],
    rowConstraint: CpcaOperatorConstraint[Rows],
    featureConstraint: CpcaOperatorConstraint[Feature],
    partition: CpcaPartition,
    blocks: Map[CpcaBlock, CpcaBlockFit],
    operatorBlocks: Vector[CpcaOperatorBlockFit[Rows, Feature, ? <: SemanticSpace]],
    rowMetricRoots: MetricRoots,
    featureMetricRoots: MetricRoots,
    storagePolicy: StoragePolicy,
    rankTolerance: Double,
    provenance: SemanticProvenance
):
  def block(block: CpcaBlock): Option[CpcaBlockFit] =
    blocks.get(block)

  def operatorBlock(block: CpcaBlock): Option[CpcaOperatorBlockFit[Rows, Feature, ? <: SemanticSpace]] =
    operatorBlocks.find(_.block == block)

  def totalSS: Double =
    partition.totalSS

/** Canonical CPCA problem over a typed table, typed row relation, typed feature
  * covariance, and typed resolved constraint operators.
  */
final class CpcaOperatorProblem[Rows <: SemanticSpace, Feature <: SemanticSpace] private[multivar] (
    val rowSpace: SpaceEvidence[Rows],
    val featureSpace: SpaceEvidence[Feature],
    val table: OpTable[Rows, Feature, UncheckedEvidence],
    val rowMetric: Op[Primal[Rows], Dual[Rows], MetricOperatorRole, CertifiedPsd],
    val featureMetric: Op[Primal[Feature], Dual[Feature], MetricOperatorRole, CertifiedPsd],
    val rowRelationship: OpRowLink[Rows, Rows, CertifiedPsd],
    val featureCovariance: Op[Dual[Feature], Primal[Feature], CovarianceOperatorRole, UncheckedEvidence],
    val rowConstraint: CpcaOperatorConstraint[Rows],
    val featureConstraint: CpcaOperatorConstraint[Feature],
    private val tableView: MatrixView,
    val constructionTolerance: Double,
    val provenance: SemanticProvenance
):
  def fit(
      blockRequest: CpcaBlockRequest,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      svdSolver: SvdSolver = DenseSolvers.svd,
      rankTolerance: Double = 1e-12,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, CpcaOperatorFit[Rows, Feature]] =
    for
      _ <- RowGeometryOps.requireTolerance("CPCA rank tolerance", rankTolerance)
      _ <- blockRequest.validateAgainst(rowSpace.dimension, featureSpace.dimension)
      dense <- tableView.toDense(policy)
      rowMetricDense <- cpcaSemantic(rowMetric.toDense)
      featureMetricDense <- cpcaSemantic(featureMetric.toDense)
      rowRoots <- MetricSqrt.factorDense(rowMetricDense, eigenSolver, rankTolerance, "CPCA row metric")
      featureRoots <- MetricSqrt.factorDense(featureMetricDense, eigenSolver, rankTolerance, "CPCA feature metric")
      zStar = rowRoots.half.applyLeft(featureRoots.half.applyRight(dense))
      inertia <- partition(zStar)
      executions <- fitBlocks(zStar, rowRoots, featureRoots, blockRequest, svdSolver, rankTolerance)
    yield
      CpcaOperatorFit(
        rowSpace,
        featureSpace,
        rowConstraint,
        featureConstraint,
        inertia,
        executions.map(value => value.compatibility.block -> value.compatibility).toMap,
        executions.flatMap(_.operator).toVector,
        rowRoots,
        featureRoots,
        policy,
        rankTolerance,
        provenance.append(
          SemanticProvenanceEvent.Derived(
            "cpca-operator-fit",
            Vector(table.valueIdentity, rowRelationship.valueIdentity, featureCovariance.valueIdentity)
          )
        )
      )

  private def partition(zStar: DMat): Either[MultivarError, CpcaPartition] =
    for
      zH <- applyRight(featureConstraint, zStar, CpcaSubspaceMode.Project)
      b11 <- rowConstraint.project(zH)
      b01 = MatrixOps.subtract(zH, b11)
      zG <- rowConstraint.project(zStar)
      b10 = MatrixOps.subtract(zG, b11)
      partial = CpcaMath.add(CpcaMath.add(b11, b01), b10)
      b00 = MatrixOps.subtract(zStar, partial)
      total = CpcaMath.frobeniusNorm2(zStar)
    yield
      val sums = Vector(
        CpcaBlock.GxH -> CpcaMath.frobeniusNorm2(b11),
        CpcaBlock.G0xH -> CpcaMath.frobeniusNorm2(b01),
        CpcaBlock.GxH0 -> CpcaMath.frobeniusNorm2(b10),
        CpcaBlock.G0xH0 -> CpcaMath.frobeniusNorm2(b00)
      )
      CpcaPartition(
        total,
        sums.map: (block, value) =>
          CpcaBlockInertia(block, value, if total > 0.0 then value / total else 0.0)
      )

  private def fitBlocks(
      zStar: DMat,
      rowRoots: MetricRoots,
      featureRoots: MetricRoots,
      request: CpcaBlockRequest,
      svdSolver: SvdSolver,
      tolerance: Double
  ): Either[MultivarError, Vector[CpcaBlockExecution[Rows, Feature]]] =
    MatrixOps.traverse(request.blocks): block =>
      fitBlock(
        zStar,
        rowRoots,
        featureRoots,
        block,
        request.requestedComponents(block),
        svdSolver,
        tolerance
      )

  private def fitBlock(
      zStar: DMat,
      rowRoots: MetricRoots,
      featureRoots: MetricRoots,
      block: CpcaBlock,
      requested: Option[ComponentCount],
      svdSolver: SvdSolver,
      tolerance: Double
  ): Either[MultivarError, CpcaBlockExecution[Rows, Feature]] =
    val rankLimit = blockRankLimit(block)
    val requestedCount = requested.map(_.value).getOrElse(rankLimit)
    if requestedCount > rankLimit then Left(MultivarError.InvalidComponentRequest(requestedCount, rankLimit))
    else if rankLimit == 0 || isZeroBlock(block) then
      Right(CpcaBlockExecution(zeroBlock(block), None))
    else
      for
        materialized <- blockMatrix(zStar, block)
        svd <- svdSolver.decompose(MatrixView.dense(materialized), ComponentCount.unsafe(requestedCount))
        kept = keptComponents(svd.singularValues, tolerance)
        execution <-
          if kept == 0 then Right(CpcaBlockExecution(zeroBlock(block), None))
          else assembleBlock(block, materialized, svd, kept, rowRoots, featureRoots)
      yield execution

  private def assembleBlock(
      block: CpcaBlock,
      blockMatrix: DMat,
      svd: SvdResult,
      kept: Int,
      rowRoots: MetricRoots,
      featureRoots: MetricRoots
  ): Either[MultivarError, CpcaBlockExecution[Rows, Feature]] =
    val singularValues = MatrixOps.takeVector(svd.singularValues, kept)
    val uStar = MatrixOps.takeColumns(svd.u, kept)
    val vStar = MatrixOps.takeColumns(svd.v, kept)
    val u = rowRoots.pinvHalf.applyLeft(uStar)
    val v = featureRoots.pinvHalf.applyLeft(vStar)
    for
      rowCoordinates <- coordinates(rowConstraint, uStar, block.rowMode)
      featureCoordinates <- coordinates(featureConstraint, vStar, block.columnMode)
      compatibility = CpcaBlockFit(
        block,
        singularValues,
        uStar,
        vStar,
        u,
        v,
        rowCoordinates,
        featureCoordinates,
        CpcaMath.sumSquares(singularValues)
      )
      operator <- assembleProgramBlock(block, blockMatrix, compatibility)
    yield CpcaBlockExecution(compatibility, Some(operator))

  private def assembleProgramBlock(
      block: CpcaBlock,
      dense: DMat,
      compatibility: CpcaBlockFit
  ): Either[MultivarError, CpcaOperatorBlockFit[Rows, Feature, ? <: SemanticSpace]] =
    val identity = ValueIdentity.derived(s"cpca-${block.label}-table", table.valueIdentity)
    val fitProvenance = provenance.append(
      SemanticProvenanceEvent.Derived(
        s"cpca-${block.label}-cross-svd",
        Vector(table.valueIdentity, rowConstraint.projector.valueIdentity, featureConstraint.projector.valueIdentity)
      )
    )
    for
      component <- SpaceRef.of(
        s"${rowSpace.id.value}.${featureSpace.id.value}.cpca.${block.label}",
        SpaceRole.Latent,
        compatibility.rank
      )
      blockTable <- cpcaSemantic(
        Op.fromDense(
          dense,
          CoordinateEvidence.dual(featureSpace),
          CoordinateEvidence.primal(rowSpace),
          OperatorRoleWitness.table,
          identity,
          fitProvenance
        )
      )
      featureVariable <- cpcaProgram(
        FrameVariable.from(
          ParameterId.unsafe(s"${featureSpace.id.value}.cpca.${block.label}.feature-frame"),
          featureSpace,
          component.evidence
        )
      )
      featureFrameOperator <- cpcaSemantic(
        Op.fromDense(
          compatibility.vStar,
          CoordinateEvidence.primal(component.evidence),
          CoordinateEvidence.dual(featureSpace),
          OperatorRoleWitness.frame,
          ValueIdentity.derived("cpca-feature-frame", identity),
          fitProvenance
        )
      )
      featureGeometry <- identityCometric(featureSpace, s"cpca-${block.label}-feature-normalization", fitProvenance)
      featureFrame = FunctionalFrame(featureFrameOperator, Some(featureGeometry))
      rowGeometry <- identityMetric(rowSpace, s"cpca-${block.label}-row-relationship", fitProvenance)
      rowRelationship = rowGeometry.retag(OperatorRoleWitness.rowLink, s"cpca-${block.label}-row-link")
      featureOperator = OperatorAlgebra
        .secondOrder(blockTable, rowRelationship, blockTable)
        .retag(OperatorRoleWitness.covariance, s"cpca-${block.label}-feature-operator")
      rowScores = featureFrame.scores(blockTable)
      featureParameterization = FrameParameterization.identity(featureVariable)
      operatorProgram <- cpcaProgram(
        OperatorPrograms.gpca(
          featureParameterization,
          featureOperator,
          FrameNormalization(featureVariable, featureGeometry)
        )
      )
      context <- cpcaSemantic(
        CertificateContext.from(
          CertificateTolerance.strict,
          CertificateNorm.Frobenius,
          s"cpca-${block.label}-cross-svd",
          "gale",
          NumericalPrecision.Float64,
          None
        )
      )
      crossResidual = cpcaCrossResidual(
        dense,
        compatibility.uStar,
        compatibility.vStar,
        compatibility.singularValues
      )
      normalizationResidual = Math.max(
        cpcaGramResidual(compatibility.uStar),
        cpcaGramResidual(compatibility.vStar)
      )
      clusters = spectralClusters(compatibility.singularValues, CertificateTolerance.strict)
      genericFit <- cpcaProgram(
        OperatorProgramFit.from(
          operatorProgram,
          Vector(FittedFrame(featureVariable, featureFrame)),
          CpcaMath.sumSquares(compatibility.singularValues),
          NumericalIdentifiability(
            compatibility.rank,
            clusters,
            Math.max(crossResidual, normalizationResidual),
            context
          ),
          fitProvenance
        )
      )
    yield
      CpcaOperatorBlockFit(
        block,
        featureFrame,
        rowScores,
        blockTable,
        featureOperator,
        genericFit,
        compatibility,
        CpcaOperatorDiagnostics(
          compatibility.rank,
          crossResidual,
          normalizationResidual,
          clusters,
          "gale.spectral.Svd"
        ),
        fitProvenance
      )

  private def blockMatrix(zStar: DMat, block: CpcaBlock): Either[MultivarError, DMat] =
    for
      right <- applyRight(featureConstraint, zStar, block.columnMode)
      out <- applyLeft(rowConstraint, right, block.rowMode)
    yield out

  private def applyLeft[S <: SemanticSpace](
      constraint: CpcaOperatorConstraint[S],
      input: DMat,
      mode: CpcaSubspaceMode
  ): Either[MultivarError, DMat] =
    mode match
      case CpcaSubspaceMode.Project  => constraint.project(input)
      case CpcaSubspaceMode.Residual => constraint.residual(input)

  private def applyRight[S <: SemanticSpace](
      constraint: CpcaOperatorConstraint[S],
      input: DMat,
      mode: CpcaSubspaceMode
  ): Either[MultivarError, DMat] =
    applyLeft(constraint, input.transpose, mode).map(_.transpose)

  private def coordinates[S <: SemanticSpace](
      constraint: CpcaOperatorConstraint[S],
      input: DMat,
      mode: CpcaSubspaceMode
  ): Either[MultivarError, Option[DMat]] =
    (mode, constraint.constraint) match
      case (CpcaSubspaceMode.Project, CpcaConstraint.Basis(_, _)) =>
        constraint.coordinates(input).map(Some(_))
      case _ => Right(None)

  private def blockRankLimit(block: CpcaBlock): Int =
    val rowLimit = block.rowMode match
      case CpcaSubspaceMode.Project  => rowConstraint.rank
      case CpcaSubspaceMode.Residual => rowSpace.dimension - rowConstraint.rank
    val featureLimit = block.columnMode match
      case CpcaSubspaceMode.Project  => featureConstraint.rank
      case CpcaSubspaceMode.Residual => featureSpace.dimension - featureConstraint.rank
    Math.min(rowLimit, featureLimit)

  private def isZeroBlock(block: CpcaBlock): Boolean =
    annihilates(rowConstraint.constraint, block.rowMode) ||
      annihilates(featureConstraint.constraint, block.columnMode)

  private def annihilates(constraint: CpcaConstraint, mode: CpcaSubspaceMode): Boolean =
    (mode, constraint) match
      case (CpcaSubspaceMode.Project, CpcaConstraint.Zero)          => true
      case (CpcaSubspaceMode.Residual, CpcaConstraint.Identity)     => true
      case (CpcaSubspaceMode.Project, CpcaConstraint.Identity)      => false
      case (CpcaSubspaceMode.Project, CpcaConstraint.Basis(_, _))   => false
      case (CpcaSubspaceMode.Residual, CpcaConstraint.Zero)         => false
      case (CpcaSubspaceMode.Residual, CpcaConstraint.Basis(_, _))  => false

  private def zeroBlock(block: CpcaBlock): CpcaBlockFit =
    CpcaBlockFit(
      block,
      DVec.zeros(0),
      DMat.zeros(rowSpace.dimension, 0),
      DMat.zeros(featureSpace.dimension, 0),
      DMat.zeros(rowSpace.dimension, 0),
      DMat.zeros(featureSpace.dimension, 0),
      None,
      None,
      0.0
    )

  private def keptComponents(values: DVec, tolerance: Double): Int =
    if values.length == 0 then 0
    else
      val cutoff = tolerance * Math.max(1.0, values(0))
      var kept = 0
      while kept < values.length && values(kept) > cutoff do kept += 1
      kept

private final case class CpcaBlockExecution[Rows <: SemanticSpace, Feature <: SemanticSpace](
    compatibility: CpcaBlockFit,
    operator: Option[CpcaOperatorBlockFit[Rows, Feature, ? <: SemanticSpace]]
)

object CpcaOperatorProblem:
  /** Dynamic compatibility boundary. Once constructed, the problem contains
    * typed operators only; the legacy metrics are not consulted by fitting.
    */
  def fromMatrices(
      table: MatrixView,
      rowMetric: Option[MvMetric],
      featureMetric: Option[MvMetric],
      rowConstraint: CpcaConstraint,
      featureConstraint: CpcaConstraint,
      rowSpace: MvSpace,
      featureSpace: MvSpace,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      tolerance: Double = 1e-12,
      policy: StoragePolicy = StoragePolicy.AllowDense,
      provenanceLabel: String = "cpca-operator-problem"
  ): Either[MultivarError, PreparedCpcaOperatorProblem] =
    val rows = SpaceRef(rowSpace)
    val features = SpaceRef(featureSpace)
    val sourceIdentity = ValueIdentity.source(ValueId.unsafe(s"${rowSpace.id.value}.${featureSpace.id.value}.cpca-source"))
    val provenance = SemanticProvenance.source(provenanceLabel)
    for
      rowValue <- rowMetric match
        case Some(value) => Right(value)
        case None        => MvMetric.identity(table.rows, Some(rowSpace))
      featureValue <- featureMetric match
        case Some(value) => Right(value)
        case None        => MvMetric.identity(table.cols, Some(featureSpace))
      _ <- requireMetric(IndexAxis.Row, rowValue, rowSpace)
      _ <- requireMetric(IndexAxis.Feature, featureValue, featureSpace)
      rowDense <- rowValue.toDense(policy)
      featureDense <- featureValue.toDense(policy)
      problem <- fromPrepared(
        rows.evidence,
        features.evidence,
        table,
        rowDense,
        featureDense,
        rowConstraint,
        featureConstraint,
        eigenSolver,
        tolerance,
        sourceIdentity,
        provenance
      )
    yield new PreparedCpcaOperatorProblem(rows, features)(problem)

  private[multivar] def fromCompatibility(
      problem: CpcaProblem,
      eigenSolver: SymmetricEigenSolver,
      tolerance: Double,
      policy: StoragePolicy
  ): Either[MultivarError, PreparedCpcaOperatorProblem] =
    fromMatrices(
      problem.diagram.table,
      Some(problem.diagram.rowMetric),
      Some(problem.diagram.columnMetric),
      problem.rowConstraint.constraint,
      problem.columnConstraint.constraint,
      problem.diagram.rowSpace,
      problem.diagram.columnSpace,
      eigenSolver,
      tolerance,
      policy,
      "cpca-compatibility-adapter"
    )

  private[multivar] def fromPrepared[Rows <: SemanticSpace, Feature <: SemanticSpace](
      rows: SpaceEvidence[Rows],
      features: SpaceEvidence[Feature],
      tableView: MatrixView,
      rowMetricDense: DMat,
      featureMetricDense: DMat,
      rowConstraintSpec: CpcaConstraint,
      featureConstraintSpec: CpcaConstraint,
      eigenSolver: SymmetricEigenSolver,
      tolerance: Double,
      sourceIdentity: ValueIdentity,
      provenance: SemanticProvenance
  ): Either[MultivarError, CpcaOperatorProblem[Rows, Feature]] =
    for
      table <- cpcaSemantic(
        Op.fromMatrixView(
          tableView,
          CoordinateEvidence.dual(features),
          CoordinateEvidence.primal(rows),
          OperatorRoleWitness.table,
          ValueIdentity.derived("cpca-table", sourceIdentity),
          provenance
        )
      )
      rowMetric <- certifiedMetric(rows, rowMetricDense, "cpca-row-metric", sourceIdentity, provenance)
      featureMetric <- certifiedMetric(features, featureMetricDense, "cpca-feature-metric", sourceIdentity, provenance)
      relationship = rowMetric.retag(OperatorRoleWitness.rowLink, "cpca-row-relationship")
      covarianceUnchecked = OperatorAlgebra
        .secondOrder(table, relationship, table)
        .retag(OperatorRoleWitness.covariance, "cpca-feature-covariance")
      rowConstraint <- CpcaOperatorConstraint.resolve(
        rowConstraintSpec,
        IndexAxis.Row,
        rows,
        rowMetric,
        eigenSolver,
        tolerance
      )
      featureConstraint <- CpcaOperatorConstraint.resolve(
        featureConstraintSpec,
        IndexAxis.Feature,
        features,
        featureMetric,
        eigenSolver,
        tolerance
      )
    yield
      new CpcaOperatorProblem(
        rows,
        features,
        table,
        rowMetric,
        featureMetric,
        relationship,
        covarianceUnchecked,
        rowConstraint,
        featureConstraint,
        tableView,
        tolerance,
        provenance
      )

  private def certifiedMetric[S <: SemanticSpace](
      space: SpaceEvidence[S],
      dense: DMat,
      label: String,
      source: ValueIdentity,
      provenance: SemanticProvenance
  ): Either[MultivarError, Op[Primal[S], Dual[S], MetricOperatorRole, CertifiedPsd]] =
    val identity = ValueIdentity.derived(label, source)
    for
      linear <- cpcaSemantic(
        Lin.fromDenseMatrix(
          dense,
          CoordinateEvidence.primal(space),
          CoordinateEvidence.dual(space),
          identity,
          provenance.append(SemanticProvenanceEvent.Derived(label, Vector(source)))
        )
      )
      certificate <- cpcaSemantic(FormCertificates.psd(linear))
      certified <- cpcaSemantic(Op.certifiedPsd(Op.fromLin(linear, OperatorRoleWitness.metric), certificate))
    yield certified

  private def requireMetric(axis: IndexAxis, metric: MvMetric, space: MvSpace): Either[MultivarError, Unit] =
    if metric.dim != space.size then Left(MultivarError.MetricShapeMismatch(axis, space.size, metric.dim))
    else
      metric.space match
        case Some(value) if value != space =>
          Left(MultivarError.MetricMismatch(s"CPCA ${axis.label} metric belongs to '${value.id.value}', expected '${space.id.value}'"))
        case _ => Right(())

final class PreparedCpcaOperatorProblem private[multivar] (
    val rows: SpaceRef,
    val features: SpaceRef
)(
    val value: CpcaOperatorProblem[rows.Id, features.Id]
):
  def tableDense: Either[MultivarError, DMat] =
    cpcaSemantic(value.table.toDense)

  /** Rebind a resampled or residualized table to the already resolved CPCA
    * geometry. Statistical constraints are re-certified against the same
    * nominal spaces; no legacy diagram or map is reconstructed.
    */
  def withTable(
      table: MatrixView,
      provenanceLabel: String
  ): Either[MultivarError, PreparedCpcaOperatorProblem] =
    if table.rows != rows.descriptor.size || table.cols != features.descriptor.size then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"replacement CPCA table is ${table.rows}x${table.cols}, expected ${rows.descriptor.size}x${features.descriptor.size}"
        )
      )
    else
      val cleanLabel = provenanceLabel.trim
      if cleanLabel.isEmpty then Left(MultivarError.InvalidId("CPCA replacement provenance", provenanceLabel, "must be non-empty"))
      else
        val source = ValueIdentity.derived("cpca-replaced-table", value.table.valueIdentity)
        val provenance = value.provenance.append(
          SemanticProvenanceEvent.Derived(cleanLabel, Vector(value.table.valueIdentity))
        )
        for
          rowMetric <- cpcaSemantic(value.rowMetric.toDense)
          featureMetric <- cpcaSemantic(value.featureMetric.toDense)
          problem <- CpcaOperatorProblem.fromPrepared(
            rows.evidence,
            features.evidence,
            table,
            rowMetric,
            featureMetric,
            value.rowConstraint.constraint,
            value.featureConstraint.constraint,
            DenseSolvers.symmetricEigen,
            value.constructionTolerance,
            source,
            provenance
          )
        yield new PreparedCpcaOperatorProblem(rows, features)(problem)

  def fit(
      blockRequest: CpcaBlockRequest,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      svdSolver: SvdSolver = DenseSolvers.svd,
      rankTolerance: Double = 1e-12,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, PreparedCpcaOperatorFit] =
    value
      .fit(blockRequest, eigenSolver, svdSolver, rankTolerance, policy)
      .map(fit => new PreparedCpcaOperatorFit(rows, features)(fit))

final class PreparedCpcaOperatorFit private[multivar] (
    val rows: SpaceRef,
    val features: SpaceRef
)(
    val value: CpcaOperatorFit[rows.Id, features.Id]
):
  def rowSpace: MvSpace = rows.descriptor
  def featureSpace: MvSpace = features.descriptor
  def rowConstraint: CpcaOperatorConstraint[rows.Id] = value.rowConstraint
  def featureConstraint: CpcaOperatorConstraint[features.Id] = value.featureConstraint
  def partition: CpcaPartition = value.partition
  def blocks: Map[CpcaBlock, CpcaBlockFit] = value.blocks
  def block(block: CpcaBlock): Option[CpcaBlockFit] = value.block(block)
  def operatorBlock(block: CpcaBlock): Option[CpcaOperatorBlockFit[rows.Id, features.Id, ? <: SemanticSpace]] =
    value.operatorBlock(block)
  def operatorBlocks: Vector[CpcaOperatorBlockFit[rows.Id, features.Id, ? <: SemanticSpace]] = value.operatorBlocks
  def totalSS: Double = value.totalSS

private def identityCometric[S <: SemanticSpace](
    space: SpaceEvidence[S],
    label: String,
    provenance: SemanticProvenance
): Either[MultivarError, OpCometric[S, CertifiedSpd]] =
  val identity = ValueIdentity.source(ValueId.unsafe(s"${space.id.value}.$label"))
  for
    linear <- cpcaSemantic(
      Lin.fromDenseMatrix(
        DMat.eye(space.dimension),
        CoordinateEvidence.dual(space),
        CoordinateEvidence.primal(space),
        identity,
        provenance
      )
    )
    certificate <- cpcaSemantic(FormCertificates.spd(linear))
    certified <- cpcaSemantic(Op.certifiedSpd(Op.fromLin(linear, OperatorRoleWitness.cometric), certificate))
  yield certified

private def identityMetric[S <: SemanticSpace](
    space: SpaceEvidence[S],
    label: String,
    provenance: SemanticProvenance
): Either[MultivarError, OpMetric[S, CertifiedSpd]] =
  val identity = ValueIdentity.source(ValueId.unsafe(s"${space.id.value}.$label"))
  for
    linear <- cpcaSemantic(
      Lin.fromDenseMatrix(
        DMat.eye(space.dimension),
        CoordinateEvidence.primal(space),
        CoordinateEvidence.dual(space),
        identity,
        provenance
      )
    )
    certificate <- cpcaSemantic(FormCertificates.spd(linear))
    certified <- cpcaSemantic(Op.certifiedSpd(Op.fromLin(linear, OperatorRoleWitness.metric), certificate))
  yield certified

private def cpcaCrossResidual(block: DMat, u: DMat, v: DMat, singularValues: DVec): Double =
  val expected = MatrixOps.scaleColumns(u, singularValues)
  val actual = GaleNumerics.multiply(block, v)
  frobenius(MatrixOps.subtract(actual, expected))

private def cpcaGramResidual(frame: DMat): Double =
  val gram = GaleNumerics.multiply(frame.transpose, frame)
  frobenius(MatrixOps.subtract(gram, DMat.eye(gram.rows)))

private def cpcaSemantic[A](result: Either[SemanticError, A]): Either[MultivarError, A] =
  result.left.map:
    case SemanticError.MultivarFailure(error) => error
    case SemanticError.LinearMapFailure(error) => LinalgErrorAdapter.toMultivarError(error)
    case error => MultivarError.SolverFailed(error.message)

private def cpcaProgram[A](result: Either[ProgramError, A]): Either[MultivarError, A] =
  result.left.map(error => MultivarError.SolverFailed(error.message))
