package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

final case class ViewAssociationScore(
    viewId: BlockId,
    rowSpace: MvSpace,
    values: DoubleMatrix
)

final case class MultisetAssociationDiagnostics(
    featureRank: Int,
    requestedComponents: Int,
    returnedComponents: Int,
    rowOperatorRepresentation: OperatorRepresentation,
    tableRepresentation: OperatorRepresentation,
    formulation: String
)

final case class MultisetAssociationFit(
    eigenvalues: DoubleVector,
    featureAxes: DoubleMatrix,
    metricLoadings: DoubleMatrix,
    directSumScores: DoubleMatrix,
    viewScores: Vector[ViewAssociationScore],
    objective: ObjectiveDefinition,
    diagnostics: MultisetAssociationDiagnostics
)

/** Executable covariance-style multiset association over a compiled direct-sum
  * study. The row objective remains a structured operator. The requested dense
  * policy applies only to the finite feature-space eigensystem.
  */
object MultisetAssociation:
  def fit(
      study: DirectSumStudy,
      problem: MaximizeAssociation[study.rowSpace.Id],
      components: ComponentCount,
      policy: StoragePolicy,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      tolerance: Double = 1e-10
  ): Either[DirectSumError, MultisetAssociationFit] =
    if components.value > study.featureSpace.evidence.dimension then
      Left(
        DirectSumError.Multivar(
          MultivarError.InvalidComponentRequest(components.value, study.featureSpace.evidence.dimension)
        )
      )
    else if !tolerance.isFinite || tolerance < 0.0 then
      Left(DirectSumError.Multivar(MultivarError.InvalidTolerance("multiset association", tolerance)))
    else if policy != StoragePolicy.AllowDense then
      Left(
        DirectSumError.Multivar(
          MultivarError.DensificationRejected("multiset feature-space eigensystem", StorageKind.Operator)
        )
      )
    else if !study.columnGeometry.isSpd then
      Left(
        DirectSumError.InvalidStudy(
          "multiset association currently requires SPD block column geometries; apply an explicit singular policy first"
        )
      )
    else
      val featureDimension = study.featureSpace.evidence.dimension
      for
        r <- study.columnGeometry.operator(DoubleMatrix.eye(featureDimension)).left.map(DirectSumError.Semantic.apply)
        metric <- MvMetric
          .denseSymmetric(
            DualityKernels.symmetrize(r),
            MetricValidation.Trusted,
            Some(study.featureSpace.descriptor)
          )
          .left
          .map(DirectSumError.Multivar.apply)
        roots <- MetricSqrt
          .factor(metric, eigenSolver, tolerance, policy, "direct-sum column geometry")
          .left
          .map(DirectSumError.Multivar.apply)
        _ <-
          if roots.rank == featureDimension then Right(())
          else
            Left(
              DirectSumError.InvalidStudy(
                s"direct-sum column geometry has rank ${roots.rank}; expected $featureDimension after SPD validation"
              )
            )
        rHalf = roots.half.applyLeft(DoubleMatrix.eye(featureDimension))
        weightedTable <- study.table(rHalf).left.map(DirectSumError.Semantic.apply)
        linkedTable <- problem.objective.operator(weightedTable).left.map(DirectSumError.Semantic.apply)
        featureObjective = DualityKernels.symmetrize(DoubleMatrix.transposeMultiply(weightedTable, linkedTable))
        eigen <- LinalgErrorAdapter
          .adapt(eigenSolver.decompose(featureObjective))
          .left
          .map(DirectSumError.Multivar.apply)
        axesInWhitenedSpace = MatrixOps.takeColumns(eigen.vectors, components.value)
        axes = roots.pinvHalf.applyLeft(axesInWhitenedSpace)
        loadings = roots.half.applyLeft(axesInWhitenedSpace)
        scores <- study.table(loadings).left.map(DirectSumError.Semantic.apply)
      yield
        val viewScores = study.blocks.map { block =>
          val rows = block.rowOffset until (block.rowOffset + block.rowSpace.size)
          ViewAssociationScore(block.id, block.rowSpace, scores.selectRows(rows))
        }
        MultisetAssociationFit(
          MatrixOps.takeVector(eigen.values, components.value),
          axes,
          loadings,
          scores,
          viewScores,
          problem.definition,
          MultisetAssociationDiagnostics(
            roots.rank,
            components.value,
            components.value,
            problem.objective.operator.descriptor.representation,
            study.table.descriptor.representation,
            "symmetric eigendecomposition of (X R^1/2)* L (X R^1/2)"
          )
        )
