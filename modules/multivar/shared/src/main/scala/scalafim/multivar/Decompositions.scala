package scalafim.multivar

import gale.linalg.DMat

final case class SvdFit(result: SvdResult, transform: FittedFrameTransform):
  def project(input: MatrixView): Either[MultivarError, DMat] =
    transform.project(input)

object Svd:
  def fit(
      input: MatrixView,
      components: ComponentCount,
      preproc: PreprocessSpec = PreprocessSpec.Pass,
      solver: SvdSolver = DenseSolvers.svd
  ): Either[MultivarError, SvdFit] =
    fitFrame(input, components, preproc, solver, "svd").map((result, transform) => SvdFit(result, transform))

final case class PcaFit(result: SvdResult, transform: FittedFrameTransform):
  def project(input: MatrixView): Either[MultivarError, DMat] =
    transform.project(input)

object Pca:
  def fit(
      input: MatrixView,
      components: ComponentCount,
      preproc: PreprocessSpec = PreprocessSpec.Center,
      solver: SvdSolver = DenseSolvers.svd
  ): Either[MultivarError, PcaFit] =
    fitFrame(input, components, preproc, solver, "pca").map((result, transform) => PcaFit(result, transform))

final case class PlscFit(
    result: SvdResult,
    operator: PairedOperatorFit[? <: SemanticSpace, ? <: SemanticSpace, ? <: SemanticSpace],
    sourceTransform: FittedFrameTransform,
    targetTransform: FittedFrameTransform
):
  def xScores: DMat = sourceTransform.trainingValues
  def yScores: DMat = targetTransform.trainingValues
  def projectX(input: MatrixView): Either[MultivarError, DMat] = sourceTransform.project(input)
  def projectY(input: MatrixView): Either[MultivarError, DMat] = targetTransform.project(input)

object Plsc:
  def fit(
      x: MatrixView,
      y: MatrixView,
      components: ComponentCount,
      xPreproc: PreprocessSpec = PreprocessSpec.Center,
      yPreproc: PreprocessSpec = PreprocessSpec.Center,
      solver: SvdSolver = DenseSolvers.svd,
      rowMetric: Option[MetricSpec] = None,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, PlscFit] =
    for
      fittedX <- xPreproc.fit(x)
      fittedY <- yPreproc.fit(y)
      xp <- fittedX.transform(x)
      yp <- fittedY.transform(y)
      problem <- PairedOperatorProblem.fromMatrices(xp, yp, rowMetric, "plsc", policy)
      operator <- problem.fitPlsc(components, covarianceScale(x.rows), solver, eigenSolver)
      xWeights <- operator.sourceWeights
      yWeights <- operator.targetWeights
      xTransform <- FittedFrameTransform.fromTraining(
        x,
        xWeights,
        fittedX,
        "plsc.source",
        components,
        Some(operator.result.singularValues)
      )
      yTransform <- FittedFrameTransform.fromTraining(
        y,
        yWeights,
        fittedY,
        "plsc.target",
        components,
        Some(operator.result.singularValues)
      )
    yield PlscFit(operator.result, operator, xTransform, yTransform)

final case class CcaFit(
    result: SvdResult,
    operator: PairedOperatorFit[? <: SemanticSpace, ? <: SemanticSpace, ? <: SemanticSpace],
    sourceTransform: FittedFrameTransform,
    targetTransform: FittedFrameTransform
):
  def xScores: DMat = sourceTransform.trainingValues
  def yScores: DMat = targetTransform.trainingValues
  def projectX(input: MatrixView): Either[MultivarError, DMat] = sourceTransform.project(input)
  def projectY(input: MatrixView): Either[MultivarError, DMat] = targetTransform.project(input)

object Cca:
  def fit(
      x: MatrixView,
      y: MatrixView,
      components: ComponentCount,
      ridge: Double = 1e-8,
      xPreproc: PreprocessSpec = PreprocessSpec.Center,
      yPreproc: PreprocessSpec = PreprocessSpec.Center,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      solver: SvdSolver = DenseSolvers.svd,
      rowMetric: Option[MetricSpec] = None,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, CcaFit] =
    CcaRegularization.symmetric(ridge).flatMap: regularization =>
      fitRegularized(x, y, components, regularization, xPreproc, yPreproc, eigenSolver, solver, rowMetric, policy)

  def fitRegularized(
      x: MatrixView,
      y: MatrixView,
      components: ComponentCount,
      regularization: CcaRegularization,
      xPreproc: PreprocessSpec = PreprocessSpec.Center,
      yPreproc: PreprocessSpec = PreprocessSpec.Center,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      solver: SvdSolver = DenseSolvers.svd,
      rowMetric: Option[MetricSpec] = None,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, CcaFit] =
    for
      fittedX <- xPreproc.fit(x)
      fittedY <- yPreproc.fit(y)
      xp <- fittedX.transform(x)
      yp <- fittedY.transform(y)
      problem <- PairedOperatorProblem.fromMatrices(xp, yp, rowMetric, "cca", policy)
      operator <- problem.fitCca(
        components,
        regularization,
        covarianceScale(x.rows),
        solver,
        eigenSolver
      )
      xWeights <- operator.sourceWeights
      yWeights <- operator.targetWeights
      xTransform <- FittedFrameTransform.fromTraining(
        x,
        xWeights,
        fittedX,
        "cca.source",
        components,
        Some(operator.result.singularValues)
      )
      yTransform <- FittedFrameTransform.fromTraining(
        y,
        yWeights,
        fittedY,
        "cca.target",
        components,
        Some(operator.result.singularValues)
      )
    yield CcaFit(operator.result, operator, xTransform, yTransform)

final case class ReducedRankRegressionFit(
    fullCoefficient: DMat,
    coefficientTransform: FittedCoefficientTransform,
    sourceTransform: FittedFrameTransform,
    targetTransform: FittedFrameTransform,
    operator: PairedOperatorFit[? <: SemanticSpace, ? <: SemanticSpace, ? <: SemanticSpace]
):
  def predictWorking(input: MatrixView): Either[MultivarError, DMat] =
    coefficientTransform.predictWorking(input)

  def predict(input: MatrixView): Either[MultivarError, DMat] =
    coefficientTransform.predict(input)

  def projectX(input: MatrixView): Either[MultivarError, DMat] =
    sourceTransform.project(input)

  def projectY(input: MatrixView): Either[MultivarError, DMat] =
    targetTransform.project(input)

object ReducedRankRegression:
  def fit(
      x: MatrixView,
      y: MatrixView,
      components: ComponentCount,
      regularization: RegressionRegularization = RegressionRegularization.Ols,
      direction: RegressionDirection = RegressionDirection.XToY,
      xPreproc: PreprocessSpec = PreprocessSpec.Center,
      yPreproc: PreprocessSpec = PreprocessSpec.Center,
      solver: SvdSolver = DenseSolvers.svd,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      rowMetric: Option[MetricSpec] = None,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, ReducedRankRegressionFit] =
    direction match
      case RegressionDirection.XToY =>
        fitXToY(x, y, components, regularization, xPreproc, yPreproc, solver, eigenSolver, rowMetric, policy)

  private def fitXToY(
      x: MatrixView,
      y: MatrixView,
      components: ComponentCount,
      regularization: RegressionRegularization,
      xPreproc: PreprocessSpec,
      yPreproc: PreprocessSpec,
      solver: SvdSolver,
      eigenSolver: SymmetricEigenSolver,
      rowMetric: Option[MetricSpec],
      policy: StoragePolicy
  ): Either[MultivarError, ReducedRankRegressionFit] =
    for
      _ <- validateRrrComponentRequest(components, x.rows, x.cols, y.cols)
      fittedX <- xPreproc.fit(x)
      fittedY <- yPreproc.fit(y)
      xp <- fittedX.transform(x)
      yp <- fittedY.transform(y)
      problem <- PairedOperatorProblem.fromMatrices(xp, yp, rowMetric, "rrr", policy)
      operator <- problem.fitReducedRankRegression(
        components,
        regularization,
        Math.max(1, x.rows - 1).toDouble,
        solver,
        eigenSolver
      )
      sourceWeights <- operator.sourceWeights
      responseLoadings <- operator.targetWeights
      coefficient <- operator.coefficient match
        case Some(value) => decompositionSemantic(value.toDense)
        case None        => Left(MultivarError.SolverFailed("RRR operator fit omitted its directed coefficient"))
      encoderWeights = MetricOperator.scaleColumnsDense(sourceWeights, operator.result.singularValues)
      lowRankCoefficient = GaleNumerics.multiply(encoderWeights, responseLoadings.transpose)
      coefficientTransform <- FittedCoefficientTransform.from(lowRankCoefficient, fittedX, fittedY, "rrr")
      sourceTransform <- FittedFrameTransform.fromTraining(
        x,
        encoderWeights,
        fittedX,
        "rrr.source",
        components,
        Some(operator.result.singularValues)
      )
      targetTransform <- FittedFrameTransform.fromTraining(
        y,
        responseLoadings,
        fittedY,
        "rrr.target",
        components,
        Some(operator.result.singularValues)
      )
    yield ReducedRankRegressionFit(coefficient, coefficientTransform, sourceTransform, targetTransform, operator)

private def fitFrame(
    input: MatrixView,
    components: ComponentCount,
    preproc: PreprocessSpec,
    solver: SvdSolver,
    method: String
): Either[MultivarError, (SvdResult, FittedFrameTransform)] =
  for
    fitted <- preproc.fit(input)
    transformed <- fitted.transform(input)
    svd <- solver.decompose(transformed, components)
    _ <- requireComponents(svd)
    transform <- FittedFrameTransform.fromTraining(
      input,
      svd.v,
      fitted,
      method,
      components,
      Some(svd.singularValues)
    )
  yield (svd, transform)

private def requireComponents(svd: SvdResult): Either[MultivarError, Unit] =
  if svd.singularValues.length == 0 then Left(MultivarError.SolverFailed("no singular values above tolerance"))
  else Right(())

private def validateRrrComponentRequest(
    components: ComponentCount,
    rows: Int,
    xCols: Int,
    yCols: Int
): Either[MultivarError, Unit] =
  val limit = Math.min(rows, Math.min(xCols, yCols))
  if components.value > limit then Left(MultivarError.InvalidComponentRequest(components.value, limit))
  else Right(())

private def covarianceScale(rows: Int): Double =
  1.0 / Math.max(1, rows - 1)

private def decompositionSemantic[A](result: Either[SemanticError, A]): Either[MultivarError, A] =
  result.left.map:
    case SemanticError.MultivarFailure(error)  => error
    case SemanticError.LinearMapFailure(error) => LinalgErrorAdapter.toMultivarError(error)
    case error                                 => MultivarError.SolverFailed(error.message)
