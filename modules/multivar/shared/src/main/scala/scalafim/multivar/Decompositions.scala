package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

final case class SvdFit(result: SvdResult, projection: BiProjection):
  def project(input: MatrixView): Either[MultivarError, DMat] =
    projection.project(input)

object Svd:
  def fit(
      input: MatrixView,
      components: ComponentCount,
      preproc: PreprocessSpec = PreprocessSpec.Pass,
      solver: SvdSolver = DenseSolvers.svd
  ): Either[MultivarError, SvdFit] =
    fitBiProjection(input, components, preproc, solver, method = "svd").map { case (svd, projection) =>
      SvdFit(svd, projection)
    }

final case class PcaFit(result: SvdResult, projection: BiProjection):
  def project(input: MatrixView): Either[MultivarError, DMat] =
    projection.project(input)

object Pca:
  def fit(
      input: MatrixView,
      components: ComponentCount,
      preproc: PreprocessSpec = PreprocessSpec.Center,
      solver: SvdSolver = DenseSolvers.svd
  ): Either[MultivarError, PcaFit] =
    val observedSpace = MvSpace(SpaceId.unsafe("pca.observed"), SpaceRole.Observed, Dimension.unsafe(input.cols))
    GenPca
      .identityBackend(input, components, preproc, solver, observedSpace, method = "pca", latentId = "pca.latent")
      .map { fit =>
        PcaFit(SvdResult(fit.result.ou, fit.result.d, fit.result.ov), fit.projection)
      }

final case class PlscFit(
    paired: PairedLatentFit,
    result: SvdResult,
    operator: PairedOperatorFit[? <: SemanticSpace, ? <: SemanticSpace, ? <: SemanticSpace]
):
  def projection: CrossProjection =
    paired.projection

object Plsc:
  def fit(
      x: MatrixView,
      y: MatrixView,
      components: ComponentCount,
      xPreproc: PreprocessSpec = PreprocessSpec.Center,
      yPreproc: PreprocessSpec = PreprocessSpec.Center,
      solver: SvdSolver = DenseSolvers.svd,
      rowMetric: Option[MvMetric] = None,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, PlscFit] =
    for
      fittedX <- xPreproc.fit(x)
      fittedY <- yPreproc.fit(y)
      xp <- fittedX.transform(x)
      yp <- fittedY.transform(y)
      problem <- PairedOperatorProblem.fromMatrices(xp, yp, rowMetric, "plsc", policy)
      operator <- problem.fitPlsc(
        components,
        covarianceScale(x.rows),
        solver,
        eigenSolver
      )
      xWeights <- operator.sourceWeights
      yWeights <- operator.targetWeights
      fit <- buildPairedLatentFit(
        method = PairedLatentMethod.Plsc,
        methodLabel = "plsc",
        components = components,
        svd = operator.result,
        spectrum = Spectrum.Covariance(operator.result.singularValues),
        xWeights = xWeights,
        yWeights = yWeights,
        xInput = x,
        yInput = y,
        xDomain = problem.sourceFeatures.descriptor,
        yDomain = problem.targetFeatures.descriptor,
        fittedX = fittedX,
        fittedY = fittedY
      )
    yield PlscFit(fit, operator.result, operator)

final case class CcaFit(
    paired: PairedLatentFit,
    result: SvdResult,
    operator: PairedOperatorFit[? <: SemanticSpace, ? <: SemanticSpace, ? <: SemanticSpace]
):
  def projection: CrossProjection =
    paired.projection

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
      rowMetric: Option[MvMetric] = None,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, CcaFit] =
    CcaRegularization.symmetric(ridge).flatMap { regularization =>
      fitRegularized(x, y, components, regularization, xPreproc, yPreproc, eigenSolver, solver, rowMetric, policy)
    }

  def fitRegularized(
      x: MatrixView,
      y: MatrixView,
      components: ComponentCount,
      regularization: CcaRegularization,
      xPreproc: PreprocessSpec = PreprocessSpec.Center,
      yPreproc: PreprocessSpec = PreprocessSpec.Center,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      solver: SvdSolver = DenseSolvers.svd,
      rowMetric: Option[MvMetric] = None,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, CcaFit] =
    val denom = covarianceScale(x.rows)
    for
      fittedX <- xPreproc.fit(x)
      fittedY <- yPreproc.fit(y)
      xp <- fittedX.transform(x)
      yp <- fittedY.transform(y)
      problem <- PairedOperatorProblem.fromMatrices(xp, yp, rowMetric, "cca", policy)
      operator <- problem.fitCca(
        components,
        regularization,
        denom,
        solver,
        eigenSolver
      )
      xWeights <- operator.sourceWeights
      yWeights <- operator.targetWeights
      fit <- buildPairedLatentFit(
        method = PairedLatentMethod.Cca(regularization),
        methodLabel = "cca",
        components = components,
        svd = operator.result,
        spectrum = Spectrum.CanonicalCorrelations(operator.result.singularValues),
        xWeights = xWeights,
        yWeights = yWeights,
        xInput = x,
        yInput = y,
        xDomain = problem.sourceFeatures.descriptor,
        yDomain = problem.targetFeatures.descriptor,
        fittedX = fittedX,
        fittedY = fittedY
      )
    yield CcaFit(fit, operator.result, operator)

final case class ReducedRankRegressionFit(
    latent: PairedLatentFit,
    fullCoefficient: DMat,
    workingCoefficient: MatrixMap,
    responsePreprocessor: FittedPreprocessor,
    operator: PairedOperatorFit[? <: SemanticSpace, ? <: SemanticSpace, ? <: SemanticSpace]
):
  require(fullCoefficient.rows == workingCoefficient.weights.rows, "full coefficient rows must match low-rank coefficient rows")
  require(fullCoefficient.cols == workingCoefficient.weights.cols, "full coefficient columns must match low-rank coefficient columns")

  def predictWorking(input: MatrixView): Either[MultivarError, DMat] =
    workingCoefficient.forward(input)

  def predict(input: MatrixView): Either[MultivarError, DMat] =
    for
      working <- predictWorking(input)
      restored <- responsePreprocessor.inverseTransform(MatrixView.dense(working), policy = StoragePolicy.AllowDense)
      dense <- restored.toDense(StoragePolicy.AllowDense)
    yield dense

  def projectX(input: MatrixView): Either[MultivarError, DMat] =
    latent.projectX(input)

  def projectY(input: MatrixView): Either[MultivarError, DMat] =
    latent.projectY(input)

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
      rowMetric: Option[MvMetric] = None,
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
      rowMetric: Option[MvMetric],
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
        case Some(value) => pairedOperatorSemantic(value.toDense)
        case None        => Left(MultivarError.SolverFailed("RRR operator fit omitted its directed coefficient"))
      encoderWeights = MetricOperator.scaleColumnsDense(sourceWeights, operator.result.singularValues)
      lowRankCoefficient = GaleNumerics.multiply(encoderWeights, responseLoadings.transpose)
      workingMap <- MatrixMap.from(
        problem.sourceFeatures.descriptor,
        problem.targetFeatures.descriptor,
        lowRankCoefficient,
        fittedX
      )
      latent <- buildPairedLatentFit(
        method = PairedLatentMethod.ReducedRankRegression(RegressionDirection.XToY, regularization),
        methodLabel = "rrr",
        components = components,
        svd = operator.result,
        spectrum = Spectrum.SingularValues(operator.result.singularValues),
        xWeights = encoderWeights,
        yWeights = responseLoadings,
        xInput = x,
        yInput = y,
        xDomain = problem.sourceFeatures.descriptor,
        yDomain = problem.targetFeatures.descriptor,
        fittedX = fittedX,
        fittedY = fittedY
      )
    yield ReducedRankRegressionFit(latent, coefficient, workingMap, fittedY, operator)

private def fitBiProjection(
    input: MatrixView,
    components: ComponentCount,
    preproc: PreprocessSpec,
    solver: SvdSolver,
    method: String
): Either[MultivarError, (SvdResult, BiProjection)] =
  for
    fitted <- preproc.fit(input)
    transformed <- fitted.transform(input)
    svd <- solver.decompose(transformed, components)
    _ <- requireComponents(svd)
    domain = MvSpace(SpaceId.unsafe(s"$method.observed"), SpaceRole.Observed, Dimension.unsafe(input.cols))
    latent = MvSpace(SpaceId.unsafe(s"$method.latent"), SpaceRole.Latent, Dimension.unsafe(svd.singularValues.length))
    map <- MatrixMap.from(domain, latent, svd.v, fitted)
    scores <- map.forward(input)
  yield
    val diagnostics = ProjectionDiagnostics(
      method = method,
      components = components,
      effectiveComponents = svd.singularValues.length,
      singularValues = Some(svd.singularValues)
    )
    val projection = BiProjection(
      map,
      scores,
      scale = Some(ComponentScale(svd.singularValues)),
      diagnostics = Some(diagnostics)
    )
    (svd, projection)

/** Whole-fit entry points reject a rank-0 SVD (an exactly zero spectrum) with a typed
  * error; callers with an empty-result contract handle rank 0 at their own call sites.
  */
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

private def pairedOperatorSemantic[A](result: Either[SemanticError, A]): Either[MultivarError, A] =
  result.left.map:
    case SemanticError.MultivarFailure(error)  => error
    case SemanticError.LinearMapFailure(error) => LinalgErrorAdapter.toMultivarError(error)
    case error                                 => MultivarError.SolverFailed(error.message)

private def buildPairedLatentFit(
    method: PairedLatentMethod,
    methodLabel: String,
    components: ComponentCount,
    svd: SvdResult,
    spectrum: Spectrum,
    xWeights: DMat,
    yWeights: DMat,
    xInput: MatrixView,
    yInput: MatrixView,
    xDomain: MvSpace,
    yDomain: MvSpace,
    fittedX: FittedPreprocessor,
    fittedY: FittedPreprocessor
): Either[MultivarError, PairedLatentFit] =
  for
    latent <- MvSpace.of(s"$methodLabel.latent", SpaceRole.Latent, svd.singularValues.length)
    xMap <- MatrixMap.from(xDomain, latent, xWeights, fittedX)
    yMap <- MatrixMap.from(yDomain, latent, yWeights, fittedY)
    xScores <- xMap.forward(xInput)
    yScores <- yMap.forward(yInput)
    diagnostics = ProjectionDiagnostics(
      method = methodLabel,
      components = components,
      effectiveComponents = svd.singularValues.length,
      singularValues = Some(svd.singularValues)
    )
    projection = CrossProjection(
      xMap,
      yMap,
      latent,
      xScores,
      yScores,
      scale = Some(ComponentScale(svd.singularValues)),
      diagnostics = Some(diagnostics)
    )
    fit <- PairedLatentFit.from(
      method,
      xWeights,
      yWeights,
      spectrum,
      projection,
      diagnostics
    )
  yield fit
