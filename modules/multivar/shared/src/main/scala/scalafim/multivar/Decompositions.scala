package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

final case class SvdFit(result: SvdResult, projection: BiProjection):
  def project(input: MatrixView): Either[MultivarError, DoubleMatrix] =
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
  def project(input: MatrixView): Either[MultivarError, DoubleMatrix] =
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

final case class PlscFit(paired: PairedLatentFit, result: SvdResult):
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
      paired <- pairedDiagram(xp, yp, "plsc", rowMetric = rowMetric)
      xMetric <- PairedGmdMetric.identity(paired.x.columnSpace)
      yMetric <- PairedGmdMetric.identity(paired.y.columnSpace)
      gmd <- PairedGmd.fit(
        paired,
        components,
        xMetric,
        yMetric,
        crossScale = covarianceScale(x.rows),
        solver,
        eigenSolver,
        policy
      )
      fit <- buildPairedLatentFit(
        method = PairedLatentMethod.Plsc,
        methodLabel = "plsc",
        components = components,
        svd = gmd.svd,
        spectrum = Spectrum.Covariance(gmd.svd.singularValues),
        xWeights = gmd.xWeights,
        yWeights = gmd.yWeights,
        xInput = x,
        yInput = y,
        xDomain = paired.x.columnSpace,
        yDomain = paired.y.columnSpace,
        fittedX = fittedX,
        fittedY = fittedY
      )
    yield PlscFit(fit, gmd.svd)

final case class CcaFit(paired: PairedLatentFit, result: SvdResult):
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
      paired <- pairedDiagram(xp, yp, "cca", rowMetric = rowMetric)
      cxx <- paired.x.rowGram(policy)
      cyy <- paired.y.rowGram(policy)
      xMetric <- PairedGmdMetric.inverseFromGram(
        MatrixOps.scale(cxx, denom),
        regularization.x.value,
        paired.x.columnSpace,
        eigenSolver,
        "cca x metric"
      )
      yMetric <- PairedGmdMetric.inverseFromGram(
        MatrixOps.scale(cyy, denom),
        regularization.y.value,
        paired.y.columnSpace,
        eigenSolver,
        "cca y metric"
      )
      gmd <- PairedGmd.fit(
        paired,
        components,
        xMetric,
        yMetric,
        crossScale = denom,
        solver,
        eigenSolver,
        policy
      )
      fit <- buildPairedLatentFit(
        method = PairedLatentMethod.Cca(regularization),
        methodLabel = "cca",
        components = components,
        svd = gmd.svd,
        spectrum = Spectrum.CanonicalCorrelations(gmd.svd.singularValues),
        xWeights = gmd.xWeights,
        yWeights = gmd.yWeights,
        xInput = x,
        yInput = y,
        xDomain = paired.x.columnSpace,
        yDomain = paired.y.columnSpace,
        fittedX = fittedX,
        fittedY = fittedY
      )
    yield CcaFit(fit, gmd.svd)

final case class ReducedRankRegressionFit(
    latent: PairedLatentFit,
    fullCoefficient: DoubleMatrix,
    workingCoefficient: MatrixMap,
    responsePreprocessor: FittedPreprocessor
):
  require(fullCoefficient.rows == workingCoefficient.weights.rows, "full coefficient rows must match low-rank coefficient rows")
  require(fullCoefficient.cols == workingCoefficient.weights.cols, "full coefficient columns must match low-rank coefficient columns")

  def predictWorking(input: MatrixView): Either[MultivarError, DoubleMatrix] =
    workingCoefficient.forward(input)

  def predict(input: MatrixView): Either[MultivarError, DoubleMatrix] =
    for
      working <- predictWorking(input)
      restored <- responsePreprocessor.inverseTransform(MatrixView.dense(working), policy = StoragePolicy.AllowDense)
      dense <- restored.toDense(StoragePolicy.AllowDense)
    yield dense

  def projectX(input: MatrixView): Either[MultivarError, DoubleMatrix] =
    latent.projectX(input)

  def projectY(input: MatrixView): Either[MultivarError, DoubleMatrix] =
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
      paired <- pairedDiagram(xp, yp, "rrr", rowMetric = rowMetric)
      cxx <- paired.x.rowGram(policy)
      cross <- DualityKernels.crossGram(paired, policy)
      // Ridge follows the covariance-scale convention shared with CCA (see
      // RegressionRegularization.Ridge): (X'X/(n-1) + lambda I)^-1 X'Y/(n-1) equals
      // (X'X + (n-1) lambda I)^-1 X'Y, applied here in the raw-Gram form so the
      // lambda = 0 path stays the exact OLS solution.
      xMetric <- PairedGmdMetric.inverseFromGram(
        cxx,
        regressionRidgeValue(regularization) * Math.max(1, x.rows - 1),
        paired.x.columnSpace,
        eigenSolver,
        "rrr x metric"
      )
      yMetric <- PairedGmdMetric.identity(paired.y.columnSpace)
      gmd <- PairedGmd.fit(
        paired,
        components,
        xMetric,
        yMetric,
        crossScale = 1.0,
        solver,
        eigenSolver,
        policy
      )
      coefficient <- xMetric.metric.matvec(cross)
      responseLoadings = gmd.yWeights
      encoderWeights = MetricOperator.scaleColumnsDense(gmd.xWeights, gmd.svd.singularValues)
      lowRankCoefficient = DoubleMatrix.multiply(encoderWeights, responseLoadings.transpose)
      workingMap <- MatrixMap.from(paired.x.columnSpace, paired.y.columnSpace, lowRankCoefficient, fittedX)
      latent <- buildPairedLatentFit(
        method = PairedLatentMethod.ReducedRankRegression(RegressionDirection.XToY, regularization),
        methodLabel = "rrr",
        components = components,
        svd = gmd.svd,
        spectrum = Spectrum.SingularValues(gmd.svd.singularValues),
        xWeights = encoderWeights,
        yWeights = responseLoadings,
        xInput = x,
        yInput = y,
        xDomain = paired.x.columnSpace,
        yDomain = paired.y.columnSpace,
        fittedX = fittedX,
        fittedY = fittedY
      )
    yield ReducedRankRegressionFit(latent, coefficient, workingMap, fittedY)

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

private def regressionRidgeValue(regularization: RegressionRegularization): Double =
  regularization match
    case RegressionRegularization.Ols =>
      0.0
    case RegressionRegularization.Ridge(value) =>
      value.value

private def covarianceScale(rows: Int): Double =
  1.0 / Math.max(1, rows - 1)

private def pairedDiagram(
    x: MatrixView,
    y: MatrixView,
    method: String,
    rowMetric: Option[MvMetric] = None
): Either[MultivarError, PairedDualityDiagram] =
  for
    sampleSpace <- rowMetric.flatMap(_.space) match
      case Some(space) => Right(space)
      case None        => MvSpace.of(s"$method.samples", SpaceRole.Samples, x.rows)
    xSpace <- MvSpace.of(s"$method.x", SpaceRole.Observed, x.cols)
    ySpace <- MvSpace.of(s"$method.y", SpaceRole.Observed, y.cols)
    paired <- Unsafe.pairedDiagramFromArrays(
      x,
      y,
      reason = s"legacy $method API receives two positional matrices",
      rowMetric = rowMetric,
      sampleSpace = Some(sampleSpace),
      xSpace = Some(xSpace),
      ySpace = Some(ySpace)
    )
  yield paired

private final case class PairedGmdMetric(metric: MvMetric, half: MetricOperator)

private object PairedGmdMetric:
  def identity(space: MvSpace): Either[MultivarError, PairedGmdMetric] =
    MvMetric.identity(space.size, Some(space)).map(PairedGmdMetric(_, MetricOperator.Identity(space.size)))

  def inverseFromGram(
      gram: DoubleMatrix,
      ridge: Double,
      space: MvSpace,
      eigenSolver: SymmetricEigenSolver,
      role: String
  ): Either[MultivarError, PairedGmdMetric] =
    for
      _ <-
        if gram.rows == space.size && gram.cols == space.size then Right(())
        else Left(MultivarError.MatrixShapeMismatch(s"$role gram ${gram.rows}x${gram.cols} does not match space '${space.id.value}'"))
      half <- MatrixOps.inverseSquareRoot(MatrixOps.addRidge(gram, ridge), eigenSolver, 1e-12)
      metricMatrix = DualityKernels.symmetrize(DoubleMatrix.multiply(half, half))
      metric <- MvMetric.denseSymmetric(metricMatrix, MetricValidation.Trusted, Some(space))
    yield PairedGmdMetric(metric, MetricOperator.Dense(half))

private final case class PairedGmdResult(svd: SvdResult, xWeights: DoubleMatrix, yWeights: DoubleMatrix)

private object PairedGmd:
  def fit(
      paired: PairedDualityDiagram,
      components: ComponentCount,
      xMetric: PairedGmdMetric,
      yMetric: PairedGmdMetric,
      crossScale: Double,
      solver: SvdSolver,
      eigenSolver: SymmetricEigenSolver,
      policy: StoragePolicy
  ): Either[MultivarError, PairedGmdResult] =
    for
      _ <- validateColumnMetric("x", xMetric.metric, paired.x.columnSpace)
      _ <- validateColumnMetric("y", yMetric.metric, paired.y.columnSpace)
      cross <- DualityKernels.crossGram(paired, policy)
      scaledCross = MatrixOps.scale(cross, crossScale)
      operator = yMetric.half.applyRight(xMetric.half.applyLeft(scaledCross))
      svd <- solver.decompose(MatrixView.dense(operator), components)
      _ <- requireComponents(svd)
      xWeights = xMetric.half.applyLeft(svd.u)
      yWeights = yMetric.half.applyLeft(svd.v)
    yield PairedGmdResult(svd, xWeights, yWeights)

  private def validateColumnMetric(
      side: String,
      metric: MvMetric,
      space: MvSpace
  ): Either[MultivarError, Unit] =
    if metric.dim != space.size then Left(MultivarError.MetricShapeMismatch(IndexAxis.Feature, space.size, metric.dim))
    else
      metric.space match
        case Some(metricSpace) if metricSpace != space =>
          Left(
            MultivarError.MatrixShapeMismatch(
              s"paired $side metric space '${metricSpace.id.value}' does not match '${space.id.value}'"
            )
          )
        case _ =>
          Right(())

private def buildPairedLatentFit(
    method: PairedLatentMethod,
    methodLabel: String,
    components: ComponentCount,
    svd: SvdResult,
    spectrum: Spectrum,
    xWeights: DoubleMatrix,
    yWeights: DoubleMatrix,
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
