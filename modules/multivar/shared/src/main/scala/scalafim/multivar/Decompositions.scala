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
    fitBiProjection(input, components, preproc, solver, method = "pca").map { case (svd, projection) =>
      PcaFit(svd, projection)
    }

final case class PlscFit(result: SvdResult, projection: CrossProjection)

object Plsc:
  def fit(
      x: MatrixView,
      y: MatrixView,
      components: ComponentCount,
      xPreproc: PreprocessSpec = PreprocessSpec.Center,
      yPreproc: PreprocessSpec = PreprocessSpec.Center,
      solver: SvdSolver = DenseSolvers.svd
  ): Either[MultivarError, PlscFit] =
    if x.rows != y.rows then Left(MultivarError.MatrixShapeMismatch(s"PLSC expected equal rows, got ${x.rows} and ${y.rows}"))
    else
      for
        fittedX <- xPreproc.fit(x)
        fittedY <- yPreproc.fit(y)
        xp <- fittedX.transform(x)
        yp <- fittedY.transform(y)
        cross <- xp.transposeMultiply(yp)
        scaledCross = MatrixOps.scale(cross, 1.0 / Math.max(1, x.rows - 1))
        svd <- solver.decompose(MatrixView.dense(scaledCross), components)
        latent = MvSpace(SpaceId.unsafe("plsc.latent"), SpaceRole.Latent, Dimension.unsafe(components.value))
        xDomain = MvSpace(SpaceId.unsafe("plsc.x"), SpaceRole.Observed, Dimension.unsafe(x.cols))
        yDomain = MvSpace(SpaceId.unsafe("plsc.y"), SpaceRole.Observed, Dimension.unsafe(y.cols))
        xMap <- MatrixMap.from(xDomain, latent, svd.u, fittedX)
        yMap <- MatrixMap.from(yDomain, latent, svd.v, fittedY)
        xScores <- xMap.forward(x)
        yScores <- yMap.forward(y)
      yield
        val diagnostics = ProjectionDiagnostics(
          method = "plsc",
          components = components,
          singularValues = Some(svd.singularValues)
        )
        PlscFit(
          svd,
          CrossProjection(
            xMap,
            yMap,
            latent,
            xScores,
            yScores,
            scale = Some(ComponentScale(svd.singularValues)),
            diagnostics = Some(diagnostics)
          )
        )

final case class CcaFit(result: SvdResult, projection: CrossProjection)

object Cca:
  def fit(
      x: MatrixView,
      y: MatrixView,
      components: ComponentCount,
      ridge: Double = 1e-8,
      xPreproc: PreprocessSpec = PreprocessSpec.Center,
      yPreproc: PreprocessSpec = PreprocessSpec.Center,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      solver: SvdSolver = DenseSolvers.svd
  ): Either[MultivarError, CcaFit] =
    if x.rows != y.rows then Left(MultivarError.MatrixShapeMismatch(s"CCA expected equal rows, got ${x.rows} and ${y.rows}"))
    else if !ridge.isFinite || ridge < 0.0 then Left(MultivarError.SolverFailed(s"CCA ridge must be finite and non-negative, got $ridge"))
    else
      val denom = 1.0 / Math.max(1, x.rows - 1)
      for
        fittedX <- xPreproc.fit(x)
        fittedY <- yPreproc.fit(y)
        xp <- fittedX.transform(x)
        yp <- fittedY.transform(y)
        cxx0 <- xp.crossProduct
        cyy0 <- yp.crossProduct
        cxy0 <- xp.transposeMultiply(yp)
        cxx = MatrixOps.addRidge(MatrixOps.scale(cxx0, denom), ridge)
        cyy = MatrixOps.addRidge(MatrixOps.scale(cyy0, denom), ridge)
        cxy = MatrixOps.scale(cxy0, denom)
        wx <- MatrixOps.inverseSquareRoot(cxx, eigenSolver, 1e-12)
        wy <- MatrixOps.inverseSquareRoot(cyy, eigenSolver, 1e-12)
        whitened = DoubleMatrix.multiply(DoubleMatrix.multiply(wx, cxy), wy)
        svd <- solver.decompose(MatrixView.dense(whitened), components)
        xWeights = DoubleMatrix.multiply(wx, svd.u)
        yWeights = DoubleMatrix.multiply(wy, svd.v)
        latent = MvSpace(SpaceId.unsafe("cca.latent"), SpaceRole.Latent, Dimension.unsafe(components.value))
        xDomain = MvSpace(SpaceId.unsafe("cca.x"), SpaceRole.Observed, Dimension.unsafe(x.cols))
        yDomain = MvSpace(SpaceId.unsafe("cca.y"), SpaceRole.Observed, Dimension.unsafe(y.cols))
        xMap <- MatrixMap.from(xDomain, latent, xWeights, fittedX)
        yMap <- MatrixMap.from(yDomain, latent, yWeights, fittedY)
        xScores <- xMap.forward(x)
        yScores <- yMap.forward(y)
      yield
        val diagnostics = ProjectionDiagnostics(
          method = "cca",
          components = components,
          singularValues = Some(svd.singularValues)
        )
        CcaFit(
          svd,
          CrossProjection(
            xMap,
            yMap,
            latent,
            xScores,
            yScores,
            scale = Some(ComponentScale(svd.singularValues)),
            diagnostics = Some(diagnostics)
          )
        )

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
    domain = MvSpace(SpaceId.unsafe(s"$method.observed"), SpaceRole.Observed, Dimension.unsafe(input.cols))
    latent = MvSpace(SpaceId.unsafe(s"$method.latent"), SpaceRole.Latent, Dimension.unsafe(components.value))
    map <- MatrixMap.from(domain, latent, svd.v, fitted)
    scores <- map.forward(input)
  yield
    val diagnostics = ProjectionDiagnostics(
      method = method,
      components = components,
      singularValues = Some(svd.singularValues)
    )
    val projection = BiProjection(
      map,
      scores,
      scale = Some(ComponentScale(svd.singularValues)),
      diagnostics = Some(diagnostics)
    )
    (svd, projection)

