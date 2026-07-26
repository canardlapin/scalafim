package scalafim.multivar
package family.spectral


import scalafim.multivar.core.*
import scalafim.multivar.capability.*

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
