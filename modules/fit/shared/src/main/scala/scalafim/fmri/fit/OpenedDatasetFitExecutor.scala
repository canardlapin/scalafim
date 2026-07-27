package scalafim.fmri.fit

import cats.Monad
import cats.data.EitherT
import scalafim.dataset.{
  AssemblyPolicy,
  DataSelection,
  DatasetRunQuery,
  OpenedDataset
}
import scalafim.fmri.model.FitPlan

object OpenedDatasetFitExecutor:
  def fit[F[_]: Monad](
      opened: OpenedDataset[F],
      plan: FitPlan,
      query: DatasetRunQuery = DatasetRunQuery.All,
      selection: DataSelection = DataSelection.All
  ): EitherT[F, FitError, FmriFitResult] =
    if opened.dataset.id != plan.model.dataset.id then
      EitherT.leftT(
        FitError.InvalidFitAxis(
          "opened dataset",
          s"opened dataset '${opened.dataset.id.value}' does not match model dataset '${plan.model.dataset.id.value}'"
        )
      )
    else
      opened
        .read(
          query,
          selection,
          AssemblyPolicy.Segmented
        )
        .leftMap(FitChunkPlan.mapDatasetError)
        .subflatMap: result =>
          result
            .toFmriSeries(opened.dataset)
            .left
            .map(FitChunkPlan.mapDatasetError)
        .flatMap: series =>
          EitherT.fromEither[F](
            FitInterpreters
              .forPlan(plan)
              .flatMap(_.fit(plan, series))
          )
