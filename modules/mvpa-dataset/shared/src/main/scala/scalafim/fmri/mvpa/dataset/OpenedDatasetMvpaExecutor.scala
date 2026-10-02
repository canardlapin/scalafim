package scalafim.fmri.mvpa.dataset

import cats.Monad
import cats.data.EitherT
import scalafim.dataset.{
  AssemblyPolicy,
  DatasetRunQuery,
  OpenedDataset
}

object OpenedDatasetMvpaExecutor:
  def observations[F[_]: Monad](
      opened: OpenedDataset[F],
      query: DatasetRunQuery,
      request: DatasetPatternRequest
  ): EitherT[F, MvpaDatasetError, DatasetObservationEvidence] =
    opened
      .read(
        query,
        request.selection,
        AssemblyPolicy.Segmented
      )
      .leftMap(error =>
        MvpaDatasetError.DatasetReadFailed(opened.dataset.id.value, error.message)
      )
      .subflatMap: result =>
        result
          .toFmriSeries(opened.dataset)
          .left
          .map(error =>
            MvpaDatasetError.DatasetReadFailed(
              opened.dataset.id.value,
              error.message
            )
          )
      .subflatMap: series =>
        DatasetObservationEvidence.fromSeries(series, request.metadata, request.featureSpaceId, Some(opened.dataset.id))
