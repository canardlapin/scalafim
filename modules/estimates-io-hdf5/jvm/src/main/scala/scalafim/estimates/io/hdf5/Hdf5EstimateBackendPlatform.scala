package scalafim.estimates.io.hdf5

import scalafim.archive.hdf5.Hdf5Archive
import scalafim.estimates.{EstimateError, EstimateSetReader}

private[hdf5] object Hdf5EstimateBackendPlatform:
  def open(root: String, archive: Hdf5Archive, limits: Hdf5EstimateLimits): Either[EstimateError, EstimateSetReader] =
    if root == null || root.isEmpty then Left(EstimateError.Invalid("local root required"))
    else
      try Hdf5EstimateStore.open(java.nio.file.Path.of(root), archive, limits)
      catch case error: java.nio.file.InvalidPathException => Left(EstimateError.Invalid(error.getReason))
