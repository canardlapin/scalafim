package scalafim.estimates.io.hdf5

import scalafim.archive.hdf5.Hdf5Archive
import scalafim.estimates.{EstimateError, EstimateSetReader}

private[hdf5] object Hdf5EstimateBackendPlatform:
  def open(root: String, archive: Hdf5Archive, limits: Hdf5EstimateLimits): Either[EstimateError, EstimateSetReader] =
    Left(EstimateError.Unsupported("physical HDF5 estimate storage requires the explicit JVM native capability"))
