package scalafim.estimates.io.hdf5

import scalafim.archive.hdf5.Hdf5Archive
import scalafim.estimates.{EstimateError, EstimateSetReader}

/** Explicit physical capability facade; scientific contracts remain in estimates. */
object Hdf5EstimateBackend:
  def open(root: String, archive: Hdf5Archive, limits: Hdf5EstimateLimits = Hdf5EstimateLimits()): Either[EstimateError, EstimateSetReader] =
    Hdf5EstimateBackendPlatform.open(root, archive, limits)
