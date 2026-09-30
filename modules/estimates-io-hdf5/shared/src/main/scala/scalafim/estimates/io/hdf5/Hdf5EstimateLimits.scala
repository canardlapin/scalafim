package scalafim.estimates.io.hdf5

import scalafim.archive.hdf5.Hdf5Limits

/** Coverage memory is independent of the native metadata cache reservation. */
final case class Hdf5EstimateLimits(
    maximumBlockCells: Int = 65536,
    maximumCoverageBytes: Long = 4L * 1024L * 1024L,
    maximumProducts: Int = 32
):
  require(maximumBlockCells > 0 && maximumBlockCells <= 65536)
  require(maximumCoverageBytes > 0 && maximumCoverageBytes <= Int.MaxValue)
  require(maximumProducts > 0 && maximumProducts <= 32)
  val archive: Hdf5Limits = Hdf5Limits(maximumBlockCells, 1048576, 521, 4194304, 1, 2, 24)
