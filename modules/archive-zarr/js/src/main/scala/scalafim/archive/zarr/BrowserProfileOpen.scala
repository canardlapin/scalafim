package scalafim.archive.zarr

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import zarr4s.*

/** Scala.js compatibility name for the portable asynchronous profile handle. */
type BrowserOpenedCanonicalBold = AsyncOpenedCanonicalBold

/** Browser facade selecting the browser gzip executor by default. */
object BrowserNeuroArchiveZarr:
  def openCanonical(
      store: AsyncObjectReader,
      capabilities: ZarrCapabilities = ZarrCapabilities(),
      limits: ProfileOpenLimits = ProfileOpenLimits.default,
      runtime: AsyncCodecRuntime = BrowserCodecRuntime.portable
  )(using ExecutionContext): Future[Either[NeuroArchiveZarrError, BrowserOpenedCanonicalBold]] =
    AsyncNeuroArchiveZarr.openCanonical(store, capabilities, limits, runtime)
