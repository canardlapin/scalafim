package scalafim.archive.zarr

import scalafim.zarr.{AsyncCodecRuntime, BrowserCodecRuntime}

object TestZarrRuntime:
  val runtime: AsyncCodecRuntime =
    BrowserCodecRuntime.portable
