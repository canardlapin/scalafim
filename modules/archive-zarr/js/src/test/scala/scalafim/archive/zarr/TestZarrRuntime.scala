package scalafim.archive.zarr

import zarr4s.{AsyncCodecRuntime, BrowserCodecRuntime}

object TestZarrRuntime:
  val runtime: AsyncCodecRuntime =
    BrowserCodecRuntime.portable
