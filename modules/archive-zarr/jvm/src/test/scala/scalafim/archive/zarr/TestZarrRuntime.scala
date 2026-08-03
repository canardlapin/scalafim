package scalafim.archive.zarr

import zarr4s.{AsyncCodecRuntime, JvmAsyncCodecRuntime}

import scala.concurrent.ExecutionContext

object TestZarrRuntime:
  val runtime: AsyncCodecRuntime =
    JvmAsyncCodecRuntime.portable(ExecutionContext.global)
