package scalafim.archive.zarr

import scalafim.zarr.{AsyncCodecRuntime, JvmAsyncCodecRuntime}

import scala.concurrent.ExecutionContext

object TestZarrRuntime:
  val runtime: AsyncCodecRuntime =
    JvmAsyncCodecRuntime.portable(ExecutionContext.global)
