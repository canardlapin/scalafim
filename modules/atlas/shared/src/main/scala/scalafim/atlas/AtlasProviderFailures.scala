package scalafim.atlas

import neuroatlas4s.*
import neurosurf4s.routing.SpatialError

/** A transform manifest does not form a valid spatial graph. */
final case class TransformGraphFailure(cause: SpatialError) extends AtlasProviderFailure:
  def message: String = s"transform manifest does not form a valid spatial graph: ${cause.message}"
