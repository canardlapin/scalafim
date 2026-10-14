package scalafim.surface.view

import scalafim.surface.VertexId

/** Semantic events emitted by a flat-map host. Coordinates never replace the
  * scientific vertex identity carried by the event. */
enum SurfaceFlatMapInteraction:
  case Hover(surface: SurfaceId, vertex: Option[VertexId])
  case Select(action: SurfaceViewerAction)
  case RoiCreated(roi: SurfaceFlatMapRoi)

enum SurfaceFlatMapRoiMode:
  case Polygon, Lasso

enum SurfaceFlatMapInteractionError:
  case InvalidState(reason: String)

  def message: String = this match
    case InvalidState(reason) => s"invalid flat-map viewer state: $reason"

object SurfaceFlatMapInteraction:
  /** Derives the selected vertex that belongs to this map. A selection on
    * another surface is intentionally absent rather than reinterpreted. */
  def selectedVertex(map: SurfaceFlatMap, model: SurfaceViewerModel,
      state: SurfaceViewerState): Either[SurfaceFlatMapInteractionError, Option[VertexId]] =
    val surface = model.surface(map.surface).toRight(
      SurfaceFlatMapInteractionError.InvalidState(s"flat-map surface '${map.surface.value}' is absent from the viewer model")
    )
    surface.flatMap: asset =>
      if asset.geometry.vertexCount != map.vertexCount then
        Left(SurfaceFlatMapInteractionError.InvalidState(
          s"flat-map vertex count ${map.vertexCount} differs from viewer surface count ${asset.geometry.vertexCount}"
        ))
      else if state.timepoint < 0 || state.timepoint >= model.frameCount then
        Left(SurfaceFlatMapInteractionError.InvalidState(
          s"timepoint ${state.timepoint} is outside viewer frame domain 0..${model.frameCount - 1}"
        ))
      else selectedVertexInMap(map, state)

  private def selectedVertexInMap(map: SurfaceFlatMap,
      state: SurfaceViewerState): Either[SurfaceFlatMapInteractionError, Option[VertexId]] =
    state.selection match
      case None => Right(None)
      case Some(selection) if selection.surface != map.surface => Right(None)
      case Some(selection) if selection.vertex.index < 0 || selection.vertex.index >= map.vertexCount =>
        Left(SurfaceFlatMapInteractionError.InvalidState(
          s"selection vertex ${selection.vertex.index} is outside flat-map domain 0..${map.vertexCount - 1}"
        ))
      case Some(selection) => Right(Some(selection.vertex))
