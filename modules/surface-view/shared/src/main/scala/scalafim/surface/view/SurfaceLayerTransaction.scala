package scalafim.surface.view

/** A fully validated and compiled compatible layer replacement. Preparation is
  * renderer-neutral and may run away from a UI thread. Apply must still verify
  * that presentation state has not changed while the work was pending.
  */
final class SurfaceLayerTransaction private[view] (
  val sourceModel: SurfaceViewerModel,
  val nextModel: SurfaceViewerModel,
  val preparedState: SurfaceViewerState,
  val preparedPlan: SurfaceRenderPlan,
  val layerIds: Vector[SurfaceLayerId]
):
  private[scalafim] def rebase(
    currentModel: SurfaceViewerModel,
    currentState: SurfaceViewerState,
    currentPlan: SurfaceRenderPlan
  ): Either[SurfaceViewError, SurfaceRenderPlan] =
    if !(currentModel eq sourceModel) then
      Left(SurfaceViewError.InvalidLayerTransaction("source model is no longer current"))
    else if !samePreparationContext(preparedState, currentState) then
      Left(SurfaceViewError.InvalidLayerTransaction("layer presentation changed while preparation was pending; prepare again"))
    else
      SurfaceCompiler.compileReadout(nextModel, currentState).map: readouts =>
        val chrome = SurfaceCompiler.compileChrome(readouts)
        preparedPlan.copy(
          slots = currentPlan.slots,
          meshes = currentPlan.meshes,
          camera = currentPlan.camera,
          lighting = currentPlan.lighting,
          clipping = currentPlan.clipping,
          chrome = chrome,
          readouts = readouts,
          receipt = preparedPlan.receipt.copy(
            meshKeys = currentPlan.receipt.meshKeys,
            cameraKey = currentPlan.receipt.cameraKey
          ),
          viewportFit = currentPlan.viewportFit,
          surfaceCameras = currentPlan.surfaceCameras
        )

  private def samePreparationContext(before: SurfaceViewerState, after: SurfaceViewerState): Boolean =
    before.layout == after.layout &&
      before.timepoint == after.timepoint &&
      before.geometryPresentations == after.geometryPresentations &&
      before.layerOrder == after.layerOrder &&
      before.presentations == after.presentations

object SurfaceLayerTransaction:
  def prepare(
    model: SurfaceViewerModel,
    state: SurfaceViewerState,
    replacements: Vector[SurfaceLayer]
  ): Either[SurfaceViewError, SurfaceLayerTransaction] =
    for
      next <- model.replaceCompatibleLayers(replacements)
      plan <- SurfaceCompiler.compile(next, state)
    yield new SurfaceLayerTransaction(model, next, state, plan, replacements.map(_.id))
