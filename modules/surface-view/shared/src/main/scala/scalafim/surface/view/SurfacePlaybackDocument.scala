package scalafim.surface.view

import intaglio.*

/** The temporal source binding remains separate from the sampled display layer.
  * Persist this alongside a scene/composition document. Restore the sampled
  * layer first, then restore that document against the rebuilt viewer model.
  */
final class SurfacePlaybackDocument private (
  val field: SurfaceTemporalFieldId,
  val layer: SurfaceLayerId,
  val surface: SurfaceId,
  val reference: SurfaceExternalReference,
  val axis: SurfaceTimeAxis,
  val vertexCount: Int,
  val mappingKey: String,
  val interpolation: SurfaceInterpolation,
  val state: SurfacePlaybackState
):
  def prepare(source: SurfaceTemporalField, suppliedReference: SurfaceExternalReference,
      mapping: ScalarMapping, cache: SurfaceFrameCache): Either[SurfaceSceneError, PreparedSurfaceLayer] =
    for
      _ <- SurfaceCompositionDocument.check(source.id == field && source.axis == axis && source.geometry.vertexCount == vertexCount,
        "temporal source identity, axis, or vertex domain changed")
      _ <- SurfaceCompositionDocument.check(suppliedReference == reference, "temporal source reference changed")
      _ <- SurfaceCompositionDocument.check(mapping.canonicalKey == mappingKey, "temporal mapping changed")
      prepared <- SurfacePlayback.prepareScalar(source, layer, surface, state.playhead, interpolation,
        mapping.colorizer, cache.without(field)).left.map(SurfaceSceneError.InvalidState.apply)
    yield prepared

object SurfacePlaybackDocument:
  def capture(source: SurfaceTemporalField, layer: SurfaceLayerId, surface: SurfaceId,
      reference: SurfaceExternalReference, mapping: ScalarMapping, interpolation: SurfaceInterpolation,
      state: SurfacePlaybackState): Either[SurfaceSceneError, SurfacePlaybackDocument] =
    source.axis.sample(state.playhead, interpolation).left.map(SurfaceSceneError.InvalidState.apply).map: _ =>
      new SurfacePlaybackDocument(source.id, layer, surface, reference, source.axis, source.geometry.vertexCount,
        mapping.canonicalKey, interpolation, state)

  def encode(document: SurfacePlaybackDocument): String =
    def number(value: Double): SceneJson = SceneJson.Num(java.lang.Double.toString(value))
    SceneJson.obj(
      "schema" -> SceneJson.Str("scalafim.surface-playback"), "revision" -> SceneJson.Num("1"),
      "field" -> SceneJson.Str(document.field.value), "layer" -> SceneJson.Str(document.layer.value),
      "surface" -> SceneJson.Str(document.surface.value), "uri" -> SceneJson.Str(document.reference.uri.value),
      "sha256" -> SceneJson.Str(document.reference.sha256.value),
      "axis" -> SceneJson.Arr(document.axis.times.map(time => number(time.value))),
      "vertexCount" -> SceneJson.Num(document.vertexCount.toString), "mappingKey" -> SceneJson.Str(document.mappingKey),
      "interpolation" -> SceneJson.Str(document.interpolation.toString), "playhead" -> number(document.state.playhead.value),
      "rate" -> number(document.state.rate.value), "status" -> SceneJson.Str(document.state.status.toString),
      "looping" -> SceneJson.Bool(document.state.looping)).render

  def decode(input: String, policy: SurfaceSceneReadPolicy = SurfaceSceneReadPolicy())
      : Either[SurfaceSceneError, SurfacePlaybackDocument] =
    import SurfaceCompositionDocument.{check, traverse}
    def invalid(error: SurfaceViewError): SurfaceSceneError = SurfaceSceneError.InvalidState(error)
    for
      value <- SceneJsonParser.parse(input)
      obj <- SceneJsonRead.obj(value, "$", Set("schema", "revision", "field", "layer", "surface", "uri", "sha256",
        "axis", "vertexCount", "mappingKey", "interpolation", "playhead", "rate", "status", "looping"), policy)
      schema <- SceneJsonRead.string(obj, "schema", "$.schema")
      _ <- check(schema == "scalafim.surface-playback", "unknown playback schema")
      revision <- SceneJsonRead.int(obj, "revision", "$.revision")
      _ <- if revision == 1 then Right(()) else Left(SurfaceSceneError.UnsupportedRevision(revision))
      fieldRaw <- SceneJsonRead.string(obj, "field", "$.field")
      field <- SurfaceTemporalFieldId.make(fieldRaw).left.map(invalid)
      layerRaw <- SceneJsonRead.string(obj, "layer", "$.layer")
      layer <- SurfaceLayerId.make(layerRaw).left.map(invalid)
      surfaceRaw <- SceneJsonRead.string(obj, "surface", "$.surface")
      surface <- SurfaceId.make(surfaceRaw).left.map(invalid)
      uriRaw <- SceneJsonRead.string(obj, "uri", "$.uri")
      uri <- SurfaceAssetUri.make(uriRaw)
      digestRaw <- SceneJsonRead.string(obj, "sha256", "$.sha256")
      digest <- SurfaceContentDigest.make(digestRaw)
      timesJson <- SceneJsonRead.array(obj, "axis", "$.axis")
      times <- traverse(timesJson)(json => SceneJsonRead.doubleValue(json, "$.axis[]")
        .flatMap(value => SurfaceSeconds.make(value).left.map(invalid)))
      axis <- SurfaceTimeAxis.make(times).left.map(invalid)
      count <- SceneJsonRead.int(obj, "vertexCount", "$.vertexCount")
      _ <- check(count > 0, "temporal vertex domain must be nonempty")
      mapping <- SceneJsonRead.string(obj, "mappingKey", "$.mappingKey")
      _ <- check(mapping.nonEmpty, "temporal mapping key must not be blank")
      interpolationRaw <- SceneJsonRead.string(obj, "interpolation", "$.interpolation")
      interpolation <- SurfaceInterpolation.values.find(_.toString == interpolationRaw)
        .toRight(SurfaceSceneError.InvalidDocument("unknown temporal interpolation"))
      playheadRaw <- SceneJsonRead.double(obj, "playhead", "$.playhead")
      playhead <- SurfaceSeconds.make(playheadRaw).left.map(invalid)
      _ <- axis.sample(playhead, interpolation).left.map(invalid)
      rateRaw <- SceneJsonRead.double(obj, "rate", "$.rate")
      rate <- SurfacePlaybackRate.make(rateRaw).left.map(invalid)
      statusRaw <- SceneJsonRead.string(obj, "status", "$.status")
      status <- SurfacePlaybackStatus.values.find(_.toString == statusRaw)
        .toRight(SurfaceSceneError.InvalidDocument("unknown playback status"))
      looping <- SceneJsonRead.bool(obj, "looping", "$.looping")
    yield new SurfacePlaybackDocument(field, layer, surface, SurfaceExternalReference(uri, digest), axis, count,
      mapping, interpolation, SurfacePlaybackState(playhead, status, rate, looping))
