package scalafim.surface.view

import image4s.geometry.{D3, Frame}
import scalafim.surface.SurfaceKind

/** A persistent frame key is checked against caller-supplied display witnesses;
  * restoration never guesses a coordinate system or manufactures a witness.
  */
final case class SurfaceCompositionFrameKey(id: String, rank: Int, unit: String, convention: String)
object SurfaceCompositionFrameKey:
  def from(frame: Frame[D3]): Either[SurfaceSceneError, SurfaceCompositionFrameKey] =
    frame.persistentKey.toRight(SurfaceSceneError.InvalidDocument("composition persistence requires a persistent frame"))
      .map(key => SurfaceCompositionFrameKey(key.id.value, key.spatialRank, key.unit.toString, key.convention.toString))

final case class SurfaceCompositionSavedInstance(id: SurfaceInstanceId, surface: SurfaceId, translation: Vector[Double])
final case class SurfaceCompositionSavedView(id: SurfaceViewId, viewport: SurfaceViewport, camera: SurfaceCamera,
  frame: SurfaceCompositionFrameKey, target: Vector[Double], eye: Vector[Double],
  instances: Vector[SurfaceCompositionSavedInstance], lighting: SurfaceLighting, clipping: SurfaceClipping)
final case class SurfaceCompositionRestored[F <: Frame[D3]](
  presentation: SurfaceCompositionPresentation, navigation: SurfaceCompositionState[F])

/** Wraps the existing asset-checked scene document without changing V1–V7. */
final class SurfaceCompositionDocument private[view] (
  val scene: SurfaceSceneDocument,
  val views: Vector[SurfaceCompositionSavedView],
  val cameraLinks: Vector[Set[SurfaceViewId]],
  val geometry: Map[SurfaceId, SurfaceGeometryPresentation]
):
  def restore[F <: Frame[D3]](model: SurfaceViewerModel, bindings: SurfaceSceneBindings,
      displays: Map[SurfaceId, SurfaceDisplayFrame[F]]): Either[SurfaceSceneError, SurfaceCompositionRestored[F]] =
    import SurfaceCompositionDocument.*
    for
      state <- scene.restore(model, bindings)
      inputs <- traverse(views): saved =>
        for
          occurrences <- traverse(saved.instances): item =>
            for
              display <- displays.get(item.surface).toRight(SurfaceSceneError.MissingSurfaceBinding(item.surface))
              frame <- SurfaceCompositionFrameKey.from(display.frame)
              _ <- check(frame == saved.frame, s"view ${saved.id.value} has a different persistent frame")
            yield SurfaceCompositionInputInstance(item.id, item.surface, display,
              item.translation(0), item.translation(1), item.translation(2))
          _ <- check(occurrences.nonEmpty, "a composition view must contain an instance")
          frame = occurrences.head.display.frame
          target <- SurfaceCameraPose.pointIn(frame, saved.target).left.map(cameraError)
          eye <- SurfaceCameraPose.pointIn(frame, saved.eye).left.map(cameraError)
          pose <- SurfaceCameraPose.make(target, eye).left.map(cameraError)
        yield SurfaceCompositionInputView(saved.id, saved.viewport, saved.camera, pose, occurrences, saved.lighting, saved.clipping)
      navigation <- SurfaceCompositionState.make(inputs, cameraLinks).left.map(compositionError)
      presentation = SurfaceCompositionPresentation.fromViewerState(state).copy(geometryPresentations = geometry)
      _ <- SurfaceCompositionCompiler.compile(model, presentation, inputs).left.map(compositionError)
    yield SurfaceCompositionRestored(presentation, navigation)

object SurfaceCompositionDocument:
  def capture[F <: Frame[D3]](model: SurfaceViewerModel, state: SurfaceViewerState,
      navigation: SurfaceCompositionState[F], bindings: SurfaceSceneBindings, provenance: SurfaceProvenance)
      : Either[SurfaceSceneError, SurfaceCompositionDocument] =
    for
      _ <- SurfaceCompositionCompiler.compile(model, SurfaceCompositionPresentation.fromViewerState(state), navigation.views).left.map(compositionError)
      _ <- check(!state.geometryPresentations.values.exists(_.isInstanceOf[SurfaceGeometryPresentation.RevealLens]),
        "persist a fixed or global morph geometry; local reveal-lens deformation persistence is not supported")
      scene <- SurfaceSceneDocument.capture(model, state, bindings, provenance)
      views <- traverse(navigation.views): view =>
        SurfaceCompositionFrameKey.from(view.pose.frame).map: frame =>
          SurfaceCompositionSavedView(view.id, view.viewport, view.camera, frame, view.pose.target.coordinates, view.pose.eye.coordinates,
            view.instances.map(i => SurfaceCompositionSavedInstance(i.id, i.surface, Vector(i.translationX, i.translationY, i.translationZ))),
            view.lighting, view.clipping)
    yield new SurfaceCompositionDocument(scene, views, navigation.cameraLinks, state.geometryPresentations)

  private[view] def check(condition: Boolean, reason: String): Either[SurfaceSceneError, Unit] =
    if condition then Right(()) else Left(SurfaceSceneError.InvalidDocument(reason))
  private[view] def compositionError(error: SurfaceCompositionError): SurfaceSceneError = SurfaceSceneError.InvalidDocument(s"$error")
  private[view] def cameraError(error: SurfaceCameraError): SurfaceSceneError = SurfaceSceneError.InvalidDocument(error.message)
  private[view] def traverse[A, B](values: Vector[A])(f: A => Either[SurfaceSceneError, B]): Either[SurfaceSceneError, Vector[B]] =
    val result = Vector.newBuilder[B]
    var index = 0
    while index < values.length do
      f(values(index)) match
        case Left(error) => return Left(error)
        case Right(value) => result += value
      index += 1
    Right(result.result())

object SurfaceCompositionCodec:
  import SurfaceCompositionDocument.*
  private def num(value: Double): SceneJson = SceneJson.Num(java.lang.Double.toString(value))
  private def vec(values: Vector[Double]): SceneJson = SceneJson.Arr(values.map(num))
  def encode(document: SurfaceCompositionDocument): String =
    val views = document.views.map: v =>
      SceneJson.obj("id" -> SceneJson.Str(v.id.value),
        "viewport" -> vec(Vector(v.viewport.x, v.viewport.y, v.viewport.width, v.viewport.height)),
        "camera" -> SurfaceSceneCodec.cameraJson(v.camera, CameraFraming.BoundsFit, SurfaceDocumentRevision.V7),
        "frame" -> SceneJson.obj("id" -> SceneJson.Str(v.frame.id), "rank" -> SceneJson.Num(v.frame.rank.toString),
          "unit" -> SceneJson.Str(v.frame.unit), "convention" -> SceneJson.Str(v.frame.convention)),
        "target" -> vec(v.target), "eye" -> vec(v.eye),
        "instances" -> SceneJson.Arr(v.instances.map(i => SceneJson.obj("id" -> SceneJson.Str(i.id.value),
          "surface" -> SceneJson.Str(i.surface.value), "translation" -> vec(i.translation)))),
        "lighting" -> SurfaceSceneCodec.lightingJson(v.lighting), "clipping" -> SurfaceSceneCodec.clippingJson(v.clipping))
    val geometry = document.geometry.toVector.sortBy(_._1.value).map: (surface, value) =>
      val (from, to, fraction) = value match
        case SurfaceGeometryPresentation.Fixed(kind) => (kind, kind, 0.0)
        case SurfaceGeometryPresentation.Morphing(a, b, t) => (a, b, t.value)
        case _: SurfaceGeometryPresentation.RevealLens => throw new IllegalStateException("unpersistable lens")
      SceneJson.obj("surface" -> SceneJson.Str(surface.value), "from" -> SceneJson.Str(from.label),
        "to" -> SceneJson.Str(to.label), "fraction" -> num(fraction))
    SceneJson.obj("schema" -> SceneJson.Str("scalafim.surface-composition"), "revision" -> SceneJson.Num("1"),
      "scene" -> SceneJsonParser.parse(SurfaceSceneCodec.encode(document.scene)).toOption.get,
      "views" -> SceneJson.Arr(views), "geometry" -> SceneJson.Arr(geometry),
      "cameraLinks" -> SceneJson.Arr(document.cameraLinks.map(link => SceneJson.Arr(link.toVector.sortBy(_.value).map(id => SceneJson.Str(id.value)))))).render

  def decode(input: String, policy: SurfaceSceneReadPolicy = SurfaceSceneReadPolicy()): Either[SurfaceSceneError, SurfaceCompositionDocument] =
    for
      json <- SceneJsonParser.parse(input)
      root <- SceneJsonRead.obj(json, "$", Set("schema", "revision", "scene", "views", "cameraLinks", "geometry"), policy)
      schema <- SceneJsonRead.string(root, "schema", "$.schema")
      _ <- check(schema == "scalafim.surface-composition", "unknown composition schema")
      revision <- SceneJsonRead.int(root, "revision", "$.revision")
      _ <- if revision == 1 then Right(()) else Left(SurfaceSceneError.UnsupportedRevision(revision))
      sceneJson <- SceneJsonRead.value(root, "scene", "$.scene")
      scene <- SurfaceSceneCodec.decode(sceneJson.render, policy)
      viewJson <- SceneJsonRead.array(root, "views", "$.views")
      views <- traverse(viewJson)(parseView(_, policy))
      linksJson <- SceneJsonRead.array(root, "cameraLinks", "$.cameraLinks")
      links <- traverse(linksJson): value =>
        value match
          case SceneJson.Arr(items) => traverse(items)(j => SceneJsonRead.stringValue(j, "$.cameraLinks[]")
            .flatMap(id => SurfaceViewId.make(id).left.map(compositionError))).flatMap(ids =>
              check(ids.distinct.length == ids.length, "duplicate linked view").map(_ => ids.toSet))
          case _ => Left(SurfaceSceneError.InvalidJson("$.cameraLinks", "expected arrays of view ids"))
      geometryJson <- SceneJsonRead.array(root, "geometry", "$.geometry")
      geometry <- traverse(geometryJson)(parseGeometry(_, policy))
      _ <- check(views.nonEmpty && views.map(_.id).distinct.length == views.length, "views must be nonempty with unique ids")
      _ <- check(geometry.map(_._1).distinct.length == geometry.length, "duplicate geometry presentation")
      _ <- check(views.flatMap(_.instances.map(_.id)).distinct.length == views.map(_.instances.length).sum, "duplicate instance id")
      _ <- check(links.forall(group => group.size >= 2 && group.forall(id => views.exists(_.id == id))) &&
        links.flatten.distinct.length == links.map(_.size).sum, "invalid or overlapping camera links")
    yield new SurfaceCompositionDocument(scene, views, links, geometry.toMap)

  private def vector(obj: SceneJson.Obj, key: String, length: Int): Either[SurfaceSceneError, Vector[Double]] =
    for
      json <- SceneJsonRead.array(obj, key, s"$$.$key")
      _ <- check(json.length == length, s"$key must have $length coordinates")
      values <- traverse(json)(SceneJsonRead.doubleValue(_, s"$$.$key[]"))
    yield values

  private def parseView(value: SceneJson, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, SurfaceCompositionSavedView] =
    for
      obj <- SceneJsonRead.obj(value, "$.views[]", Set("id", "viewport", "camera", "frame", "target", "eye", "instances", "lighting", "clipping"), policy)
      idString <- SceneJsonRead.string(obj, "id", "$.views[].id")
      id <- SurfaceViewId.make(idString).left.map(compositionError)
      box <- vector(obj, "viewport", 4)
      _ <- check(box(0) >= 0 && box(1) >= 0 && box(2) > 0 && box(3) > 0 && box(0) + box(2) <= 1 && box(1) + box(3) <= 1, "invalid viewport")
      cameraJson <- SceneJsonRead.value(obj, "camera", "$.views[].camera")
      camera <- SurfaceSceneCodec.parseCamera(cameraJson, policy, SurfaceDocumentRevision.V7)
      frameJson <- SceneJsonRead.value(obj, "frame", "$.views[].frame")
      frameObj <- SceneJsonRead.obj(frameJson, "$.views[].frame", Set("id", "rank", "unit", "convention"), policy)
      frameId <- SceneJsonRead.string(frameObj, "id", "$.frame.id")
      rank <- SceneJsonRead.int(frameObj, "rank", "$.frame.rank")
      unit <- SceneJsonRead.string(frameObj, "unit", "$.frame.unit")
      convention <- SceneJsonRead.string(frameObj, "convention", "$.frame.convention")
      _ <- check(frameId.nonEmpty && rank == 3 && unit.nonEmpty && convention.nonEmpty, "invalid persistent frame key")
      target <- vector(obj, "target", 3)
      eye <- vector(obj, "eye", 3)
      _ <- check(target != eye, "camera target and eye must differ")
      instancesJson <- SceneJsonRead.array(obj, "instances", "$.views[].instances")
      instances <- traverse(instancesJson)(parseInstance(_, policy))
      _ <- check(instances.nonEmpty, "empty composition view")
      lightingJson <- SceneJsonRead.value(obj, "lighting", "$.views[].lighting")
      lighting <- SurfaceSceneCodec.parseLighting(lightingJson, policy)
      clippingJson <- SceneJsonRead.value(obj, "clipping", "$.views[].clipping")
      clipping <- SurfaceSceneCodec.parseClipping(clippingJson, policy)
    yield SurfaceCompositionSavedView(id, SurfaceViewport(box(0), box(1), box(2), box(3)), camera,
      SurfaceCompositionFrameKey(frameId, rank, unit, convention), target, eye, instances, lighting, clipping)

  private def parseInstance(value: SceneJson, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, SurfaceCompositionSavedInstance] =
    for
      obj <- SceneJsonRead.obj(value, "$.instances[]", Set("id", "surface", "translation"), policy)
      raw <- SceneJsonRead.string(obj, "id", "$.instances[].id")
      id <- SurfaceInstanceId.make(raw).left.map(compositionError)
      surfaceRaw <- SceneJsonRead.string(obj, "surface", "$.instances[].surface")
      surface <- SurfaceId.make(surfaceRaw).left.map(SurfaceSceneError.InvalidState.apply)
      translation <- vector(obj, "translation", 3)
    yield SurfaceCompositionSavedInstance(id, surface, translation)

  private def parseGeometry(value: SceneJson, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, (SurfaceId, SurfaceGeometryPresentation)] =
    for
      obj <- SceneJsonRead.obj(value, "$.geometry[]", Set("surface", "from", "to", "fraction"), policy)
      surfaceRaw <- SceneJsonRead.string(obj, "surface", "$.geometry[].surface")
      surface <- SurfaceId.make(surfaceRaw).left.map(SurfaceSceneError.InvalidState.apply)
      from <- SceneJsonRead.string(obj, "from", "$.geometry[].from")
      to <- SceneJsonRead.string(obj, "to", "$.geometry[].to")
      _ <- check(from.trim.nonEmpty && to.trim.nonEmpty, "empty geometry kind")
      fraction <- SceneJsonRead.double(obj, "fraction", "$.geometry[].fraction")
      checked <- SurfaceMorphFraction.make(fraction).left.map(SurfaceSceneError.InvalidState.apply)
    yield surface -> (if from == to && fraction == 0 then SurfaceGeometryPresentation.Fixed(SurfaceKind.fromString(from))
      else SurfaceGeometryPresentation.Morphing(SurfaceKind.fromString(from), SurfaceKind.fromString(to), checked))
