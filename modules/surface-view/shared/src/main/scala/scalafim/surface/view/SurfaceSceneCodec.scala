package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

object SurfaceSceneCodec:
  def encode(document: SurfaceSceneDocument): String =
    documentJson(document).render

  def decode(
    input: String,
    policy: SurfaceSceneReadPolicy = SurfaceSceneReadPolicy()
  ): Either[SurfaceSceneError, SurfaceSceneDocument] =
    for
      value <- SceneJsonParser.parse(input)
      root <- SceneJsonRead.obj(value, "$", RootFields, policy)
      schema <- SceneJsonRead.string(root, "schema", "$.schema")
      _ <-
        if schema == "scalafim.surface-scene" then Right(())
        else Left(SurfaceSceneError.InvalidJson("$.schema", s"expected 'scalafim.surface-scene'; got '$schema'"))
      revisionValue <- SceneJsonRead.int(root, "revision", "$.revision")
      revision <-
        if revisionValue == 1 then Right(SurfaceDocumentRevision.V1)
        else if revisionValue == 2 then Right(SurfaceDocumentRevision.V2)
        else if revisionValue == 3 then Right(SurfaceDocumentRevision.V3)
        else if revisionValue == 4 then Right(SurfaceDocumentRevision.V4)
        else if revisionValue == 5 then Right(SurfaceDocumentRevision.V5)
        else if revisionValue == 6 then Right(SurfaceDocumentRevision.V6)
        else Left(SurfaceSceneError.UnsupportedRevision(revisionValue))
      assetsJson <- SceneJsonRead.array(root, "assets", "$.assets")
      assets <- traverseIndexed(assetsJson)(parseAsset(_, _, policy))
      layersJson <- SceneJsonRead.array(root, "layers", "$.layers")
      layers <- traverseIndexed(layersJson)(parseLayer(_, _, policy, revision))
      layoutJson <- SceneJsonRead.value(root, "layout", "$.layout")
      layout <- parseLayout(layoutJson, policy)
      cameraJson <- SceneJsonRead.value(root, "camera", "$.camera")
      camera <- parseCamera(cameraJson, policy)
      lightingJson <- SceneJsonRead.value(root, "lighting", "$.lighting")
      lighting <- parseLighting(lightingJson, policy)
      clippingJson <- SceneJsonRead.value(root, "clipping", "$.clipping")
      clipping <- parseClipping(clippingJson, policy)
      timepoint <- SceneJsonRead.int(root, "timepoint", "$.timepoint")
      selectionJson <- SceneJsonRead.value(root, "selection", "$.selection")
      selection <- parseSelection(selectionJson, policy, revision)
      statesJson <- SceneJsonRead.array(root, "layerStates", "$.layerStates")
      states <- traverseIndexed(statesJson)(parseLayerState(_, _, policy))
      requirementsJson <- SceneJsonRead.array(root, "requiredFeatures", "$.requiredFeatures")
      requirements <- traverseIndexed(requirementsJson): (value, index) =>
        SceneJsonRead.stringValue(value, s"$$.requiredFeatures[$index]").flatMap(parseFeature)
      provenanceJson <- SceneJsonRead.value(root, "provenance", "$.provenance")
      provenance <- parseProvenance(provenanceJson, policy)
      legends <- if revision.value >= 6 then
        SceneJsonRead.array(root, "legends", "$.legends").flatMap(values =>
          traverseIndexed(values)((value, index) => SurfaceSceneLegendCodec.decode(value, s"$$.legends[$index]", policy)))
        else if root.fields.exists(_._1 == "legends") then
          Left(SurfaceSceneError.InvalidJson("$.legends", "legends require revision 6"))
        else Right(Vector.empty)
      document <- SurfaceSceneDocument.make(
        revision,
        assets,
        layers,
        layout,
        camera,
        lighting,
        clipping,
        timepoint,
        selection,
        states,
        requirements.toSet,
        provenance,
        legends
      )
    yield document

  private val RootFields = Set(
    "schema", "revision", "assets", "layers", "layout", "camera", "lighting",
    "clipping", "timepoint", "selection", "layerStates", "requiredFeatures", "provenance", "legends"
  )

  private def documentJson(document: SurfaceSceneDocument): SceneJson =
    val fields = Vector(
      "schema" -> SceneJson.Str("scalafim.surface-scene"),
      "revision" -> SceneJson.Num(document.revision.value.toString),
      "assets" -> SceneJson.Arr(document.assets.map(assetJson)),
      "layers" -> SceneJson.Arr(document.layers.map(layerJson(_, document.revision))),
      "layout" -> layoutJson(document.layout),
      "camera" -> cameraJson(document.camera),
      "lighting" -> lightingJson(document.lighting),
      "clipping" -> clippingJson(document.clipping),
      "timepoint" -> SceneJson.Num(document.timepoint.toString),
      "selection" -> document.selection.fold[SceneJson](SceneJson.Null)(selectionJson(_, document.revision)),
      "layerStates" -> SceneJson.Arr(document.layerStates.map(layerStateJson)),
      "requiredFeatures" -> SceneJson.Arr(
        document.requiredFeatures.toVector.sortBy(SurfaceSceneNames.feature).map(feature => SceneJson.Str(SurfaceSceneNames.feature(feature)))
      ),
      "provenance" -> provenanceJson(document.provenance)
    )
    SceneJson.Obj(fields ++ Option.when(document.revision.value >= 6)(
      "legends" -> SceneJson.Arr(document.legends.map(SurfaceSceneLegendCodec.encode))))

  private def assetJson(asset: SurfaceSceneAsset): SceneJson =
    SceneJson.obj(
      "id" -> SceneJson.Str(asset.id.value),
      "uri" -> SceneJson.Str(asset.reference.uri.value),
      "sha256" -> SceneJson.Str(asset.reference.sha256.value),
      "hemisphere" -> SceneJson.Str(hemisphereName(asset.hemisphere)),
      "kind" -> SceneJson.Str(asset.kind.label),
      "vertexCount" -> SceneJson.Num(asset.vertexCount.toString),
      "faceCount" -> SceneJson.Num(asset.faceCount.toString),
      "topologyIdentity" -> SceneJson.Str(asset.topologyIdentity)
    )

  private def layerJson(layer: SurfaceSceneLayer, revision: SurfaceDocumentRevision): SceneJson =
    val fields = Vector(
      "id" -> SceneJson.Str(layer.id.value),
      "surface" -> SceneJson.Str(layer.surface.value),
      "uri" -> SceneJson.Str(layer.reference.uri.value),
      "sha256" -> SceneJson.Str(layer.reference.sha256.value),
      "encoding" -> SceneJson.Str(layerKindName(layer.encoding)),
      "frameCount" -> SceneJson.Num(layer.frameCount.toString),
      "blendMode" -> SceneJson.Str(blendName(layer.blendMode))
    )
    val association = layer.association match
      case SurfaceSampleAssociation.Vertex => "vertex"
      case SurfaceSampleAssociation.Face => "face"
    SceneJson.Obj(fields ++ Option.when(revision != SurfaceDocumentRevision.V1)(
      "association" -> SceneJson.Str(association)) ++ Option.when(revision.value >= 3)(
      "vertexInterpolation" -> SceneJson.Str(layer.vertexInterpolation match
        case SurfaceVertexInterpolation.Color => "color"
        case SurfaceVertexInterpolation.NearestSample => "nearest-sample")) ++ Option.when(revision.value >= 4)(
      "scalarMappingKey" -> layer.scalarMappingKey.fold[SceneJson](SceneJson.Null)(SceneJson.Str.apply)) ++ Option.when(revision.value >= 5)(
      "scalarInterpolation" -> SceneJson.Bool(layer.scalarInterpolation)))

  private def layoutJson(layout: SurfaceLayout): SceneJson =
    layout match
      case SurfaceLayout.Single(surface) => SceneJson.obj(
        "kind" -> SceneJson.Str("single"),
        "surface" -> SceneJson.Str(surface.value)
      )
      case SurfaceLayout.Bilateral(left, right, order) => SceneJson.obj(
        "kind" -> SceneJson.Str("bilateral"),
        "left" -> SceneJson.Str(left.value),
        "right" -> SceneJson.Str(right.value),
        "order" -> SceneJson.Str(orderName(order))
      )

  private def cameraJson(camera: SurfaceCamera): SceneJson =
    val projection = camera.projection match
      case CameraProjection.Perspective(fieldOfView) => SceneJson.obj(
        "kind" -> SceneJson.Str("perspective"),
        "value" -> number(fieldOfView.value)
      )
      case CameraProjection.Orthographic(scale) => SceneJson.obj(
        "kind" -> SceneJson.Str("orthographic"),
        "value" -> number(scale.value)
      )
    SceneJson.obj(
      "viewpoint" -> SceneJson.Str(viewpointName(camera.viewpoint)),
      "projection" -> projection,
      "zoom" -> number(camera.zoom.value),
      "pan" -> SceneJson.Arr(Vector(number(camera.panX), number(camera.panY))),
      "orbit" -> SceneJson.Arr(Vector(number(camera.orbit.yawDegrees), number(camera.orbit.pitchDegrees)))
    )

  private def lightingJson(lighting: SurfaceLighting): SceneJson =
    lighting match
      case SurfaceLighting.Unlit => SceneJson.obj("kind" -> SceneJson.Str("unlit"))
      case SurfaceLighting.Directional(ambient, diffuse, x, y, z) => SceneJson.obj(
        "kind" -> SceneJson.Str("directional"),
        "ambient" -> number(ambient.value),
        "diffuse" -> number(diffuse.value),
        "direction" -> SceneJson.Arr(Vector(number(x), number(y), number(z)))
      )

  private def clippingJson(clipping: SurfaceClipping): SceneJson =
    clipping match
      case SurfaceClipping.Disabled => SceneJson.obj("kind" -> SceneJson.Str("disabled"))
      case SurfaceClipping.NearFar(near, far) => SceneJson.obj(
        "kind" -> SceneJson.Str("near-far"),
        "near" -> number(near),
        "far" -> number(far)
      )
      case SurfaceClipping.WorldPlanes(planes) =>
        val encoded = planes.map: plane =>
          SceneJson.obj(
          "normal" -> SceneJson.Arr(Vector(number(plane.normalX), number(plane.normalY), number(plane.normalZ))),
          "offset" -> number(plane.offset),
          "keep" -> SceneJson.Str(if plane.keep == ClipKeepSide.Positive then "positive" else "negative")
          )
        SceneJson.obj(
          "kind" -> SceneJson.Str("world-planes"),
          "planes" -> SceneJson.Arr(encoded)
        )

  private def selectionJson(selection: SurfaceSelection, revision: SurfaceDocumentRevision): SceneJson =
    val fields = Vector(
      "surface" -> SceneJson.Str(selection.surface.value),
      "vertex" -> SceneJson.Num(selection.vertex.index.toString)
    )
    SceneJson.Obj(fields ++ Option.when(revision != SurfaceDocumentRevision.V1)(
      "face" -> selection.face.fold[SceneJson](SceneJson.Null)(face => SceneJson.Num(face.index.toString))))

  private def layerStateJson(state: SurfaceSceneLayerState): SceneJson =
    SceneJson.obj(
      "id" -> SceneJson.Str(state.id.value),
      "visible" -> SceneJson.Bool(state.visible),
      "opacity" -> number(state.opacity.toDouble),
      "window" -> state.window.fold[SceneJson](SceneJson.Null): window =>
        SceneJson.Arr(Vector(number(window.lower), number(window.upper))),
      "threshold" -> state.threshold.fold[SceneJson](SceneJson.Null):
        case DisplayThreshold.Disabled => SceneJson.obj("kind" -> SceneJson.Str("disabled"))
        case DisplayThreshold.TransparentBand(band) => SceneJson.obj(
          "kind" -> SceneJson.Str("transparent-band"),
          "lower" -> number(band.lower),
          "upper" -> number(band.upper)
        )
    )

  private def provenanceJson(provenance: SurfaceProvenance): SceneJson =
    SceneJson.obj(
      "producer" -> SceneJson.Str(provenance.producer),
      "softwareVersion" -> SceneJson.Str(provenance.softwareVersion),
      "createdAt" -> SceneJson.Str(provenance.createdAt),
      "entries" -> SceneJson.Obj(provenance.entries.map((key, value) => key -> SceneJson.Str(value)))
    )

  private def parseAsset(
    value: SceneJson,
    index: Int,
    policy: SurfaceSceneReadPolicy
  ): Either[SurfaceSceneError, SurfaceSceneAsset] =
    val path = s"$$.assets[$index]"
    for
      obj <- SceneJsonRead.obj(value, path, Set("id", "uri", "sha256", "hemisphere", "kind", "vertexCount", "faceCount", "topologyIdentity"), policy)
      idRaw <- SceneJsonRead.string(obj, "id", s"$path.id")
      id <- SurfaceId.make(idRaw).left.map(SurfaceSceneError.InvalidState.apply)
      reference <- parseReference(obj, path)
      hemisphereRaw <- SceneJsonRead.string(obj, "hemisphere", s"$path.hemisphere")
      hemisphere <- parseHemisphere(hemisphereRaw, s"$path.hemisphere")
      kindRaw <- SceneJsonRead.string(obj, "kind", s"$path.kind")
      kind <-
        if kindRaw.trim.nonEmpty then Right(SurfaceKind.fromString(kindRaw))
        else Left(SurfaceSceneError.InvalidJson(s"$path.kind", "must be non-empty"))
      vertices <- SceneJsonRead.int(obj, "vertexCount", s"$path.vertexCount")
      faces <- SceneJsonRead.int(obj, "faceCount", s"$path.faceCount")
      topology <- SceneJsonRead.string(obj, "topologyIdentity", s"$path.topologyIdentity")
      _ <-
        if vertices > 0 && faces > 0 && topology.matches("[0-9a-f]{16}") then Right(())
        else Left(SurfaceSceneError.InvalidJson(path, "invalid mesh identity"))
    yield SurfaceSceneAsset(id, reference, hemisphere, kind, vertices, faces, topology)

  private def parseLayer(
    value: SceneJson,
    index: Int,
    policy: SurfaceSceneReadPolicy,
    revision: SurfaceDocumentRevision
  ): Either[SurfaceSceneError, SurfaceSceneLayer] =
    val path = s"$$.layers[$index]"
    for
      obj <- SceneJsonRead.obj(value, path, Set("id", "surface", "uri", "sha256", "encoding", "frameCount", "blendMode") ++ Option.when(revision != SurfaceDocumentRevision.V1)("association") ++
        Option.when(revision.value >= 3)("vertexInterpolation") ++
        Option.when(revision.value >= 4)("scalarMappingKey") ++ Option.when(revision.value >= 5)("scalarInterpolation"), policy)
      idRaw <- SceneJsonRead.string(obj, "id", s"$path.id")
      id <- SurfaceLayerId.make(idRaw).left.map(SurfaceSceneError.InvalidState.apply)
      surfaceRaw <- SceneJsonRead.string(obj, "surface", s"$path.surface")
      surface <- SurfaceId.make(surfaceRaw).left.map(SurfaceSceneError.InvalidState.apply)
      reference <- parseReference(obj, path)
      encodingRaw <- SceneJsonRead.string(obj, "encoding", s"$path.encoding")
      encoding <- parseLayerKind(encodingRaw, s"$path.encoding")
      frames <- SceneJsonRead.int(obj, "frameCount", s"$path.frameCount")
      _ <- if frames > 0 then Right(()) else Left(SurfaceSceneError.InvalidJson(s"$path.frameCount", "must be positive"))
      blendRaw <- SceneJsonRead.string(obj, "blendMode", s"$path.blendMode")
      blend <- parseBlend(blendRaw, s"$path.blendMode")
      association <-
        if revision == SurfaceDocumentRevision.V1 then Right(SurfaceSampleAssociation.Vertex)
        else SceneJsonRead.string(obj, "association", s"$path.association").flatMap:
          case "vertex" => Right(SurfaceSampleAssociation.Vertex)
          case "face" => Right(SurfaceSampleAssociation.Face)
          case other => Left(SurfaceSceneError.InvalidJson(s"$path.association", s"unknown association: $other"))
      interpolation <-
        if revision.value < 3 then Right(SurfaceVertexInterpolation.Color)
        else SceneJsonRead.string(obj, "vertexInterpolation", s"$path.vertexInterpolation").flatMap:
          case "color" => Right(SurfaceVertexInterpolation.Color)
          case "nearest-sample" => Right(SurfaceVertexInterpolation.NearestSample)
          case other => Left(SurfaceSceneError.InvalidJson(s"$path.vertexInterpolation", s"unknown interpolation: $other"))
      mappingKey <-
        if revision.value < 4 then Right(None)
        else obj.fields.toMap.get("scalarMappingKey") match
          case Some(SceneJson.Null) => Right(None)
          case Some(SceneJson.Str(value)) if value.nonEmpty => Right(Some(value))
          case _ => Left(SurfaceSceneError.InvalidJson(s"$path.scalarMappingKey", "expected a nonempty mapping key or null"))
      scalarInterpolation <- if revision.value < 5 then Right(false)
        else SceneJsonRead.bool(obj, "scalarInterpolation", s"$path.scalarInterpolation")
    yield SurfaceSceneLayer(id, surface, reference, encoding, frames, blend, association, interpolation, mappingKey, scalarInterpolation)

  private def parseReference(obj: SceneJson.Obj, path: String): Either[SurfaceSceneError, SurfaceExternalReference] =
    for
      uriRaw <- SceneJsonRead.string(obj, "uri", s"$path.uri")
      uri <- SurfaceAssetUri.make(uriRaw)
      digestRaw <- SceneJsonRead.string(obj, "sha256", s"$path.sha256")
      digest <- SurfaceContentDigest.make(digestRaw)
    yield SurfaceExternalReference(uri, digest)

  private def parseLayout(value: SceneJson, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, SurfaceLayout] =
    val path = "$.layout"
    for
      loose <- SceneJsonRead.obj(value, path, Set("kind", "surface", "left", "right", "order"), policy)
      kind <- SceneJsonRead.string(loose, "kind", s"$path.kind")
      result <- kind match
        case "single" =>
          for
            _ <- SceneJsonRead.checkFields(loose, path, Set("kind", "surface"), policy)
            raw <- SceneJsonRead.string(loose, "surface", s"$path.surface")
            id <- SurfaceId.make(raw).left.map(SurfaceSceneError.InvalidState.apply)
          yield SurfaceLayout.Single(id)
        case "bilateral" =>
          for
            _ <- SceneJsonRead.checkFields(loose, path, Set("kind", "left", "right", "order"), policy)
            leftRaw <- SceneJsonRead.string(loose, "left", s"$path.left")
            left <- SurfaceId.make(leftRaw).left.map(SurfaceSceneError.InvalidState.apply)
            rightRaw <- SceneJsonRead.string(loose, "right", s"$path.right")
            right <- SurfaceId.make(rightRaw).left.map(SurfaceSceneError.InvalidState.apply)
            orderRaw <- SceneJsonRead.string(loose, "order", s"$path.order")
            order <- parseOrder(orderRaw, s"$path.order")
          yield SurfaceLayout.Bilateral(left, right, order)
        case other => Left(SurfaceSceneError.InvalidJson(s"$path.kind", s"unknown layout '$other'"))
    yield result

  private def parseCamera(value: SceneJson, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, SurfaceCamera] =
    val path = "$.camera"
    for
      obj <- SceneJsonRead.obj(value, path, Set("viewpoint", "projection", "zoom", "pan", "orbit"), policy)
      viewpointRaw <- SceneJsonRead.string(obj, "viewpoint", s"$path.viewpoint")
      viewpoint <- parseViewpoint(viewpointRaw, s"$path.viewpoint")
      projectionJson <- SceneJsonRead.value(obj, "projection", s"$path.projection")
      projection <- parseProjection(projectionJson, policy)
      zoomRaw <- SceneJsonRead.double(obj, "zoom", s"$path.zoom")
      zoom <- CameraZoom.make(zoomRaw).left.map(SurfaceSceneError.InvalidState.apply)
      panJson <- SceneJsonRead.array(obj, "pan", s"$path.pan")
      pan <- pair(panJson, s"$path.pan")
      orbitJson <- SceneJsonRead.array(obj, "orbit", s"$path.orbit")
      orbitPair <- pair(orbitJson, s"$path.orbit")
      orbit <- SurfaceOrbit.make(orbitPair._1, orbitPair._2).left.map(SurfaceSceneError.InvalidState.apply)
      camera <- SurfaceCamera.make(viewpoint, projection, zoom, pan._1, pan._2, orbit).left.map(SurfaceSceneError.InvalidState.apply)
    yield camera

  private def parseProjection(value: SceneJson, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, CameraProjection] =
    val path = "$.camera.projection"
    for
      obj <- SceneJsonRead.obj(value, path, Set("kind", "value"), policy)
      kind <- SceneJsonRead.string(obj, "kind", s"$path.kind")
      value <- SceneJsonRead.double(obj, "value", s"$path.value")
      projection <- kind match
        case "perspective" => FieldOfViewDegrees.make(value).map(CameraProjection.Perspective.apply).left.map(SurfaceSceneError.InvalidState.apply)
        case "orthographic" => OrthographicScale.make(value).map(CameraProjection.Orthographic.apply).left.map(SurfaceSceneError.InvalidState.apply)
        case other => Left(SurfaceSceneError.InvalidJson(s"$path.kind", s"unknown projection '$other'"))
    yield projection

  private def parseLighting(value: SceneJson, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, SurfaceLighting] =
    val path = "$.lighting"
    for
      obj <- SceneJsonRead.obj(value, path, Set("kind", "ambient", "diffuse", "direction"), policy)
      kind <- SceneJsonRead.string(obj, "kind", s"$path.kind")
      lighting <- kind match
        case "unlit" => SceneJsonRead.checkFields(obj, path, Set("kind"), policy).map(_ => SurfaceLighting.Unlit)
        case "directional" =>
          for
            ambient <- SceneJsonRead.double(obj, "ambient", s"$path.ambient")
            diffuse <- SceneJsonRead.double(obj, "diffuse", s"$path.diffuse")
            directionJson <- SceneJsonRead.array(obj, "direction", s"$path.direction")
            direction <- triple(directionJson, s"$path.direction")
            result <- SurfaceLighting.directional(ambient, diffuse, direction._1, direction._2, direction._3)
              .left.map(SurfaceSceneError.InvalidState.apply)
          yield result
        case other => Left(SurfaceSceneError.InvalidJson(s"$path.kind", s"unknown lighting '$other'"))
    yield lighting

  private def parseClipping(value: SceneJson, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, SurfaceClipping] =
    val path = "$.clipping"
    for
      obj <- SceneJsonRead.obj(value, path, Set("kind", "near", "far", "planes"), policy)
      kind <- SceneJsonRead.string(obj, "kind", s"$path.kind")
      clipping <- kind match
        case "disabled" => SceneJsonRead.checkFields(obj, path, Set("kind"), policy).map(_ => SurfaceClipping.Disabled)
        case "near-far" =>
          for
            near <- SceneJsonRead.double(obj, "near", s"$path.near")
            far <- SceneJsonRead.double(obj, "far", s"$path.far")
            result <- SurfaceClipping.nearFar(near, far).left.map(SurfaceSceneError.InvalidState.apply)
          yield result
        case "world-planes" =>
          for
            values <- SceneJsonRead.array(obj, "planes", s"$path.planes")
            planes <- traverseIndexed(values)(parsePlane(_, _, policy))
            result <- SurfaceClipping.worldPlanes(planes).left.map(SurfaceSceneError.InvalidState.apply)
          yield result
        case other => Left(SurfaceSceneError.InvalidJson(s"$path.kind", s"unknown clipping '$other'"))
    yield clipping

  private def parsePlane(value: SceneJson, index: Int, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, WorldClipPlane] =
    val path = s"$$.clipping.planes[$index]"
    for
      obj <- SceneJsonRead.obj(value, path, Set("normal", "offset", "keep"), policy)
      normalJson <- SceneJsonRead.array(obj, "normal", s"$path.normal")
      normal <- triple(normalJson, s"$path.normal")
      offset <- SceneJsonRead.double(obj, "offset", s"$path.offset")
      keepRaw <- SceneJsonRead.string(obj, "keep", s"$path.keep")
      keep <- keepRaw match
        case "positive" => Right(ClipKeepSide.Positive)
        case "negative" => Right(ClipKeepSide.Negative)
        case other => Left(SurfaceSceneError.InvalidJson(s"$path.keep", s"unknown keep side '$other'"))
      plane <- WorldClipPlane.make(normal._1, normal._2, normal._3, offset, keep).left.map(SurfaceSceneError.InvalidState.apply)
    yield plane

  private def parseSelection(value: SceneJson, policy: SurfaceSceneReadPolicy, revision: SurfaceDocumentRevision): Either[SurfaceSceneError, Option[SurfaceSelection]] =
    value match
      case SceneJson.Null => Right(None)
      case other =>
        val path = "$.selection"
        for
          obj <- SceneJsonRead.obj(other, path, Set("surface", "vertex") ++ Option.when(revision != SurfaceDocumentRevision.V1)("face"), policy)
          surfaceRaw <- SceneJsonRead.string(obj, "surface", s"$path.surface")
          surface <- SurfaceId.make(surfaceRaw).left.map(SurfaceSceneError.InvalidState.apply)
          vertex <- SceneJsonRead.int(obj, "vertex", s"$path.vertex")
          _ <- if vertex >= 0 then Right(()) else Left(SurfaceSceneError.InvalidJson(s"$path.vertex", "must be non-negative"))
          face <-
            if revision == SurfaceDocumentRevision.V1 then Right(None)
            else SceneJsonRead.value(obj, "face", s"$path.face").flatMap:
              case SceneJson.Null => Right(None)
              case _ => SceneJsonRead.int(obj, "face", s"$path.face").flatMap: index =>
                if index >= 0 then Right(Some(FaceId(index)))
                else Left(SurfaceSceneError.InvalidJson(s"$path.face", "must be non-negative"))
        yield Some(SurfaceSelection(surface, VertexId(vertex), face))

  private def parseLayerState(value: SceneJson, index: Int, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, SurfaceSceneLayerState] =
    val path = s"$$.layerStates[$index]"
    for
      obj <- SceneJsonRead.obj(value, path, Set("id", "visible", "opacity", "window", "threshold"), policy)
      idRaw <- SceneJsonRead.string(obj, "id", s"$path.id")
      id <- SurfaceLayerId.make(idRaw).left.map(SurfaceSceneError.InvalidState.apply)
      visible <- SceneJsonRead.bool(obj, "visible", s"$path.visible")
      opacityRaw <- SceneJsonRead.double(obj, "opacity", s"$path.opacity")
      opacity <- DisplayOpacity.make(opacityRaw).left.map(error => SurfaceSceneError.InvalidState(SurfaceViewError.DisplayFailure(error)))
      windowJson <- SceneJsonRead.value(obj, "window", s"$path.window")
      window <- parseWindow(windowJson, s"$path.window")
      thresholdJson <- SceneJsonRead.value(obj, "threshold", s"$path.threshold")
      threshold <- parseThreshold(thresholdJson, s"$path.threshold", policy)
    yield SurfaceSceneLayerState(id, visible, opacity, window, threshold)

  private def parseWindow(value: SceneJson, path: String): Either[SurfaceSceneError, Option[DisplayWindow]] =
    value match
      case SceneJson.Null => Right(None)
      case SceneJson.Arr(values) =>
        pair(values, path).flatMap: (lower, upper) =>
          DisplayWindow.make(lower, upper)
            .map(Some(_))
            .left.map(error => SurfaceSceneError.InvalidState(SurfaceViewError.DisplayFailure(error)))
      case _ => Left(SurfaceSceneError.InvalidJson(path, "expected null or a two-number array"))

  private def parseThreshold(
    value: SceneJson,
    path: String,
    policy: SurfaceSceneReadPolicy
  ): Either[SurfaceSceneError, Option[DisplayThreshold]] =
    value match
      case SceneJson.Null => Right(None)
      case other =>
        for
          obj <- SceneJsonRead.obj(other, path, Set("kind", "lower", "upper"), policy)
          kind <- SceneJsonRead.string(obj, "kind", s"$path.kind")
          threshold <- kind match
            case "disabled" => SceneJsonRead.checkFields(obj, path, Set("kind"), policy).map(_ => DisplayThreshold.Disabled)
            case "transparent-band" =>
              for
                lower <- SceneJsonRead.double(obj, "lower", s"$path.lower")
                upper <- SceneJsonRead.double(obj, "upper", s"$path.upper")
                result <- DisplayThreshold.transparentBand(lower, upper)
                  .left.map(error => SurfaceSceneError.InvalidState(SurfaceViewError.DisplayFailure(error)))
              yield result
            case unknown => Left(SurfaceSceneError.InvalidJson(s"$path.kind", s"unknown threshold '$unknown'"))
        yield Some(threshold)

  private def parseProvenance(value: SceneJson, policy: SurfaceSceneReadPolicy): Either[SurfaceSceneError, SurfaceProvenance] =
    val path = "$.provenance"
    for
      obj <- SceneJsonRead.obj(value, path, Set("producer", "softwareVersion", "createdAt", "entries"), policy)
      producer <- SceneJsonRead.string(obj, "producer", s"$path.producer")
      version <- SceneJsonRead.string(obj, "softwareVersion", s"$path.softwareVersion")
      created <- SceneJsonRead.string(obj, "createdAt", s"$path.createdAt")
      entriesValue <- SceneJsonRead.value(obj, "entries", s"$path.entries")
      entriesObj <- SceneJsonRead.obj(entriesValue, s"$path.entries", Set.empty, policy, allowAny = true)
      entries <- traverse(entriesObj.fields): (key, value) =>
        SceneJsonRead.stringValue(value, s"$path.entries.$key").map(key -> _)
      provenance <- SurfaceProvenance.make(producer, version, created, entries)
    yield provenance

  private def pair(values: Vector[SceneJson], path: String): Either[SurfaceSceneError, (Double, Double)] =
    if values.length != 2 then Left(SurfaceSceneError.InvalidJson(path, "expected two numbers"))
    else for
      first <- SceneJsonRead.doubleValue(values(0), s"$path[0]")
      second <- SceneJsonRead.doubleValue(values(1), s"$path[1]")
    yield (first, second)

  private def triple(values: Vector[SceneJson], path: String): Either[SurfaceSceneError, (Double, Double, Double)] =
    if values.length != 3 then Left(SurfaceSceneError.InvalidJson(path, "expected three numbers"))
    else for
      first <- SceneJsonRead.doubleValue(values(0), s"$path[0]")
      second <- SceneJsonRead.doubleValue(values(1), s"$path[1]")
      third <- SceneJsonRead.doubleValue(values(2), s"$path[2]")
    yield (first, second, third)

  private def parseHemisphere(value: String, path: String): Either[SurfaceSceneError, CorticalHemisphere] =
    value match
      case "left" => Right(CorticalHemisphere.Left)
      case "right" => Right(CorticalHemisphere.Right)
      case other => Left(SurfaceSceneError.InvalidJson(path, s"unknown hemisphere '$other'"))

  private def parseLayerKind(value: String, path: String): Either[SurfaceSceneError, SurfaceLayerKind] =
    value match
      case "scalar" => Right(SurfaceLayerKind.Scalar)
      case "labels" => Right(SurfaceLayerKind.Labels)
      case "mask" => Right(SurfaceLayerKind.Mask)
      case "packed-rgba" => Right(SurfaceLayerKind.PackedRgba)
      case other => Left(SurfaceSceneError.InvalidJson(path, s"unknown layer encoding '$other'"))

  private def parseBlend(value: String, path: String): Either[SurfaceSceneError, DisplayBlendMode] =
    value match
      case "normal" => Right(DisplayBlendMode.Normal)
      case "additive" => Right(DisplayBlendMode.Additive)
      case "multiply" => Right(DisplayBlendMode.Multiply)
      case "screen" => Right(DisplayBlendMode.Screen)
      case other => Left(SurfaceSceneError.InvalidJson(path, s"unknown blend mode '$other'"))

  private def parseOrder(value: String, path: String): Either[SurfaceSceneError, BilateralOrder] =
    value match
      case "left-then-right" => Right(BilateralOrder.LeftThenRight)
      case "right-then-left" => Right(BilateralOrder.RightThenLeft)
      case other => Left(SurfaceSceneError.InvalidJson(path, s"unknown bilateral order '$other'"))

  private def parseViewpoint(value: String, path: String): Either[SurfaceSceneError, SurfaceViewpoint] =
    value match
      case "lateral-left" => Right(SurfaceViewpoint.Lateral(CorticalHemisphere.Left))
      case "lateral-right" => Right(SurfaceViewpoint.Lateral(CorticalHemisphere.Right))
      case "medial-left" => Right(SurfaceViewpoint.Medial(CorticalHemisphere.Left))
      case "medial-right" => Right(SurfaceViewpoint.Medial(CorticalHemisphere.Right))
      case "anterior" => Right(SurfaceViewpoint.Anterior)
      case "posterior" => Right(SurfaceViewpoint.Posterior)
      case "dorsal" => Right(SurfaceViewpoint.Dorsal)
      case "ventral" => Right(SurfaceViewpoint.Ventral)
      case other => Left(SurfaceSceneError.InvalidJson(path, s"unknown viewpoint '$other'"))

  private def parseFeature(value: String): Either[SurfaceSceneError, SurfaceBackendFeature] =
    SurfaceBackendFeature.values.find(feature => SurfaceSceneNames.feature(feature) == value) match
      case Some(feature) => Right(feature)
      case None => Left(SurfaceSceneError.InvalidJson("$.requiredFeatures", s"unknown feature '$value'"))

  private def hemisphereName(value: CorticalHemisphere): String =
    value match
      case CorticalHemisphere.Left => "left"
      case CorticalHemisphere.Right => "right"

  private def layerKindName(value: SurfaceLayerKind): String =
    value match
      case SurfaceLayerKind.Scalar => "scalar"
      case SurfaceLayerKind.Labels => "labels"
      case SurfaceLayerKind.Mask => "mask"
      case SurfaceLayerKind.PackedRgba => "packed-rgba"

  private def blendName(value: DisplayBlendMode): String = value.toString.toLowerCase

  private def orderName(value: BilateralOrder): String =
    value match
      case BilateralOrder.LeftThenRight => "left-then-right"
      case BilateralOrder.RightThenLeft => "right-then-left"

  private def viewpointName(value: SurfaceViewpoint): String =
    value match
      case SurfaceViewpoint.Lateral(CorticalHemisphere.Left) => "lateral-left"
      case SurfaceViewpoint.Lateral(CorticalHemisphere.Right) => "lateral-right"
      case SurfaceViewpoint.Medial(CorticalHemisphere.Left) => "medial-left"
      case SurfaceViewpoint.Medial(CorticalHemisphere.Right) => "medial-right"
      case SurfaceViewpoint.Anterior => "anterior"
      case SurfaceViewpoint.Posterior => "posterior"
      case SurfaceViewpoint.Dorsal => "dorsal"
      case SurfaceViewpoint.Ventral => "ventral"

  private def number(value: Double): SceneJson = SceneJson.Num(java.lang.Double.toString(value))

  private def traverse[A, B](values: Vector[A])(f: A => Either[SurfaceSceneError, B]): Either[SurfaceSceneError, Vector[B]] =
    val result = Vector.newBuilder[B]
    var index = 0
    while index < values.length do
      f(values(index)) match
        case Left(error) => return Left(error)
        case Right(value) => result += value
      index += 1
    Right(result.result())

  private def traverseIndexed[A, B](values: Vector[A])(
    f: (A, Int) => Either[SurfaceSceneError, B]
  ): Either[SurfaceSceneError, Vector[B]] =
    val result = Vector.newBuilder[B]
    var index = 0
    while index < values.length do
      f(values(index), index) match
        case Left(error) => return Left(error)
        case Right(value) => result += value
      index += 1
    Right(result.result())

private[view] enum SceneJson:
  case Obj(fields: Vector[(String, SceneJson)])
  case Arr(values: Vector[SceneJson])
  case Str(value: String)
  case Num(raw: String)
  case Bool(value: Boolean)
  case Null

  def render: String =
    this match
      case Obj(fields) => fields.map((key, value) => s"${SceneJson.Str(key).render}:${value.render}").mkString("{", ",", "}")
      case Arr(values) => values.map(_.render).mkString("[", ",", "]")
      case Str(value) =>
        val result = new StringBuilder("\"")
        value.foreach:
          case '"' => result.append("\\\"")
          case '\\' => result.append("\\\\")
          case '\b' => result.append("\\b")
          case '\f' => result.append("\\f")
          case '\n' => result.append("\\n")
          case '\r' => result.append("\\r")
          case '\t' => result.append("\\t")
          case character if character < ' ' => result.append(f"\\u${character.toInt}%04x")
          case character => result.append(character)
        result.append('"').result()
      case Num(raw) => raw
      case Bool(value) => value.toString
      case Null => "null"

private[view] object SceneJson:
  def obj(fields: (String, SceneJson)*): SceneJson = Obj(fields.toVector)

private[view] object SceneJsonRead:
  def obj(
    value: SceneJson,
    path: String,
    allowed: Set[String],
    policy: SurfaceSceneReadPolicy,
    allowAny: Boolean = false
  ): Either[SurfaceSceneError, SceneJson.Obj] =
    value match
      case result: SceneJson.Obj =>
        if allowAny then Right(result) else checkFields(result, path, allowed, policy).map(_ => result)
      case _ => Left(SurfaceSceneError.InvalidJson(path, "expected object"))

  def checkFields(
    value: SceneJson.Obj,
    path: String,
    allowed: Set[String],
    policy: SurfaceSceneReadPolicy
  ): Either[SurfaceSceneError, Unit] =
    if policy.unknownFields == SurfaceUnknownFieldPolicy.Ignore then Right(())
    else
      value.fields.find((name, _) => !allowed(name)) match
        case Some((name, _)) => Left(SurfaceSceneError.UnknownField(path, name))
        case None => Right(())

  def value(obj: SceneJson.Obj, name: String, path: String): Either[SurfaceSceneError, SceneJson] =
    obj.fields.find(_._1 == name).map(_._2).toRight(SurfaceSceneError.InvalidJson(path, "missing required field"))

  def string(obj: SceneJson.Obj, name: String, path: String): Either[SurfaceSceneError, String] =
    value(obj, name, path).flatMap(stringValue(_, path))

  def stringValue(value: SceneJson, path: String): Either[SurfaceSceneError, String] =
    value match
      case SceneJson.Str(result) => Right(result)
      case _ => Left(SurfaceSceneError.InvalidJson(path, "expected string"))

  def array(obj: SceneJson.Obj, name: String, path: String): Either[SurfaceSceneError, Vector[SceneJson]] =
    value(obj, name, path).flatMap:
      case SceneJson.Arr(values) => Right(values)
      case _ => Left(SurfaceSceneError.InvalidJson(path, "expected array"))

  def bool(obj: SceneJson.Obj, name: String, path: String): Either[SurfaceSceneError, Boolean] =
    value(obj, name, path).flatMap:
      case SceneJson.Bool(result) => Right(result)
      case _ => Left(SurfaceSceneError.InvalidJson(path, "expected boolean"))

  def int(obj: SceneJson.Obj, name: String, path: String): Either[SurfaceSceneError, Int] =
    value(obj, name, path).flatMap:
      case SceneJson.Num(raw) =>
        raw.toIntOption.toRight(SurfaceSceneError.InvalidJson(path, "expected exact 32-bit integer"))
      case _ => Left(SurfaceSceneError.InvalidJson(path, "expected integer"))

  def double(obj: SceneJson.Obj, name: String, path: String): Either[SurfaceSceneError, Double] =
    value(obj, name, path).flatMap(doubleValue(_, path))

  def doubleValue(value: SceneJson, path: String): Either[SurfaceSceneError, Double] =
    value match
      case SceneJson.Num(raw) =>
        raw.toDoubleOption.filter(_.isFinite).toRight(SurfaceSceneError.InvalidJson(path, "expected finite number"))
      case _ => Left(SurfaceSceneError.InvalidJson(path, "expected number"))

private object SceneJsonParser:
  def parse(input: String): Either[SurfaceSceneError, SceneJson] =
    val parser = new Parser(input)
    parser.value("$").flatMap: result =>
      parser.skipWhitespace()
      if parser.finished then Right(result)
      else Left(SurfaceSceneError.InvalidJson("$", s"unexpected trailing input at offset ${parser.offset}"))

  private final class Parser(input: String):
    private var cursor = 0

    def offset: Int = cursor
    def finished: Boolean = cursor == input.length

    def skipWhitespace(): Unit =
      while cursor < input.length && input.charAt(cursor).isWhitespace do cursor += 1

    def value(path: String): Either[SurfaceSceneError, SceneJson] =
      skipWhitespace()
      if cursor >= input.length then Left(error(path, "unexpected end of input"))
      else input.charAt(cursor) match
        case '{' => objectValue(path)
        case '[' => arrayValue(path)
        case '"' => stringValue(path).map(SceneJson.Str.apply)
        case 't' => literal("true", SceneJson.Bool(true), path)
        case 'f' => literal("false", SceneJson.Bool(false), path)
        case 'n' => literal("null", SceneJson.Null, path)
        case character if character == '-' || character.isDigit => numberValue(path)
        case character => Left(error(path, s"unexpected character '$character'"))

    private def objectValue(path: String): Either[SurfaceSceneError, SceneJson] =
      cursor += 1
      skipWhitespace()
      val fields = Vector.newBuilder[(String, SceneJson)]
      val seen = scala.collection.mutable.HashSet.empty[String]
      if consume('}') then Right(SceneJson.Obj(Vector.empty))
      else
        var done = false
        while !done do
          val key = stringValue(path) match
            case Left(error) => return Left(error)
            case Right(result) => result
          if !seen.add(key) then return Left(error(path, s"duplicate field '$key'"))
          skipWhitespace()
          if !consume(':') then return Left(error(path, "expected ':'"))
          value(s"$path.$key") match
            case Left(error) => return Left(error)
            case Right(result) => fields += key -> result
          skipWhitespace()
          if consume('}') then done = true
          else if consume(',') then skipWhitespace()
          else return Left(error(path, "expected ',' or '}'"))
        Right(SceneJson.Obj(fields.result()))

    private def arrayValue(path: String): Either[SurfaceSceneError, SceneJson] =
      cursor += 1
      skipWhitespace()
      val values = Vector.newBuilder[SceneJson]
      if consume(']') then Right(SceneJson.Arr(Vector.empty))
      else
        var index = 0
        var done = false
        while !done do
          value(s"$path[$index]") match
            case Left(error) => return Left(error)
            case Right(result) => values += result
          index += 1
          skipWhitespace()
          if consume(']') then done = true
          else if consume(',') then skipWhitespace()
          else return Left(error(path, "expected ',' or ']'"))
        Right(SceneJson.Arr(values.result()))

    private def stringValue(path: String): Either[SurfaceSceneError, String] =
      skipWhitespace()
      if !consume('"') then Left(error(path, "expected string"))
      else
        val result = new StringBuilder
        while cursor < input.length do
          val character = input.charAt(cursor)
          cursor += 1
          character match
            case '"' => return Right(result.result())
            case '\\' =>
              if cursor >= input.length then return Left(error(path, "unterminated escape"))
              val escaped = input.charAt(cursor)
              cursor += 1
              escaped match
                case '"' => result.append('"')
                case '\\' => result.append('\\')
                case '/' => result.append('/')
                case 'b' => result.append('\b')
                case 'f' => result.append('\f')
                case 'n' => result.append('\n')
                case 'r' => result.append('\r')
                case 't' => result.append('\t')
                case 'u' =>
                  if cursor + 4 > input.length then return Left(error(path, "short unicode escape"))
                  val raw = input.substring(cursor, cursor + 4)
                  parseHex(raw) match
                    case None => return Left(error(path, s"invalid unicode escape '$raw'"))
                    case Some(code) => result.append(code.toChar)
                  cursor += 4
                case other => return Left(error(path, s"invalid escape '$other'"))
            case control if control < ' ' => return Left(error(path, "unescaped control character"))
            case other => result.append(other)
        Left(error(path, "unterminated string"))

    private def numberValue(path: String): Either[SurfaceSceneError, SceneJson] =
      val start = cursor
      consume('-')
      if consume('0') then ()
      else if cursor < input.length && input.charAt(cursor).isDigit then
        while cursor < input.length && input.charAt(cursor).isDigit do cursor += 1
      else return Left(error(path, "invalid number"))
      if consume('.') then
        val fractionStart = cursor
        while cursor < input.length && input.charAt(cursor).isDigit do cursor += 1
        if cursor == fractionStart then return Left(error(path, "invalid number fraction"))
      if cursor < input.length && (input.charAt(cursor) == 'e' || input.charAt(cursor) == 'E') then
        cursor += 1
        if cursor < input.length && (input.charAt(cursor) == '+' || input.charAt(cursor) == '-') then cursor += 1
        val exponentStart = cursor
        while cursor < input.length && input.charAt(cursor).isDigit do cursor += 1
        if cursor == exponentStart then return Left(error(path, "invalid number exponent"))
      Right(SceneJson.Num(input.substring(start, cursor)))

    private def literal(expected: String, result: SceneJson, path: String): Either[SurfaceSceneError, SceneJson] =
      if input.startsWith(expected, cursor) then
        cursor += expected.length
        Right(result)
      else Left(error(path, s"expected '$expected'"))

    private def parseHex(value: String): Option[Int] =
      try Some(java.lang.Integer.parseInt(value, 16))
      catch case _: NumberFormatException => None

    private def consume(expected: Char): Boolean =
      if cursor < input.length && input.charAt(cursor) == expected then
        cursor += 1
        true
      else false

    private def error(path: String, reason: String): SurfaceSceneError =
      SurfaceSceneError.InvalidJson(path, s"$reason at offset $cursor")
