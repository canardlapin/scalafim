package scalafim.surface.view

import image4s.geometry.{Affine, D3, Frame, Vec}
import intaglio.*
import scalafim.image.{SampleSpaces, SomeScalarVolume}
import scalafim.image.SampleSpaces.*
import scalafim.surface.*
import scalafim.surface.view.raster.{SurfaceRasterStyle, TriangleCulling}

/** Frozen-input adapter. It consumes source JSON only; renderer output is
  * deliberately absent from this boundary.
  */
object SurfaceQualificationFixture:
  final case class SourceSummary(surface: SurfaceId, vertices: Int, faces: Int)
  final case class Prepared(width: Int, height: Int, model: SurfaceViewerModel,
      state: SurfaceViewerState, plan: SurfaceCompositionPlan,
      decorations: SurfaceDecorationGeometry, sourceDecorations: Vector[SurfaceDecoration],
      sourceSummaries: Vector[SourceSummary], style: SurfaceRasterStyle,
      private val decorationsLowerer: (SurfaceCompositionPlan, Int, Int) => Either[String, SurfaceDecorationGeometry]):
    def numericReadouts: Vector[(SurfaceId, SurfaceLayerId, Vector[Double])] =
      model.layers.collect:
        case layer if layer.kind == SurfaceLayerKind.Scalar =>
          (layer.surfaceId, layer.id, Vector.tabulate(layer.sampleCount)(layer.describe(_, state.timepoint).toDouble))

    /** Bounds of the actual lowered sphere geometry, not a source-anchor echo. */
    def markerCenters: Vector[(SurfaceDecorationId, SurfaceId, VertexId, Vector[Double])] =
      val spheres = sourceDecorations.collect:
        case marker: SurfaceMarkerDecoration if marker.shape == SurfaceMarkerShape.Sphere => marker.id
      decorations.meshes.filter(mesh => spheres.contains(mesh.id.decoration)).flatMap: mesh =>
        mesh.target match
          case SurfaceDecorationTarget.Anchor(SurfaceDecorationAnchor.Vertex(anchor)) =>
            val center = Vector.tabulate(3): axis =>
              val coordinates = Vector.tabulate(mesh.positions.length / 3)(i => mesh.positions(i * 3 + axis).toDouble)
              (coordinates.min + coordinates.max) / 2.0
            Vector((mesh.id.decoration, anchor.surface, anchor.vertex, center))
          case _ => Vector.empty

    def lowerDecorations(plan: SurfaceCompositionPlan, width: Int, height: Int): Either[String, SurfaceDecorationGeometry] =
      decorationsLowerer(plan, width, height)

  def decode(json: String): Either[String, Prepared] =
    for
      root <- SceneJsonParser.parse(json).left.map(_.message)
      obj <- objectAt(root, "$")
      _ <- check(obj.fields.forall((name, _) => Set("width", "height", "camera", "profile", "surfaces", "views", "timeSeconds", "culling")(name)), "unsupported top-level field")
      _ <- check(obj.fields.toMap.get("culling").forall(_ == SceneJson.Str("double-sided")), "only explicit double-sided qualification is supported")
      timeSeconds <- obj.fields.toMap.get("timeSeconds") match
        case None => Right(0.0)
        case Some(SceneJson.Num(value)) => value.toDoubleOption.filter(_.isFinite).toRight("$.timeSeconds: expected finite number")
        case _ => Left("$.timeSeconds: expected number")
      width <- integer(obj, "width", "$")
      height <- integer(obj, "height", "$")
      _ <- check(width > 0 && height > 0, "width and height must be positive")
      cameraJson <- field(obj, "camera", "$")
      camera <- parseCamera(cameraJson)
      profileJson <- field(obj, "profile", "$")
      lighting <- parseProfile(profileJson)
      surfaceJson <- field(obj, "surfaces", "$")
      surfaces <- array(surfaceJson, "$.surfaces").flatMap(values => traverse(values.zipWithIndex)(parseSurface(_, _, timeSeconds)))
      _ <- check(surfaces.nonEmpty, "surfaces must be nonempty")
      views <- obj.fields.toMap.get("views") match
        case None => Right(Vector.empty)
        case Some(value) => parseViews(value, surfaces)
      assets <- traverse(surfaces.map(_.asset))(value => Right(value))
      layers = surfaces.flatMap(s => s.curvatureLayer.toVector ++ s.layers.map(_._1))
      model <- SurfaceViewerModel.make(assets, layers).left.map(_.message)
      initial <- viewerState(model, surfaces, lighting)
      profileObject <- objectAt(profileJson, "$.profile")
      profileName <- string(profileObject, "name", "$.profile")
      state = initial.copy(colorPipeline = if profileName == "observed-default" then SurfaceColorPipeline.LegacyDisplayBytes else SurfaceColorPipeline.DisplaySrgbLinearLight,
        surfaceBase = if profileName == "observed-default" then SurfaceDisplayDefaults.LegacySurfaceBase else Rgba32.unsafe(204, 204, 204),
        presentations = initial.presentations.map: (id, value) =>
          id -> value.copy(visible = surfaces.flatMap(_.layers).find(_._1.id == id).forall(_._2)))
      plan <- composition(model, state, surfaces, camera, views)
      sourceDecorations = surfaces.flatMap(_.decorations)
      networks = surfaces.flatMap(_.networks)
      decorations <- lowerDecorations(sourceDecorations, networks, plan, width, height)
    yield Prepared(width, height, model, state, plan, decorations, sourceDecorations,
      surfaces.map(s => SourceSummary(s.id, s.geometry.vertexCount, s.geometry.faceCount)), SurfaceRasterStyle(culling = TriangleCulling.None),
      (nextPlan, nextWidth, nextHeight) => lowerDecorations(sourceDecorations, networks, nextPlan, nextWidth, nextHeight))

  private final case class InputSurface(id: SurfaceId, geometry: SurfaceGeometry, asset: SurfaceAsset,
      curvatureLayer: Option[SurfaceLayer], layers: Vector[(SurfaceLayer, Boolean)], decorations: Vector[SurfaceDecoration],
      networks: Vector[NetworkInput], morph: Option[SurfaceMorphFraction], timepoint: Int, opacity: DisplayOpacity,
      clip: Option[WorldClipPlane])

  private final case class NetworkInput(surface: SurfaceId, display: SurfaceNetworkDisplay,
      nodeSize: SurfaceDecorationSize, nodeColor: Rgba32, width: Option[SurfaceNetworkDecorationRadius], showNodes: Boolean, depthPolicy: SurfaceDecorationDepthPolicy)

  private final case class InputView(id: SurfaceViewId, viewport: SurfaceViewport,
      camera: SurfaceCameraPacket, surfaces: Vector[SurfaceId])

  private def parseViews(value: SceneJson, surfaces: Vector[InputSurface]): Either[String, Vector[InputView]] =
    val path = "$.views"
    for
      entries <- array(value, path)
      _ <- check(entries.nonEmpty, s"$path must be nonempty when present")
      views <- traverse(entries.zipWithIndex): (raw, index) =>
        val viewPath = s"$path[$index]"
        for
          obj <- objectAt(raw, viewPath)
          _ <- check(obj.fields.forall((name, _) => Set("id", "viewport", "camera", "surfaces")(name)),
            s"$viewPath contains an unsupported field")
          id <- string(obj, "id", viewPath).flatMap(SurfaceViewId.make(_).left.map(_.toString))
          bounds <- field(obj, "viewport", viewPath).flatMap(array(_, s"$viewPath.viewport")).flatMap(doubles(_, s"$viewPath.viewport"))
          _ <- check(bounds.length == 4, s"$viewPath.viewport: expected [x, y, width, height]")
          camera <- field(obj, "camera", viewPath).flatMap(parseCamera(_, s"$viewPath.camera"))
          selected <- field(obj, "surfaces", viewPath).flatMap(array(_, s"$viewPath.surfaces")).flatMap: rawIds =>
            traverse(rawIds.zipWithIndex):
              case (rawId, sourceIndex) => rawId match
                case SceneJson.Str(value) => SurfaceId.make(value).left.map(_.message)
                case _ => Left(s"$viewPath.surfaces[$sourceIndex]: expected string")
          _ <- check(selected.distinct.length == selected.length, s"$viewPath has duplicate source surface IDs")
          _ <- check(selected.forall(id => surfaces.exists(_.id == id)), s"$viewPath names a missing source surface")
        yield InputView(id, SurfaceViewport(bounds(0), bounds(1), bounds(2), bounds(3)), camera, selected)
      _ <- check(views.map(_.id).distinct.length == views.length, s"$path has duplicate IDs")
    yield views

  private def lowerDecorations(sourceDecorations: Vector[SurfaceDecoration], networks: Vector[NetworkInput],
      plan: SurfaceCompositionPlan, width: Int, height: Int): Either[String, SurfaceDecorationGeometry] =
    for
      packets <- SurfaceDecorationCompiler.compile(sourceDecorations, plan).left.map(_.toString)
      ordinary <- SurfaceDecorationGeometry.lower(packets, plan, width, height).left.map(_.toString)
      loweredNetworks <- traverse(networks)(lowerNetwork(_, plan, width, height))
    yield ordinary.copy(
      meshes = ordinary.meshes ++ loweredNetworks.flatMap(_.meshes),
      labels = ordinary.labels ++ loweredNetworks.flatMap(_.labels)
    )

  private def parseSurface(value: SceneJson, index: Int, timeSeconds: Double): Either[String, InputSurface] =
    val path = s"$$.surfaces[$index]"
    for
      obj <- objectAt(value, path)
      id <- string(obj, "id", path).flatMap(value => SurfaceId.make(value).left.map(_.message))
      hemisphere <- string(obj, "hemisphere", path).flatMap:
        case "left" => Right(Hemisphere.Left)
        case "right" => Right(Hemisphere.Right)
        case other => Left(s"$path.hemisphere: expected left or right, got $other")
      vertices <- field(obj, "vertices", path).flatMap(array(_, s"$path.vertices")).flatMap(rows(_, s"$path.vertices", 3))
      faces <- field(obj, "faces", path).flatMap(array(_, s"$path.faces")).flatMap(intRows(_, s"$path.faces", 3))
      _ <- check(obj.fields.forall((name, _) => Set("id", "hemisphere", "vertices", "faces", "curvature", "showCurvature", "layers", "annotations", "crosshair", "morph", "alpha", "clip")(name)), s"$path contains an unsupported field")
      opacity <- optionalNumber(obj, "alpha", 1).flatMap(value => DisplayOpacity.make(value).left.map(_.toString))
      clip <- obj.fields.toMap.get("clip") match
        case None => Right(None)
        case Some(raw) => parseClip(raw).map(Some.apply)
      geometry <- either(TriangleMesh.fromRows(vertices, faces.map(v => (v(0), v(1), v(2)))), s"$path mesh")
        .map(mesh => SurfaceGeometry(mesh, hemisphere, SurfaceKind.Midthickness))
      morph <- obj.fields.toMap.get("morph") match
        case None => Right(None)
        case Some(raw) => parseMorph(raw, geometry, path).map(Some.apply)
      asset <- morph match
        case None => SurfaceAsset.make(id, geometry).left.map(_.message)
        case Some((variant, _)) => SurfaceAsset.make(id, SurfaceSet.of(SurfaceKind.Midthickness, geometry, variant.kind -> variant)).left.map(_.message)
      layers <- field(obj, "layers", path).flatMap(array(_, s"$path.layers"))
      outlineDecorations <- traverse(layers.collect { case layer @ SceneJson.Obj(fields) if fields.toMap.get("kind").contains(SceneJson.Str("outline")) => layer })(parseOutline(_, id, geometry, path))
      networks <- traverse(layers.collect { case layer @ SceneJson.Obj(fields) if fields.toMap.get("kind").contains(SceneJson.Str("network")) => layer })(parseNetwork(_, id, geometry, path))
      displayLayers = layers.filter:
        case SceneJson.Obj(fields) =>
          val kind = fields.toMap.get("kind")
          !kind.contains(SceneJson.Str("outline")) && !kind.contains(SceneJson.Str("network"))
        case _ => true
      lastVisible = displayLayers.lastIndexWhere:
        case SceneJson.Obj(fields) => !fields.toMap.get("visible").contains(SceneJson.Bool(false))
        case _ => true
      decodedLayers <- traverse(displayLayers.zipWithIndex)((value, index) => parseLayer(value, id, geometry, index == lastVisible, timeSeconds))
      curvature <- obj.fields.toMap.get("curvature") match
        case None => Right(None)
        case Some(raw) => array(raw, s"$path.curvature").flatMap(values => doubles(values, s"$path.curvature")).flatMap: values =>
          check(values.length == geometry.vertexCount, s"$path.curvature length differs from vertex count").map(_ => Some(values))
      show <- optionalBoolean(obj, "showCurvature", false)
      layer <- if !show then Right(None) else curvature.toRight(s"$path.showCurvature requires curvature").flatMap: values =>
        SurfaceLayer.curvatureUnderlayWithStyle(SurfaceLayerId.unsafe(s"${id.value}-curvature"), id,
          SurfaceField.full(geometry, values, "frozen curvature"), geometry, SurfaceCurvatureDisplay.Default).left.map(_.message).map(Some.apply)
      annotations <- obj.fields.toMap.get("annotations").fold[Either[String, Vector[SurfaceDecoration]]](Right(Vector.empty))(parseAnnotations(_, id, geometry, path))
      crosshair <- obj.fields.toMap.get("crosshair").fold[Either[String, Vector[SurfaceDecoration]]](Right(Vector.empty))(parseCrosshair(_, id, geometry, path))
    yield InputSurface(id, geometry, asset, layer, decodedLayers, outlineDecorations ++ annotations ++ crosshair,
      networks, morph.map(_._2), 0, opacity, clip)

  private def parseNetwork(value: SceneJson, surface: SurfaceId, geometry: SurfaceGeometry,
      surfacePath: String): Either[String, NetworkInput] =
    val path = s"$surfacePath network"
    for
      obj <- objectAt(value, path)
      _ <- check(obj.fields.forall((name, _) => Set("id", "kind", "edges", "colorMap", "weightRange", "threshold",
        "renderMode", "tubeRadius", "tubeRadiusScale", "showNodes", "nodeRadius", "nodeColor", "topN", "presentation")(name)),
        s"$path contains an unsupported field")
      rawId <- string(obj, "id", path)
      edges <- field(obj, "edges", path).flatMap(array(_, s"$path.edges")).flatMap: values =>
        traverse(values.zipWithIndex): (raw, index) =>
          val edgePath = s"$path.edges[$index]"
          for
            edge <- objectAt(raw, edgePath)
            source <- integer(edge, "source", edgePath)
            target <- integer(edge, "target", edgePath)
            _ <- check(source >= 0 && source < geometry.vertexCount && target >= 0 && target < geometry.vertexCount,
              s"$edgePath endpoint lies outside source mesh")
            weight <- number(edge, "weight", edgePath)
          yield (source, target, weight)
      colors <- field(obj, "colorMap", path).flatMap(array(_, s"$path.colorMap")).flatMap(rows(_, s"$path.colorMap", 3)).flatMap: values =>
        check(values.length == 2, s"$path.colorMap requires [negative, positive] colors").flatMap(_ => traverse(values)(rgba))
      weightRange <- field(obj, "weightRange", path).flatMap(array(_, s"$path.weightRange")).flatMap(doubles(_, s"$path.weightRange"))
      _ <- check(weightRange == Vector(-1.0, 1.0), s"$path only qualifies the frozen [-1, 1] weight range")
      threshold <- number(obj, "threshold", path).flatMap(SurfaceNetworkThreshold.make(_).left.map(_.message))
      topN <- integer(obj, "topN", path).flatMap(SurfaceNetworkTopN.make(_).left.map(_.message))
      radius <- number(obj, "tubeRadius", path).flatMap(SurfaceNetworkRadius.make(_).left.map(_.message))
      style <- string(obj, "renderMode", path).flatMap:
        case "tube" => SurfaceNetworkStyle.tube(radius).left.map(_.message)
        case "line" => Right(SurfaceNetworkStyle.line(radius))
        case other => Left(s"$path: unsupported network render mode $other")
      scaleWidth <- optionalBoolean(obj, "tubeRadiusScale", false)
      width <-
        if !scaleWidth then Right(None)
        else SurfaceNetworkDecorationRadius.byAbsoluteWeight(radius,
          SurfaceNetworkRadius.unsafe(Double.MinPositiveValue), radius).left.map(_.toString).map(Some.apply)
      depthPolicy <- obj.fields.toMap.get("presentation") match
        case None => Right(SurfaceDecorationDepthPolicy.DepthTest)
        case Some(SceneJson.Str("signed-absolute-overlay")) => Right(SurfaceDecorationDepthPolicy.Overlay)
        case _ => Left(s"$path: unsupported network presentation")
      showNodes <- optionalBoolean(obj, "showNodes", true)
      nodeRadius <- number(obj, "nodeRadius", path).flatMap(SurfaceDecorationSize.world(_).left.map(_.toString))
      nodeColor <- number(obj, "nodeColor", path).flatMap(packedColor)
      nodes <- traverse(edges.flatMap(edge => Vector(edge._1, edge._2)).distinct): vertex =>
        SurfaceNetworkNodeId.make(s"${surface.value}/$rawId/$vertex").left.map(_.message).flatMap: id =>
          SurfaceNetworkNode.onSurface(id, surface, VertexId(vertex), geometry).left.map(_.message)
      networkEdges <- traverse(edges): (source, target, weight) =>
        SurfaceNetworkEdge.make(nodes.find(_.vertex == VertexId(source)).get.id, nodes.find(_.vertex == VertexId(target)).get.id, weight).left.map(_.message)
      network <- SurfaceNetwork.make(nodes, networkEdges).left.map(_.message)
      filter <- SurfaceNetworkFilter.make(threshold, Some(topN)).left.map(_.message)
      display <- SurfaceNetworkDisplay.compile(network, filter, style, SurfaceNetworkPalette(positive = colors(1), negative = colors(0))).left.map(_.message)
    yield NetworkInput(surface, display, nodeRadius, nodeColor, width, showNodes, depthPolicy)

  private def lowerNetwork(input: NetworkInput, plan: SurfaceCompositionPlan,
      width: Int, height: Int): Either[String, SurfaceDecorationGeometry] =
    val views = plan.views.filter(_.instances.exists(_.surface == input.surface))
    for
      _ <- check(views.nonEmpty, s"network surface '${input.surface.value}' has no display instance")
      lowered <- traverse(views): view =>
        for
          instance <- view.instances.find(_.surface == input.surface).toRight(s"network surface '${input.surface.value}' has no display instance")
          geometry <- SurfaceNetworkDecorations.lower(input.display, plan,
            SurfaceNetworkDecorationMapping(view.id, Map(input.surface -> instance.id)),
            SurfaceMarkerShape.Sphere, input.nodeSize, width, height, input.width, input.nodeColor, input.showNodes,
            input.depthPolicy).left.map(_.toString)
        yield geometry
    yield SurfaceDecorationGeometry(lowered.flatMap(_.meshes), lowered.flatMap(_.labels), width, height,
      SurfaceDecorationBinding.key(plan, width, height))

  private def parseClip(value: SceneJson): Either[String, WorldClipPlane] =
    for
      obj <- objectAt(value, "clip")
      normal <- vector(obj, "normal", "clip")
      point <- vector(obj, "point", "clip")
      flip <- optionalBoolean(obj, "flip", false)
      plane <- WorldClipPlane.make(normal(0), normal(1), normal(2), -normal.zip(point).map(_ * _).sum,
        if flip then ClipKeepSide.Negative else ClipKeepSide.Positive).left.map(_.message)
    yield plane

  private def parseLayer(value: SceneJson, surface: SurfaceId, geometry: SurfaceGeometry, topVisible: Boolean,
      timeSeconds: Double): Either[String, (SurfaceLayer, Boolean)] =
    val path = s"layer on ${surface.value}"
    for
      obj <- objectAt(value, path)
      rawId <- string(obj, "id", path)
      id <- SurfaceLayerId.make(s"${surface.value}/$rawId").left.map(_.message)
      kind <- string(obj, "kind", path)
      opacity <- optionalNumber(obj, "opacity", 1.0).flatMap(x => DisplayOpacity.make(x).left.map(_.toString))
      visible <- optionalBoolean(obj, "visible", true)
      blend <- obj.fields.toMap.get("blendMode").getOrElse(SceneJson.Str("normal")) match
        case SceneJson.Str("normal") => Right(DisplayBlendMode.Normal)
        case SceneJson.Str("additive") => Right(DisplayBlendMode.PlusLighter)
        case SceneJson.Str("multiply") => Right(DisplayBlendMode.Multiply)
        case _ => Left(s"$path: unsupported blend mode")
      result <- kind match
        case "scalar" =>
          for
            raw <- field(obj, "values", path).flatMap(array(_, path))
            values <- traverse(raw):
              case SceneJson.Str("NaN") => Right(Double.NaN)
              case SceneJson.Num(value) => value.toDoubleOption.filter(_.isFinite).toRight(s"$path: invalid scalar")
              case _ => Left(s"$path: invalid scalar")
            range <- field(obj, "range", path).flatMap(array(_, path)).flatMap(doubles(_, path))
            _ <- check(range.length == 2, "range needs two endpoints")
            window <- DisplayWindow.make(range(0), range(1)).left.map(_.toString)
            palette <- field(obj, "palette", path).flatMap(array(_, path)).flatMap(rows(_, path, 3))
            colors <- traverse(palette)(rgba)
            ramp <- ScalarRamp.discrete(colors).left.map(_.toString)
            threshold <- obj.fields.toMap.get("threshold") match
              case None => Right(Vector(0.0, 0.0))
              case Some(v) => array(v, path).flatMap(doubles(_, path))
            _ <- check(threshold.length == 2, "threshold needs two endpoints")
            visibility <- if threshold(0) == threshold(1) then Right(ScalarVisibility.All)
              else ScalarInterval.make(threshold.min, threshold.max, ScalarEndpointInclusion.Both).left.map(_.toString).map(ScalarVisibility.Outside.apply)
            mapping = ScalarMapping(ScalarScale.sequential(window, ramp), visibility = visibility)
            edges <- optionalBoolean(obj, "thresholdEdges", false)
            layer <- if edges && topVisible && visible && threshold.min < threshold.max && blend == DisplayBlendMode.Normal then
              SurfaceLayer.scalarWithThresholdEdges(id, surface, geometry, values.toArray, mapping, opacity).left.map(_.message)
            else SurfaceLayer.scalar(id, surface, geometry, values.toArray, mapping.colorizer, opacity = opacity, blendMode = blend).left.map(_.message)
          yield layer
        case "rgba" =>
          for
            values <- field(obj, "values", path).flatMap(array(_, path)).flatMap(rows(_, path, 4))
            colors <- traverse(values)(rgba)
            layer <- SurfaceLayer.packedRgba(id, surface, geometry, colors, opacity = opacity, blendMode = blend).left.map(_.message)
          yield layer
        case "volume" => parseVolume(obj, id, surface, geometry)
        case "labels" =>
          for
            labels <- field(obj, "labels", path).flatMap(array(_, path)).flatMap(ints(_, s"$path.labels"))
            definitions <- field(obj, "definitions", path).flatMap(array(_, path)).flatMap(parseDefinitions)
            default <- number(obj, "defaultColor", path).flatMap(packedColor)
            layer <- SurfaceLayer.labels(id, surface, geometry, labels.toArray, LabelColorizer(definitions, default),
              opacity = opacity, blendMode = blend).left.map(_.message)
          yield layer
        case "outline" => Left(s"$path: outlines are decorations, not layers")
        case "temporal" =>
          for
            frames <- field(obj, "frames", path).flatMap(array(_, path)).flatMap(values => traverse(values)(array(_, s"$path.frames").flatMap(doubles(_, s"$path.frames"))))
            times <- field(obj, "times", path).flatMap(array(_, path)).flatMap(doubles(_, s"$path.times"))
            _ <- check(frames.nonEmpty && frames.length == times.length, s"$path: temporal frame/times mismatch")
            _ <- check(frames.forall(_.length == geometry.vertexCount), s"$path: temporal frame length differs from vertex count")
            _ <- check(times.sliding(2).forall(pair => pair.length < 2 || pair(0) < pair(1)), s"$path: temporal times must increase")
            range <- field(obj, "range", path).flatMap(array(_, path)).flatMap(doubles(_, path))
            _ <- check(range.length == 2, "range needs two endpoints")
            palette <- field(obj, "palette", path).flatMap(array(_, path)).flatMap(rows(_, path, 3)).flatMap(values => traverse(values)(rgba))
            ramp <- ScalarRamp.discrete(palette).left.map(_.toString)
            window <- DisplayWindow.make(range(0), range(1)).left.map(_.toString)
            mapping = ScalarMapping(ScalarScale.sequential(window, ramp))
            sourceTimes <- traverse(times)(value => SurfaceSeconds.make(value).left.map(_.message))
            axis <- SurfaceTimeAxis.make(sourceTimes).left.map(_.message)
            matrix = SurfaceMatrix.fromRows(geometry, Vector.tabulate(geometry.vertexCount)(VertexId(_)), Vector.tabulate(geometry.vertexCount)(vertex => frames.map(_(vertex))))
            field <- SurfaceTemporalField.fromMatrix(SurfaceTemporalFieldId.unsafe(s"${surface.value}/${id.value}"), matrix, axis).left.map(_.message)
            prepared <- SurfacePlayback.prepareScalar(field, id, surface, SurfaceSeconds.unsafe(timeSeconds), SurfaceInterpolation.Linear,
              mapping.colorizer, SurfaceFrameCache.Disabled, opacity = opacity, blendMode = blend).left.map(_.message)
          yield prepared.layer
        case other => Left(s"unsupported qualification layer: $other")
    yield (result, visible)

  private def parseVolume(obj: SceneJson.Obj, id: SurfaceLayerId, surface: SurfaceId,
      geometry: SurfaceGeometry): Either[String, SurfaceLayer] =
    val path = s"volume ${id.value}"
    for
      dims <- field(obj, "dims", path).flatMap(array(_, path)).flatMap(ints(_, path))
      _ <- check(dims.length == 4 && dims.forall(_ > 0), s"$path: expected positive 4D dimensions")
      frame <- integer(obj, "frame", path)
      _ <- check(frame >= 0 && frame < dims(3), s"$path: frame outside volume")
      offsets <- field(obj, "depthOffsetsMm", path).flatMap(array(_, path)).flatMap(doubles(_, path))
      _ <- check(offsets == Vector(0.0), s"$path: only vertex sampling is supported by this adapter")
      reducer <- string(obj, "reducer", path)
      outside <- string(obj, "outside", path)
      tie <- string(obj, "nearestTie", path)
      order <- string(obj, "matrixOrder", path)
      _ <- check(reducer == "mean" && outside == "transparent" && tie == "floor(x + .5)" && order == "row-major", s"$path: unsupported volume contract")
      interpolation <- string(obj, "interpolation", path).flatMap:
        case "nearest" => Right(SurfaceVoxelInterpolation.Nearest)
        case "linear" => Right(SurfaceVoxelInterpolation.Linear)
        case other => Left(s"$path: unsupported interpolation $other")
      matrix <- field(obj, "voxelToWorld", path).flatMap(array(_, path)).flatMap(doubles(_, path))
      affine <- Affine.fromRowMajor[D3](matrix).left.map(_.message)
      space <- SampleSpaces.make(dims.take(3), affine = Some(affine)).left.map(_.toString)
      values <- field(obj, "voxelValues", path).flatMap(array(_, path)).flatMap(doubles(_, path))
      _ <- check(values.length.toLong == dims.take(3).map(_.toLong).product, s"$path: voxel count differs from source dimensions")
      // SurfViewJS source arrays are x-fastest; the image provider's canonical
      // ordinal is explicit and must not be inferred from a cubic specimen.
      canonical = Array.tabulate(values.length): ordinal =>
        val grid = space.indexToGrid3D(ordinal)
        values(grid(0) + dims(0) * (grid(1) + dims(1) * grid(2)))
      volume <- either(SomeScalarVolume.unsafeCopyFromCanonicalArray(canonical, space, id.value), path)
      mappingObj <- field(obj, "mapping", path).flatMap(objectAt(_, path))
      range <- field(mappingObj, "range", path).flatMap(array(_, path)).flatMap(doubles(_, path))
      _ <- check(range.length == 2, s"$path: range needs two endpoints")
      window <- DisplayWindow.make(range(0), range(1)).left.map(_.toString)
      palette <- field(mappingObj, "palette", path).flatMap(array(_, path)).flatMap(rows(_, path, 3)).flatMap(values => traverse(values)(rgba))
      ramp <- ScalarRamp.discrete(palette).left.map(_.toString)
      threshold <- field(mappingObj, "threshold", path).flatMap(array(_, path)).flatMap(doubles(_, path))
      _ <- check(threshold.length == 2 && threshold(0) == threshold(1), s"$path: volume threshold adapter is not qualified")
      opacity <- optionalNumber(mappingObj, "opacity", 1).flatMap(DisplayOpacity.make(_).left.map(_.toString))
      sampling = VolumeSurfaceSamplingPlan(SurfaceGeometryPair(geometry, geometry), SurfaceSamplingPath.White,
        SurfaceSampleAggregation.Average, interpolation)
      projected = SurfaceVolumeProjection.materialize(VolToSurfMorphism(SurfaceDomainId("qualification-volume"), SurfaceDomainId(surface.value), sampling), volume)
      layer <- SurfaceVolumeProjection.scalarLayer(projected, id, surface, geometry,
        ScalarMapping(ScalarScale.sequential(window, ramp)).colorizer, opacity).left.map(_.message)
    yield layer

  private def rgba(values: Vector[Double]): Either[String, Rgba32] =
    if values.exists(x => !x.isFinite || x < 0 || x > 1) then Left("RGBA channels must be finite in [0,1]")
    else Right(Rgba32.unsafe(math.round(values(0) * 255).toInt, math.round(values(1) * 255).toInt,
      math.round(values(2) * 255).toInt, if values.length == 4 then math.round(values(3) * 255).toInt else 255))

  private def packedColor(value: Double): Either[String, Rgba32] =
    if !value.isWhole || value < 0 || value > 0xffffff then Left("packed color must be a 24-bit integer")
    else
      val packed = value.toInt
      Right(Rgba32.unsafe((packed >>> 16) & 0xff, (packed >>> 8) & 0xff, packed & 0xff))

  private def parseDefinitions(values: Vector[SceneJson]): Either[String, Map[Int, Rgba32]] =
    traverse(values): raw =>
      for
        obj <- objectAt(raw, "label definition")
        id <- integer(obj, "id", "label definition")
        color <- number(obj, "color", "label definition").flatMap(packedColor)
      yield id -> color
    .flatMap(entries => check(entries.map(_._1).distinct.length == entries.length, "duplicate label definition").map(_ => entries.toMap))

  private def parseOutline(value: SceneJson, surface: SurfaceId, geometry: SurfaceGeometry, path: String): Either[String, SurfaceDecoration] =
    for
      obj <- objectAt(value, path)
      labels <- field(obj, "labels", path).flatMap(array(_, path)).flatMap(ints(_, s"$path.labels"))
      _ <- check(labels.length == geometry.vertexCount, s"$path: outline labels differ from vertex count")
      color <- number(obj, "color", path).flatMap(packedColor)
      width <- number(obj, "width", path).flatMap(SurfaceDecorationSize.pixels(_).left.map(_.toString))
      mode <- obj.fields.toMap.get("mode") match
        case None | Some(SceneJson.Str("side-bands")) => Right(SurfaceContourMode.SideBands)
        case Some(SceneJson.Str("rounded-strokes")) => Right(SurfaceContourMode.RoundedStrokes)
        case _ => Left(s"$path.mode: expected side-bands or rounded-strokes")
      halo <- optionalBoolean(obj, "halo", false).flatMap: enabled =>
        if !enabled then Right(None)
        else for
          haloColor <- number(obj, "haloColor", path).flatMap(packedColor)
          haloWidth <- number(obj, "haloWidth", path).flatMap(SurfaceDecorationSize.pixels(_).left.map(_.toString))
          haloOpacity <- optionalNumber(obj, "haloOpacity", 1).flatMap(DisplayOpacity.make(_).left.map(_.toString))
        yield Some(SurfaceContourHalo(Rgba32.unsafe(haloColor.red, haloColor.green, haloColor.blue,
          math.round(haloOpacity.toDouble * 255).toInt), haloWidth))
      offset <- obj.fields.toMap.get("offset") match
        case None => Right(SurfaceContourOffset.Zero)
        case Some(SceneJson.Num(value)) => value.toDoubleOption.filter(_.isFinite).toRight(s"$path.offset: expected finite number").flatMap(SurfaceContourOffset.make(_).left.map(_.toString))
        case _ => Left(s"$path.offset: expected number")
      policy = SurfaceContourPolicy.Transitions
      id <- SurfaceDecorationId.make(s"${surface.value}/boundaries").left.map(_.toString)
    yield SurfaceContourDecoration(id, surface, labels, policy, SurfaceContourStyle(width, color, halo, mode = mode), offset)

  private def parseAnnotations(value: SceneJson, surface: SurfaceId, geometry: SurfaceGeometry, path: String): Either[String, Vector[SurfaceDecoration]] =
    array(value, s"$path.annotations").flatMap: entries =>
      traverse(entries): raw =>
        for
          obj <- objectAt(raw, s"$path.annotations")
          rawId <- string(obj, "id", s"$path.annotations")
          id <- SurfaceDecorationId.make(rawId).left.map(_.toString)
          vertex <- integer(obj, "vertex", s"$path.annotations")
          _ <- check(vertex >= 0 && vertex < geometry.vertexCount, s"$path.annotations: vertex outside mesh")
          radius <- number(obj, "radius", s"$path.annotations").flatMap(SurfaceDecorationSize.world(_).left.map(_.toString))
          active <- optionalBoolean(obj, "active", false)
          color <- number(obj, if active then "colorOn" else "colorOff", s"$path.annotations").flatMap(packedColor)
        yield SurfaceMarkerDecoration(id, SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(surface, VertexId(vertex))), SurfaceMarkerShape.Sphere, radius, color)

  private def parseCrosshair(value: SceneJson, surface: SurfaceId, geometry: SurfaceGeometry, path: String): Either[String, Vector[SurfaceDecoration]] =
    for
      obj <- objectAt(value, s"$path.crosshair")
      vertex <- integer(obj, "vertex", s"$path.crosshair")
      _ <- check(vertex >= 0 && vertex < geometry.vertexCount, s"$path.crosshair: vertex outside mesh")
      options <- field(obj, "options", s"$path.crosshair").flatMap(objectAt(_, s"$path.crosshair.options"))
      size <- number(options, "size", s"$path.crosshair.options").flatMap(SurfaceDecorationSize.world(_).left.map(_.toString))
      color <- number(options, "color", s"$path.crosshair.options").flatMap(packedColor)
      id <- SurfaceDecorationId.make(s"${surface.value}/crosshair").left.map(_.toString)
      anchor = SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(surface, VertexId(vertex)))
      label <- obj.fields.toMap.get("label") match
        case None => Right(Vector.empty[SurfaceDecoration])
        case Some(SceneJson.Str(text)) => SurfaceDecorationId.make(s"${surface.value}/crosshair-label").left.map(_.toString).flatMap(labelId =>
          SurfaceLabelDecoration.make(labelId, anchor, text, size, color).left.map(_.toString).map(value => Vector(value: SurfaceDecoration)))
        case _ => Left(s"$path.crosshair.label: expected string")
    yield Vector(SurfaceMarkerDecoration(id, anchor, SurfaceMarkerShape.Crosshair, size, color)) ++ label

  private def parseMorph(value: SceneJson, base: SurfaceGeometry, path: String): Either[String, (SurfaceGeometry, SurfaceMorphFraction)] =
    for
      obj <- objectAt(value, s"$path.morph")
      vertices <- field(obj, "vertices", s"$path.morph").flatMap(array(_, s"$path.morph.vertices")).flatMap(rows(_, s"$path.morph.vertices", 3))
      _ <- check(vertices.length == base.vertexCount, s"$path.morph: vertex count differs from source")
      fraction <- number(obj, "weight", s"$path.morph").flatMap(SurfaceMorphFraction.make(_).left.map(_.message))
      variant = SurfaceGeometry(TriangleMesh.fromRows(vertices, (0 until base.faceCount).map(index => (base.mesh.faceIndices(index * 3), base.mesh.faceIndices(index * 3 + 1), base.mesh.faceIndices(index * 3 + 2)))), base.hemisphere, SurfaceKind.Custom("qualification-morph"), base.surfaceToWorld)
    yield variant -> fraction

  private def optionalNumber(obj: SceneJson.Obj, name: String, default: Double): Either[String, Double] =
    if obj.fields.toMap.contains(name) then number(obj, name, "optional") else Right(default)

  private def optionalBoolean(obj: SceneJson.Obj, name: String, default: Boolean): Either[String, Boolean] =
    obj.fields.toMap.get(name) match
      case None => Right(default)
      case Some(SceneJson.Bool(value)) => Right(value)
      case _ => Left(s"$name must be boolean")

  private def viewerState(model: SurfaceViewerModel, surfaces: Vector[InputSurface], lighting: SurfaceLighting): Either[String, SurfaceViewerState] =
    val initial = SurfaceViewerState.initial(model)
    val layout = SurfaceLayout.Single(surfaces.head.id)
    val configured = surfaces.foldLeft[Either[SurfaceViewError, SurfaceViewerState]](Right(initial)): (current, surface) =>
      for
        state <- current
        morphed <- surface.morph.fold[Either[SurfaceViewError, SurfaceViewerState]](Right(state)): fraction =>
          for
            started <- SurfaceViewer.reduce(model, state, SurfaceViewerAction.BeginGeometryMorph(surface.id, SurfaceKind.Custom("qualification-morph")))
            next <- SurfaceViewer.reduce(model, started, SurfaceViewerAction.SetGeometryMorphFraction(surface.id, fraction))
          yield next
      yield morphed
    configured.flatMap: state =>
      for
        laidOut <- SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetLayout(layout))
        timed <- SurfaceViewer.reduce(model, laidOut, SurfaceViewerAction.SetTimepoint(surfaces.map(_.timepoint).max))
        lit <- SurfaceViewer.reduce(model, timed, SurfaceViewerAction.SetLighting(lighting))
      yield lit
    .left.map(_.message)

  private def composition(model: SurfaceViewerModel, state: SurfaceViewerState, surfaces: Vector[InputSurface],
      camera: SurfaceCameraPacket, declaredViews: Vector[InputView]): Either[String, SurfaceCompositionPlan] =
    for
      _ <- check(surfaces.map(_.clip).distinct.length == 1, "one shared view requires the same clipping plane for every surface")
      clipping = surfaces.head.clip.fold[SurfaceClipping](SurfaceClipping.Disabled)(plane => SurfaceClipping.WorldPlanes(Vector(plane)))
      compiled <- traverse(surfaces): surface =>
        SurfaceCompiler.compile(model, state.copy(layout = SurfaceLayout.Single(surface.id))).left.map(_.message)
          .flatMap(plan => SurfaceCompositionPlan.fromLegacy(plan).left.map(_.toString))
      resources = SurfaceCompositionResources(compiled.flatMap(_.resources.meshes).toMap, compiled.flatMap(_.resources.layers).toMap)
      defaultView <- SurfaceViewId.make("qualification").left.map(_.toString)
      inputs = if declaredViews.isEmpty then Vector(InputView(defaultView, SurfaceViewport(0, 0, 1, 1), camera, surfaces.map(_.id))) else declaredViews
      views <- traverse(inputs): input =>
        for
          instances <- traverse(input.surfaces.zipWithIndex): (surfaceId, occurrence) =>
            surfaces.find(_.id == surfaceId).toRight(s"missing source surface ${surfaceId.value}").flatMap: surface =>
              resources.meshes.values.find(_.surface == surface.id).toRight(s"missing mesh for ${surface.id.value}").flatMap: mesh =>
                val keys = state.layerOrder.flatMap(id => resources.layers.values.find(p => p.layer == id && p.surface == surface.id).map(_.resourceKey))
                SurfaceInstanceId.make(s"${input.id.value}/$occurrence-${surface.id.value}").left.map(_.toString).map: id =>
                  SurfaceCompositionInstance(id, surface.id, mesh.resourceKey, keys, opacity = surface.opacity)
        yield SurfaceCompositionView(input.id, input.viewport, input.camera, instances, state.lighting, clipping,
          fragmentSurfaces = compiled.flatMap(_.views).flatMap(_.fragmentSurfaces).toSet.intersect(input.surfaces.toSet),
          colorPipeline = state.colorPipeline, surfaceBase = state.surfaceBase)
      result <- SurfaceCompositionPlan.make(views, resources).left.map(_.toString)
    yield result

  private def parseCamera(value: SceneJson): Either[String, SurfaceCameraPacket] =
    parseCamera(value, "$.camera")

  private def parseCamera(value: SceneJson, path: String): Either[String, SurfaceCameraPacket] =
    for
      obj <- objectAt(value, path)
      projection <- string(obj, "projection", path)
      eye <- vector(obj, "eye", path)
      target <- vector(obj, "target", path)
      up <- vector(obj, "up", path)
      frame = Frame.named[D3]("qualification camera").toOption.get
      targetPoint <- SurfaceCameraPose.pointIn[frame.type](frame, target).left.map(_.message)
      eyePoint <- SurfaceCameraPose.pointIn[frame.type](frame, eye).left.map(_.message)
      pose <- SurfaceCameraPose.make(targetPoint, eyePoint).left.map(_.message)
      upVector <- Vec.fromVector[D3](frame, up).left.map(_.message)
      frustum <- projection match
        case "perspective" =>
          for
            fov <- number(obj, "fovDegrees", path); aspect <- number(obj, "aspect", path)
            near <- number(obj, "near", path); far <- number(obj, "far", path)
          yield SurfaceCameraFrustum.Perspective(fov, aspect, near, far)
        case "orthographic" =>
          for
            left <- number(obj, "left", path); right <- number(obj, "right", path)
            bottom <- number(obj, "bottom", path); top <- number(obj, "top", path)
            near <- number(obj, "near", path); far <- number(obj, "far", path)
          yield SurfaceCameraFrustum.Orthographic(left, right, bottom, top, near, far)
        case other => Left(s"$path.projection: unsupported $other")
      packet <- SurfaceCameraPacket.lookAt(pose, upVector, frustum).left.map(_.toString)
    yield packet

  private def parseProfile(value: SceneJson): Either[String, SurfaceLighting] =
    for
      obj <- objectAt(value, "$.profile")
      name <- string(obj, "name", "$.profile")
      result <- name match
        case "controlled-unlit" => Right(SurfaceLighting.Unlit)
        case "controlled-lit" =>
          for
            ambient <- number(obj, "ambientIntensity", "$.profile"); directional <- number(obj, "directionalIntensity", "$.profile")
            direction <- vector(obj, "direction", "$.profile")
            value <- SurfaceLighting.directional(ambient / math.Pi, directional / math.Pi, direction(0), direction(1), direction(2)).left.map(_.message)
          yield value
        case "observed-default" => Right(SurfaceLighting.Default)
        case other => Left(s"$$.profile.name: unsupported $other")
    yield result

  private def objectAt(value: SceneJson, path: String): Either[String, SceneJson.Obj] = value match
    case value: SceneJson.Obj => Right(value)
    case _ => Left(s"$path: expected object")
  private def field(obj: SceneJson.Obj, name: String, path: String): Either[String, SceneJson] = obj.fields.find(_._1 == name).map(_._2).toRight(s"$path.$name: missing required field")
  private def string(obj: SceneJson.Obj, name: String, path: String): Either[String, String] = field(obj,name,path).flatMap { case SceneJson.Str(value) => Right(value); case _ => Left(s"$path.$name: expected string") }
  private def number(obj: SceneJson.Obj, name: String, path: String): Either[String, Double] = field(obj,name,path).flatMap { case SceneJson.Num(value) => value.toDoubleOption.filter(_.isFinite).toRight(s"$path.$name: expected finite number"); case _ => Left(s"$path.$name: expected number") }
  private def integer(obj: SceneJson.Obj, name: String, path: String): Either[String, Int] = field(obj,name,path).flatMap { case SceneJson.Num(value) => value.toIntOption.toRight(s"$path.$name: expected integer"); case _ => Left(s"$path.$name: expected integer") }
  private def array(value: SceneJson, path: String): Either[String, Vector[SceneJson]] = value match { case SceneJson.Arr(values) => Right(values); case _ => Left(s"$path: expected array") }
  private def vector(obj: SceneJson.Obj, name: String, path: String): Either[String, Vector[Double]] = field(obj,name,path).flatMap(array(_,s"$path.$name")).flatMap(values => doubles(values,s"$path.$name")).flatMap(v => check(v.length == 3,s"$path.$name: expected three numbers").map(_ => v))
  private def doubles(values: Vector[SceneJson], path: String): Either[String, Vector[Double]] = traverse(values.zipWithIndex)((value,index) => value match { case SceneJson.Num(raw) => raw.toDoubleOption.filter(_.isFinite).toRight(s"$path[$index]: expected finite number"); case _ => Left(s"$path[$index]: expected number") })
  private def ints(values: Vector[SceneJson], path: String): Either[String, Vector[Int]] = traverse(values.zipWithIndex)((value,index) => value match { case SceneJson.Num(raw) => raw.toIntOption.toRight(s"$path[$index]: expected integer"); case _ => Left(s"$path[$index]: expected integer") })
  private def rows(values: Vector[SceneJson], path: String, width: Int): Either[String, Vector[Vector[Double]]] = traverse(values.zipWithIndex)((value,index) => array(value,s"$path[$index]").flatMap(v => doubles(v,s"$path[$index]")).flatMap(row => check(row.length == width,s"$path[$index]: expected $width values").map(_ => row)))
  private def intRows(values: Vector[SceneJson], path: String, width: Int): Either[String, Vector[Vector[Int]]] = traverse(values.zipWithIndex)((value,index) => array(value,s"$path[$index]").flatMap(v => traverse(v.zipWithIndex)((n,j) => n match { case SceneJson.Num(raw) => raw.toIntOption.toRight(s"$path[$index][$j]: expected integer"); case _ => Left(s"$path[$index][$j]: expected integer") })).flatMap(row => check(row.length == width,s"$path[$index]: expected $width values").map(_ => row)))
  private def traverse[A,B](values: Vector[A])(f: A => Either[String,B]): Either[String,Vector[B]] = values.foldLeft[Either[String,Vector[B]]](Right(Vector.empty))((result,value) => for before <- result; next <- f(value) yield before :+ next)
  private def check(value: Boolean, error: String): Either[String,Unit] = Either.cond(value, (), error)
  private def either[A](value: => A, context: String): Either[String,A] = try Right(value) catch case error: IllegalArgumentException => Left(s"$context: ${error.getMessage}")
