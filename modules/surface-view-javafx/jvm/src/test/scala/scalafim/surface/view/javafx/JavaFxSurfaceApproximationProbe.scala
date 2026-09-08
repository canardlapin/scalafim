package scalafim.surface.view.javafx

import java.util.concurrent.{CountDownLatch, TimeUnit}
import javafx.application.Platform
import javafx.scene.{Group, Scene, SceneAntialiasing, SubScene}
import javafx.scene.image.WritableImage
import javafx.stage.Stage
import intaglio.*
import scalafim.surface.view.*
import scalafim.surface.{SurfaceGeometry, SurfaceKind, SurfaceSet, SurfaceFaceField, VertexId, TriangleMesh as ScientificMesh}
import scalafim.surface.view.raster.*

/** Native check of certified constant-color subdivision with palette-center UVs. */
object JavaFxSurfaceApproximationProbe:
  private val base = Rgba32.unsafe(184, 184, 184)

  def main(args: Array[String]): Unit =
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup(() => ())
    Platform.runLater: () =>
      Platform.setImplicitExit(false)
      val stage = new Stage()
      val rows = Vector.newBuilder[String]
      val builds = Vector.newBuilder[String]
      var overBudgetCases = 0
      val hosts = scala.collection.mutable.Map.empty[(Boolean, String), (JavaFxSurfaceBackend, SubScene)]
      try
        for aa <- Vector(false, true); perspective <- Vector(false, true); size <- Vector(64, 128, 256); mode <- Vector("layered", "lit"); width <- Vector(1) do
          val mapping = SurfaceScalarFixture.thresholded
          val scalarModel = SurfaceScalarFixture.model(mapping)
          val under = SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("curvature"), SurfaceScalarFixture.Surface,
            SurfaceFaceFixture.geometry, Array(0.0, 0.0, 1.0, 1.0), ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(0, 1),
              ScalarRamp.linear(Rgba32.unsafe(40, 40, 40), Rgba32.unsafe(180, 180, 180))))).toOption.get
          val over = SurfaceLayer.interpolatedScalar(SurfaceScalarFixture.Layer, SurfaceScalarFixture.Surface,
            SurfaceFaceFixture.geometry, SurfaceScalarFixture.Values.toArray, mapping, opacity = DisplayOpacity.unsafe(0.7)).toOption.get
          val model = SurfaceViewerModel.make(scalarModel.surfaces, Vector(under, over)).toOption.get
          val initial = SurfaceFaceFixture.state(model)
          val state = if !perspective then initial else
            val projected = SurfaceViewer.reduce(model, initial, SurfaceViewerAction.SetProjection(
              CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50)))).toOption.get
            SurfaceViewer.reduce(model, projected, SurfaceViewerAction.OrbitBy(30, 20)).toOption.get
          val compiled = SurfaceCompiler.compile(model, state).toOption.get
          val plan = if mode != "lit" then compiled else compiled.copy(
            meshes = compiled.meshes.map(_.copy(normals = new FloatBufferView(Array[Float](
              -0.6f, 0, 0.8f, 0.6f, 0, 0.8f, 0, 0.6f, 0.8f, 0, -0.6f, 0.8f)))),
            lighting = SurfaceLighting.directional(0.2, 0.8, 0.4, 0.3, 0.85).toOption.get)
          val reference = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(size, size),
            SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
          val packet = plan.meshes.head
          val hostKey = (aa, mode)
          val (backend, scene) = hosts.get(hostKey) match
            case Some(host) => host
            case None =>
              val backend = JavaFxSurfaceBackend.createApproximate(JavaFxApproximationConfig.make().toOption.get).toOption.get
              val cold = backend.render(plan).fold(e => throw new IllegalArgumentException(e.message), identity)
              val costs = cold.approximation.get
              builds += s"{\"mode\":\"$mode\",\"antialiasing\":$aa,\"preparationNanos\":${costs.preparationNanos},\"renderNanos\":${cold.elapsedNanos},\"derivedFaces\":${costs.derivedFaces},\"generatedBytes\":${costs.generatedBytes}}"
              val scene = backend.newSubScene(JavaFxSnapshotConfig.make(size, size,
                if aa then SceneAntialiasing.BALANCED else SceneAntialiasing.DISABLED).toOption.get).toOption.get
              hosts(hostKey) = (backend, scene)
              (backend, scene)
          val update = backend.render(plan).fold(e => throw new IllegalArgumentException(e.message), identity)
          require(!update.dirty.geometry && !update.dirty.layerData, "camera or unchanged scene rebuilt approximation")
          val approximation = update.approximation.get
          require(approximation.reused && approximation.preparationNanos == 0)
          val textureBytes = approximation.textureBytes
          scene.setWidth(size)
          scene.setHeight(size)
          locally:
            stage.setScene(new Scene(new Group(scene), size, size))
            stage.show()
            val image = new WritableImage(size, size)
            scene.snapshot(null, image)
            if !perspective && size == 64 && mode == "layered" then
              val controller = JavaFxSurfaceController.attach(model, state, backend, scene).toOption.get
              try
                val chunk = backend.chunks.head
                val mesh = backend.pickingPlan.get.meshes.head
                val face = mesh.indices.length / 6
                val offset = face * 3
                val ids = Vector(mesh.indices(offset), mesh.indices(offset + 1), mesh.indices(offset + 2))
                val xyz = Vector.tabulate(3)(axis => ids.map(v => mesh.positions(v * 3 + axis).toDouble).sum / 3)
                val pick = controller.pick(new _root_.javafx.scene.input.PickResult(chunk.view,
                  new _root_.javafx.geometry.Point3D(xyz(0), xyz(1), xyz(2)), 4, face, _root_.javafx.geometry.Point2D.ZERO)).toOption.get
                val expected = mesh.sourceBarycentric(face, 1.0/3, 1.0/3, 1.0/3)
                require(pick.face.index == mesh.sourceFace(face))
                require(math.abs(pick.barycentricA - expected._1) < 1e-6 && math.abs(pick.barycentricB - expected._2) < 1e-6)
                require(pick.vertex.index == mesh.pickedVertex(face, 1.0/3, 1.0/3, 1.0/3))
              finally controller.dispose()
            var checked = 0
            var away = 0
            var maxError = 0
            var awayError = 0
            var bad = 0
            val boundaries = SurfaceMappingPartition.boundaries(mapping)
            def sample(x: Int, y: Int): Option[Double] =
              if x < 0 || y < 0 || x >= size || y >= size then None
              else reference.pick(x, y).toOption.flatten.map: pick =>
                val (a, b, c) = packet.sourceFaceVertices(pick.face)
                pick.barycentricA * SurfaceScalarFixture.Values(a) + pick.barycentricB * SurfaceScalarFixture.Values(b) + pick.barycentricC * SurfaceScalarFixture.Values(c)
            for y <- 0 until size; x <- 0 until size; pick <- reference.pick(x, y).toOption.flatten do
              if math.min(pick.barycentricA, math.min(pick.barycentricB, pick.barycentricC)) > 0.04 then
                val value = sample(x, y).get
                val actual = image.getPixelReader.getArgb(x, y)
                val expected = reference.image.pixelUnsafe(x, y)
                val error = Vector(math.abs(((actual >>> 16) & 255) - expected.red),
                  math.abs(((actual >>> 8) & 255) - expected.green), math.abs((actual & 255) - expected.blue)).max
                checked += 1
                maxError = math.max(maxError, error)
                if error > 2 then bad += 1
                val dx = sample(x + 1, y).fold(0.0)(v => math.abs(v - value))
                val dy = sample(x, y + 1).fold(0.0)(v => math.abs(v - value))
                val exclusion = 2.0 * math.sqrt(dx * dx + dy * dy)
                if boundaries.forall(b => math.abs(value - b) > exclusion) then
                  away += 1
                  awayError = math.max(awayError, error)
            require(checked > 100 && away > 100, s"insufficient coverage: $checked/$away")
            if awayError > 2 then overBudgetCases += 1
            val row = s"{\"mode\":\"$mode\",\"antialiasing\":$aa,\"perspective\":$perspective,\"size\":$size,\"certifiedChannelError\":${approximation.maximumChannelError},\"triangles\":${approximation.derivedFaces},\"textureBytes\":${textureBytes},\"checked\":$checked,\"awayChecked\":$away,\"maximumError\":$maxError,\"awayMaximumError\":$awayError,\"pixelsOverBudget\":$bad,\"awayBudgetPassed\":${awayError <= 2}}"
            rows += row
            println(s"approximation_probe=$row")
            if aa && perspective && size == 256 && mode == "lit" && width == 1 then
              val out = new java.awt.image.BufferedImage(size, size, java.awt.image.BufferedImage.TYPE_INT_ARGB)
              for y <- 0 until size; x <- 0 until size do out.setRGB(x, y, image.getPixelReader.getArgb(x, y))
              javax.imageio.ImageIO.write(out, "png", new java.io.File("/private/tmp/scalafim-approximation-javafx.png"))
        java.nio.file.Files.writeString(java.nio.file.Path.of("/private/tmp/scalafim-approximation-builds.json"), builds.result().mkString("[\n", ",\n", "\n]\n"))
        val updates = verifyUpdates()
        val network = verifyNetwork(stage)
        java.nio.file.Files.writeString(java.nio.file.Path.of("/private/tmp/scalafim-approximation-network.json"), network)
        java.nio.file.Files.writeString(java.nio.file.Path.of("/private/tmp/scalafim-approximation-updates.json"), updates)
        java.nio.file.Files.writeString(java.nio.file.Path.of("/private/tmp/scalafim-approximation-javafx.json"), rows.result().mkString("[\n", ",\n", "\n]\n"))
        require(overBudgetCases == 0, s"$overBudgetCases native cases exceeded the interior color budget")
      catch case error: Throwable => failure = error
      finally
        hosts.values.foreach(_._1.dispose())
        stage.close()
        done.countDown()
    try
      require(done.await(180, TimeUnit.SECONDS), "JavaFX partition probe timed out")
      if failure != null then throw failure.nn
    finally Platform.exit()


  private def verifyNetwork(stage: Stage): String =
    val surface = SurfaceScalarFixture.Surface
    val geometry = SurfaceFaceFixture.geometry
    val mapping = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(0, 1),
      ScalarRamp.linear(Rgba32.unsafe(100, 100, 100), Rgba32.unsafe(100, 100, 100))),
      invalid = Rgba32.unsafe(255, 0, 255))
    val scalar = SurfaceLayer.interpolatedScalar(SurfaceScalarFixture.Layer, surface, geometry, Array.fill(4)(0.5), mapping).toOption.get
    val nearest = SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("nearest"), surface, geometry,
      Vector.fill(4)(Rgba32.unsafe(40, 40, 40)), interpolation = SurfaceVertexInterpolation.NearestSample).toOption.get
    val faces = SurfaceLayer.facePackedRgba(SurfaceLayerId.unsafe("faces"), surface,
      SurfaceFaceField.make(geometry, Array.fill(2)(Rgba32.unsafe(40, 40, 40))).toOption.get)
    val nodes = Vector(0, 2).map(v => SurfaceNetworkNode.onSurface(SurfaceNetworkNodeId.unsafe(s"v$v"),
      surface, VertexId(v), geometry).toOption.get)
    val network = SurfaceNetwork.make(nodes, Vector(SurfaceNetworkEdge.unsafe(nodes(0).id, nodes(1).id, 1))).toOption.get
    val display = SurfaceNetworkDisplay.compile(network, SurfaceNetworkFilter.All,
      SurfaceNetworkStyle.tube(SurfaceNetworkRadius.unsafe(0.1), sides = 8).toOption.get).toOption.get
    val rows = Vector.newBuilder[String]
    for (mode, extra) <- Vector("scalar" -> Vector.empty, "face" -> Vector(faces), "nearest" -> Vector(nearest)); lit <- Vector(false, true) do
      val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), extra :+ scalar).toOption.get
      val compiled = SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get
      val attached = SurfaceNetworkCompiler.attach(compiled, surface, SurfaceLayerId.unsafe("network"), display).toOption.get.plan
      val plan = if !lit then attached else attached.copy(lighting = SurfaceLighting.directional(0.4, 0.6, 0, 0, 1).toOption.get)
      val backend = JavaFxSurfaceBackend.createApproximate(JavaFxApproximationConfig.make(maxChannelError = 2).toOption.get).toOption.get
      try
        val receipt = backend.render(plan).fold(e => throw new IllegalArgumentException(e.message), identity)
        val scene = backend.newSubScene(JavaFxSnapshotConfig.make(128, 128, SceneAntialiasing.DISABLED).toOption.get).toOption.get
        stage.setScene(new Scene(new Group(scene), 128, 128))
        stage.show()
        val actual = scene.snapshot(null, new WritableImage(128, 128))
        val expected = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(128, 128), SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
        var checked = 0
        var networkPixels = 0
        var maximumError = 0
        for y <- 5 until 123; x <- 5 until 123; pick <- expected.pick(x, y).toOption.flatten do
          if math.min(pick.barycentricA, math.min(pick.barycentricB, pick.barycentricC)) > 0.1 then
            val pixel = actual.getPixelReader.getArgb(x, y)
            val color = expected.image.pixelUnsafe(x, y)
            val error = Vector(math.abs(((pixel >>> 16) & 255) - color.red),
              math.abs(((pixel >>> 8) & 255) - color.green), math.abs((pixel & 255) - color.blue)).max
            maximumError = math.max(maximumError, error)
            checked += 1
            if pick.face >= geometry.faceCount then networkPixels += 1
        require(checked > 1000 && networkPixels > 20, s"insufficient $mode network coverage: $checked/$networkPixels")
        require(maximumError <= 3, s"$mode network lit=$lit error $maximumError exceeds 3")
        val row = s"{\"mode\":\"$mode\",\"lit\":$lit,\"checked\":$checked,\"networkPixels\":$networkPixels,\"maximumError\":$maximumError,\"derivedFaces\":${receipt.approximation.get.derivedFaces}}"
        println(s"network_probe=$row")
        rows += row
      finally backend.dispose()
    rows.result().mkString("[\n", ",\n", "\n]\n")

  private def verifyUpdates(): String =
    val geometry = SurfaceFaceFixture.geometry
    val white = SurfaceGeometry(geometry.mesh, geometry.hemisphere, SurfaceKind.White, geometry.surfaceToWorld)
    val pial = SurfaceGeometry(ScientificMesh.fromRows(
      Seq(Seq(-1.0, -1.0, 0.1), Seq(1.0, -1.0, 0.4), Seq(1.0, 1.0, 0.2), Seq(-1.0, 1.0, 0.1)),
      Seq((0, 1, 2), (0, 2, 3))), white.hemisphere, SurfaceKind.Pial)
    val asset = SurfaceAsset.make(SurfaceScalarFixture.Surface, SurfaceSet.of(SurfaceKind.White, white, SurfaceKind.Pial -> pial)).toOption.get
    val field = SurfaceLayer.interpolatedScalar(SurfaceScalarFixture.Layer, SurfaceScalarFixture.Surface, white,
      Array(-2.0, 4.0, 4.0, -2.0, -1.0, 3.0, 3.0, -1.0), SurfaceScalarFixture.thresholded, frameCount = 2).toOption.get
    val model = SurfaceViewerModel.make(Vector(asset), Vector(field)).toOption.get
    var state = SurfaceFaceFixture.state(model)
    val settings = JavaFxApproximationConfig.make(maxChannelError = 8, maxTriangles = 100000).toOption.get
    val backend = JavaFxSurfaceBackend.createApproximate(settings, JavaFxAtlasConfig.make(maxTextureSize = 64).toOption.get).toOption.get
    val rows = Vector.newBuilder[String]
    try
      val initial = SurfaceCompiler.compile(model, state).toOption.get
      backend.render(initial).toOption.get
      require(backend.chunks.length > 1, "small texture budget did not exercise chunk-local geometry")
      val scene = backend.newSubScene(JavaFxSnapshotConfig.make(96, 96).toOption.get).toOption.get
      val actions = Vector(
        "timepoint" -> Vector(SurfaceViewerAction.SetTimepoint(1)),
        "threshold" -> Vector(SurfaceViewerAction.SetLayerThreshold(SurfaceScalarFixture.Layer, DisplayThreshold.transparentBand(-0.5, 0.5).toOption.get)),
        "morph" -> Vector(SurfaceViewerAction.BeginGeometryMorph(SurfaceScalarFixture.Surface, SurfaceKind.Pial),
          SurfaceViewerAction.SetGeometryMorphFraction(SurfaceScalarFixture.Surface, SurfaceMorphFraction.unsafe(0.5)))
      )
      actions.foreach: (label, changes) =>
        changes.foreach(action => state = SurfaceViewer.reduce(model, state, action).toOption.get)
        val plan = SurfaceCompiler.compile(model, state).toOption.get
        val observed = backend.renderObserved(plan, SurfaceAdmissionPath.DerivedGeometryUpdate).toOption.get
        require(observed.native.dirty.geometry && observed.observation.geometryUploads > 0)
        require(SurfaceBackendAdmission.validate(observed.observation).isEmpty)
        require(SurfaceBackendAdmission.validate(observed.observation.copy(path = SurfaceAdmissionPath.StyleUpdate)).nonEmpty)
        val controller = JavaFxSurfaceController.attach(model, state, backend, scene).toOption.get
        try
          val chunk = backend.chunks.last
          val packet = backend.pickingPlan.get.meshes.head
          val localFace = chunk.faceCount / 2
          val face = chunk.faceStart + localFace
          val ids = Vector.tabulate(3)(i => packet.indices(face * 3 + i))
          val point = Vector.tabulate(3)(axis => ids.map(v => packet.positions(v * 3 + axis).toDouble).sum / 3)
          val picked = controller.pick(new _root_.javafx.scene.input.PickResult(chunk.view,
            new _root_.javafx.geometry.Point3D(point(0), point(1), point(2)), 4, localFace, _root_.javafx.geometry.Point2D.ZERO)).toOption.get
          require(picked.face.index == packet.sourceFace(face))
          require(picked.vertex.index == packet.pickedVertex(face, 1.0/3, 1.0/3, 1.0/3))
          require(chunk.mesh.getPoints.size() == chunk.faceCount * 9)
        finally controller.dispose()
        rows += s"{\"update\":\"$label\",\"geometryUploads\":${observed.observation.geometryUploads},\"uploadedBytes\":${observed.observation.uploadedBytes},\"derivedFaces\":${observed.native.approximation.get.derivedFaces},\"pickPassed\":true}"
    finally backend.dispose()
    require(backend.resourceKeys.isEmpty && backend.chunks.isEmpty && backend.root.getChildren.isEmpty)
    val limited = JavaFxSurfaceBackend.createApproximate(JavaFxApproximationConfig.make(maxChannelError = 8, maxDepth = 0).toOption.get).toOption.get
    val preservationStage = new Stage()
    try
      val constant = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1, 1),
        ScalarRamp.linear(Rgba32.unsafe(100, 100, 100), Rgba32.unsafe(100, 100, 100))))
      limited.render(SurfaceScalarFixture.plan(constant)).toOption.get
      val scene = limited.newSubScene(JavaFxSnapshotConfig.make(64, 64).toOption.get).toOption.get
      preservationStage.setScene(new Scene(new Group(scene), 64, 64))
      preservationStage.show()
      val before = scene.snapshot(null, new WritableImage(64, 64))
      val center = before.getPixelReader.getArgb(32, 32)
      require((0 until 3).forall(channel => math.abs(((center >>> (channel * 8)) & 255) - 100) <= 1),
        s"preservation fixture did not render its gray surface: ${center.toHexString}")
      val keys = limited.resourceKeys
      require(limited.render(SurfaceScalarFixture.plan(SurfaceScalarFixture.thresholded)).isLeft)
      require(limited.resourceKeys == keys)
      val preserved = scene.snapshot(null, new WritableImage(64, 64))
      require((0 until 64).forall(y => (0 until 64).forall(x =>
        preserved.getPixelReader.getArgb(x, y) == before.getPixelReader.getArgb(x, y))),
        "failed preparation changed the displayed image")
    finally
      limited.dispose()
      preservationStage.close()
    rows.result().mkString("[\n", ",\n", "\n]\n")
