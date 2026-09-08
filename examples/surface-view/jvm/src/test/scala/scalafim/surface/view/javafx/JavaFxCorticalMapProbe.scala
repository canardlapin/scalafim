package scalafim.surface.view.javafx

import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.concurrent.{CountDownLatch, TimeUnit}
import javafx.application.Platform
import javafx.scene.{Group, Scene, SceneAntialiasing, SubScene}
import javafx.scene.image.WritableImage
import javafx.scene.input.PickResult
import javafx.stage.Stage
import intaglio.*
import intaglio.svg.*
import scalafim.surface.*
import scalafim.surface.io.{FreeSurferSurfaceReader, FreeSurferMorphometryReader}
import scalafim.surface.view.*
import scalafim.surface.view.raster.*
import scalafim.examples.surfaceview.*

/** Opt-in native acceptance using external checksum-pinned fsaverage6 data. */
object JavaFxCorticalMapProbe:
  private val hashes = Map(
    "lh.pial" -> "e31e4b615abcad1bc5c6b69401bbfcbc1cc00a484257ae9e7c3df080cc68654c",
    "lh.white" -> "9e927bc7ed863e0e4d01035616456e06b170847dfe7b4fc1e46628ed030e598b",
    "lh.inflated" -> "b38f14b0b073df96a9c9daf3de41d4d8f5b32d7c7148dad36e21c44e552dcab8",
    "lh.sulc" -> "8e684b1405e86b6d3fa05e4f26628cb6c2b1080b95982faf24caa7345cfdf7a5")

  def main(args: Array[String]): Unit =
    val corpus = Path.of(args.headOption.getOrElse("/Users/bbuchsbaum/code/neuroatlas/data-raw/fsaverage6"))
    val output = Path.of(args.lift(1).getOrElse("/private/tmp/scalafim-cortical-semantics"))
    val modes = args.lift(2).fold(Vector(CorticalMapMode.Layered))(s => s.split(",").toVector.map(CorticalMapMode.valueOf))
    val triangleBudget = args.lift(3).fold(2000000)(_.toInt)
    Files.createDirectories(output)
    hashes.foreach: (file, expected) =>
      val actual = MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(corpus.resolve(file)))
        .map(b => f"${b & 255}%02x").mkString
      require(actual == expected, s"cortical corpus hash mismatch: $file")
    val pial = FreeSurferSurfaceReader.read(corpus.resolve("lh.pial"))
    val white = FreeSurferSurfaceReader.read(corpus.resolve("lh.white"))
    val inflated = FreeSurferSurfaceReader.read(corpus.resolve("lh.inflated"))
    require(pial.vertexCount == 40962 && pial.faceCount == 81920)
    val surfaces = SurfaceSet.of(SurfaceKind.Pial, pial, SurfaceKind.White -> white, SurfaceKind.Inflated -> inflated)
    val folding = FreeSurferMorphometryReader.read(corpus.resolve("lh.sulc"), pial, "sulcal depth")
    if args.lift(4).contains("publication-only") then
      modes.foreach(mode => exportPublication(CorticalMapSemantics.build(surfaces, folding, mode), mode, output))
      return
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup(() => ())
    Platform.runLater: () =>
      Platform.setImplicitExit(false)
      val stage = new Stage()
      val rows = scala.collection.mutable.ArrayBuffer.empty[String]
      try
        for mode <- modes do
          val example = CorticalMapSemantics.build(surfaces, folding, mode)
          exportPublication(example, mode, output)
          val backend = JavaFxSurfaceBackend.createApproximate(JavaFxApproximationConfig.make(maxChannelError = 4,
            maxTriangles = triangleBudget, maxGeneratedBytes = 1024L * 1024 * 1024).toOption.get).toOption.get
          try
            println(s"cortical_preparing=$mode vertices=${pial.vertexCount} faces=${pial.faceCount}")
            val started = System.nanoTime()
            val receipt = backend.render(example.plan).fold(e => throw new AssertionError(e.message), identity)
            val renderNanos = System.nanoTime() - started
            val scene = backend.newSubScene(JavaFxSnapshotConfig.make(768, 768, SceneAntialiasing.DISABLED).toOption.get).toOption.get
            stage.setScene(new Scene(new Group(scene), 768, 768))
            stage.show()
            val controller = JavaFxSurfaceController.attach(example.model, example.state, backend, scene).toOption.get
            try
              if args.lift(4).contains("updates") then
                verifyUpdates(mode, controller, backend, scene, output, triangleBudget, rows)
              else
                val configurations = for
                  perspective <- Vector(false, true)
                  lit <- Vector(false, true)
                  size <- Vector(384, 768, 1024)
                yield (perspective, lit, size)
                for (perspective, lit, size) <- configurations if args.lift(5).forall(prefix => s"${if perspective then "perspective" else "ortho"}-${if lit then "lit" else "unlit"}-$size".startsWith(prefix)) do
                  val projection = if perspective then CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50))
                    else CameraProjection.Orthographic(OrthographicScale.unsafe(100))
                  controller.dispatch(SurfaceViewerAction.SetProjection(projection)).toOption.get
                  controller.dispatch(SurfaceViewerAction.FitCamera).toOption.get
                  val light = if lit then SurfaceLighting.directional(0.4, 0.6, -1, -0.2, 0.3).toOption.get else SurfaceLighting.Unlit
                  val updated = controller.dispatch(SurfaceViewerAction.SetLighting(light))
                    .fold(e => throw new AssertionError(e.message), identity)
                  scene.setWidth(size)
                  scene.setHeight(size)
                  stage.setWidth(size + 40)
                  stage.setHeight(size + 60)
                  val label = s"$mode-${if perspective then "perspective" else "ortho"}-${if lit then "lit" else "unlit"}-$size"
                  val metrics = verify(label, controller, backend, scene, output)
                  val approximation = updated.approximation
                  val nativeMeshBytes = backend.chunks.map(c => (c.mesh.getPoints.size().toLong + c.mesh.getNormals.size() +
                    c.mesh.getTexCoords.size() + c.mesh.getFaces.size()) * 4).sum
                  val atlasBytes = backend.chunks.map(c => c.atlas.width.toLong * c.atlas.height * 4).sum
                  val row = s"{\"mode\":\"$mode\",\"perspective\":$perspective,\"lit\":$lit,\"vertices\":${pial.vertexCount},\"sourceFaces\":${pial.faceCount},\"triangleBudget\":$triangleBudget,\"byteBudget\":1073741824,\"coldRenderNanos\":$renderNanos,\"derivedFaces\":${backend.chunks.map(_.faceCount).sum},\"generatedBytes\":${approximation.fold("null")(_.generatedBytes.toString)},\"nativeMeshBytes\":$nativeMeshBytes,\"atlasBytes\":$atlasBytes,\"certifiedChannelError\":${approximation.fold(0)(_.maximumChannelError)},$metrics}"
                  rows += row
                  println(s"cortical_native=$row")
                  Files.writeString(output.resolve("native.json"), rows.mkString("[\n", ",\n", "\n]\n"))
            finally controller.dispose()
          finally backend.dispose()
        Files.writeString(output.resolve("native.json"), rows.mkString("[\n", ",\n", "\n]\n"))
      catch case error: Throwable => failure = error
      finally
        stage.close()
        done.countDown()
    try
      require(done.await(1200, TimeUnit.SECONDS), "cortical native probe timed out")
      if failure != null then throw failure.nn
    finally Platform.exit()

  private def verifyUpdates(mode: CorticalMapMode, controller: JavaFxSurfaceController,
      backend: JavaFxSurfaceBackend, scene: SubScene, output: Path, triangleBudget: Int,
      rows: scala.collection.mutable.ArrayBuffer[String]): Unit =
    val surface = CorticalMapSemantics.Surface
    val layer = CorticalMapSemantics.Overlay
    val scalar = mode != CorticalMapMode.Nearest && mode != CorticalMapMode.Face
    def dispatch(action: SurfaceViewerAction): JavaFxInterpretReceipt =
      controller.dispatch(action).fold(e => throw new AssertionError(e.message), identity)
    def meshBytes: Long = backend.chunks.map(c => (c.mesh.getPoints.size().toLong + c.mesh.getNormals.size() +
      c.mesh.getTexCoords.size() + c.mesh.getFaces.size()) * 4).sum
    def atlasBytes: Long = backend.chunks.map(c => c.atlas.width.toLong * c.atlas.height * 4).sum
    for perspective <- Vector(false, true) do
      dispatch(SurfaceViewerAction.SetGeometryState(surface, SurfaceKind.Pial))
      dispatch(SurfaceViewerAction.SetTimepoint(0))
      dispatch(SurfaceViewerAction.SetProjection(if perspective then CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50))
        else CameraProjection.Orthographic(OrthographicScale.unsafe(100))))
      dispatch(SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Lateral(CorticalHemisphere.Left)))
      dispatch(SurfaceViewerAction.FitCamera)
      val projectionName = if perspective then "perspective" else "ortho"
      val changes = Vector(
        ("unchanged", Vector(SurfaceViewerAction.SetLighting(SurfaceLighting.Unlit)), true, true),
        ("camera", Vector(SurfaceViewerAction.OrbitBy(5, 2)), true, true),
        ("timepoint", Vector(SurfaceViewerAction.SetTimepoint(1)), false, false),
        ("opacity", Vector(SurfaceViewerAction.SetLayerOpacity(layer, DisplayOpacity.unsafe(if perspective then 0.7 else 0.6))), false, false)) ++
        (if scalar then Vector(("threshold", Vector(SurfaceViewerAction.SetLayerThreshold(layer,
          DisplayThreshold.transparentBand(if perspective then -0.5 else -0.75, if perspective then 0.5 else 0.75).toOption.get)), false, false)) else Vector.empty) ++
        Vector(
          ("morph-half", Vector(SurfaceViewerAction.BeginGeometryMorph(surface, SurfaceKind.Inflated),
            SurfaceViewerAction.SetGeometryMorphFraction(surface, SurfaceMorphFraction.unsafe(0.5)), SurfaceViewerAction.FitCamera), false, true),
          ("morph-end", Vector(SurfaceViewerAction.SetGeometryMorphFraction(surface, SurfaceMorphFraction.unsafe(1)), SurfaceViewerAction.FitCamera), false, true),
          ("white", Vector(SurfaceViewerAction.SetGeometryState(surface, SurfaceKind.White), SurfaceViewerAction.FitCamera), false, true),
          ("restored", Vector(SurfaceViewerAction.SetGeometryState(surface, SurfaceKind.Pial), SurfaceViewerAction.FitCamera), false, true))
      for (change, actions, cameraOnly, preserveData) <- changes do
        val before = controller.plan
        var uploadedMeshBytes = 0L
        var uploadedAtlasBytes = 0L
        var uploadedUvBytes = 0L
        var rebuilt = 0
        var updated = 0
        val started = System.nanoTime()
        val receipts = actions.map: action =>
          val previousMeshes = backend.chunks.map(_.mesh)
          val previousAtlases = backend.chunks.map(_.atlas.image)
          val receipt = dispatch(action)
          val sameMeshes = previousMeshes.length == backend.chunks.length && previousMeshes.zip(backend.chunks).forall((m, c) => m eq c.mesh)
          if !sameMeshes then
            rebuilt += 1
            uploadedMeshBytes += meshBytes
            uploadedAtlasBytes += atlasBytes
          else
            uploadedMeshBytes += receipt.geometryBytesUpdated
            if receipt.atlasUpdates > 0 then uploadedAtlasBytes += atlasBytes
          uploadedUvBytes += receipt.textureCoordinateBytesUpdated
          updated += receipt.geometryUpdates
          if cameraOnly then
            require(sameMeshes && previousAtlases.zip(backend.chunks).forall((a, c) => a eq c.atlas.image) &&
              !receipt.dirty.geometry && !receipt.dirty.layerData && receipt.atlasUpdates == 0 &&
              receipt.geometryBytesUpdated == 0 && receipt.textureCoordinateBytesUpdated == 0,
              s"$mode $change uploaded data or replaced native resources")
          receipt
        val dispatchNanos = System.nanoTime() - started
        require(controller.plan.receipt.meshKeys == before.receipt.meshKeys, "update changed scientific topology identity")
        if preserveData then require(controller.plan.receipt.layerKeys == before.receipt.layerKeys, "camera/morph update changed source layer identity")
        if !scalar && (change == "timepoint" || change == "opacity") then
          require(uploadedMeshBytes == 0 && rebuilt == 0, "unlit categorical data/style update uploaded geometry")
        val label = s"$mode-$projectionName-unlit-update-$change"
        val metrics = verify(label, controller, backend, scene, output)
        val approximation = receipts.last.approximation
        val row = s"{\"mode\":\"$mode\",\"perspective\":$perspective,\"update\":\"$change\",\"actionCount\":${actions.length},\"dispatchNanos\":$dispatchNanos,\"meshRebuilds\":$rebuilt,\"geometryUpdates\":$updated,\"uploadedMeshBytes\":$uploadedMeshBytes,\"uploadedAtlasBytes\":$uploadedAtlasBytes,\"uploadedUvBytes\":$uploadedUvBytes,\"nativeMeshBytes\":$meshBytes,\"atlasBytes\":$atlasBytes,\"derivedFaces\":${backend.chunks.map(_.faceCount).sum},\"generatedBytes\":${approximation.fold("null")(_.generatedBytes.toString)},\"triangleBudget\":$triangleBudget,\"byteBudget\":1073741824,$metrics}"
        rows += row
        println(s"cortical_update=$row")
        Files.writeString(output.resolve("updates.json"), rows.mkString("[\n", ",\n", "\n]\n"))

  private[javafx] def verify(name: String, controller: JavaFxSurfaceController, backend: JavaFxSurfaceBackend, scene: SubScene, output: Path, writeImages: Boolean = true): String =
    val width = scene.getWidth.toInt
    val height = scene.getHeight.toInt
    val dimensions = RasterDimensions.unsafe(width, height)
    val started = System.nanoTime()
    val native = scene.snapshot(null, new WritableImage(width, height))
    val snapshotNanos = System.nanoTime() - started
    val reference = SurfaceRasterizer.render(controller.plan, dimensions,
      SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
    if writeImages then
      val actualImage = new java.awt.image.BufferedImage(width, height, java.awt.image.BufferedImage.TYPE_INT_ARGB)
      val expectedImage = new java.awt.image.BufferedImage(width, height, java.awt.image.BufferedImage.TYPE_INT_ARGB)
      for y <- 0 until height; x <- 0 until width do
        actualImage.setRGB(x, y, native.getPixelReader.getArgb(x, y))
        val color = reference.image.pixelUnsafe(x, y)
        expectedImage.setRGB(x, y, (color.alpha << 24) | (color.red << 16) | (color.green << 8) | color.blue)
      javax.imageio.ImageIO.write(actualImage, "png", output.resolve(s"$name-javafx.png").toFile)
      javax.imageio.ImageIO.write(expectedImage, "png", output.resolve(s"$name-reference.png").toFile)
    val method = classOf[SubScene].getDeclaredMethod("pickRootSG", java.lang.Double.TYPE, java.lang.Double.TYPE)
    method.setAccessible(true)
    val categoricalUnlit = (name.startsWith("Nearest-") || name.startsWith("Face-")) && name.contains("-unlit-")
    val edgeLimit = if categoricalUnlit then 1 else 25
    val edgeOutliers = scala.collection.mutable.ArrayBuffer.empty[(Int, Int, Int, Int)]
    var checked = 0
    var smoothChecked = 0
    var maximumSmoothError = 0
    var picked = 0
    var pickNanos = 0L
    var maximumError = 0
    var maximumPickError = 0.0
    val candidates = scala.collection.mutable.ArrayBuffer.empty[(Int, Int, SurfacePick)]
    val sourceTriangles = scala.collection.mutable.Map.empty[(SurfaceId, Int), Vector[NativeSubpixelCoverage.Point]]
    var foreground = 0
    var union = 0
    var intersection = 0
    for y <- 1 until height - 1; x <- 1 until width - 1 do
      val pixel = native.getPixelReader.getArgb(x, y)
      val expected = reference.image.pixelUnsafe(x, y)
      val expectedForeground = expected.toPackedInt != -1
      val nativeForeground = pixel != -1
      if expectedForeground then foreground += 1
      if expectedForeground || nativeForeground then union += 1
      if expectedForeground && nativeForeground then intersection += 1
      reference.pick(x, y).toOption.flatten.foreach: p =>
        val neighbors = for dy <- -1 to 1; dx <- -1 to 1 yield (x + dx, y + dy)
        val triangle = sourceTriangles.getOrElseUpdate((p.surface, p.face),
          projectedTriangle(controller.plan, controller.plan.meshes.find(_.surface == p.surface).get,
            p.face, width, height, originalFace = true))
        val sameFace = NativeSubpixelCoverage.containsPixel(triangle, x, y)
        val smooth = neighbors.forall: (nx, ny) =>
          val c = reference.image.pixelUnsafe(nx, ny)
          Vector(math.abs(c.red - expected.red), math.abs(c.green - expected.green), math.abs(c.blue - expected.blue)).max <= 20
        if smooth && neighbors.forall((nx, ny) => reference.pick(nx, ny).toOption.flatten.nonEmpty) && math.min(p.barycentricA, math.min(p.barycentricB, p.barycentricC)) > 0.03 then
          val error = Vector(math.abs(((pixel >>> 16) & 255) - expected.red),
            math.abs(((pixel >>> 8) & 255) - expected.green), math.abs((pixel & 255) - expected.blue)).max
          if error > edgeLimit then edgeOutliers += ((x, y, p.face, error))
          maximumSmoothError = math.max(maximumSmoothError, error)
          smoothChecked += 1
          if sameFace then
            maximumError = math.max(maximumError, error)
            checked += 1
          val weights = Vector(p.barycentricA, p.barycentricB, p.barycentricC).sorted
          if sameFace && weights(2) - weights(1) > 0.02 then candidates += ((x, y, p))
    val indices = if candidates.size <= 64 then candidates.indices.toVector
      else Vector.tabulate(64)(i => i * (candidates.size - 1) / 63)
    indices.foreach: index =>
      val (x, y, p) = candidates(index)
      val pickStarted = System.nanoTime()
      val result = method.invoke(scene, Double.box(x + 0.5), Double.box(y + 0.5)).asInstanceOf[PickResult]
      val actual = controller.pick(result).fold(e => throw new AssertionError(e.message), identity)
      pickNanos += System.nanoTime() - pickStarted
      require(actual.surface == p.surface && actual.face.index == p.face && actual.vertex.index == p.vertex,
        s"cortical native ID mismatch at $x,$y: $actual versus $p")
      val error = Vector(math.abs(actual.barycentricA - p.barycentricA),
        math.abs(actual.barycentricB - p.barycentricB), math.abs(actual.barycentricC - p.barycentricC)).max
      maximumPickError = math.max(maximumPickError, error)
      picked += 1
    var classifiedCoveragePixels = 0
    val coverageEvidence = scala.collection.mutable.ArrayBuffer.empty[String]
    if edgeOutliers.nonEmpty then println(s"cortical_coverage_outliers=$name count=${edgeOutliers.size} first=${edgeOutliers.take(16).mkString(",")}")
    if edgeOutliers.nonEmpty then
      val prepared = backend.pickingPlan.getOrElse(controller.plan)
      val colors = JavaFxSurfaceProbe.compositeColors(prepared, JavaFxMaterialMode.Lit)
      // An ID swatch is valid only for independent constant-colored corners.
      // This includes both bounded cells and exact nearest/face partitions.
      val coverageEligible = prepared.meshes.size == 1 && prepared.meshes.forall: mesh =>
        val c = colors(mesh.surface)
        mesh.indices.length == mesh.positions.length / 3 &&
          (0 until mesh.indices.length).forall(i => mesh.indices(i) == i) &&
          (0 until mesh.indices.length / 3).forall(f => c(f * 3) == c(f * 3 + 1) && c(f * 3) == c(f * 3 + 2))
      if coverageEligible then
        val encoded = prepared.meshes.map: mesh =>
          require(mesh.indices.length / 3 < 0xffffff, "native triangle ID encoding exceeds RGB24")
          mesh.surface -> Array.tabulate(mesh.positions.length / 3)(v => (((v / 3) + 1) << 8) | 255)
        .toMap
        try
          backend.chunks.foreach: chunk =>
            val mesh = prepared.meshes.find(_.surface == chunk.surface).get
            chunk.atlas.commit(chunk.atlas.write(mesh.indices, encoded(chunk.surface)))
          val ids = scene.snapshot(null, new WritableImage(width, height))
          edgeOutliers.foreach: (x, y, _, _) =>
            val gpuFace = (ids.getPixelReader.getArgb(x, y) & 0xffffff) - 1
            val mesh = prepared.meshes.head
            require(gpuFace >= 0 && gpuFace < mesh.indices.length / 3, "native triangle ID pass produced an invalid ID")
            val color = colors(mesh.surface)(gpuFace * 3)
            val expected = reference.pick(x, y).toOption.flatten.get
            val nativeArgb = native.getPixelReader.getArgb(x, y)
            val actualColor = Rgba32.unsafe((nativeArgb >>> 16) & 255, (nativeArgb >>> 8) & 255, nativeArgb & 255, (nativeArgb >>> 24) & 255)
            val occlusion = NativeSubpixelCoverage.certify(projectedTriangle(prepared, mesh, gpuFace, width, height),
              x + 0.5, y + 0.5, expected.depth, expected.face, mesh.sourceFace(gpuFace), Rgba32.fromPackedInt(color), actualColor)
            val nearestSource = controller.plan.meshes.find(_.surface == mesh.surface).exists(_.nearestPartition.nonEmpty)
            val boundary = if name.startsWith("Nearest-") && nearestSource then
              NativeSubpixelCoverage.certifyNearestBoundary(projectedTriangle(prepared, mesh, gpuFace, width, height),
                x + 0.5, y + 0.5, expected.depth, expected.face, mesh.sourceFace(gpuFace), expected.vertex,
                mesh.pickedVertex(gpuFace, 1.0 / 3, 1.0 / 3, 1.0 / 3), Rgba32.fromPackedInt(color), actualColor)
            else None
            val evidence = occlusion.orElse(boundary)
            val classification = if occlusion.nonEmpty then "nearer-face-coverage" else "nearest-cell-boundary"
            if evidence.isEmpty then
              println(s"cortical_unclassified=$name reference=$expected actual=$actualColor cell=${Rgba32.fromPackedInt(color)} triangle=${projectedTriangle(prepared, mesh, gpuFace, width, height)}")
            evidence.foreach: e =>
              classifiedCoveragePixels += 1
              val row = s"{\"classification\":\"$classification\",\"x\":$x,\"y\":$y,\"referenceFace\":${expected.face},\"gpuFace\":${mesh.sourceFace(gpuFace)},\"maximumVertexShiftPixels\":${e.maximumVertexShift},\"nativeDepth\":${e.nativeDepth},\"referenceDepth\":${e.referenceDepth},\"nativeCellColorError\":${e.maximumColorError}}"
              coverageEvidence += row
              println(s"cortical_subpixel_coverage=$name $row")
            println(s"cortical_gpu_face=$name x=$x y=$y derivedFace=$gpuFace originalFace=${mesh.sourceFace(gpuFace)} color=$color classified=${evidence.nonEmpty}")
            val sourceFace = expected.face
            val cells = (0 until mesh.indices.length / 3).filter(i => mesh.sourceFace(i) == sourceFace || mesh.sourceFace(i) == mesh.sourceFace(gpuFace))
            val triangles = cells.map: index =>
              val points = Vector.tabulate(3)(corner => Vector.tabulate(3)(axis => mesh.positions((index * 3 + corner) * 3 + axis)).mkString("[", ",", "]"))
              s"{\"derivedFace\":$index,\"sourceFace\":${mesh.sourceFace(index)},\"points\":${points.mkString("[", ",", "]")}}"
            val view = prepared.camera.viewMatrix.unsafeArray.mkString("[", ",", "]")
            val projection = prepared.camera.projectionMatrix.unsafeArray.mkString("[", ",", "]")
            Files.writeString(output.resolve(s"$name-$x-$y-triangles.json"), s"{\"view\":$view,\"projection\":$projection,\"triangles\":${triangles.mkString("[", ",", "]")}}")
        finally
          backend.chunks.foreach: chunk =>
            val mesh = prepared.meshes.find(_.surface == chunk.surface).get
            chunk.atlas.commit(chunk.atlas.write(mesh.indices, colors(chunk.surface)))
    val viewportPixelArea = controller.plan.viewportFit.resolve(controller.plan.slots, width, height)
      .map(slot => slot.viewport.width * width * slot.viewport.height * height).sum
    val iou = intersection.toDouble / union
    println(s"cortical_coverage=$name foreground=$foreground checked=$checked picked=$picked maxError=$maximumError smoothMax=$maximumSmoothError coverageEdges=$classifiedCoveragePixels pickError=$maximumPickError iou=$iou")
    require(foreground > viewportPixelArea * 0.2 && foreground < viewportPixelArea * 0.8 && checked >= (if width < 600 then 1 else 100) && smoothChecked > viewportPixelArea * 0.1 && picked >= (if width < 600 then 1 else 10),
      "insufficient cortical coverage within the fitted logical viewport")
    require(iou >= 0.99 && maximumError <= 5 && (maximumSmoothError <= 25 || classifiedCoveragePixels == edgeOutliers.size) && maximumPickError <= 1e-3, "cortical native comparison exceeded its declared budget")
    if categoricalUnlit then
      require(maximumSmoothError <= 1 || classifiedCoveragePixels == edgeOutliers.size,
        "categorical colors differ outside certified subpixel coverage")
    s"\"viewportPixelArea\":$viewportPixelArea,\"classifiedCoveragePixels\":$classifiedCoveragePixels,\"coverageEvidence\":${coverageEvidence.mkString("[", ",", "]")},\"width\":$width,\"height\":$height,\"snapshotNanos\":$snapshotNanos,\"foreground\":$foreground,\"checkedPixels\":$checked,\"smoothPixelsIncludingFaceEdges\":$smoothChecked,\"maximumSmoothErrorIncludingFaceEdges\":$maximumSmoothError,\"checkedPicks\":$picked,\"pickBatchNanos\":$pickNanos,\"maximumChannelError\":$maximumError,\"maximumBarycentricError\":$maximumPickError,\"maskIoU\":$iou"


  private def projectedTriangle(plan: SurfaceRenderPlan, mesh: SurfaceMeshPacket, face: Int,
      width: Int, height: Int, originalFace: Boolean = false): Vector[NativeSubpixelCoverage.Point] =
    val slot = plan.viewportFit.resolve(plan.slots, width.toDouble, height.toDouble).find(_.surface == mesh.surface).get
    def multiply(matrix: FloatBufferView, p: Vector[Double]): Vector[Double] =
      Vector.tabulate(4)(row => (0 until 4).map(column => matrix(row * 4 + column) * p(column)).sum)
    Vector.tabulate(3): corner =>
      // Nearest lowering keeps each original corner at offsets 0, 6, and 12.
      // Other controller plans preserve original face order, including face-constant expansion.
      val index = (if originalFace && mesh.nearestPartition.nonEmpty then face * 18 + corner * 6
        else mesh.indices(face * 3 + corner)) * 3
      val world = Vector(mesh.positions(index) + slot.worldOffsetX, mesh.positions(index + 1) + slot.worldOffsetY,
        mesh.positions(index + 2) + slot.worldOffsetZ, 1.0)
      val clip = multiply(plan.camera.projectionMatrix, multiply(plan.camera.viewMatrix, world))
      val viewport = slot.viewport
      NativeSubpixelCoverage.Point(
        (viewport.x + (clip(0) / clip(3) + 1) * 0.5 * viewport.width) * width,
        (viewport.y + (1 - clip(1) / clip(3)) * 0.5 * viewport.height) * height,
        if clip(3) > 0 then (clip(2) / clip(3) + 1) * 0.5 else Double.NaN)

  private def exportPublication(example: SurfaceViewerExample, mode: CorticalMapMode, output: Path): Unit =
    val overlay = if mode == CorticalMapMode.Nearest || mode == CorticalMapMode.Face then
      SurfaceLegendRequest.Categorical(SurfaceLegendLayers.one(CorticalMapSemantics.Overlay),
        LegendTitle.make("Synthetic anterior-posterior bands").toOption.get,
        Map(1 -> "y < -40 mm", 2 -> "-40 mm <= y < 0 mm", 3 -> "y >= 0 mm"))
    else SurfaceLegendRequest.Split(SurfaceLegendLayers.one(CorticalMapSemantics.Overlay),
      LegendTitle.make("Synthetic coordinate field", Some("dimensionless")).toOption.get)
    val underlay = mode match
      case CorticalMapMode.Layered => Vector(SurfaceLegendRequest.Continuous(
        SurfaceLegendLayers.one(CorticalMapSemantics.Underlay), LegendTitle.make("FreeSurfer sulcal depth").toOption.get))
      case CorticalMapMode.CurvatureLayered => Vector(SurfaceLegendRequest.Continuous(
        SurfaceLegendLayers.one(CorticalMapSemantics.Curvature), LegendTitle.make("Standardized umbrella curvature").toOption.get))
      case _ => Vector.empty
    val preset = SurfacePublicationPreset.ManuscriptDoubleColumn
    val publication = SurfaceLegendPublication.prepare(example.model, example.state,
      SurfacePublicationSpec(preset, Some(s"fsaverage6: $mode display"), SurfaceOrientationMark.LeftLateral), Vector(overlay) ++ underlay)
      .fold(e => throw new AssertionError(e.message), identity)
    val artifact = publication.renderWith: input =>
      SurfaceRasterizer.render(input.plan, input.dimensions,
        SurfaceRasterStyle(background = input.background, culling = TriangleCulling.None)).map(_.image)
    .fold(e => throw new AssertionError(e.toString), identity)
    val svg = SvgRenderer.render(artifact.scene, SvgOptions.unsafe(preset.width, preset.height, Some(mode.toString))).toOption.get
    Files.writeString(output.resolve(s"$mode-publication.svg"), svg.value)
