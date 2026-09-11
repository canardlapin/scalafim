package scalafim.surface.view.javafx

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*
import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import _root_.javafx.application.Platform
import _root_.javafx.scene.{Group, Scene as FxScene, SceneAntialiasing}
import _root_.javafx.scene.image.{PixelFormat, WritableImage}

/** Real bilateral cortex for the flat per-face texel encodings.
  *
  * Inputs are the retained scene of the scalar lookup spike (see
  * JavaFxScalarLutCortexProbe): inflated geometry, normals, camera and slots
  * fitted at 1350 x 762, raw beta/FIR values and FreeSurfer sulcal depth. The
  * plan has an interpolated-scalar underlay (binary step at 0) and overlay
  * (saturated onset at the cutoff, transparent below it). Missing values show
  * the underlay.
  *
  * mode `reference` (no JavaFX): checks the face rule against the recipe at
  * every face, then writes the flat point reference, the 4 x 4 sub-pixel flat
  * reference with its coverage, and the interpolated-scalar and
  * interpolated-colour raster references.
  * mode `native`: renders one encoding and records memory, first show time and
  * 1,200 lattice picks against the raster. With `bench` it times 20 palette,
  * cutoff and map-value updates (after 4 warm-ups), each followed by a snapshot,
  * which includes JavaFX's texture upload and mipmap regeneration, and checks
  * that the native meshes are the same objects with identical arrays afterwards.
  *
  * args: mode encoding scene output antialiasing lighting [bench]
  */
object JavaFxFaceTexelCortexProbe:
  import JavaFxScalarLutCortexProbe.{Inputs, load, loadValues, overlayMapping, underlayMapping, finalPlan}
  private val Width = 1350
  private val Height = 762

  private def hemi(surface: SurfaceId): String = if surface.value.endsWith("lh") then "lh" else "rh"

  /** The same split scale, cutoff and saturated onset with different hues. */
  def paletteMapping(range: Double, cutoff: Double): ScalarMapping =
    ScalarMapping(ScalarScale.split(DisplayWindow.unsafe(-range, range), 0.0, -cutoff, cutoff,
      ScalarRamp.linear(Rgba32.unsafe(120, 0, 200), Rgba32.unsafe(220, 160, 255)),
      ScalarRamp.linear(Rgba32.unsafe(0, 150, 60), Rgba32.unsafe(190, 255, 140))).toOption.get)

  def scalarPlan(inputs: Inputs, values: Map[SurfaceId, Array[Double]], over: ScalarMapping,
      lighting: SurfaceLighting): SurfaceRenderPlan =
    val under = underlayMapping(inputs.sulcLimit)
    JavaFxScalarLutCortexProbe.compileWith(inputs, lighting, (mesh, geometry) => Vector(
      SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe(s"cortex-${hemi(mesh.surface)}"), mesh.surface, geometry,
        inputs.sulc(mesh.surface), under).toOption.get,
      SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe(s"result-${hemi(mesh.surface)}"), mesh.surface, geometry,
        values(mesh.surface), over).toOption.get))

  final case class Rule(colors: Map[SurfaceId, Array[Int]], faces: Int, maxDifference: Int, offByOne: Int,
      missingFaces: Int, belowCutoffFaces: Int)

  /** Rule colours per surface, checked face by face against the recipe written out directly. */
  def rule(inputs: Inputs, plan: SurfaceRenderPlan, values: Map[SurfaceId, Array[Double]], over: ScalarMapping): Rule =
    val under = underlayMapping(inputs.sulcLimit)
    var faces = 0; var maxDifference = 0; var offByOne = 0; var missing = 0; var below = 0
    val colors = plan.meshes.map: mesh =>
      val computed = SurfaceFaceTexels.colors(plan, mesh).fold(e => throw new IllegalStateException(e.message), identity)
      val v = values(mesh.surface)
      val s = inputs.sulc(mesh.surface)
      val indices = mesh.indices
      var face = 0
      while face < computed.length do
        val (a, b, c) = (indices(face * 3), indices(face * 3 + 1), indices(face * 3 + 2))
        def mean(x: Array[Double]): Double =
          if x(a).isFinite && x(b).isFinite && x(c).isFinite then (x(a) + x(b) + x(c)) / 3 else Double.NaN
        val value = mean(v)
        if value.isNaN then missing += 1 else if over.color(value).alpha == 0 then below += 1
        val expected = DisplayBlendMode.Normal.composite(
          DisplayBlendMode.Normal.composite(SurfaceFaceTexels.Base, under.color(mean(s)), DisplayOpacity.Opaque),
          over.color(value), DisplayOpacity.Opaque).toPackedInt
        val difference = Vector(24, 16, 8, 0).map(shift => math.abs(((expected >>> shift) & 255) - ((computed(face) >>> shift) & 255))).max
        if difference > maxDifference then maxDifference = difference
        if difference == 1 then offByOne += 1
        face += 1
      faces += computed.length
      mesh.surface -> computed
    .toMap
    require(maxDifference <= 1, s"face rule differs from the recipe by $maxDifference")
    Rule(colors, faces, maxDifference, offByOne, missing, below)

  private def argb(image: WritableImage): Array[Int] =
    val pixels = new Array[Int](Width * Height)
    image.getPixelReader.getPixels(0, 0, Width, Height, PixelFormat.getIntArgbInstance(), pixels, 0, Width)
    pixels

  private def signature(pixels: Array[Int]): Long =
    var hash = 0xcbf29ce484222325L
    var index = 0
    while index < pixels.length do
      hash = (hash ^ pixels(index).toLong) * 0x100000001b3L
      index += 1
    hash

  private def meshSignature(chunks: Vector[JavaFxSurfaceChunk]): Long =
    var hash = 0xcbf29ce484222325L
    def mix(value: Int): Unit = hash = (hash ^ value.toLong) * 0x100000001b3L
    chunks.foreach: chunk =>
      chunk.mesh.getPoints.toArray(null: Array[Float]).foreach(v => mix(java.lang.Float.floatToIntBits(v)))
      chunk.mesh.getNormals.toArray(null: Array[Float]).foreach(v => mix(java.lang.Float.floatToIntBits(v)))
      chunk.mesh.getTexCoords.toArray(null: Array[Float]).foreach(v => mix(java.lang.Float.floatToIntBits(v)))
      chunk.mesh.getFaces.toArray(null: Array[Int]).foreach(mix)
    hash

  private def stats(values: Vector[Double]): String =
    val sorted = values.sorted
    def at(p: Double) = sorted(math.min(sorted.length - 1, (sorted.length * p).toInt))
    f"""{"n":${sorted.length},"median":${at(0.5)}%.3f,"p90":${at(0.9)}%.3f,"max":${sorted.last}%.3f,"min":${sorted.head}%.3f}"""

  private def origin(name: String): String = Class.forName(name).getProtectionDomain.getCodeSource.getLocation.toString

  private def rasterPixels(result: SurfaceRasterResult, width: Int, height: Int): Array[Int] =
    Array.tabulate(width * height): index =>
      val c = result.image.pixelUnsafe(index % width, index / width)
      0xff000000 | (c.red << 16) | (c.green << 8) | c.blue

  def main(args: Array[String]): Unit =
    val mode = args(0)
    val encoding = JavaFxAtlasEncoding.valueOf(args(1))
    val sceneName = args(2)
    val output = Path.of(args(3))
    val antialiasing = if args(4) == "DISABLED" then SceneAntialiasing.DISABLED else SceneAntialiasing.BALANCED
    val lit = args(5) == "lit"
    val bench = args.lift(6).contains("bench")
    val inputsDir = Path.of(System.getProperty("probe.inputs", "/Users/bbuchsbaum/code/scala/plsneuro-fixtures/spike-lut-1/inputs"))
    Files.createDirectories(output)
    val lightingName = if lit then "lit" else "unlit"
    val lighting = if lit then SurfaceLighting.Default else SurfaceLighting.Unlit
    val inputs = load(inputsDir, sceneName)
    val over = overlayMapping(inputs.range, inputs.cutoff)
    val compileStarted = System.nanoTime()
    val plan = scalarPlan(inputs, inputs.values, over, lighting)
    val compileMs = (System.nanoTime() - compileStarted) / 1e6
    val ruleStarted = System.nanoTime()
    val faceRule = rule(inputs, plan, inputs.values, over)
    val ruleMs = (System.nanoTime() - ruleStarted) / 1e6
    val ruleRecord = s""""rule":{"faces":${faceRule.faces},"maxDifferenceFromRecipe":${faceRule.maxDifference},""" +
      s""""offByOne":${faceRule.offByOne},"missingFaces":${faceRule.missingFaces},"belowCutoffFaces":${faceRule.belowCutoffFaces},"ruleMs":$ruleMs}"""
    val style = SurfaceRasterStyle(culling = TriangleCulling.None)
    if mode == "reference" then
      val started = System.nanoTime()
      val one = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(Width, Height), style).fold(e => throw new IllegalStateException(e.toString), identity)
      JavaFxFaceTexelPlanarProbe.writeRgb(output.resolve(s"ref-interp-scalar-$sceneName-$lightingName.png"), rasterPixels(one, Width, Height), Width, Height)
      val colour = finalPlan(inputs, inputs.values, inputs.cutoff, lighting)
      val colourRaster = SurfaceRasterizer.render(colour, RasterDimensions.unsafe(Width, Height), style).fold(e => throw new IllegalStateException(e.toString), identity)
      JavaFxFaceTexelPlanarProbe.writeRgb(output.resolve(s"ref-interp-colour-$sceneName-$lightingName.png"), rasterPixels(colourRaster, Width, Height), Width, Height)
      val (flatOne, coverOne) = JavaFxFaceTexelPlanarProbe.flat(one, Width, Height, faceRule.colors)
      JavaFxFaceTexelPlanarProbe.writeRgb(output.resolve(s"ref-flat-$sceneName.png"), flatOne, Width, Height)
      JavaFxFaceTexelPlanarProbe.writeRgb(output.resolve(s"cover-$sceneName.png"), coverOne, Width, Height)
      val (distances, residual) = JavaFxFaceTexelPlanarProbe.edgeDistances(one, Width, Height, plan.meshes)
      JavaFxFaceTexelPlanarProbe.writeFloats(output.resolve(s"edge-distance-$sceneName.f32"), distances)
      println(s"edge_distance_fit $sceneName maxResidualPx=$residual")
      // Face picks depend only on geometry and camera; the vertex-colour plan shares both and rasterizes faster.
      val four = SurfaceRasterizer.render(colour, RasterDimensions.unsafe(Width * 4, Height * 4), style).fold(e => throw new IllegalStateException(e.toString), identity)
      val (flatFour, coverFour) = JavaFxFaceTexelPlanarProbe.flat(four, Width * 4, Height * 4, faceRule.colors)
      JavaFxFaceTexelPlanarProbe.writeRgb(output.resolve(s"ref-flat-4x-$sceneName.png"), flatFour, Width * 4, Height * 4)
      JavaFxFaceTexelPlanarProbe.writeRgb(output.resolve(s"cover-4x-$sceneName.png"), coverFour, Width * 4, Height * 4)
      val record = s"""{"mode":"reference","scene":"$sceneName","lighting":"$lightingName",$ruleRecord,"planCompileMs":$compileMs,""" +
        s""""referenceMs":${(System.nanoTime() - started) / 1e6},"range":${inputs.range},"cutoff":${inputs.cutoff},"sulcLimit":${inputs.sulcLimit}}"""
      Files.writeString(output.resolve(s"reference-$sceneName-$lightingName.json"), record + "\n")
      println(s"face_cortex_reference=$record")
      println("PASS cortex references written")
      return
    val material = origin("com.sun.prism.es2.ES2PhongMaterial")
    val shader = origin("com.sun.prism.es2.ES2PhongShader")
    Option(System.getProperty("probe.expectedOrigin")).foreach(expected =>
      require(material == expected && shader == expected, s"unexpected material/shader origin: $material / $shader"))
    val label = s"$mode-$encoding-$sceneName-$lightingName-$antialiasing"
    val raster = Option.when(System.getProperty("probe.raster", "true") == "true"):
      SurfaceRasterizer.render(plan, RasterDimensions.unsafe(Width, Height), style).fold(e => throw new IllegalStateException(e.toString), identity)
    val variants =
      if !bench then Vector.empty
      else
        val other = loadValues(inputsDir, if sceneName == "beta" then "fir" else "beta", plan.meshes.map(_.surface))
        Vector(
          "palette" -> scalarPlan(inputs, inputs.values, paletteMapping(inputs.range, inputs.cutoff), lighting),
          "cutoff" -> scalarPlan(inputs, inputs.values, overlayMapping(inputs.range, inputs.cutoff * 1.5), lighting),
          "mapValues" -> scalarPlan(inputs, other, over, lighting))
    val done = new CountDownLatch(1)
    @volatile var failure: Option[Throwable] = None
    @volatile var record = ""
    Platform.startup(() => ())
    Platform.runLater: () =>
      try
        val config = JavaFxAtlasConfig.make(encoding = encoding).toOption.get
        val backend = JavaFxSurfaceBackend.create(config).fold(e => throw new IllegalStateException(e.message), identity)
        try
          val started = System.nanoTime()
          backend.render(plan).fold(e => throw new IllegalStateException(e.message), identity)
          val buildMs = (System.nanoTime() - started) / 1e6
          val settings = JavaFxSnapshotConfig.make(Width, Height, antialiasing).toOption.get
          val sub = backend.newSubScene(settings).toOption.get
          val host = new FxScene(new Group(sub), Width.toDouble, Height.toDouble)
          host.getRoot.applyCss()
          host.getRoot.layout()
          val firstStarted = System.nanoTime()
          var image = backend.snapshot(settings).toOption.get
          val firstMs = (System.nanoTime() - firstStarted) / 1e6
          val warm = Vector.fill(6):
            val s = System.nanoTime()
            image = backend.snapshot(settings).toOption.get
            (System.nanoTime() - s) / 1e6
          val pixels = argb(image)
          val colours = pixels.distinct.length
          require(colours > 32, s"blank or degenerate frame: $colours colours")
          val base = signature(pixels)
          require(signature(argb(backend.snapshot(settings).toOption.get)) == base, "repeat snapshot differs")
          JavaFxFaceTexelPlanarProbe.writeRgb(output.resolve(s"$label.png"), pixels, Width, Height)
          val chunks = backend.chunks
          val textureBytes = chunks.map(c => c.atlas.width.toLong * c.atlas.height * 4).sum
          val textures = chunks.map(c => s"${c.atlas.width}x${c.atlas.height}").mkString("[\"", "\",\"", "\"]")
          val meshBytes = chunks.map(c => (c.mesh.getPoints.size.toLong + c.mesh.getNormals.size + c.mesh.getTexCoords.size + c.mesh.getFaces.size) * 4).sum
          val renderedFaces = chunks.map(_.renderedFaceCount).sum
          val points = chunks.map(_.mesh.getPoints.size / 3).sum
          val texCoords = chunks.map(_.mesh.getTexCoords.size / 2).sum
          val meshesBefore = chunks.map(_.mesh)
          val meshHashBefore = meshSignature(chunks)
          val pickRecord = raster.fold("null"): reference =>
            var sampled = 0; var agree = 0; var adjacent = 0; var miss = 0; var wrongSurface = 0
            var y = 5
            while y < Height && sampled < 1200 do
              var x = 7
              while x < Width && sampled < 1200 do
                reference.pick(x, y).toOption.flatten.foreach: expected =>
                  sampled += 1
                  val native = JavaFxNativePick.at(sub, x + 0.5, y + 0.5).flatMap: result =>
                    chunks.find(_.view eq result.getIntersectedNode).flatMap(chunk =>
                      chunk.packetFace(result.getIntersectedFace).map(face => (chunk.surface, face)))
                  native match
                    case None => miss += 1
                    case Some((surface, face)) =>
                      if surface != expected.surface then wrongSurface += 1
                      else if face == expected.face then agree += 1
                      else
                        val indices = plan.meshes.find(_.surface == surface).get.indices
                        val a = Set(indices(face * 3), indices(face * 3 + 1), indices(face * 3 + 2))
                        val b = Set(indices(expected.face * 3), indices(expected.face * 3 + 1), indices(expected.face * 3 + 2))
                        if (a intersect b).nonEmpty then adjacent += 1
                x += 17
              y += 11
            s"""{"sampled":$sampled,"sameFace":$agree,"sharedVertexNeighbour":$adjacent,"nativeMiss":$miss,"wrongSurface":$wrongSurface}"""
          val benchRecord =
            if variants.isEmpty then "null"
            else
              def alternate(name: String, changed: SurfaceRenderPlan): String =
                val rows = Vector.tabulate(24): i =>
                  val target = if i % 2 == 0 then changed else plan
                  val s = System.nanoTime()
                  val receipt = backend.render(target).fold(e => throw new IllegalStateException(s"$name: ${e.message}"), identity)
                  val mid = System.nanoTime()
                  val shot = signature(argb(backend.snapshot(settings).toOption.get))
                  val end = System.nanoTime()
                  if i % 2 == 0 then require(shot != base, s"$name update $i did not change the frame")
                  else require(shot == base, s"$name restoration $i differs from the original frame")
                  ((mid - s) / 1e6, (end - mid) / 1e6, (end - s) / 1e6, receipt)
                val measured = rows.drop(4)
                val receipts = measured.map(_._4)
                s""""$name":{"renderMs":${stats(measured.map(_._1))},"snapshotAfterMs":${stats(measured.map(_._2))},""" +
                  s""""renderAndSnapshotMs":${stats(measured.map(_._3))},"textureCoordinateBytes":[${receipts.map(_.textureCoordinateBytesUpdated).distinct.mkString(",")}],""" +
                  s""""geometryBytes":[${receipts.map(_.geometryBytesUpdated).distinct.mkString(",")}],""" +
                  s""""atlasUpdates":[${receipts.map(_.atlasUpdates).distinct.mkString(",")}],"geometryRebuilds":${receipts.count(_.dirty.geometry)},"frameChangeAndRestoreVerified":true}"""
              val body = variants.map(alternate.tupled).mkString(",")
              val unchangedMeshes = backend.chunks.map(_.mesh).zip(meshesBefore).forall(_ eq _) && meshSignature(backend.chunks) == meshHashBefore
              require(unchangedMeshes, "native meshes changed during colour updates")
              s"{$body,\"nativeMeshesUnchanged\":$unchangedMeshes}"
          record = s"""{"label":"$label","mode":"$mode","encoding":"$encoding","scene":"$sceneName","lighting":"$lightingName",""" +
            s""""antialiasing":"$antialiasing","width":$Width,"height":$Height,"range":${inputs.range},"cutoff":${inputs.cutoff},""" +
            s"""$ruleRecord,"renderedFaces":$renderedFaces,"points":$points,"textureCoordinates":$texCoords,"textures":$textures,""" +
            s""""textureBytes":$textureBytes,"meshArrayBytes":$meshBytes,"planCompileMs":$compileMs,"buildMs":$buildMs,""" +
            s""""firstSnapshotMs":$firstMs,"firstShowMs":${buildMs + firstMs},"warmSnapshotMs":${stats(warm)},"distinctColours":$colours,"signature":$base,""" +
            s""""picks":$pickRecord,"bench":$benchRecord,""" +
            s""""runtime":{"javaVersion":"${System.getProperty("java.version")}","javafxVersion":"${System.getProperty("javafx.runtime.version")}",""" +
            s""""prismOrder":"${System.getProperty("prism.order")}","affineAtlas":"${System.getProperty("scalafim.javafx.affineAtlas")}",""" +
            s""""materialOrigin":"$material","shaderOrigin":"$shader"}}"""
          Files.writeString(output.resolve(s"$label.json"), record + "\n")
        finally backend.dispose()
      catch case error: Throwable => failure = Some(error)
      finally done.countDown()
    try
      require(done.await(900, TimeUnit.SECONDS), "cortex probe timed out")
      failure.foreach(throw _)
      println(s"face_cortex=$record")
      println("PASS cortex snapshot written")
    finally Platform.exit()
