package scalafim.surface.view.javafx

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*
import java.awt.image.BufferedImage
import java.io.{BufferedInputStream, DataInputStream}
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import javax.imageio.ImageIO
import _root_.javafx.application.Platform
import _root_.javafx.scene.{Group, Scene as FxScene, SceneAntialiasing}
import _root_.javafx.scene.image.{PixelFormat, WritableImage}

/** Real bilateral cortex comparison for the scalar lookup encoding.
  *
  * Inputs are the retained PLS Neuro raster-spike scene (inflated geometry,
  * normals, camera and slots fitted at 1350 x 762, recipe metadata), the raw
  * projected values and the FreeSurfer sulcal depth. Three plans share that
  * geometry and camera:
  *
  *  - `scalar`: interpolated-scalar underlay (sulcal depth, binary step at 0)
  *    and overlay (projected values, saturated onset at the cutoff);
  *  - `final`: final vertex colours from the same two mappings (the atlas path);
  *  - `scene`: the application's own exported plan, to check the harness.
  *
  * The mappings are verified against the exporter's colour rule at every
  * vertex before rendering. Missing values show the underlay in `scalar` and
  * `final`; the exported `scene` shows its grey invalid-sample colour instead.
  *
  * args: mode encoding scene output antialiasing lighting [bench]
  */
object JavaFxScalarLutCortexProbe:
  private val Width = 1350
  private val Height = 762
  private val Sulcal = Rgba32.unsafe(65, 72, 82)
  private val Gyral = Rgba32.unsafe(202, 205, 209)
  private val Transparent = Rgba32.unsafe(0, 0, 0, 0)

  final case class Inputs(scene: SurfaceRenderPlan, metadata: String, values: Map[SurfaceId, Array[Double]],
      sulc: Map[SurfaceId, Array[Double]], range: Double, cutoff: Double, sulcLimit: Double,
      overlayChecked: Int, underlayMismatches: Int)

  def overlayMapping(range: Double, cutoff: Double): ScalarMapping =
    ScalarMapping(ScalarScale.split(DisplayWindow.unsafe(-range, range), 0.0, -cutoff, cutoff,
      ScalarRamp.linear(Rgba32.unsafe(0, 255, 255), Rgba32.unsafe(0, 64, 255)),
      ScalarRamp.linear(Rgba32.unsafe(255, 64, 0), Rgba32.unsafe(255, 255, 0))).toOption.get)

  def underlayMapping(limit: Double): ScalarMapping =
    ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-limit, limit),
      ScalarRamp.make(Vector(0.0 -> Gyral, 0.5 -> Gyral, 0.5000001 -> Sulcal, 1.0 -> Sulcal)).toOption.get))

  /** The exporter's overlay rule (SpikeSceneExport.overlayColor) for finite values. */
  private def exporterColor(value: Double, range: Double, cutoff: Double): Rgba32 =
    if math.abs(value) < cutoff then Transparent
    else
      val t = math.min(1.0, (math.abs(value) - cutoff) / (range - cutoff))
      val g = math.round(64 + 191 * t).toInt
      if value > 0 then Rgba32.unsafe(255, g, 0) else Rgba32.unsafe(0, g, 255)

  private def readFloats(in: DataInputStream): Array[Float] = Array.fill(in.readInt())(in.readFloat())
  private def readInts(in: DataInputStream): Array[Int] = Array.fill(in.readInt())(in.readInt())
  private def readSlots(in: DataInputStream): Vector[SurfaceViewSlot] =
    Vector.fill(in.readInt()):
      val surface = SurfaceId.unsafe(in.readUTF())
      val viewport = SurfaceViewport(in.readDouble(), in.readDouble(), in.readDouble(), in.readDouble())
      SurfaceViewSlot(surface, viewport, in.readDouble(), in.readDouble(), in.readDouble())

  /** Reader for the raster spike's SFIM-SPIKE-PLAN-1 codec; returns the plan with 1350 x 762 fitted slots. */
  def readScene(path: Path): (SurfaceRenderPlan, String) =
    val in = new DataInputStream(new BufferedInputStream(Files.newInputStream(path), 1 << 20))
    try
      require(in.readUTF() == "SFIM-SPIKE-PLAN-1", s"unexpected plan magic in $path")
      val metadata = in.readUTF()
      val slots = readSlots(in)
      val meshes = Vector.fill(in.readInt()):
        val surface = SurfaceId.unsafe(in.readUTF())
        val key = in.readUTF()
        val geometryKey = in.readUTF()
        val positions = readFloats(in)
        val normals = readFloats(in)
        val indices = readInts(in)
        SurfaceMeshPacket(surface, SurfaceResourceKey(key), new FloatBufferView(positions), new FloatBufferView(normals),
          new IntBufferView(indices), if geometryKey == key then None else Some(SurfaceResourceKey(geometryKey)))
      val layers = Vector.fill(in.readInt()):
        val layer = SurfaceLayerId.unsafe(in.readUTF())
        val surface = SurfaceId.unsafe(in.readUTF())
        val key = in.readUTF()
        val colors = readInts(in)
        val opacity = DisplayOpacity.unsafe(in.readDouble())
        SurfaceLayerPacket(layer, surface, SurfaceResourceKey(key), new IntBufferView(colors), opacity, DisplayBlendMode.valueOf(in.readUTF()))
      val view = readFloats(in)
      val projection = readFloats(in)
      val camera = SurfaceCameraPacket(new FloatBufferView(view), new FloatBufferView(projection), in.readDouble(), in.readDouble(), in.readDouble())
      val lighting = in.readUTF() match
        case "unlit" => SurfaceLighting.Unlit
        case "directional" =>
          val ambient = in.readDouble()
          val diffuse = in.readDouble()
          SurfaceLighting.Directional(LightFraction.unsafe(ambient), LightFraction.unsafe(diffuse), in.readDouble(), in.readDouble(), in.readDouble())
        case other => throw new IllegalStateException(s"unknown lighting '$other'")
      val clipping = in.readUTF() match
        case "disabled" => SurfaceClipping.Disabled
        case "near-far" => SurfaceClipping.NearFar(in.readDouble(), in.readDouble())
        case other => throw new IllegalStateException(s"unsupported clipping '$other'")
      val passes = Vector.fill(in.readInt()):
        val slot = in.readInt()
        val mesh = in.readUTF()
        val layer = in.readUTF()
        SurfaceDrawPass(slot, SurfaceResourceKey(mesh), SurfaceResourceKey(layer), DisplayBlendMode.valueOf(in.readUTF()))
      val profile = SurfaceProfile(in.readInt(), in.readInt(), in.readInt(), in.readInt(), in.readInt(), in.readLong())
      val meshKeys = Vector.fill(in.readInt())(SurfaceResourceKey(in.readUTF()))
      val layerKeys = Vector.fill(in.readInt())(SurfaceResourceKey(in.readUTF()))
      val receipt = SurfaceRenderReceipt(meshKeys, layerKeys, in.readUTF(), in.readInt(), in.readInt())
      val fitted = Vector.fill(in.readInt()):
        val width = in.readInt()
        val height = in.readInt()
        (width, height, readSlots(in))
      val sized = fitted.find(f => f._1 == Width && f._2 == Height).getOrElse(throw new IllegalStateException("no 1350x762 slots"))._3
      (SurfaceRenderPlan(sized, meshes, layers, camera, lighting, clipping, passes, Scene.empty, Vector.empty, profile, receipt), metadata)
    finally in.close()

  private def f64(path: Path): Array[Double] =
    val bytes = Files.readAllBytes(path)
    val buffer = ByteBuffer.wrap(bytes).order(ByteOrder.BIG_ENDIAN)
    Array.fill(bytes.length / 8)(buffer.getDouble())

  /** FreeSurfer "new" curvature format: 0xFFFFFF, vertices, faces, values per vertex, big-endian floats. */
  private def curv(path: Path): Array[Double] =
    val buffer = ByteBuffer.wrap(Files.readAllBytes(path)).order(ByteOrder.BIG_ENDIAN)
    require((buffer.get() & 255) == 255 && (buffer.get() & 255) == 255 && (buffer.get() & 255) == 255, s"$path is not a curv file")
    val vertices = buffer.getInt()
    buffer.getInt()
    require(buffer.getInt() == 1, "one value per vertex expected")
    Array.fill(vertices)(buffer.getFloat().toDouble)

  private def number(metadata: String, key: String): Double =
    s"\"$key\":([-0-9.eE]+)".r.findFirstMatchIn(metadata).map(_.group(1).toDouble).getOrElse(throw new IllegalStateException(key))

  private def hemi(surface: SurfaceId): String = if surface.value.endsWith("lh") then "lh" else "rh"

  def loadValues(dir: Path, scene: String, surfaces: Vector[SurfaceId]): Map[SurfaceId, Array[Double]] =
    surfaces.map(id => id -> f64(dir.resolve(s"values-$scene-${hemi(id)}.f64be"))).toMap

  def load(dir: Path, scene: String): Inputs =
    val (plan, metadata) = readScene(dir.resolve(s"scene-$scene-unlit.plan"))
    val surfaces = plan.meshes.map(_.surface)
    val values = loadValues(dir, scene, surfaces)
    val sulc = surfaces.map(id => id -> curv(dir.resolve(s"${hemi(id)}.sulc"))).toMap
    val range = number(metadata, "symmetricRange")
    val cutoff = number(metadata, "cutoff")
    val magnitudes = sulc.values.flatten.filter(_.isFinite).map(math.abs).toArray.sorted
    val sulcLimit = magnitudes(math.ceil(0.98 * magnitudes.length).toInt - 1)
    val over = overlayMapping(range, cutoff)
    val under = underlayMapping(sulcLimit)
    var overlayChecked = 0
    var underlayMismatches = 0
    plan.meshes.foreach: mesh =>
      val vertices = mesh.positions.length / 3
      val v = values(mesh.surface)
      val s = sulc(mesh.surface)
      require(v.length == vertices && s.length == vertices, s"${mesh.surface.value}: sample domain mismatch")
      var index = 0
      while index < vertices do
        if v(index).isFinite then
          val expected = exporterColor(v(index), range, cutoff)
          val actual = over.color(v(index))
          require(actual == expected || (expected.alpha == 0 && actual.alpha == 0), s"overlay mapping differs at ${mesh.surface.value}:$index: $actual vs $expected")
          overlayChecked += 1
        index += 1
      val sceneUnder = plan.layers.find(layer => layer.surface == mesh.surface && layer.layer.value.startsWith("cortex-")).get.colors
      index = 0
      while index < vertices do
        if under.color(s(index)).toPackedInt != sceneUnder(index) then underlayMismatches += 1
        index += 1
    Inputs(plan, metadata, values, sulc, range, cutoff, sulcLimit, overlayChecked, underlayMismatches)

  private def geometryFor(mesh: SurfaceMeshPacket): SurfaceGeometry =
    SurfaceGeometry(TriangleMesh.fromArrays(mesh.positions.unsafeArray.map(_.toDouble), mesh.indices.unsafeArray),
      if hemi(mesh.surface) == "lh" then Hemisphere.Left else Hemisphere.Right, SurfaceKind.Inflated)

  private def compileWith(inputs: Inputs, lighting: SurfaceLighting,
      layersFor: (SurfaceMeshPacket, SurfaceGeometry) => Vector[SurfaceLayer]): SurfaceRenderPlan =
    val geometries = inputs.scene.meshes.map(mesh => mesh -> geometryFor(mesh))
    val assets = geometries.map((mesh, geometry) => SurfaceAsset.make(mesh.surface, geometry).toOption.get)
    val model = SurfaceViewerModel.make(assets, geometries.flatMap(layersFor.tupled)).fold(e => throw new IllegalStateException(e.toString), identity)
    val ids = inputs.scene.meshes.map(_.surface)
    val state = SurfaceViewerState.initial(model).copy(
      layout = SurfaceLayout.Bilateral(ids.find(hemi(_) == "lh").get, ids.find(hemi(_) == "rh").get), lighting = lighting)
    val compiled = SurfaceCompiler.compile(model, state).fold(e => throw new IllegalStateException(e.toString), identity)
    require(compiled.slots.map(_.surface) == inputs.scene.slots.map(_.surface), "slot order differs from the scene")
    compiled.copy(slots = inputs.scene.slots, camera = inputs.scene.camera, surfaceCameras = Map.empty,
      viewportFit = SurfaceViewportFit.Fill,
      meshes = compiled.meshes.map: packet =>
        val original = inputs.scene.meshes.find(_.surface == packet.surface).get
        require(java.util.Arrays.equals(packet.positions.unsafeArray, original.positions.unsafeArray), "compiled positions differ from the scene")
        require(java.util.Arrays.equals(packet.indices.unsafeArray, original.indices.unsafeArray), "compiled indices differ from the scene")
        packet.copy(normals = original.normals))

  def scalarPlan(inputs: Inputs, values: Map[SurfaceId, Array[Double]], cutoff: Double, lighting: SurfaceLighting): SurfaceRenderPlan =
    val over = overlayMapping(inputs.range, cutoff)
    val under = underlayMapping(inputs.sulcLimit)
    compileWith(inputs, lighting, (mesh, geometry) => Vector(
      SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe(s"cortex-${hemi(mesh.surface)}"), mesh.surface, geometry,
        inputs.sulc(mesh.surface), under).toOption.get,
      SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe(s"result-${hemi(mesh.surface)}"), mesh.surface, geometry,
        values(mesh.surface), over).toOption.get))

  def finalPlan(inputs: Inputs, values: Map[SurfaceId, Array[Double]], cutoff: Double, lighting: SurfaceLighting): SurfaceRenderPlan =
    val over = overlayMapping(inputs.range, cutoff)
    val under = underlayMapping(inputs.sulcLimit)
    compileWith(inputs, lighting, (mesh, geometry) => Vector(
      SurfaceLayer.packedRgba(SurfaceLayerId.unsafe(s"cortex-${hemi(mesh.surface)}"), mesh.surface, geometry,
        inputs.sulc(mesh.surface).toVector.map(under.color)).toOption.get,
      SurfaceLayer.packedRgba(SurfaceLayerId.unsafe(s"result-${hemi(mesh.surface)}"), mesh.surface, geometry,
        values(mesh.surface).toVector.map(over.color)).toOption.get))

  private def argb(image: WritableImage): Array[Int] =
    val pixels = new Array[Int](Width * Height)
    image.getPixelReader.getPixels(0, 0, Width, Height, PixelFormat.getIntArgbInstance(), pixels, 0, Width)
    pixels

  private def writePng(path: Path, pixels: Array[Int]): Unit =
    val image = new BufferedImage(Width, Height, BufferedImage.TYPE_INT_ARGB)
    image.setRGB(0, 0, Width, Height, pixels, 0, Width)
    require(ImageIO.write(image, "png", path.toFile), s"could not write $path")

  private def signature(pixels: Array[Int]): Long =
    var hash = 0xcbf29ce484222325L
    var index = 0
    while index < pixels.length do
      hash = (hash ^ pixels(index).toLong) * 0x100000001b3L
      index += 1
    hash

  private def stats(values: Vector[Double]): String =
    val sorted = values.sorted
    def at(p: Double) = sorted(math.min(sorted.length - 1, (sorted.length * p).toInt))
    f"""{"n":${sorted.length},"median":${at(0.5)}%.3f,"p90":${at(0.9)}%.3f,"max":${sorted.last}%.3f,"min":${sorted.head}%.3f}"""

  private def origin(name: String): String = Class.forName(name).getProtectionDomain.getCodeSource.getLocation.toString

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
    val material = origin("com.sun.prism.es2.ES2PhongMaterial")
    val shader = origin("com.sun.prism.es2.ES2PhongShader")
    Option(System.getProperty("probe.expectedOrigin")).foreach(expected =>
      require(material == expected && shader == expected, s"unexpected material/shader origin: $material / $shader"))
    val lightingName = if lit then "lit" else "unlit"
    val label = s"$mode-$encoding-$sceneName-$lightingName-$antialiasing"
    val inputs = load(inputsDir, sceneName)
    val lighting = if lit then SurfaceLighting.Default else SurfaceLighting.Unlit
    val compileStarted = System.nanoTime()
    def variant(cutoff: Double, values: Map[SurfaceId, Array[Double]]): SurfaceRenderPlan = mode match
      case "scalar" => scalarPlan(inputs, values, cutoff, lighting)
      case "final" => finalPlan(inputs, values, cutoff, lighting)
      case other => throw new IllegalArgumentException(s"no variants for mode $other")
    val plan = mode match
      case "scene" => readScene(inputsDir.resolve(s"scene-$sceneName-$lightingName.plan"))._1
      case _ => variant(inputs.cutoff, inputs.values)
    val compileMs = (System.nanoTime() - compileStarted) / 1e6
    val missingFaces = if mode == "scalar" then plan.meshes.map(mesh => SurfaceScalarLookup.plan(plan, mesh).toOption.get.missingFaces).sum else 0
    // The portable raster is the interpolation reference for this exact plan.
    val raster = Option.when(System.getProperty("probe.raster", "true") == "true"):
      val started = System.nanoTime()
      val result = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(Width, Height),
        SurfaceRasterStyle(culling = TriangleCulling.None)).fold(e => throw new IllegalStateException(e.toString), identity)
      val pixels = Array.tabulate(Width * Height): index =>
        val c = result.image.pixelUnsafe(index % Width, index / Width)
        (c.alpha << 24) | (c.red << 16) | (c.green << 8) | c.blue
      writePng(output.resolve(s"raster-$mode-$sceneName-$lightingName.png"), pixels)
      (result, (System.nanoTime() - started) / 1e6)
    val (palette, mapChange) =
      if bench then
        val other = loadValues(inputsDir, if sceneName == "beta" then "fir" else "beta", plan.meshes.map(_.surface))
        (Some(variant(inputs.cutoff * 1.5, inputs.values)), Some(variant(inputs.cutoff, other)))
      else (None, None)
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
          writePng(output.resolve(s"$label.png"), pixels)
          val chunks = backend.chunks
          val textureBytes = chunks.map(c => c.atlas.width.toLong * c.atlas.height * 4).sum
          val textures = chunks.map(c => s"${c.atlas.width}x${c.atlas.height}").mkString("[\"", "\",\"", "\"]")
          val meshBytes = chunks.map(c => (c.mesh.getPoints.size.toLong + c.mesh.getNormals.size + c.mesh.getTexCoords.size + c.mesh.getFaces.size) * 4).sum
          val renderedFaces = chunks.map(_.renderedFaceCount).sum
          val points = chunks.map(_.mesh.getPoints.size / 3).sum
          val texCoords = chunks.map(_.mesh.getTexCoords.size / 2).sum
          // Native picks against the raster reference on a fixed pixel lattice.
          val pickRecord = raster.fold("null"): (reference, _) =>
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
            if !bench then "null"
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
                  s""""atlasUpdates":[${receipts.map(_.atlasUpdates).distinct.mkString(",")}],"geometryRebuilds":${receipts.count(_.dirty.geometry)},"frameChangeAndRestoreVerified":true}"""
              s"{${alternate("paletteCutoff", palette.get)},${alternate("mapValues", mapChange.get)}}"
          record = s"""{"label":"$label","mode":"$mode","encoding":"$encoding","scene":"$sceneName","lighting":"$lightingName",""" +
            s""""antialiasing":"$antialiasing","width":$Width,"height":$Height,"range":${inputs.range},"cutoff":${inputs.cutoff},""" +
            s""""sulcLimit":${inputs.sulcLimit},"overlayVerticesCheckedAgainstExporter":${inputs.overlayChecked},""" +
            s""""underlayMismatchesAgainstScene":${inputs.underlayMismatches},"missingFaces":$missingFaces,""" +
            s""""renderedFaces":$renderedFaces,"points":$points,"textureCoordinates":$texCoords,"textures":$textures,""" +
            s""""textureBytes":$textureBytes,"meshArrayBytes":$meshBytes,"planCompileMs":$compileMs,"buildMs":$buildMs,""" +
            s""""firstSnapshotMs":$firstMs,"warmSnapshotMs":${stats(warm)},"distinctColours":$colours,"signature":$base,""" +
            s""""rasterMs":${raster.fold("null")(_._2.toString)},"picks":$pickRecord,"bench":$benchRecord,""" +
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
      println(s"scalar_lut_cortex=$record")
      println("PASS cortex snapshot written")
    finally Platform.exit()
