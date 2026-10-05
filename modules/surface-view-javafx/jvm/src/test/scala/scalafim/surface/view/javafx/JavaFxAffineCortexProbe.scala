package scalafim.surface.view.javafx

import java.io.{BufferedInputStream, DataInputStream}
import java.lang.management.ManagementFactory
import java.awt.image.BufferedImage
import java.security.MessageDigest
import javax.imageio.ImageIO
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import _root_.javafx.application.Platform
import _root_.javafx.scene.{Group, Scene as FxScene, SceneAntialiasing, SubScene}
import _root_.javafx.scene.image.WritableImage
import _root_.javafx.scene.paint.Color
import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

/** Candidate-consumer probe. It preserves the source mesh packets, camera and
  * fitted slots; scalar colours are independently re-derived at each vertex.
  * It is a qualification harness, not a production renderer or performance claim.
  */
object JavaFxAffineCortexProbe:
  private val Width = 1350
  private val Height = 762
  private final case class Source(plan: SurfaceRenderPlan, values: Map[SurfaceId, Array[Double]], range: Double, cutoff: Double)
  private def floats(in: DataInputStream): Array[Float] = Array.fill(in.readInt())(in.readFloat())
  private def ints(in: DataInputStream): Array[Int] = Array.fill(in.readInt())(in.readInt())
  private def slots(in: DataInputStream): Vector[SurfaceViewSlot] = Vector.fill(in.readInt()):
    val id = SurfaceId.unsafe(in.readUTF()); val viewport = SurfaceViewport(in.readDouble(), in.readDouble(), in.readDouble(), in.readDouble())
    SurfaceViewSlot(id, viewport, in.readDouble(), in.readDouble(), in.readDouble())
  private def number(json: String, key: String): Double = s"\\\"$key\\\":([-0-9.eE]+)".r.findFirstMatchIn(json).get.group(1).toDouble
  private def hemi(id: SurfaceId): String = if id.value.endsWith("lh") then "lh" else "rh"
  private def f64(path: Path): Array[Double] =
    val b = ByteBuffer.wrap(Files.readAllBytes(path)).order(ByteOrder.BIG_ENDIAN)
    Array.fill(b.remaining / 8)(b.getDouble())

  /** Exact SFIM-SPIKE-PLAN-1 V1 reader: source packet fields, camera and slots only. */
  private def read(dir: Path, scene: String): Source =
    val in = new DataInputStream(new BufferedInputStream(Files.newInputStream(dir.resolve(s"scene-$scene-unlit.plan"))))
    try
      require(in.readUTF() == "SFIM-SPIKE-PLAN-1")
      val metadata = in.readUTF(); slots(in)
      val meshes = Vector.fill(in.readInt()):
        val id = SurfaceId.unsafe(in.readUTF()); val key = in.readUTF(); val geometryKey = in.readUTF()
        val p = floats(in); val n = floats(in); val f = ints(in)
        SurfaceMeshPacket(id, SurfaceResourceKey(key), new FloatBufferView(p), new FloatBufferView(n), new IntBufferView(f), Option.when(geometryKey != key)(SurfaceResourceKey(geometryKey)))
      val layers = Vector.fill(in.readInt()):
        val id = SurfaceLayerId.unsafe(in.readUTF()); val surface = SurfaceId.unsafe(in.readUTF()); val key = in.readUTF()
        SurfaceLayerPacket(id, surface, SurfaceResourceKey(key), new IntBufferView(ints(in)), DisplayOpacity.unsafe(in.readDouble()), DisplayBlendMode.valueOf(in.readUTF()))
      val camera = SurfaceCameraPacket(new FloatBufferView(floats(in)), new FloatBufferView(floats(in)), in.readDouble(), in.readDouble(), in.readDouble())
      val lighting = if in.readUTF() == "unlit" then SurfaceLighting.Unlit else SurfaceLighting.Directional(LightFraction.unsafe(in.readDouble()), LightFraction.unsafe(in.readDouble()), in.readDouble(), in.readDouble(), in.readDouble())
      val clipping = if in.readUTF() == "disabled" then SurfaceClipping.Disabled else SurfaceClipping.NearFar(in.readDouble(), in.readDouble())
      val passes = Vector.fill(in.readInt()):
        SurfaceDrawPass(in.readInt(), SurfaceResourceKey(in.readUTF()), SurfaceResourceKey(in.readUTF()), DisplayBlendMode.valueOf(in.readUTF()))
      val profile = SurfaceProfile(in.readInt(), in.readInt(), in.readInt(), in.readInt(), in.readInt(), in.readLong())
      val receipt = SurfaceRenderReceipt(Vector.fill(in.readInt())(SurfaceResourceKey(in.readUTF())), Vector.fill(in.readInt())(SurfaceResourceKey(in.readUTF())), in.readUTF(), in.readInt(), in.readInt())
      val fitted = Vector.fill(in.readInt()):
        (in.readInt(), in.readInt(), slots(in))
      val fittedSlots = fitted.find(x => x._1 == Width && x._2 == Height).get._3
      val plan = SurfaceRenderPlan(fittedSlots, meshes, layers, camera, lighting, clipping, passes, Scene.empty, Vector.empty, profile, receipt)
      val values = meshes.map(m => m.surface -> f64(dir.resolve(s"values-$scene-${hemi(m.surface)}.f64be"))).toMap
      meshes.foreach(m => require(values(m.surface).length == m.positions.length / 3))
      Source(plan, values, number(metadata, "symmetricRange"), number(metadata, "cutoff"))
    finally in.close()

  private val InputHashes = Map(
    "scene-beta-unlit.plan" -> "d216594d61ab9b0ac147beb079656a5170d586cd1dad0e2fa2894521b9ddc1ea",
    "scene-fir-unlit.plan" -> "9d022f4913128cac7416684554cbcd08326888c3a15683df2530481f8f0c8423",
    "values-beta-lh.f64be" -> "dfd58905ad68b00b8fa97a32ff9b676996d92ba49df1fc7e5e1f92eea030243c",
    "values-beta-rh.f64be" -> "5021639b1cada7f41e71d58d903c05f66adabda1ac3e0b267df26e9657abd22a",
    "values-fir-lh.f64be" -> "5f20a593aac23d81e5ee54755dcc471be7c3b3ef1c80e9854946dba6c37908b1",
    "values-fir-rh.f64be" -> "22bd1dad1aa663ae3e5afc0000d5305041090e8f0de473b1e862ceb4a0ee8e4a")

  private def verifyInputs(dir: Path): Unit =
    InputHashes.foreach: (name, expected) =>
      val digest = MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(dir.resolve(name)))
        .map(byte => f"${byte & 255}%02x").mkString
      require(digest == expected, s"input hash differs: $name")

  private final case class Exporter(range: Double, cutoff: Double, reverse: Boolean) extends Colorizer[Double]:
    def color(value: Double): Rgba32 =
      if !value.isFinite || math.abs(value) < cutoff then Rgba32.unsafe(0, 0, 0, 0)
      else
        val g = math.round(64 + 191 * math.min(1.0, (math.abs(value) - cutoff) / (range - cutoff))).toInt
        if (value > 0) != reverse then Rgba32.unsafe(255, g, 0) else Rgba32.unsafe(0, g, 255)

  private final case class Settings(lighting: SurfaceLighting, cutoff: Double, opacity: Double = 1,
      reverse: Boolean = false, shift: Double = 0)
  private final case class Prepared(plan: SurfaceRenderPlan, model: SurfaceViewerModel, state: SurfaceViewerState)

  private def prepare(source: Source, settings: Settings): Prepared =
    val geometries = source.plan.meshes.map: m =>
      val coordinates = m.positions.unsafeArray.map(_.toDouble)
      if settings.shift != 0 then
        var vertex = 0
        while vertex < coordinates.length do
          coordinates(vertex) += settings.shift
          vertex += 3
      m -> SurfaceGeometry(TriangleMesh.fromArrays(coordinates, m.indices.unsafeArray),
        if hemi(m.surface) == "lh" then Hemisphere.Left else Hemisphere.Right, SurfaceKind.Inflated)
    val assets = geometries.map((m, g) => SurfaceAsset.make(m.surface, g).toOption.get)
    val layers = geometries.flatMap: (m, g) =>
      val under = source.plan.layers.find(p => p.surface == m.surface && p.layer.value.startsWith("cortex-")).get
      val over = source.plan.layers.find(p => p.surface == m.surface && !p.layer.value.startsWith("cortex-")).get
      Vector(
        SurfaceLayer.packedRgba(under.layer, m.surface, g,
          under.colors.unsafeArray.toVector.map(Rgba32.fromPackedInt), opacity=under.opacity, blendMode=under.blendMode).toOption.get,
        SurfaceLayer.scalar(over.layer, m.surface, g, source.values(m.surface),
          Exporter(source.range, settings.cutoff, settings.reverse), opacity=DisplayOpacity.unsafe(settings.opacity),
          blendMode=over.blendMode).toOption.get)
    val model = SurfaceViewerModel.make(assets, layers).toOption.get
    val ids = source.plan.meshes.map(_.surface)
    val state = SurfaceViewerState.initial(model).copy(lighting=settings.lighting,
      layout=SurfaceLayout.Bilateral(ids.find(hemi(_) == "lh").get, ids.find(hemi(_) == "rh").get))
    val compiled = SurfaceCompiler.compile(model, state).toOption.get
    var finiteChecked = 0
    compiled.layers.filterNot(_.layer.value.startsWith("cortex-")).foreach: packet =>
      val numeric = source.values(packet.surface)
      val exported = source.plan.layers.find(p => p.layer == packet.layer).get
      var i = 0
      while i < numeric.length do
        val value = numeric(i)
        val expected =
          if !value.isFinite || math.abs(value) < settings.cutoff then 0
          else
            val intensity = math.round(64 + 191 * math.min(1.0,
              (math.abs(value) - settings.cutoff) / (source.range - settings.cutoff))).toInt
            if (value > 0) != settings.reverse then (255 << 24) | (intensity << 16) | 255
            else (intensity << 16) | (255 << 8) | 255
        require(packet.colors(i) == expected, s"mapped value differs: ${packet.surface.value}:$i")
        if value.isFinite then
          finiteChecked += 1
          if settings.cutoff == source.cutoff && !settings.reverse then
            require(packet.colors(i) == exported.colors(i), s"source finite palette differs: ${packet.surface.value}:$i")
        i += 1
      require(packet.opacity.toDouble == settings.opacity)
    require(finiteChecked > 0)
    val meshes = compiled.meshes.map: packet =>
      val original = source.plan.meshes.find(_.surface == packet.surface).get
      require(java.util.Arrays.equals(packet.indices.unsafeArray, original.indices.unsafeArray))
      if settings.shift == 0 then require(java.util.Arrays.equals(packet.positions.unsafeArray, original.positions.unsafeArray))
      packet.copy(resourceKey=original.resourceKey, normals=original.normals,
        geometryRevision=Some(if settings.shift == 0 then original.geometryKey
          else SurfaceResourceKey(original.geometryKey.value + ":qualification-translation")))
    val drawPasses = compiled.drawPasses.map: pass =>
      val surface = compiled.meshes.find(_.resourceKey == pass.mesh).get.surface
      pass.copy(mesh=meshes.find(_.surface == surface).get.resourceKey)
    val plan = compiled.copy(slots=source.plan.slots, camera=source.plan.camera, meshes=meshes,
      drawPasses=drawPasses, receipt=compiled.receipt.copy(meshKeys=meshes.map(_.resourceKey), cameraKey=source.plan.receipt.cameraKey))
    Prepared(plan, model, state)

  private def pixels(scene: SubScene): Array[Int] =
    val image = scene.snapshot(null, new WritableImage(Width, Height))
    val result = new Array[Int](Width * Height)
    image.getPixelReader.getPixels(0, 0, Width, Height,
      _root_.javafx.scene.image.PixelFormat.getIntArgbInstance(), result, 0, Width)
    result

  private def save(path: Path, pixels: Array[Int]): Unit =
    val image = new BufferedImage(Width, Height, BufferedImage.TYPE_INT_ARGB)
    image.setRGB(0, 0, Width, Height, pixels, 0, Width)
    require(ImageIO.write(image, "png", path.toFile))

  private def checkPicks(prepared: Prepared, source: Source, backend: JavaFxSurfaceBackend, scene: SubScene): String =
    val controller = JavaFxSurfaceController.attachRendered(prepared.model, prepared.state, backend, scene).toOption.get
    try
      require(backend.pickingPlan.contains(prepared.plan), "selection attachment changed the authoritative scene")
      val reference = SurfaceRasterizer.render(prepared.plan, RasterDimensions.unsafe(Width, Height),
        SurfaceRasterStyle(culling=TriangleCulling.None)).toOption.get
      var sampled = 0
      var agreed = 0
      var missing = 0
      var readouts = 0
      var maximumBarycentricError = 0.0
      var y = 5
      while y < Height && sampled < 1200 do
        var x = 7
        while x < Width && sampled < 1200 do
          reference.pick(x, y).toOption.flatten.foreach: expected =>
            sampled += 1
            JavaFxNativePick.at(scene, x + .5, y + .5).flatMap(result => controller.pick(result).toOption) match
              case None => missing += 1
              case Some(actual) =>
                require(actual.surface == expected.surface && actual.face.index == expected.face && actual.vertex.index == expected.vertex,
                  s"native original identity differs at $x,$y: $actual vs $expected")
                val error = Vector(math.abs(actual.barycentricA-expected.barycentricA),
                  math.abs(actual.barycentricB-expected.barycentricB), math.abs(actual.barycentricC-expected.barycentricC)).max
                maximumBarycentricError = math.max(maximumBarycentricError, error)
                require(error <= 1e-3, s"native original barycentrics differ at $x,$y by $error")
                agreed += 1
                val result = prepared.model.layers.find(layer => layer.surfaceId == actual.surface && layer.kind == SurfaceLayerKind.Scalar).get
                require(result.describe(actual.vertex.index, 0) == source.values(actual.surface)(actual.vertex.index).toString)
                if readouts < 8 then
                  val selected = SurfaceViewer.reduce(prepared.model, prepared.state,
                    SurfaceViewerAction.Select(actual.surface, actual.vertex)).toOption.get
                  val readout = SurfaceCompiler.compile(prepared.model, selected).toOption.get.readouts.head
                  require(readout.vertex == actual.vertex.index && readout.surface == actual.surface)
                  require(readout.layerValues.toMap.apply(result.id) == source.values(actual.surface)(actual.vertex.index).toString)
                  readouts += 1
          x += 17
        y += 11
      require(sampled == 1200 && agreed == 1200 && missing == 0 && readouts == 8,
        s"insufficient native picks: sampled=$sampled agreed=$agreed missing=$missing readouts=$readouts")
      s"""{"sampled":$sampled,"agreed":$agreed,"missing":$missing,"selectionReadouts":$readouts,"maximumBarycentricError":$maximumBarycentricError}"""
    finally controller.dispose()

  def main(args: Array[String]): Unit =
    require(args.length == 5, "inputdir scene(beta|fir) output lighting(Unlit|Default|Soft) encoding")
    val dir = Path.of(args(0))
    verifyInputs(dir)
    val source = read(dir, args(1))
    val alternate = read(dir, if args(1) == "beta" then "fir" else "beta")
    val output = Path.of(args(2))
    Files.createDirectories(output)
    val soft = SurfaceLighting.directional(.72, .28, -.4, -.5, .7681145747868608).toOption.get
    val lighting = args(3) match
      case "Unlit" => SurfaceLighting.Unlit
      case "Default" => SurfaceLighting.Default
      case "Soft" => soft
      case other => throw new IllegalArgumentException(other)
    val initial = prepare(source, Settings(lighting, source.cutoff))
    val variants = Vector(
      "palette" -> prepare(source, Settings(lighting, source.cutoff, reverse=true)).plan,
      "cutoff" -> prepare(source, Settings(lighting, source.cutoff * 1.5)).plan,
      "opacity" -> prepare(source, Settings(lighting, source.cutoff, opacity=.5)).plan,
      "map" -> prepare(alternate, Settings(lighting, alternate.cutoff)).plan,
      "morph" -> prepare(source, Settings(lighting, source.cutoff, shift=2)).plan,
      "lighting" -> prepare(source, Settings(if lighting == SurfaceLighting.Unlit then soft else SurfaceLighting.Unlit, source.cutoff)).plan)
    val view = initial.plan.camera.viewMatrix.unsafeArray.clone()
    view(3) += 2f
    val cameraVariant = initial.plan.copy(camera=initial.plan.camera.copy(viewMatrix=new FloatBufferView(view)),
      receipt=initial.plan.receipt.copy(cameraKey=initial.plan.receipt.cameraKey + ":qualification-pan"))
    val config = JavaFxAtlasConfig.make(encoding=JavaFxAtlasEncoding.valueOf(args(4)),
      lightingPolicy=JavaFxAtlasLighting.WorldVertexLambert).toOption.get
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup(() => ())
    Platform.runLater: () =>
      try
        val backend = JavaFxSurfaceBackend.createDiagnostic(config).toOption.get
        try
          backend.render(initial.plan).toOption.get
          val scene = backend.newSubScene(JavaFxSnapshotConfig.make(Width, Height, SceneAntialiasing.BALANCED).toOption.get).toOption.get
          val host = new FxScene(new Group(scene), Width, Height)
          host.getRoot.applyCss()
          host.getRoot.layout()
          val baseline = pixels(scene)
          require(baseline.distinct.length > 32, "blank/degenerate cortex")
          require(java.util.Arrays.equals(baseline, pixels(scene)), "repeat cortex snapshot differs")
          save(output.resolve("baseline.png"), baseline)
          val beforePicks = checkPicks(initial, source, backend, scene)
          val allocated = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
          require(allocated.isThreadAllocatedMemorySupported)
          if !allocated.isThreadAllocatedMemoryEnabled then allocated.setThreadAllocatedMemoryEnabled(true)
          val rows = Vector.newBuilder[String]
          var updates = 0
          val allVariants = variants ++ Vector("camera" -> cameraVariant, "background" -> initial.plan)
          for cycle <- 0 until 2; (name, changed) <- allVariants do
            for restoring <- Vector(false, true) do
              val target = if restoring then initial.plan else changed
              scene.setFill(if name == "background" && !restoring then Color.LIGHTGRAY else Color.WHITE)
              val allocationStart = allocated.getThreadAllocatedBytes(Thread.currentThread.getId)
              val started = System.nanoTime()
              val receipt = backend.render(target).fold(error => throw new IllegalStateException(s"$name: ${error.message}"), identity)
              val elapsed = System.nanoTime() - started
              val renderAllocated = allocated.getThreadAllocatedBytes(Thread.currentThread.getId) - allocationStart
              val shotStart = System.nanoTime()
              val observed = pixels(scene)
              val snapshotNanos = System.nanoTime() - shotStart
              val differences = observed.indices.count(i => observed(i) != baseline(i))
              if restoring then require(differences == 0, s"$name restoration differs at $differences pixels")
              else require(differences > 0, s"$name produced no framebuffer change")
              val retainedAtlasBytes = backend.chunks.map(c => c.atlas.width.toLong * c.atlas.height * 4).sum
              val retainedMeshBytes = backend.chunks.map(c =>
                (c.mesh.getPoints.size().toLong + c.mesh.getNormals.size() + c.mesh.getTexCoords.size() + c.mesh.getFaces.size()) * 4).sum
              val faces = backend.chunks.map(_.renderedFaceCount.toLong).sum
              require(backend.pickingPlan.contains(target))
              require(backend.chunks.forall(c => target.meshes.exists(m => m.surface == c.surface && m.resourceKey == c.meshKey)))
              rows += s"""{"cycle":$cycle,"variant":"$name","restoring":$restoring,"renderNanos":$elapsed,"snapshotNanos":$snapshotNanos,"renderAllocatedBytes":$renderAllocated,"changedPixels":$differences,"atlasUpdates":${receipt.atlasUpdates},"uvBytesUpdated":${receipt.textureCoordinateBytesUpdated},"geometryBytesUpdated":${receipt.geometryBytesUpdated},"renderedFaces":$faces,"retainedAtlasBytes":$retainedAtlasBytes,"retainedMeshBytes":$retainedMeshBytes}"""
              updates += 1
          require(updates == 32)
          val afterPicks = checkPicks(initial, source, backend, scene)
          save(output.resolve("restored.png"), pixels(scene))
          val hashes = InputHashes.toVector.sortBy(_._1).map((name, hash) => s"\"$name\":\"$hash\"").mkString("{", ",", "}")
          val record = s"""{"schema":"scalafim.javafx-affine-cortex.v1","scene":"${args(1)}","lighting":"${args(3)}","encoding":"${args(4)}","vertices":${source.plan.meshes.map(_.positions.length.toLong / 3).sum},"sourceFaces":${source.plan.meshes.map(_.indices.length.toLong / 3).sum},"inputs":$hashes,"updates":${rows.result().mkString("[", ",", "]")},"beforePicks":$beforePicks,"afterPicks":$afterPicks}"""
          Files.writeString(output.resolve("receipt.json"), record + "\n")
          println(s"PASS ${args(1)} ${args(3)} cortex: 32 verified updates/restorations, 2400 native original-face picks")
        finally backend.dispose()
      catch case error: Throwable => failure = error
      finally done.countDown()
    try
      require(done.await(300, TimeUnit.SECONDS), "cortex probe timed out")
      if failure != null then throw failure.nn
    finally Platform.exit()
