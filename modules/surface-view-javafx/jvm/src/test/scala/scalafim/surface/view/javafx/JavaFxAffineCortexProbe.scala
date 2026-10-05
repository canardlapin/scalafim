package scalafim.surface.view.javafx

import java.io.{BufferedInputStream, DataInputStream}
import java.lang.management.ManagementFactory
import java.awt.image.BufferedImage
import java.security.MessageDigest
import javax.imageio.ImageIO
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.FutureTask
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

  private def prepare(source: Source, settings: Settings, reuse: Option[Prepared] = None): Prepared =
    val geometries = source.plan.meshes.map: m =>
      val existing = reuse.filter(_ => settings.shift == 0).flatMap(_.model.surface(m.surface)).map(_.geometry)
      val geometry = existing.getOrElse:
        val coordinates = m.positions.unsafeArray.map(_.toDouble)
        if settings.shift != 0 then
          var vertex = 0
          while vertex < coordinates.length do
            var axis = 0
            while axis < 3 do
              coordinates(vertex + axis) += settings.shift * source.plan.camera.viewMatrix(axis)
              axis += 1
            vertex += 3
        SurfaceGeometry(TriangleMesh.fromArrays(coordinates, m.indices.unsafeArray),
          if hemi(m.surface) == "lh" then Hemisphere.Left else Hemisphere.Right, SurfaceKind.Inflated)
      m -> geometry
    val assets = reuse.filter(_ => settings.shift == 0).fold(
      geometries.map((m, g) => SurfaceAsset.make(m.surface, g).toOption.get))(_.model.surfaces)
    val layers = geometries.flatMap: (m, g) =>
      val under = source.plan.layers.find(p => p.surface == m.surface && p.layer.value.startsWith("cortex-")).get
      val over = source.plan.layers.find(p => p.surface == m.surface && !p.layer.value.startsWith("cortex-")).get
      Vector(
        reuse.filter(_ => settings.shift == 0).flatMap(_.model.layer(under.layer)).getOrElse(
          SurfaceLayer.packedRgba(under.layer, m.surface, g,
            under.colors.unsafeArray.toVector.map(Rgba32.fromPackedInt), opacity=under.opacity, blendMode=under.blendMode).toOption.get),
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
      val original = reuse.filter(_ => settings.shift == 0).flatMap(_.plan.meshes.find(_.surface == packet.surface))
        .getOrElse(source.plan.meshes.find(_.surface == packet.surface).get)
      require(java.util.Arrays.equals(packet.indices.unsafeArray, original.indices.unsafeArray))
      if settings.shift == 0 then require(java.util.Arrays.equals(packet.positions.unsafeArray, original.positions.unsafeArray))
      packet.copy(resourceKey=original.resourceKey,
        positions=if reuse.nonEmpty && settings.shift == 0 then original.positions else packet.positions,
        indices=original.indices, normals=original.normals,
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

  private def maxChannelDifference(a: Array[Int], b: Array[Int]): Int =
    var maximum = 0; var i = 0
    while i < a.length do
      var shift = 0
      while shift <= 24 do
        maximum = math.max(maximum, math.abs(((a(i) >>> shift) & 255) - ((b(i) >>> shift) & 255)))
        shift += 8
      i += 1
    maximum

  private def invertedComposite(plan: SurfaceRenderPlan): SurfaceRenderPlan =
    val colors = JavaFxSurfaceProbe.compositeColors(plan, JavaFxMaterialMode.Unlit, JavaFxAtlasLighting.NativePhong)
    val layers = plan.meshes.map: mesh =>
      val inverted = colors(mesh.surface).map(value => value ^ 0xffffff00)
      SurfaceLayerPacket(SurfaceLayerId.unsafe("adversarial-final-rgb:" + mesh.surface.value), mesh.surface,
        SurfaceResourceKey("adversarial-final-rgb:" + mesh.surface.value), new IntBufferView(inverted),
        DisplayOpacity.unsafe(1), DisplayBlendMode.Normal)
    plan.copy(layers = layers, receipt = plan.receipt.copy(layerKeys = layers.map(_.resourceKey)))

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

  private def verifyRuntime(): Unit =
    val expected = Option(System.getProperty("probe.expectedOrigin")).getOrElse(
      throw new IllegalStateException("diagnostic runtime origin pin is required"))
    for name <- Vector("com.sun.prism.es2.ES2PhongMaterial", "com.sun.prism.es2.ES2PhongShader") do
      require(Class.forName(name).getProtectionDomain.getCodeSource.getLocation.toString == expected,
        s"unexpected $name origin")

  private def onFx[A](operation: => A): A =
    val task = new FutureTask[A](() => operation)
    Platform.runLater(task)
    task.get(300, TimeUnit.SECONDS)

  private final case class Publication(receipt: JavaFxInterpretReceipt, pixels: Array[Int],
      queueNanos: Long, publishNanos: Long, snapshotNanos: Long, pixelCopyNanos: Long,
      allocated: Long, faces: Long, atlasBytes: Long, meshBytes: Long,
      heapUsed: Long, heapCommitted: Long, heapMax: Long)

  /** Worker input -> public scalar/model/plan -> optional CPU atlas preparation
    * -> queued FX publication -> synchronous snapshot. Node.snapshot includes
    * draw/readback; this does not measure the PLS consumer's full FX callback.
    */
  private def benchmarkMain(source: Source, alternate: Source, output: Path, lighting: SurfaceLighting,
      config: JavaFxAtlasConfig, sceneName: String, lightingName: String, antialiasing: SceneAntialiasing): Unit =
    require(source.plan.meshes.length == alternate.plan.meshes.length)
    source.plan.meshes.zip(alternate.plan.meshes).foreach: (left, right) =>
      require(left.surface == right.surface && java.util.Arrays.equals(left.positions.unsafeArray, right.positions.unsafeArray) &&
        java.util.Arrays.equals(left.normals.unsafeArray, right.normals.unsafeArray) && java.util.Arrays.equals(left.indices.unsafeArray, right.indices.unsafeArray),
        s"alternate source geometry differs for ${left.surface.value}")
    val initial = prepare(source, Settings(lighting, source.cutoff))
    val allocation = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    require(allocation.isThreadAllocatedMemorySupported)
    if !allocation.isThreadAllocatedMemoryEnabled then allocation.setThreadAllocatedMemoryEnabled(true)
    Platform.startup(() => ())
    try
      val (backend, scene, host) = onFx:
        verifyRuntime()
        val backend = JavaFxSurfaceBackend.createDiagnostic(config).toOption.get
        backend.render(initial.plan).toOption.get
        val scene = backend.newSubScene(JavaFxSnapshotConfig.make(Width, Height, antialiasing).toOption.get).toOption.get
        val host = new FxScene(new Group(scene), Width, Height)
        host.getRoot.applyCss(); host.getRoot.layout()
        (backend, scene, host)
      try
        val baseline = onFx(pixels(scene))
        require(baseline.distinct.length > 32, "blank/degenerate cortex")
        require(java.util.Arrays.equals(baseline, onFx(pixels(scene))), "repeat native baseline differs")
        save(output.resolve("initial.png"), baseline)
        val beforePicks = onFx(checkPicks(initial, source, backend, scene))
        val rows = Vector.newBuilder[String]
        val variants = Vector("palette", "cutoff", "opacity", "map", "adversarialFinalRgb", "unchanged")
        def target(name: String, restoring: Boolean): SurfaceRenderPlan =
          if name == "unchanged" then initial.plan
          else if restoring then prepare(source, Settings(lighting, source.cutoff), Some(initial)).plan
          else name match
            case "palette" => prepare(source, Settings(lighting, source.cutoff, reverse = true), Some(initial)).plan
            case "cutoff" => prepare(source, Settings(lighting, source.cutoff * 1.5), Some(initial)).plan
            case "opacity" => prepare(source, Settings(lighting, source.cutoff, opacity = .5), Some(initial)).plan
            case "map" => prepare(alternate, Settings(lighting, alternate.cutoff), Some(initial)).plan
            case "adversarialFinalRgb" => invertedComposite(initial.plan)
            case other => throw new IllegalArgumentException(other)
        var warmed = baseline
        var currentCpuPlan = initial.plan
        var stableFaces = -1L
        var publications = 0
        def publish(name: String, restoring: Boolean, worker: Boolean, phase: String, cycle: Int): Unit =
          val inputStarted = System.nanoTime()
          val workerAllocStart = allocation.getThreadAllocatedBytes(Thread.currentThread.getId)
          val planStarted = System.nanoTime()
          val plan = if name == "unchanged" then currentCpuPlan else target(name, restoring)
          val planNanos = System.nanoTime() - planStarted
          val captured = if worker && name != "unchanged" then
            val captureStarted = System.nanoTime()
            val basis = onFx(backend.colorPreparationBasis.toOption.get)
            Some((basis, System.nanoTime() - captureStarted))
          else None
          val atlasStarted = System.nanoTime()
          val prepared = captured.map((basis, _) => basis.prepare(plan).fold(e => throw new IllegalStateException(e.message), identity))
          val atlasNanos = if prepared.nonEmpty then System.nanoTime() - atlasStarted else 0L
          val workerAllocated = allocation.getThreadAllocatedBytes(Thread.currentThread.getId) - workerAllocStart
          val queued = System.nanoTime()
          val result = onFx:
            val queueNanos = System.nanoTime() - queued
            val allocatedBefore = allocation.getThreadAllocatedBytes(Thread.currentThread.getId)
            val fxStarted = System.nanoTime()
            val receipt = prepared.fold(backend.render(plan))(value => backend.render(plan, value)).fold(
              e => throw new IllegalStateException(s"$phase/$name: ${e.message}"), identity)
            val fxNanos = System.nanoTime() - fxStarted
            val fxAllocated = allocation.getThreadAllocatedBytes(Thread.currentThread.getId) - allocatedBefore
            val snapshotStarted = System.nanoTime()
            val image = scene.snapshot(null, new WritableImage(Width, Height))
            val snapshotNanos = System.nanoTime() - snapshotStarted
            val copyStarted = System.nanoTime()
            val observed = new Array[Int](Width * Height)
            image.getPixelReader.getPixels(0, 0, Width, Height,
              _root_.javafx.scene.image.PixelFormat.getIntArgbInstance(), observed, 0, Width)
            val copyNanos = System.nanoTime() - copyStarted
            val heap = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage
            require(backend.pickingPlan.contains(plan))
            Publication(receipt, observed, queueNanos, fxNanos, snapshotNanos, copyNanos, fxAllocated,
              backend.chunks.map(_.renderedFaceCount.toLong).sum,
              backend.chunks.map(c => c.atlas.width.toLong * c.atlas.height * 4).sum,
              backend.chunks.map(c => (c.mesh.getPoints.size().toLong + c.mesh.getNormals.size() + c.mesh.getTexCoords.size() + c.mesh.getFaces.size()) * 4).sum,
              heap.getUsed, heap.getCommitted, heap.getMax)
          val endToEndNanos = System.nanoTime() - inputStarted
          currentCpuPlan = plan
          val changed = result.pixels.indices.count(i => result.pixels(i) != warmed(i))
          if phase == "first" && restoring then
            val maximum = maxChannelDifference(baseline, result.pixels)
            if maximum > 2 then
              save(output.resolve(s"failed-$name-restoration.png"), result.pixels)
              Files.writeString(output.resolve("failure.json"),
                s"""{"variant":"$name","phase":"$phase","maximumChannelDifference":$maximum,"changedPixels":$changed,"faces":${result.faces},"chunksReplaced":${result.receipt.chunksReplaced}}\n""")
            // Pairwise differences across subdivisions are observations, not
            // the frozen footprint-envelope/AA-off-center correctness gate.
            save(output.resolve(s"first-$name-restored.png"), result.pixels)
          else if restoring || name == "unchanged" then
            require(changed == 0, s"$phase/$name restoration differs at $changed pixels")
          else require(changed > 0, s"$name produced no framebuffer change")
          if name == "unchanged" then
            require(result.receipt.atlasUpdates == 0 && result.receipt.geometryUpdates == 0 && result.receipt.textureCoordinateBytesUpdated == 0)
          if phase == "warm" then
            require(result.receipt.textureCoordinateBytesUpdated == 0 && result.receipt.geometryBytesUpdated == 0 &&
              result.receipt.geometryUpdates == 0 && result.receipt.chunksReplaced == 0 && result.faces == stableFaces,
              s"$name warm publication changed retained topology")
          rows += s"""{"phase":"$phase","cycle":$cycle,"variant":"$name","restoring":$restoring,"workerPrepared":$worker,"publicPlanNanos":$planNanos,"basisCaptureWallNanos":${captured.fold(0L)(_._2)},"atlasPreparationNanos":$atlasNanos,"workerAllocatedBytes":$workerAllocated,"fxQueueWaitNanos":${result.queueNanos},"fxPublishNanos":${result.publishNanos},"snapshotNanos":${result.snapshotNanos},"pixelCopyNanos":${result.pixelCopyNanos},"inputToSnapshotNanos":$endToEndNanos,"fxPublishAllocatedBytes":${result.allocated},"changedPixels":$changed,"atlasUpdates":${result.receipt.atlasUpdates},"uvBytes":${result.receipt.textureCoordinateBytesUpdated},"geometryBytes":${result.receipt.geometryBytesUpdated},"chunksReplaced":${result.receipt.chunksReplaced},"faces":${result.faces},"retainedAtlasBytes":${result.atlasBytes},"retainedMeshBytes":${result.meshBytes},"heapUsed":${result.heapUsed},"heapCommitted":${result.heapCommitted},"heapMax":${result.heapMax}}"""
          publications += 1
        variants.filterNot(_ == "unchanged").foreach: name =>
          publish(name, restoring = false, worker = true, phase = "first", cycle = 0)
          publish(name, restoring = true, worker = true, phase = "first", cycle = 0)
        warmed = onFx(pixels(scene))
        stableFaces = onFx(backend.chunks.map(_.renderedFaceCount.toLong).sum)
        save(output.resolve("warmed.png"), warmed)
        for worker <- Vector(false, true); cycle <- 0 until 5; name <- variants; restoring <- Vector(false, true) do
          publish(name, restoring, worker, "warm", cycle)
        require(publications == 130)
        onFx(backend.render(initial.plan).toOption.get)
        val restored = onFx(pixels(scene))
        require(java.util.Arrays.equals(restored, warmed))
        save(output.resolve("restored.png"), restored)
        val afterPicks = onFx(checkPicks(initial, source, backend, scene))
        val fullReference = onFx:
          val full = JavaFxSurfaceProbe.compileRetaining(initial.plan, JavaFxSurfaceProgram.materialMode(initial.plan),
            config, None, retainedChunks = backend.chunks).toOption.get
          require(full.chunks.length == backend.chunks.length)
          full.chunks.zip(backend.chunks).foreach: (a,b) =>
            require(a.surface == b.surface && a.faceStart == b.faceStart && a.faceCount == b.faceCount)
            require(java.util.Arrays.equals(a.mesh.getPoints.toArray(null: Array[Float]), b.mesh.getPoints.toArray(null: Array[Float])))
            require(java.util.Arrays.equals(a.mesh.getNormals.toArray(null: Array[Float]), b.mesh.getNormals.toArray(null: Array[Float])))
            require(java.util.Arrays.equals(a.mesh.getTexCoords.toArray(null: Array[Float]), b.mesh.getTexCoords.toArray(null: Array[Float])))
            require(java.util.Arrays.equals(a.mesh.getFaces.toArray(null: Array[Int]), b.mesh.getFaces.toArray(null: Array[Int])))
            val left = a.atlas.pixelBuffer.getBuffer; val right = b.atlas.pixelBuffer.getBuffer
            require(left.capacity() == right.capacity())
            var i = 0
            while i < left.capacity() do
              require(left.get(i) == right.get(i), s"full retained reference atlas texel differs at $i")
              i += 1
          full.setViewportSize(Width,Height)
          val referenceScene = new SubScene(full.root,Width,Height,true,antialiasing)
          full.attachCamera(referenceScene); referenceScene.setFill(Color.WHITE)
          val referenceHost = new FxScene(new Group(referenceScene),Width,Height)
          referenceHost.getRoot.applyCss(); referenceHost.getRoot.layout()
          pixels(referenceScene)
        require(java.util.Arrays.equals(restored, fullReference), "incremental final frame differs from same-layout full retained rebuild")
        save(output.resolve("full-retained-reference.png"), fullReference)
        val hashes = InputHashes.toVector.sortBy(_._1).map((name, hash) => s"\"$name\":\"$hash\"").mkString("{", ",", "}")
        val record = s"""{"schema":"scalafim.javafx-retained-cortex.v1","diagnosticBackend":true,"consumerFullCallbackMeasured":false,"isolatedGpuDrawMeasured":false,"denseFirstRefinementFootprintQualified":false,"fullRetainedReferenceExact":true,"scene":"$sceneName","lighting":"$lightingName","antialiasing":"$antialiasing","encoding":"${config.encoding}","vertices":${source.plan.meshes.map(_.positions.length.toLong / 3).sum},"sourceFaces":${source.plan.meshes.map(_.indices.length.toLong / 3).sum},"inputs":$hashes,"firstPublications":10,"warmPublications":120,"baselineMaxChannelDifference":${maxChannelDifference(baseline, warmed)},"beforePicks":$beforePicks,"afterPicks":$afterPicks,"updates":${rows.result().mkString("[", ",", "]")}}"""
        Files.writeString(output.resolve("receipt.json"), record + "\n")
        println(s"PASS $sceneName $lightingName retained cortex: 130 measured publications, 2400 native original-face picks")
      finally onFx(backend.dispose())
    finally Platform.exit()

  def main(args: Array[String]): Unit =
    require(args.length == 5 || ((args.length == 6 || args.length == 7) && args(5) == "benchmark"),
      "inputdir scene(beta|fir) output lighting(Unlit|Default|Soft) encoding [benchmark [DISABLED|BALANCED]]")
    val benchmark = args.lift(5).contains("benchmark")
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
    val config = JavaFxAtlasConfig.make(encoding=JavaFxAtlasEncoding.valueOf(args(4)),
      lightingPolicy=JavaFxAtlasLighting.WorldVertexLambert).toOption.get
    if benchmark then require(config.encoding == JavaFxAtlasEncoding.RetainedAffineOpaque,
      "benchmark requires RetainedAffineOpaque so CPU atlas preparation has a stable owner")
    if benchmark then
      benchmarkMain(source, alternate, output, lighting, config, args(1), args(3),
        args.lift(6).getOrElse("BALANCED") match
          case "BALANCED" => SceneAntialiasing.BALANCED
          case "DISABLED" => SceneAntialiasing.DISABLED
          case other => throw new IllegalArgumentException(other))
      return
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
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup(() => ())
    Platform.runLater: () =>
      try
        val backend = JavaFxSurfaceBackend.createDiagnostic(config).toOption.get
        try
          Option(System.getProperty("probe.expectedOrigin")).foreach: expected =>
            for name <- Vector("com.sun.prism.es2.ES2PhongMaterial", "com.sun.prism.es2.ES2PhongShader") do
              val actual = Class.forName(name).getProtectionDomain.getCodeSource.getLocation.toString
              require(actual == expected, s"unexpected $name origin: $actual")
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
          val heap = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage
          val record = s"""{"schema":"scalafim.javafx-affine-cortex.v1","diagnosticBackend":true,"benchmark":$benchmark,"scene":"${args(1)}","lighting":"${args(3)}","encoding":"${args(4)}","vertices":${source.plan.meshes.map(_.positions.length.toLong / 3).sum},"sourceFaces":${source.plan.meshes.map(_.indices.length.toLong / 3).sum},"inputs":$hashes,"heap":{"used":${heap.getUsed},"committed":${heap.getCommitted},"max":${heap.getMax}},"updates":${rows.result().mkString("[", ",", "]")},"beforePicks":$beforePicks,"afterPicks":$afterPicks}"""
          Files.writeString(output.resolve("receipt.json"), record + "\n")
          println(s"PASS ${args(1)} ${args(3)} cortex: 32 verified updates/restorations, 2400 native original-face picks")
        finally backend.dispose()
      catch case error: Throwable => failure = error
      finally done.countDown()
    try
      require(done.await(300, TimeUnit.SECONDS), "cortex probe timed out")
      if failure != null then throw failure.nn
    finally Platform.exit()
