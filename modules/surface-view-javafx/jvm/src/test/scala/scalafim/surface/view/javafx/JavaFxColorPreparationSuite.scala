package scalafim.surface.view.javafx

import intaglio.*
import scalafim.surface.view.*
import java.util.concurrent.{CountDownLatch, FutureTask, TimeUnit}

class JavaFxColorPreparationSuite extends munit.FunSuite:
  import JavaFxAffineFixture.*
  private val config = JavaFxAtlasConfig.make(encoding = JavaFxAtlasEncoding.RetainedAffineOpaque).toOption.get
  private val grey = Vector(1, 100, 101).map(v => Rgba32.unsafe(v, v, v))
  private def atlas(probe: JavaFxSurfaceProbeResult): Vector[Int] =
    val image = probe.chunks.head.atlas.image
    Vector.tabulate(image.getWidth.toInt * image.getHeight.toInt)(i =>
      image.getPixelReader.getArgb(i % image.getWidth.toInt, i / image.getWidth.toInt))

  test("worker preparation owns pixels and agrees with synchronous final lowering"):
    val original = plan(model(grey))
    val target = plan(model(Vector.fill(3)(Rgba32.unsafe(80, 80, 80))))
    val probe = JavaFxSurfaceProbe.compile(original, config = config).toOption.get
    val basis = probe.colorPreparationBasis
    val task = new FutureTask(() => basis.prepare(target).toOption.get)
    val worker = new Thread(task, "retained-colour-test")
    worker.start()
    val prepared = task.get(10, TimeUnit.SECONDS)
    worker.join(1000)
    assert(!worker.isAlive)
    assertEquals(probe.updatePreparedColors(prepared, target, commit = false).toOption.get.textureCoordinateBytesUpdated, 0L)
    val cold = JavaFxSurfaceProbe.compile(target, JavaFxSurfaceProgram.materialMode(target), config).toOption.get
    assertEquals(atlas(probe), atlas(cold))

  test("prepared colour content accepts camera/readout rebasing but rejects changed buffers with unchanged keys"):
    val original = plan(model(grey))
    val probe = JavaFxSurfaceProbe.compile(original, config = config).toOption.get
    val prepared = probe.colorPreparationBasis.prepare(original).toOption.get
    val view = original.camera.viewMatrix.unsafeArray.clone()
    view(3) += 2
    val rebased = original.copy(camera = original.camera.copy(viewMatrix = new FloatBufferView(view)),
      receipt = original.receipt.copy(cameraKey = original.receipt.cameraKey + ":pan"), readouts = Vector.empty)
    assertEquals(probe.validatePrepared(prepared, rebased), Right(()))
    val mesh = original.meshes.head
    val layer = original.layers.head
    val changes = Vector(
      original.copy(meshes = Vector(mesh.copy(positions = new FloatBufferView(mesh.positions.unsafeArray.clone())))),
      original.copy(meshes = Vector(mesh.copy(normals = new FloatBufferView(mesh.normals.unsafeArray.clone())))),
      original.copy(meshes = Vector(mesh.copy(indices = new IntBufferView(mesh.indices.unsafeArray.clone())))),
      original.copy(layers = Vector(layer.copy(colors = new IntBufferView(layer.colors.unsafeArray.clone())))),
      original.copy(layers = Vector(layer.copy(opacity = DisplayOpacity.unsafe(.5)))),
      original.copy(lighting = SurfaceLighting.Unlit))
    val before = atlas(probe)
    changes.foreach(changed => assert(probe.validatePrepared(prepared, changed).isLeft))
    assertEquals(atlas(probe), before)

  test("a basis is reusable across stable colours; target A cannot be published with preparation B"):
    val original = plan(model(grey))
    val target = plan(model(Vector.fill(3)(Rgba32.unsafe(80, 80, 80))))
    val probe = JavaFxSurfaceProbe.compile(original, config = config).toOption.get
    val basis = probe.colorPreparationBasis
    val a = basis.prepare(original).toOption.get
    val b = basis.prepare(target).toOption.get
    assert(probe.updatePreparedColors(b, original, commit = false).isLeft)
    assert(probe.updatePreparedColors(b, target, commit = false).isRight)
    assert(probe.updatePreparedColors(a, original, commit = false).isRight)

  test("foreign and replaced layout work is refused; actual topology is checked despite reused keys"):
    val original = plan(model(grey))
    val first = JavaFxSurfaceProbe.compile(original, config = config).toOption.get
    val other = JavaFxSurfaceProbe.compile(original, config = config).toOption.get
    val prepared = first.colorPreparationBasis.prepare(original).toOption.get
    assert(other.validatePrepared(prepared, original).isLeft)
    val indices = original.meshes.head.indices.unsafeArray.clone()
    indices(0) = 1
    val changed = original.copy(meshes = Vector(original.meshes.head.copy(indices = new IntBufferView(indices))))
    assert(first.colorPreparationBasis.prepare(changed).isLeft)
    first.invalidateColorPreparationBasis()
    assert(first.validatePrepared(prepared, original).isLeft)

  test("worker refinement accounts for persistent faces before any native mutation"):
    val limited = JavaFxAtlasConfig.make(encoding = JavaFxAtlasEncoding.RetainedAffineOpaque, maxRenderedFaces = 3).toOption.get
    val original = plan(model(Vector(0, 255, 255).map(v => Rgba32.unsafe(v, v, v))))
    val target = plan(model(Vector(255, 0, 255).map(v => Rgba32.unsafe(v, v, v))))
    val probe = JavaFxSurfaceProbe.compile(original, config = limited).toOption.get
    val before = atlas(probe)
    assert(probe.colorPreparationBasis.prepare(target).left.toOption.get.message.contains("rendered faces 4 exceed budget 3"))
    assertEquals(atlas(probe), before)
    assertEquals(probe.chunks.head.renderedFaceCount, 1)

/** Deterministic worker/FX interleaving, native refusal atomicity and key-collision
  * controls. Runs in a separate hidden native process; no Stage is created.
  */
object JavaFxColorPreparationProbe:
  def main(args: Array[String]): Unit =
    import _root_.javafx.application.Platform
    import _root_.javafx.scene.{Group, Scene, SceneAntialiasing}
    import JavaFxAffineFixture.*
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup(() => ())
    Platform.runLater: () =>
      try
        val config = JavaFxAtlasConfig.make(encoding = JavaFxAtlasEncoding.RetainedAffineOpaque).toOption.get
        val grey = Vector(1, 100, 101).map(v => Rgba32.unsafe(v, v, v))
        // A pixel-visible orthographic triangle, rather than the tiny default
        // perspective fixture used for toolkit-free buffer/dispatch tests.
        val viewMatrix = Array[Float](1,0,0,0, 0,1,0,0, 0,0,1,-10, 0,0,0,1)
        val projection = Array[Float](1.5f,0,0,0, 0,1.5f,0,0, 0,0,1,0, 0,0,0,1)
        def visible(colors: Vector[Rgba32]): SurfaceRenderPlan =
          val raw = plan(model(colors))
          raw.copy(camera = SurfaceCameraPacket(new FloatBufferView(viewMatrix), new FloatBufferView(projection), 0, 0, 1),
            lighting = SurfaceLighting.Unlit,
            slots = Vector(SurfaceViewSlot(surface, SurfaceViewport(0,0,1,1), -.5, -.5, 0)),
            receipt = raw.receipt.copy(cameraKey = "visible-orthographic"))
        val original = visible(grey)
        val target = visible(Vector.fill(3)(Rgba32.unsafe(80, 80, 80)))
        val snapshot = JavaFxSnapshotConfig.make(192, 192, SceneAntialiasing.DISABLED).toOption.get
        def pixels(backend: JavaFxSurfaceBackend): Vector[Int] =
          val image = backend.snapshot(snapshot).toOption.get
          Vector.tabulate(192 * 192)(i => image.getPixelReader.getArgb(i % 192, i / 192))
        val backend = JavaFxSurfaceBackend.createDiagnostic(config).toOption.get
        val cold = JavaFxSurfaceBackend.createDiagnostic(config).toOption.get
        try
          backend.render(original).toOption.get
          val scene = backend.newSubScene(snapshot).toOption.get
          val host = new Scene(new Group(scene), 192, 192)
          host.getRoot.applyCss(); host.getRoot.layout()
          val baseline = pixels(backend)
          require(baseline.distinct.length > 1)
          val basis = backend.colorPreparationBasis.toOption.get
          val started = new CountDownLatch(1)
          val resume = new CountDownLatch(1)
          val task = new FutureTask(() =>
            started.countDown()
            require(resume.await(10, TimeUnit.SECONDS))
            basis.prepare(target).toOption.get)
          val worker = new Thread(task, "retained-native-worker")
          worker.start()
          require(started.await(10, TimeUnit.SECONDS))
          val view = original.camera.viewMatrix.unsafeArray.clone()
          view(3) += .1f
          val camera = original.copy(camera = original.camera.copy(viewMatrix = new FloatBufferView(view)),
            receipt = original.receipt.copy(cameraKey = original.receipt.cameraKey + ":pan"))
          backend.render(camera).toOption.get
          resume.countDown()
          val prepared = task.get(10, TimeUnit.SECONDS)
          worker.join(1000)
          require(!worker.isAlive)
          val rebased = target.copy(camera = camera.camera, receipt = target.receipt.copy(cameraKey = camera.receipt.cameraKey))
          val receipt = backend.render(rebased, prepared).toOption.get
          require(receipt.geometryUpdates == 0 && receipt.textureCoordinateBytesUpdated == 0)
          cold.render(rebased).toOption.get
          val coldScene = cold.newSubScene(snapshot).toOption.get
          val coldHost = new Scene(new Group(coldScene), 192, 192)
          coldHost.getRoot.applyCss(); coldHost.getRoot.layout()
          require(pixels(backend) == pixels(cold), "worker publication differs from cold final frame")
          val stable = pixels(backend)
          require(backend.render(original, prepared).isLeft, "wrong target accepted on a no-op/key path")
          require(pixels(backend) == stable && backend.pickingPlan.contains(rebased))
          val foreign = cold.colorPreparationBasis.toOption.get.prepare(rebased).toOption.get
          require(backend.render(rebased, foreign).isLeft)
          val newTopology = rebased.copy(receipt = rebased.receipt.copy(meshKeys = Vector(SurfaceResourceKey("different-topology"))))
          require(backend.render(newTopology, foreign).isLeft, "foreign preparation ignored on a full-rebuild path")
          val empty = JavaFxSurfaceBackend.createDiagnostic(config).toOption.get
          try require(empty.render(rebased, prepared).isLeft)
          finally empty.dispose()
          backend.render(original).toOption.get
          require(pixels(backend) == baseline)
          // Different pixels with deliberately reused resource keys must still
          // be published when prepared for their exact target buffers.
          val changedColors = original.layers.head.colors.unsafeArray.clone()
          java.util.Arrays.fill(changedColors, Rgba32.unsafe(60, 60, 60).toPackedInt)
          val collision = original.copy(layers = Vector(original.layers.head.copy(colors = new IntBufferView(changedColors))))
          val collisionPrepared = basis.prepare(collision).toOption.get
          require(backend.render(collision, collisionPrepared).toOption.get.atlasUpdates > 0)
          require(pixels(backend) != baseline)
          backend.render(original, basis.prepare(original).toOption.get).toOption.get
          require(pixels(backend) == baseline)
          val moved = original.meshes.head.positions.unsafeArray.clone()
          moved(0) += .1f
          val morph = original.copy(meshes = Vector(original.meshes.head.copy(positions = new FloatBufferView(moved))))
          val morphPrepared = basis.prepare(morph).toOption.get
          require(backend.render(morph, morphPrepared).toOption.get.geometryUpdates > 0)
          val afterMorph = pixels(backend)
          require(backend.render(morph, morphPrepared).isLeft, "geometry-stale basis accepted on no-op")
          require(pixels(backend) == afterMorph && backend.pickingPlan.contains(morph))
          // A -> B geometry -> A is the stale-basis hazard: the old A basis
          // would otherwise describe this restoration as colour-only work.
          val staleRestoration = basis.prepare(original).toOption.get
          require(backend.render(original, staleRestoration).isLeft, "pre-morph A basis accepted after publishing B")
          require(pixels(backend) == afterMorph && backend.pickingPlan.contains(morph))
          val fresh = backend.colorPreparationBasis.toOption.get
          backend.render(original, fresh.prepare(original).toOption.get).toOption.get
          require(pixels(backend) == baseline)
          val disposed = JavaFxSurfaceBackend.createDiagnostic(config).toOption.get
          disposed.render(original).toOption.get
          val disposedWork = disposed.colorPreparationBasis.toOption.get.prepare(original).toOption.get
          disposed.dispose()
          require(disposed.render(original, disposedWork).isLeft, "disposed owner accepted prepared work")
          // Two atlas chunks with independent vertex triples. Refinement of
          // the late chunk must leave the early native resources mounted.
          val vertices = new FloatBufferView(Array[Float](0,0,0, 1,0,0, 0,1,0, 3,0,0, 4,0,0, 3,1,0))
          val normals = new FloatBufferView(Array.fill(6)(Array[Float](0,0,1)).flatten)
          val faces = new IntBufferView((Vector.fill(256)(Vector(0,1,2)) ++ Vector.fill(44)(Vector(3,4,5))).flatten.toArray)
          val packet = original.meshes.head.copy(positions = vertices, normals = normals, indices = faces)
          val projectionTwo = projection.clone(); projectionTwo(0) = .4f
          def twoPlan(first: Vector[Int], second: Vector[Int]): SurfaceRenderPlan =
            val colors = new IntBufferView((first ++ second).map(v => Rgba32.unsafe(v,v,v).toPackedInt).toArray)
            original.copy(meshes = Vector(packet), layers = Vector(original.layers.head.copy(colors = colors)),
              camera = original.camera.copy(projectionMatrix = new FloatBufferView(projectionTwo)),
              slots = Vector(SurfaceViewSlot(surface, SurfaceViewport(0,0,1,1), -2, -.5, 0)),
              receipt = original.receipt.copy(cameraKey = "two-visible-triangles"))
          val twoOriginal = twoPlan(Vector(0,255,255), Vector(0,255,255))
          val twoTarget = twoPlan(Vector(100,100,100), Vector(255,0,255))
          val twoConfig = JavaFxAtlasConfig.make(maxTextureSize = 64,
            encoding = JavaFxAtlasEncoding.RetainedAffineOpaque).toOption.get
          val partial = JavaFxSurfaceBackend.createDiagnostic(twoConfig).toOption.get
          try
            partial.render(twoOriginal).toOption.get
            val partialScene = partial.newSubScene(snapshot).toOption.get
            val partialHost = new Scene(new Group(partialScene),192,192)
            partialHost.getRoot.applyCss(); partialHost.getRoot.layout()
            val old = partial.chunks
            require(old.map(_.renderedFaceCount) == Vector(256,44))
            val oldBasis = partial.colorPreparationBasis.toOption.get
            val prepared = oldBasis.prepare(twoTarget).toOption.get
            val changed = partial.render(twoTarget, prepared).toOption.get
            require(changed.chunksReplaced == 1 && changed.geometryUpdates == 1)
            require(partial.chunks.head eq old.head)
            require(!(partial.chunks(1).view eq old(1).view))
            require(partial.chunks.map(_.renderedFaceCount) == Vector(256,176))
            val replacement = partial.chunks(1)
            require(changed.geometryBytesUpdated == (replacement.mesh.getPoints.size().toLong +
              replacement.mesh.getNormals.size() + replacement.mesh.getFaces.size()) * 4)
            require(changed.textureCoordinateBytesUpdated == replacement.mesh.getTexCoords.size().toLong * 4)
            for chunk <- partial.chunks; face <- 0 until chunk.renderedFaceCount do
              require(chunk.packetFace(face).contains(chunk.faceStart + (if chunk.faceStart == 0 then face else face / 4)))
            val fullBefore = JavaFxSurfaceProbe.compile(twoOriginal, config = twoConfig).toOption.get
            val full = JavaFxSurfaceProbe.compileRetaining(twoTarget, JavaFxMaterialMode.Unlit, twoConfig, Some(fullBefore)).toOption.get
            full.setViewportSize(192,192)
            val fullScene = new _root_.javafx.scene.SubScene(full.root,192,192,true,SceneAntialiasing.DISABLED)
            full.attachCamera(fullScene)
            fullScene.setFill(_root_.javafx.scene.paint.Color.WHITE)
            val fullHost = new Scene(new Group(fullScene),192,192)
            fullHost.getRoot.applyCss(); fullHost.getRoot.layout()
            val fullPixels = fullScene.snapshot(null, new _root_.javafx.scene.image.WritableImage(192,192))
            val partialPixels = pixels(partial)
            require(partialPixels.distinct.length > 1)
            require(partialPixels == Vector.tabulate(192*192)(i => fullPixels.getPixelReader.getArgb(i%192,i/192)),
              "affected-chunk publication differs from full retained rebuild")
            require(partial.render(twoTarget, prepared).isLeft, "pre-refinement work accepted after publication")
            require(partial.render(twoOriginal, oldBasis.prepare(twoOriginal).toOption.get).isLeft)
            val currentBasis = partial.colorPreparationBasis.toOption.get
            val stable = partial.render(twoOriginal, currentBasis.prepare(twoOriginal).toOption.get).toOption.get
            require(stable.chunksReplaced == 0 && stable.geometryUpdates == 0 && stable.textureCoordinateBytesUpdated == 0)
            println("PASS: exactly one affected chunk; unchanged native identities; exact full-rebuild frame; 432 original-face mappings; stale-basis refusal")
          finally partial.dispose()
          val limitedTwo = JavaFxSurfaceBackend.createDiagnostic(JavaFxAtlasConfig.make(maxTextureSize = 64,
            encoding = JavaFxAtlasEncoding.RetainedAffineOpaque, maxRenderedFaces = 400).toOption.get).toOption.get
          try
            limitedTwo.render(twoOriginal).toOption.get
            val limitedScene = limitedTwo.newSubScene(snapshot).toOption.get
            val limitedHost = new Scene(new Group(limitedScene),192,192)
            limitedHost.getRoot.applyCss(); limitedHost.getRoot.layout()
            val old = limitedTwo.chunks
            val before = pixels(limitedTwo)
            require(limitedTwo.render(twoTarget).isLeft)
            require(limitedTwo.chunks.zip(old).forall((a,b) => a eq b))
            require(pixels(limitedTwo) == before && limitedTwo.pickingPlan.contains(twoOriginal))
            println("PASS: late-chunk retained budget refusal preserves all earlier resources, pixels and pick/camera plan")
          finally limitedTwo.dispose()
          println("PASS: worker/camera interleaving, exact rebasing, key-collision updates and stale/foreign refusal on all paths")
        finally
          cold.dispose()
          backend.dispose()
      catch case error: Throwable => failure = error
      finally done.countDown()
    try
      require(done.await(60, TimeUnit.SECONDS), "colour preparation probe timed out")
      if failure != null then throw failure.nn
    finally Platform.exit()
