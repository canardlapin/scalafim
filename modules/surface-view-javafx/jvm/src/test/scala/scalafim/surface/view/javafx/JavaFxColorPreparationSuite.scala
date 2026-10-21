package scalafim.surface.view.javafx

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

private[javafx] object ColorPreparationFixture:
  val surface = SurfaceId.unsafe("left")
  val layer = SurfaceLayerId.unsafe("color")
  val geometry = SurfaceGeometry(TriangleMesh.fromRows(
    Seq(Seq(0.0, 0.0, 0.0), Seq(1.0, 0.0, 0.0), Seq(0.0, 1.0, 0.0)), Seq((0, 1, 2))),
    Hemisphere.Left, SurfaceKind.Inflated)
  val red = Rgba32.unsafe(255, 0, 0)
  val blue = Rgba32.unsafe(0, 0, 255)
  def colors(values: Vector[Rgba32]): SurfaceLayer = SurfaceLayer.packedRgba(layer, surface, geometry, values).toOption.get
  def model: SurfaceViewerModel = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get),
    Vector(colors(Vector.fill(3)(red)))).toOption.get
  def state(model: SurfaceViewerModel): SurfaceViewerState = SurfaceViewerState.initial(model).copy(lighting = SurfaceLighting.Unlit)

class JavaFxColorPreparationSuite extends munit.FunSuite:
  import ColorPreparationFixture.*

  test("CPU preparation has analytic corner colours and constant-cell texture coordinates"):
    val layout = JavaFxAtlasPixelLayout(surface, 0, 1, 4, 4, 4)
    val indices = new IntBufferView(Array(0, 1, 2))
    val rgb = JavaFxAtlasPixels.prepare(layout, indices,
      Array(red.toPackedInt, Rgba32.unsafe(0, 255, 0).toPackedInt, blue.toPackedInt))
    assertEquals(rgb.pixels(12), 0xffff0000)
    assertEquals(rgb.pixels(15), 0xff00ff00)
    assertEquals(rgb.pixels(0), 0xff0000ff)
    val constant = JavaFxAtlasPixels.prepare(layout, indices, Array.fill(3)(Rgba32.unsafe(255, 0, 0, 128).toPackedInt))
    assertEquals(constant.pixels.toVector, Vector.fill(16)(0x80800000))
    assertEquals(constant.coordinates.toVector, Vector.fill(6)(0.5f))

  test("worker output equals ordinary native atlas updates and rejects changed packet inputs"):
    val viewer = model
    val before = SurfaceCompiler.compile(viewer, state(viewer)).toOption.get
    val transaction = SurfaceLayerTransaction.prepare(viewer, state(viewer), Vector(colors(Vector(red, blue, red)))).toOption.get
    val next = transaction.preparedPlan
    val ordinary = JavaFxSurfaceProbe.compile(before).toOption.get
    val worker = JavaFxSurfaceProbe.compile(before).toOption.get
    val basis = worker.colorPreparationBasis(new JavaFxColorPreparationToken)
    val prepared = java.util.concurrent.CompletableFuture.supplyAsync(() => basis.prepare(next)).get().toOption.get
    ordinary.updateColors(next).toOption.get
    worker.updatePreparedColors(prepared, next, commit = false).toOption.get
    val a = ordinary.chunks.head; val b = worker.chunks.head
    for y <- 0 until a.atlas.height; x <- 0 until a.atlas.width do
      assertEquals(b.atlas.image.getPixelReader.getArgb(x, y), a.atlas.image.getPixelReader.getArgb(x, y))
    assertEquals(b.mesh.getTexCoords.toArray(Array.emptyFloatArray).toVector,
      a.mesh.getTexCoords.toArray(Array.emptyFloatArray).toVector)
    assert(!prepared.matches(next.copy(layers = next.layers.map(_.copy(coverage = SurfaceLayerCoverage.Faces(0, 0))))))
    assert(!prepared.matches(next.copy(meshes = next.meshes.map(m => m.copy(normals = new FloatBufferView(m.normals.unsafeArray.clone()))))))
    assert(JavaFxSurfaceBackend.validateCapabilities(next.copy(layers = next.layers.map(_.copy(coverage = SurfaceLayerCoverage.Faces(0, 0))))).isLeft)
    val broken = next.copy(meshes = next.meshes.map(_.copy(indices = new IntBufferView(Array(0, 2, 1)))))
    assert(basis.prepare(broken).isLeft)
    assert(worker.validatePrepared(prepared, broken).isLeft)

  test("native dirt checks actual buffers when caller-visible resource keys are unchanged"):
    val viewer = model
    val before = SurfaceCompiler.compile(viewer, state(viewer)).toOption.get
    val changed = before.copy(layers = before.layers.map(_.copy(colors = new IntBufferView(Array.fill(3)(blue.toPackedInt)))))
    assert(JavaFxSurfaceProgram.compile(Some(before), changed).dirty.layerData)
    val moved = before.copy(meshes = before.meshes.map(m => m.copy(
      positions = new FloatBufferView(m.positions.unsafeArray.map(_ + 1.0f)))))
    assert(JavaFxSurfaceProgram.compile(Some(before), moved).dirty.geometry)
    val reordered = before.copy(meshes = before.meshes.map(_.copy(indices = new IntBufferView(Array(0, 2, 1)))))
    assert(JavaFxSurfaceProgram.compile(Some(before), reordered).commands.exists:
      case JavaFxSurfaceCommand.RebuildGeometry(_) => true
      case _ => false
    )
    val equivalent = before.copy(layers = before.layers.map(l => l.copy(colors = new IntBufferView(l.colors.unsafeArray.clone()))))
    assert(JavaFxSurfaceProgram.compile(Some(before), equivalent).dirty.isClean)

  test("prepared colours remain compatible with a camera-only packet update"):
    val viewer = model
    val before = SurfaceCompiler.compile(viewer, state(viewer)).toOption.get
    val worker = JavaFxSurfaceProbe.compile(before).toOption.get
    val prepared = worker.colorPreparationBasis(new JavaFxColorPreparationToken).prepare(before).toOption.get
    val movedView = before.camera.viewMatrix.unsafeArray.clone()
    movedView(3) = movedView(3) + 0.25f
    val moved = before.copy(surfaceCameras = Map(surface -> before.camera.copy(
      viewMatrix = new FloatBufferView(movedView))))
    assert(JavaFxSurfaceProgram.compile(Some(before), moved).dirty.camera)
    assert(worker.validatePrepared(prepared, moved).isRight)

  test("affine worker preparation retains the native sampler refusal"):
    val viewer = model
    val plan = SurfaceCompiler.compile(viewer, state(viewer)).toOption.get
    val config = JavaFxAtlasConfig.make(encoding = JavaFxAtlasEncoding.AdaptiveAffineOpaque).toOption.get
    val basis = new JavaFxColorPreparationBasis(new JavaFxColorPreparationToken, plan.meshes, Vector.empty, config)
    assertEquals(basis.prepare(plan).swap.toOption, Some(JavaFxSurfaceError.SamplerUnqualified(config.encoding)))

/** Actual Application-Thread publication; no GPU interpolation qualification claim. */
object JavaFxColorPreparationProbe:
  def main(args: Array[String]): Unit =
    import ColorPreparationFixture.*
    val done = new java.util.concurrent.CountDownLatch(1)
    @volatile var failure: Option[Throwable] = None
    def record(error: Throwable): Unit = failure match
      case None => failure = Some(error)
      case Some(previous) => if previous ne error then previous.addSuppressed(error)
    def cleanup(action: => Unit): Unit =
      try action
      catch case error: Throwable => record(error)
    try
      _root_.javafx.application.Platform.startup: () =>
        var backend: Option[JavaFxSurfaceBackend] = None
        var foreign: Option[JavaFxSurfaceBackend] = None
        var controller: Option[JavaFxSurfaceController] = None
        try
          val viewer = model
          val initial = state(viewer)
          val renderer = JavaFxSurfaceBackend.create().toOption.get
          backend = Some(renderer)
          val original = SurfaceCompiler.compile(viewer, initial).toOption.get
          renderer.render(original).toOption.get
          val scene = renderer.newSubScene(JavaFxSnapshotConfig.make(128, 128).toOption.get).toOption.get
          val active = JavaFxSurfaceController.attach(viewer, initial, renderer, scene).toOption.get
          controller = Some(active)
          val transaction = SurfaceLayerTransaction.prepare(viewer, initial, Vector(colors(Vector.fill(3)(blue)))).toOption.get
          val basis = renderer.colorPreparationBasis.toOption.get
          val prepared = java.util.concurrent.CompletableFuture.supplyAsync(() => basis.prepare(transaction.preparedPlan)).get().toOption.get
          active.dispatch(SurfaceViewerAction.SetPan(0.1, 0.2)).toOption.get
          active.dispatch(SurfaceViewerAction.SetSurfaceViewpoints(Map(
            surface -> SurfaceViewpoint.Medial(CorticalHemisphere.Left)
          ))).toOption.get
          active.dispatch(SurfaceViewerAction.Select(surface, VertexId(1))).toOption.get
          val moved = active.state
          val other = JavaFxSurfaceBackend.create().toOption.get
          foreign = Some(other)
          other.render(original).toOption.get
          require(other.render(transaction.preparedPlan, prepared).isLeft, "foreign owner accepted")
          val wrong = other.colorPreparationBasis.toOption.get.prepare(transaction.preparedPlan).toOption.get
          val before = active.plan
          require(active.applyLayerTransaction(transaction, wrong).isLeft, "foreign preparation accepted")
          require(active.plan eq before, "failed publication changed controller plan")
          require(active.state == moved, "failed publication changed controller state")
          val receipt = active.applyLayerTransaction(transaction, prepared).toOption.get
          require(!receipt.dirty.geometry && receipt.atlasUpdates == 1, "warm update uploaded geometry or missed atlas")
          require(active.plan.surfaceCameras.keySet == Set(surface), "warm update lost the live per-surface camera")
          require(active.state == moved && active.plan.readouts.head.vertex == 1, "live camera/selection lost")
          require(active.applyLayerTransaction(transaction, prepared).isLeft, "superseded source model accepted")
          require(renderer.render(transaction.preparedPlan, prepared).isLeft, "stale no-op preparation accepted")
          renderer.render(original).toOption.get
          require(renderer.render(transaction.preparedPlan, prepared).isLeft, "A-B-A revived preparation")
          val collision = original.copy(layers = original.layers.map(l =>
            l.copy(colors = new IntBufferView(Array.fill(3)(blue.toPackedInt)))))
          val collidingPrepared = renderer.colorPreparationBasis.toOption.get.prepare(collision).toOption.get
          val substituted = collision.copy(layers = collision.layers.map(l =>
            l.copy(colors = new IntBufferView(Array.fill(3)(red.toPackedInt)))))
          val priorPixels = renderer.chunks.head.atlas.image.getPixelReader.getArgb(1, 1)
          require(renderer.render(substituted, collidingPrepared).isLeft, "substituted same-key buffers accepted")
          require(renderer.chunks.head.atlas.image.getPixelReader.getArgb(1, 1) == priorPixels,
            "rejected substituted buffers changed displayed pixels")
          val changed = renderer.render(collision, collidingPrepared).toOption.get
          require(changed.atlasUpdates == 1, "same-key colour change skipped atlas upload")
          require(renderer.chunks.head.atlas.image.getPixelReader.getArgb(1, 1) == 0xff0000ff,
            "published same-key plan differs from displayed pixels")
          require(renderer.render(collision, collidingPrepared).isLeft, "same-key update did not expire preparation")
          println("PASS: same-key colour upload; worker preparation; live camera/selection; foreign and stale A-B-A refusal; failed publication preserves controller; no geometry upload")
        catch case error: Throwable => record(error)
        finally
          try cleanup(controller.foreach(_.dispose()))
          finally
            try cleanup(backend.foreach(value => value.dispose().fold(error => throw IllegalStateException(error.message), identity)))
            finally
              try cleanup(foreign.foreach(value => value.dispose().fold(error => throw IllegalStateException(error.message), identity)))
              finally done.countDown()
      val completed = done.await(45, java.util.concurrent.TimeUnit.SECONDS)
      require(completed, "native colour preparation probe timed out")
      failure.foreach(throw _)
    finally _root_.javafx.application.Platform.exit()
