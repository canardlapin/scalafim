package scalafim.surface.view.javafx

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

private[javafx] object JavaFxAffineFixture:
  val surface: SurfaceId = SurfaceId.unsafe("affine")
  val config: JavaFxAtlasConfig = JavaFxAtlasConfig.make(
    maxTextureSize = 64, encoding = JavaFxAtlasEncoding.AffineMidpointOpaque).toOption.get

  def model(colors: Vector[Rgba32], rotation: Int = 0, copies: Int = 1, shift: Double = 0): SurfaceViewerModel =
    val order = Vector(0, 1, 2).drop(rotation) ++ Vector(0, 1, 2).take(rotation)
    val geometry = SurfaceGeometry(TriangleMesh.fromRows(
      Vector(Vector(shift, 0.0, 0.0), Vector(1.0 + shift, 0.0, 0.0), Vector(shift, 1.0, 0.0)),
      Vector.fill(copies)((order(0), order(1), order(2)))), Hemisphere.Left, SurfaceKind.Inflated)
    val layer = SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("rgb"), surface, geometry, colors).toOption.get
    SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), Vector(layer)).toOption.get

  def plan(model: SurfaceViewerModel): SurfaceRenderPlan =
    SurfaceCompiler.compile(model, SurfaceViewerState.initial(model)).toOption.get

  val rgb: Vector[Rgba32] = Vector(Rgba32.unsafe(255, 0, 0), Rgba32.unsafe(0, 255, 0), Rgba32.unsafe(0, 0, 255))

class JavaFxAffineAtlasSuite extends munit.FunSuite:
  import JavaFxAffineFixture.*

  /** Locate the physical point in the actual uploaded children, then sample
    * the actual atlas with an independent bilinear sampler. No implementation
    * colour weights or child barycentric tables are used by this oracle.
    */
  private def sample(chunk: JavaFxSurfaceChunk, x: Double, y: Double, channel: Int): Double =
    val mesh = chunk.mesh
    val faces = mesh.getFaces
    val points = mesh.getPoints
    val uv = mesh.getTexCoords
    var face = 0
    while face < chunk.renderedFaceCount do
      val i = faces.get(face * 9) * 3
      val j = faces.get(face * 9 + 3) * 3
      val k = faces.get(face * 9 + 6) * 3
      val ax = points.get(i).toDouble
      val ay = points.get(i + 1).toDouble
      val bx = points.get(j).toDouble - ax
      val by = points.get(j + 1).toDouble - ay
      val cx = points.get(k).toDouble - ax
      val cy = points.get(k + 1).toDouble - ay
      val determinant = bx * cy - by * cx
      val b = ((x - ax) * cy - (y - ay) * cx) / determinant
      val c = (bx * (y - ay) - by * (x - ax)) / determinant
      val a = 1 - b - c
      if a >= -1e-10 && b >= -1e-10 && c >= -1e-10 then
        def coordinate(axis: Int): Double =
          a * uv.get(faces.get(face * 9 + 2) * 2 + axis) +
            b * uv.get(faces.get(face * 9 + 5) * 2 + axis) +
            c * uv.get(faces.get(face * 9 + 8) * 2 + axis)
        val tx = coordinate(0) * chunk.atlas.width - 0.5
        val ty = coordinate(1) * chunk.atlas.height - 0.5
        val ix = math.floor(tx).toInt.max(0).min(chunk.atlas.width - 2)
        val iy = math.floor(ty).toInt.max(0).min(chunk.atlas.height - 2)
        val u = tx - ix
        val v = ty - iy
        def pixel(dx: Int, dy: Int): Double =
          (chunk.atlas.image.getPixelReader.getArgb(ix + dx, iy + dy) >>> channel) & 255
        return (1-u)*(1-v)*pixel(0,0) + u*(1-v)*pixel(1,0) +
          (1-u)*v*pixel(0,1) + u*v*pixel(1,1)
      face += 1
    fail(s"sample ($x,$y) did not intersect an uploaded child")

  test("actual affine tiles preserve saturated colours and cyclic face ordering including diagonal edges"):
    val random = new scala.util.Random(20260909)
    val triples = Vector(Vector(Rgba32.unsafe(0,0,0), Rgba32.unsafe(255,255,255), Rgba32.unsafe(255,255,255)), rgb) ++
      Vector.fill(64)(Vector.fill(3)(Rgba32.unsafe(random.nextInt(256), random.nextInt(256), random.nextInt(256))))
    val samples = Vector((0.5,0.5),(0.48,0.48),(0.0,0.0),(1.0,0.0),(0.0,1.0)) ++
      Vector.fill(40):
        val x = random.nextDouble()
        val y = random.nextDouble()
        if x + y <= 1 then (x,y) else (1-x,1-y)
    for colors <- triples; rotation <- 0 until 3 do
      val render = JavaFxSurfaceProbe.compile(plan(model(colors, rotation)), config = config).toOption.get
      for (x,y) <- samples; (shift, packedShift) <- Vector((16,24),(8,16),(0,8)) do
        val expected = colors.zip(Vector(1-x-y,x,y)).map((color,weight) =>
          ((color.toPackedInt >>> packedShift) & 255) * weight).sum
        assertEqualsDouble(sample(render.chunks.head,x,y,shift),expected,0.50001)

  test("independent sampler retains the known legacy diagonal failure as a discriminating control"):
    val colors = Vector(Rgba32.unsafe(0,0,0),Rgba32.unsafe(255,255,255),Rgba32.unsafe(255,255,255))
    val render = JavaFxSurfaceProbe.compile(plan(model(colors))).toOption.get
    assertEqualsDouble(sample(render.chunks.head,0.5,0.5,16),233.75,1e-10)
    assertEqualsDouble(sample(render.chunks.head,0.48,0.48,16),228.344,1e-10)

  test("subdivision respects atlas chunks, original face identity, winding and actual resource counts"):
    val render = JavaFxSurfaceProbe.compile(plan(model(rgb,copies=300)),config=config).toOption.get
    assertEquals(render.chunks.map(_.faceCount),Vector(256,44))
    assertEquals(render.receipt.facesUploaded,1200)
    assertEquals(render.receipt.verticesUploaded,1800)
    assertEquals(render.receipt.atlasPixels,4864L)
    for chunk <- render.chunks do
      assertEquals(chunk.mesh.getFaces.size(),chunk.renderedFaceCount * 9)
      assertEquals(chunk.packetFace(-1),None)
      assertEquals(chunk.packetFace(chunk.renderedFaceCount),None)
      for face <- 0 until chunk.renderedFaceCount do
        assertEquals(chunk.packetFace(face),Some(chunk.faceStart + face / 4))
        val f = chunk.mesh.getFaces
        val p = chunk.mesh.getPoints
        val a = f.get(face*9)*3
        val b = f.get(face*9+3)*3
        val c = f.get(face*9+6)*3
        val area = (p.get(b)-p.get(a))*(p.get(c+1)-p.get(a+1)) - (p.get(b+1)-p.get(a+1))*(p.get(c)-p.get(a))
        assertEqualsDouble(area.toDouble,0.25,1e-10)

  test("colour and geometry updates retain topology and recompute midpoint data"):
    val original = plan(model(rgb))
    val changed = plan(model(rgb.reverse))
    val shifted = plan(model(rgb.reverse,shift=3))
    val render = JavaFxSurfaceProbe.compile(original,config=config).toOption.get
    val mesh = render.chunks.head.mesh
    val atlas = render.chunks.head.atlas
    val colors = render.updateColors(changed).toOption.get
    assert(!colors.geometryRebuilt)
    assertEquals(colors.dirtyPixels,16L)
    assertEqualsDouble(sample(render.chunks.head,0.2,0.3,16),0.3*255,0.5)
    val geometry = render.updateGeometry(shifted).toOption.get
    assertEquals(geometry.verticesUpdated,6)
    assertEquals(geometry.bytesUpdated,144L)
    assertEqualsDouble(mesh.getPoints.get(9).toDouble,3.5,0)
    assertEqualsDouble(mesh.getPoints.get(12).toDouble,3.5,0)
    assertEqualsDouble(mesh.getPoints.get(15).toDouble,3.0,0)
    assert(mesh eq render.chunks.head.mesh)
    assert(atlas eq render.chunks.head.atlas)

  test("encoding and expanded resource budget are admitted before native allocation"):
    assert(JavaFxAtlasConfig.make(tileSize=8,encoding=JavaFxAtlasEncoding.AffineMidpointOpaque).isLeft)
    assert(JavaFxAtlasConfig.make(maxRenderedFaces=0).isLeft)
    assert(JavaFxAtlasConfig.make(maxTextureSize=Int.MaxValue,encoding=JavaFxAtlasEncoding.AffineMidpointOpaque).isLeft)
    val limited = JavaFxAtlasConfig.make(encoding=JavaFxAtlasEncoding.AffineMidpointOpaque,maxRenderedFaces=3).toOption.get
    val refused = JavaFxSurfaceProbe.compile(plan(model(rgb)),config=limited)
    assert(refused.left.toOption.get.message.contains("rendered faces 4 exceed budget 3"))

  test("opaque gate examines final channels and constant updates keep the same midpoint mesh"):
    assert(!JavaFxAffineAtlas.opaque(Map(surface -> Array(Rgba32.unsafe(1,2,3,127).toPackedInt))))
    val constant = plan(model(Vector.fill(3)(Rgba32.unsafe(200,100,50))))
    val render = JavaFxSurfaceProbe.compile(constant,config=config).toOption.get
    val native = render.chunks.head.mesh
    val update = render.updateColors(plan(model(rgb))).toOption.get
    assertEquals(update.textureCoordinateBytesUpdated,96L)
    assertEquals(render.updateColors(constant).toOption.get.textureCoordinateBytesUpdated,96L)
    assert(native eq render.chunks.head.mesh)

/** Explicit native dispatch/pick coverage without a visible Stage. */
object JavaFxAffineAtlasProbe:
  def main(args: Array[String]): Unit =
    import java.util.concurrent.{CountDownLatch, TimeUnit}
    import _root_.javafx.application.Platform
    import _root_.javafx.geometry.{Point2D, Point3D}
    import _root_.javafx.scene.{Group, Scene, SceneAntialiasing}
    import _root_.javafx.scene.input.PickResult
    import JavaFxAffineFixture.*
    val done = new CountDownLatch(1)
    @volatile var failure: Option[Throwable] = None
    Platform.startup(() => ())
    Platform.runLater: () =>
      try
        val fixture = model(rgb,copies=300)
        val state = SurfaceViewerState.initial(fixture)
        val backend = JavaFxSurfaceBackend.create(config).toOption.get
        try
          backend.render(plan(fixture)).toOption.get
          val sub = backend.newSubScene(JavaFxSnapshotConfig.make(192,192,SceneAntialiasing.DISABLED).toOption.get).toOption.get
          val host = new Scene(new Group(sub),192,192)
          host.getRoot.applyCss()
          host.getRoot.layout()
          backend.snapshot(JavaFxSnapshotConfig.make(192,192,SceneAntialiasing.DISABLED).toOption.get).toOption.get
          val controller = JavaFxSurfaceController.attach(fixture,state,backend,sub).toOption.get
          try
            var checked = 0
            for chunk <- backend.chunks; local <- 0 until chunk.renderedFaceCount do
              val xy = Vector((0.1,0.1),(0.8,0.1),(0.1,0.8),(0.4,0.3))(local % 4)
              val pick = controller.pick(new PickResult(chunk.view,new Point3D(xy._1,xy._2,0),1,local,Point2D.ZERO)).toOption.get
              require(pick.face.index == chunk.faceStart + local/4)
              require(math.abs(pick.barycentricA-(1-xy._1-xy._2)) < 1e-7)
              require(math.abs(pick.barycentricB-xy._1) < 1e-7)
              require(math.abs(pick.barycentricC-xy._2) < 1e-7)
              require(pick.vertex.index == Vector(0,1,2,1)(local % 4))
              checked += 1
            println(s"PASS: $checked child picks preserve original face, vertex and barycentric identity across atlas chunks; no Stage shown")
          finally controller.dispose()
        finally backend.dispose()
      catch case error: Throwable => failure = Some(error)
      finally done.countDown()
    try
      require(done.await(60,TimeUnit.SECONDS),"affine native probe timed out")
      failure.foreach(throw _)
    finally Platform.exit()
