package scalafim.surface.view.javafx

import intaglio.*
import scalafim.surface.view.*

class JavaFxAdaptiveAtlasSuite extends munit.FunSuite:
  import JavaFxAffineFixture.*
  private val adaptive = JavaFxAtlasConfig.make(maxTextureSize=64,
    encoding=JavaFxAtlasEncoding.AdaptiveAffineOpaque).toOption.get

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
      val render = JavaFxSurfaceProbe.compile(plan(model(colors, rotation)), config = adaptive).toOption.get
      for (x,y) <- samples; (shift, packedShift) <- Vector((16,24),(8,16),(0,8)) do
        val expected = colors.zip(Vector(1-x-y,x,y)).map((color,weight) =>
          ((color.toPackedInt >>> packedShift) & 255) * weight).sum
        assertEqualsDouble(sample(render.chunks.head,x,y,shift),expected,0.50001)


  test("cyclic anchors avoid needless subdivision without duplicating source vertices"):
    val grey = Vector(Rgba32.unsafe(0,0,0),Rgba32.unsafe(255,255,255),Rgba32.unsafe(255,255,255))
    assertEquals(JavaFxAdaptiveAtlas.anchor(grey(0).toPackedInt,grey(1).toPackedInt,grey(2).toPackedInt),1)
    val render = JavaFxSurfaceProbe.compile(plan(model(grey,copies=300)),config=adaptive).toOption.get
    assertEquals(render.receipt.facesUploaded,300)
    assertEquals(render.receipt.verticesUploaded,6) // three shared vertices per atlas chunk
    assertEquals(render.chunks.map(_.renderedFaceCount),Vector(256,44))
    for chunk <- render.chunks; face <- 0 until chunk.renderedFaceCount do
      assertEquals(chunk.packetFace(face),Some(chunk.faceStart+face))
    assertEqualsDouble(sample(render.chunks.head,0.5,0.5,16),255,1e-8)

  test("saturated RGB uses midpoint fallback and morphs shared source plus synthetic vertices"):
    val render = JavaFxSurfaceProbe.compile(plan(model(rgb,copies=300)),config=adaptive).toOption.get
    assertEquals(render.receipt.facesUploaded,1200)
    assertEquals(render.receipt.verticesUploaded,906)
    val receipt = render.updateGeometry(plan(model(rgb,copies=300,shift=3))).toOption.get
    assertEquals(receipt.verticesUpdated,906)
    for chunk <- render.chunks; face <- 0 until chunk.renderedFaceCount do
      assertEquals(chunk.packetFace(face),Some(chunk.faceStart+face/4))
    assertEqualsDouble(render.chunks.head.mesh.getPoints.get(9).toDouble,3.5,0)

  test("anchor-only colour changes reuse geometry; split changes refuse before mutation"):
    val grey = Vector(Rgba32.unsafe(0,0,0),Rgba32.unsafe(255,255,255),Rgba32.unsafe(255,255,255))
    val render = JavaFxSurfaceProbe.compile(plan(model(grey)),config=adaptive).toOption.get
    val mesh = render.chunks.head.mesh
    val update = render.updateColors(plan(model(grey.reverse))).toOption.get
    assertEquals(update.textureCoordinateBytesUpdated,24L)
    assert(mesh eq render.chunks.head.mesh)
    val before = Array.tabulate(16)(i => render.chunks.head.atlas.image.getPixelReader.getArgb(i%4,i/4))
    assertEquals(render.updateColors(plan(model(rgb))),Left(JavaFxSurfaceError.AtlasLayoutChanged))
    val after = Array.tabulate(16)(i => render.chunks.head.atlas.image.getPixelReader.getArgb(i%4,i/4))
    assertEquals(after.toVector,before.toVector)
    assertEqualsDouble(sample(render.chunks.head,0.5,0.5,16),127.5,1e-8)

  test("resource admission charges actual adaptive faces, including fallback"):
    val limited = JavaFxAtlasConfig.make(encoding=JavaFxAtlasEncoding.AdaptiveAffineOpaque,maxRenderedFaces=3).toOption.get
    assert(JavaFxSurfaceProbe.compile(plan(model(Vector.fill(3)(Rgba32.unsafe(80,90,100)))),config=limited).isRight)
    val refused = JavaFxSurfaceProbe.compile(plan(model(rgb)),config=limited)
    assert(refused.left.toOption.get.message.contains("rendered faces 4 exceed budget 3"))

  test("mixed one/four-child layouts preserve winding and source identities across chunk boundaries"):
    val grey = Vector(Rgba32.unsafe(0,0,0),Rgba32.unsafe(255,255,255),Rgba32.unsafe(255,255,255))
    val vertices = Vector.fill(3)(Vector(Vector(0.0,0.0,0.0),Vector(1.0,0.0,0.0),Vector(0.0,1.0,0.0))).flatten
    val indices = Vector.tabulate(300)(f => ((f%3)*3,(f%3)*3+1,(f%3)*3+2))
    val geometry = scalafim.surface.SurfaceGeometry(scalafim.surface.TriangleMesh.fromRows(vertices,indices),
      scalafim.surface.Hemisphere.Left,scalafim.surface.SurfaceKind.Inflated)
    val layer = SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("mixed"),surface,geometry,grey++rgb++grey.reverse).toOption.get
    val fixture = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface,geometry).toOption.get),Vector(layer)).toOption.get
    val render = JavaFxSurfaceProbe.compile(plan(fixture),config=adaptive).toOption.get
    assertEquals(render.receipt.facesUploaded,600)
    assertEquals(render.receipt.verticesUploaded,318)
    for chunk <- render.chunks do
      var native = 0
      for face <- chunk.faceStart until chunk.faceStart+chunk.faceCount do
        val children = if face%3 == 1 then 4 else 1
        for child <- 0 until children do
          assertEquals(chunk.packetFace(native),Some(face))
          val f = chunk.mesh.getFaces
          val p = chunk.mesh.getPoints
          val a = f.get(native*9)*3
          val b = f.get(native*9+3)*3
          val c = f.get(native*9+6)*3
          val area = (p.get(b)-p.get(a))*(p.get(c+1)-p.get(a+1))-(p.get(b+1)-p.get(a+1))*(p.get(c)-p.get(a))
          assertEqualsDouble(area.toDouble,if children == 4 then 0.25 else 1.0,1e-9)
          native += 1
      assertEquals(native,chunk.renderedFaceCount)
      assertEquals(chunk.packetFace(-1),None)
      assertEquals(chunk.packetFace(native),None)

/** Native update and original-face dispatch, without opening a Stage. */
object JavaFxAdaptiveAtlasProbe:
  def main(args: Array[String]): Unit =
    import java.util.concurrent.{CountDownLatch,TimeUnit}
    import _root_.javafx.application.Platform
    import _root_.javafx.geometry.{Point2D,Point3D}
    import _root_.javafx.scene.{Group,Scene,SceneAntialiasing}
    import _root_.javafx.scene.input.PickResult
    import JavaFxAffineFixture.*
    val done = new CountDownLatch(1)
    @volatile var failure: Option[Throwable] = None
    Platform.startup(() => ())
    Platform.runLater: () =>
      try
        val adaptive = JavaFxAtlasConfig.make(maxTextureSize=64,encoding=JavaFxAtlasEncoding.AdaptiveAffineOpaque).toOption.get
        val grey = Vector(Rgba32.unsafe(0,0,0),Rgba32.unsafe(255,255,255),Rgba32.unsafe(255,255,255))
        val fixture = model(grey,copies=300)
        val backend = JavaFxSurfaceBackend.create(adaptive).toOption.get
        try
          backend.render(plan(fixture)).toOption.get
          val snapshot = JavaFxSnapshotConfig.make(192,192,SceneAntialiasing.DISABLED).toOption.get
          val sub = backend.newSubScene(snapshot).toOption.get
          val host = new Scene(new Group(sub),192,192)
          host.getRoot.applyCss()
          host.getRoot.layout()
          val controller = JavaFxSurfaceController.attach(fixture,SurfaceViewerState.initial(fixture),backend,sub).toOption.get
          try
            var checked = 0
            for colors <- Vector(rgb,grey,grey.reverse,rgb.reverse,grey) do
              val next = plan(model(colors,copies=300))
              val previousFaces = backend.chunks.map(_.renderedFaceCount).sum
              val receipt = backend.render(next).toOption.get
              val currentFaces = backend.chunks.map(_.renderedFaceCount).sum
              require(receipt.dirty.geometry == (previousFaces != currentFaces))
              backend.snapshot(snapshot).toOption.get
              for chunk <- backend.chunks; local <- 0 until chunk.renderedFaceCount do
                val faces = chunk.mesh.getFaces
                val points = chunk.mesh.getPoints
                def axis(d: Int): Double = (0 until 3).map(c => points.get(faces.get(local*9+c*3)*3+d).toDouble).sum/3
                val point = new Point3D(axis(0),axis(1),axis(2))
                val pick = controller.pick(new PickResult(chunk.view,point,1,local,Point2D.ZERO)).toOption.get
                require(pick.face.index == chunk.packetFace(local).get)
                require(math.abs(pick.barycentricA-(1-point.getX-point.getY)) < 1e-7)
                require(math.abs(pick.barycentricB-point.getX) < 1e-7)
                require(math.abs(pick.barycentricC-point.getY) < 1e-7)
                checked += 1
            println(s"PASS: $checked picks after split, collapse and anchor changes; original barycentrics retained")
          finally controller.dispose()
        finally backend.dispose()
        val limited = JavaFxSurfaceBackend.create(JavaFxAtlasConfig.make(
          encoding=JavaFxAtlasEncoding.AdaptiveAffineOpaque,maxRenderedFaces=3).toOption.get).toOption.get
        try
          val original = plan(model(grey))
          limited.render(original).toOption.get
          val view = limited.chunks.head.view
          require(limited.render(plan(model(rgb))).isLeft)
          require(limited.chunks.head.view eq view)
          require(limited.pickingPlan.contains(original))
          println("PASS: over-budget colour-driven subdivision leaves original native view and pick plan intact")
        finally limited.dispose()
      catch case error: Throwable => failure = Some(error)
      finally done.countDown()
    try
      require(done.await(60,TimeUnit.SECONDS),"adaptive native probe timed out")
      failure.foreach(throw _)
    finally Platform.exit()
