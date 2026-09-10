package scalafim.surface.view.javafx

import intaglio.*
import scalafim.surface.view.*

class JavaFxRetainedAtlasSuite extends munit.FunSuite:
  import JavaFxAffineFixture.*
  private val config = JavaFxAtlasConfig.make(maxTextureSize = 64,
    encoding = JavaFxAtlasEncoding.RetainedAffineOpaque).toOption.get
  private def grey(a: Int, b: Int, c: Int) = Vector(a,b,c).map(v => Rgba32.unsafe(v,v,v))

  test("retained scalar ramps choose their middle corner instead of a feasible extrapolating endpoint"):
    val result = JavaFxSurfaceProbe.compile(plan(model(grey(1,100,101))), config = config).toOption.get
    assertEquals(result.chunks.head.atlas.adaptiveLayout.get.anchor(0), 1)
    assertEquals(result.receipt.facesUploaded, 1)

  test("a valid retained anchor survives a change to constant colour without any UV upload"):
    val original = plan(model(grey(0,255,255)))
    val result = JavaFxSurfaceProbe.compile(original, config = config).toOption.get
    assertEquals(result.chunks.head.atlas.adaptiveLayout.get.anchor(0), 1)
    val before = result.chunks.head.mesh.getTexCoords.toArray(null: Array[Float]).toVector
    assert(before.distinct.size > 1)
    val receipt = result.updateColors(plan(model(grey(100,100,100)))).toOption.get
    assertEquals(receipt.textureCoordinateBytesUpdated, 0L)
    assertEquals(result.chunks.head.mesh.getTexCoords.toArray(null: Array[Float]).toVector, before)
    assertEquals(result.chunks.head.atlas.adaptiveLayout.get.anchor(0), 1)

  test("invalid retained anchor promotes faces once and preserves their source correspondence across chunks"):
    val original = plan(model(grey(0,255,255), copies = 300))
    val before = JavaFxSurfaceProbe.compile(original, config = config).toOption.get
    val next = plan(model(grey(255,0,255), copies = 300))
    assertEquals(before.requiresAtlasRebuild(next, JavaFxMaterialMode.Unlit), Right(true))
    val promoted = JavaFxSurfaceProbe.compileRetaining(next, JavaFxMaterialMode.Unlit, config, Some(before)).toOption.get
    assertEquals(promoted.receipt.facesUploaded, 1200)
    assertEquals(promoted.receipt.verticesUploaded, 906)
    assertEquals(promoted.requiresAtlasRebuild(original, JavaFxMaterialMode.Unlit), Right(false))
    val receipt = promoted.updateColors(original).toOption.get
    assertEquals(receipt.textureCoordinateBytesUpdated, 0L)
    for chunk <- promoted.chunks; face <- 0 until chunk.renderedFaceCount do
      assertEquals(chunk.packetFace(face), Some(chunk.faceStart + face / 4))
    val moved = promoted.updateGeometry(plan(model(grey(0,255,255), copies = 300, shift = 3))).toOption.get
    assertEquals(moved.verticesUpdated, 906)
    assertEqualsDouble(promoted.chunks.head.mesh.getPoints.get(9).toDouble, 3.5, 0)

  test("retained refinements consume budget even if the current map has a cheaper static encoding"):
    val limited = JavaFxAtlasConfig.make(encoding = JavaFxAtlasEncoding.RetainedAffineOpaque,
      maxRenderedFaces = 3).toOption.get
    val first = JavaFxSurfaceProbe.compile(plan(model(grey(0,255,255))), config = limited).toOption.get
    val next = plan(model(grey(255,0,255)))
    assert(JavaFxSurfaceProbe.compile(next, config = limited).isRight)
    val refused = JavaFxSurfaceProbe.compileRetaining(next, JavaFxMaterialMode.Unlit, limited, Some(first))
    assert(refused.left.toOption.get.message.contains("rendered faces 4 exceed budget 3"))
    assertEquals(first.receipt.facesUploaded, 1)

  test("midpoint texels after multiple colour changes retain affine continuation without clipping"):
    val original = plan(model(grey(0,255,255)))
    val first = JavaFxSurfaceProbe.compile(original, config = config).toOption.get
    val promoted = JavaFxSurfaceProbe.compileRetaining(plan(model(grey(255,0,255))),
      JavaFxMaterialMode.Unlit, config, Some(first)).toOption.get
    for colors <- Vector(rgb, grey(100,100,100), grey(255,0,255), grey(0,255,255)) do
      assertEquals(promoted.updateColors(plan(model(colors))).toOption.get.textureCoordinateBytesUpdated, 0L)
      val image = promoted.chunks.head.atlas.image
      // Independently check the four square continuations using original RGB.
      for shift <- Vector(8,16,24) do
        val c = colors.map(value => ((value.toPackedInt >>> shift) & 255).toDouble)
        val mid = Vector((c(0)+c(1))/2, (c(1)+c(2))/2, (c(2)+c(0))/2)
        val children = Vector(Vector(c(0),mid(0),mid(2)), Vector(c(1),mid(1),mid(0)),
          Vector(c(2),mid(2),mid(1)), Vector(mid(0),mid(1),mid(2)))
        for child <- 0 until 4 do
          val x = (child%2)*2; val y = (child/2)*2
          val p = children(child)(0); val q = children(child)(1); val r = children(child)(2)
          for (dx,dy,expected) <- Vector((0,1,p),(1,1,q),(0,0,r),(1,0,q+r-p)) do
            val actual = (image.getPixelReader.getArgb(x+dx,y+dy) >>> (shift-8)) & 255
            assertEqualsDouble(actual.toDouble, expected, 0.5)

/** Native update and original-face dispatch, without opening a Stage. */
object JavaFxRetainedAtlasProbe:
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
        val adaptive = JavaFxAtlasConfig.make(maxTextureSize=64,encoding=JavaFxAtlasEncoding.RetainedAffineOpaque).toOption.get
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
              require(receipt.textureCoordinateBytesUpdated == 0L)
              require(currentFaces >= previousFaces)
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
            println(s"PASS: $checked picks after retained subdivision and colour changes; original barycentrics retained")
          finally controller.dispose()
        finally backend.dispose()
        val limited = JavaFxSurfaceBackend.create(JavaFxAtlasConfig.make(
          encoding=JavaFxAtlasEncoding.RetainedAffineOpaque,maxRenderedFaces=3).toOption.get).toOption.get
        try
          val original = plan(model(grey))
          limited.render(original).toOption.get
          val view = limited.chunks.head.view
          require(limited.render(plan(model(rgb))).isLeft)
          val differentAnchor = Vector(Rgba32.unsafe(255,255,255),Rgba32.unsafe(0,0,0),Rgba32.unsafe(255,255,255))
          require(limited.render(plan(model(differentAnchor))).isLeft)
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
