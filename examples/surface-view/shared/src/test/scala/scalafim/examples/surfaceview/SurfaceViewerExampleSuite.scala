package scalafim.examples.surfaceview

import intaglio.*
import scalafim.image.DMat
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

class SurfaceViewerExampleSuite extends munit.FunSuite:
  test("portable GIFTI-derived example has one exact JVM/Scala.js semantic receipt"):
    val first = SurfaceViewerExample.semanticReceipt()
    val second = SurfaceViewerExample.semanticReceipt()
    assertEquals(second, first)
    assertEquals(first.schema, "scalafim.surface-view-example.v1")
    assertEquals(first.source, "tetra_lh_midthickness.surf.gii")
    assertEquals(first.vertices, 8)
    assertEquals(first.faces, 4)
    assertEquals(first.layerOrder, Vector("activation", "parcels"))
    assertEquals(first.slotOrder, Vector("gifti-left", "gifti-right"))
    assertEquals(first.selectedSurface, "gifti-left")
    assertEquals(first.selectedVertex, 2)
    assertEquals(first.cameraDirection, Vector(0.0, 0.0, 1.0))
    assertEquals(first.imageHash, -748488703)
    assertEquals(first.shadedPixels, 11342)
    assert(first.shadedPixels >= 10000, "visual fixture must occupy a meaningful image area")
    assertEquals(first.pickedSurface, "gifti-left")
    assertEquals(first.pickedVertex, 2)

  test("aspect-preserving default matches an analytic mask; explicit Fill retains the old receipt"):
    val example = SurfaceViewerExample.portable
    val legacy = example.copy(plan = example.plan.copy(viewportFit = scalafim.surface.view.SurfaceViewportFit.Fill))
    val old = SurfaceViewerExample.semanticReceipt(legacy)
    assertEquals(old.imageHash, 895258625)
    assertEquals(old.shadedPixels, 12720)
    val white = Rgba32.unsafe(255, 255, 255)
    for (current, contained) <- Vector(example -> true, legacy -> false) do
      val image = SurfaceRasterizer.render(current.plan, RasterDimensions.unsafe(320, 180),
        SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get.image
      // Orthographic limits +/-2/3 put each unit triangle in a 160x160
      // physical viewport, centered vertically in 320x180. Pixel centers
      // therefore have x=27..132, y=37..142, and y>=x+10 in each half.
      // Explicit Fill stretches y by 9/8: its diagonal is 8*y>=9*x,
      // evaluated below at doubled integer pixel-center coordinates.
      var y = 0
      while y < 180 do
        var x = 0
        while x < 320 do
          val local = x % 160
          val expected = if contained then
            local >= 27 && local <= 132 && y >= 37 && y <= 142 && y >= local + 10
          else local >= 27 && local <= 132 && y >= 30 && y <= 149 && 8 * (2 * y + 1) >= 9 * (2 * local + 1)
          assertEquals(image.pixelUnsafe(x, y) != white, expected, s"coverage at ($x,$y), contained=$contained")
          x += 1
        y += 1

  test("visual QA accepts the exact oracle and rejects flips and color corruption"):
    val dimensions = RasterDimensions.unsafe(320, 180)
    val reference = SurfaceRasterizer.render(
      SurfaceViewerExample.portable.plan,
      dimensions,
      SurfaceRasterStyle(culling = TriangleCulling.None)
    ).toOption.get.image
    val exact = SurfaceVisualQa.compare(reference, reference).toOption.get
    assertEqualsDouble(exact.maskIntersectionOverUnion, 1.0, 0.0)
    assertEqualsDouble(exact.centroidDistancePixels, 0.0, 0.0)
    assertEqualsDouble(exact.meanInteriorChannelError, 0.0, 0.0)
    assertEquals(exact.violations(SurfaceVisualQaPolicy.Browser), Vector.empty)

    val horizontalFlip = RasterImage.tabulate(dimensions): (x, y) =>
      reference.pixelUnsafe(dimensions.width - 1 - x, y)
    val verticalFlip = RasterImage.tabulate(dimensions): (x, y) =>
      reference.pixelUnsafe(x, dimensions.height - 1 - y)
    val colorCorruption = RasterImage.tabulate(dimensions): (x, y) =>
      val pixel = reference.pixelUnsafe(x, y)
      Rgba32.unsafe(pixel.blue, pixel.green, pixel.red, pixel.alpha)
    assert(SurfaceVisualQa.compare(reference, horizontalFlip).toOption.get
      .violations(SurfaceVisualQaPolicy.Browser).nonEmpty)
    assert(SurfaceVisualQa.compare(reference, verticalFlip).toOption.get
      .violations(SurfaceVisualQaPolicy.Browser).nonEmpty)
    assert(SurfaceVisualQa.compare(reference, colorCorruption).toOption.get
      .violations(SurfaceVisualQaPolicy.Browser).exists(_.contains("channel error")))

  test("live JVM and browser QA share one exact interior landmark"):
    val reference = SurfaceViewerVisualQaFixture.reference(SurfaceViewerExample.portable)
    val pick = SurfaceViewerVisualQaFixture.referencePick(reference)
    assertEquals(reference.image.dimensions, SurfaceViewerVisualQaFixture.Dimensions)
    assertEquals(SurfaceViewerVisualQaFixture.InteriorPickX, 160)
    assertEquals(SurfaceViewerVisualQaFixture.InteriorPickY, 360)
    assertEquals(pick.surface, SurfaceViewerExample.LeftSurface)
    assertEquals(pick.face, 0)
    assertEquals(pick.vertex, 0)
    assertEquals(reference.image.width, 960)
    assertEquals(reference.image.height, 540)

  test("visual QA rejects dimension drift and invalid policies"):
    val white = Rgba32.unsafe(255, 255, 255)
    val first = RasterImage.solid(RasterDimensions.unsafe(4, 4), white)
    val second = RasterImage.solid(RasterDimensions.unsafe(5, 4), white)
    assert(SurfaceVisualQa.compare(first, second).isLeft)
    assert(SurfaceVisualQaPolicy.make(0, 0.9, 3.0, 24.0).isLeft)
    assert(SurfaceVisualQaPolicy.make(10, 1.1, 3.0, 24.0).isLeft)
    assert(SurfaceVisualQaPolicy.make(10, 0.9, -1.0, 24.0).isLeft)
    assert(SurfaceVisualQaPolicy.make(10, 0.9, 3.0, Double.NaN).isLeft)

  test("production cortical fixture enforces bilateral corpus identity"):
    val left = corticalGeometry(Hemisphere.Left)
    val right = corticalGeometry(Hemisphere.Right)
    val example = CorticalSurfaceAcceptance.build(left, right).toOption.get
    assertEquals(example.plan.profile.verticesPacked, 20484)
    assertEquals(example.plan.profile.facesPacked, 40960)
    assertEquals(example.plan.slots.map(_.surface), Vector(
      SurfaceViewerExample.LeftSurface,
      SurfaceViewerExample.RightSurface
    ))
    assert(example.plan.slots.head.worldOffsetX > 0.0)
    assert(example.plan.slots(1).worldOffsetX < 0.0)
    assertEqualsDouble(
      example.plan.meshes.head.positions(0).toDouble,
      left.mesh.vertex(VertexId(0)).x,
      0.0
    )
    assert(CorticalSurfaceAcceptance.build(right, left).isLeft)
    assert(SurfaceLayer.scalar(
      SurfaceLayerId.unsafe("wrong-length"),
      SurfaceViewerExample.LeftSurface,
      left,
      Array.fill(left.vertexCount - 1)(0.0),
      ScalarColorizer(DisplayWindow.unsafe(-1.0, 1.0), ColorRamp.Heat)
    ).isLeft)

    val cameraPlan = CorticalSurfaceAcceptance.cameraOnlyPlan(example).toOption.get
    assertEquals(cameraPlan.receipt.meshKeys, example.plan.receipt.meshKeys)
    assertEquals(cameraPlan.receipt.layerKeys, example.plan.receipt.layerKeys)
    assert(cameraPlan.receipt.cameraKey != example.plan.receipt.cameraKey)

  private def corticalGeometry(hemisphere: Hemisphere): SurfaceGeometry =
    val vertexCount = CorticalSurfaceAcceptance.LeftCorpus.vertices
    val faceCount = CorticalSurfaceAcceptance.LeftCorpus.faces
    val coordinates = Array.tabulate(vertexCount * 3): index =>
      val vertex = index / 3
      index % 3 match
        case 0 =>
          val hemisphereOffset = if hemisphere == Hemisphere.Left then -20.0 else 20.0
          math.cos(vertex * 0.013) + hemisphereOffset
        case 1 => math.sin(vertex * 0.017)
        case _ => vertex.toDouble / vertexCount
    val faces = Array.tabulate(faceCount * 3): index =>
      val face = index / 3
      index % 3 match
        case 0 => face % (vertexCount - 2)
        case 1 => face % (vertexCount - 2) + 1
        case _ => face % (vertexCount - 2) + 2
    SurfaceGeometry(
      TriangleMesh.fromArrays(coordinates, faces),
      hemisphere,
      SurfaceKind.Pial,
      DMat.eye(4)
    )
