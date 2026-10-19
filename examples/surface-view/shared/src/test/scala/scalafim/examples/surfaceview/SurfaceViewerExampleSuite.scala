package scalafim.examples.surfaceview

import image4s.geometry.Affine
import image4s.geometry.D3
import intaglio.*
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
    assertEquals(first.imageHash, 895258625)
    assertEquals(first.shadedPixels, 12720)
    assert(first.shadedPixels >= 10000, "visual fixture must occupy a meaningful image area")
    assertEquals(first.pickedSurface, "gifti-left")
    assertEquals(first.pickedVertex, 2)

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
    // Synthetic count fixture: a valid closed bipyramid, not cortical anatomy.
    val ringCount = vertexCount - 2
    require(faceCount == 2 * ringCount)
    val coordinates = Array.tabulate(vertexCount * 3): index =>
      val vertex = index / 3
      val pole = vertex == 0 || vertex == vertexCount - 1
      val angle = 2.0 * math.Pi * (vertex - 1) / ringCount
      index % 3 match
        case 0 =>
          val hemisphereOffset = if hemisphere == Hemisphere.Left then -20.0 else 20.0
          hemisphereOffset + (if pole then 0.0 else math.cos(angle))
        case 1 => if pole then 0.0 else math.sin(angle)
        case _ => if vertex == 0 then 1.0 else if vertex == vertexCount - 1 then -1.0 else 0.0
    val faces = Array.tabulate(faceCount * 3): index =>
      val face = index / 3
      val current = face / 2 + 1
      val next = face / 2 % ringCount + 2
      val wrapped = if next > ringCount then 1 else next
      if face % 2 == 0 then
        index % 3 match
          case 0 => 0
          case 1 => current
          case _ => wrapped
      else
        index % 3 match
          case 0 => vertexCount - 1
          case 1 => wrapped
          case _ => current
    SurfaceGeometry(
      TriangleMesh.fromArrays(coordinates, faces),
      hemisphere,
      SurfaceKind.Pial,
      Affine.identity[D3]
    )
