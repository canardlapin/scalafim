package scalafim.examples.surfaceview

import intaglio.*
import scalafim.image.DMat
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

import scala.collection.mutable

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
      1e-5
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
    val (unitVertices, faces) = icosphere(subdivisions = 5)
    val hemisphereOffset = if hemisphere == Hemisphere.Left then -20.0 else 20.0
    val coordinates = unitVertices.map: (x, y, z) =>
      Vector(x + hemisphereOffset, y, (z + 1.0) / 2.0)
    require(
      coordinates.length == CorticalSurfaceAcceptance.LeftCorpus.vertices,
      "generated cortical fixture must have fsaverage5 vertex cardinality"
    )
    require(
      faces.length == CorticalSurfaceAcceptance.LeftCorpus.faces,
      "generated cortical fixture must have fsaverage5 face cardinality"
    )
    SurfaceGeometry(
      TriangleMesh.fromRows(coordinates, faces),
      hemisphere,
      SurfaceKind.Pial,
      DMat.eye(4)
    )

  private def icosphere(
    subdivisions: Int
  ): (Vector[(Double, Double, Double)], Vector[(Int, Int, Int)]) =
    require(subdivisions >= 0, "icosphere subdivisions must be non-negative")
    val golden = (1.0 + math.sqrt(5.0)) / 2.0
    var vertices = mutable.ArrayBuffer.from(Vector(
      normalized(-1.0, golden, 0.0),
      normalized(1.0, golden, 0.0),
      normalized(-1.0, -golden, 0.0),
      normalized(1.0, -golden, 0.0),
      normalized(0.0, -1.0, golden),
      normalized(0.0, 1.0, golden),
      normalized(0.0, -1.0, -golden),
      normalized(0.0, 1.0, -golden),
      normalized(golden, 0.0, -1.0),
      normalized(golden, 0.0, 1.0),
      normalized(-golden, 0.0, -1.0),
      normalized(-golden, 0.0, 1.0)
    ))
    var faces = Vector(
      (0, 11, 5), (0, 5, 1), (0, 1, 7), (0, 7, 10), (0, 10, 11),
      (1, 5, 9), (5, 11, 4), (11, 10, 2), (10, 7, 6), (7, 1, 8),
      (3, 9, 4), (3, 4, 2), (3, 2, 6), (3, 6, 8), (3, 8, 9),
      (4, 9, 5), (2, 4, 11), (6, 2, 10), (8, 6, 7), (9, 8, 1)
    )

    var level = 0
    while level < subdivisions do
      val refinedVertices = mutable.ArrayBuffer.from(vertices)
      val midpointIndices = mutable.HashMap.empty[(Int, Int), Int]
      def midpoint(first: Int, second: Int): Int =
        val key = if first < second then (first, second) else (second, first)
        midpointIndices.getOrElseUpdate(
          key,
          {
            val a = vertices(first)
            val b = vertices(second)
            val point = normalized(
              (a._1 + b._1) / 2.0,
              (a._2 + b._2) / 2.0,
              (a._3 + b._3) / 2.0
            )
            val index = refinedVertices.length
            refinedVertices += point
            index
          }
        )

      val refinedFaces = Vector.newBuilder[(Int, Int, Int)]
      faces.foreach: (a, b, c) =>
        val ab = midpoint(a, b)
        val bc = midpoint(b, c)
        val ca = midpoint(c, a)
        refinedFaces += ((a, ab, ca))
        refinedFaces += ((b, bc, ab))
        refinedFaces += ((c, ca, bc))
        refinedFaces += ((ab, bc, ca))
      vertices = refinedVertices
      faces = refinedFaces.result()
      level += 1

    (vertices.toVector, faces)

  private def normalized(
    x: Double,
    y: Double,
    z: Double
  ): (Double, Double, Double) =
    val length = math.sqrt(x * x + y * y + z * z)
    (x / length, y / length, z / length)
