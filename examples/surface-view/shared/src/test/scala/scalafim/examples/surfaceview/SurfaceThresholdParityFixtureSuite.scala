package scalafim.examples.surfaceview

import image4s.geometry.Affine
import image4s.geometry.D3
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

class SurfaceThresholdParityFixtureSuite extends munit.FunSuite:
  test("fixed-seed smoothed noise is deterministic and standardized"):
    val geometry = sphereGeometry()
    val topology = MeshTopology.from(geometry)
    val first = SurfaceThresholdParityFixture.deterministicNoise(topology)
    val second = SurfaceThresholdParityFixture.deterministicNoise(topology)
    val different = SurfaceThresholdParityFixture.deterministicNoise(topology, seed = 17)
    assertEquals(first.toVector, second.toVector)
    assert(first.indices.exists(index => first(index) != different(index)))
    assert(first.forall(_.isFinite))
    assertEqualsDouble(first.sum / first.length, 0.0, 1e-12)
    val variance = first.iterator.map(value => value * value).sum / first.length
    assertEqualsDouble(variance, 1.0, 1e-12)

  test("four-view fixture holds geometry and thresholded colors constant"):
    val fixture = SurfaceThresholdParityFixture.build(sphereGeometry()).toOption.get
    assertEquals(fixture.views.map(_.label), Vector("Ventral", "Lateral", "Posterior", "Top"))
    assertEquals(fixture.views.map(_.plan.camera.directionX), Vector(0.0, -1.0, 0.0, 0.0))
    assertEquals(fixture.views.map(_.plan.camera.directionY), Vector(0.0, 0.0, -1.0, 0.0))
    assertEquals(fixture.views.map(_.plan.camera.directionZ), Vector(-1.0, 0.0, 0.0, 1.0))
    assertEquals(fixture.views.map(_.plan.receipt.meshKeys).distinct.length, 1)
    assertEquals(fixture.views.map(_.plan.receipt.layerKeys).distinct.length, 1)
    assertEquals(fixture.views.map(_.plan.receipt.cameraKey).distinct.length, 4)

    val overlay = fixture.views.head.plan.layers.find(_.layer == SurfaceThresholdParityFixture.Noise).get
    var visible = 0
    var vertex = 0
    while vertex < overlay.colors.length do
      if (overlay.colors(vertex) & 0xff) != 0 then visible += 1
      vertex += 1
    assertEquals(visible, fixture.visibleNegativeVertices + fixture.visiblePositiveVertices)
    assert(fixture.visibleNegativeVertices > 0)
    assert(fixture.visiblePositiveVertices > 0)

  test("curvature underlay is finite, bounded, and independent of camera direction"):
    val topology = MeshTopology.from(sphereGeometry())
    val values = SurfaceThresholdParityFixture.curvatureValues(topology)
    assertEquals(values.length, topology.mesh.vertexCount)
    assert(values.forall(_.isFinite))
    assert(values.forall(value => value >= -1.0 && value <= 1.0))

  test("orientation QA makes the direct raster decisively better than flips"):
    val fixture = SurfaceThresholdParityFixture.build(sphereGeometry()).toOption.get
    val reference = SurfaceThresholdParityFixture.reference(fixture.views.head).image
    val receipt = SurfaceThresholdParityFixture.orientationQa(reference, reference)
    assertEqualsDouble(receipt.directMeanChannelError, 0.0, 0.0)
    assert(receipt.margin > 1.0)

  private def sphereGeometry(): SurfaceGeometry =
    val latitudeBands = 50
    val longitudeBands = 100
    val coordinates = Vector.newBuilder[Vector[Double]]
    var latitude = 0
    while latitude <= latitudeBands do
      val theta = math.Pi * latitude / latitudeBands
      var longitude = 0
      while longitude < longitudeBands do
        val phi = 2.0 * math.Pi * longitude / longitudeBands
        coordinates += Vector(
          70.0 * math.sin(theta) * math.cos(phi),
          85.0 * math.sin(theta) * math.sin(phi),
          65.0 * math.cos(theta)
        )
        longitude += 1
      latitude += 1
    val faces = Vector.newBuilder[(Int, Int, Int)]
    latitude = 0
    while latitude < latitudeBands do
      var longitude = 0
      while longitude < longitudeBands do
        val next = (longitude + 1) % longitudeBands
        val a = latitude * longitudeBands + longitude
        val b = latitude * longitudeBands + next
        val c = (latitude + 1) * longitudeBands + longitude
        val d = (latitude + 1) * longitudeBands + next
        faces += ((a, c, b))
        faces += ((b, c, d))
        longitude += 1
      latitude += 1
    SurfaceGeometry(
      TriangleMesh.fromRows(coordinates.result(), faces.result()),
      Hemisphere.Left,
      SurfaceKind.Pial,
      Affine.identity[D3]
    )
