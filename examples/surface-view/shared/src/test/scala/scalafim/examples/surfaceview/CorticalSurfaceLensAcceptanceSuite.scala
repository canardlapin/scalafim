package scalafim.examples.surfaceview

import image4s.geometry.Affine
import image4s.geometry.D3
import scalafim.surface.*
import scalafim.surface.view.*

class CorticalSurfaceLensAcceptanceSuite extends munit.FunSuite:
  test("cortical lens stages preserve topology, overlays, camera, and exact endpoints"):
    val pial = sphere(SurfaceKind.Pial, 70.0)
    val inflated = sphere(SurfaceKind.Inflated, 88.0)
    val example = CorticalSurfaceLensAcceptance.build(pial, inflated).toOption.get
    assertEquals(example.cases.map(_.label), Vector("Folded", "Opening", "Reveal", "Full lens"))
    assert(example.activeVertices > 1)
    assert(example.activeVertices < pial.vertexCount)
    assertEquals(example.cases.map(_.plan.receipt.meshKeys).distinct.length, 1)
    assertEquals(example.cases.map(_.plan.receipt.layerKeys).distinct.length, 1)
    assertEquals(example.cases.map(_.plan.receipt.cameraKey).distinct.length, 1)
    assertEquals(example.cases.map(_.plan.meshes.head.geometryKey).distinct.length, 4)
    assertEquals(example.cases.head.plan.meshes.head.positions(example.center.index * 3),
      pial.mesh.vertex(example.center).x.toFloat)
    assertEquals(example.cases.last.plan.meshes.head.positions(example.center.index * 3),
      pial.mesh.vertex(example.center).x.toFloat)
    assertEquals(example.cases.last.plan.readouts.head.vertex, example.center.index)
    assertEqualsDouble(
      example.cases.last.plan.readouts.head.worldX,
      pial.mesh.vertex(example.center).x,
      1e-5
    )
    assertEquals(example.quality.invertedTriangles, 0)
    assert(example.quality.minimumAreaRatio > 0.0)
    assert(example.quality.p95EdgeStrain <= example.quality.maximumEdgeStrain)
    assert(example.cases.forall(current =>
      current.guide.centerX >= 0.0 && current.guide.centerX <= CorticalSurfaceLensAcceptance.Dimensions.width &&
        current.guide.centerY >= 0.0 && current.guide.centerY <= CorticalSurfaceLensAcceptance.Dimensions.height
    ))
    assertEquals(example.cases.map(_.guide).distinct.length, 1)

  test("cortical lens rejects endpoint role, hemisphere, and topology drift"):
    val pial = sphere(SurfaceKind.Pial, 70.0)
    val inflated = sphere(SurfaceKind.Inflated, 88.0)
    assert(CorticalSurfaceLensAcceptance.build(inflated, pial).isLeft)
    assert(CorticalSurfaceLensAcceptance.build(
      pial,
      SurfaceGeometry(inflated.mesh, Hemisphere.Right, SurfaceKind.Inflated, Affine.identity[D3])
    ).isLeft)
    val rewound = inflated.mesh.faceIndices.toArray
    val swap = rewound(1)
    rewound(1) = rewound(2)
    rewound(2) = swap
    assert(CorticalSurfaceLensAcceptance.build(
      pial,
      SurfaceGeometry(
        TriangleMesh.fromArrays(inflated.mesh.coordinates.toArray, rewound),
        Hemisphere.Left,
        SurfaceKind.Inflated,
        Affine.identity[D3]
      )
    ).isLeft)

  private def sphere(kind: SurfaceKind, radius: Double): SurfaceGeometry =
    val latitudeBands = 28
    val longitudeBands = 56
    val coordinates = Vector.newBuilder[Vector[Double]]
    var latitude = 0
    while latitude <= latitudeBands do
      val theta = math.Pi * latitude / latitudeBands
      var longitude = 0
      while longitude < longitudeBands do
        val phi = 2.0 * math.Pi * longitude / longitudeBands
        coordinates += Vector(
          radius * math.sin(theta) * math.cos(phi),
          1.10 * radius * math.sin(theta) * math.sin(phi),
          0.92 * radius * math.cos(theta)
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
      kind,
      Affine.identity[D3]
    )
