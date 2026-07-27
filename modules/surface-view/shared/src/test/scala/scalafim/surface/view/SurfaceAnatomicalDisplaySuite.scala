package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

class SurfaceAnatomicalDisplaySuite extends munit.FunSuite:
  private val surfaceId = SurfaceId.unsafe("left")
  private val curvatureId = SurfaceLayerId.unsafe("curvature")

  private def geometry(
    vertices: Seq[Seq[Double]],
    faces: Seq[(Int, Int, Int)] = Seq((0, 1, 2))
  ): SurfaceGeometry =
    SurfaceGeometry(TriangleMesh.fromRows(vertices, faces), Hemisphere.Left, SurfaceKind.Inflated)

  private val folded = geometry(Seq(
    Seq(-1.0, -1.0, 0.0),
    Seq(1.0, -1.0, 0.2),
    Seq(0.0, 1.0, -0.2)
  ))
  private val inflated = geometry(Seq(
    Seq(-2.0, -1.0, 0.0),
    Seq(2.0, -1.0, 0.0),
    Seq(0.0, 2.0, 0.0)
  ))

  test("curvature is an ordinary scalar field transferable only across exact topology"):
    val curvature = SurfaceField.full(folded, Seq(-1.0, 0.0, 1.0), "sulcal curvature")
    val layer = SurfaceLayer.curvatureUnderlay(curvatureId, surfaceId, curvature, inflated).toOption.get
    assertEquals(layer.kind, SurfaceLayerKind.Scalar)
    assert(layer.isCompatibleWith(inflated))
    val model = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(surfaceId, inflated).toOption.get),
      Vector(layer)
    ).toOption.get
    val plan = SurfaceCompiler.compile(model, SurfaceViewerState.initial(model)).toOption.get
    assertEquals(plan.layers.head.colors(0), Rgba32.unsafe(48, 48, 48).toPackedInt)
    assertEquals(plan.layers.head.colors(2), Rgba32.unsafe(208, 208, 208).toPackedInt)

    val rewound = geometry(inflated.mesh.vertices.map(point => Seq(point.x, point.y, point.z)), Seq((0, 2, 1)))
    assertEquals(
      SurfaceLayer.curvatureUnderlay(curvatureId, surfaceId, curvature, rewound).left.toOption,
      Some(SurfaceViewError.IncompatibleLayerDomain(curvatureId, surfaceId))
    )
    val sparse = SurfaceField.fromIndexed(folded, Seq(VertexId(0), VertexId(2)), Seq(-1.0, 1.0))
    assert(SurfaceLayer.curvatureUnderlay(curvatureId, surfaceId, sparse, inflated).isLeft)

  test("world clip planes normalize normals and retain an explicit side"):
    assert(WorldClipPlane.make(0.0, 0.0, 0.0, 0.0).isLeft)
    assert(SurfaceClipping.worldPlanes(Vector.empty).isLeft)
    val positive = WorldClipPlane.unsafe(2.0, 0.0, 0.0, 0.0, ClipKeepSide.Positive)
    val negative = WorldClipPlane.unsafe(2.0, 0.0, 0.0, 0.0, ClipKeepSide.Negative)
    assertEqualsDouble(positive.normalX, 1.0, 0.0)
    assert(positive.orientedDistance(1.0, 0.0, 0.0) > 0.0)
    assert(positive.orientedDistance(-1.0, 0.0, 0.0) < 0.0)
    assert(negative.orientedDistance(-1.0, 0.0, 0.0) > 0.0)

  test("publication decoration records camera and layer identity and composes through Scene"):
    val curvature = SurfaceField.full(folded, Seq(-1.0, 0.0, 1.0))
    val layer = SurfaceLayer.curvatureUnderlay(curvatureId, surfaceId, curvature, inflated).toOption.get
    val model = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(surfaceId, inflated).toOption.get),
      Vector(layer)
    ).toOption.get
    val plan = SurfaceCompiler.compile(model, SurfaceViewerState.initial(model)).toOption.get
    val spec = SurfacePublicationSpec(
      SurfacePublicationPreset.ManuscriptSingleColumn,
      Some("Curvature"),
      SurfaceOrientationMark.LeftLateral,
      Vector(SurfaceLegendItem.unsafe("sulcus", Rgba32.unsafe(48, 48, 48)))
    )
    val (decorated, receipt) = SurfacePublication.decorate(plan, spec)
    assertEquals(receipt.width, 1200)
    assertEquals(receipt.cameraKey, plan.receipt.cameraKey)
    assertEquals(receipt.layerKeys, plan.receipt.layerKeys)
    assertEquals(receipt.chromeGrobs, 4)
    assertEquals(receipt.orientation, SurfaceOrientationMark.LeftLateral)
    assertEquals(receipt.title, Some("Curvature"))
    assertEquals(receipt.legendLabels, Vector("sulcus"))
    assertEquals(receipt.background, Rgba32.unsafe(255, 255, 255))
    val image = RasterImage.unsafeFromOwnedPackedArray(
      RasterDimensions.unsafe(2, 2),
      Array.fill(4)(Rgba32.unsafe(255, 255, 255).toPackedInt)
    )
    val scene = SurfacePublication.compose(image, decorated)
    assertEquals(scene.size, 5)
