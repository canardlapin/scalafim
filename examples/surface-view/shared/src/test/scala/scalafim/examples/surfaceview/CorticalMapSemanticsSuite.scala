package scalafim.examples.surfaceview

import munit.FunSuite
import scalafim.surface.*
import scalafim.surface.view.*

class CorticalMapSemanticsSuite extends FunSuite:
  private val geometry = SurfaceGeometry(TriangleMesh.fromRows(
    Seq(Seq(-1.0, -60.0, 0.0), Seq(-1.0, -20.0, 10.0), Seq(-1.0, 20.0, 0.0)),
    Seq((0, 1, 2))), Hemisphere.Left, SurfaceKind.Pial)
  private val surfaces = SurfaceSet.of(SurfaceKind.Pial, geometry)
  private val folding = SurfaceField(geometry, Array(0, 1, 2), Array(-1.0, 0.0, 1.0))

  test("fixture keeps scalar, nearest, face and ordered folding contracts distinct"):
    val expected = Vector(CorticalMapMode.Scalar -> SurfaceMapInterpolation.VertexScalar,
      CorticalMapMode.Layered -> SurfaceMapInterpolation.VertexScalar,
      CorticalMapMode.CurvatureLayered -> SurfaceMapInterpolation.VertexScalar,
      CorticalMapMode.Nearest -> SurfaceMapInterpolation.NearestVertex,
      CorticalMapMode.Face -> SurfaceMapInterpolation.FaceConstant)
    expected.foreach: (mode, interpolation) =>
      val example = CorticalMapSemantics.build(surfaces, folding, mode)
      assertEquals(example.plan.layers.last.interpolation, interpolation)
      val ids = mode match
        case CorticalMapMode.Layered => Vector(CorticalMapSemantics.Underlay, CorticalMapSemantics.Overlay)
        case CorticalMapMode.CurvatureLayered => Vector(CorticalMapSemantics.Curvature, CorticalMapSemantics.Overlay)
        case _ => Vector(CorticalMapSemantics.Overlay)
      assertEquals(example.plan.layers.map(_.layer), ids)
    assertEqualsDouble(CorticalMapSemantics.scalar(0, 0), 0, 1e-15)
    assertEqualsDouble(CorticalMapSemantics.scalar(25 * math.Pi / 2, 0), 4, 1e-15)
    assertEquals(Vector(-41.0, -40.0, -1.0, 0.0).map(CorticalMapSemantics.category), Vector(1, 2, 2, 3))

  test("unmatched or incomplete folding data cannot produce a plausible cortical map"):
    val reversed = SurfaceGeometry(TriangleMesh.fromArrays(geometry.mesh.coordinates, Array(0, 2, 1)),
      Hemisphere.Left, SurfaceKind.Pial)
    intercept[IllegalArgumentException](CorticalMapSemantics.build(surfaces, folding.copy(geometry = reversed), CorticalMapMode.Layered))
    intercept[IllegalArgumentException](CorticalMapSemantics.build(surfaces,
      folding.copy(indices = Array(1, 0, 2)), CorticalMapMode.Layered))
    intercept[IllegalArgumentException](CorticalMapSemantics.build(surfaces,
      folding.copy(data = Array(0.0, Double.NaN, 1.0)), CorticalMapMode.Layered))

  test("second frames change observations while preserving geometry and mapping policy"):
    for mode <- CorticalMapMode.values do
      val example = CorticalMapSemantics.build(surfaces, folding, mode)
      val state = SurfaceViewer.reduce(example.model, example.state, SurfaceViewerAction.SetTimepoint(1)).toOption.get
      val next = SurfaceCompiler.compile(example.model, state).toOption.get
      assertEquals(next.receipt.meshKeys, example.plan.receipt.meshKeys)
      assertEquals(next.meshes.head.positions.unsafeArray.toVector, example.plan.meshes.head.positions.unsafeArray.toVector)
      assertEquals(next.layers.last.interpolation, example.plan.layers.last.interpolation)
      assertNotEquals(next.layers.last.resourceKey, example.plan.layers.last.resourceKey)
      if mode == CorticalMapMode.Scalar || mode == CorticalMapMode.Layered || mode == CorticalMapMode.CurvatureLayered then
        assertEqualsDouble(next.layers.last.scalarField.get.samples(0),
          example.plan.layers.last.scalarField.get.samples(0) * 0.8, 1e-14)
