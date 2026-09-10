package scalafim.surface.view.raster

import intaglio.RasterDimensions
import scalafim.surface.*
import scalafim.surface.view.*

class SurfacePairedRasterSuite extends munit.FunSuite:
  private val left = SurfaceId.unsafe("left")
  private val right = SurfaceId.unsafe("right")
  private def asset(id: SurfaceId, hemisphere: Hemisphere, sign: Double): SurfaceAsset =
    val positions = Vector(3.0, 1.0).flatMap(x =>
      Vector(Vector(sign * x, -2.0, -1.0), Vector(sign * x, 3.0, -1.0), Vector(sign * x, -1.0, 4.0)))
    val faces = if sign < 0 then Vector((0, 2, 1), (3, 4, 5)) else Vector((0, 1, 2), (3, 5, 4))
    SurfaceAsset.make(id, SurfaceGeometry(TriangleMesh.fromRows(positions, faces), hemisphere, SurfaceKind.Inflated)).toOption.get
  private val model = SurfaceViewerModel.make(Vector(asset(left, Hemisphere.Left, -1), asset(right, Hemisphere.Right, 1)), Vector.empty).toOption.get
  private def state(medial: Boolean = false): SurfaceViewerState =
    val initial = SurfaceViewerState.initial(model)
    initial.copy(layout = SurfaceLayout.Bilateral(left, right), lighting = SurfaceLighting.Unlit,
      surfaceViewpoints = if medial then Map(left -> SurfaceViewpoint.Medial(CorticalHemisphere.Left), right -> SurfaceViewpoint.Medial(CorticalHemisphere.Right))
        else Map(left -> SurfaceViewpoint.Lateral(CorticalHemisphere.Left), right -> SurfaceViewpoint.Lateral(CorticalHemisphere.Right)))
  private def plan(value: SurfaceViewerState): SurfaceRenderPlan = SurfaceCompiler.compile(model, value).toOption.get

  private def faces(compiled: SurfaceRenderPlan, width: Int, height: Int): Map[SurfaceId, Set[Int]] =
    val result = SurfaceRasterizer.render(compiled, RasterDimensions.unsafe(width, height)).toOption.get
    val picks = for
      y <- 0 until height
      x <- 0 until width
      hit <- result.pick(x, y).toOption.flatten
    yield hit
    assert(picks.nonEmpty)
    picks.foreach(hit => assertEqualsDouble(hit.barycentricA + hit.barycentricB + hit.barycentricC, 1.0, 1e-6))
    picks.groupBy(_.surface).view.mapValues(_.map(_.face).toSet).toMap

  test("lateral views expose both outer faces; medial views expose both inner faces with original IDs"):
    for
      medial <- Vector(false, true)
      projection <- Vector(CameraProjection.Perspective(FieldOfViewDegrees.Default), CameraProjection.Orthographic(OrthographicScale.unsafe(5)))
      order <- BilateralOrder.values
      (width, height) <- Vector(240 -> 100, 100 -> 240, 480 -> 200)
    do
      val initial = state(medial)
      val compiled = plan(initial.copy(layout = SurfaceLayout.Bilateral(left, right, order), camera = initial.camera.copy(projection = projection)))
      val expected = if medial then 1 else 0
      assertEquals(faces(compiled, width, height), Map(left -> Set(expected), right -> Set(expected)))

  test("the previous global left camera exposes the right medial face and fails paired-lateral semantics"):
    val initial = state()
    val wrong = plan(initial.copy(surfaceViewpoints = Map.empty))
    assertEquals(faces(wrong, 240, 120), Map(left -> Set(0), right -> Set(1)))
    assertNotEquals(faces(wrong, 240, 120), faces(plan(initial), 240, 120))
