package scalafim.surface.view.raster.spike

import java.nio.file.Files

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

class SpikeSuite extends munit.FunSuite:
  private val left = SurfaceId.unsafe("left")
  private val right = SurfaceId.unsafe("right")

  private def geometry(hemisphere: Hemisphere, shift: Double): SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Seq(Seq(-1.0 + shift, -1.0, 0.0), Seq(1.0 + shift, -1.0, 0.2), Seq(-0.6 + shift, 0.8, -0.1), Seq(0.9 + shift, 0.9, 0.3)),
        Seq((0, 1, 2), (1, 3, 2))
      ),
      hemisphere,
      SurfaceKind.Inflated
    )

  private def plan(lighting: SurfaceLighting): SurfaceRenderPlan =
    val leftGeometry = geometry(Hemisphere.Left, 0.0)
    val rightGeometry = geometry(Hemisphere.Right, 3.0)
    val colors = Vector(Rgba32.unsafe(255, 0, 0), Rgba32.unsafe(0, 255, 0), Rgba32.unsafe(0, 0, 255), Rgba32.unsafe(255, 255, 0, 128))
    val layers = Vector(
      SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("left-colors"), left, leftGeometry, colors).toOption.get,
      SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("right-colors"), right, rightGeometry, colors.reverse).toOption.get
    )
    val model = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(left, leftGeometry).toOption.get, SurfaceAsset.make(right, rightGeometry).toOption.get),
      layers
    ).toOption.get
    val actions = Vector(
      SurfaceViewerAction.SetProjection(CameraProjection.Orthographic(OrthographicScale.Default)),
      SurfaceViewerAction.SetLighting(lighting),
      SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(left, right)),
      SurfaceViewerAction.FitCamera
    )
    val state = actions.foldLeft(SurfaceViewerState.initial(model))((s, a) => SurfaceViewer.reduce(model, s, a).toOption.get)
    SurfaceCompiler.compile(model, state).toOption.get

  test("plan codec round-trips a bilateral lit plan to identical raster pixels"):
    val original = plan(SurfaceLighting.Default)
    val dimensions = RasterDimensions.unsafe(96, 64)
    val fitted = Vector(SpikeFittedSlots(96, 64, original.viewportFit.resolve(original.slots, 96.0, 64.0)))
    val file = Files.createTempFile("spike-plan", ".plan")
    SpikePlanCodec.write(file, SpikeScene(original, fitted, """{"fixture":true}"""))
    val decoded = SpikePlanCodec.read(file)
    Files.delete(file)
    assertEquals(decoded.metadata, """{"fixture":true}""")
    assertEquals(decoded.plan.receipt, original.receipt)
    assertEquals(decoded.plan.lighting, original.lighting)
    val expected = SurfaceRasterizer.render(original, dimensions).toOption.get.image
    // The decoded plan carries pre-fitted slots with Fill, matching the producer's Contain fit.
    val actual = SurfaceRasterizer.render(decoded.planFor(96, 64), dimensions).toOption.get.image
    assertEquals(SpikeImages.pixelSha256(actual), SpikeImages.pixelSha256(expected))
    assert(SurfaceRasterizer.render(original, dimensions).toOption.get.receipt.shadedPixels > 100)

  test("banded parallel rasterizer is pixel-identical to the reference interpreter"):
    Vector(SurfaceLighting.Default, SurfaceLighting.Unlit).foreach: lighting =>
      val original = plan(lighting)
      Vector(RasterDimensions.unsafe(96, 64), RasterDimensions.unsafe(37, 53)).foreach: dimensions =>
        val expected = SurfaceRasterizer.render(original, dimensions).toOption.get
        val parallel = ParallelSurfaceRasterizer.render(original, dimensions, 3)
        assertEquals(SpikeImages.pixelSha256(parallel.image), SpikeImages.pixelSha256(expected.image))
        assertEquals(parallel.receipt.shadedPixels, expected.receipt.shadedPixels)
        assertEquals(parallel.receipt.trianglesInput, expected.receipt.trianglesInput)
        assertEquals(parallel.receipt.trianglesCulled, expected.receipt.trianglesCulled)
        assert(expected.receipt.shadedPixels > 50)
