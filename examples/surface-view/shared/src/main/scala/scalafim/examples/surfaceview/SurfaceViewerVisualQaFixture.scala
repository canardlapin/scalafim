package scalafim.examples.surfaceview

import scalafim.graphics.RasterDimensions
import scalafim.surface.view.raster.*

/** One backend-neutral visual contract shared by the live JVM and browser
  * examples. Coordinates use the top-left raster convention.
  */
object SurfaceViewerVisualQaFixture:
  val Dimensions: RasterDimensions = RasterDimensions.unsafe(960, 540)
  val InteriorPickX: Int = Dimensions.width / 6
  val InteriorPickY: Int = Dimensions.height * 2 / 3
  val Policy: SurfaceVisualQaPolicy = SurfaceVisualQaPolicy.NativeBackend

  def reference(example: SurfaceViewerExample): SurfaceRasterResult =
    SurfaceRasterizer.render(
      example.plan,
      Dimensions,
      SurfaceRasterStyle(culling = TriangleCulling.None)
    ).fold(error => throw new IllegalStateException(error.message), identity)

  def referencePick(reference: SurfaceRasterResult): SurfacePick =
    reference.pick(InteriorPickX, InteriorPickY)
      .fold(error => throw new IllegalStateException(error.message), identity)
      .getOrElse(throw new IllegalStateException("CPU reference pick missed the left surface interior"))
