package scalafim.surface.view.raster

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

/** Analytic coordinate-valued ellipsoid; nearest vertex samples, not scalar fragments. */
object SurfaceScalarMappingPlate:
  def main(args: Array[String]): Unit =
    val rings = 30
    val slices = 60
    val vertices = Vector(Seq(0.0, -4.0, 0.0)) ++ (for
      ring <- 1 until rings
      slice <- 0 until slices
    yield
      val latitude = math.Pi * ring / rings
      val longitude = 2 * math.Pi * slice / slices
      Seq(2.0 * math.sin(latitude) * math.cos(longitude), 2.0 - 6.0 * math.cos(latitude),
        math.sin(latitude) * math.sin(longitude))).toVector ++ Vector(Seq(0.0, 8.0, 0.0))
    val faces = Vector.newBuilder[(Int, Int, Int)]
    for slice <- 0 until slices do faces += ((0, 1 + slice, 1 + (slice + 1) % slices))
    for ring <- 0 until rings - 2; slice <- 0 until slices do
      val a = 1 + ring * slices + slice
      val b = 1 + ring * slices + (slice + 1) % slices
      val c = a + slices
      val d = b + slices
      faces += ((a, c, b))
      faces += ((b, c, d))
    for slice <- 0 until slices do
      faces += ((vertices.length - 1, 1 + (rings - 2) * slices + (slice + 1) % slices, 1 + (rings - 2) * slices + slice))
    val geometry = SurfaceGeometry(TriangleMesh.fromRows(vertices, faces.result()), Hemisphere.Left, SurfaceKind.Inflated)
    val id = SurfaceId.unsafe("ellipsoid")
    val layerId = SurfaceLayerId.unsafe("y-coordinate")
    val low = ScalarRamp.linear(Rgba32.unsafe(0, 0, 200), Rgba32.unsafe(240, 240, 240))
    val high = ScalarRamp.linear(Rgba32.unsafe(240, 240, 240), Rgba32.unsafe(200, 0, 0))
    val window = DisplayWindow.unsafe(-4.0, 8.0)
    val continuous = ScalarMapping(ScalarScale.diverging(window, 0.0, low, high).toOption.get)
    val narrow = DisplayThreshold.TransparentBand(ThresholdBand.unsafe(-1.0, 2.0))
    val wide = DisplayThreshold.TransparentBand(ThresholdBand.unsafe(-2.0, 4.0))
    val mappings = Vector(continuous, continuous.resolve(threshold = Some(narrow)).toOption.get,
      continuous.resolve(threshold = Some(wide)).toOption.get,
      ScalarMapping(ScalarScale.split(window, 0.0, -1.0, 2.0, low, high).toOption.get))
    val titles = Vector("Continuous", "Hide (-1, 2)", "Hide (-2, 4)", "Split tails (-1, 2)")
    val out = new java.awt.image.BufferedImage(1024, 488, java.awt.image.BufferedImage.TYPE_INT_ARGB)
    val graphics = out.createGraphics()
    graphics.setColor(java.awt.Color.WHITE)
    graphics.fillRect(0, 0, out.getWidth, out.getHeight)
    graphics.setColor(java.awt.Color.DARK_GRAY)
    graphics.setFont(new java.awt.Font("SansSerif", java.awt.Font.PLAIN, 15))
    for (mapping, panel) <- mappings.zipWithIndex do
      val layer = SurfaceLayer.scalar(layerId, id, geometry, vertices.map(_(1)).toArray, mapping.colorizer,
        interpolation = SurfaceVertexInterpolation.NearestSample).toOption.get
      val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(id, geometry).toOption.get), Vector(layer)).toOption.get
      val state = SurfaceFaceFixture.state(model).copy(camera = SurfaceFaceFixture.state(model).camera.copy(
        projection = CameraProjection.Orthographic(OrthographicScale.unsafe(7.0))))
      val plan = SurfaceCompiler.compile(model, state).toOption.get
      val raster = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(256, 400),
        SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get.image
      for y <- 0 until 400; x <- 0 until 256 do
        val pixel = raster.pixelUnsafe(x, y)
        out.setRGB(panel * 256 + x, y + 44,
          (pixel.alpha << 24) | (pixel.red << 16) | (pixel.green << 8) | pixel.blue)
      graphics.drawString(titles(panel), panel * 256 + 34, 28)
    graphics.drawString("Ellipsoid y samples | limits [-4, 8], center 0 | unlit nearest-vertex colors; gray = hidden", 24, 472)
    graphics.dispose()
    val target = if args.nonEmpty then args(0) else "/private/tmp/scalafim-scalar-mapping-plate.png"
    javax.imageio.ImageIO.write(out, "png", new java.io.File(target))
    println(s"scalar_mapping_plate=$target vertices=${vertices.length} faces=${geometry.faceCount}")
