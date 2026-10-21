package scalafim.surface.view.javafx

import scalafim.surface.view.*

/** CPU-only descriptor: no native node, image, direct buffer or mutable owner. */
private[javafx] final case class JavaFxAtlasPixelLayout(
  surface: SurfaceId, faceStart: Int, faceCount: Int, width: Int, height: Int, tileSize: Int
)
private[javafx] final class JavaFxColorPreparationToken
private[javafx] final case class JavaFxPreparedAtlasUpdate(pixels: Array[Int], coordinates: Array[Float])

/** Private arrays are owned by this result and never changed after preparation. */
final class JavaFxPreparedColorUpdate private[javafx] (
  private[javafx] val owner: JavaFxColorPreparationToken,
  private[javafx] val plan: SurfaceRenderPlan,
  private[javafx] val layouts: Vector[JavaFxAtlasPixelLayout],
  private[javafx] val atlases: Vector[JavaFxPreparedAtlasUpdate]
):
  private[javafx] def matches(next: SurfaceRenderPlan): Boolean =
    JavaFxColorPreparationBasis.sameContent(plan, next)

/** Capture on the Application Thread; preparation uses only immutable CPU input.
  * The existing legacy layout is supported. Affine sampler admission and
  * adaptive refinement remain separate from the worker preparation contract.
  */
final class JavaFxColorPreparationBasis private[javafx] (
  private val owner: JavaFxColorPreparationToken,
  private val meshes: Vector[SurfaceMeshPacket],
  private val layouts: Vector[JavaFxAtlasPixelLayout],
  private val config: JavaFxAtlasConfig
):
  def prepare(plan: SurfaceRenderPlan): Either[JavaFxSurfaceError, JavaFxPreparedColorUpdate] =
    if config.encoding != JavaFxAtlasEncoding.LegacyTriangle then
      return Left(JavaFxSurfaceError.SamplerUnqualified(config.encoding))
    JavaFxSurfaceBackend.validateCapabilities(plan) match
      case Left(error) => return Left(error)
      case Right(_) => ()
    if meshes.length != plan.meshes.length || !meshes.zip(plan.meshes).forall((a, b) =>
        a.surface == b.surface && a.resourceKey == b.resourceKey &&
          java.util.Arrays.equals(a.indices.unsafeArray, b.indices.unsafeArray) &&
          java.util.Arrays.equals(a.positions.unsafeArray, b.positions.unsafeArray) &&
          java.util.Arrays.equals(a.normals.unsafeArray, b.normals.unsafeArray)) then
      return Left(JavaFxSurfaceError.IncompatiblePlan("geometry changed; prepare again from the mounted scene"))
    JavaFxSurfaceProbe.validatePlan(plan, JavaFxSurfaceProgram.materialMode(plan), config).map: colors =>
      val prepared = layouts.map: layout =>
        val mesh = plan.meshes.find(_.surface == layout.surface).get
        JavaFxAtlasPixels.prepare(layout, mesh.indices, colors(layout.surface))
      new JavaFxPreparedColorUpdate(owner, plan, layouts, prepared)

private[javafx] object JavaFxColorPreparationBasis:
  /** Case-class equality binds every packet field, including coverage, scalar
    * data and sample provenance. Read-only buffers must be the prepared buffers;
    * caller-visible keys alone are never accepted as evidence of equality.
    */
  def sameContent(a: SurfaceRenderPlan, b: SurfaceRenderPlan): Boolean =
    a.meshes == b.meshes && a.layers == b.layers && a.lighting == b.lighting &&
      a.fragmentSurfaces == b.fragmentSurfaces && a.colorPipeline == b.colorPipeline &&
      a.surfaceOpacities == b.surfaceOpacities && a.surfaceBase == b.surfaceBase

private[javafx] object JavaFxAtlasPixels:
  def prepare(layout: JavaFxAtlasPixelLayout, indices: IntBufferView, colors: Array[Int]): JavaFxPreparedAtlasUpdate =
    val pixels = new Array[Int](layout.width * layout.height)
    val coordinates = new Array[Float](layout.faceCount * 6)
    val columns = layout.width / layout.tileSize
    val denominator = (layout.tileSize - 1).toDouble
    var face = 0
    while face < layout.faceCount do
      val source = (layout.faceStart + face) * 3
      val a = colors(indices(source)); val b = colors(indices(source + 1)); val c = colors(indices(source + 2))
      val x = (face % columns) * layout.tileSize
      val y = (face / columns) * layout.tileSize
      var row = 0
      while row < layout.tileSize do
        var column = 0
        while column < layout.tileSize do
          var wb = column / denominator
          var wc = (layout.tileSize - 1 - row) / denominator
          var wa = 1.0 - wb - wc
          if wa < 0.0 then
            val sum = wb + wc
            wb /= sum; wc /= sum; wa = 0.0
          pixels((y + row) * layout.width + x + column) = interpolate(a, b, c, wa, wb, wc)
          column += 1
        row += 1
      val left = (x + 0.5f) / layout.width
      val right = (x + layout.tileSize - 0.5f) / layout.width
      val top = (y + 0.5f) / layout.height
      val bottom = (y + layout.tileSize - 0.5f) / layout.height
      val offset = face * 6
      if a == b && b == c then
        var corner = 0
        while corner < 3 do
          coordinates(offset + corner * 2) = (left + right) * 0.5f
          coordinates(offset + corner * 2 + 1) = (top + bottom) * 0.5f
          corner += 1
      else
        coordinates(offset) = left; coordinates(offset + 1) = bottom
        coordinates(offset + 2) = right; coordinates(offset + 3) = bottom
        coordinates(offset + 4) = left; coordinates(offset + 5) = top
      face += 1
    JavaFxPreparedAtlasUpdate(pixels, coordinates)

  private def interpolate(a: Int, b: Int, c: Int, wa: Double, wb: Double, wc: Double): Int =
    def channel(shift: Int): Int =
      math.round(((a >>> shift) & 255) * wa + ((b >>> shift) & 255) * wb + ((c >>> shift) & 255) * wc)
        .toInt.max(0).min(255)
    val alpha = channel(0)
    val red = (channel(24) * alpha + 127) / 255
    val green = (channel(16) * alpha + 127) / 255
    val blue = (channel(8) * alpha + 127) / 255
    (alpha << 24) | (red << 16) | (green << 8) | blue
