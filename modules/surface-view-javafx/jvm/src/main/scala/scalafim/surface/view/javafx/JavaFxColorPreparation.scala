package scalafim.surface.view.javafx

import java.nio.IntBuffer
import scalafim.surface.view.*

/** CPU-only atlas description. It owns no JavaFX node, image or mutable native buffer. */
private[javafx] final case class JavaFxAtlasPixelLayout(
  faceStart: Int, faceCount: Int, width: Int, height: Int, tileSize: Int,
  encoding: JavaFxAtlasEncoding, adaptiveLayout: Option[JavaFxAdaptiveLayout]
)

private[javafx] final case class JavaFxColorChunkBasis(
  surface: SurfaceId, meshKey: SurfaceResourceKey, atlas: JavaFxAtlasPixelLayout,
  pointCapacity: Int, normalCapacity: Int, textureCapacity: Int
)

private[javafx] final case class JavaFxPreparedAtlasUpdate(
  pixels: Array[Int]
)

private[javafx] final case class JavaFxPreparedMeshData(
  points: Array[Float], normals: Array[Float], coordinates: Array[Float], faces: Array[Int]
)

private[javafx] final class JavaFxColorPreparationToken

/** Owned worker output. Arrays are private and are never mutated after preparation. */
final class JavaFxPreparedColorUpdate private[javafx] (
  private[javafx] val owner: JavaFxColorPreparationToken,
  private[javafx] val plan: SurfaceRenderPlan,
  private[javafx] val mode: JavaFxMaterialMode,
  private[javafx] val colorsBySurface: Map[SurfaceId, Array[Int]],
  private[javafx] val chunks: Vector[JavaFxColorChunkBasis],
  private[javafx] val layouts: Vector[Option[JavaFxAdaptiveLayout]],
  private[javafx] val atlases: Vector[JavaFxPreparedAtlasUpdate],
  private[javafx] val replacementMeshes: Vector[Option[JavaFxPreparedMeshData]],
  private[javafx] val geometryChanged: Boolean,
  val requiresRebuild: Boolean
):
  private[javafx] def matches(next: SurfaceRenderPlan): Boolean =
    JavaFxColorPreparationBasis.sameContent(plan, next)

/** Capture on the FX thread, then prepare retained colours on any worker.
  * The captured inputs and returned pixels contain no native objects. Public
  * render-plan buffers are read-only; internal unsafeArray mutation during
  * preparation or publication is prohibited and cannot be detected by identity.
  */
final class JavaFxColorPreparationBasis private[javafx] (
  private val owner: JavaFxColorPreparationToken,
  private val meshes: Vector[SurfaceMeshPacket],
  private val chunks: Vector[JavaFxColorChunkBasis],
  private val config: JavaFxAtlasConfig
):
  def prepare(plan: SurfaceRenderPlan): Either[JavaFxSurfaceError, JavaFxPreparedColorUpdate] =
    if !config.encoding.retainsLayout then
      Left(JavaFxSurfaceError.IncompatiblePlan("worker atlas preparation requires retained affine layout"))
    else prepareChecked(plan, JavaFxSurfaceProgram.materialMode(plan), renderAtlases = true)

  private[javafx] def prepareChecked(plan: SurfaceRenderPlan, mode: JavaFxMaterialMode,
      renderAtlases: Boolean = false): Either[JavaFxSurfaceError, JavaFxPreparedColorUpdate] =
    if meshes.map(m => (m.surface, m.resourceKey)) != plan.meshes.map(m => (m.surface, m.resourceKey)) then
      return Left(JavaFxSurfaceError.IncompatiblePlan("mesh resource keys changed"))
    // Keys are hashes, not proofs. A CPU comparison checks actual topology
    // before a captured layout can be reused, even for recompiled equal packets.
    var meshIndex = 0
    var geometryChanged = false
    while meshIndex < meshes.length do
      if !java.util.Arrays.equals(meshes(meshIndex).indices.unsafeArray, plan.meshes(meshIndex).indices.unsafeArray) then
        return Left(JavaFxSurfaceError.IncompatiblePlan("original face topology changed"))
      if !java.util.Arrays.equals(meshes(meshIndex).positions.unsafeArray, plan.meshes(meshIndex).positions.unsafeArray) ||
          !java.util.Arrays.equals(meshes(meshIndex).normals.unsafeArray, plan.meshes(meshIndex).normals.unsafeArray) then
        geometryChanged = true
      meshIndex += 1
    val colors = JavaFxSurfaceProbe.validatePlan(plan, mode, config) match
      case Left(error) => return Left(error)
      case Right(value) => value
    var changed = false
    val layouts = Vector.newBuilder[Option[JavaFxAdaptiveLayout]]
    var faces = 0L
    var chunkIndex = 0
    while chunkIndex < chunks.length do
      val chunk = chunks(chunkIndex)
      val packet = plan.meshes.find(_.surface == chunk.surface).get
      val layout = chunk.atlas.adaptiveLayout.map: before =>
        if config.encoding.retainsLayout then
          val same = JavaFxAdaptiveAtlas.matches(before, packet.indices, colors(packet.surface),
            chunk.atlas.faceStart, chunk.atlas.faceCount, retain = true)
          if same then before
          else
            changed = true
            JavaFxAdaptiveAtlas.layout(packet.indices, colors(packet.surface),
              chunk.atlas.faceStart, chunk.atlas.faceCount, Some(before), preferMedian = true)
        else
          val same = JavaFxAdaptiveAtlas.matches(before, packet.indices, colors(packet.surface),
            chunk.atlas.faceStart, chunk.atlas.faceCount)
          if !same then changed = true
          if same then before else JavaFxAdaptiveAtlas.layout(packet.indices, colors(packet.surface),
            chunk.atlas.faceStart, chunk.atlas.faceCount)
      faces += layout.fold(chunk.atlas.faceCount.toLong * config.encoding.facesPerSource)(_.renderedFaces.toLong)
      layouts += layout
      val layoutChanged = layout != chunk.atlas.adaptiveLayout
      val count = JavaFxSurfaceProbe.attributeCount(packet, chunk.atlas.faceCount, config.encoding, layout) match
        case Left(error) => return Left(error)
        case Right(value) => value
      if !layoutChanged && (count != chunk.pointCapacity || count != chunk.normalCapacity) then
        return Left(JavaFxSurfaceError.IncompatiblePlan("morph buffers changed vertex count"))
      if !layoutChanged && layout.fold(chunk.atlas.faceCount * config.encoding.facesPerSource)(_.renderedFaces) * 6 != chunk.textureCapacity then
        return Left(JavaFxSurfaceError.IncompatiblePlan("texture-coordinate capacity changed"))
      chunkIndex += 1
    if faces > config.maxRenderedFaces then
      return Left(JavaFxSurfaceError.IncompatiblePlan(s"rendered faces $faces exceed budget ${config.maxRenderedFaces}"))
    val resultingLayouts = layouts.result()
    val rendered =
      if renderAtlases then chunks.zip(resultingLayouts).map: (chunk, layout) =>
        val packet = plan.meshes.find(_.surface == chunk.surface).get
        JavaFxPreparedAtlasUpdate(JavaFxAtlasPixels.render(chunk.atlas.copy(adaptiveLayout = layout), packet.indices, colors(packet.surface)))
      else Vector.empty
    val replacements = chunks.zip(resultingLayouts).map: (chunk, layout) =>
      if !renderAtlases || layout == chunk.atlas.adaptiveLayout then None
      else
        val packet = plan.meshes.find(_.surface == chunk.surface).get
        val refined = layout.get
        Some(JavaFxPreparedMeshData(
          JavaFxAdaptiveAtlas.attributes(packet, packet.positions, chunk.atlas.faceStart, chunk.atlas.faceCount, refined),
          JavaFxAdaptiveAtlas.attributes(packet, packet.normals, chunk.atlas.faceStart, chunk.atlas.faceCount, refined),
          JavaFxAdaptiveAtlas.coordinates(packet, chunk.atlas.faceStart, chunk.atlas.faceCount,
            chunk.atlas.copy(adaptiveLayout = layout), colors(packet.surface), refined),
          JavaFxAdaptiveAtlas.faces(packet, chunk.atlas.faceStart, chunk.atlas.faceCount, refined)))
    Right(new JavaFxPreparedColorUpdate(owner, plan, mode, colors, chunks, resultingLayouts, rendered, replacements, geometryChanged, changed))

private[javafx] object JavaFxColorPreparationBasis:
  /** Cheap publication binding; camera, slots, chrome and readouts may rebase.
    * Replaced buffers are rejected even when their 32-bit keys are unchanged.
    */
  def sameContent(a: SurfaceRenderPlan, b: SurfaceRenderPlan): Boolean =
    val meshes = a.meshes.length == b.meshes.length && a.meshes.zip(b.meshes).forall: (x, y) =>
        x.surface == y.surface && x.resourceKey == y.resourceKey && x.geometryKey == y.geometryKey &&
          (x.positions eq y.positions) && (x.normals eq y.normals) && (x.indices eq y.indices)
    val layers = a.layers.length == b.layers.length && a.layers.zip(b.layers).forall: (x, y) =>
        x.layer == y.layer && x.surface == y.surface && x.resourceKey == y.resourceKey &&
          (x.colors eq y.colors) && x.opacity == y.opacity && x.blendMode == y.blendMode
    a.lighting == b.lighting && meshes && layers

private[javafx] object JavaFxAtlasPixels:
  def render(layout: JavaFxAtlasPixelLayout, indices: IntBufferView, colors: Array[Int]): Array[Int] =
    val result = new Array[Int](layout.width * layout.height)
    write(layout, IntBuffer.wrap(result), indices, colors)
    result

  def write(layout: JavaFxAtlasPixelLayout, target: IntBuffer, indices: IntBufferView,
      colors: Array[Int]): Unit =
    var localFace = 0
    while localFace < layout.faceCount do
      val offset = (layout.faceStart + localFace) * 3
      val a = colors(indices(offset)); val b = colors(indices(offset + 1)); val c = colors(indices(offset + 2))
      val columns = layout.width / layout.tileSize
      val x = (localFace % columns) * layout.tileSize
      val y = (localFace / columns) * layout.tileSize
      if layout.encoding.adaptive then
        val pivot = if layout.encoding.retainsLayout then layout.adaptiveLayout.get.anchor(localFace)
          else JavaFxAdaptiveAtlas.anchor(a, b, c)
        JavaFxAdaptiveAtlas.write(target, layout.width, x, y, a, b, c, pivot)
      else if layout.encoding == JavaFxAtlasEncoding.AffineMidpointOpaque then
        JavaFxAffineAtlas.write(target, layout.width, x, y, a, b, c)
      else
        val denominator = (layout.tileSize - 1).toDouble
        var row = 0
        while row < layout.tileSize do
          var col = 0
          while col < layout.tileSize do
            var wb = col / denominator
            var wc = (layout.tileSize - 1 - row) / denominator
            var wa = 1.0 - wb - wc
            if wa < 0.0 then
              val sum = wb + wc
              wb /= sum; wc /= sum; wa = 0.0
            target.put((y + row) * layout.width + x + col, interpolate(a, b, c, wa, wb, wc))
            col += 1
          row += 1
      localFace += 1

  private def interpolate(a: Int, b: Int, c: Int, wa: Double, wb: Double, wc: Double): Int =
    def channel(shift: Int): Int =
      math.round(((a >>> shift) & 255) * wa + ((b >>> shift) & 255) * wb + ((c >>> shift) & 255) * wc)
        .toInt.max(0).min(255)
    val alpha = channel(0)
    val red = (channel(24) * alpha + 127) / 255
    val green = (channel(16) * alpha + 127) / 255
    val blue = (channel(8) * alpha + 127) / 255
    (alpha << 24) | (red << 16) | (green << 8) | blue
