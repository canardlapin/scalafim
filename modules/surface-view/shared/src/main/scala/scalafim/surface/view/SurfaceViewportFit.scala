package scalafim.surface.view

/** Fits logical viewports before individual slots are rendered.
  * Mesh coordinates, camera state and slot order are unaffected.
  */
sealed trait SurfaceViewportFit:
  final def resolve(slots: Vector[SurfaceViewSlot], width: Double, height: Double): Vector[SurfaceViewSlot] =
    require(width.isFinite && height.isFinite && width > 0.0 && height > 0.0,
      "Viewport dimensions must be finite and positive")
    this match
      case SurfaceViewportFit.Fill => slots
      case SurfaceViewportFit.Contain(aspectRatio) =>
        val canvasAspect = width / height
        val scaleX = math.min(1.0, aspectRatio / canvasAspect)
        val scaleY = math.min(1.0, canvasAspect / aspectRatio)
        val offsetX = (1.0 - scaleX) * 0.5
        val offsetY = (1.0 - scaleY) * 0.5
        slots.map: slot =>
          val v = slot.viewport
          slot.copy(viewport = SurfaceViewport(offsetX + v.x * scaleX, offsetY + v.y * scaleY,
            v.width * scaleX, v.height * scaleY))
      case SurfaceViewportFit.Pack(aspectRatio) =>
        if slots.isEmpty then slots
        else
          // Maximize the common physical tile size. Equal scale keeps paired
          // surfaces comparable; a wider grid wins ties to avoid needless rows.
          def tileWidth(columns: Int): Double =
            val rows = 1 + (slots.length - 1) / columns
            math.min(width / columns, (height / rows) * aspectRatio)
          val columns = (1 to slots.length).maxBy(c => (tileWidth(c), c))
          val rows = 1 + (slots.length - 1) / columns
          val physicalWidth = tileWidth(columns)
          val physicalHeight = physicalWidth / aspectRatio
          val offsetY = math.max(0.0, height - rows * physicalHeight) * 0.5
          slots.zipWithIndex.map: (slot, index) =>
            val row = index / columns
            val column = index % columns
            val rowCount = math.min(columns, slots.length - row * columns)
            val offsetX = math.max(0.0, width - rowCount * physicalWidth) * 0.5
            slot.copy(viewport = SurfaceViewport(
              (offsetX + column * physicalWidth) / width,
              (offsetY + row * physicalHeight) / height,
              physicalWidth / width,
              physicalHeight / height
            ))

object SurfaceViewportFit:
  /** Interpret normalized slots over the full canvas. */
  case object Fill extends SurfaceViewportFit

  /** Center the group at a fixed physical width/height ratio, without cropping. */
  final case class Contain(aspectRatio: Double) extends SurfaceViewportFit:
    require(aspectRatio.isFinite && aspectRatio > 0.0, "Viewport aspect ratio must be finite and positive")

  /** Pack ordered, equal-scale tiles at a fixed physical width/height ratio.
    * Chooses the row/column grid giving the largest tiles without cropping,
    * centers each row, and replaces the input viewport rectangles. In contrast
    * to Contain, aspectRatio describes one tile rather than the entire group.
    * Mesh coordinates, cameras and the caller's fitted anatomical margins remain
    * unchanged. Resolve again on resize; no scientific scene rebuild is needed.
    */
  final case class Pack(aspectRatio: Double) extends SurfaceViewportFit:
    require(aspectRatio.isFinite && aspectRatio > 0.0, "Viewport aspect ratio must be finite and positive")
