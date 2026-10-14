package scalafim.surface.view

/** Fits the entire logical viewport group before individual slots are rendered.
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

object SurfaceViewportFit:
  /** Interpret normalized slots over the full canvas. */
  case object Fill extends SurfaceViewportFit

  /** Center the group at a fixed physical width/height ratio, without cropping. */
  final case class Contain(aspectRatio: Double) extends SurfaceViewportFit:
    require(aspectRatio.isFinite && aspectRatio > 0.0, "Viewport aspect ratio must be finite and positive")
