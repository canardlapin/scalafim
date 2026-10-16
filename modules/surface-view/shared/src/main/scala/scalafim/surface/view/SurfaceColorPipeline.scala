package scalafim.surface.view

import intaglio.Rgba32

/** Defines how display RGB values are interpreted and composited. */
enum SurfaceColorPipeline:
  /** Historical byte-space display behavior. */
  case LegacyDisplayBytes
  /** sRGB display values decoded to linear light for composition. */
  case DisplaySrgbLinearLight

object SurfaceDisplayDefaults:
  val LegacySurfaceBase: Rgba32 = Rgba32.unsafe(184, 184, 184)
