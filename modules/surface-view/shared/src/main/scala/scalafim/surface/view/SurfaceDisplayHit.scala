package scalafim.surface.view

/** Decoration identity never occupies the scientific vertex/face domain. */
final case class SurfaceDecorationHit(
  id: SurfaceDecorationElementId,
  origin: SurfaceDisplayOrigin,
  target: SurfaceDecorationTarget,
  depth: Double
)

enum SurfaceDisplayHit[+A]:
  case Scientific(value: A)
  case Decoration(value: SurfaceDecorationHit)
