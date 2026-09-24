package scalafim.image

import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Point

/** Neuroimaging syntax over image4s frame-owned D3 points.
  *
  * `Point[F, D3]` is the frame-typed world point: its RAS-mm coordinates cannot be mixed with another frame's without
  * a checked alignment. [[WorldPoint]] remains the unowned coordinate record for display and interchange; move between
  * the two with `WorldPoint.in(frame)`, `GridSpec.bind` and `toWorldPoint`.
  */
extension [F <: Frame[D3]](point: Point[F, D3])
  /** Right (+) / left (-) RAS coordinate in millimetres. */
  def x: Double = point.coordinates(0)

  /** Anterior (+) / posterior (-) RAS coordinate in millimetres. */
  def y: Double = point.coordinates(1)

  /** Superior (+) / inferior (-) RAS coordinate in millimetres. */
  def z: Double = point.coordinates(2)

  def along(axis: SpatialAxis): Double =
    point.coordinates(axis.index)

  /** The unowned coordinate record for this point. */
  def toWorldPoint: WorldPoint =
    WorldPoint.of(point)
