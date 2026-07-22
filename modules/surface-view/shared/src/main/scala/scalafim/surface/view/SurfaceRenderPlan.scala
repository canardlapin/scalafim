package scalafim.surface.view

import scalafim.graphics.*

opaque type SurfaceResourceKey = String

object SurfaceResourceKey:
  private[view] def apply(value: String): SurfaceResourceKey = value

  extension (key: SurfaceResourceKey)
    def value: String = key

final class FloatBufferView private[view] (private val values: Array[Float]):
  def length: Int = values.length
  inline def apply(index: Int): Float = values(index)
  private[scalafim] def unsafeArray: Array[Float] = values

final class IntBufferView private[view] (private val values: Array[Int]):
  def length: Int = values.length
  inline def apply(index: Int): Int = values(index)
  private[scalafim] def unsafeArray: Array[Int] = values

final case class SurfaceViewport(x: Double, y: Double, width: Double, height: Double)

/** A surface-specific viewport plus a display-only world translation. The
  * offset recenters independently stored hemispheres for bilateral viewing;
  * scientific coordinates and readouts remain unchanged.
  */
final case class SurfaceViewSlot(
  surface: SurfaceId,
  viewport: SurfaceViewport,
  worldOffsetX: Double = 0.0,
  worldOffsetY: Double = 0.0,
  worldOffsetZ: Double = 0.0
)

final case class SurfaceMeshPacket(
  surface: SurfaceId,
  resourceKey: SurfaceResourceKey,
  positions: FloatBufferView,
  normals: FloatBufferView,
  indices: IntBufferView,
  private val geometryRevision: Option[SurfaceResourceKey] = None
):
  def geometryKey: SurfaceResourceKey = geometryRevision.getOrElse(resourceKey)

final case class SurfaceLayerPacket(
  layer: SurfaceLayerId,
  surface: SurfaceId,
  resourceKey: SurfaceResourceKey,
  colors: IntBufferView,
  opacity: DisplayOpacity,
  blendMode: DisplayBlendMode
)

final case class SurfaceCameraPacket(
  viewMatrix: FloatBufferView,
  projectionMatrix: FloatBufferView,
  directionX: Double,
  directionY: Double,
  directionZ: Double
)

final case class SurfaceDrawPass(
  slot: Int,
  mesh: SurfaceResourceKey,
  layer: SurfaceResourceKey,
  blendMode: DisplayBlendMode
)

final case class SurfaceReadout(
  surface: SurfaceId,
  vertex: Int,
  worldX: Double,
  worldY: Double,
  worldZ: Double,
  layerValues: Vector[(SurfaceLayerId, String)]
)

final case class SurfaceProfile(
  meshesPacked: Int,
  verticesPacked: Int,
  facesPacked: Int,
  layersColored: Int,
  colorValuesWritten: Int,
  primitiveBytes: Long
)

final case class SurfaceRenderReceipt(
  meshKeys: Vector[SurfaceResourceKey],
  layerKeys: Vector[SurfaceResourceKey],
  cameraKey: String,
  drawPassCount: Int,
  timepoint: Int
)

final case class SurfaceRenderPlan(
  slots: Vector[SurfaceViewSlot],
  meshes: Vector[SurfaceMeshPacket],
  layers: Vector[SurfaceLayerPacket],
  camera: SurfaceCameraPacket,
  lighting: SurfaceLighting,
  clipping: SurfaceClipping,
  drawPasses: Vector[SurfaceDrawPass],
  chrome: Scene,
  readouts: Vector[SurfaceReadout],
  profile: SurfaceProfile,
  receipt: SurfaceRenderReceipt
)
