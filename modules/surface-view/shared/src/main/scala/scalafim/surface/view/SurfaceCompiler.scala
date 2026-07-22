package scalafim.surface.view

import scalafim.graphics.*
import scalafim.surface.*
import scala.util.hashing.MurmurHash3

object SurfaceCompiler:
  private final case class CameraFrame(x: Double, y: Double, z: Double, radius: Double):
    def stableKey: String = s"$x:$y:$z:$radius"

  private final case class GeometryFrame(
    from: SurfaceGeometry,
    to: SurfaceGeometry,
    fraction: Double,
    deformation: Option[SurfaceLensDeformation] = None
  ):
    inline def coordinateAt(offset: Int): Double =
      deformation match
        case Some(value) => from.mesh.coordinates(offset) + fraction * value.displacementAtUnsafe(offset)
        case None => from.mesh.coordinates(offset) + fraction * (to.mesh.coordinates(offset) - from.mesh.coordinates(offset))

  def compile(model: SurfaceViewerModel, state: SurfaceViewerState): Either[SurfaceViewError, SurfaceRenderPlan] =
    for
      _ <- SurfaceViewer.validateLayout(model, state.layout)
      _ <-
        if state.timepoint >= 0 && state.timepoint < model.frameCount then Right(())
        else Left(SurfaceViewError.TimepointOutOfBounds(state.timepoint, model.frameCount))
      frames <- resolveFrames(model, state)
    yield compileUnsafe(model, state, frames)

  private def compileUnsafe(
    model: SurfaceViewerModel,
    state: SurfaceViewerState,
    frames: Map[SurfaceId, GeometryFrame]
  ): SurfaceRenderPlan =
    val viewportSlots = compileSlots(state.layout)
    val assets = viewportSlots.map(slot => model.surface(slot.surface).get)
    val meshes = assets.map(asset => packMesh(asset, frames(asset.id)))
    val frame = familyFrame(assets)
    val slots = centerBilateralSlots(state.layout, viewportSlots, assets, frame)
    val visibleSurfaces = slots.iterator.map(_.surface).toSet
    val orderedLayers = state.layerOrder.flatMap(model.layer)
    val layerPackets = Vector.newBuilder[SurfaceLayerPacket]
    val passes = Vector.newBuilder[SurfaceDrawPass]
    var colorValuesWritten = 0

    var layerIndex = 0
    while layerIndex < orderedLayers.length do
      val layer = orderedLayers(layerIndex)
      val presentation = state.presentations(layer.id)
      if presentation.visible && visibleSurfaces(layer.surfaceId) then
        val colors = new Array[Int](layer.geometry.vertexCount)
        layer.writeColors(state.timepoint, presentation, colors)
        val key = layerKey(layer, state.timepoint, presentation, colors)
        layerPackets += SurfaceLayerPacket(
          layer.id,
          layer.surfaceId,
          key,
          new IntBufferView(colors),
          presentation.opacity,
          layer.blendMode
        )
        val slot = slots.indexWhere(_.surface == layer.surfaceId)
        val meshKey = meshes(slot).resourceKey
        passes += SurfaceDrawPass(slot, meshKey, key, layer.blendMode)
        colorValuesWritten += colors.length
      layerIndex += 1

    val layers = layerPackets.result()
    val drawPasses = passes.result()
    val camera = cameraPacket(state.camera, frame)
    val readouts = compileReadout(model, state, frames)
    val chrome = compileChrome(readouts)
    val vertices = assets.iterator.map(_.domain.vertexCount).sum
    val faces = assets.iterator.map(_.domain.faceCount).sum
    val primitiveBytes =
      vertices.toLong * 3L * 4L * 2L + faces.toLong * 3L * 4L + colorValuesWritten.toLong * 4L + 32L * 4L
    val profile = SurfaceProfile(meshes.length, vertices, faces, layers.length, colorValuesWritten, primitiveBytes)
    val receipt = SurfaceRenderReceipt(
      meshes.map(_.resourceKey),
      layers.map(_.resourceKey),
      cameraKey(state.camera, frame),
      drawPasses.length,
      state.timepoint
    )
    SurfaceRenderPlan(
      slots,
      meshes,
      layers,
      camera,
      state.lighting,
      state.clipping,
      drawPasses,
      chrome,
      readouts,
      profile,
      receipt
    )

  private def compileChrome(readouts: Vector[SurfaceReadout]): Scene =
    readouts.headOption match
      case None => Scene.empty
      case Some(readout) =>
        val values = readout.layerValues.map((id, value) => s"${id.value}=$value").mkString("  ")
        val label = s"${readout.surface.value}  vertex=${readout.vertex}  $values".trim
        Scene(Vector(Grob.textUnsafe(
          label,
          Point.npcUnsafe(0.015, 0.985),
          anchor = Anchor(HJust.Left, VJust.Top),
          gp = GraphicParams.unsafe(fill = Some(Rgba.White), stroke = None)
        )))

  private def compileSlots(layout: SurfaceLayout): Vector[SurfaceViewSlot] =
    layout match
      case SurfaceLayout.Single(surface) =>
        Vector(SurfaceViewSlot(surface, SurfaceViewport(0.0, 0.0, 1.0, 1.0)))
      case SurfaceLayout.Bilateral(left, right, BilateralOrder.LeftThenRight) =>
        Vector(
          SurfaceViewSlot(left, SurfaceViewport(0.0, 0.0, 0.5, 1.0)),
          SurfaceViewSlot(right, SurfaceViewport(0.5, 0.0, 0.5, 1.0))
        )
      case SurfaceLayout.Bilateral(left, right, BilateralOrder.RightThenLeft) =>
        Vector(
          SurfaceViewSlot(right, SurfaceViewport(0.0, 0.0, 0.5, 1.0)),
          SurfaceViewSlot(left, SurfaceViewport(0.5, 0.0, 0.5, 1.0))
        )

  private def centerBilateralSlots(
    layout: SurfaceLayout,
    slots: Vector[SurfaceViewSlot],
    assets: Vector[SurfaceAsset],
    frame: CameraFrame
  ): Vector[SurfaceViewSlot] =
    layout match
      case _: SurfaceLayout.Bilateral =>
        slots.zip(assets).map: (slot, asset) =>
          val local = familyFrame(Vector(asset))
          slot.copy(
            worldOffsetX = frame.x - local.x,
            worldOffsetY = frame.y - local.y,
            worldOffsetZ = frame.z - local.z
          )
      case _: SurfaceLayout.Single => slots

  private def packMesh(asset: SurfaceAsset, frame: GeometryFrame): SurfaceMeshPacket =
    val from = frame.from.mesh.coordinates
    val to = frame.to.mesh.coordinates
    val positions = new Array[Float](asset.domain.vertexCount * 3)
    val transform = frame.from.surfaceToWorld
    var vertex = 0
    while vertex < asset.domain.vertexCount do
      val offset = vertex * 3
      val x = frame.coordinateAt(offset)
      val y = frame.coordinateAt(offset + 1)
      val z = frame.coordinateAt(offset + 2)
      val wx = transform(0, 0) * x + transform(0, 1) * y + transform(0, 2) * z + transform(0, 3)
      val wy = transform(1, 0) * x + transform(1, 1) * y + transform(1, 2) * z + transform(1, 3)
      val wz = transform(2, 0) * x + transform(2, 1) * y + transform(2, 2) * z + transform(2, 3)
      val ww = transform(3, 0) * x + transform(3, 1) * y + transform(3, 2) * z + transform(3, 3)
      val inverseW = if ww == 0.0 then 1.0 else 1.0 / ww
      positions(offset) = (wx * inverseW).toFloat
      positions(offset + 1) = (wy * inverseW).toFloat
      positions(offset + 2) = (wz * inverseW).toFloat
      vertex += 1

    val indices = asset.topologyIndices
    val indexArray = indices.unsafeArray
    val normals = computeNormals(positions, indexArray)
    val key = topologyKey(asset)
    val geometryKey = meshKey(asset, positions)
    SurfaceMeshPacket(
      asset.id,
      key,
      new FloatBufferView(positions),
      new FloatBufferView(normals),
      indices,
      Some(geometryKey)
    )

  private def computeNormals(positions: Array[Float], indices: Array[Int]): Array[Float] =
    val accum = new Array[Double](positions.length)
    var face = 0
    while face < indices.length do
      val a = indices(face) * 3
      val b = indices(face + 1) * 3
      val c = indices(face + 2) * 3
      val abx = positions(b) - positions(a)
      val aby = positions(b + 1) - positions(a + 1)
      val abz = positions(b + 2) - positions(a + 2)
      val acx = positions(c) - positions(a)
      val acy = positions(c + 1) - positions(a + 1)
      val acz = positions(c + 2) - positions(a + 2)
      val nx = aby * acz - abz * acy
      val ny = abz * acx - abx * acz
      val nz = abx * acy - aby * acx
      accum(a) += nx; accum(a + 1) += ny; accum(a + 2) += nz
      accum(b) += nx; accum(b + 1) += ny; accum(b + 2) += nz
      accum(c) += nx; accum(c + 1) += ny; accum(c + 2) += nz
      face += 3
    val normals = new Array[Float](positions.length)
    var offset = 0
    while offset < normals.length do
      val x = accum(offset)
      val y = accum(offset + 1)
      val z = accum(offset + 2)
      val norm = math.sqrt(x * x + y * y + z * z)
      if norm > 0.0 then
        normals(offset) = (x / norm).toFloat
        normals(offset + 1) = (y / norm).toFloat
        normals(offset + 2) = (z / norm).toFloat
      offset += 3
    normals

  private def familyFrame(assets: Vector[SurfaceAsset]): CameraFrame =
    var minimumX = Double.PositiveInfinity
    var minimumY = Double.PositiveInfinity
    var minimumZ = Double.PositiveInfinity
    var maximumX = Double.NegativeInfinity
    var maximumY = Double.NegativeInfinity
    var maximumZ = Double.NegativeInfinity
    var assetIndex = 0
    while assetIndex < assets.length do
      val bounds = assets(assetIndex).cameraBounds
      minimumX = math.min(minimumX, bounds.minimumX)
      minimumY = math.min(minimumY, bounds.minimumY)
      minimumZ = math.min(minimumZ, bounds.minimumZ)
      maximumX = math.max(maximumX, bounds.maximumX)
      maximumY = math.max(maximumY, bounds.maximumY)
      maximumZ = math.max(maximumZ, bounds.maximumZ)
      assetIndex += 1
    val dx = maximumX - minimumX
    val dy = maximumY - minimumY
    val dz = maximumZ - minimumZ
    CameraFrame(
      (minimumX + maximumX) * 0.5,
      (minimumY + maximumY) * 0.5,
      (minimumZ + maximumZ) * 0.5,
      0.5 * math.sqrt(dx * dx + dy * dy + dz * dz)
    )

  private def cameraPacket(camera: SurfaceCamera, frame: CameraFrame): SurfaceCameraPacket =
    val (baseX, baseY, baseZ) = camera.viewpoint.cameraDirection
    val yaw = camera.orbit.yawDegrees * math.Pi / 180.0
    val yawCos = math.cos(yaw)
    val yawSin = math.sin(yaw)
    val yawedX = yawCos * baseX - yawSin * baseY
    val yawedY = yawSin * baseX + yawCos * baseY
    val yawedZ = baseZ
    val horizontal = math.sqrt(yawedX * yawedX + yawedY * yawedY)
    val (rightX, rightY) =
      if horizontal > 1e-12 then (-yawedY / horizontal, yawedX / horizontal)
      else (yawCos, yawSin)
    val pitch = camera.orbit.pitchDegrees * math.Pi / 180.0
    val pitchCos = math.cos(pitch)
    val pitchSin = math.sin(pitch)
    val crossX = rightY * yawedZ
    val crossY = -rightX * yawedZ
    val crossZ = rightX * yawedY - rightY * yawedX
    val dx = yawedX * pitchCos + crossX * pitchSin
    val dy = yawedY * pitchCos + crossY * pitchSin
    val dz = yawedZ * pitchCos + crossZ * pitchSin
    // Keep the camera outside the canonical bounding sphere regardless of
    // whether coordinates are normalized units or anatomical millimetres.
    val eyeDistance = math.max(4.0, frame.radius * 2.0) / camera.zoom.value
    val targetX = frame.x + camera.panX
    val targetY = frame.y + camera.panY
    val targetZ = frame.z
    val ex = dx * eyeDistance + targetX
    val ey = dy * eyeDistance + targetY
    val ez = dz * eyeDistance + targetZ
    val (upx, upy, upz) = if math.abs(dz) > 0.9 then (0.0, 1.0, 0.0) else (0.0, 0.0, 1.0)
    val view = lookAt(ex, ey, ez, targetX, targetY, targetZ, upx, upy, upz)
    val projection = camera.projection match
      case CameraProjection.Perspective(fov) => perspective(fov.value, 1.0, 0.01, 1000.0)
      case CameraProjection.Orthographic(scale) => orthographic(scale.value / camera.zoom.value)
    SurfaceCameraPacket(new FloatBufferView(view), new FloatBufferView(projection), dx, dy, dz)

  private def lookAt(
    ex: Double, ey: Double, ez: Double,
    tx: Double, ty: Double, tz: Double,
    upx: Double, upy: Double, upz: Double
  ): Array[Float] =
    var fx = tx - ex; var fy = ty - ey; var fz = tz - ez
    val fn = math.sqrt(fx * fx + fy * fy + fz * fz); fx /= fn; fy /= fn; fz /= fn
    var sx = fy * upz - fz * upy; var sy = fz * upx - fx * upz; var sz = fx * upy - fy * upx
    val sn = math.sqrt(sx * sx + sy * sy + sz * sz); sx /= sn; sy /= sn; sz /= sn
    val ux = sy * fz - sz * fy; val uy = sz * fx - sx * fz; val uz = sx * fy - sy * fx
    Array(
      sx.toFloat, sy.toFloat, sz.toFloat, (-(sx * ex + sy * ey + sz * ez)).toFloat,
      ux.toFloat, uy.toFloat, uz.toFloat, (-(ux * ex + uy * ey + uz * ez)).toFloat,
      (-fx).toFloat, (-fy).toFloat, (-fz).toFloat, (fx * ex + fy * ey + fz * ez).toFloat,
      0.0f, 0.0f, 0.0f, 1.0f
    )

  private def perspective(fovDegrees: Double, aspect: Double, near: Double, far: Double): Array[Float] =
    val f = 1.0 / math.tan(fovDegrees * math.Pi / 360.0)
    Array(
      (f / aspect).toFloat, 0.0f, 0.0f, 0.0f,
      0.0f, f.toFloat, 0.0f, 0.0f,
      0.0f, 0.0f, ((far + near) / (near - far)).toFloat, ((2.0 * far * near) / (near - far)).toFloat,
      0.0f, 0.0f, -1.0f, 0.0f
    )

  private def orthographic(scale: Double): Array[Float] =
    val inverse = 1.0 / scale
    Array(
      inverse.toFloat, 0.0f, 0.0f, 0.0f,
      0.0f, inverse.toFloat, 0.0f, 0.0f,
      0.0f, 0.0f, -0.00200002f, -1.00002f,
      0.0f, 0.0f, 0.0f, 1.0f
    )

  private def compileReadout(
    model: SurfaceViewerModel,
    state: SurfaceViewerState,
    frames: Map[SurfaceId, GeometryFrame]
  ): Vector[SurfaceReadout] =
    state.selection.toVector.flatMap: selection =>
      model.surface(selection.surface).map: asset =>
        val vertex = selection.vertex.index
        val frame = frames(asset.id)
        val offset = vertex * 3
        val from = frame.from.mesh.coordinates
        val to = frame.to.mesh.coordinates
        val px = frame.coordinateAt(offset)
        val py = frame.coordinateAt(offset + 1)
        val pz = frame.coordinateAt(offset + 2)
        val transform = frame.from.surfaceToWorld
        val tx = transform(0, 0) * px + transform(0, 1) * py + transform(0, 2) * pz + transform(0, 3)
        val ty = transform(1, 0) * px + transform(1, 1) * py + transform(1, 2) * pz + transform(1, 3)
        val tz = transform(2, 0) * px + transform(2, 1) * py + transform(2, 2) * pz + transform(2, 3)
        val tw = transform(3, 0) * px + transform(3, 1) * py + transform(3, 2) * pz + transform(3, 3)
        val inverseW = if tw == 0.0 then 1.0 else 1.0 / tw
        val values = state.layerOrder.flatMap: id =>
          model.layer(id).filter(_.surfaceId == selection.surface).map(layer => id -> layer.describe(vertex, state.timepoint))
        SurfaceReadout(selection.surface, vertex, tx * inverseW, ty * inverseW, tz * inverseW, values)

  private def meshKey(asset: SurfaceAsset, positions: Array[Float]): SurfaceResourceKey =
    var hash = MurmurHash3.stringHash(asset.domain.display)
    var index = 0
    while index < positions.length do
      hash = MurmurHash3.mix(hash, java.lang.Float.floatToIntBits(positions(index)))
      index += 1
    SurfaceResourceKey(s"mesh:${asset.id.value}:${asset.domain.topology.stableKey}:${hex(MurmurHash3.finalizeHash(hash, positions.length))}")

  private def topologyKey(asset: SurfaceAsset): SurfaceResourceKey =
    SurfaceResourceKey(s"mesh:${asset.id.value}:${asset.domain.display}:${asset.domain.topology.stableKey}")

  private def resolveFrames(
    model: SurfaceViewerModel,
    state: SurfaceViewerState
  ): Either[SurfaceViewError, Map[SurfaceId, GeometryFrame]] =
    val expected = model.surfaces.iterator.map(_.id).toSet
    if state.geometryPresentations.keySet != expected then
      Left(SurfaceViewError.IncompatibleMorph("geometry presentations must exactly match viewer surfaces"))
    else
      val resolved = Map.newBuilder[SurfaceId, GeometryFrame]
      var index = 0
      while index < model.surfaces.length do
        val asset = model.surfaces(index)
        state.geometryPresentations(asset.id) match
          case SurfaceGeometryPresentation.Fixed(kind) =>
            asset.geometries.get(kind) match
              case None => return Left(SurfaceViewError.IncompatibleMorph(
                s"surface '${asset.id.value}' has no '${kind.label}' geometry"
              ))
              case Some(geometry) => resolved += asset.id -> GeometryFrame(geometry, geometry, 0.0)
          case SurfaceGeometryPresentation.Morphing(from, to, fraction) =>
            (asset.geometries.get(from), asset.geometries.get(to)) match
              case (Some(start), Some(end)) =>
                resolved += asset.id -> GeometryFrame(start, end, fraction.value)
              case _ => return Left(SurfaceViewError.IncompatibleMorph(
                s"surface '${asset.id.value}' lacks a requested morph endpoint"
              ))
          case SurfaceGeometryPresentation.RevealLens(from, to, deformation, fraction) =>
            (asset.geometries.get(from), asset.geometries.get(to)) match
              case (Some(start), Some(end)) if deformation.isCompatibleWith(start) =>
                resolved += asset.id -> GeometryFrame(start, end, fraction.value, Some(deformation))
              case (Some(_), Some(_)) => return Left(SurfaceViewError.IncompatibleMorph(
                s"surface '${asset.id.value}' reveal lens belongs to another mesh domain"
              ))
              case _ => return Left(SurfaceViewError.IncompatibleMorph(
                s"surface '${asset.id.value}' lacks a requested reveal-lens endpoint"
              ))
        index += 1
      Right(resolved.result())

  private def layerKey(
    layer: SurfaceLayer,
    timepoint: Int,
    presentation: SurfaceLayerPresentation,
    colors: Array[Int]
  ): SurfaceResourceKey =
    var hash = MurmurHash3.stringHash(layer.id.value)
    var index = 0
    while index < colors.length do
      hash = MurmurHash3.mix(hash, colors(index))
      index += 1
    hash = MurmurHash3.mix(hash, timepoint)
    hash = MurmurHash3.mix(hash, java.lang.Double.hashCode(presentation.opacity.toDouble))
    SurfaceResourceKey(s"layer:${layer.id.value}:$timepoint:${hex(MurmurHash3.finalizeHash(hash, colors.length + 2))}")

  private def cameraKey(camera: SurfaceCamera, frame: CameraFrame): String =
    s"${camera.viewpoint}:${camera.projection}:${camera.zoom.value}:${camera.panX}:${camera.panY}:${camera.orbit}:${frame.stableKey}"

  private def hex(value: Int): String =
    val raw = java.lang.Integer.toHexString(value)
    "0" * (8 - raw.length) + raw
