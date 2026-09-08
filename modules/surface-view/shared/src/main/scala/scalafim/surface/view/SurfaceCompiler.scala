package scalafim.surface.view

import intaglio.*
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
      _ <- validateMappings(model, state)
      frames <- resolveFrames(model, state)
    yield compileUnsafe(model, state, frames)

  private def validateMappings(model: SurfaceViewerModel, state: SurfaceViewerState): Either[SurfaceViewError, Unit] =
    var index = 0
    while index < model.layers.length do
      val layer = model.layers(index)
      state.presentations.get(layer.id) match
        case None => return Left(SurfaceViewError.UnknownLayer(layer.id))
        case Some(presentation) => layer.effectiveScalarMapping(presentation) match
          case Left(error) => return Left(error)
          case Right(_) => ()
      index += 1
    Right(())

  private def compileUnsafe(
    model: SurfaceViewerModel,
    state: SurfaceViewerState,
    frames: Map[SurfaceId, GeometryFrame]
  ): SurfaceRenderPlan =
    val viewportSlots = compileSlots(state.layout)
    val assets = viewportSlots.map(slot => model.surface(slot.surface).get)
    val fragmentSurfaces = model.layers.groupBy(_.surfaceId).collect:
      case (surface, layers) if SurfaceFragmentEvaluator.required(layers.map(_.interpolation)) => surface
    .toSet
    val meshes = assets.map: asset =>
      val mesh = packMesh(asset, frames(asset.id))
      val lowered = if model.layers.exists(layer => layer.surfaceId == asset.id && layer.interpolation == SurfaceMapInterpolation.NearestVertex) then
        SurfaceNearestPartition.lower(mesh)
      else if model.layers.exists(layer => layer.surfaceId == asset.id && layer.association == SurfaceSampleAssociation.Face) then
        separateCorners(mesh)
      else mesh
      if fragmentSurfaces(asset.id) && lowered.sourceVertices.nonEmpty then lowered.copy(sampleNormals = Some(mesh.normals), samplePositions = Some(mesh.positions)) else lowered
    val frame = layoutFrame(state.layout, assets)
    val slots = centerBilateralSlots(state.layout, viewportSlots, assets, frame)
    val visibleSurfaces = slots.iterator.map(_.surface).toSet
    val orderedLayers = state.layerOrder.flatMap(model.layer)
    val layerPackets = Vector.newBuilder[SurfaceLayerPacket]
    val passes = Vector.newBuilder[SurfaceDrawPass]
    var colorValuesWritten = 0
    var sampleColorBytes = 0L

    var layerIndex = 0
    while layerIndex < orderedLayers.length do
      val layer = orderedLayers(layerIndex)
      val presentation = state.presentations(layer.id)
      if presentation.visible && visibleSurfaces(layer.surfaceId) then
        val samples = new Array[Int](layer.sampleCount)
        layer.writeColors(state.timepoint, presentation, samples)
        val mesh = meshes(slots.indexWhere(_.surface == layer.surfaceId))
        val colors = mesh.sourceVertices match
          case None => samples
          case Some(source) =>
            val expanded = new Array[Int](source.length)
            var corner = 0
            while corner < expanded.length do
              expanded(corner) = layer.interpolation match
                case SurfaceMapInterpolation.FaceConstant => samples(mesh.sourceFace(corner / 3))
                case SurfaceMapInterpolation.NearestVertex => samples(source(corner))
                case SurfaceMapInterpolation.VertexColor | SurfaceMapInterpolation.VertexScalar => samples(source(corner))
              corner += 1
            expanded
        val scalarSamples = layer.scalarSamples(state.timepoint)
        val mapping = layer.effectiveScalarMapping(presentation).toOption.get
        val scalarField = scalarSamples.map(values => new SurfaceScalarPacket(new DoubleBufferView(values), mapping.get))
        val key = layerKey(layer, state.timepoint, presentation, colors, scalarSamples)
        layerPackets += SurfaceLayerPacket(
          layer.id,
          layer.surfaceId,
          key,
          new IntBufferView(colors),
          presentation.opacity,
          layer.blendMode,
          layer.association,
          layer.interpolation,
          mapping,
          scalarField,
          Option.when(fragmentSurfaces(layer.surfaceId))(new IntBufferView(samples))
        )
        val slot = slots.indexWhere(_.surface == layer.surfaceId)
        val meshKey = meshes(slot).resourceKey
        passes += SurfaceDrawPass(slot, meshKey, key, layer.blendMode)
        colorValuesWritten += colors.length
        if fragmentSurfaces(layer.surfaceId) && (samples ne colors) then sampleColorBytes += samples.length.toLong * 4L
      layerIndex += 1

    val layers = layerPackets.result()
    val drawPasses = passes.result()
    val camera = cameraPacket(state.camera, frame, state.clipping)
    val readouts = compileReadout(model, state, frames)
    val chrome = compileChrome(readouts)
    val vertices = meshes.iterator.map(_.positions.length / 3).sum
    val faces = meshes.iterator.map(_.indices.length / 3).sum
    val primitiveBytes =
      vertices.toLong * 3L * 4L * 2L + faces.toLong * 3L * 4L + colorValuesWritten.toLong * 4L + 32L * 4L +
        meshes.iterator.flatMap(_.sourceVertices).map(_.length.toLong * 4L).sum +
        meshes.iterator.flatMap(_.nearestPartition).map(_.originalIndices.length.toLong * 4L).sum +
        layers.iterator.flatMap(_.scalarField).map(_.samples.length.toLong * 8L).sum + sampleColorBytes +
        meshes.iterator.flatMap(_.sampleNormals).map(_.length.toLong * 4L).sum +
        meshes.iterator.flatMap(_.samplePositions).map(_.length.toLong * 4L).sum
    val profile = SurfaceProfile(meshes.length, vertices, faces, layers.length, colorValuesWritten, primitiveBytes)
    val receipt = SurfaceRenderReceipt(
      meshes.map(_.resourceKey),
      layers.map(_.resourceKey),
      cameraKey(state.camera, frame) + (state.clipping match
        case SurfaceClipping.NearFar(near, far) => s":depth:$near:$far"
        case _ => ""),
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
      receipt,
      SurfaceViewportFit.Contain(slots.length.toDouble * state.camera.aspectRatio.value),
      fragmentSurfaces
    )

  /** Display-only corner expansion. Original face order and normals survive;
    * the source map translates picks back to scientific vertex identities.
    */
  private def separateCorners(mesh: SurfaceMeshPacket): SurfaceMeshPacket =
    val count = mesh.indices.length
    val positions = new Array[Float](count * 3)
    val normals = new Array[Float](count * 3)
    val indices = new Array[Int](count)
    val source = new Array[Int](count)
    var corner = 0
    while corner < count do
      val vertex = mesh.indices(corner)
      source(corner) = vertex
      indices(corner) = corner
      var axis = 0
      while axis < 3 do
        positions(corner * 3 + axis) = mesh.positions(vertex * 3 + axis)
        normals(corner * 3 + axis) = mesh.normals(vertex * 3 + axis)
        axis += 1
      corner += 1
    SurfaceMeshPacket(mesh.surface, SurfaceResourceKey(mesh.resourceKey.value + ":corners-v1"),
      new FloatBufferView(positions), new FloatBufferView(normals), new IntBufferView(indices),
      Some(SurfaceResourceKey(mesh.geometryKey.value + ":corners-v1")), Some(new IntBufferView(source)))

  private def layoutFrame(layout: SurfaceLayout, assets: Vector[SurfaceAsset]): CameraFrame =
    val worldFrame = familyFrame(assets)
    layout match
      case _: SurfaceLayout.Bilateral =>
        worldFrame.copy(radius = assets.map(asset => familyFrame(Vector(asset)).radius).max)
      case _: SurfaceLayout.Single => worldFrame

  /** Explicit fitting uses displayed bounds around the canonical anchor; ordinary morph/orbit/zoom never refits. */
  private[view] def fitCamera(model: SurfaceViewerModel, state: SurfaceViewerState): Either[SurfaceViewError, SurfaceCamera] =
    SurfaceViewer.validateLayout(model, state.layout).flatMap: _ =>
      resolveFrames(model, state).flatMap: frames =>
        val originalSlots = compileSlots(state.layout)
        val assets = originalSlots.map(slot => model.surface(slot.surface).get)
        val frame = layoutFrame(state.layout, assets)
        val slots = centerBilateralSlots(state.layout, originalSlots, assets, frame)
        val centered = state.camera.copy(zoom = CameraZoom.Default, panX = 0.0, panY = 0.0)
        val view = cameraPacket(centered, frame, SurfaceClipping.Disabled).viewMatrix
        val points = slots.zip(assets).flatMap: (slot, asset) =>
          val bounds = displayedBounds(asset, frames(asset.id))
          for
            x <- Vector(bounds.minimumX, bounds.maximumX)
            y <- Vector(bounds.minimumY, bounds.maximumY)
            z <- Vector(bounds.minimumZ, bounds.maximumZ)
          yield
            val rx = x + slot.worldOffsetX - frame.x
            val ry = y + slot.worldOffsetY - frame.y
            val rz = z + slot.worldOffsetZ - frame.z
            (view(0) * rx + view(1) * ry + view(2) * rz,
             view(4) * rx + view(5) * ry + view(6) * rz,
             view(8) * rx + view(9) * ry + view(10) * rz)
        val extentX = points.map(p => math.abs(p._1)).max
        val extentY = points.map(p => math.abs(p._2)).max
        val epsilon = math.max(1e-12, frame.radius * 1e-12)
        val aspect = if extentX > epsilon && extentY > epsilon then extentX / extentY else 1.0
        CameraAspectRatio.make(aspect).flatMap: ratio =>
          val fitting = centered.copy(aspectRatio = ratio)
          if math.max(extentX, extentY) <= epsilon then Right(fitting)
          else fitting.projection match
            case _: CameraProjection.Perspective =>
              val projection = cameraPacket(fitting, frame, SurfaceClipping.Disabled).projectionMatrix
              val required = points.map: (x, y, z) =>
                val extent = math.max(math.abs(x) * projection(0), math.abs(y) * projection(5)) / 0.9
                z + math.max(extent, math.max(0.02, frame.radius * 1e-6))
              .max
              CameraZoom.make(baseDistance(fitting, frame) / math.max(0.02, required))
                .map(zoom => fitting.copy(zoom = zoom))
            case _: CameraProjection.Orthographic =>
              OrthographicScale.make(math.max(extentY, extentX / ratio.value) / 0.9)
                .map(scale => fitting.copy(projection = CameraProjection.Orthographic(scale)))

  private def displayedBounds(asset: SurfaceAsset, frame: GeometryFrame): SurfaceWorldBounds =
    if frame.from == asset.geometry && frame.fraction == 0.0 then asset.cameraBounds
    else
      val positions = packMesh(asset, frame).positions
      var minimumX = Double.PositiveInfinity
      var minimumY = Double.PositiveInfinity
      var minimumZ = Double.PositiveInfinity
      var maximumX = Double.NegativeInfinity
      var maximumY = Double.NegativeInfinity
      var maximumZ = Double.NegativeInfinity
      var offset = 0
      while offset < positions.length do
        minimumX = math.min(minimumX, positions(offset).toDouble)
        minimumY = math.min(minimumY, positions(offset + 1).toDouble)
        minimumZ = math.min(minimumZ, positions(offset + 2).toDouble)
        maximumX = math.max(maximumX, positions(offset).toDouble)
        maximumY = math.max(maximumY, positions(offset + 1).toDouble)
        maximumZ = math.max(maximumZ, positions(offset + 2).toDouble)
        offset += 3
      SurfaceWorldBounds(minimumX, minimumY, minimumZ, maximumX, maximumY, maximumZ)

  private def compileChrome(readouts: Vector[SurfaceReadout]): Scene =
    readouts.headOption match
      case None => Scene.empty
      case Some(readout) =>
        val values = readout.layerValues.map((id, value) => s"${id.value}=$value").mkString("  ")
        val face = readout.face.fold("")(index => s"  face=$index")
        val label = s"${readout.surface.value}  vertex=${readout.vertex}$face  $values".trim
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

  private def cameraPacket(camera: SurfaceCamera, frame: CameraFrame, clipping: SurfaceClipping): SurfaceCameraPacket =
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
    val eyeDistance = baseDistance(camera, frame) / camera.zoom.value
    val targetX = frame.x + camera.panX
    val targetY = frame.y + camera.panY
    val targetZ = frame.z
    val ex = dx * eyeDistance + targetX
    val ey = dy * eyeDistance + targetY
    val ez = dz * eyeDistance + targetZ
    val (upx, upy, upz) = if math.abs(dz) > 0.9 then (0.0, 1.0, 0.0) else (0.0, 0.0, 1.0)
    val view = lookAt(ex, ey, ez, targetX, targetY, targetZ, upx, upy, upz)
    val projection = camera.projection match
      case CameraProjection.Perspective(fov) =>
        // Preserve depth precision and the far extent for anatomical coordinates
        // and narrow fields of view; the backend consumes these same planes.
        val near = math.max(0.01, eyeDistance / 10000.0)
        val far = math.max(1000.0, eyeDistance + frame.radius * 2.0)
        perspective(fov.value, camera.aspectRatio.value, near, far)
      case CameraProjection.Orthographic(scale) => orthographic(scale.value / camera.zoom.value, camera.aspectRatio.value)
    clipping match
      case SurfaceClipping.NearFar(near, far) =>
        // Change only depth coefficients. Keep the fitted field of view and
        // legacy default projection bytes unchanged when no range is supplied.
        camera.projection match
          case _: CameraProjection.Perspective =>
            projection(10) = ((far + near) / (near - far)).toFloat
            projection(11) = ((2.0 * far * near) / (near - far)).toFloat
          case _: CameraProjection.Orthographic =>
            projection(10) = (2.0 / (near - far)).toFloat
            projection(11) = ((far + near) / (near - far)).toFloat
      case _ => ()
    SurfaceCameraPacket(new FloatBufferView(view), new FloatBufferView(projection), dx, dy, dz)

  private def baseDistance(camera: SurfaceCamera, frame: CameraFrame): Double =
    val fitDistance = camera.projection match
      case CameraProjection.Perspective(fov) =>
        val limitingHalfAngle = math.atan(math.tan(fov.value * math.Pi / 360.0) * math.min(1.0, camera.aspectRatio.value))
        frame.radius / math.sin(limitingHalfAngle)
      case _: CameraProjection.Orthographic => frame.radius * 2.0
    math.max(4.0, fitDistance)

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

  private def orthographic(scale: Double, aspect: Double): Array[Float] =
    val inverse = 1.0 / scale
    Array(
      (inverse / aspect).toFloat, 0.0f, 0.0f, 0.0f,
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
          model.layer(id).filter(_.surfaceId == selection.surface).flatMap: layer =>
            val sample = layer.association match
              case SurfaceSampleAssociation.Vertex => Some(vertex)
              case SurfaceSampleAssociation.Face => selection.face.map(_.index)
            sample.map(index => id -> layer.describe(index, state.timepoint))
        SurfaceReadout(selection.surface, vertex, tx * inverseW, ty * inverseW, tz * inverseW, values, selection.face.map(_.index))

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
    colors: Array[Int],
    scalarSamples: Option[Array[Double]]
  ): SurfaceResourceKey =
    var hash = MurmurHash3.stringHash(layer.id.value)
    layer.effectiveScalarMapping(presentation).toOption.get.foreach: mapping =>
      hash = MurmurHash3.mix(hash, MurmurHash3.stringHash(mapping.canonicalKey))
    if layer.association == SurfaceSampleAssociation.Face then hash = MurmurHash3.mix(hash, 0xface)
    if layer.interpolation == SurfaceMapInterpolation.NearestVertex then hash = MurmurHash3.mix(hash, 0x6ea2)
    scalarSamples.foreach: values =>
      hash = MurmurHash3.mix(hash, 0x5ca1)
      var sample = 0
      while sample < values.length do
        hash = MurmurHash3.mix(hash, java.lang.Double.hashCode(values(sample)))
        sample += 1
    var index = 0
    while index < colors.length do
      hash = MurmurHash3.mix(hash, colors(index))
      index += 1
    hash = MurmurHash3.mix(hash, timepoint)
    hash = MurmurHash3.mix(hash, java.lang.Double.hashCode(presentation.opacity.toDouble))
    SurfaceResourceKey(s"layer:${layer.id.value}:$timepoint:${hex(MurmurHash3.finalizeHash(hash, colors.length + 2))}")

  private def cameraKey(camera: SurfaceCamera, frame: CameraFrame): String =
    s"camera-fit-v3:${camera.viewpoint}:${camera.projection}:${camera.aspectRatio.value}:${camera.zoom.value}:${camera.panX}:${camera.panY}:${camera.orbit}:${frame.stableKey}"

  private def hex(value: Int): String =
    val raw = java.lang.Integer.toHexString(value)
    "0" * (8 - raw.length) + raw
