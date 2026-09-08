package scalafim.surface.view.javafx

import intaglio.*
import scalafim.surface.view.*

/** Explicit opt-in. The byte limit covers generated primitive buffers, provenance,
  * and base-level face textures; JVM object and driver overhead are not included.
  */
final case class JavaFxApproximationConfig private (
  quality: SurfaceFragmentApproximationConfig,
  maxGeneratedBytes: Long
)
object JavaFxApproximationConfig:
  def make(maxChannelError: Int = 1, maxTriangles: Int = 1000000,
      maxGeneratedBytes: Long = 256L * 1024 * 1024, maxDepth: Int = 24,
      maxCutsPerFace: Int = 64): Either[JavaFxSurfaceError, JavaFxApproximationConfig] =
    if maxGeneratedBytes <= 0 then Left(JavaFxSurfaceError.IncompatiblePlan("approximation byte budget must be positive"))
    else if maxTriangles > Int.MaxValue / 9 then Left(JavaFxSurfaceError.IncompatiblePlan("approximation triangle limit exceeds native array indexing"))
    else SurfaceFragmentApproximationConfig.make(maxChannelError, maxTriangles, maxDepth, maxCutsPerFace)
      .left.map(e => JavaFxSurfaceError.IncompatiblePlan(e.message)).map(new JavaFxApproximationConfig(_, maxGeneratedBytes))

final case class JavaFxApproximationReceipt(
  sourceFaces: Int,
  partitionFaces: Int,
  derivedFaces: Int,
  maximumChannelError: Int,
  primitiveBytes: Long,
  textureBytes: Long,
  preparationNanos: Long,
  reused: Boolean = false
):
  def generatedBytes: Long = primitiveBytes + textureBytes

private[javafx] final case class JavaFxPreparedSurface(plan: SurfaceRenderPlan, approximation: Option[JavaFxApproximationReceipt])

private[javafx] object JavaFxSurfaceApproximation:
  private val PrimitiveBytesPerFace = 328L // 196 portable bytes plus 132 native mesh-buffer bytes per face

  def prepare(plan: SurfaceRenderPlan, config: JavaFxApproximationConfig, atlas: JavaFxAtlasConfig)
      : Either[JavaFxSurfaceError, JavaFxPreparedSurface] =
    val started = System.nanoTime()
    if atlas.maxTextureSize > 16384 || atlas.facesPerAtlas <= 0 then
      return Left(JavaFxSurfaceError.IncompatiblePlan("bounded fragment rendering requires a safe texture limit no larger than 16384"))
    val meshes = Vector.newBuilder[SurfaceMeshPacket]
    val layers = Vector.newBuilder[SurfaceLayerPacket]
    var sourceFaces = 0
    var partitionFaces = 0
    var derivedFaces = 0
    var maximumError = 0
    var primitiveBytes = 0L
    var textureBytes = 0L
    val legacyMeshes = plan.meshes.filterNot(mesh => plan.fragmentSurfaces(mesh.surface))
    val legacy = JavaFxSurfaceProbe.compositeColors(plan.copy(meshes = legacyMeshes), JavaFxSurfaceProgram.materialMode(plan))
    val legacyVarying = legacyMeshes.filter: mesh =>
      val colors = legacy(mesh.surface)
      (0 until mesh.indices.length / 3).exists: face =>
        val offset = face * 3
        colors(mesh.indices(offset)) != colors(mesh.indices(offset + 1)) ||
          colors(mesh.indices(offset)) != colors(mesh.indices(offset + 2))
    .map(_.surface).toSet
    if plan.fragmentSurfaces.isEmpty && legacyVarying.isEmpty then return Right(JavaFxPreparedSurface(plan, None))
    var index = 0
    while index < plan.meshes.length do
      val mesh = plan.meshes(index)
      val inputLayers = plan.layers.filter(_.surface == mesh.surface)
      val styleKey = inputLayers.map(l => s"${l.resourceKey.value}:${l.opacity.toDouble}:${l.blendMode}:${l.coverage}").mkString("|")
      val normalDigest = java.security.MessageDigest.getInstance("SHA-256")
      val sourceNormals = mesh.sampleNormals.getOrElse(mesh.normals)
      var n = 0
      while n < sourceNormals.length do
        val bits = java.lang.Float.floatToIntBits(sourceNormals(n))
        normalDigest.update((bits >>> 24).toByte)
        normalDigest.update((bits >>> 16).toByte)
        normalDigest.update((bits >>> 8).toByte)
        normalDigest.update(bits.toByte)
        n += 1
      val normalKey = normalDigest.digest().map(b => f"${b & 255}%02x").mkString
      val recipe = s"${mesh.geometryKey.value}|$normalKey|$styleKey|${plan.lighting}|${config.quality.maxChannelError}"
      val layerKey = SurfaceResourceKey(s"javafx-composed:$recipe")
      val layerId = SurfaceLayerId.unsafe(s"javafx-composed:${mesh.surface.value}")
      if !plan.fragmentSurfaces(mesh.surface) && !legacyVarying(mesh.surface) then
        val colors = legacy(mesh.surface)
        meshes += mesh
        layers += SurfaceLayerPacket(layerId, mesh.surface, layerKey, new IntBufferView(colors), DisplayOpacity.Opaque, DisplayBlendMode.Normal)
        primitiveBytes += colors.length.toLong * 4
        textureBytes += atlasBytes(mesh.indices.length / 3, atlas)
      else
        val remaining = config.maxGeneratedBytes - primitiveBytes - textureBytes
        val tileBytes = atlas.tileSize.toLong * atlas.tileSize * 4
        val paddingAllowance = ((config.quality.maxTriangles.toLong / atlas.facesPerAtlas) + 1) * atlas.tilesPerRow * tileBytes
        val byBytes = math.max(0L, (remaining - paddingAllowance) / (PrimitiveBytesPerFace + tileBytes))
        val limit = math.min(config.quality.maxTriangles - derivedFaces, math.min(Int.MaxValue.toLong, byBytes).toInt)
        if limit <= 0 then return Left(JavaFxSurfaceError.IncompatiblePlan("approximation exceeds its generated byte or triangle budget"))
        val legacyCorners = legacyVarying(mesh.surface)
        // Rounding shaded corners to RGBA8 can change the eventual interpolated
        // channel by one. Reserve that error inside the requested budget.
        val cornerRounding = if legacyCorners && plan.lighting != SurfaceLighting.Unlit then 1 else 0
        val residualError = config.quality.maxChannelError - cornerRounding
        if residualError < 1 then return Left(JavaFxSurfaceError.IncompatiblePlan(
          "bounded legacy corner lighting requires a channel-error budget of at least 2"))
        val quality = SurfaceFragmentApproximationConfig.make(residualError, limit,
          config.quality.maxDepth, config.quality.maxCutsPerFace).toOption.get
        // Legacy composition and lighting happen at render corners. Approximate
        // that already composed color field, not normalized fragment lighting.
        // This temporary domain is lifted back to scientific IDs below.
        val (approximationMesh, approximationLayers, approximationLighting) = if legacyCorners then
          val colors = new IntBufferView(legacy(mesh.surface))
          (mesh.copy(sourceVertices = None, nearestPartition = None, constantPartition = None,
            samplePositions = None, sampleNormals = None),
            Vector(SurfaceLayerPacket(layerId, mesh.surface, layerKey, colors, DisplayOpacity.Opaque,
              DisplayBlendMode.Normal, sampleColors = Some(colors))), SurfaceLighting.Unlit)
        else (mesh, inputLayers, plan.lighting)
        val edgeAliases = if !legacyCorners then None else
          val ids = scala.collection.mutable.Map.empty[(Float, Float, Float), Int]
          Some(new IntBufferView(Array.tabulate(mesh.positions.length / 3): vertex =>
            val p = (mesh.positions(vertex * 3), mesh.positions(vertex * 3 + 1), mesh.positions(vertex * 3 + 2))
            ids.getOrElseUpdate(p, ids.size)))
        val approximation = SurfaceFragmentApproximation.build(approximationMesh, approximationLayers, approximationLighting, quality, edgeAliases) match
          case Left(error) => return Left(JavaFxSurfaceError.IncompatiblePlan(error.message))
          case Right(value) => value
        val count = approximation.cells.length
        val triangles = approximation.cells.map: cell =>
          val t = cell.triangle
          if !legacyCorners then t else
            def lift(w: SurfaceFaceWeights): SurfaceFaceWeights =
              val (a, b, c) = mesh.sourceBarycentric(t.sourceFace, w.a, w.b, w.c)
              SurfaceFaceWeights(a, b, c)
            SurfaceMappingTriangle(mesh.sourceFace(t.sourceFace), mesh.sourceFaceVertices(t.sourceFace), lift(t.a), lift(t.b), lift(t.c))
        val positions = new Array[Float](Math.multiplyExact(count, 9))
        val normals = new Array[Float](positions.length)
        val indices = Array.tabulate(Math.multiplyExact(count, 3))(identity)
        val colors = new Array[Int](indices.length)
        val owners = new Array[Int](indices.length)
        val original = if legacyCorners then mesh.positions else mesh.samplePositions.getOrElse(mesh.positions)
        var face = 0
        while face < count do
          val cell = approximation.cells(face)
          val t = cell.triangle
          val (a, b, c) = t.sourceVertices
          val corners = Vector(t.a, t.b, t.c)
          var corner = 0
          while corner < 3 do
            val w = corners(corner)
            val vertex = face * 3 + corner
            var axis = 0
            while axis < 3 do
              positions(vertex * 3 + axis) = (w.a * original(a * 3 + axis) + w.b * original(b * 3 + axis) + w.c * original(c * 3 + axis)).toFloat
              axis += 1
            colors(vertex) = cell.color.toPackedInt
            val provenance = triangles(face)
            val ownerWeights = if corner == 0 then provenance.a else if corner == 1 then provenance.b else provenance.c
            val (oa, ob, oc) = provenance.sourceVertices
            owners(vertex) = SurfaceNearestPartition.nearestVertex(oa, ob, oc, ownerWeights.a, ownerWeights.b, ownerWeights.c)
            corner += 1
          // Lighting is already baked into the cell color, but Prism still
          // constructs a tangent frame from geometry, UVs and these normals.
          // A dummy Z normal can be parallel to a triangle edge, yielding NaNs
          // and black pixels even with an emission-only material. Use the
          // actual display triangle normal; zero-area cells have no coverage.
          val offset = face * 9
          val ux = positions(offset + 3).toDouble - positions(offset)
          val uy = positions(offset + 4).toDouble - positions(offset + 1)
          val uz = positions(offset + 5).toDouble - positions(offset + 2)
          val vx = positions(offset + 6).toDouble - positions(offset)
          val vy = positions(offset + 7).toDouble - positions(offset + 1)
          val vz = positions(offset + 8).toDouble - positions(offset + 2)
          val nx = uy * vz - uz * vy
          val ny = uz * vx - ux * vz
          val nz = ux * vy - uy * vx
          val length = math.sqrt(nx * nx + ny * ny + nz * nz)
          corner = 0
          while corner < 3 do
            normals(offset + corner * 3) = (if length > 0 then nx / length else 0).toFloat
            normals(offset + corner * 3 + 1) = (if length > 0 then ny / length else 0).toFloat
            normals(offset + corner * 3 + 2) = (if length > 0 then nz / length else 1).toFloat
            corner += 1
          face += 1
        val key = SurfaceResourceKey(s"${mesh.resourceKey.value}:javafx-constant-v2:$recipe")
        meshes += SurfaceMeshPacket(mesh.surface, key, new FloatBufferView(positions), new FloatBufferView(normals),
          new IntBufferView(indices), sourceVertices = Some(new IntBufferView(owners)), constantPartition = Some(triangles))
        layers += SurfaceLayerPacket(layerId, mesh.surface, layerKey, new IntBufferView(colors), DisplayOpacity.Opaque,
          DisplayBlendMode.Normal, SurfaceSampleAssociation.Face, SurfaceMapInterpolation.FaceConstant)
        sourceFaces += triangles.iterator.map(_.sourceFace).toSet.size
        partitionFaces += approximation.initialTriangles
        derivedFaces += count
        maximumError = math.max(maximumError, approximation.maximumChannelError + cornerRounding)
        primitiveBytes += count.toLong * PrimitiveBytesPerFace
        textureBytes += atlasBytes(count, atlas)
      if primitiveBytes + textureBytes > config.maxGeneratedBytes then
        return Left(JavaFxSurfaceError.IncompatiblePlan("approximation exceeds its generated byte budget"))
      index += 1
    val outputMeshes = meshes.result()
    val outputLayers = layers.result()
    val passes = plan.slots.zipWithIndex.map: (slot, index) =>
      SurfaceDrawPass(index, outputMeshes.find(_.surface == slot.surface).get.resourceKey,
        outputLayers.find(_.surface == slot.surface).get.resourceKey, DisplayBlendMode.Normal)
    val planBytes = outputMeshes.iterator.map: mesh =>
      (mesh.positions.length.toLong + mesh.normals.length + mesh.indices.length) * 4 +
        mesh.sourceVertices.fold(0L)(_.length.toLong * 4) + mesh.constantPartition.fold(0L)(_.length.toLong * 88)
    .sum + outputLayers.iterator.map(_.colors.length.toLong * 4).sum + 32L * 4
    val profile = SurfaceProfile(outputMeshes.length, outputMeshes.map(_.positions.length / 3).sum,
      outputMeshes.map(_.indices.length / 3).sum, outputLayers.length, outputLayers.map(_.colors.length).sum, planBytes)
    val receipt = plan.receipt.copy(meshKeys = outputMeshes.map(_.resourceKey), layerKeys = outputLayers.map(_.resourceKey), drawPassCount = passes.length)
    val display = plan.copy(meshes = outputMeshes, layers = outputLayers, lighting = SurfaceLighting.Unlit,
      drawPasses = passes, profile = profile, receipt = receipt, fragmentSurfaces = Set.empty)
    Right(JavaFxPreparedSurface(display, Some(JavaFxApproximationReceipt(sourceFaces, partitionFaces, derivedFaces,
      maximumError, primitiveBytes, textureBytes, System.nanoTime() - started))))

  private def atlasBytes(faces: Int, config: JavaFxAtlasConfig): Long =
    var remaining = faces
    var bytes = 0L
    while remaining > 0 do
      val count = math.min(remaining, config.facesPerAtlas)
      val columns = math.min(config.tilesPerRow, count)
      val rows = (count + columns - 1) / columns
      bytes += columns.toLong * rows * config.tileSize * config.tileSize * 4
      remaining -= count
    bytes
