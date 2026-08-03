package scalafim.surface.view

import intaglio.*

/** Pinned, allocation-explicit fixture shared by every backend benchmark.
  * The mesh and layer keys deliberately encode only the resource changes that
  * an incremental renderer is expected to observe.
  */
object SurfaceBenchmarkFixture:
  def plan(
    vertices: Int,
    layers: Int,
    dataRevision: Int = 0,
    styleRevision: Int = 0,
    cameraRevision: Int = 0,
    timepoint: Int = 0
  ): SurfaceRenderPlan =
    require(SurfaceBenchmarkMatrix.VertexCounts.contains(vertices), s"unsupported benchmark vertex count $vertices")
    require(SurfaceBenchmarkMatrix.LayerCounts.contains(layers), s"unsupported benchmark layer count $layers")
    require(dataRevision >= 0, s"negative data revision $dataRevision")
    require(styleRevision >= 0, s"negative style revision $styleRevision")
    require(cameraRevision >= 0, s"negative camera revision $cameraRevision")
    require(timepoint >= 0, s"negative timepoint $timepoint")

    val columns = if vertices == 32768 then 256 else 512
    val rows = (vertices + columns - 1) / columns
    val positions = new Array[Float](vertices * 3)
    val normals = new Array[Float](vertices * 3)
    var vertex = 0
    while vertex < vertices do
      val row = vertex / columns
      val column = vertex % columns
      val longitude = 2.0 * math.Pi * column.toDouble / math.max(1, columns - 1).toDouble
      val latitude = -math.Pi * 0.5 + math.Pi * row.toDouble / math.max(1, rows - 1).toDouble
      val radius = math.cos(latitude)
      val offset = vertex * 3
      positions(offset) = (0.75 * radius * math.cos(longitude)).toFloat
      positions(offset + 1) = (radius * math.sin(longitude)).toFloat
      positions(offset + 2) = (0.82 * math.sin(latitude)).toFloat
      val length = math.sqrt(
        positions(offset).toDouble * positions(offset).toDouble +
          positions(offset + 1).toDouble * positions(offset + 1).toDouble +
          positions(offset + 2).toDouble * positions(offset + 2).toDouble
      ).max(1e-12)
      normals(offset) = (positions(offset) / length).toFloat
      normals(offset + 1) = (positions(offset + 1) / length).toFloat
      normals(offset + 2) = (positions(offset + 2) / length).toFloat
      vertex += 1

    var cells = 0
    var row = 0
    while row + 1 < rows do
      var column = 0
      while column + 1 < columns do
        if (row + 1) * columns + column + 1 < vertices then cells += 1
        column += 1
      row += 1
    val indices = new Array[Int](cells * 6)
    var offset = 0
    row = 0
    while row + 1 < rows do
      var column = 0
      while column + 1 < columns do
        val a = row * columns + column
        val b = a + 1
        val c = (row + 1) * columns + column
        val d = c + 1
        if d < vertices then
          indices(offset) = a
          indices(offset + 1) = b
          indices(offset + 2) = c
          indices(offset + 3) = b
          indices(offset + 4) = d
          indices(offset + 5) = c
          offset += 6
        column += 1
      row += 1

    val surface = SurfaceId.unsafe(s"benchmark-$vertices")
    val meshKey = SurfaceResourceKey(s"benchmark-mesh-$vertices-v1")
    val mesh = SurfaceMeshPacket(
      surface,
      meshKey,
      new FloatBufferView(positions),
      new FloatBufferView(normals),
      new IntBufferView(indices)
    )
    val opacity = DisplayOpacity.unsafe(if styleRevision == 0 then 1.0 else 0.625)
    val layerPackets = Vector.tabulate(layers): layerIndex =>
      val colors = new Array[Int](vertices)
      var index = 0
      while index < vertices do
        val red = (index + dataRevision * 17 + timepoint * 29 + layerIndex * 31) & 0xff
        val green = (index / columns * 3 + layerIndex * 19) & 0xff
        val blue = (255 - red + layerIndex * 7) & 0xff
        colors(index) = (red << 24) | (green << 16) | (blue << 8) | (if layerIndex == 0 then 255 else 96)
        index += 1
      SurfaceLayerPacket(
        SurfaceLayerId.unsafe(s"layer-$layerIndex"),
        surface,
        SurfaceResourceKey(s"benchmark-layer-$vertices-$layerIndex-d$dataRevision-t$timepoint"),
        new IntBufferView(colors),
        opacity,
        DisplayBlendMode.Normal
      )
    val viewMatrix = new FloatBufferView(Array(
      1f, 0f, 0f, (-cameraRevision.toDouble * 0.01).toFloat,
      0f, 1f, 0f, 0f,
      0f, 0f, 1f, -4f,
      0f, 0f, 0f, 1f
    ))
    val fieldOfView = 35.0
    val near = 0.01
    val far = 100.0
    val focal = 1.0 / math.tan(fieldOfView * math.Pi / 360.0)
    val projectionMatrix = new FloatBufferView(Array(
      focal.toFloat, 0f, 0f, 0f,
      0f, focal.toFloat, 0f, 0f,
      0f, 0f, ((far + near) / (near - far)).toFloat, ((2.0 * far * near) / (near - far)).toFloat,
      0f, 0f, -1f, 0f
    ))
    val slot = SurfaceViewSlot(surface, SurfaceViewport(0.0, 0.0, 1.0, 1.0))
    val passes = layerPackets.map(layer => SurfaceDrawPass(0, meshKey, layer.resourceKey, layer.blendMode))
    SurfaceRenderPlan(
      Vector(slot),
      Vector(mesh),
      layerPackets,
      SurfaceCameraPacket(viewMatrix, projectionMatrix, 0.0, 0.0, -1.0),
      SurfaceLighting.Unlit,
      SurfaceClipping.Disabled,
      passes,
      Scene.empty,
      Vector.empty,
      SurfaceProfile(
        meshesPacked = 1,
        verticesPacked = vertices,
        facesPacked = indices.length / 3,
        layersColored = layers,
        colorValuesWritten = vertices * layers,
        primitiveBytes = (positions.length.toLong + normals.length.toLong + indices.length.toLong + vertices.toLong * layers) * 4L
      ),
      SurfaceRenderReceipt(
        Vector(meshKey),
        layerPackets.map(_.resourceKey),
        s"benchmark-camera-$cameraRevision",
        passes.length,
        timepoint
      )
    )

  def planFor(benchmark: SurfaceBenchmarkCase): SurfaceRenderPlan =
    benchmark.path match
      case SurfaceAdmissionPath.CameraOnly => plan(benchmark.vertices, benchmark.layers, cameraRevision = 1)
      case SurfaceAdmissionPath.StyleUpdate => plan(benchmark.vertices, benchmark.layers, styleRevision = 1)
      case SurfaceAdmissionPath.LayerDataUpdate => plan(benchmark.vertices, benchmark.layers, dataRevision = 1)
      case SurfaceAdmissionPath.TimepointUpdate => plan(benchmark.vertices, benchmark.layers, timepoint = 1)
      case _ => plan(benchmark.vertices, benchmark.layers)
