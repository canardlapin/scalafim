package scalafim.surface.view.javafx

import java.nio.{ByteBuffer, ByteOrder, IntBuffer}
import javafx.geometry.Rectangle2D
import javafx.scene.{Camera, DepthTest, Group, ParallelCamera, PerspectiveCamera, SceneAntialiasing, SubScene}
import javafx.scene.image.{PixelBuffer, PixelFormat, WritableImage}
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.scene.shape.{CullFace, MeshView, TriangleMesh, VertexFormat}
import javafx.scene.transform.Affine

import intaglio.*
import scalafim.surface.view.*

enum JavaFxSurfaceError:
  case InvalidTileSize(value: Int)
  case InvalidTextureSize(value: Int)
  case IncompatiblePlan(reason: String)

  def message: String =
    this match
      case InvalidTileSize(value) => s"JavaFX face-atlas tile size must be at least 4; got $value"
      case InvalidTextureSize(value) => s"JavaFX maximum texture size must be at least 64; got $value"
      case IncompatiblePlan(reason) => s"JavaFX surface plan is incompatible with the existing scene: $reason"

enum JavaFxMaterialMode:
  case Lit, Unlit

final case class JavaFxAtlasConfig private (tileSize: Int, maxTextureSize: Int):
  val tilesPerRow: Int = maxTextureSize / tileSize
  val facesPerAtlas: Int = tilesPerRow * tilesPerRow

object JavaFxAtlasConfig:
  def make(tileSize: Int = 4, maxTextureSize: Int = 4096): Either[JavaFxSurfaceError, JavaFxAtlasConfig] =
    if tileSize < 4 then Left(JavaFxSurfaceError.InvalidTileSize(tileSize))
    else if maxTextureSize < 64 || maxTextureSize < tileSize then
      Left(JavaFxSurfaceError.InvalidTextureSize(maxTextureSize))
    else Right(new JavaFxAtlasConfig(tileSize, maxTextureSize))

  val Default: JavaFxAtlasConfig = make().toOption.get

final case class JavaFxSurfaceBuildReceipt(
  chunks: Int,
  verticesUploaded: Int,
  facesUploaded: Int,
  atlasPixels: Long,
  directAtlasBytes: Long,
  meshBuildNanos: Long,
  atlasBuildNanos: Long
)

final case class JavaFxSurfaceUpdateReceipt(
  geometryRebuilt: Boolean,
  atlasesUpdated: Int,
  dirtyPixels: Long,
  updateNanos: Long,
  verticesUpdated: Int = 0,
  bytesUpdated: Long = 0L
)

final class JavaFxFaceAtlas private[javafx] (
  val faceStart: Int,
  val faceCount: Int,
  val width: Int,
  val height: Int,
  val tileSize: Int,
  private val pixels: IntBuffer,
  val pixelBuffer: PixelBuffer[IntBuffer],
  val image: WritableImage
):
  def direct: Boolean = pixels.isDirect

  private[javafx] def write(
    indices: IntBufferView,
    colors: Array[Int]
  ): Rectangle2D =
    var localFace = 0
    while localFace < faceCount do
      val face = faceStart + localFace
      val offset = face * 3
      val a = colors(indices(offset))
      val b = colors(indices(offset + 1))
      val c = colors(indices(offset + 2))
      writeTile(localFace, a, b, c)
      localFace += 1
    pixels.position(0)
    new Rectangle2D(0.0, 0.0, width.toDouble, height.toDouble)

  def commit(region: Rectangle2D): Unit =
    pixelBuffer.updateBuffer(_ => region)

  private def writeTile(localFace: Int, a: Int, b: Int, c: Int): Unit =
    val tilesPerRow = width / tileSize
    val tileX = (localFace % tilesPerRow) * tileSize
    val tileY = (localFace / tilesPerRow) * tileSize
    val denominator = (tileSize - 1).toDouble
    var y = 0
    while y < tileSize do
      var x = 0
      while x < tileSize do
        var weightB = x / denominator
        var weightC = (tileSize - 1 - y) / denominator
        var weightA = 1.0 - weightB - weightC
        if weightA < 0.0 then
          val sum = weightB + weightC
          weightB /= sum
          weightC /= sum
          weightA = 0.0
        pixels.put((tileY + y) * width + tileX + x, interpolateArgbPre(a, b, c, weightA, weightB, weightC))
        x += 1
      y += 1

  private def interpolateArgbPre(
    a: Int,
    b: Int,
    c: Int,
    wa: Double,
    wb: Double,
    wc: Double
  ): Int =
    def channel(shift: Int): Int =
      math.round(((a >>> shift) & 0xff) * wa + ((b >>> shift) & 0xff) * wb + ((c >>> shift) & 0xff) * wc)
        .toInt.max(0).min(255)
    val alpha = channel(0)
    val red = (channel(24) * alpha + 127) / 255
    val green = (channel(16) * alpha + 127) / 255
    val blue = (channel(8) * alpha + 127) / 255
    (alpha << 24) | (red << 16) | (green << 8) | blue

final case class JavaFxSurfaceChunk(
  surface: SurfaceId,
  meshKey: SurfaceResourceKey,
  faceStart: Int,
  faceCount: Int,
  mesh: TriangleMesh,
  atlas: JavaFxFaceAtlas,
  material: PhongMaterial,
  view: MeshView
)

final class JavaFxSurfaceProbeResult private[javafx] (
  val root: Group,
  val chunks: Vector[JavaFxSurfaceChunk],
  val surfaceGroups: Map[SurfaceId, Group],
  val receipt: JavaFxSurfaceBuildReceipt,
  private var activeMaterialMode: JavaFxMaterialMode,
  val config: JavaFxAtlasConfig,
  private val perspectiveCamera: PerspectiveCamera,
  private val parallelCamera: ParallelCamera,
  private val cameraTransform: Affine,
  private val layoutTransforms: Map[SurfaceId, Affine],
  private var plan: SurfaceRenderPlan
):
  private val subScenes = scala.collection.mutable.ArrayBuffer.empty[SubScene]
  private var viewportWidth = 1.0
  private var viewportHeight = 1.0

  def camera: Camera =
    if JavaFxSurfaceProbe.isOrthographic(plan.camera) then parallelCamera
    else perspectiveCamera

  def newSubScene(width: Double, height: Double): SubScene =
    setViewportSize(width.toInt, height.toInt)
    val scene = new SubScene(root, width, height, true, SceneAntialiasing.BALANCED)
    attachCamera(scene)
    scene.setFill(Color.WHITE)
    scene

  def materialMode: JavaFxMaterialMode = activeMaterialMode

  def setMaterialMode(mode: JavaFxMaterialMode): Unit =
    if mode != activeMaterialMode then
      chunks.foreach: chunk =>
        JavaFxSurfaceProbe.configureMaterial(chunk.material, chunk.atlas.image, mode)
      activeMaterialMode = mode

  def setLayout(slots: Vector[SurfaceViewSlot]): Unit =
    plan = plan.copy(slots = slots)
    val visible = slots.iterator.map(_.surface).toSet
    surfaceGroups.foreach:
      case (surface, group) => group.setVisible(visible(surface))
    JavaFxSurfaceProbe.configureLayout(plan, layoutTransforms, viewportWidth, viewportHeight)

  private[javafx] def setViewportSize(width: Int, height: Int): Unit =
    viewportWidth = width.toDouble
    viewportHeight = height.toDouble
    JavaFxSurfaceProbe.configureLayout(plan, layoutTransforms, viewportWidth, viewportHeight)

  private[javafx] def attachCamera(scene: SubScene): Unit =
    scene.setCamera(camera)
    if !subScenes.contains(scene) then subScenes += scene

  def updateColors(next: SurfaceRenderPlan, commit: Boolean = false): Either[JavaFxSurfaceError, JavaFxSurfaceUpdateReceipt] =
    if next.meshes.map(_.resourceKey) != plan.meshes.map(_.resourceKey) then
      Left(JavaFxSurfaceError.IncompatiblePlan("mesh resource keys changed"))
    else
      val started = System.nanoTime()
      val colorsBySurface = JavaFxSurfaceProbe.compositeColors(next)
      var dirtyPixels = 0L
      var index = 0
      while index < chunks.length do
        val chunk = chunks(index)
        val mesh = next.meshes.find(_.surface == chunk.surface).get
        val region = chunk.atlas.write(mesh.indices, colorsBySurface(chunk.surface))
        if commit then chunk.atlas.commit(region)
        dirtyPixels += chunk.atlas.width.toLong * chunk.atlas.height.toLong
        index += 1
      plan = next
      Right(JavaFxSurfaceUpdateReceipt(
        geometryRebuilt = false,
        atlasesUpdated = chunks.length,
        dirtyPixels = dirtyPixels,
        updateNanos = System.nanoTime() - started
      ))

  def updateGeometry(next: SurfaceRenderPlan): Either[JavaFxSurfaceError, JavaFxSurfaceUpdateReceipt] =
    if next.meshes.map(_.resourceKey) != plan.meshes.map(_.resourceKey) then
      Left(JavaFxSurfaceError.IncompatiblePlan("mesh topology resource keys changed"))
    else
      val started = System.nanoTime()
      var verticesUpdated = 0
      var bytesUpdated = 0L
      var index = 0
      while index < chunks.length do
        val chunk = chunks(index)
        val packet = next.meshes.find(_.surface == chunk.surface).get
        val points = packet.positions.unsafeArray
        val normals = packet.normals.unsafeArray
        if chunk.mesh.getPoints.size() != points.length || chunk.mesh.getNormals.size() != normals.length then
          return Left(JavaFxSurfaceError.IncompatiblePlan("morph buffers changed vertex count"))
        chunk.mesh.getPoints.setAll(points, 0, points.length)
        chunk.mesh.getNormals.setAll(normals, 0, normals.length)
        verticesUpdated += points.length / 3
        bytesUpdated += (points.length.toLong + normals.length.toLong) * 4L
        index += 1
      plan = next
      Right(JavaFxSurfaceUpdateReceipt(
        geometryRebuilt = false,
        atlasesUpdated = 0,
        dirtyPixels = 0L,
        updateNanos = System.nanoTime() - started,
        verticesUpdated = verticesUpdated,
        bytesUpdated = bytesUpdated
      ))

  def applyCamera(next: SurfaceRenderPlan): Either[JavaFxSurfaceError, JavaFxSurfaceUpdateReceipt] =
    if next.meshes.map(_.resourceKey) != plan.meshes.map(_.resourceKey) then
      Left(JavaFxSurfaceError.IncompatiblePlan("camera update also changed mesh resources"))
    else
      val started = System.nanoTime()
      plan = next
      JavaFxSurfaceProbe.configureCamera(perspectiveCamera, cameraTransform, next)
      JavaFxSurfaceProbe.configureLayout(plan, layoutTransforms, viewportWidth, viewportHeight)
      subScenes.foreach(_.setCamera(camera))
      Right(JavaFxSurfaceUpdateReceipt(
        geometryRebuilt = false,
        atlasesUpdated = 0,
        dirtyPixels = 0L,
        updateNanos = System.nanoTime() - started
      ))

object JavaFxSurfaceProbe:
  def compile(
    plan: SurfaceRenderPlan,
    materialMode: JavaFxMaterialMode = JavaFxMaterialMode.Unlit,
    config: JavaFxAtlasConfig = JavaFxAtlasConfig.Default
  ): Either[JavaFxSurfaceError, JavaFxSurfaceProbeResult] =
    val meshStarted = System.nanoTime()
    val colorsBySurface = compositeColors(plan)
    val chunks = Vector.newBuilder[JavaFxSurfaceChunk]
    var verticesUploaded = 0
    var facesUploaded = 0
    var atlasPixels = 0L
    var meshNanos = 0L
    var atlasNanos = 0L
    var meshIndex = 0
    while meshIndex < plan.meshes.length do
      val packet = plan.meshes(meshIndex)
      val faceCount = packet.indices.length / 3
      var faceStart = 0
      while faceStart < faceCount do
        val count = math.min(config.facesPerAtlas, faceCount - faceStart)
        val atlasStarted = System.nanoTime()
        val atlas = buildAtlas(faceStart, count, packet.indices, colorsBySurface(packet.surface), config)
        atlasNanos += System.nanoTime() - atlasStarted
        atlasPixels += atlas.width.toLong * atlas.height.toLong
        val chunkStarted = System.nanoTime()
        val triangleMesh = buildMesh(packet, faceStart, count, atlas, config)
        val material = materialFor(atlas.image, materialMode)
        val view = new MeshView(triangleMesh)
        view.setMaterial(material)
        // The v1 render plan has no culling field, so its portable semantics
        // are explicitly two-sided. A native backend must not invent back-face
        // culling for meshes whose winding convention came from external IO.
        view.setCullFace(CullFace.NONE)
        view.setDepthTest(DepthTest.ENABLE)
        chunks += JavaFxSurfaceChunk(packet.surface, packet.resourceKey, faceStart, count, triangleMesh, atlas, material, view)
        meshNanos += System.nanoTime() - chunkStarted
        verticesUploaded += packet.positions.length / 3
        facesUploaded += count
        faceStart += count
      meshIndex += 1
    val built = chunks.result()
    val root = new Group()
    val surfaceGroups = built.groupBy(_.surface).map: (surface, surfaceChunks) =>
      val group = new Group()
      surfaceChunks.foreach(chunk => group.getChildren.add(chunk.view))
      surface -> group
    plan.slots.iterator.map(_.surface).toVector.distinct.foreach: surface =>
      surfaceGroups.get(surface).foreach(group => root.getChildren.add(group))
    root.setDepthTest(DepthTest.ENABLE)
    val perspectiveCamera = new PerspectiveCamera(true)
    val parallelCamera = new ParallelCamera()
    val cameraTransform = new Affine()
    root.getTransforms.add(cameraTransform)
    val layoutTransforms = surfaceGroups.map: (surface, group) =>
      val transform = new Affine()
      group.getTransforms.add(transform)
      surface -> transform
    configureCamera(perspectiveCamera, cameraTransform, plan)
    val receipt = JavaFxSurfaceBuildReceipt(
      built.length,
      verticesUploaded,
      facesUploaded,
      atlasPixels,
      atlasPixels * 4L,
      math.max(meshNanos, System.nanoTime() - meshStarted - atlasNanos),
      atlasNanos
    )
    val result = new JavaFxSurfaceProbeResult(
      root, built, surfaceGroups, receipt, materialMode, config,
      perspectiveCamera, parallelCamera, cameraTransform, layoutTransforms, plan
    )
    result.setLayout(plan.slots)
    Right(result)

  private[javafx] def configureCamera(
    camera: PerspectiveCamera,
    transform: Affine,
    plan: SurfaceRenderPlan
  ): Unit =
    val packet = plan.camera
    val view = packet.viewMatrix
    // SurfaceRenderPlan uses an OpenGL-style right-handed camera looking down
    // -Z with y up. JavaFX looks down +Z with y down, so rows 1 and 2 are
    // negated while the world-space mesh buffer remains unchanged.
    transform.setToTransform(
      view(0), view(1), view(2), view(3),
      -view(4), -view(5), -view(6), -view(7),
      -view(8), -view(9), -view(10), -view(11)
    )
    val projection = packet.projectionMatrix
    if projection(15) == 0.0f && projection(5) > 0.0f then
      val fov = 2.0 * math.atan(1.0 / projection(5)) * 180.0 / math.Pi
      camera.setFieldOfView(fov)
      camera.setVerticalFieldOfView(true)
    camera.setNearClip(0.01)
    camera.setFarClip(1000.0)

  private[javafx] def configureLayout(
    plan: SurfaceRenderPlan,
    transforms: Map[SurfaceId, Affine],
    viewportWidth: Double,
    viewportHeight: Double
  ): Unit =
    plan.slots.foreach: slot =>
      transforms.get(slot.surface).foreach: transform =>
        val layout = viewLayout(plan, slot.viewport, viewportWidth, viewportHeight)
        val world = conjugateViewLayout(plan.camera, layout._1, layout._2)
        val translated = Array(
          world._2(0) + world._1(0) * slot.worldOffsetX + world._1(1) * slot.worldOffsetY + world._1(2) * slot.worldOffsetZ,
          world._2(1) + world._1(3) * slot.worldOffsetX + world._1(4) * slot.worldOffsetY + world._1(5) * slot.worldOffsetZ,
          world._2(2) + world._1(6) * slot.worldOffsetX + world._1(7) * slot.worldOffsetY + world._1(8) * slot.worldOffsetZ
        )
        transform.setToTransform(
          world._1(0), world._1(1), world._1(2), translated(0),
          world._1(3), world._1(4), world._1(5), translated(1),
          world._1(6), world._1(7), world._1(8), translated(2)
        )

  private def viewLayout(
    plan: SurfaceRenderPlan,
    viewport: SurfaceViewport,
    width: Double,
    height: Double
  ): (Array[Double], Array[Double]) =
    val projection = plan.camera.projectionMatrix
    val shiftX = 2.0 * viewport.x + viewport.width - 1.0
    val shiftY = 2.0 * viewport.y + viewport.height - 1.0
    if projection(15) == 0.0f then
      val aspect = width / height
      val p0 = projection(0).toDouble
      val p5 = projection(5).toDouble
      (
        Array(
          viewport.width * aspect * p0 / p5, 0.0, shiftX * aspect / p5,
          0.0, viewport.height, shiftY / p5,
          0.0, 0.0, 1.0
        ),
        Array(0.0, 0.0, 0.0)
      )
    else
      (
        Array(
          width * viewport.width * projection(0) * 0.5, 0.0, 0.0,
          0.0, height * viewport.height * projection(5) * 0.5, 0.0,
          0.0, 0.0, 1.0
        ),
        Array(
          width * (viewport.x + viewport.width * 0.5),
          height * (viewport.y + viewport.height * 0.5),
          0.0
        )
      )

  private[javafx] def isOrthographic(camera: SurfaceCameraPacket): Boolean =
    camera.projectionMatrix(15) == 1.0f

  private def conjugateViewLayout(
    packet: SurfaceCameraPacket,
    layout: Array[Double],
    offset: Array[Double]
  ): (Array[Double], Array[Double]) =
    val view = packet.viewMatrix
    val rotation = Array(
      view(0).toDouble, view(1).toDouble, view(2).toDouble,
      -view(4).toDouble, -view(5).toDouble, -view(6).toDouble,
      -view(8).toDouble, -view(9).toDouble, -view(10).toDouble
    )
    val translation = Array(view(3).toDouble, -view(7).toDouble, -view(11).toDouble)
    val product = new Array[Double](9)
    var row = 0
    while row < 3 do
      var column = 0
      while column < 3 do
        var value = 0.0
        var inner = 0
        while inner < 3 do
          var second = 0
          while second < 3 do
            value += rotation(inner * 3 + row) * layout(inner * 3 + second) * rotation(second * 3 + column)
            second += 1
          inner += 1
        product(row * 3 + column) = value
        column += 1
      row += 1
    val shifted = new Array[Double](3)
    row = 0
    while row < 3 do
      var viewOffset = offset(row) - translation(row)
      var column = 0
      while column < 3 do
        viewOffset += layout(row * 3 + column) * translation(column)
        column += 1
      column = 0
      while column < 3 do
        shifted(column) += rotation(row * 3 + column) * viewOffset
        column += 1
      row += 1
    (product, shifted)

  private[javafx] def compositeColors(plan: SurfaceRenderPlan): Map[SurfaceId, Array[Int]] =
    plan.meshes.map: mesh =>
      val colors = Array.fill(mesh.positions.length / 3)(Rgba32.unsafe(184, 184, 184).toPackedInt)
      var layerIndex = 0
      while layerIndex < plan.layers.length do
        val layer = plan.layers(layerIndex)
        if layer.surface == mesh.surface then
          var vertex = 0
          while vertex < colors.length do
            val under = unpack(colors(vertex))
            val over = unpack(layer.colors(vertex))
            colors(vertex) = layer.blendMode.composite(under, over, layer.opacity).toPackedInt
            vertex += 1
        layerIndex += 1
      mesh.surface -> colors
    .toMap

  private def buildAtlas(
    faceStart: Int,
    faceCount: Int,
    indices: IntBufferView,
    colors: Array[Int],
    config: JavaFxAtlasConfig
  ): JavaFxFaceAtlas =
    val columns = math.min(config.tilesPerRow, faceCount)
    val rows = (faceCount + columns - 1) / columns
    val width = columns * config.tileSize
    val height = rows * config.tileSize
    val byteBuffer = ByteBuffer.allocateDirect(width * height * 4).order(ByteOrder.nativeOrder())
    val pixels = byteBuffer.asIntBuffer()
    val pixelBuffer = new PixelBuffer[IntBuffer](width, height, pixels, PixelFormat.getIntArgbPreInstance())
    val image = new WritableImage(pixelBuffer)
    val atlas = new JavaFxFaceAtlas(faceStart, faceCount, width, height, config.tileSize, pixels, pixelBuffer, image)
    atlas.write(indices, colors)
    atlas

  private def buildMesh(
    packet: SurfaceMeshPacket,
    faceStart: Int,
    faceCount: Int,
    atlas: JavaFxFaceAtlas,
    config: JavaFxAtlasConfig
  ): TriangleMesh =
    val mesh = new TriangleMesh(VertexFormat.POINT_NORMAL_TEXCOORD)
    val points = packet.positions.unsafeArray
    val normals = packet.normals.unsafeArray
    mesh.getPoints.setAll(points, 0, points.length)
    mesh.getNormals.setAll(normals, 0, normals.length)
    val texCoords = new Array[Float](faceCount * 6)
    val faces = new Array[Int](faceCount * 9)
    val tilesPerRow = atlas.width / config.tileSize
    var localFace = 0
    while localFace < faceCount do
      val tileX = (localFace % tilesPerRow) * config.tileSize
      val tileY = (localFace / tilesPerRow) * config.tileSize
      val left = (tileX + 0.5f) / atlas.width
      val right = (tileX + config.tileSize - 0.5f) / atlas.width
      val top = (tileY + 0.5f) / atlas.height
      val bottom = (tileY + config.tileSize - 0.5f) / atlas.height
      val textureOffset = localFace * 6
      texCoords(textureOffset) = left
      texCoords(textureOffset + 1) = bottom
      texCoords(textureOffset + 2) = right
      texCoords(textureOffset + 3) = bottom
      texCoords(textureOffset + 4) = left
      texCoords(textureOffset + 5) = top
      val sourceOffset = (faceStart + localFace) * 3
      val faceOffset = localFace * 9
      var corner = 0
      while corner < 3 do
        val vertex = packet.indices(sourceOffset + corner)
        faces(faceOffset + corner * 3) = vertex
        faces(faceOffset + corner * 3 + 1) = vertex
        faces(faceOffset + corner * 3 + 2) = localFace * 3 + corner
        corner += 1
      localFace += 1
    mesh.getTexCoords.setAll(texCoords, 0, texCoords.length)
    mesh.getFaces.setAll(faces, 0, faces.length)
    mesh

  private def materialFor(image: WritableImage, mode: JavaFxMaterialMode): PhongMaterial =
    val material = new PhongMaterial(Color.WHITE)
    material.setSpecularColor(Color.TRANSPARENT)
    configureMaterial(material, image, mode)
    material

  private[javafx] def configureMaterial(
    material: PhongMaterial,
    image: WritableImage,
    mode: JavaFxMaterialMode
  ): Unit =
    mode match
      case JavaFxMaterialMode.Lit =>
        material.setDiffuseColor(Color.WHITE)
        material.setDiffuseMap(image)
        material.setSelfIlluminationMap(null)
      case JavaFxMaterialMode.Unlit =>
        material.setDiffuseColor(Color.BLACK)
        material.setDiffuseMap(null)
        material.setSelfIlluminationMap(image)

  private def unpack(value: Int): Rgba32 =
    Rgba32.fromPackedInt(value)
