package scalafim.surface.view.raster

import intaglio.*
import scalafim.surface.view.*

enum SurfaceRasterError:
  case PixelOutsideBounds(x: Int, y: Int, width: Int, height: Int)
  case InvalidPlan(reason: String)

  def message: String =
    this match
      case PixelOutsideBounds(x, y, width, height) => s"pixel ($x, $y) is outside ${width}x$height"
      case InvalidPlan(reason) => s"invalid surface render plan: $reason"

enum TriangleCulling:
  case None, Back, Front

final case class SurfaceRasterStyle(
  background: Rgba32 = Rgba32.unsafe(255, 255, 255),
  surfaceBase: Rgba32 = Rgba32.unsafe(184, 184, 184),
  culling: TriangleCulling = TriangleCulling.Back
)

final case class SurfacePick(
  surface: SurfaceId,
  face: Int,
  vertex: Int,
  barycentricA: Double,
  barycentricB: Double,
  barycentricC: Double,
  depth: Double
)

final case class SurfaceRasterReceipt(
  trianglesInput: Int,
  trianglesAfterClipping: Int,
  trianglesClippedAway: Int,
  trianglesCulled: Int,
  shadedPixels: Int,
  depthRejectedPixels: Int,
  colorValuesComposited: Int,
  setupNanos: Long,
  renderNanos: Long,
  scalarFragmentsEvaluated: Int = 0
)

final case class SurfaceRasterObservedResult(
  result: SurfaceRasterResult,
  observation: SurfaceBackendObservation
)

final class SurfaceRasterResult private[raster] (
  val image: RasterImage,
  val receipt: SurfaceRasterReceipt,
  surfaces: Vector[SurfaceId],
  faceAt: Array[Int],
  slotAt: Array[Int],
  vertexAt: Array[Int],
  baryA: Array[Float],
  baryB: Array[Float],
  baryC: Array[Float],
  depthAt: Array[Double]
):
  def pick(x: Int, y: Int): Either[SurfaceRasterError, Option[SurfacePick]] =
    if x < 0 || x >= image.width || y < 0 || y >= image.height then
      Left(SurfaceRasterError.PixelOutsideBounds(x, y, image.width, image.height))
    else
      val index = y * image.width + x
      val slot = slotAt(index)
      if slot < 0 then Right(None)
      else
        Right(Some(SurfacePick(
          surfaces(slot),
          faceAt(index),
          vertexAt(index),
          baryA(index).toDouble,
          baryB(index).toDouble,
          baryC(index).toDouble,
          depthAt(index)
        )))

object SurfaceRasterizer:
  val capabilities: SurfaceBackendCapabilities = SurfaceBackendCapabilities(
    SurfaceBackendId.unsafe("reference-raster"),
    SurfacePlanRevision.Current,
    Set(
      SurfaceBackendFeature.DeterministicPixels,
      SurfaceBackendFeature.FacewiseData,
      SurfaceBackendFeature.NearestVertexSampling,
      SurfaceBackendFeature.ScalarInterpolation,
      SurfaceBackendFeature.FragmentComposition,
      SurfaceBackendFeature.Lighting,
      SurfaceBackendFeature.DepthBuffer,
      SurfaceBackendFeature.BackFaceCulling,
      SurfaceBackendFeature.WorldClipping,
      SurfaceBackendFeature.BilateralViewports,
      SurfaceBackendFeature.NativePicking,
      SurfaceBackendFeature.HighResolutionSnapshot
    ),
    Vector(
      "fragment surfaces evaluate layer composition and normalized world-space normals per pixel; legacy paths retain vertex lighting"
    )
  )

  private final case class ClipVertex(
    worldX: Double,
    worldY: Double,
    worldZ: Double,
    x: Double,
    y: Double,
    z: Double,
    w: Double,
    red: Double,
    green: Double,
    blue: Double,
    alpha: Double,
    baryA: Double,
    baryB: Double,
    baryC: Double
  ):
    def interpolate(that: ClipVertex, fraction: Double): ClipVertex =
      def mix(a: Double, b: Double): Double = a + (b - a) * fraction
      ClipVertex(
        mix(worldX, that.worldX), mix(worldY, that.worldY), mix(worldZ, that.worldZ),
        mix(x, that.x), mix(y, that.y), mix(z, that.z), mix(w, that.w),
        mix(red, that.red), mix(green, that.green), mix(blue, that.blue), mix(alpha, that.alpha),
        mix(baryA, that.baryA), mix(baryB, that.baryB), mix(baryC, that.baryC)
      )

  private final case class ScreenVertex(
    x: Double,
    y: Double,
    depth: Double,
    inverseW: Double,
    redOverW: Double,
    greenOverW: Double,
    blueOverW: Double,
    alphaOverW: Double,
    baryAOverW: Double,
    baryBOverW: Double,
    baryCOverW: Double
  )

  def render(
    plan: SurfaceRenderPlan,
    dimensions: RasterDimensions,
    style: SurfaceRasterStyle = SurfaceRasterStyle()
  ): Either[SurfaceRasterError, SurfaceRasterResult] =
    validatePlan(plan).map(_ => renderUnsafe(plan, dimensions, style))

  def renderObserved(
    plan: SurfaceRenderPlan,
    dimensions: RasterDimensions,
    style: SurfaceRasterStyle = SurfaceRasterStyle(),
    path: SurfaceAdmissionPath = SurfaceAdmissionPath.Snapshot
  ): Either[SurfaceRasterError, SurfaceRasterObservedResult] =
    render(plan, dimensions, style).map: result =>
      val receipt = result.receipt
      val observation = SurfaceBackendObservation(
        capabilities,
        SurfacePlanRevision.Current,
        path,
        Vector(
          SurfaceResourceEvent.DrawSubmitted(plan.receipt.drawPassCount, receipt.trianglesAfterClipping),
          SurfaceResourceEvent.Resized(dimensions.width, dimensions.height)
        ),
        Vector(
          SurfacePhaseTiming.unsafe(SurfaceRenderPhase.Compile, receipt.setupNanos),
          SurfacePhaseTiming.unsafe(SurfaceRenderPhase.Snapshot, receipt.renderNanos)
        )
      )
      SurfaceRasterObservedResult(result, observation)

  private def validatePlan(plan: SurfaceRenderPlan): Either[SurfaceRasterError, Unit] =
    if plan.layers.exists(layer => (layer.interpolation == SurfaceMapInterpolation.VertexScalar) != layer.scalarField.nonEmpty) then
      Left(SurfaceRasterError.InvalidPlan("scalar policy and payload disagree"))
    else if plan.layers.exists(layer => layer.scalarField.nonEmpty && !plan.fragmentSurfaces(layer.surface)) then
      Left(SurfaceRasterError.InvalidPlan("scalar interpolation requires fragment composition"))
    else if plan.layers.exists(layer => plan.fragmentSurfaces(layer.surface) &&
        !plan.meshes.exists(mesh => mesh.surface == layer.surface &&
          layer.sampleColors.exists(_.length == (if layer.association == SurfaceSampleAssociation.Face then
            mesh.nearestPartition.fold(mesh.indices.length / 3)(_.originalFaceCount)
          else mesh.sampleNormals.getOrElse(mesh.normals).length / 3)) &&
          layer.scalarField.forall(field => layer.association == SurfaceSampleAssociation.Vertex &&
            layer.scalarMapping.exists(_.canonicalKey == field.mapping.canonicalKey) &&
            field.samples.length == mesh.sampleNormals.getOrElse(mesh.normals).length / 3))) then
      Left(SurfaceRasterError.InvalidPlan("fragment sample buffers must match the original vertex or face domain"))
    else if plan.slots.length != plan.meshes.length then
      Left(SurfaceRasterError.InvalidPlan("slot and mesh counts differ"))
    else if plan.camera.viewMatrix.length != 16 || plan.camera.projectionMatrix.length != 16 then
      Left(SurfaceRasterError.InvalidPlan("camera matrices must be 4x4"))
    else Right(())

  private def renderUnsafe(
    plan: SurfaceRenderPlan,
    dimensions: RasterDimensions,
    style: SurfaceRasterStyle
  ): SurfaceRasterResult =
    val setupStarted = System.nanoTime()
    val pixelCount = dimensions.pixelCount
    val pixels = Array.fill(pixelCount)(style.background.toPackedInt)
    val depths = Array.fill(pixelCount)(Double.PositiveInfinity)
    val faceAt = Array.fill(pixelCount)(-1)
    val slotAt = Array.fill(pixelCount)(-1)
    val vertexAt = Array.fill(pixelCount)(-1)
    val baryA = new Array[Float](pixelCount)
    val baryB = new Array[Float](pixelCount)
    val baryC = new Array[Float](pixelCount)
    val composedColors = plan.meshes.map: mesh =>
      val colors = Array.fill(mesh.positions.length / 3)(style.surfaceBase.toPackedInt)
      var layerIndex = 0
      while layerIndex < plan.layers.length do
        val layer = plan.layers(layerIndex)
        if layer.surface == mesh.surface && !plan.fragmentSurfaces(mesh.surface) then
          var vertex = 0
          while vertex < colors.length do
            val under = packed(colors(vertex))
            val over = packed(layer.colors(vertex))
            colors(vertex) = layer.blendMode.composite(under, over, layer.opacity).toPackedInt
            vertex += 1
        layerIndex += 1
      colors
    val setupNanos = System.nanoTime() - setupStarted
    val renderStarted = System.nanoTime()
    var trianglesInput = 0
    var trianglesAfterClipping = 0
    var trianglesClippedAway = 0
    var trianglesCulled = 0
    var shadedPixels = 0
    var depthRejected = 0
    var scalarFragments = 0

    val fittedSlots = plan.viewportFit.resolve(plan.slots, dimensions.width.toDouble, dimensions.height.toDouble)
    var slot = 0
    while slot < plan.meshes.length do
      val mesh = plan.meshes(slot)
      val viewport = fittedSlots(slot).viewport
      val colors = composedColors(slot)
      val fragment = Option.when(plan.fragmentSurfaces(mesh.surface))(
        new SurfaceFragmentEvaluator(mesh, plan.layers.filter(_.surface == mesh.surface), plan.lighting, style.surfaceBase))
      var faceOffset = 0
      while faceOffset < mesh.indices.length do
        trianglesInput += 1
        val face = faceOffset / 3
        val ia = mesh.indices(faceOffset)
        val ib = mesh.indices(faceOffset + 1)
        val ic = mesh.indices(faceOffset + 2)
        val ba = mesh.sourceBarycentric(face, 1.0, 0.0, 0.0)
        val bb = mesh.sourceBarycentric(face, 0.0, 1.0, 0.0)
        val bc = mesh.sourceBarycentric(face, 0.0, 0.0, 1.0)
        val (sourceA, sourceB, sourceC) = mesh.sourceFaceVertices(face)
        val triangle = Vector(
          clipVertex(plan, plan.slots(slot), mesh, colors, ia, ba._1, ba._2, ba._3),
          clipVertex(plan, plan.slots(slot), mesh, colors, ib, bb._1, bb._2, bb._3),
          clipVertex(plan, plan.slots(slot), mesh, colors, ic, bc._1, bc._2, bc._3)
        )
        val clipped = clipTriangle(triangle, plan.clipping)
        if clipped.length < 3 then trianglesClippedAway += 1
        else
          var fan = 1
          while fan + 1 < clipped.length do
            trianglesAfterClipping += 1
            val a = toScreen(clipped(0), viewport, dimensions)
            val b = toScreen(clipped(fan), viewport, dimensions)
            val c = toScreen(clipped(fan + 1), viewport, dimensions)
            val area = edge(a.x, a.y, b.x, b.y, c.x, c.y)
            val culled =
              area == 0.0 ||
                (style.culling == TriangleCulling.Back && area <= 0.0) ||
                (style.culling == TriangleCulling.Front && area >= 0.0)
            if culled then trianglesCulled += 1
            else
              val counts = rasterTriangle(
                a, b, c, area, slot, mesh.sourceFace(face), sourceA, sourceB, sourceC,
                mesh.nearestPartition.filter(face < _.renderFaceCount).fold(-1)(_ => mesh.sourceVertex(ia)),
                fragment,
                dimensions, pixels, depths, faceAt, slotAt, vertexAt, baryA, baryB, baryC
              )
              shadedPixels += counts._1
              depthRejected += counts._2
              scalarFragments += counts._3
            fan += 1
        faceOffset += 3
      slot += 1

    val renderNanos = System.nanoTime() - renderStarted
    val image = RasterImage.unsafeFromOwnedPackedArray(dimensions, pixels)
    val receipt = SurfaceRasterReceipt(
      trianglesInput,
      trianglesAfterClipping,
      trianglesClippedAway,
      trianglesCulled,
      shadedPixels,
      depthRejected,
      plan.layers.iterator.filterNot(layer => plan.fragmentSurfaces(layer.surface)).map(_.colors.length).sum,
      setupNanos,
      renderNanos,
      scalarFragments
    )
    new SurfaceRasterResult(
      image, receipt, plan.slots.map(_.surface), faceAt, slotAt, vertexAt,
      baryA, baryB, baryC, depths
    )

  private def clipVertex(
    plan: SurfaceRenderPlan,
    slot: SurfaceViewSlot,
    mesh: SurfaceMeshPacket,
    colors: Array[Int],
    vertex: Int,
    baryA: Double,
    baryB: Double,
    baryC: Double
  ): ClipVertex =
    val offset = vertex * 3
    val x = mesh.positions(offset).toDouble
    val y = mesh.positions(offset + 1).toDouble
    val z = mesh.positions(offset + 2).toDouble
    val view = multiply(
      plan.camera.viewMatrix,
      x + slot.worldOffsetX,
      y + slot.worldOffsetY,
      z + slot.worldOffsetZ,
      1.0
    )
    val clip = multiply(plan.camera.projectionMatrix, view._1, view._2, view._3, view._4)
    val color = packed(colors(vertex))
    val light = lightFactor(plan.lighting, mesh, offset)
    ClipVertex(
      x, y, z,
      clip._1, clip._2, clip._3, clip._4,
      color.red.toDouble * light,
      color.green.toDouble * light,
      color.blue.toDouble * light,
      color.alpha.toDouble,
      baryA, baryB, baryC
    )

  private def multiply(matrix: FloatBufferView, x: Double, y: Double, z: Double, w: Double): (Double, Double, Double, Double) =
    (
      matrix(0) * x + matrix(1) * y + matrix(2) * z + matrix(3) * w,
      matrix(4) * x + matrix(5) * y + matrix(6) * z + matrix(7) * w,
      matrix(8) * x + matrix(9) * y + matrix(10) * z + matrix(11) * w,
      matrix(12) * x + matrix(13) * y + matrix(14) * z + matrix(15) * w
    )

  private def lightFactor(lighting: SurfaceLighting, mesh: SurfaceMeshPacket, offset: Int): Double =
    lighting match
      case SurfaceLighting.Unlit => 1.0
      case SurfaceLighting.Directional(ambient, diffuse, dx, dy, dz) =>
        val dot = math.max(0.0, mesh.normals(offset) * dx + mesh.normals(offset + 1) * dy + mesh.normals(offset + 2) * dz)
        math.min(1.0, ambient.value + diffuse.value * dot)

  private def clipTriangle(
    vertices: Vector[ClipVertex],
    clipping: SurfaceClipping
  ): Vector[ClipVertex] =
    var polygon =
      clipping match
        case SurfaceClipping.WorldPlanes(planes) => clipWorldPlanes(vertices, planes)
        case _ => vertices
    var plane = 0
    while plane < 6 && polygon.nonEmpty do
      val output = Vector.newBuilder[ClipVertex]
      var previous = polygon.last
      var previousDistance = planeDistance(previous, plane)
      var index = 0
      while index < polygon.length do
        val current = polygon(index)
        val currentDistance = planeDistance(current, plane)
        val previousInside = previousDistance >= 0.0
        val currentInside = currentDistance >= 0.0
        if previousInside != currentInside then
          val fraction = previousDistance / (previousDistance - currentDistance)
          output += previous.interpolate(current, fraction)
        if currentInside then output += current
        previous = current
        previousDistance = currentDistance
        index += 1
      polygon = output.result()
      plane += 1
    polygon

  private def clipWorldPlanes(
    vertices: Vector[ClipVertex],
    planes: Vector[WorldClipPlane]
  ): Vector[ClipVertex] =
    var polygon = vertices
    var planeIndex = 0
    while planeIndex < planes.length && polygon.nonEmpty do
      val plane = planes(planeIndex)
      val output = Vector.newBuilder[ClipVertex]
      var previous = polygon.last
      var previousDistance = plane.orientedDistance(previous.worldX, previous.worldY, previous.worldZ)
      var index = 0
      while index < polygon.length do
        val current = polygon(index)
        val currentDistance = plane.orientedDistance(current.worldX, current.worldY, current.worldZ)
        val previousInside = previousDistance >= 0.0
        val currentInside = currentDistance >= 0.0
        if previousInside != currentInside then
          val fraction = previousDistance / (previousDistance - currentDistance)
          output += previous.interpolate(current, fraction)
        if currentInside then output += current
        previous = current
        previousDistance = currentDistance
        index += 1
      polygon = output.result()
      planeIndex += 1
    polygon

  private def planeDistance(vertex: ClipVertex, plane: Int): Double =
    plane match
      case 0 => vertex.x + vertex.w
      case 1 => vertex.w - vertex.x
      case 2 => vertex.y + vertex.w
      case 3 => vertex.w - vertex.y
      case 4 => vertex.z + vertex.w
      case _ => vertex.w - vertex.z

  private def toScreen(vertex: ClipVertex, viewport: SurfaceViewport, dimensions: RasterDimensions): ScreenVertex =
    val inverseW = 1.0 / vertex.w
    val ndcX = vertex.x * inverseW
    val ndcY = vertex.y * inverseW
    val ndcZ = vertex.z * inverseW
    val x = (viewport.x + (ndcX + 1.0) * 0.5 * viewport.width) * dimensions.width
    val y = (viewport.y + (1.0 - (ndcY + 1.0) * 0.5) * viewport.height) * dimensions.height
    ScreenVertex(
      x, y, (ndcZ + 1.0) * 0.5, inverseW,
      vertex.red * inverseW, vertex.green * inverseW, vertex.blue * inverseW, vertex.alpha * inverseW,
      vertex.baryA * inverseW, vertex.baryB * inverseW, vertex.baryC * inverseW
    )

  private def rasterTriangle(
    a: ScreenVertex,
    b: ScreenVertex,
    c: ScreenVertex,
    area: Double,
    slot: Int,
    face: Int,
    ia: Int,
    ib: Int,
    ic: Int,
    sampleOwner: Int,
    fragment: Option[SurfaceFragmentEvaluator],
    dimensions: RasterDimensions,
    pixels: Array[Int],
    depths: Array[Double],
    faceAt: Array[Int],
    slotAt: Array[Int],
    vertexAt: Array[Int],
    baryA: Array[Float],
    baryB: Array[Float],
    baryC: Array[Float]
  ): (Int, Int, Int) =
    val minX = math.max(0, math.floor(math.min(a.x, math.min(b.x, c.x))).toInt)
    val maxX = math.min(dimensions.width - 1, math.ceil(math.max(a.x, math.max(b.x, c.x))).toInt)
    val minY = math.max(0, math.floor(math.min(a.y, math.min(b.y, c.y))).toInt)
    val maxY = math.min(dimensions.height - 1, math.ceil(math.max(a.y, math.max(b.y, c.y))).toInt)
    var shaded = 0
    var rejected = 0
    var scalarEvaluated = 0
    var y = minY
    while y <= maxY do
      var x = minX
      while x <= maxX do
        val px = x + 0.5
        val py = y + 0.5
        val wa = edge(b.x, b.y, c.x, c.y, px, py) / area
        val wb = edge(c.x, c.y, a.x, a.y, px, py) / area
        val wc = 1.0 - wa - wb
        if wa >= -1e-12 && wb >= -1e-12 && wc >= -1e-12 then
          val depth = wa * a.depth + wb * b.depth + wc * c.depth
          val pixelIndex = y * dimensions.width + x
          if depth < depths(pixelIndex) then
            val reciprocal = wa * a.inverseW + wb * b.inverseW + wc * c.inverseW
            val inv = 1.0 / reciprocal
            val ba = (wa * a.baryAOverW + wb * b.baryAOverW + wc * c.baryAOverW) * inv
            val bb = (wa * a.baryBOverW + wb * b.baryBOverW + wc * c.baryBOverW) * inv
            val bc = (wa * a.baryCOverW + wb * b.baryCOverW + wc * c.baryCOverW) * inv
            val color = fragment match
              case Some(evaluator) =>
                scalarEvaluated += evaluator.scalarLayerCount
                evaluator.color(face, ia, ib, ic, ba, bb, bc)
              case None => Rgba32.unsafe(
                clampByte((wa * a.redOverW + wb * b.redOverW + wc * c.redOverW) * inv),
                clampByte((wa * a.greenOverW + wb * b.greenOverW + wc * c.greenOverW) * inv),
                clampByte((wa * a.blueOverW + wb * b.blueOverW + wc * c.blueOverW) * inv),
                clampByte((wa * a.alphaOverW + wb * b.alphaOverW + wc * c.alphaOverW) * inv))
            val red = color.red
            val green = color.green
            val blue = color.blue
            val alpha = color.alpha
            if alpha > 0 then
              val chosen = SurfaceNearestPartition.nearestVertex(ia, ib, ic, ba, bb, bc)
              if sampleOwner < 0 || chosen == sampleOwner then
                pixels(pixelIndex) = compositeOverOpaque(pixels(pixelIndex), red, green, blue, alpha)
                depths(pixelIndex) = depth
                faceAt(pixelIndex) = face
                slotAt(pixelIndex) = slot
                baryA(pixelIndex) = ba.toFloat
                baryB(pixelIndex) = bb.toFloat
                baryC(pixelIndex) = bc.toFloat
                vertexAt(pixelIndex) = chosen
                shaded += 1
          else rejected += 1
        x += 1
      y += 1
    (shaded, rejected, scalarEvaluated)

  private inline def edge(ax: Double, ay: Double, bx: Double, by: Double, px: Double, py: Double): Double =
    (px - ax) * (by - ay) - (py - ay) * (bx - ax)

  private def packed(value: Int): Rgba32 =
    Rgba32.fromPackedInt(value)

  private def clampByte(value: Double): Int =
    math.round(value).toInt.max(0).min(255)

  private def compositeOverOpaque(under: Int, red: Int, green: Int, blue: Int, alpha: Int): Int =
    if alpha == 255 then (red << 24) | (green << 16) | (blue << 8) | 255
    else
      val inverse = 255 - alpha
      val underRed = (under >>> 24) & 0xff
      val underGreen = (under >>> 16) & 0xff
      val underBlue = (under >>> 8) & 0xff
      val outRed = (red * alpha + underRed * inverse + 127) / 255
      val outGreen = (green * alpha + underGreen * inverse + 127) / 255
      val outBlue = (blue * alpha + underBlue * inverse + 127) / 255
      (outRed << 24) | (outGreen << 16) | (outBlue << 8) | 255
