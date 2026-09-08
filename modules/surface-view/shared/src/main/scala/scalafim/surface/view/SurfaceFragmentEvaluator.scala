package scalafim.surface.view

import intaglio.*

/** Ordered layer evaluation in original-triangle coordinates. Base colors are
  * evaluated before opacity/composition; world-space lighting follows composition.
  * Pure legacy vertex-color scenes retain their original precomposition path.
  */
final class SurfaceFragmentEvaluator(
  mesh: SurfaceMeshPacket,
  layers: Vector[SurfaceLayerPacket],
  lighting: SurfaceLighting,
  base: Rgba32,
  private val region: Option[SurfaceMappingTriangle] = None
):
  val scalarLayerCount: Int = layers.count(_.scalarField.nonEmpty)
  private val normals = mesh.sampleNormals.getOrElse(mesh.normals)

  private val regionSamples = layers.map: layer =>
    for triangle <- region if layer.coverage.contains(triangle.sourceFace); field <- layer.scalarField yield
      val (a, b, c) = triangle.sourceVertices
      val w = triangle.centroid
      field.mapping.evaluate(SurfaceScalarInterpolation.value(field.samples(a), field.samples(b), field.samples(c), w.a, w.b, w.c))
  private val regionOwner = region.map: triangle =>
    val (a, b, c) = triangle.sourceVertices
    val w = triangle.centroid
    SurfaceNearestPartition.nearestVertex(a, b, c, w.a, w.b, w.c)

  /** One-sided extension of a partition cell's interior to its texture padding.
    * Endpoint inclusion remains exact in color(); this method deliberately uses
    * the cell's branch at its boundary to prevent filtering from smearing a seam.
    * The caller must first partition all scalar and nearest-sample boundaries.
    */
  def within(triangle: SurfaceMappingTriangle): SurfaceFragmentEvaluator =
    new SurfaceFragmentEvaluator(mesh, layers, lighting, base, Some(triangle))

  def color(face: Int, a: Int, b: Int, c: Int, wa: Double, wb: Double, wc: Double): Rgba32 =
    evaluate(face, a, b, c, wa, wb, wc, false)

  /** Extend a cell's smooth branch outside its triangle for texture filtering.
    * Signed affine weights are display padding only, never scientific samples.
    */
  def extrapolatedColor(face: Int, a: Int, b: Int, c: Int, wa: Double, wb: Double, wc: Double): Rgba32 =
    require(region.nonEmpty, "texture extrapolation requires a partition region")
    require(wa.isFinite && wb.isFinite && wc.isFinite && math.abs(wa + wb + wc - 1) <= 1e-9)
    evaluate(face, a, b, c, wa, wb, wc, true)

  private def evaluate(face: Int, a: Int, b: Int, c: Int, wa: Double, wb: Double, wc: Double, extrapolate: Boolean): Rgba32 =
    var composed = base
    var index = 0
    while index < layers.length do
      val layer = layers(index)
      val over = if !layer.coverage.contains(face) then Rgba32.unsafe(0, 0, 0, 0) else layer.interpolation match
        case SurfaceMapInterpolation.VertexScalar =>
          val field = layer.scalarField.get
          val value = if !extrapolate then SurfaceScalarInterpolation.value(field.samples(a), field.samples(b), field.samples(c), wa, wb, wc)
            else
              val x = field.samples(a)
              val y = field.samples(b)
              val z = field.samples(c)
              val direct = wa * x + wb * y + wc * z
              if direct.isFinite then direct
              else
                val scale = math.max(math.abs(x), math.max(math.abs(y), math.abs(z)))
                (wa * (x / scale) + wb * (y / scale) + wc * (z / scale)) * scale
          regionSamples(index) match
            case None => field.mapping.color(value)
            case Some(sample) if sample.state == ScalarSampleState.ClampedLow || sample.state == ScalarSampleState.ClampedHigh => sample.color
            case Some(sample) => sample.coordinate match
              case None => sample.color
              case Some(coordinate) =>
                val segment = field.mapping.scale.segments(coordinate.segment)
                segment.ramp.colorAt(segment.window.normalize(value))
        case SurfaceMapInterpolation.FaceConstant => Rgba32.fromPackedInt(layer.sampleColors.get(face))
        case SurfaceMapInterpolation.NearestVertex =>
          val vertex = regionOwner.getOrElse(SurfaceNearestPartition.nearestVertex(a, b, c, wa, wb, wc))
          Rgba32.fromPackedInt(layer.sampleColors.get(vertex))
        case SurfaceMapInterpolation.VertexColor =>
          val colors = layer.sampleColors.get
          SurfaceFragmentEvaluator.interpolateColor(colors(a), colors(b), colors(c), wa, wb, wc)
      composed = layer.blendMode.composite(composed, over, layer.opacity)
      index += 1
    lighting match
      case SurfaceLighting.Unlit => composed
      case SurfaceLighting.Directional(ambient, diffuse, dx, dy, dz) =>
        val nx = wa * normals(a * 3) + wb * normals(b * 3) + wc * normals(c * 3)
        val ny = wa * normals(a * 3 + 1) + wb * normals(b * 3 + 1) + wc * normals(c * 3 + 1)
        val nz = wa * normals(a * 3 + 2) + wb * normals(b * 3 + 2) + wc * normals(c * 3 + 2)
        val length = math.sqrt(nx * nx + ny * ny + nz * nz)
        val dot = if length == 0.0 then 0.0 else math.max(0.0, (nx * dx + ny * dy + nz * dz) / length)
        val factor = math.min(1.0, ambient.value + diffuse.value * dot)
        Rgba32.unsafe(math.round(composed.red * factor).toInt, math.round(composed.green * factor).toInt,
          math.round(composed.blue * factor).toInt, composed.alpha)

object SurfaceFragmentEvaluator:
  def required(policies: Iterable[SurfaceMapInterpolation]): Boolean =
    policies.exists(_ == SurfaceMapInterpolation.VertexScalar) ||
      (policies.exists(_ == SurfaceMapInterpolation.VertexColor) && policies.exists(_ != SurfaceMapInterpolation.VertexColor))

  def interpolateColor(a: Int, b: Int, c: Int, wa: Double, wb: Double, wc: Double): Rgba32 =
    def channel(shift: Int): Int = math.round(
      ((a >>> shift) & 255) * wa + ((b >>> shift) & 255) * wb + ((c >>> shift) & 255) * wc).toInt.max(0).min(255)
    Rgba32.unsafe(channel(24), channel(16), channel(8), channel(0))
