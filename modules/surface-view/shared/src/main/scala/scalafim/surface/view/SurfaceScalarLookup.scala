package scalafim.surface.view

import intaglio.*

enum SurfaceLookupError:
  case InvalidAxis(texels: Int, pad: Int, hidden: Int)
  case NotAFragmentSurface(surface: SurfaceId)
  case UnsupportedLayer(layer: SurfaceLayerId, reason: String)
  case UnsupportedLayerCount(surface: SurfaceId, count: Int)
  case SampleDomainMismatch(layer: SurfaceLayerId, expected: Int, actual: Int)

  def message: String = this match
    case InvalidAxis(texels, pad, hidden) =>
      s"lookup axis needs an even texel count holding a hidden block, two pads and two data texels per half; got $texels/$pad/$hidden"
    case NotAFragmentSurface(surface) => s"surface '${surface.value}' has no interpolated-scalar layers"
    case UnsupportedLayer(layer, reason) => s"layer '${layer.value}' cannot be lowered to a lookup table: $reason"
    case UnsupportedLayerCount(surface, count) => s"surface '${surface.value}' has $count fragment layers; the lookup table supports one overlay over at most one underlay"
    case SampleDomainMismatch(layer, expected, actual) => s"layer '${layer.value}' has $actual scalar samples for $expected render vertices"

/** One mirrored lookup axis. Each half holds `hidden` invalid-sample texels,
  * `pad` lower-edge texels, `dataTexels` interior texels and `pad` upper-edge
  * texels; the second half is its mirror image. Repeat wrapping therefore joins
  * the two hidden blocks at coordinate 0 (the missing-sample seam) and the two
  * upper pads at coordinate 0.5, and every automatically generated mip level is
  * a box average of neighbouring content only. No data texel is reserved.
  */
final case class SurfaceLookupAxis private (texels: Int, pad: Int, hidden: Int):
  val half: Int = texels / 2
  val dataTexels: Int = half - hidden - 2 * pad

  /** Texel-centre texture coordinate for a normalized position clamped to [0, 1]. */
  def coordinate(fraction: Double): Float =
    val t = if fraction.isNaN then 0.0 else math.max(0.0, math.min(1.0, fraction))
    ((hidden + pad + 0.5 + t * (dataTexels - 1)) / texels).toFloat

  /** The seam between the two hidden blocks. A face whose corners all sample it
    * is uniformly hidden at every mip level up to log2(2 * hidden).
    */
  def anchor: Float = 0.0f

  private def forward(index: Int): Int = if index < half then index else texels - 1 - index

  def hiddenTexel(index: Int): Boolean = forward(index) < hidden

  /** Index of the data texel nearest to a texture coordinate. */
  def dataIndex(coordinate: Double): Int =
    val texel = math.max(0, math.min(texels - 1, math.floor(coordinate * texels).toInt))
    math.max(0, math.min(dataTexels - 1, forward(texel) - hidden - pad))

  /** Normalized position represented by texel `index` of the full mirrored axis. */
  def fraction(index: Int): Double =
    math.max(0.0, math.min(1.0, (forward(index) - hidden - pad).toDouble / (dataTexels - 1)))

object SurfaceLookupAxis:
  def make(texels: Int, pad: Int, hidden: Int): Either[SurfaceLookupError, SurfaceLookupAxis] =
    if texels < 8 || texels % 2 != 0 || pad < 0 || hidden < 1 || texels / 2 - hidden - 2 * pad < 2 then
      Left(SurfaceLookupError.InvalidAxis(texels, pad, hidden))
    else Right(new SurfaceLookupAxis(texels, pad, hidden))

  /** 2048 texels: 768 data texels per half. The 256-texel hidden seam and the
    * 64-texel pads keep neighbouring blocks out of footprints up to mip level 7.
    */
  val Overlay: SurfaceLookupAxis = make(2048, 64, 128).toOption.get
  /** 1024 texels: 384 data texels per half behind 32-texel pads and a 128-texel seam. */
  val Underlay: SurfaceLookupAxis = make(1024, 32, 64).toOption.get
  /** Constant rows for surfaces without an underlay scalar. */
  val Constant: SurfaceLookupAxis = make(16, 1, 2).toOption.get

final case class SurfaceLookupLayer(mapping: ScalarMapping, opacity: DisplayOpacity, blendMode: DisplayBlendMode):
  def key: String = s"${mapping.canonicalKey}|${opacity.toDouble}|$blendMode"

/** Baked opaque table: columns are overlay scalar positions, rows are underlay
  * scalar positions, each texel the same ordered composition the fragment
  * evaluator performs at a vertex. Hidden-seam texels hold each mapping's
  * invalid colour. Colours are stored as packed `Rgba32`.
  */
final class SurfaceScalarLookupTable private[view] (
  val overlayAxis: SurfaceLookupAxis,
  val underlayAxis: SurfaceLookupAxis,
  val overlay: SurfaceLookupLayer,
  val underlay: Option[SurfaceLookupLayer],
  val base: Rgba32,
  private val pixels: Array[Int],
  val key: String
):
  def width: Int = overlayAxis.texels
  def height: Int = underlayAxis.texels
  def pixel(x: Int, y: Int): Int = pixels(y * width + x)
  private[scalafim] def unsafePixels: Array[Int] = pixels

  /** Overlay texture coordinate: the raw value normalized inside the mapping
    * window. Non-finite values sit on the hidden seam.
    */
  def overlayCoordinate(value: Double): Float =
    if !value.isFinite then overlayAxis.anchor
    else overlayAxis.coordinate(overlay.mapping.scale.window.normalize(value))

  def underlayCoordinate(value: Double): Float = underlay match
    case None => underlayAxis.coordinate(0.5)
    case Some(layer) =>
      if !value.isFinite then underlayAxis.anchor
      else underlayAxis.coordinate(layer.mapping.scale.window.normalize(value))

  def missingCoordinate: Float = overlayAxis.anchor

  /** Bilinear texel-centre sample with repeat wrapping; the CPU model of the sampler at mip level zero. */
  def sample(u: Double, v: Double): Rgba32 =
    def wrap(index: Int, size: Int): Int = ((index % size) + size) % size
    val x = u * width - 0.5
    val y = v * height - 0.5
    val x0 = math.floor(x).toInt
    val y0 = math.floor(y).toInt
    val fx = x - x0
    val fy = y - y0
    def channel(shift: Int): Int =
      def at(px: Int, py: Int): Double = ((pixel(wrap(px, width), wrap(py, height)) >>> shift) & 255).toDouble
      val top = at(x0, y0) * (1 - fx) + at(x0 + 1, y0) * fx
      val bottom = at(x0, y0 + 1) * (1 - fx) + at(x0 + 1, y0 + 1) * fx
      math.round(top * (1 - fy) + bottom * fy).toInt.max(0).min(255)
    Rgba32.unsafe(channel(24), channel(16), channel(8), channel(0))

/** Per-surface lowering of an interpolated-scalar plan to one lookup table,
  * interleaved (u, v) texture coordinates and one coordinate index per face
  * corner. The first `vertexCount` pairs belong to the render vertices. A face
  * with any non-finite overlay (or underlay) corner owns three appended pairs
  * whose overlay (or underlay) coordinate is the hidden seam, reproducing the
  * reference rule that one missing corner hides that layer across the face.
  * Geometry, faces and identities are untouched.
  */
final case class SurfaceScalarLookupPlan(
  table: SurfaceScalarLookupTable,
  vertexCount: Int,
  coordinates: Array[Float],
  faceCoordinates: Array[Int],
  missingFaces: Int
)

object SurfaceScalarLookup:
  val Base: Rgba32 = Rgba32.unsafe(184, 184, 184)

  def bake(overlay: SurfaceLookupLayer, underlay: Option[SurfaceLookupLayer],
      overlayAxis: SurfaceLookupAxis = SurfaceLookupAxis.Overlay,
      underlayAxis: SurfaceLookupAxis = SurfaceLookupAxis.Underlay,
      base: Rgba32 = Base): SurfaceScalarLookupTable =
    val rows = if underlay.isEmpty then SurfaceLookupAxis.Constant else underlayAxis
    val width = overlayAxis.texels
    val height = rows.texels
    val window = overlay.mapping.scale.window
    val columns = new Array[Int](overlayAxis.half)
    var x = 0
    while x < overlayAxis.half do
      columns(x) = (if overlayAxis.hiddenTexel(x) then overlay.mapping.invalid
        else overlay.mapping.color(window.lower + overlayAxis.fraction(x) * window.width)).toPackedInt
      x += 1
    val rowColors = new Array[Int](rows.half)
    var y = 0
    while y < rows.half do
      rowColors(y) = underlay match
        case None => base.toPackedInt
        case Some(layer) =>
          val w = layer.mapping.scale.window
          val color = if rows.hiddenTexel(y) then layer.mapping.invalid else layer.mapping.color(w.lower + rows.fraction(y) * w.width)
          layer.blendMode.composite(base, color, layer.opacity).toPackedInt
      y += 1
    val pixels = new Array[Int](width * height)
    y = 0
    while y < rows.half do
      val under = Rgba32.fromPackedInt(rowColors(y))
      val mirrorRow = height - 1 - y
      x = 0
      while x < overlayAxis.half do
        val texel = overlay.blendMode.composite(under, Rgba32.fromPackedInt(columns(x)), overlay.opacity).toPackedInt
        val mirrorColumn = width - 1 - x
        pixels(y * width + x) = texel
        pixels(y * width + mirrorColumn) = texel
        pixels(mirrorRow * width + x) = texel
        pixels(mirrorRow * width + mirrorColumn) = texel
        x += 1
      y += 1
    val key = s"scalar-lookup-v2|${width}x$height|${overlayAxis.pad}/${overlayAxis.hidden}|${rows.pad}/${rows.hidden}|" +
      s"${base.toPackedInt}|${overlay.key}|${underlay.fold("none")(_.key)}"
    new SurfaceScalarLookupTable(overlayAxis, rows, overlay, underlay, base, pixels, key)

  /** Texture coordinates and face-corner indices for one surface's samples. */
  def lower(table: SurfaceScalarLookupTable, overlay: DoubleBufferView, underlay: Option[DoubleBufferView],
      indices: IntBufferView): SurfaceScalarLookupPlan =
    val vertices = overlay.length
    val faces = indices.length / 3
    def overlayMissing(vertex: Int): Boolean = !overlay(vertex).isFinite
    def underlayMissing(vertex: Int): Boolean = underlay match
      case Some(samples) => !samples(vertex).isFinite
      case None => false
    def underlayValue(vertex: Int): Double = underlay match
      case Some(samples) => samples(vertex)
      case None => Double.NaN
    var missing = 0
    var face = 0
    while face < faces do
      var corner = 0
      var any = false
      while corner < 3 do
        val vertex = indices(face * 3 + corner)
        if overlayMissing(vertex) || underlayMissing(vertex) then any = true
        corner += 1
      if any then missing += 1
      face += 1
    val coordinates = new Array[Float]((vertices + missing * 3) * 2)
    var vertex = 0
    while vertex < vertices do
      coordinates(vertex * 2) = table.overlayCoordinate(overlay(vertex))
      coordinates(vertex * 2 + 1) = table.underlayCoordinate(underlayValue(vertex))
      vertex += 1
    val faceCoordinates = new Array[Int](faces * 3)
    var next = vertices
    face = 0
    while face < faces do
      val a = indices(face * 3)
      val b = indices(face * 3 + 1)
      val c = indices(face * 3 + 2)
      val hideOverlay = overlayMissing(a) || overlayMissing(b) || overlayMissing(c)
      val hideUnderlay = underlayMissing(a) || underlayMissing(b) || underlayMissing(c)
      var corner = 0
      while corner < 3 do
        val source = indices(face * 3 + corner)
        if !hideOverlay && !hideUnderlay then faceCoordinates(face * 3 + corner) = source
        else
          coordinates(next * 2) = if hideOverlay then table.overlayAxis.anchor else table.overlayCoordinate(overlay(source))
          coordinates(next * 2 + 1) = if hideUnderlay then table.underlayAxis.anchor else table.underlayCoordinate(underlayValue(source))
          faceCoordinates(face * 3 + corner) = next
          next += 1
        corner += 1
      face += 1
    SurfaceScalarLookupPlan(table, vertices, coordinates, faceCoordinates, missing)

  /** Lower the fragment layers of one surface. Draw order is the plan's layer order:
    * an optional underlay first, then the overlay. Both must carry raw samples.
    */
  def plan(plan: SurfaceRenderPlan, mesh: SurfaceMeshPacket,
      overlayAxis: SurfaceLookupAxis = SurfaceLookupAxis.Overlay,
      underlayAxis: SurfaceLookupAxis = SurfaceLookupAxis.Underlay): Either[SurfaceLookupError, SurfaceScalarLookupPlan] =
    if !plan.fragmentSurfaces(mesh.surface) then return Left(SurfaceLookupError.NotAFragmentSurface(mesh.surface))
    val layers = plan.layers.filter(_.surface == mesh.surface)
    if layers.isEmpty || layers.length > 2 then return Left(SurfaceLookupError.UnsupportedLayerCount(mesh.surface, layers.length))
    val vertices = mesh.positions.length / 3
    var index = 0
    while index < layers.length do
      val layer = layers(index)
      if layer.interpolation != SurfaceMapInterpolation.VertexScalar || layer.scalarField.isEmpty then
        return Left(SurfaceLookupError.UnsupportedLayer(layer.layer, "only interpolated scalar layers carry raw samples"))
      if layer.coverage != SurfaceLayerCoverage.All then
        return Left(SurfaceLookupError.UnsupportedLayer(layer.layer, "partial face coverage is not representable per vertex"))
      if mesh.sourceVertices.nonEmpty || mesh.constantPartition.nonEmpty || mesh.nearestPartition.nonEmpty then
        return Left(SurfaceLookupError.UnsupportedLayer(layer.layer, "render vertices must be the scientific vertices"))
      val samples = layer.scalarField.get.samples
      if samples.length != vertices then return Left(SurfaceLookupError.SampleDomainMismatch(layer.layer, vertices, samples.length))
      index += 1
    def lookupLayer(layer: SurfaceLayerPacket): SurfaceLookupLayer =
      SurfaceLookupLayer(layer.scalarField.get.mapping, layer.opacity, layer.blendMode)
    val overlay = layers.last
    val underlay = if layers.length == 2 then Some(layers.head) else None
    val table = bake(lookupLayer(overlay), underlay.map(lookupLayer), overlayAxis, underlayAxis)
    Right(lower(table, overlay.scalarField.get.samples, underlay.map(_.scalarField.get.samples), mesh.indices))
