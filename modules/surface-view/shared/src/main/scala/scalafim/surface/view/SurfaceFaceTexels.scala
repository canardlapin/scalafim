package scalafim.surface.view

import intaglio.*

enum SurfaceFaceTexelError:
  case InvalidLayout(faces: Int, texelsPerFace: Int, maxTextureSize: Int)
  case GeneratedCorners(surface: SurfaceId)
  case SampleDomainMismatch(layer: SurfaceLayerId, expected: Int, actual: Int)
  case InterpolatedLayer(layer: SurfaceLayerId, policy: SurfaceMapInterpolation)

  def message: String = this match
    case InvalidLayout(faces, texels, max) =>
      s"$faces faces with ${texels}x$texels texel blocks do not fit a ${max}x$max texture"
    case GeneratedCorners(surface) =>
      s"surface '${surface.value}' renders generated corners or partitions; flat face texels need the scientific faces"
    case SampleDomainMismatch(layer, expected, actual) =>
      s"layer '${layer.value}' has $actual samples for $expected render elements"
    case InterpolatedLayer(layer, policy) =>
      s"layer '${layer.value}' declares $policy; flat face texels show one colour per face and accept only face-flat scalar or face-constant layers"

/** Row-major placement of one square block of texels per face in one texture.
  *
  * Every corner of a face addresses the centre of that face's block, so the
  * texture coordinate is constant across the face. Its screen-space derivatives
  * are zero, which selects the base mip level under any mipmap policy, and
  * multisample extrapolation of a constant is the same constant; no tile has
  * padding. With 2x2 blocks the centre is the shared corner of four equal
  * texels, so bilinear filtering returns the block colour for any coordinate
  * error below half a texel. 1x1 blocks rely on addressing the texel centre.
  * The width is a power of two, so every horizontal coordinate is exact.
  */
final case class SurfaceFaceTexelLayout private (faceCount: Int, texelsPerFace: Int, width: Int, height: Int):
  val blocksPerRow: Int = width / texelsPerFace

  def blockX(face: Int): Int = (face % blocksPerRow) * texelsPerFace
  def blockY(face: Int): Int = (face / blocksPerRow) * texelsPerFace
  def u(face: Int): Float = ((blockX(face) + texelsPerFace * 0.5) / width).toFloat
  def v(face: Int): Float = ((blockY(face) + texelsPerFace * 0.5) / height).toFloat

  /** Interleaved (u, v) per face; the coordinate index of face `f` is `f`. */
  def coordinates: Array[Float] =
    val out = new Array[Float](faceCount * 2)
    var face = 0
    while face < faceCount do
      out(face * 2) = u(face)
      out(face * 2 + 1) = v(face)
      face += 1
    out

object SurfaceFaceTexelLayout:
  def make(faceCount: Int, texelsPerFace: Int, maxTextureSize: Int): Either[SurfaceFaceTexelError, SurfaceFaceTexelLayout] =
    if faceCount < 1 || (texelsPerFace != 1 && texelsPerFace != 2) || maxTextureSize < 2 then
      Left(SurfaceFaceTexelError.InvalidLayout(faceCount, texelsPerFace, maxTextureSize))
    else
      val side = math.ceil(math.sqrt(faceCount.toDouble)).toLong * texelsPerFace
      var width = 2
      while width < side && width.toLong * 2 <= maxTextureSize do width *= 2
      val blocksPerRow = width / texelsPerFace
      val height = ((faceCount.toLong + blocksPerRow - 1) / blocksPerRow) * texelsPerFace
      if height > maxTextureSize then Left(SurfaceFaceTexelError.InvalidLayout(faceCount, texelsPerFace, maxTextureSize))
      else Right(new SurfaceFaceTexelLayout(faceCount, texelsPerFace, width, height.toInt))

/** The flat per-face colour rule.
  *
  * Face texels show exactly what the plan declares: face-flat scalar layers
  * ([[SurfaceMapInterpolation.FaceScalarMean]], [[SurfaceMapInterpolation.FaceScalarMaxMagnitude]])
  * contribute their mapping of the face's reduced value, face-constant layers
  * their face sample, and layers composite in plan order over the neutral base,
  * with coverage, exactly as [[SurfaceFragmentEvaluator]] composes a fragment of
  * that face (unlit). Layers that declare vertex interpolation are refused, so a
  * plan never claims an interpolated field while showing face colours. Lighting
  * is not part of the colour: the native renderer shades each pixel.
  */
final class SurfaceFaceColorRule private[view] (val faceCount: Int, base: Rgba32, layers: Vector[SurfaceFaceTexels.FaceLayer]):
  private val ops = layers.toArray
  private val transparent = Rgba32.unsafe(0, 0, 0, 0)

  /** Pure: disjoint face ranges may be evaluated concurrently into one target. */
  def write(target: Array[Int], from: Int, until: Int): Unit =
    require(from >= 0 && until <= faceCount && target.length >= faceCount, "face range outside the rule")
    var face = from
    while face < until do
      var composed = base
      var index = 0
      while index < ops.length do
        val op = ops(index)
        val over = if !op.coverage.contains(face) then transparent else op.color(face)
        composed = op.blendMode.composite(composed, over, op.opacity)
        index += 1
      target(face) = composed.toPackedInt
      face += 1

object SurfaceFaceTexels:
  val Base: Rgba32 = Rgba32.unsafe(184, 184, 184)

  private[view] sealed trait FaceLayer:
    def coverage: SurfaceLayerCoverage
    def blendMode: DisplayBlendMode
    def opacity: DisplayOpacity
    def color(face: Int): Rgba32

  private final class ScalarFaces(values: Array[Double], mapping: ScalarMapping, val coverage: SurfaceLayerCoverage,
      val blendMode: DisplayBlendMode, val opacity: DisplayOpacity) extends FaceLayer:
    def color(face: Int): Rgba32 = mapping.color(values(face))

  private final class ConstantFaces(colors: IntBufferView, val coverage: SurfaceLayerCoverage,
      val blendMode: DisplayBlendMode, val opacity: DisplayOpacity) extends FaceLayer:
    def color(face: Int): Rgba32 = Rgba32.fromPackedInt(colors(face))

  /** Reduce one layer's samples face by face into `target[from, until)`. Pure. */
  def reduceInto(reduction: SurfaceFaceReduction, samples: DoubleBufferView, indices: IntBufferView,
      target: Array[Double], from: Int, until: Int): Unit =
    var face = from
    while face < until do
      val offset = face * 3
      target(face) = reduction.reduce(samples(indices(offset)), samples(indices(offset + 1)), samples(indices(offset + 2)))
      face += 1

  def reduce(reduction: SurfaceFaceReduction, samples: DoubleBufferView, indices: IntBufferView): Array[Double] =
    val out = new Array[Double](indices.length / 3)
    reduceInto(reduction, samples, indices, out, 0, out.length)
    out

  /** Validate one surface's layers and lower them to a face rule. `reduced` supplies the
    * per-face values of a validated face-flat layer (for example from a cache); it is
    * called only after every layer has been validated.
    */
  def lower(plan: SurfaceRenderPlan, mesh: SurfaceMeshPacket, base: Rgba32 = Base,
      reduced: Option[SurfaceLayerPacket => Array[Double]] = None): Either[SurfaceFaceTexelError, SurfaceFaceColorRule] =
    if mesh.sourceVertices.nonEmpty || mesh.constantPartition.nonEmpty || mesh.nearestPartition.nonEmpty then
      return Left(SurfaceFaceTexelError.GeneratedCorners(mesh.surface))
    val vertices = mesh.positions.length / 3
    val faces = mesh.indices.length / 3
    val layers = plan.layers.filter(_.surface == mesh.surface)
    val refusal = layers.iterator.map: layer =>
      layer.interpolation match
        case policy if policy.faceFlat =>
          val actual = layer.scalarField.fold(-1)(_.samples.length)
          Option.when(actual != vertices)(SurfaceFaceTexelError.SampleDomainMismatch(layer.layer, vertices, actual))
        case SurfaceMapInterpolation.FaceConstant =>
          val actual = layer.sampleColors.fold(-1)(_.length)
          Option.when(actual != faces)(SurfaceFaceTexelError.SampleDomainMismatch(layer.layer, faces, actual))
        case policy => Some(SurfaceFaceTexelError.InterpolatedLayer(layer.layer, policy))
    .collectFirst { case Some(error) => error }
    refusal.toLeft:
      val values = reduced.getOrElse((layer: SurfaceLayerPacket) =>
        reduce(layer.interpolation.faceReduction.get, layer.scalarField.get.samples, mesh.indices))
      val ops = layers.map: layer =>
        if layer.interpolation.faceFlat then
          new ScalarFaces(values(layer), layer.scalarField.get.mapping, layer.coverage, layer.blendMode, layer.opacity)
        else new ConstantFaces(layer.sampleColors.get, layer.coverage, layer.blendMode, layer.opacity)
      new SurfaceFaceColorRule(faces, base, ops)

  def colors(plan: SurfaceRenderPlan, mesh: SurfaceMeshPacket, base: Rgba32 = Base): Either[SurfaceFaceTexelError, Array[Int]] =
    lower(plan, mesh, base).map: rule =>
      val out = new Array[Int](rule.faceCount)
      rule.write(out, 0, rule.faceCount)
      out
