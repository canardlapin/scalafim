package scalafim.surface.view

import intaglio.*

enum SurfaceFaceTexelError:
  case InvalidLayout(faces: Int, texelsPerFace: Int, maxTextureSize: Int)
  case GeneratedCorners(surface: SurfaceId)
  case SampleDomainMismatch(layer: SurfaceLayerId, expected: Int, actual: Int)

  def message: String = this match
    case InvalidLayout(faces, texels, max) =>
      s"$faces faces with ${texels}x$texels texel blocks do not fit a ${max}x$max texture"
    case GeneratedCorners(surface) =>
      s"surface '${surface.value}' renders generated corners or partitions; flat face texels need the scientific faces"
    case SampleDomainMismatch(layer, expected, actual) =>
      s"layer '${layer.value}' has $actual samples for $expected render elements"

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
  * A face shows the unlit colour the portable fragment evaluator produces at
  * its centroid. Each vertex-scalar layer therefore applies its mapping to the
  * mean of the face's three samples; a face with any non-finite sample is
  * missing for that layer, exactly as in the interpolated reference (so flat
  * and interpolated coverage agree); vertex-colour layers contribute their
  * channel mean and face-constant layers their face sample. Layers composite in
  * plan order over the neutral base. With the application's recipe this gives
  * saturated colour from the cutoff onward and the curvature underlay, which
  * follows the sign of the face's mean curvature, wherever the overlay is below
  * the cutoff or missing; never a flat grey. Lighting is not part of the colour:
  * the native renderer shades each pixel from the interpolated vertex normals.
  */
/** One surface's lowered face rule. `write` is pure: disjoint face ranges may be
  * evaluated concurrently into the same target array.
  */
final class SurfaceFaceColorRule private[view] (val faceCount: Int, evaluator: SurfaceFragmentEvaluator, indices: IntBufferView):
  private val third = 1.0 / 3.0

  def write(target: Array[Int], from: Int, until: Int): Unit =
    require(from >= 0 && until <= faceCount && target.length >= faceCount, "face range outside the rule")
    var face = from
    while face < until do
      val offset = face * 3
      target(face) = evaluator.color(face, indices(offset), indices(offset + 1), indices(offset + 2), third, third, third).toPackedInt
      face += 1

object SurfaceFaceTexels:
  val Base: Rgba32 = Rgba32.unsafe(184, 184, 184)

  def lower(plan: SurfaceRenderPlan, mesh: SurfaceMeshPacket, base: Rgba32 = Base): Either[SurfaceFaceTexelError, SurfaceFaceColorRule] =
    if mesh.sourceVertices.nonEmpty || mesh.constantPartition.nonEmpty || mesh.nearestPartition.nonEmpty then
      Left(SurfaceFaceTexelError.GeneratedCorners(mesh.surface))
    else
      val vertices = mesh.positions.length / 3
      val faces = mesh.indices.length / 3
      // Non-fragment surfaces carry render-vertex colours only; with no generated
      // corners those are the scientific vertex samples.
      val layers = plan.layers.filter(_.surface == mesh.surface).map: layer =>
        if layer.sampleColors.nonEmpty then layer else layer.copy(sampleColors = Some(layer.colors))
      val mismatch = layers.iterator.map: layer =>
        val (expected, actual) = layer.interpolation match
          case SurfaceMapInterpolation.VertexScalar => (vertices, layer.scalarField.fold(-1)(_.samples.length))
          case SurfaceMapInterpolation.FaceConstant => (faces, layer.sampleColors.get.length)
          case SurfaceMapInterpolation.VertexColor | SurfaceMapInterpolation.NearestVertex => (vertices, layer.sampleColors.get.length)
        Option.when(expected != actual)(SurfaceFaceTexelError.SampleDomainMismatch(layer.layer, expected, actual))
      .collectFirst { case Some(error) => error }
      mismatch.toLeft(new SurfaceFaceColorRule(faces, new SurfaceFragmentEvaluator(mesh, layers, SurfaceLighting.Unlit, base), mesh.indices))

  def colors(plan: SurfaceRenderPlan, mesh: SurfaceMeshPacket, base: Rgba32 = Base): Either[SurfaceFaceTexelError, Array[Int]] =
    lower(plan, mesh, base).map: rule =>
      val out = new Array[Int](rule.faceCount)
      rule.write(out, 0, rule.faceCount)
      out
