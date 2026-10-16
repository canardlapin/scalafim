package scalafim.surface.view.raster

import intaglio.*
import scalafim.surface.view.*

final class SurfaceDecoratedRasterResult private[raster] (
  val image: RasterImage,
  val scientific: SurfaceRasterResult,
  hits: Array[Option[SurfaceDecorationHit]]
):
  def pick(x: Int, y: Int): Either[SurfaceRasterError, Option[SurfaceDisplayHit[SurfacePick]]] =
    scientific.pick(x, y).map: hit =>
      hits(y * image.width + x).map(SurfaceDisplayHit.Decoration(_))
        .orElse(hit.map(SurfaceDisplayHit.Scientific(_)))

/** Decoration geometry has its own identities and buffers but shares the
  * composition's projection, clipping and depth domains. Transparent texels do
  * not write depth or intercept scientific picks.
  */
object SurfaceDecorationRaster:
  import SurfaceRasterizer.*
  private final case class Fragment(mesh: Int, depth: Double, color: Rgba32)

  def render(plan: SurfaceCompositionPlan, geometry: SurfaceDecorationGeometry,
      dimensions: RasterDimensions, style: SurfaceRasterStyle = SurfaceRasterStyle()): Either[SurfaceRasterError, SurfaceDecoratedRasterResult] =
    renderWithCollectionPolicy(plan, geometry, dimensions, style, DefaultFragmentCapacity, explicitCapacity = false)

  /** Decorations and scientific surfaces consume one shared contribution
    * budget, so a decorated render cannot bypass the visibility cap. */
  def renderChecked(plan: SurfaceCompositionPlan, geometry: SurfaceDecorationGeometry,
      dimensions: RasterDimensions, style: SurfaceRasterStyle,
      fragmentCapacity: Int): Either[SurfaceRasterError, SurfaceDecoratedRasterResult] =
    renderWithCollectionPolicy(plan, geometry, dimensions, style, fragmentCapacity, explicitCapacity = true)

  private def renderWithCollectionPolicy(plan: SurfaceCompositionPlan, geometry: SurfaceDecorationGeometry,
      dimensions: RasterDimensions, style: SurfaceRasterStyle, fragmentCapacity: Int,
      explicitCapacity: Boolean): Either[SurfaceRasterError, SurfaceDecoratedRasterResult] =
    SurfaceDecorationValidation.validate(geometry, plan, dimensions.width, dimensions.height)
      .left.map(SurfaceRasterError.InvalidPlan(_)).flatMap: _ =>
        if geometry.meshes.exists(_.depthPolicy == SurfaceDecorationDepthPolicy.OrderedDepthTest) && style.surfaceBase.alpha != 255 then
          Left(SurfaceRasterError.InvalidPlan("ordered depth-tested decorations require an opaque surface base"))
        else if geometry.meshes.isEmpty && geometry.labels.isEmpty then
          SurfaceRasterizer.renderCompositionChecked(plan, dimensions, style, fragmentCapacity,
            forceCollection = false, allowLegacyFastPath = false, explicitFragmentCapacity = explicitCapacity).map: surface =>
            new SurfaceDecoratedRasterResult(surface.image, surface, Array.fill(dimensions.pixelCount)(None))
        else SurfaceRasterizer.renderCompositionChecked(plan, dimensions, style, fragmentCapacity,
          forceCollection = false, allowLegacyFastPath = false, explicitFragmentCapacity = explicitCapacity,
          retainOpaqueWinners = true).flatMap: surface =>
          val count = dimensions.width * dimensions.height
          var contributionCount = surface.collected.fold(0)(_.iterator.map(_.length).sum)
          var overflow = false
          val pixels = Array.tabulate(count)(i => surface.image.pixelUnsafe(i % dimensions.width, i / dimensions.width).toPackedInt)
          val fragments = Array.fill(count)(Vector.empty[Fragment])
          val hits = Array.fill[Option[SurfaceDecorationHit]](count)(None)
          val depths = Array.fill(count)(Double.PositiveInfinity)
          val colors = new Array[Int](count)
          val touched = scala.collection.mutable.ArrayBuffer.empty[Int]
          geometry.meshes.zipWithIndex.foreach: (mesh, index) =>
            val view = plan.views.find(_.id == mesh.origin.view).get
            val box = plan.fittedViewports(dimensions.width, dimensions.height)
              .find(_._1.id == view.id).get._2
            // One frontmost contribution per mesh and pixel prevents a shared
            // triangle edge from blending the same glyph texel twice.
            val sampler = mesh.texture.map(t => new SurfaceTextureSampler(t.image))
            var face = 0
            while face < mesh.indices.length / 3 do
              val corners = Vector.tabulate(3): corner =>
                val vertex = mesh.indices(face * 3 + corner)
                val offset = vertex * 3
                val x = mesh.positions(offset).toDouble
                val y = mesh.positions(offset + 1).toDouble
                val z = mesh.positions(offset + 2).toDouble
                val eye = multiply(view.camera.viewMatrix, x, y, z, 1)
                val clip = multiply(view.camera.projectionMatrix, eye._1, eye._2, eye._3, eye._4)
                val uv = mesh.texture.map(_.coordinates)
                ClipVertex(x, y, z, clip._1, clip._2, clip._3, clip._4, 0, 0, 0, 0,
                  uv.fold(0.0)(_(vertex * 2).toDouble), uv.fold(0.0)(_(vertex * 2 + 1).toDouble), 0)
              val polygon = clipTriangle(corners, view.clipping)
              var triangle = 1
              while triangle + 1 < polygon.length do
                val a = toScreen(polygon.head, box, dimensions)
                val b = toScreen(polygon(triangle), box, dimensions)
                val c = toScreen(polygon(triangle + 1), box, dimensions)
                draw(a, b, c, mesh, sampler, dimensions, depths, colors, touched)
                triangle += 1
              face += 1
            touched.foreach: pixel =>
              // The scientific result retains its original fragments. Keeping
              // this depth-tested decoration here (rather than testing only
              // the nearest scientific depth) lets an opaque surface hide it
              // while a translucent one correctly reveals it.
              val retainedFragments = surface.collected.nonEmpty ||
                mesh.depthPolicy == SurfaceDecorationDepthPolicy.Overlay ||
                depths(pixel) <= surface.depthAt(pixel) + 1e-14
              if retainedFragments then
                if contributionCount == fragmentCapacity then overflow = true
                else
                  fragments(pixel) = fragments(pixel) :+ Fragment(index, depths(pixel), Rgba32.fromPackedInt(colors(pixel)))
                  contributionCount += 1
              depths(pixel) = Double.PositiveInfinity
            touched.clear()
          var pixel = 0
          while pixel < count && !overflow do
            val decorations = fragments(pixel)
            // Untouched pixels already contain the resolved scientific stack.
            if decorations.nonEmpty then
              val ordinary = decorations.filter(fragment =>
                geometry.meshes(fragment.mesh).depthPolicy == SurfaceDecorationDepthPolicy.DepthTest)
              // Only ordinary opaque solids write depth. Ordered strokes and
              // textured labels leave the established scene depth unchanged.
              val establishedDepth = ordinary.iterator.filter(fragment =>
                geometry.meshes(fragment.mesh).texture.isEmpty && fragment.color.alpha == 255)
                .foldLeft(surface.depthAt(pixel))((depth, fragment) => math.min(depth, fragment.depth))
              val strokes = decorations.filter(fragment =>
                geometry.meshes(fragment.mesh).depthPolicy == SurfaceDecorationDepthPolicy.OrderedDepthTest &&
                  fragment.depth <= establishedDepth + 1e-14).sortBy(_.mesh)
              val overlays = decorations.filter(fragment =>
                geometry.meshes(fragment.mesh).depthPolicy == SurfaceDecorationDepthPolicy.Overlay).sortBy(_.mesh)
              val ordered = ordinary.sortBy(fragment => (-fragment.depth, fragment.mesh)) ++ strokes ++ overlays
              surface.collected match
                case Some(scientific) =>
                  val depthTested = ordinary.iterator.map(collected)
                  // At exactly equal depth, a decoration is the final visual
                  // contribution. Use the domain rank explicitly; resource IDs
                  // must not decide whether a marker is visible.
                  val base = (scientific(pixel).iterator.map(value => (value, 0)) ++
                    depthTested.map(value => (value, 1))).toVector
                    .sortBy { case (value, domain) => (-value.depth, domain, value.tie, value.face, value.vertex) }
                    .map(_._1)
                  val all = base ++ (strokes ++ overlays).map(collected)
                  if all.nonEmpty then
                    pixels(pixel) = compositeStack(style.background, all)
                case None =>
                  ordered.foreach: fragment =>
                    val color = fragment.color
                    pixels(pixel) = compositeOverOpaque(pixels(pixel), color.red, color.green, color.blue, color.alpha)
              ordered.foreach: fragment =>
                val mesh = geometry.meshes(fragment.mesh)
                val frontVisible = mesh.depthPolicy == SurfaceDecorationDepthPolicy.Overlay ||
                  fragment.depth <= establishedDepth + 1e-14
                if mesh.pickable && frontVisible then
                  hits(pixel) = Some(SurfaceDecorationHit(mesh.id, mesh.origin, mesh.target, fragment.depth))
            pixel += 1
          if overflow then Left(SurfaceRasterError.FragmentCapacityExceeded(fragmentCapacity))
          else Right(new SurfaceDecoratedRasterResult(RasterImage.unsafeFromOwnedPackedArray(dimensions, pixels), surface, hits))

  private def collected(fragment: Fragment): SurfaceCollectedFragment =
    val color = fragment.color
    SurfaceCollectedFragment(fragment.depth, color.red, color.green, color.blue, color.alpha / 255.0,
      -1, -1, -1, 0f, 0f, 0f, s"decoration-${fragment.mesh}", false)

  private def draw(a: ScreenVertex, b: ScreenVertex, c: ScreenVertex, mesh: SurfaceDecorationMesh, sampler: Option[SurfaceTextureSampler],
      dimensions: RasterDimensions, depths: Array[Double], colors: Array[Int], touched: scala.collection.mutable.ArrayBuffer[Int]): Unit =
    val area = edge(a.x, a.y, b.x, b.y, c.x, c.y)
    if math.abs(area) > 1e-14 && area.isFinite then
      val minX = math.max(0, math.floor(math.min(a.x, math.min(b.x, c.x))).toInt)
      val maxX = math.min(dimensions.width - 1, math.ceil(math.max(a.x, math.max(b.x, c.x))).toInt)
      val minY = math.max(0, math.floor(math.min(a.y, math.min(b.y, c.y))).toInt)
      val maxY = math.min(dimensions.height - 1, math.ceil(math.max(a.y, math.max(b.y, c.y))).toInt)
      var y = minY
      while y <= maxY do
        var x = minX
        while x <= maxX do
          val wa = edge(b.x, b.y, c.x, c.y, x + .5, y + .5) / area
          val wb = edge(c.x, c.y, a.x, a.y, x + .5, y + .5) / area
          val wc = 1 - wa - wb
          if wa >= -1e-12 && wb >= -1e-12 && wc >= -1e-12 then
            val depth = wa * a.depth + wb * b.depth + wc * c.depth
            val pixel = y * dimensions.width + x
            if depth < depths(pixel) then
              val color = mesh.texture match
                case None => mesh.color
                case Some(texture) =>
                  val invW = wa * a.inverseW + wb * b.inverseW + wc * c.inverseW
                  val u = (wa * a.baryAOverW + wb * b.baryAOverW + wc * c.baryAOverW) / invW
                  val v = (wa * a.baryBOverW + wb * b.baryBOverW + wc * c.baryBOverW) / invW
                  val daDx = (c.y - b.y) / area
                  val dbDx = (a.y - c.y) / area
                  val daDy = (b.x - c.x) / area
                  val dbDy = (c.x - a.x) / area
                  def derivative(da: Double, db: Double, va: Double, vb: Double, vc: Double, value: Double): Double =
                    val dc = -da - db
                    val dq = da * a.inverseW + db * b.inverseW + dc * c.inverseW
                    (da * va + db * vb + dc * vc - value * dq) / invW
                  sampler.get.sample(u, v,
                    derivative(daDx, dbDx, a.baryAOverW, b.baryAOverW, c.baryAOverW, u),
                    derivative(daDy, dbDy, a.baryAOverW, b.baryAOverW, c.baryAOverW, u),
                    derivative(daDx, dbDx, a.baryBOverW, b.baryBOverW, c.baryBOverW, v),
                    derivative(daDy, dbDy, a.baryBOverW, b.baryBOverW, c.baryBOverW, v))
              if color.alpha > 0 then
                if !depths(pixel).isFinite then touched += pixel
                depths(pixel) = depth
                colors(pixel) = color.toPackedInt
          x += 1
        y += 1

