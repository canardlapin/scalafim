package scalafim.surface.view

import intaglio.*

/** Resolves decoration hits against the same view ray as a native scientific
  * hit. Native pickers often choose the scientific face on a coplanar tie even
  * though the later decoration draw wins; this gives that tie a stable policy.
  */
object SurfaceDecorationPicking:
  final case class Projected(x: Double, y: Double, depth: Double)

  def project(plan: SurfaceCompositionPlan, viewId: SurfaceViewId, point: SurfaceDecorationWorldPoint,
      width: Int, height: Int): Option[Projected] =
    plan.views.find(_.id == viewId).flatMap: view =>
      val m = view.camera.viewMatrix
      val p = view.camera.projectionMatrix
      val x = m(0) * point.x + m(1) * point.y + m(2) * point.z + m(3)
      val y = m(4) * point.x + m(5) * point.y + m(6) * point.z + m(7)
      val z = m(8) * point.x + m(9) * point.y + m(10) * point.z + m(11)
      val w = p(14) * z + p(15)
      if w <= 0 || !w.isFinite then None
      else
        val box = view.viewportFit.resolve(Vector(SurfaceViewSlot(view.instances.head.surface, view.viewport)), width, height).head.viewport
        Some(Projected((box.x + (p(0) * x / w + 1) * box.width / 2) * width,
          (box.y + (1 - p(5) * y / w) * box.height / 2) * height, ((p(10) * z + p(11)) / w + 1) / 2))

  def pick(geometry: SurfaceDecorationGeometry, plan: SurfaceCompositionPlan, x: Double, y: Double,
      scientificDepth: Option[Double]): Option[SurfaceDecorationHit] =
    val width = geometry.canvasWidth
    val height = geometry.canvasHeight
    val selected = plan.views.find: view =>
      val box = view.viewportFit.resolve(Vector(SurfaceViewSlot(view.instances.head.surface, view.viewport)), width, height).head.viewport
      x >= box.x * width && x < (box.x + box.width) * width && y >= box.y * height && y < (box.y + box.height) * height
    selected.flatMap: view =>
      val box = view.viewportFit.resolve(Vector(SurfaceViewSlot(view.instances.head.surface, view.viewport)), width, height).head.viewport
      val nx = 2 * (x / width - box.x) / box.width - 1
      val ny = 1 - 2 * (y / height - box.y) / box.height
      val m = view.camera.viewMatrix
      val p = view.camera.projectionMatrix
      val right = V(m(0), m(1), m(2))
      val up = V(m(4), m(5), m(6))
      val back = V(m(8), m(9), m(10))
      val eye = right * -m(3).toDouble + up * -m(7).toDouble + back * -m(11).toDouble
      val orthographic = p(15) == 1f
      val origin = if orthographic then eye + right * (nx / p(0)) + up * (ny / p(5)) else eye
      val direction = if orthographic then back * -1 else right * (nx / p(0)) + up * (ny / p(5)) - back
      var winner: Option[(SurfaceDecorationHit, Boolean)] = None
      geometry.meshes.foreach: mesh =>
        if mesh.origin.view == view.id && mesh.pickable then
          var face = 0
          while face < mesh.indices.length / 3 do
            val ia = mesh.indices(face * 3)
            val ib = mesh.indices(face * 3 + 1)
            val ic = mesh.indices(face * 3 + 2)
            def vertex(i: Int) = V(mesh.positions(i * 3), mesh.positions(i * 3 + 1), mesh.positions(i * 3 + 2))
            intersect(origin, direction, vertex(ia), vertex(ib), vertex(ic)).foreach: (t, b, c) =>
              val point = origin + direction * t
              val clipW = p(14) * (m(8) * point.x + m(9) * point.y + m(10) * point.z + m(11)) + p(15)
              val eyeZ = m(8) * point.x + m(9) * point.y + m(10) * point.z + m(11)
              val depth = ((p(10) * eyeZ + p(11)) / clipW + 1) / 2
              val worldVisible = view.clipping match
                case SurfaceClipping.WorldPlanes(planes) => planes.forall(_.orientedDistance(point.x, point.y, point.z) >= 0)
                case _ => true
              val alpha = mesh.texture match
                case None => mesh.color.alpha
                case Some(texture) =>
                  val u = texture.coordinates(ia * 2) * (1 - b - c) + texture.coordinates(ib * 2) * b + texture.coordinates(ic * 2) * c
                  val v = texture.coordinates(ia * 2 + 1) * (1 - b - c) + texture.coordinates(ib * 2 + 1) * b + texture.coordinates(ic * 2 + 1) * c
                  texture.image.pixelUnsafe((u * texture.image.width).toInt.max(0).min(texture.image.width - 1),
                    (v * texture.image.height).toInt.max(0).min(texture.image.height - 1)).alpha
              val overlay = mesh.depthPolicy == SurfaceDecorationDepthPolicy.Overlay
              val visible = clipW > 0 && depth >= 0 && depth <= 1 && worldVisible && alpha > 0 &&
                (overlay || scientificDepth.forall(depth <= _ + 1e-14))
              if visible && winner.forall: (hit, isOverlay) =>
                  (overlay && !isOverlay) || (overlay == isOverlay && depth <= hit.depth)
              then winner = Some(SurfaceDecorationHit(mesh.id, mesh.origin, mesh.target, depth) -> overlay)
            face += 1
      winner.map(_._1)

  private final case class V(x: Double, y: Double, z: Double):
    def +(v: V): V = V(x + v.x, y + v.y, z + v.z)
    def -(v: V): V = V(x - v.x, y - v.y, z - v.z)
    def *(n: Double): V = V(x * n, y * n, z * n)
    def dot(v: V): Double = x * v.x + y * v.y + z * v.z
    def cross(v: V): V = V(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)

  private def intersect(origin: V, direction: V, a: V, b: V, c: V): Option[(Double, Double, Double)] =
    val ab = b - a
    val ac = c - a
    val cross = direction.cross(ac)
    val determinant = ab.dot(cross)
    if math.abs(determinant) < 1e-12 then None
    else
      val ap = origin - a
      val wb = ap.dot(cross) / determinant
      val q = ap.cross(ab)
      val wc = direction.dot(q) / determinant
      val t = ac.dot(q) / determinant
      if t >= 0 && wb >= -1e-9 && wc >= -1e-9 && wb + wc <= 1 + 1e-9 then Some((t, wb, wc)) else None
