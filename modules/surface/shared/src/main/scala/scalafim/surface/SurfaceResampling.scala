package scalafim.surface

/** Sphere checks and radius normalisation for registered spherical meshes (e.g. `?h.sphere.reg`, fsLR spheres). */
object SphereMesh:
  /** Approximately spherical about the origin: `min radius * tolerance > max radius` (neurotransform's test). */
  def isSphere(mesh: TriangleMesh, tolerance: Double = 1.001): Boolean =
    val radii = Array.tabulate(mesh.vertexCount)(i => radius(mesh.coordinates, i))
    radii.nonEmpty && radii.min > 0.0 && radii.min * tolerance > radii.max

  /** Project every vertex radially onto the sphere of `radius`; the topology is unchanged. */
  def withRadius(mesh: TriangleMesh, radius: Double = 100.0): Either[SurfaceError, TriangleMesh] =
    if !(radius.isFinite && radius > 0.0) then Left(SurfaceError.InvalidGeometry(s"sphere radius must be positive and finite, got $radius"))
    else if !isSphere(mesh) then Left(SurfaceError.InvalidGeometry("mesh is not approximately spherical about the origin"))
    else
      val out = new Array[Double](mesh.coordinates.length)
      var i = 0
      while i < mesh.vertexCount do
        val scale = radius / SphereMesh.radius(mesh.coordinates, i)
        out(3 * i) = mesh.coordinates(3 * i) * scale
        out(3 * i + 1) = mesh.coordinates(3 * i + 1) * scale
        out(3 * i + 2) = mesh.coordinates(3 * i + 2) * scale
        i += 1
      Right(TriangleMesh.fromArrays(out, mesh.faceIndices.clone()))

  private[surface] def radius(c: Array[Double], i: Int): Double =
    math.sqrt(c(3 * i) * c(3 * i) + c(3 * i + 1) * c(3 * i + 1) + c(3 * i + 2) * c(3 * i + 2))

/** A sparse resampling operator from moving-mesh vertex data to reference-mesh vertices: row `rows(k)` (reference
  * vertex) receives `vals(k)` times column `cols(k)` (moving vertex). Zero-based.
  */
final case class SurfaceResamplingPlan(
    rows: IArray[Int],
    cols: IArray[Int],
    vals: IArray[Double],
    referenceVertices: Int,
    movingVertices: Int,
    method: SurfaceResampling.Method
):
  def nonZeros: Int = vals.length

object SurfaceResampling:
  enum Method derives CanEqual:
    case Barycentric, Nearest

  /** Element: each output row sums to one (interpolation). Sum: each input column sums to one (mass preserving).
    * None: raw weights.
    */
  enum Normalization derives CanEqual:
    case Element, Sum, None

  /** Resample from `moving` onto `reference`. Spherical meshes are first projected to a common radius. */
  def plan(
      reference: TriangleMesh,
      moving: TriangleMesh,
      method: Method = Method.Barycentric,
      spherical: Boolean = true,
      radius: Double = 100.0
  ): Either[SurfaceError, SurfaceResamplingPlan] =
    for
      ref <- if spherical then SphereMesh.withRadius(reference, radius) else Right(reference)
      mov <- if spherical then SphereMesh.withRadius(moving, radius) else Right(moving)
    yield method match
      case Method.Nearest =>
        val index = VertexIndex(mov)
        val cols = Array.tabulate(ref.vertexCount)(i => index.nearest(ref.coordinates(3 * i), ref.coordinates(3 * i + 1), ref.coordinates(3 * i + 2)))
        SurfaceResamplingPlan(IArray.from(0 until ref.vertexCount), IArray.unsafeFromArray(cols), IArray.fill(ref.vertexCount)(1.0), ref.vertexCount, mov.vertexCount, method)
      case Method.Barycentric =>
        val locator = FaceLocator(mov)
        val vertices = VertexIndex(mov)
        val rows = Array.newBuilder[Int]
        val cols = Array.newBuilder[Int]
        val vals = Array.newBuilder[Double]
        var i = 0
        while i < ref.vertexCount do
          val (x, y, z) = (ref.coordinates(3 * i), ref.coordinates(3 * i + 1), ref.coordinates(3 * i + 2))
          locator.locate(x, y, z) match
            case Some((face, weights)) =>
              var k = 0
              while k < 3 do
                if weights(k) > 1e-10 then
                  rows += i
                  cols += mov.faceIndices(3 * face + k)
                  vals += weights(k)
                k += 1
            case scala.None =>
              rows += i
              cols += vertices.nearest(x, y, z)
              vals += 1.0
          i += 1
        SurfaceResamplingPlan(IArray.unsafeFromArray(rows.result()), IArray.unsafeFromArray(cols.result()), IArray.unsafeFromArray(vals.result()), ref.vertexCount, mov.vertexCount, method)

  /** Apply a plan to per-vertex values (moving -> reference), or its transpose (`inverse`, reference -> moving). */
  def apply(plan: SurfaceResamplingPlan, values: Array[Double], inverse: Boolean = false, normalization: Normalization = Normalization.Element): Either[SurfaceError, Array[Double]] =
    val (rows, cols, nOut, nIn) =
      if inverse then (plan.cols, plan.rows, plan.movingVertices, plan.referenceVertices)
      else (plan.rows, plan.cols, plan.referenceVertices, plan.movingVertices)
    if values.length != nIn then Left(SurfaceError.InvalidField(s"expected $nIn input values, got ${values.length}"))
    else
      val weights = normalization match
        case Normalization.None => plan.vals
        case Normalization.Element => normalised(rows, plan.vals, nOut)
        case Normalization.Sum => normalised(cols, plan.vals, nIn)
      val out = new Array[Double](nOut)
      var k = 0
      while k < weights.length do
        out(rows(k)) += weights(k) * values(cols(k))
        k += 1
      Right(out)

  private def normalised(groups: IArray[Int], vals: IArray[Double], size: Int): IArray[Double] =
    val totals = new Array[Double](size)
    var k = 0
    while k < vals.length do
      totals(groups(k)) += vals(k)
      k += 1
    IArray.tabulate(vals.length)(k => vals(k) / (if totals(groups(k)) == 0.0 then 1.0 else totals(groups(k))))

/** Uniform 3D grid over a mesh's vertices for nearest-vertex queries. */
private final class VertexIndex(mesh: TriangleMesh):
  private val grid = SpatialHash(mesh.coordinates, mesh.vertexCount, Array.tabulate(mesh.vertexCount)(i => (i, i, i)), singlePoints = true)

  def nearest(x: Double, y: Double, z: Double): Int =
    grid.nearestPoint(x, y, z)

/** Finds the triangle hit by the ray from the origin through a query point, with barycentric weights. Only triangles
  * facing the query (positive ray parameter) qualify, so the antipodal triangle of a sphere is never chosen.
  */
private final class FaceLocator(mesh: TriangleMesh):
  private val c = mesh.coordinates
  private val f = mesh.faceIndices
  private val grid = SpatialHash(c, mesh.vertexCount, Array.tabulate(mesh.faceCount)(i => (f(3 * i), f(3 * i + 1), f(3 * i + 2))), singlePoints = false)

  def locate(x: Double, y: Double, z: Double): Option[(Int, Array[Double])] =
    var best = -1
    var bestMin = Double.NegativeInfinity
    var bestWeights: Array[Double] = null
    grid.candidates(x, y, z).foreach: face =>
      hit(face, x, y, z).foreach: w =>
        val m = math.min(w(0), math.min(w(1), w(2)))
        if m > bestMin then
          bestMin = m
          best = face
          bestWeights = w
    Option.when(best >= 0 && bestMin >= -1e-6):
      val clamped = bestWeights.map(v => math.max(0.0, v))
      val total = clamped.sum
      (best, clamped.map(_ / total))

  /** Möller–Trumbore along the ray origin -> q; weights for (v0, v1, v2) when the hit has t > 0. */
  private def hit(face: Int, qx: Double, qy: Double, qz: Double): Option[Array[Double]] =
    val (a, b, d) = (f(3 * face), f(3 * face + 1), f(3 * face + 2))
    val (e1x, e1y, e1z) = (c(3 * b) - c(3 * a), c(3 * b + 1) - c(3 * a + 1), c(3 * b + 2) - c(3 * a + 2))
    val (e2x, e2y, e2z) = (c(3 * d) - c(3 * a), c(3 * d + 1) - c(3 * a + 1), c(3 * d + 2) - c(3 * a + 2))
    val (px, py, pz) = (qy * e2z - qz * e2y, qz * e2x - qx * e2z, qx * e2y - qy * e2x)
    val det = e1x * px + e1y * py + e1z * pz
    if math.abs(det) < 1e-15 then scala.None
    else
      val inv = 1.0 / det
      val (sx, sy, sz) = (-c(3 * a), -c(3 * a + 1), -c(3 * a + 2)) // ray origin (0,0,0) minus v0
      val u = (sx * px + sy * py + sz * pz) * inv
      val (tx, ty, tz) = (sy * e1z - sz * e1y, sz * e1x - sx * e1z, sx * e1y - sy * e1x)
      val v = (qx * tx + qy * ty + qz * tz) * inv
      val t = (e2x * tx + e2y * ty + e2z * tz) * inv
      Option.when(t > 0.0)(Array(1.0 - u - v, u, v))

/** Buckets items (vertices, or triangles by their bounding boxes) into a uniform grid over the mesh bounds. */
private final class SpatialHash(c: Array[Double], vertexCount: Int, items: Array[(Int, Int, Int)], singlePoints: Boolean):
  private val (lo, hi) =
    val lo = Array(Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity)
    val hi = Array(Double.NegativeInfinity, Double.NegativeInfinity, Double.NegativeInfinity)
    var i = 0
    while i < vertexCount do
      var k = 0
      while k < 3 do
        lo(k) = math.min(lo(k), c(3 * i + k))
        hi(k) = math.max(hi(k), c(3 * i + k))
        k += 1
      i += 1
    (lo, hi)
  private val cells = math.max(1, math.min(64, math.cbrt(items.length.toDouble / 4.0).toInt))
  private val size = Array.tabulate(3)(k => math.max((hi(k) - lo(k)) / cells, 1e-9))
  private val buckets = Array.fill(cells * cells * cells)(Vector.newBuilder[Int])

  private def cell(v: Double, k: Int): Int = math.max(0, math.min(cells - 1, ((v - lo(k)) / size(k)).toInt))
  private def key(i: Int, j: Int, k: Int): Int = (i * cells + j) * cells + k

  items.indices.foreach: n =>
    val (a, b, d) = items(n)
    val ids = if singlePoints then Vector(a) else Vector(a, b, d)
    val ranges = (0 until 3).map(k => (ids.map(v => cell(c(3 * v + k), k)).min, ids.map(v => cell(c(3 * v + k), k)).max))
    for i <- ranges(0)._1 to ranges(0)._2; j <- ranges(1)._1 to ranges(1)._2; k <- ranges(2)._1 to ranges(2)._2 do buckets(key(i, j, k)) += n

  private val built = buckets.map(_.result())

  /** Items in the query's cell and its neighbours, widening until something is found, then `extra` rings further. */
  def candidates(x: Double, y: Double, z: Double, extra: Int = 0): Vector[Int] =
    val (ci, cj, ck) = (cell(x, 0), cell(y, 1), cell(z, 2))
    def within(ring: Int): Vector[Int] =
      (for
        i <- math.max(0, ci - ring) to math.min(cells - 1, ci + ring)
        j <- math.max(0, cj - ring) to math.min(cells - 1, cj + ring)
        k <- math.max(0, ck - ring) to math.min(cells - 1, ck + ring)
      yield built(key(i, j, k))).flatten.distinct.toVector
    var ring = 1
    var found = within(ring)
    while found.isEmpty && ring <= cells do
      ring += 1
      found = within(ring)
    if extra > 0 && found.nonEmpty then within(ring + extra) else found

  def nearestPoint(x: Double, y: Double, z: Double): Int =
    // a closer point can sit one ring beyond the first non-empty one
    val pool = candidates(x, y, z, extra = 1)
    val all = if pool.isEmpty then items.indices.toVector else pool
    all.minBy: n =>
      val v = items(n)._1
      val (dx, dy, dz) = (c(3 * v) - x, c(3 * v + 1) - y, c(3 * v + 2) - z)
      dx * dx + dy * dy + dz * dz
