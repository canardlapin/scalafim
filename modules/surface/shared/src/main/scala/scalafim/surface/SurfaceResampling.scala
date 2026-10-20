package scalafim.surface

import mesh4s.geometry.{NearestVertexIndex, NondegeneracyTolerance, RadialFaceIndex, SphereGeometry, SurfaceRealization}
import spatial4s.Point

/** Sphere checks and radius normalisation for registered spherical meshes (e.g. `?h.sphere.reg`, fsLR spheres). */
object SphereMesh:
  /** Approximately spherical about the origin: `min radius * tolerance > max radius` (neurotransform's test). */
  def isSphere(mesh: TriangleMesh, tolerance: Double = 1.001): Boolean =
    SphereGeometry.isSphere(mesh.points, tolerance)

  /** Project every vertex radially onto the sphere of `radius`; the topology is unchanged. */
  def withRadius(mesh: TriangleMesh, radius: Double = 100.0): Either[SurfaceError, TriangleMesh] =
    SphereGeometry.withRadius(mesh.points, radius, 1.001)
      .left.map(error => SurfaceError.InvalidGeometry(error.message))
      .map(points => mesh.withCoordinates(points.interleavedDoubles))

  private[surface] def radius(c: MeshCoordinates, i: Int): Double =
    SphereGeometry.radius(c.field, c.field.space.indexAtValidatedOrdinal(i))

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
      result <- method match
        case Method.Nearest =>
          val index = new NearestVertexIndex(mov.points)
          // Registered surface coordinates are deliberately interpreted in the
          // moving chart here. A shared topology alone never aligns frames.
          val cols = Array.tabulate(ref.vertexCount): i =>
            val query = Point.in(index.frame)(ref.coordinates(3 * i), ref.coordinates(3 * i + 1), ref.coordinates(3 * i + 2)).toOption.get
            index.nearest(query).get.ordinal // TriangleMesh requires a nonempty vertex domain.
          Right(SurfaceResamplingPlan(IArray.from(0 until ref.vertexCount), IArray.unsafeFromArray(cols), IArray.fill(ref.vertexCount)(1.0), ref.vertexCount, mov.vertexCount, method))
        case Method.Barycentric =>
          for
            realization <- SurfaceRealization.fromPointField(mov.topology, mov.points)
              .left.map(error => SurfaceError.InvalidGeometry(error.message))
            locator <- RadialFaceIndex.create(realization, NondegeneracyTolerance.scaleAware)
              .left.map(error => SurfaceError.InvalidGeometry(error.message))
          yield
            val vertices = new NearestVertexIndex(mov.points)
            val rows = Array.newBuilder[Int]
            val cols = Array.newBuilder[Int]
            val vals = Array.newBuilder[Double]
            var i = 0
            while i < ref.vertexCount do
              val (x, y, z) = (ref.coordinates(3 * i), ref.coordinates(3 * i + 1), ref.coordinates(3 * i + 2))
              val query = Point.in(locator.frame)(x, y, z).toOption.get
              locator.locate(query) match
                case Some(hit) =>
                  val face = hit.face.ordinal
                  val weights = Array(hit.weights.first, hit.weights.second, hit.weights.third)
                  var k = 0
                  while k < 3 do
                    if weights(k) > 1e-10 then
                      rows += i
                      cols += mov.faceIndices(3 * face + k)
                      vals += weights(k)
                    k += 1
                case scala.None =>
                  rows += i
                  cols += vertices.nearest(Point.in(vertices.frame)(x, y, z).toOption.get).get.ordinal
                  vals += 1.0
              i += 1
            SurfaceResamplingPlan(IArray.unsafeFromArray(rows.result()), IArray.unsafeFromArray(cols.result()), IArray.unsafeFromArray(vals.result()), ref.vertexCount, mov.vertexCount, method)
    yield result

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
