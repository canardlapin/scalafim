package scalafim.surface.view

import intaglio.*

/** Coordinates in the original scientific face, retained through display cuts. */
final case class SurfaceFaceWeights(a: Double, b: Double, c: Double):
  require(a.isFinite && b.isFinite && c.isFinite && a >= 0 && b >= 0 && c >= 0)
  require(math.abs(a + b + c - 1.0) <= 1e-12)

  def interpolate(other: SurfaceFaceWeights, t: Double): SurfaceFaceWeights =
    SurfaceFaceWeights((1 - t) * a + t * other.a, (1 - t) * b + t * other.b, (1 - t) * c + t * other.c)

final case class SurfaceMappingTriangle(
  sourceFace: Int,
  sourceVertices: (Int, Int, Int),
  a: SurfaceFaceWeights,
  b: SurfaceFaceWeights,
  c: SurfaceFaceWeights
):
  def weights(wa: Double, wb: Double, wc: Double): SurfaceFaceWeights =
    SurfaceFaceWeights(wa * a.a + wb * b.a + wc * c.a,
      wa * a.b + wb * b.b + wc * c.b, wa * a.c + wb * b.c + wc * c.c)
  def centroid: SurfaceFaceWeights = weights(1.0 / 3, 1.0 / 3, 1.0 / 3)
  /** Fraction of the original triangle area, independent of its embedding. */
  def areaFraction: Double = (b.b - a.b) * (c.c - a.c) - (b.c - a.c) * (c.b - a.b)

final case class SurfacePartitionBudget private (maxTriangles: Int, maxCutsPerFace: Int)
object SurfacePartitionBudget:
  def make(maxTriangles: Int, maxCutsPerFace: Int): Either[SurfacePartitionError, SurfacePartitionBudget] =
    if maxTriangles <= 0 || maxCutsPerFace < 0 then Left(SurfacePartitionError.InvalidBudget)
    else Right(new SurfacePartitionBudget(maxTriangles, maxCutsPerFace))

enum SurfacePartitionError:
  case InvalidBudget
  case TriangleBudgetExceeded(limit: Int)
  case CutBudgetExceeded(face: Int, requested: Int, limit: Int)
  case InvalidSamples(layer: SurfaceLayerId, expected: Int, actual: Int)

  def message: String = this match
    case InvalidBudget => "partition needs a positive triangle budget and nonnegative cut budget"
    case TriangleBudgetExceeded(limit) => s"mapping partition exceeds $limit triangles"
    case CutBudgetExceeded(face, requested, limit) => s"face $face needs $requested cuts; limit is $limit"
    case InvalidSamples(layer, expected, actual) => s"layer ${layer.value} needs $expected original samples; got $actual"

/** Partition at every scalar mapping break before allocating native textures.
  * This is geometric provenance, not a claim of native pixel accuracy. Nonfinite
  * scalar faces have a constant invalid interior and need no scalar cuts.
  */
object SurfaceMappingPartition:
  def boundaries(mapping: ScalarMapping): Vector[Double] =
    mapping.boundaries

  def build(mesh: SurfaceMeshPacket, layers: Vector[SurfaceLayerPacket], budget: SurfacePartitionBudget)
      : Either[SurfacePartitionError, Vector[SurfaceMappingTriangle]] =
    val fields = layers.filter(_.surface == mesh.surface).flatMap(layer => layer.scalarField.map(layer -> _))
    val originalCount = mesh.sampleNormals.getOrElse(mesh.normals).length / 3
    fields.find(_._2.samples.length != originalCount) match
      case Some((layer, field)) => return Left(SurfacePartitionError.InvalidSamples(layer.layer, originalCount, field.samples.length))
      case None => ()
    val breaks = fields.map((layer, field) => (layer.coverage, field, boundaries(field.mapping)))
    val output = Vector.newBuilder[SurfaceMappingTriangle]
    var triangleCount = 0
    var renderFace = 0
    while renderFace < mesh.indices.length / 3 do
      val face = mesh.sourceFace(renderFace)
      val vertices = mesh.sourceFaceVertices(renderFace)
      def original(a: Double, b: Double, c: Double): SurfaceFaceWeights =
        val weights = mesh.sourceBarycentric(renderFace, a, b, c)
        SurfaceFaceWeights(weights._1, weights._2, weights._3)
      var polygons = Vector(Vector(original(1, 0, 0), original(0, 1, 0), original(0, 0, 1)))
      var cutCount = 0
      var fieldIndex = 0
      while fieldIndex < breaks.length do
        val (coverage, field, cuts) = breaks(fieldIndex)
        val va = field.samples(vertices._1)
        val vb = field.samples(vertices._2)
        val vc = field.samples(vertices._3)
        if coverage.contains(face) && va.isFinite && vb.isFinite && vc.isFinite then
          val lo = math.min(va, math.min(vb, vc))
          val hi = math.max(va, math.max(vb, vc))
          var cutIndex = 0
          while cutIndex < cuts.length do
            val cut = cuts(cutIndex)
            if cut > lo && cut < hi then
              cutCount += 1
              if cutCount > budget.maxCutsPerFace then
                return Left(SurfacePartitionError.CutBudgetExceeded(face, cutCount, budget.maxCutsPerFace))
              val scale = math.max(math.abs(cut), math.max(math.abs(lo), math.abs(hi)))
              val da = va / scale - cut / scale
              val db = vb / scale - cut / scale
              val dc = vc / scale - cut / scale
              val next = Vector.newBuilder[Vector[SurfaceFaceWeights]]
              var remainingTriangles = 0L
              var polygonIndex = 0
              while polygonIndex < polygons.length do
                val pieces = split(polygons(polygonIndex), da, db, dc, va, vb, vc, cut, vertices)
                var pieceIndex = 0
                while pieceIndex < pieces.length do
                  val piece = pieces(pieceIndex)
                  remainingTriangles += piece.length - 2
                  if remainingTriangles + triangleCount > budget.maxTriangles then
                    return Left(SurfacePartitionError.TriangleBudgetExceeded(budget.maxTriangles))
                  next += piece
                  pieceIndex += 1
                polygonIndex += 1
              polygons = next.result()
            cutIndex += 1
        fieldIndex += 1
      var polygonIndex = 0
      while polygonIndex < polygons.length do
        val polygon = polygons(polygonIndex)
        var corner = 1
        while corner + 1 < polygon.length do
          val triangle = SurfaceMappingTriangle(face, vertices, polygon.head, polygon(corner), polygon(corner + 1))
          if triangle.areaFraction > 0.0 then
            if triangleCount == budget.maxTriangles then
              return Left(SurfacePartitionError.TriangleBudgetExceeded(budget.maxTriangles))
            output += triangle
            triangleCount += 1
          corner += 1
        polygonIndex += 1
      renderFace += 1
    Right(output.result())

  private def split(polygon: Vector[SurfaceFaceWeights], da: Double, db: Double, dc: Double,
      va: Double, vb: Double, vc: Double, cut: Double, vertices: (Int, Int, Int))
      : Vector[Vector[SurfaceFaceWeights]] =
    def distance(p: SurfaceFaceWeights): Double = p.a * da + p.b * db + p.c * dc
    val distances = polygon.map(distance)
    if !distances.exists(_ < 0) || !distances.exists(_ > 0) then Vector(polygon)
    else
      val lower = Vector.newBuilder[SurfaceFaceWeights]
      val upper = Vector.newBuilder[SurfaceFaceWeights]
      var i = 0
      while i < polygon.length do
        val j = (i + 1) % polygon.length
        val p = polygon(i)
        val dp = distances(i)
        val dq = distances(j)
        if dp <= 0 then lower += p
        if dp >= 0 then upper += p
        if (dp < 0 && dq > 0) || (dp > 0 && dq < 0) then
          // Share precisely the same intersection on both sides of the seam.
          val q = polygon(j)
          val zero = if p.a == 0 && q.a == 0 then 0 else if p.b == 0 && q.b == 0 then 1 else if p.c == 0 && q.c == 0 then 2 else -1
          val crossing = if zero < 0 then p.interpolate(q, dp / (dp - dq)) else
            // Both incident scientific faces must compute exactly the same
            // original-edge cut, independent of their third sample or winding.
            val ids = Vector(vertices._1, vertices._2, vertices._3)
            val values = Vector(va, vb, vc)
            val edge = (0 until 3).filter(_ != zero).sortBy(ids(_))
            val lo = edge(0)
            val hi = edge(1)
            val scale = math.max(math.abs(cut), math.max(math.abs(values(lo)), math.abs(values(hi))))
            val dl = values(lo) / scale - cut / scale
            val dh = values(hi) / scale - cut / scale
            val t = (dl / (dl - dh)).max(0).min(1)
            val weights = new Array[Double](3)
            weights(lo) = 1 - t
            weights(hi) = t
            SurfaceFaceWeights(weights(0), weights(1), weights(2))
          lower += crossing
          upper += crossing
        i += 1
      Vector(lower.result(), upper.result()).filter(_.length >= 3)
