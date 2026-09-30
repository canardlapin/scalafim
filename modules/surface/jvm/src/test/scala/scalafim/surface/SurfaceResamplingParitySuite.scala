package scalafim.surface

import scala.io.Source

/** Parity with neurotransform 0.1.0's surface_resampling_plan on the same icospheres (resources/resampling_oracle). */
class SurfaceResamplingParitySuite extends munit.FunSuite:
  private def lines(name: String): Vector[String] =
    val stream = getClass.getResourceAsStream(s"/scalafim/surface/resampling_oracle/$name")
    assert(stream != null, s"missing $name")
    try Source.fromInputStream(stream, "UTF-8").getLines().filter(_.nonEmpty).toVector
    finally stream.close()

  private def mesh(prefix: String): TriangleMesh =
    TriangleMesh.fromRows(
      lines(s"${prefix}_vertices.tsv").map(_.split("\t").toVector.map(_.toDouble)),
      lines(s"${prefix}_faces.tsv").map(_.split("\t").map(_.toDouble.toInt)).map(a => (a(0), a(1), a(2)))
    )

  private def triplets(name: String): Vector[(Int, Int, Double)] =
    lines(name).drop(1).map(_.split("\t")).map(a => (a(0).toInt, a(1).toInt, a(2).toDouble))

  private val moving = mesh("moving")
  private val reference = mesh("reference")

  test("nearest-vertex plans are identical"):
    val ours = SurfaceResampling.plan(reference, moving, SurfaceResampling.Method.Nearest).fold(e => fail(e.message), identity)
    val theirs = triplets("plan_nearest.tsv").sortBy(_._1)
    assertEquals(ours.cols.toVector, theirs.map(_._2))

  test("barycentric plans pick the same face as neurotransform unless it picks the far side of the sphere"):
    // neurotransform projects each query orthogonally onto candidate triangle planes; ScalaFIM casts the radial ray.
    // Both select the same containing triangle on the near side, with weights differing only by that projection.
    val ours = SurfaceResampling.plan(reference, moving).fold(e => fail(e.message), identity)
    val theirs = triplets("plan_barycentric.tsv").groupBy(_._1)
    val mine = ours.rows.indices.groupBy(ours.rows(_)).view.mapValues(_.map(k => (ours.cols(k), ours.vals(k))).toMap).toMap
    val movingOnSphere = SphereMesh.withRadius(moving).fold(e => fail(e.message), identity)
    val referenceOnSphere = SphereMesh.withRadius(reference).fold(e => fail(e.message), identity)
    def nearSide(row: Int, weights: Seq[(Int, Double)]): Boolean =
      val q = Vector.tabulate(3)(k => referenceOnSphere.coordinates(3 * row + k))
      val hit = Vector.tabulate(3)(k => weights.map((c, w) => w * movingOnSphere.coordinates(3 * c + k)).sum)
      q.zip(hit).map(_ * _).sum > 0
    var sameFace = 0
    var farSide = 0
    var differentFace = 0
    var fallback = 0
    theirs.foreach: (row, entries) =>
      val weights = entries.map(e => (e._2, e._3))
      if !nearSide(row, weights) then farSide += 1
      else if weights.size == 1 && weights.head._2 == 1.0 then
        // neurotransform's orthogonal projection missed every triangle and fell back to the nearest vertex,
        // which must be one of the vertices of the triangle ScalaFIM's ray hits.
        assert(mine(row).contains(weights.head._1), s"reference vertex $row: fallback vertex ${weights.head._1} is not on ScalaFIM's face ${mine(row).keySet}")
        fallback += 1
      else if weights.map(_._1).toSet.subsetOf(mine(row).keySet) || mine(row).keySet.subsetOf(weights.map(_._1).toSet) then
        weights.foreach((c, w) => assertEqualsDouble(mine(row).getOrElse(c, 0.0), w, 0.05, s"reference vertex $row, moving vertex $c"))
        sameFace += 1
      else differentFace += 1
    assertEquals(differentFace, 0, "near-side rows must select the same triangle")
    assert(sameFace > 0)
    println(s"surface resampling parity: $sameFace rows share neurotransform's face; $fallback neurotransform nearest-vertex fallbacks; far-side faces chosen by neurotransform: $farSide of ${theirs.size} rows")
