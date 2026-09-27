package scalafim.transform.scenarios

import image4s.geometry.{Affine, D3}
import scalafim.transform.oracle.OracleFixtures

/** Small numeric helpers shared by the registration-chain scenarios. They build references; they never call the
  * transform code under test.
  */
private[scenarios] object ChainScenarioSupport:
  /** `affine * v` for a 3-vector. */
  def affineAt(affine: Affine[D3], v: Vector[Double]): Vector[Double] =
    val m = affine.rowMajor
    Vector.tabulate(3)(r => m(4 * r) * v(0) + m(4 * r + 1) * v(1) + m(4 * r + 2) * v(2) + m(4 * r + 3))

  /** ITK's LPS millimetres <-> RAS millimetres (the flip is its own inverse). */
  def flipLps(v: Vector[Double]): Vector[Double] = Vector(-v(0), -v(1), v(2))

  def maxAbsDifference(actual: Vector[Double], expected: Vector[Double]): Double =
    actual.zip(expected).map((a, e) => math.abs(a - e)).maxOption.getOrElse(Double.NaN)

  /** The largest entry; NaN if the collection is empty or holds any NaN, so neither can pass a tolerance. */
  def worst(values: Iterable[Double]): Double =
    values
      .foldLeft(Option.empty[Double])((acc, v) => Some(acc.fold(v)(a => if a.isNaN || v.isNaN then Double.NaN else math.max(a, v))))
      .getOrElse(Double.NaN)

  /** The exact affine `y = A x + t` through point pairs, by least squares on the normal equations of `[x 1]`. With four
    * or more affinely independent pairs of an affine map this recovers it to rounding; the residual is returned so a
    * caller can check that the pairs really are affine.
    */
  def fitAffine(pairs: Vector[(Vector[Double], Vector[Double])]): (Vector[Double], Double) =
    require(pairs.size >= 4, s"an affine fit needs at least four point pairs, got ${pairs.size}")
    val rows = pairs.map((x, _) => x :+ 1.0)
    val gram = Vector.tabulate(4, 4)((i, j) => rows.map(r => r(i) * r(j)).sum)
    val coefficients = Vector.tabulate(3): out =>
      val rhs = Vector.tabulate(4)(i => rows.zip(pairs).map((r, p) => r(i) * p._2(out)).sum)
      solve(gram, rhs)
    val rowMajor = coefficients.flatten ++ Vector(0.0, 0.0, 0.0, 1.0)
    val residual = worst(pairs.map((x, y) => maxAbsDifference(Vector.tabulate(3)(r => (0 until 4).map(c => rowMajor(4 * r + c) * (x :+ 1.0)(c)).sum), y)))
    (rowMajor, residual)

  /** Gaussian elimination with partial pivoting for a small dense system. */
  def solve(matrix: Vector[Vector[Double]], rhs: Vector[Double]): Vector[Double] =
    val n = rhs.size
    val a = Array.tabulate(n, n + 1)((i, j) => if j < n then matrix(i)(j) else rhs(i))
    for col <- 0 until n do
      val pivot = (col until n).maxBy(r => math.abs(a(r)(col)))
      val tmp = a(col)
      a(col) = a(pivot)
      a(pivot) = tmp
      require(math.abs(a(col)(col)) > 1e-300, "singular system")
      for r <- col + 1 until n do
        val f = a(r)(col) / a(col)(col)
        for c <- col to n do a(r)(c) -= f * a(col)(c)
    val x = new Array[Double](n)
    for r <- (n - 1) to 0 by -1 do x(r) = (a(r)(n) - (r + 1 until n).map(c => a(r)(c) * x(c)).sum) / a(r)(r)
    x.toVector

  /** The SHA-256 a fixture manifest records for `file` (the `"name": "<hex>"` entry of its hash table). */
  def recordedSha256(manifestPath: String, file: String): Option[String] =
    val quoted = java.util.regex.Pattern.quote(s"\"$file\"")
    s"$quoted\\s*:\\s*\"([0-9a-f]{64})\"".r.findFirstMatchIn(OracleFixtures.text(manifestPath)).map(_.group(1))
