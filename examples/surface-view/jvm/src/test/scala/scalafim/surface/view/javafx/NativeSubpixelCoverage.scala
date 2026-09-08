package scalafim.surface.view.javafx

import intaglio.*

/** Test-only classification of observed native 1/256-pixel vertex quantization.
  * It never excuses a color mismatch or an unexplained visibility change.
  */
private[javafx] object NativeSubpixelCoverage:
  final case class Point(x: Double, y: Double, depth: Double)
  final case class Evidence(maximumVertexShift: Double, nativeDepth: Double,
      referenceDepth: Double, maximumColorError: Int)

  /** The entire pixel square, including a quantization guard, lies in one face. */
  def containsPixel(points: Vector[Point], x: Int, y: Int): Boolean =
    val guard = 1.0 / 256
    points.length == 3 && points.forall(p => p.x.isFinite && p.y.isFinite &&
      p.depth.isFinite && p.depth >= 0 && p.depth <= 1) &&
      Vector((x - guard, y - guard), (x + 1 + guard, y - guard),
        (x - guard, y + 1 + guard), (x + 1 + guard, y + 1 + guard)).forall: (px, py) =>
        weights(points, px, py).exists(_.forall(_ >= 0))

  def certify(points: Vector[Point], x: Double, y: Double, referenceDepth: Double,
      referenceFace: Int, nativeFace: Int, cell: Rgba32, actual: Rgba32): Option[Evidence] =
    if referenceFace == nativeFace || referenceFace < 0 || nativeFace < 0 then None
    else newlyCovered(points, x, y, referenceDepth, cell, actual).filter(_.nativeDepth < referenceDepth)

  /** The same scientific face remains visible, but raster quantization selects
    * a different nearest-sample cell. This is a cell-boundary classification,
    * not an occlusion exception: both original face IDs must be equal and both
    * original vertex IDs must be valid and different. The GPU cell must newly
    * cover the center under the same 1/256 rule and explain the actual color.
    */
  def certifyNearestBoundary(points: Vector[Point], x: Double, y: Double, referenceDepth: Double,
      referenceFace: Int, nativeFace: Int, referenceVertex: Int, nativeVertex: Int,
      cell: Rgba32, actual: Rgba32): Option[Evidence] =
    if referenceFace < 0 || referenceFace != nativeFace || referenceVertex < 0 || nativeVertex < 0 ||
        referenceVertex == nativeVertex then None
    else newlyCovered(points, x, y, referenceDepth, cell, actual).filter: _ =>
      // Extrapolate the unrounded cell plane at the exact center. The same
      // scientific face must agree to two float32 depth units; movement of its
      // raster boundary cannot excuse an unrelated foreground/background face.
      weights(points, x, y).exists: w =>
        val exactDepth = w.zip(points).map((weight, p) => weight * p.depth).sum
        math.abs(exactDepth - referenceDepth) <= 2 * math.ulp(referenceDepth.toFloat).toDouble

  private def newlyCovered(points: Vector[Point], x: Double, y: Double, referenceDepth: Double,
      cell: Rgba32, actual: Rgba32): Option[Evidence] =
    if points.length != 3 || !x.isFinite || !y.isFinite || !referenceDepth.isFinite ||
        referenceDepth < 0 || referenceDepth > 1 ||
        points.exists(p => !p.x.isFinite || !p.y.isFinite || !p.depth.isFinite || p.depth < 0 || p.depth > 1) then None
    else
      val quantized = points.map(p => p.copy(x = math.rint(p.x * 256) / 256, y = math.rint(p.y * 256) / 256))
      for
        exact <- weights(points, x, y)
        rounded <- weights(quantized, x, y)
        if exact.exists(_ < 0) && rounded.forall(_ >= 0)
        depth = rounded.zip(points).map((w, p) => w * p.depth).sum
        error = Vector(math.abs(cell.red - actual.red), math.abs(cell.green - actual.green),
          math.abs(cell.blue - actual.blue), math.abs(cell.alpha - actual.alpha)).max
        if error <= 1
      yield Evidence(points.zip(quantized).map((p, q) => math.max(math.abs(p.x - q.x), math.abs(p.y - q.y))).max,
        depth, referenceDepth, error)

  private def weights(points: Vector[Point], x: Double, y: Double): Option[Vector[Double]] =
    val a = points(0)
    val b = points(1)
    val c = points(2)
    val denominator = (b.y - c.y) * (a.x - c.x) + (c.x - b.x) * (a.y - c.y)
    if denominator == 0 || !denominator.isFinite then None
    else
      val wa = ((b.y - c.y) * (x - c.x) + (c.x - b.x) * (y - c.y)) / denominator
      val wb = ((c.y - a.y) * (x - c.x) + (a.x - c.x) * (y - c.y)) / denominator
      val result = Vector(wa, wb, 1 - wa - wb)
      Option.when(result.forall(_.isFinite))(result)
