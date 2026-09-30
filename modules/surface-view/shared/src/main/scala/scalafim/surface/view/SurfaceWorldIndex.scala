package scalafim.surface.view

import scalafim.image.WorldPoint
import scalafim.surface.*

/** An immutable snapshot of one surface's transformed vertex coordinates.
  * Reuse for repeated world-space picks. Prepare again after changing mesh
  * coordinates or placement; mutating the source never changes this snapshot.
  * Vertex ids always refer to the original ordering captured at preparation.
  */
final class SurfaceWorldIndex private (
  val surface: SurfaceId,
  private val points: Array[Double],
  private val order: Array[Int]
):
  val vertexCount: Int = order.length
  /** Primitive retained storage, excluding object/array headers. */
  val retainedBytes: Long = points.length.toLong * 8L + order.length.toLong * 4L

  def nearestVertex(world: WorldPoint, maximumDistance: SurfaceLinkRadius): Either[SurfaceViewError, SurfaceSelection] =
    if !world.x.isFinite || !world.y.isFinite || !world.z.isFinite then
      Left(SurfaceViewError.InvalidWorldIndex("query coordinates must be finite"))
    else
      var best = Int.MaxValue
      var distance = Double.PositiveInfinity
      def queryCoordinate(axis: Int): Double =
        if axis == 0 then world.x else if axis == 1 then world.y else world.z
      def visit(from: Int, until: Int, depth: Int): Unit =
        if from < until then
          val middle = from + (until - from) / 2
          val vertex = order(middle)
          val offset = vertex * 3
          // hypot avoids overflow/underflow changing the nearest vertex.
          val d = math.hypot(math.hypot(points(offset) - world.x, points(offset + 1) - world.y), points(offset + 2) - world.z)
          if d < distance || (d == distance && vertex < best) then
            distance = d
            best = vertex
          val delta = queryCoordinate(depth % 3) - points(offset + depth % 3)
          if delta <= 0.0 then
            visit(from, middle, depth + 1)
            if math.abs(delta) <= distance then visit(middle + 1, until, depth + 1)
          else
            visit(middle + 1, until, depth + 1)
            if math.abs(delta) <= distance then visit(from, middle, depth + 1)
      visit(0, vertexCount, 0)
      if distance <= maximumDistance.value then Right(SurfaceSelection(surface, VertexId.unsafe(best)))
      else Left(SurfaceViewError.LinkDistanceExceeded(distance, maximumDistance.value))

object SurfaceWorldIndex:
  def prepare(surface: SurfaceId, geometry: SurfaceGeometry): Either[SurfaceViewError, SurfaceWorldIndex] =
    val count = geometry.vertexCount
    if count <= 0 then return Left(SurfaceViewError.InvalidWorldIndex("surface must contain vertices"))
    val points = new Array[Double](count * 3)
    var vertex = 0
    while vertex < count do
      val offset = vertex * 3
      if !geometry.mesh.coordinates(offset).isFinite || !geometry.mesh.coordinates(offset + 1).isFinite || !geometry.mesh.coordinates(offset + 2).isFinite then
        return Left(SurfaceViewError.InvalidWorldIndex(s"vertex $vertex has non-finite local coordinates"))
      // Affine construction already validates the homogeneous row. Match its
      // translation-first accumulation without allocating a Vector per vertex.
      var axis = 0
      while axis < 3 do
        var value = geometry.surfaceToWorld.matrix(axis, 3)
        var column = 0
        while column < 3 do
          value += geometry.surfaceToWorld.matrix(axis, column) * geometry.mesh.coordinates(offset + column)
          column += 1
        if !value.isFinite then
          return Left(SurfaceViewError.InvalidWorldIndex(s"vertex $vertex has non-finite world coordinates"))
        points(offset + axis) = value
        axis += 1
      vertex += 1
    val order = Array.tabulate(count)(identity)
    def less(a: Int, b: Int, axis: Int): Boolean =
      val x = points(a * 3 + axis)
      val y = points(b * 3 + axis)
      x < y || (x == y && a < b)
    def swap(a: Int, b: Int): Unit =
      val value = order(a)
      order(a) = order(b)
      order(b) = value
    def build(from: Int, until: Int, depth: Int): Unit =
      if until - from > 1 then
        val target = from + (until - from) / 2
        val axis = depth % 3
        var low = from
        var high = until - 1
        while low < high do
          val pivot = order(low + (high - low) / 2)
          var left = low
          var right = high
          while left <= right do
            while less(order(left), pivot, axis) do left += 1
            while less(pivot, order(right), axis) do right -= 1
            if left <= right then
              swap(left, right)
              left += 1
              right -= 1
          if target <= right then high = right
          else if target >= left then low = left
          else
            low = target
            high = target
        build(from, target, depth + 1)
        build(target + 1, until, depth + 1)
    build(0, count, 0)
    Right(new SurfaceWorldIndex(surface, points, order))
