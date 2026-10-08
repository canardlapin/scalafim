package scalafim.fmri.fit.profile

import scalafim.fmri.hrf.family.{ShapeChart, ShapeChartError}

/** Retained reference storage policy; both alternatives preserve full jets.
  * Reconstruction trades six bands per 3D reference for one band per worker.
  * Setup still constructs a full reference before discarding second bands.
  */
enum TrialReferenceStorage:
  case FullJets, ReconstructSecondBands

/** An ordered, explicit reference bank. It has no grid/interpolation semantics.
  * Count limits belong to the caller's work budget, not the numeric container.
  */
final class TrialReferencePoints private (
    val chart: ShapeChart, val coordinates: Vector[Vector[Double]]):
  val count: Int = coordinates.length

  def coordinatesInto(node: Int, out: Array[Double]): Unit =
    require(node >= 0 && node < count && out.length == chart.dimension)
    var axis = 0
    while axis < out.length do
      out(axis) = coordinates(node)(axis)
      axis += 1

  /** Coordinate-only normalized Euclidean routing, smallest-index exact tie. */
  def nearest(actual: Vector[Double]): Either[ShapeChartError, Int] =
    chart.point(actual).map: _ =>
      var best = 0
      var distance = Double.PositiveInfinity
      var node = 0
      while node < count do
        var candidate = 0.0
        var axis = 0
        while axis < chart.dimension do
          val delta = (actual(axis) - coordinates(node)(axis)) / chart.width(axis)
          candidate += delta * delta
          axis += 1
        if candidate < distance then
          best = node
          distance = candidate
        node += 1
      best

object TrialReferencePoints:
  def apply(chart: ShapeChart, coordinates: Vector[Vector[Double]]): TrialReferencePoints =
    require(coordinates.nonEmpty, "reference bank must be nonempty")
    require(coordinates.distinct.length == coordinates.length, "reference coordinates must be distinct")
    require(coordinates.forall(p => chart.point(p).isRight), "reference coordinates must lie inside the chart")
    new TrialReferencePoints(chart, coordinates)

  def fromGrid(grid: NodeGrid): TrialReferencePoints =
    apply(grid.chart, Vector.tabulate(grid.count)(i => grid.point(i).coordinates))
