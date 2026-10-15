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
  def nearest(actual: Vector[Double]): Either[ShapeChartError, Int] = nearestIn(actual, None)

  /** Restrict final routing to the decoder's frozen, already scored references. */
  private[profile] def nearestAmong(actual: Vector[Double], candidates: Vector[Int]): Either[ShapeChartError, Int] =
    require(candidates.nonEmpty && candidates.forall(i => i >= 0 && i < count))
    nearestIn(actual, Some(candidates))

  private def nearestIn(actual: Vector[Double], candidates: Option[Vector[Int]]): Either[ShapeChartError, Int] =
    chart.point(actual).map: _ =>
      var best = candidates match
        case None => 0
        case Some(nodes) => nodes.min
      var distance = Double.PositiveInfinity
      var index = 0
      val length = candidates match
        case None => count
        case Some(nodes) => nodes.length
      while index < length do
        val node = candidates match
          case None => index
          case Some(nodes) => nodes(index)
        var candidate = 0.0
        var axis = 0
        while axis < chart.dimension do
          val delta = (actual(axis) - coordinates(node)(axis)) / chart.width(axis)
          candidate += delta * delta
          axis += 1
        if candidate < distance || (candidate == distance && node < best) then
          best = node
          distance = candidate
        index += 1
      best

object TrialReferencePoints:
  def apply(chart: ShapeChart, coordinates: Vector[Vector[Double]]): TrialReferencePoints =
    require(coordinates.nonEmpty, "reference bank must be nonempty")
    require(coordinates.distinct.length == coordinates.length, "reference coordinates must be distinct")
    require(coordinates.forall(p => chart.point(p).isRight), "reference coordinates must lie inside the chart")
    new TrialReferencePoints(chart, coordinates)

  def fromGrid(grid: NodeGrid): TrialReferencePoints =
    apply(grid.chart, Vector.tabulate(grid.count)(i => grid.point(i).coordinates))

/** Opt-in decoder integration, with frozen routing and one shared full-jet bank.
  * Continuous decoder evaluations still construct exact, charged factors. This
  * policy is diagnostic and does not certify the bounded production work gate.
  * Final corrected readout routes within candidates, never to a third reference.
  */
final case class TrialReferenceDecodePolicy(
    points: TrialReferencePoints,
    candidates: Vector[Int],
    storage: TrialReferenceStorage = TrialReferenceStorage.ReconstructSecondBands):
  require(points.count <= 8, "decoder bank must contain at most eight references")
  require(candidates.nonEmpty && candidates.length <= 2, "routing must select one or two references")
  require(candidates.distinct.length == candidates.length, "routing references must be distinct")
  require(candidates.forall(i => i >= 0 && i < points.count), "routing reference must belong to the bank")

/** An explicit-bank objective over one mutable worker. No second bank or grid
  * is constructed; all numerical work remains in the supplied bank's ledger.
  */
final class TrialReferenceShapeObjective(
    val bank: TrialReferenceBank,
    val referenceCandidates: Vector[Int]) extends RoutedShapeObjective:
  // Reuse the public policy's invariant checks for direct adapter callers.
  private val checked = TrialReferenceDecodePolicy(bank.points, referenceCandidates, bank.storage)
  def chart: ShapeChart = checked.points.chart
  def referenceCount: Int = checked.points.count
  def referenceCoordinatesInto(node: Int, out: Array[Double]): Unit = checked.points.coordinatesInto(node, out)
  def amplitudeCount: Int = bank.amplitudeCount
  def scoreNode(node: Int): Double = bank.scoreNode(node)
  def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean = bank.jetAtNode(node, out)
  def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean = bank.jetAt(coordinates, out)
  def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double = bank.energyAt(coordinates, out)
