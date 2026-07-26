package scalafim.connectivity

import scalafim.graph.Direction
import scalafim.graph.Graph
import scalafim.graph.GraphBuildErrors
import scalafim.graph.VertexBasis

opaque type EdgeSpaceIx = Int

object EdgeSpaceIx:
  def from(value: Int, edgeSpace: EdgeSpace): Either[ConnectivityError, EdgeSpaceIx] =
    if value >= 0 && value < edgeSpace.size then Right(value)
    else Left(ConnectivityError.InvalidPlan(s"edge-space index $value out of bounds for size ${edgeSpace.size}"))

  private[connectivity] def unsafe(value: Int): EdgeSpaceIx =
    value

  extension (index: EdgeSpaceIx)
    inline def value: Int = index

enum EdgeEligibility:
  case All
  case PositiveOnly
  case NegativeOnly
  case NonZero(tolerance: Double)

  private[connectivity] def accepts(value: Double): Boolean =
    this match
      case All                => true
      case PositiveOnly       => value > 0.0
      case NegativeOnly       => value < 0.0
      case NonZero(tolerance) => Math.abs(value) > tolerance

  private[connectivity] def validate: Either[ConnectivityError, Unit] =
    this match
      case NonZero(tolerance) if !tolerance.isFinite || tolerance < 0.0 =>
        Left(ConnectivityError.InvalidScalar("eligibility tolerance", tolerance, "must be finite and non-negative"))
      case _ => Right(())

enum SelectionScore:
  case Raw
  case Absolute
  case PositiveMagnitude
  case NegativeMagnitude
  case DistanceAscending

  private[connectivity] def score(value: Double): Double =
    this match
      case Raw               => value
      case Absolute          => Math.abs(value)
      case PositiveMagnitude => Math.max(value, 0.0)
      case NegativeMagnitude => Math.max(-value, 0.0)
      case DistanceAscending => value

  private[connectivity] def better(left: Double, right: Double): Boolean =
    this match
      case DistanceAscending => left < right
      case _                 => left > right

  private[connectivity] def passes(score: Double, threshold: Double, inclusive: Boolean): Boolean =
    this match
      case DistanceAscending => if inclusive then score <= threshold else score < threshold
      case _                 => if inclusive then score >= threshold else score > threshold

enum TiePolicy:
  case ExactByEdgeSpaceOrder
  case IncludeBoundaryTies

final class EdgeMask private (
    val edgeSpace: EdgeSpace,
    private val selected: Vector[Boolean]
):
  def size: Int =
    selected.length

  def contains(index: EdgeSpaceIx): Boolean =
    selected(index.value)

  def selectedIndices: Vector[EdgeSpaceIx] =
    selected.indices.collect { case index if selected(index) => EdgeSpaceIx.unsafe(index) }.toVector

  lazy val region: EdgeMaskRegion =
    EdgeMaskRegion.from(
      edgeSpace,
      selected.iterator.zipWithIndex.collect:
        case (true, index) => index
    )

  def union(that: EdgeMask): Either[ConnectivityError, EdgeMask] =
    combine(that)(_ || _)

  def intersect(that: EdgeMask): Either[ConnectivityError, EdgeMask] =
    combine(that)(_ && _)

  def diff(that: EdgeMask): Either[ConnectivityError, EdgeMask] =
    combine(that)((left, right) => left && !right)

  def complement: EdgeMask =
    new EdgeMask(edgeSpace, selected.map(!_))

  private def combine(
      that: EdgeMask
  )(
      operation: (Boolean, Boolean) => Boolean
  ): Either[ConnectivityError, EdgeMask] =
    if !edgeSpace.sameOrderingAs(that.edgeSpace) then
      Left(
        ConnectivityError.IncompatibleEdgeSpace(
          "edge-mask Boolean operation requires the same scientific edge ordering"
        )
      )
    else
      Right(
        new EdgeMask(
          edgeSpace,
          selected.indices.map(index => operation(selected(index), that.selected(index))).toVector
        )
      )

object EdgeMask:
  def from(edgeSpace: EdgeSpace, selected: Iterable[Boolean]): Either[ConnectivityError, EdgeMask] =
    val values = selected.toVector
    if values.length != edgeSpace.size then
      Left(ConnectivityError.EdgeVectorLengthMismatch(edgeSpace.size, values.length))
    else Right(new EdgeMask(edgeSpace, values))

  def fromIndices(edgeSpace: EdgeSpace, selected: Iterable[EdgeSpaceIx]): Either[ConnectivityError, EdgeMask] =
    val indices = selected.toVector
    val values = Array.fill(edgeSpace.size)(false)
    var position = 0
    var error = Option.empty[ConnectivityError]
    while position < indices.length && error.isEmpty do
      val index = indices(position).value
      if index < 0 || index >= edgeSpace.size then
        error = Some(ConnectivityError.InvalidPlan(s"edge-space index $index out of bounds for size ${edgeSpace.size}"))
      else values(index) = true
      position += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(new EdgeMask(edgeSpace, values.toVector))

enum EdgeSelection:
  case AllEligible
  case NonZero(tolerance: Double)
  case Threshold(value: Double, inclusive: Boolean = true)
  case Density(fraction: Double, ties: TiePolicy = TiePolicy.ExactByEdgeSpaceOrder)
  case Mask(mask: EdgeMask)

  private[connectivity] def validate(edgeSpace: EdgeSpace): Either[ConnectivityError, Unit] =
    this match
      case NonZero(tolerance) if !tolerance.isFinite || tolerance < 0.0 =>
        Left(ConnectivityError.InvalidScalar("selection tolerance", tolerance, "must be finite and non-negative"))
      case Threshold(value, _) if !value.isFinite =>
        Left(ConnectivityError.InvalidScalar("selection threshold", value, "must be finite"))
      case Density(fraction, _) if !fraction.isFinite || fraction < 0.0 || fraction > 1.0 =>
        Left(ConnectivityError.InvalidScalar("selection density", fraction, "must be finite and between zero and one"))
      case Mask(mask) if !mask.edgeSpace.sameOrderingAs(edgeSpace) =>
        Left(ConnectivityError.IncompatibleEdgeSpace("projection mask edge space does not match the connectivity matrix"))
      case _ => Right(())

enum DiagonalTreatment:
  case Ignore
  case ValidateAndIgnore
  case Reject

enum ZeroEdgePolicy:
  case ExcludeAfterTransform
  case PreserveSelected

trait WeightTransform[W]:
  def description: String

  def transform(value: Double, measure: ConnectivityMeasure): Either[ConnectivityError, W]

  def numericalValue(weight: W): Double

  def isZero(weight: W): Boolean =
    numericalValue(weight) == 0.0

object WeightTransform:
  val preserve: WeightTransform[Double] =
    doubleTransform("preserve")((value, _) => Right(value))

  val absolute: WeightTransform[Double] =
    doubleTransform("absolute")((value, _) => Right(Math.abs(value)))

  val positiveMagnitude: WeightTransform[Double] =
    doubleTransform("positive-magnitude")((value, _) => Right(Math.max(value, 0.0)))

  val negativeMagnitude: WeightTransform[Double] =
    doubleTransform("negative-magnitude")((value, _) => Right(Math.max(-value, 0.0)))

  def rbfDistance(scale: Double): Either[ConnectivityError, WeightTransform[Double]] =
    if !scale.isFinite || scale <= 0.0 then
      Left(ConnectivityError.InvalidScalar("RBF scale", scale, "must be finite and positive"))
    else
      Right(
        doubleTransform(s"rbf-distance:$scale"): (value, measure) =>
          if measure.scale != ConnectivityValueScale.Distance then
            Left(ConnectivityError.InvalidPlan("RBF distance transformation requires a distance connectivity measure"))
          else Right(Math.exp(-(value * value) / (2.0 * scale * scale)))
      )

  private def doubleTransform(
      label: String
  )(
      f: (Double, ConnectivityMeasure) => Either[ConnectivityError, Double]
  ): WeightTransform[Double] =
    new WeightTransform[Double]:
      def description: String = label
      def transform(value: Double, measure: ConnectivityMeasure): Either[ConnectivityError, Double] = f(value, measure)
      def numericalValue(weight: Double): Double = weight

final case class SelectionReceipt(
    description: String,
    requestedThreshold: Option[Double],
    realizedThreshold: Option[Double],
    requestedDensity: Option[Double],
    realizedDensity: Option[Double],
    tiePolicy: Option[TiePolicy],
    requestedMask: Vector[EdgeSpaceIx]
)

final case class ProjectionReceipt(
    projectionVersion: String,
    sourceMeasureId: String,
    sourceMeasureScale: String,
    sourceBasisKeys: Vector[NodeId],
    sourceBasisProvenance: String,
    sourceTopology: EdgeTopology,
    sourceVectorizationOrder: VectorizationOrder,
    eligibility: EdgeEligibility,
    selectionScore: SelectionScore,
    selection: SelectionReceipt,
    weightTransform: String,
    diagonalTreatment: DiagonalTreatment,
    zeroPolicy: ZeroEdgePolicy,
    selectedEdgeCount: Int,
    zeroWeightedDegreeVertices: Vector[NodeId]
)

final class ProjectedConnectivityGraph[D <: Direction, W] private[connectivity] (
    val sourceEdgeSpace: EdgeSpace,
    val graph: Graph[D, NodeId, NodeSpec, W],
    val receipt: ProjectionReceipt,
    val sourceCoordinateByGraphEdge: Vector[EdgeSpaceIx]
):
  require(graph.size == sourceCoordinateByGraphEdge.length, "every graph edge must have one source coordinate")
  require(
    sourceCoordinateByGraphEdge.map(_.value).distinct.length == sourceCoordinateByGraphEdge.length,
    "source-coordinate correspondence must be injective"
  )

  def sourceEdgeOfGraphEdge(graphEdgeIndex: Int): EdgeRef =
    sourceEdgeSpace.edge(sourceCoordinateByGraphEdge(graphEdgeIndex).value)

  def sourceMask: EdgeMask =
    EdgeMask.fromIndices(sourceEdgeSpace, sourceCoordinateByGraphEdge).toOption.get

final class ConnectivityGraphProjection[D <: Direction, W] private (
    val direction: D,
    val eligibility: EdgeEligibility,
    val selectionScore: SelectionScore,
    val selection: EdgeSelection,
    val transform: WeightTransform[W],
    val diagonalTreatment: DiagonalTreatment,
    val zeroPolicy: ZeroEdgePolicy,
    private val expectedTopology: EdgeTopology,
    private val buildGraph: (
        VertexBasis[NodeId, NodeSpec],
        Iterable[(NodeId, NodeId, W)]
    ) => Either[GraphBuildErrors[NodeId], Graph[D, NodeId, NodeSpec, W]]
):
  def project(matrix: ConnectivityMatrix): Either[ConnectivityError, ProjectedConnectivityGraph[D, W]] =
    for
      _ <- validate(matrix)
      candidates <- candidatesFrom(matrix)
      selected = select(candidates)
      transformed <- transformSelected(selected, matrix.measure)
      retained = applyZeroPolicy(transformed)
      graph <- buildGraph(matrix.edgeSpace.sourceAxis.basis, retained.map(candidate => (candidate.edge.source, candidate.edge.target, candidate.weight)))
        .left.map(errors => ConnectivityError.InvalidPlan(s"projected graph construction failed: ${errors.message}"))
      correspondence <- correspondenceFor(graph, retained)
      receipt = receiptFor(matrix, candidates, selected, retained)
    yield new ProjectedConnectivityGraph(matrix.edgeSpace, graph, receipt, correspondence)

  private def validate(matrix: ConnectivityMatrix): Either[ConnectivityError, Unit] =
    if matrix.edgeSpace.topology == EdgeTopology.Rectangular then
      Left(ConnectivityError.InvalidPlan("rectangular connectivity cannot be projected to a version-one graph"))
    else if matrix.edgeSpace.topology != expectedTopology then
      Left(
        ConnectivityError.InvalidPlan(
          s"${matrix.edgeSpace.topology.label} connectivity cannot use a ${expectedTopology.label} graph projection"
        )
      )
    else if selectionScore == SelectionScore.DistanceAscending && matrix.measure.scale != ConnectivityValueScale.Distance then
      Left(ConnectivityError.InvalidPlan("distance-ascending selection requires a distance connectivity measure"))
    else
      for
        _ <- eligibility.validate
        _ <- selection.validate(matrix.edgeSpace)
        _ <- validateDiagonal(matrix)
      yield ()

  private def validateDiagonal(matrix: ConnectivityMatrix): Either[ConnectivityError, Unit] =
    diagonalTreatment match
      case DiagonalTreatment.Ignore => Right(())
      case DiagonalTreatment.ValidateAndIgnore =>
        matrix.diagonalPolicy.materializedValue match
          case None => Right(())
          case Some(expected) =>
            var index = 0
            var error = Option.empty[ConnectivityError]
            while index < matrix.values.rows && error.isEmpty do
              val value = matrix.values(index, index)
              if Math.abs(value - expected) > 1e-12 then
                error = Some(
                  ConnectivityError.InvalidPlan(
                    s"projection diagonal validation expected $expected at ($index,$index), got $value"
                  )
                )
              index += 1
            error.toLeft(())
      case DiagonalTreatment.Reject =>
        if matrix.diagonalPolicy == DiagonalPolicy.StructuralZero then Right(())
        else
          Left(
            ConnectivityError.InvalidPlan(
              s"diagonal treatment 'reject' requires structural-zero input, got ${matrix.diagonalPolicy.label}"
            )
          )

  private def candidatesFrom(matrix: ConnectivityMatrix): Either[ConnectivityError, Vector[Candidate]] =
    matrix.edgeVector.map: vector =>
      val out = Vector.newBuilder[Candidate]
      var index = 0
      while index < vector.length do
        val value = vector(index)
        if eligibility.accepts(value) then
          out += Candidate(EdgeSpaceIx.unsafe(index), matrix.edgeSpace.edge(index), value, selectionScore.score(value))
        index += 1
      out.result()

  private def select(candidates: Vector[Candidate]): Vector[Candidate] =
    selection match
      case EdgeSelection.AllEligible => candidates
      case EdgeSelection.NonZero(tolerance) => candidates.filter(candidate => Math.abs(candidate.value) > tolerance)
      case EdgeSelection.Threshold(value, inclusive) =>
        candidates.filter(candidate => selectionScore.passes(candidate.score, value, inclusive))
      case EdgeSelection.Mask(mask) => candidates.filter(candidate => mask.contains(candidate.sourceIndex))
      case EdgeSelection.Density(fraction, ties) => selectDensity(candidates, fraction, ties)

  private def selectDensity(
      candidates: Vector[Candidate],
      fraction: Double,
      ties: TiePolicy
  ): Vector[Candidate] =
    val requested = Math.ceil(fraction * candidates.length.toDouble).toInt
    if requested <= 0 then Vector.empty
    else
      val ranked = candidates.sortWith: (left, right) =>
        if left.score == right.score then left.sourceIndex.value < right.sourceIndex.value
        else selectionScore.better(left.score, right.score)
      val limited =
        ties match
          case TiePolicy.ExactByEdgeSpaceOrder => ranked.take(requested)
          case TiePolicy.IncludeBoundaryTies =>
            val boundary = ranked(Math.min(requested, ranked.length) - 1).score
            ranked.takeWhile(candidate => selectionScore.better(candidate.score, boundary) || candidate.score == boundary)
      limited.sortBy(_.sourceIndex.value)

  private def transformSelected(
      selected: Vector[Candidate],
      measure: ConnectivityMeasure
  ): Either[ConnectivityError, Vector[TransformedCandidate[W]]] =
    val out = Vector.newBuilder[TransformedCandidate[W]]
    var index = 0
    var error = Option.empty[ConnectivityError]
    while index < selected.length && error.isEmpty do
      val candidate = selected(index)
      transform.transform(candidate.value, measure) match
        case Left(value) => error = Some(value)
        case Right(weight) =>
          val numerical = transform.numericalValue(weight)
          if !numerical.isFinite then
            error = Some(
              ConnectivityError.NonFiniteValue("projected graph weight", candidate.sourceIndex.value, numerical)
            )
          else out += TransformedCandidate(candidate.sourceIndex, candidate.edge, candidate.score, weight, numerical)
      index += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(out.result())

  private def applyZeroPolicy(values: Vector[TransformedCandidate[W]]): Vector[TransformedCandidate[W]] =
    zeroPolicy match
      case ZeroEdgePolicy.ExcludeAfterTransform => values.filterNot(candidate => transform.isZero(candidate.weight))
      case ZeroEdgePolicy.PreserveSelected      => values

  private def correspondenceFor(
      graph: Graph[D, NodeId, NodeSpec, W],
      retained: Vector[TransformedCandidate[W]]
  ): Either[ConnectivityError, Vector[EdgeSpaceIx]] =
    val sourceByEndpoints = retained.map: candidate =>
      endpointIndexKey(candidate.edge.sourceIndex, candidate.edge.targetIndex) -> candidate.sourceIndex
    .toMap
    val correspondence = graph.edges.map: edge =>
      sourceByEndpoints.get(endpointIndexKey(edge.endpoints.first.toInt, edge.endpoints.second.toInt))
    if correspondence.forall(_.nonEmpty) then Right(correspondence.flatten)
    else Left(ConnectivityError.InvalidPlan("canonical graph edge could not be mapped to its source EdgeSpace coordinate"))

  private def endpointIndexKey(from: Int, to: Int): (Int, Int) =
    direction match
      case Direction.Directed => from -> to
      case Direction.Undirected =>
        if from <= to then from -> to else to -> from

  private def receiptFor(
      matrix: ConnectivityMatrix,
      candidates: Vector[Candidate],
      selected: Vector[Candidate],
      retained: Vector[TransformedCandidate[W]]
  ): ProjectionReceipt =
    val boundary = selection match
      case EdgeSelection.Density(_, _) if selected.nonEmpty => Some(selected.map(_.score).reduce(boundaryScore))
      case EdgeSelection.Threshold(value, _)                 => Some(value)
      case _                                                 => None
    val requestedThreshold = selection match
      case EdgeSelection.Threshold(value, _) => Some(value)
      case _                                 => None
    val requestedDensity = selection match
      case EdgeSelection.Density(fraction, _) => Some(fraction)
      case _                                  => None
    val tiePolicy = selection match
      case EdgeSelection.Density(_, ties) => Some(ties)
      case _                              => None
    val requestedMask = selection match
      case EdgeSelection.Mask(mask) => mask.selectedIndices
      case _                        => Vector.empty
    val description = selection match
      case EdgeSelection.AllEligible          => "all-eligible"
      case EdgeSelection.NonZero(tolerance)    => s"non-zero:$tolerance"
      case EdgeSelection.Threshold(value, inc) => s"threshold:$value:inclusive=$inc"
      case EdgeSelection.Density(fraction, _)  => s"density:$fraction"
      case EdgeSelection.Mask(_)               => "mask"
    val zeroDegree = zeroWeightedDegreeVertices(matrix.edgeSpace.sourceAxis, retained)
    ProjectionReceipt(
      projectionVersion = ConnectivityGraphProjection.version,
      sourceMeasureId = matrix.measure.id,
      sourceMeasureScale = matrix.measure.scale.label,
      sourceBasisKeys = matrix.edgeSpace.sourceAxis.ids,
      sourceBasisProvenance = matrix.edgeSpace.sourceAxis.provenance.description,
      sourceTopology = matrix.edgeSpace.topology,
      sourceVectorizationOrder = matrix.edgeSpace.order,
      eligibility = eligibility,
      selectionScore = selectionScore,
      selection = SelectionReceipt(
        description,
        requestedThreshold,
        boundary,
        requestedDensity,
        requestedDensity.map(_ => if candidates.isEmpty then 0.0 else retained.length.toDouble / candidates.length.toDouble),
        tiePolicy,
        requestedMask
      ),
      weightTransform = transform.description,
      diagonalTreatment = diagonalTreatment,
      zeroPolicy = zeroPolicy,
      selectedEdgeCount = retained.length,
      zeroWeightedDegreeVertices = zeroDegree
    )

  private def boundaryScore(left: Double, right: Double): Double =
    if selectionScore.better(left, right) then right else left

  private def zeroWeightedDegreeVertices(
      axis: NodeAxis,
      values: Vector[TransformedCandidate[W]]
  ): Vector[NodeId] =
    val magnitudes = Array.fill(axis.size)(0.0)
    values.foreach: candidate =>
      val magnitude = Math.abs(candidate.numericalValue)
      magnitudes(candidate.edge.sourceIndex) += magnitude
      magnitudes(candidate.edge.targetIndex) += magnitude
    axis.ids.indices.collect { case index if magnitudes(index) == 0.0 => axis.ids(index) }.toVector

  private final case class Candidate(
      sourceIndex: EdgeSpaceIx,
      edge: EdgeRef,
      value: Double,
      score: Double
  )

  private final case class TransformedCandidate[A](
      sourceIndex: EdgeSpaceIx,
      edge: EdgeRef,
      score: Double,
      weight: A,
      numericalValue: Double
  )

object ConnectivityGraphProjection:
  val version: String = "connectivity-graph-projection-v1"

  def undirected[W](
      eligibility: EdgeEligibility,
      score: SelectionScore,
      selection: EdgeSelection,
      transform: WeightTransform[W],
      diagonal: DiagonalTreatment = DiagonalTreatment.ValidateAndIgnore,
      zeroPolicy: ZeroEdgePolicy = ZeroEdgePolicy.ExcludeAfterTransform
  ): ConnectivityGraphProjection[Direction.Undirected.type, W] =
    new ConnectivityGraphProjection(
      Direction.Undirected,
      eligibility,
      score,
      selection,
      transform,
      diagonal,
      zeroPolicy,
      EdgeTopology.Undirected,
      (basis, edges) => Graph.undirected(basis, edges)
    )

  def directed[W](
      eligibility: EdgeEligibility,
      score: SelectionScore,
      selection: EdgeSelection,
      transform: WeightTransform[W],
      diagonal: DiagonalTreatment = DiagonalTreatment.ValidateAndIgnore,
      zeroPolicy: ZeroEdgePolicy = ZeroEdgePolicy.ExcludeAfterTransform
  ): ConnectivityGraphProjection[Direction.Directed.type, W] =
    new ConnectivityGraphProjection(
      Direction.Directed,
      eligibility,
      score,
      selection,
      transform,
      diagonal,
      zeroPolicy,
      EdgeTopology.Directed,
      (basis, edges) => Graph.directed(basis, edges)
    )

extension (matrix: ConnectivityMatrix)
  def projectUndirected[W](
      projection: ConnectivityGraphProjection[Direction.Undirected.type, W]
  ): Either[ConnectivityError, ProjectedConnectivityGraph[Direction.Undirected.type, W]] =
    projection.project(matrix)

  def projectDirected[W](
      projection: ConnectivityGraphProjection[Direction.Directed.type, W]
  ): Either[ConnectivityError, ProjectedConnectivityGraph[Direction.Directed.type, W]] =
    projection.project(matrix)
