package scalafim.fmri.mvpa.pattern

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import gale.optim.*
import scalafim.fmri.mvpa.{AxisDescriptor, AxisRef}

enum SpatialSupportError:
  case Invalid(detail: String)
  case AxisMismatch
  case Budget(requiredCells: Long, allowedCells: Long)
  case Solver(error: FirstOrderError)

enum SupportTopology:
  case SurfaceTriangles(meshIdentity: String)
  case VolumeFaceNeighbours(gridIdentity: String)
  case Declared(description: String)

final case class SupportEdge(left: Int, right: Int, weight: Double)

/** Undirected sparse anatomical graph. Weights and their units are scientific
  * inputs, never inferred from the numerical solver. Edges are canonicalized.
  */
final class SupportGraph private (
    val axis: AxisDescriptor, val edges: Vector[SupportEdge],
    val topology: SupportTopology, val weightUnits: String
):
  private[pattern] val degree = Array.fill(axis.size)(0)
  private[pattern] val weightedDegree = Array.fill(axis.size)(0.0)
  edges.foreach: edge =>
    degree(edge.left) += 1
    degree(edge.right) += 1
    weightedDegree(edge.left) += edge.weight
    weightedDegree(edge.right) += edge.weight

object SupportGraph:
  def apply(axis: AxisDescriptor, edges: Vector[SupportEdge], topology: SupportTopology,
      weightUnits: String): Either[SpatialSupportError, SupportGraph] =
    val ordered = edges.map(e => if e.left < e.right then e else SupportEdge(e.right, e.left, e.weight))
      .sortBy(e => (e.left, e.right))
    val description = topology match
      case SupportTopology.SurfaceTriangles(value) => value
      case SupportTopology.VolumeFaceNeighbours(value) => value
      case SupportTopology.Declared(value) => value
    if description.trim.isEmpty || weightUnits.trim.isEmpty then Left(SpatialSupportError.Invalid("graph provenance/weight units are required"))
    else if ordered.exists(e => e.left < 0 || e.right >= axis.size || e.left == e.right || !e.weight.isFinite || e.weight <= 0.0) then
      Left(SpatialSupportError.Invalid("edges require distinct in-range endpoints and finite positive weights"))
    else if ordered.map(e => (e.left, e.right)).distinct.size != ordered.size then Left(SpatialSupportError.Invalid("duplicate undirected edge"))
    else
      val graph = new SupportGraph(axis, ordered, topology, weightUnits)
      if graph.weightedDegree.exists(!_.isFinite) then Left(SpatialSupportError.Invalid("weighted degree overflow"))
      else Right(graph)

/** All three penalties are independent. Sparsity acts on the nonnegative
  * support envelope; signedSmoothness acts on A, and defaults to zero.
  */
final class SupportPenalty private (val sparsity: Double, val supportTv: Double, val signedSmoothness: Double)
object SupportPenalty:
  def apply(sparsity: Double, supportTv: Double, signedSmoothness: Double = 0.0): Either[SpatialSupportError, SupportPenalty] =
    if Vector(sparsity, supportTv, signedSmoothness).exists(v => !v.isFinite || v < 0.0) then
      Left(SpatialSupportError.Invalid("penalties must be finite and nonnegative"))
    else Right(new SupportPenalty(sparsity, supportTv, signedSmoothness))

final case class SupportWork(variableCells: Long, edgeCells: Long, plannedWorkspaceCells: Long)
final class SupportUpdate private[pattern] (
    val loadings: DMat, val envelope: Vector[Double], val objective: Double,
    val maximumConstraintViolation: Double, val feasibilityTolerance: Double,
    val solution: FirstOrderSolution, val work: SupportWork
):
  def converged: Boolean = solution.status == FirstOrderStoppingStatus.Converged

/** A conditional convex proximal subproblem, not the joint reduced-rank fit:
  * 1/2 ||A-Z||² + 1/2 ||g-h||² + sparsity sum(g) + supportTv TV_w(g)
  * + signedSmoothness/2 sum_edges w ||A_v-A_w||², ||A_v|| <= g_v.
  * Generic iteration/stopping is provided by Gale. Domain callbacks implement
  * the exact row-envelope cone proximal and sparse graph operations.
  */
object SpatialSupport:
  def update[P](axis: AxisRef[P], reference: DMat, envelopeReference: Vector[Double],
      graph: SupportGraph, penalty: SupportPenalty, maximumWorkspaceCells: Long,
      config: FirstOrderConfig = FirstOrderConfig.portable,
      feasibilityTolerance: Double = 1e-8): Either[SpatialSupportError, SupportUpdate] =
    val p = axis.size
    val r = reference.cols
    val cells = p.toLong * (r.toLong + 1L)
    // Conservatively accounts for simultaneously live callback/solver blocks;
    // excludes Gale/operator object headers and cumulative iteration allocation.
    val workspace = if cells > (Long.MaxValue - 16L * graph.edges.size) / 32L then Long.MaxValue else 32L * cells + 16L * graph.edges.size
    val work = SupportWork(cells, graph.edges.size.toLong, workspace)
    if graph.axis != axis.descriptor then Left(SpatialSupportError.AxisMismatch)
    else if reference.rows != p || r < 1 || envelopeReference.size != p then Left(SpatialSupportError.Invalid("reference shapes differ from neural axis"))
    else if cells > Int.MaxValue || maximumWorkspaceCells < work.plannedWorkspaceCells then Left(SpatialSupportError.Budget(work.plannedWorkspaceCells, maximumWorkspaceCells))
    else if !feasibilityTolerance.isFinite || feasibilityTolerance < 0.0 ||
        envelopeReference.exists(!_.isFinite) || !ResidualCovariance.finite(reference) then Left(SpatialSupportError.Invalid("nonfinite reference or invalid feasibility tolerance"))
    else
      val width = r + 1
      val rows = cells.toInt
      def norm(at: DMat, vertex: Int): Double =
        var value = 0.0
        var component = 0
        while component < r do
          value = math.hypot(value, at(vertex * width + component, 0))
          component += 1
        value
      def violation(at: DMat): Double =
        var maximum = 0.0
        var vertex = 0
        while vertex < p do
          maximum = math.max(maximum, norm(at, vertex) - at(vertex * width + r, 0))
          vertex += 1
        maximum
      val initial = DMat.tabulate(rows, 1): (index, _) =>
        val vertex = index / width
        val component = index % width
        if component < r then reference(vertex, component)
        else
          var length = 0.0
          var c = 0
          while c < r do
            length = math.hypot(length, reference(vertex, c))
            c += 1
          math.max(length, envelopeReference(vertex))
      val smooth = new SmoothObjective:
        val variableRows = rows
        val lipschitz = 1.0 + 2.0 * (penalty.signedSmoothness * graph.weightedDegree.max)
        def value(at: DMat): Either[FirstOrderError, Double] =
          var sum = 0.0
          var vertex = 0
          while vertex < p do
            var component = 0
            while component < width do
              val center = if component < r then reference(vertex, component) else envelopeReference(vertex)
              val difference = at(vertex * width + component, 0) - center
              sum += 0.5 * difference * difference
              component += 1
            vertex += 1
          graph.edges.foreach: edge =>
            var component = 0
            while component < r do
              val difference = at(edge.left * width + component, 0) - at(edge.right * width + component, 0)
              sum += 0.5 * penalty.signedSmoothness * edge.weight * difference * difference
              component += 1
          Right(sum)
        def gradient(at: DMat): Either[FirstOrderError, DMat] =
          val result = Array.tabulate(rows): index =>
            val vertex = index / width
            val component = index % width
            at(index, 0) - (if component < r then reference(vertex, component) else envelopeReference(vertex))
          graph.edges.foreach: edge =>
            var component = 0
            while component < r do
              val left = edge.left * width + component
              val right = edge.right * width + component
              val gradient = penalty.signedSmoothness * edge.weight * (at(left, 0) - at(right, 0))
              result(left) += gradient
              result(right) -= gradient
              component += 1
          Right(DMat.dense(rows, 1, result.toVector))
      val direct = new ProximalTerm:
        val variableRows = rows
        def value(at: DMat): Either[FirstOrderError, Double] =
          if violation(at) > feasibilityTolerance then Left(FirstOrderError.OracleFailure("support cone", "infeasible envelope"))
          else
            var sum = 0.0
            var vertex = 0
            while vertex < p do
              sum += at(vertex * width + r, 0)
              vertex += 1
            Right(penalty.sparsity * sum)
        def proximal(at: DMat, step: Double): Either[FirstOrderError, DMat] =
          val result = new Array[Double](rows)
          var vertex = 0
          while vertex < p do
            val length = norm(at, vertex)
            val shifted = at(vertex * width + r, 0) - step * penalty.sparsity
            val scale = if length <= shifted then 1.0 else if length <= -shifted then 0.0 else 0.5 * (1.0 + shifted / length)
            var component = 0
            while component < r do
              result(vertex * width + component) = at(vertex * width + component, 0) * scale
              component += 1
            result(vertex * width + r) = if length <= shifted then shifted else if length <= -shifted then 0.0 else 0.5 * (length + shifted)
            vertex += 1
          Right(DMat.dense(rows, 1, result.toVector))
      val incidence = new DoubleLinearOperator:
        val rows = graph.edges.size
        val cols = cells.toInt
        def applyTo(x: DVec, into: MutableDVec): Unit =
          var index = 0
          while index < rows do
            val edge = graph.edges(index)
            into(index) = x(edge.left * width + r) - x(edge.right * width + r)
            index += 1
        override def transposeApplyTo(x: DVec, into: MutableDVec): Unit =
          var index = 0
          while index < cols do
            into(index) = 0.0
            index += 1
          index = 0
          while index < rows do
            val edge = graph.edges(index)
            into(edge.left * width + r) += x(index)
            into(edge.right * width + r) -= x(index)
            index += 1
      val tv = new LinearCompositeFunctional:
        val targetRows = graph.edges.size
        def value(at: DMat): Either[FirstOrderError, Double] =
          var sum = 0.0
          var index = 0
          while index < targetRows do
            sum += penalty.supportTv * graph.edges(index).weight * math.abs(at(index, 0))
            index += 1
          Right(sum)
        def proximalConjugate(at: DMat, step: Double): Either[FirstOrderError, DMat] =
          Right(DMat.tabulate(targetRows, at.cols): (index, column) =>
            val bound = penalty.supportTv * graph.edges(index).weight
            math.max(-bound, math.min(bound, at(index, column))))
      val solved =
        if graph.edges.isEmpty || penalty.supportTv == 0.0 then FirstOrderSolvers.proximalGradient(smooth, direct, initial, config)
        else BoundedLinearOperator.from(incidence, math.sqrt(2.0 * graph.degree.max))
          .flatMap(FirstOrderSolvers.smoothCompositePrimalDual(smooth, direct, tv, _, initial, config))
      solved.left.map(SpatialSupportError.Solver.apply).flatMap: solution =>
        val maximum = violation(solution.primal)
        if maximum > feasibilityTolerance then Left(SpatialSupportError.Invalid(s"returned cone violation $maximum"))
        else Right(new SupportUpdate(DMat.tabulate(p, r)((v, c) => solution.primal(v * width + c, 0)),
          Vector.tabulate(p)(v => solution.primal(v * width + r, 0)), solution.objective,
          maximum, feasibilityTolerance, solution, work))
