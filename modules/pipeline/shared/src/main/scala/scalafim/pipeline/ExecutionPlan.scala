package scalafim.pipeline

import scala.collection.mutable

import scalafim.graph.Cycle
import scalafim.graph.Dag
import scalafim.graph.DirectedGraph
import scalafim.graph.Graph
import scalafim.graph.VertexBasis
import scalafim.graph.stronglyConnectedComponents

final case class PipelineStage(index: Int, nodes: Vector[PipelineNode]):
  require(index >= 0, "pipeline stage index must be non-negative")
  require(nodes.nonEmpty, "pipeline stage must contain at least one node")

  def nodeIds: Vector[NodeId] =
    nodes.map(_.id)

final case class ExecutionPlan private[pipeline] (
    graph: PipelineGraph,
    stages: Vector[PipelineStage]
):
  def nodesInOrder: Vector[PipelineNode] =
    stages.flatMap(_.nodes)

object ExecutionPlan:
  def fromGraph(graph: PipelineGraph): Either[PipelineError, ExecutionPlan] =
    for
      _ <- validateUniqueNodes(graph.nodes)
      _ <- validateDependencies(graph.nodes)
      stages <- stage(graph.nodes)
    yield ExecutionPlan(graph, stages)

  private def validateUniqueNodes(nodes: Vector[PipelineNode]): Either[PipelineError, Unit] =
    val seen = scala.collection.mutable.HashSet.empty[NodeId]
    var i = 0
    while i < nodes.length do
      val id = nodes(i).id
      if seen.contains(id) then return Left(PipelineError.DuplicateNode(id))
      seen += id
      i += 1
    Right(())

  private def validateDependencies(nodes: Vector[PipelineNode]): Either[PipelineError, Unit] =
    val byId = nodes.map(node => node.id -> node).toMap
    var i = 0
    while i < nodes.length do
      val node = nodes(i)
      var j = 0
      val deps = node.dependencies
      while j < deps.length do
        val dep = deps(j)
        byId.get(dep.nodeId) match
          case None =>
            return Left(PipelineError.UnknownDependency(node.id, dep.nodeId))
          case Some(depNode) if depNode.output.kind != dep.kind =>
            return Left(PipelineError.ArtifactKindMismatch(dep.nodeId, dep.kind.diagnosticName, depNode.output.kind.diagnosticName))
          case Some(_) =>
            ()
        j += 1
      i += 1
    Right(())

  private def stage(nodes: Vector[PipelineNode]): Either[PipelineError, Vector[PipelineStage]] =
    dependencyGraph(nodes).flatMap: graph =>
      val selfCycles = nodes.collect:
        case node if node.dependencies.exists(_.nodeId == node.id) => node.id
      Dag.from(graph) match
        case Right(dag) if selfCycles.isEmpty =>
          val byId = nodes.map(node => node.id -> node).toMap
          Right(
            dag.topologicalLayers.zipWithIndex.map: (layer, index) =>
              PipelineStage(index, layer.map(byId))
          )
        case Left(witness) =>
          Left(PipelineError.CyclicGraph(blockedByCycles(nodes, graph, selfCycles, Some(witness))))
        case Right(_) =>
          Left(PipelineError.CyclicGraph(blockedByCycles(nodes, graph, selfCycles, None)))

  private def dependencyGraph(
      nodes: Vector[PipelineNode]
  ): Either[PipelineError, DirectedGraph[NodeId, PipelineNode, Unit]] =
    val basis =
      VertexBasis
        .from(nodes.map(node => node.id -> node))
        .left
        .map(error => PipelineError.InvalidGraph(s"pipeline dependency basis failed: ${error.message}"))
    val edges = nodes.flatMap: node =>
      node.dependencies
        .map(_.nodeId)
        .distinct
        .filter(_ != node.id)
        .map(dependency => (dependency, node.id, ()))
    basis.flatMap: value =>
      Graph
        .directed(value, edges)
        .left
        .map(errors => PipelineError.InvalidGraph(s"pipeline dependency graph failed: ${errors.message}"))

  private def blockedByCycles(
      nodes: Vector[PipelineNode],
      graph: DirectedGraph[NodeId, PipelineNode, Unit],
      selfCycles: Vector[NodeId],
      witness: Option[Cycle[NodeId]]
  ): Vector[NodeId] =
    val componentCycles = graph.stronglyConnectedComponents
      .filter(_.keys.length > 1)
      .flatMap(_.keys)
    val seeds = (selfCycles ++ componentCycles ++ witness.toVector.flatMap(_.keys.dropRight(1))).toSet
    val dependents = mutable.HashMap.empty[NodeId, Vector[NodeId]]
    nodes.foreach: node =>
      node.dependencies.map(_.nodeId).distinct.foreach: dependency =>
        dependents.update(dependency, dependents.getOrElse(dependency, Vector.empty) :+ node.id)

    val blocked = mutable.HashSet.from(seeds)
    val queue = mutable.Queue.from(seeds.toVector)
    while queue.nonEmpty do
      val current = queue.dequeue()
      dependents.getOrElse(current, Vector.empty).foreach: dependent =>
        if blocked.add(dependent) then queue.enqueue(dependent)

    nodes.collect { case node if blocked(node.id) => node.id }
