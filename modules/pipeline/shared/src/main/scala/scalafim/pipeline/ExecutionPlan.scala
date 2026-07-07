package scalafim.pipeline

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
            return Left(PipelineError.ArtifactKindMismatch(dep.nodeId, dep.kind.label, depNode.output.kind.label))
          case Some(_) =>
            ()
        j += 1
      i += 1
    Right(())

  private def stage(nodes: Vector[PipelineNode]): Either[PipelineError, Vector[PipelineStage]] =
    var remaining = nodes
    var available = Set.empty[NodeId]
    val out = Vector.newBuilder[PipelineStage]
    var index = 0

    while remaining.nonEmpty do
      val ready = remaining.filter(node => node.dependencies.forall(ref => available.contains(ref.nodeId)))
      if ready.isEmpty then
        return Left(PipelineError.CyclicGraph(remaining.map(_.id)))

      out += PipelineStage(index, ready)
      val readyIds = ready.map(_.id).toSet
      available = available ++ readyIds
      remaining = remaining.filterNot(node => readyIds.contains(node.id))
      index += 1

    Right(out.result())
