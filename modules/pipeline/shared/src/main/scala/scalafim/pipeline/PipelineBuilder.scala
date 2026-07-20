package scalafim.pipeline

final case class PipelineBuilder private (graph: PipelineGraph):
  def id: PipelineId =
    graph.id

  def build: PipelineGraph =
    graph

  def inputNode[A](
      nodeId: NodeId,
      kind: ArtifactKind[A],
      label: Option[String] = None
  ): Either[PipelineError, PipelineBuilder.Added[A]] =
    graph.addInput(nodeId, kind, label).map { case (next, ref) =>
      PipelineBuilder.Added(PipelineBuilder.fromGraph(next), ref)
    }

  def input[A](
      nodeId: String,
      kind: ArtifactKind[A],
      label: Option[String]
  ): Either[PipelineError, PipelineBuilder.Added[A]] =
    NodeId(nodeId).flatMap(id => inputNode(id, kind, label))

  def input[A](
      nodeId: String,
      kind: ArtifactKind[A]
  ): Either[PipelineError, PipelineBuilder.Added[A]] =
    input(nodeId, kind, None)

  def stepNode[A, B](
      nodeId: NodeId,
      step: PipelineStep[A, B],
      input: PipelineExpr[A],
      label: Option[String] = None
  ): Either[PipelineError, PipelineBuilder.Added[B]] =
    graph.addStep(nodeId, step, input, label).map { case (next, ref) =>
      PipelineBuilder.Added(PipelineBuilder.fromGraph(next), ref)
    }

  def step[A, B](
      nodeId: String,
      step: PipelineStep[A, B],
      input: PipelineExpr[A],
      label: Option[String]
  ): Either[PipelineError, PipelineBuilder.Added[B]] =
    NodeId(nodeId).flatMap(id => stepNode(id, step, input, label))

  def step[A, B](
      nodeId: String,
      step: PipelineStep[A, B],
      input: PipelineExpr[A]
  ): Either[PipelineError, PipelineBuilder.Added[B]] =
    this.step(nodeId, step, input, None)

  def stepNode[A, B](
      nodeId: NodeId,
      step: PipelineStep[A, B],
      input: ArtifactRef[A],
      label: Option[String]
  ): Either[PipelineError, PipelineBuilder.Added[B]] =
    stepNode(nodeId, step, input.expr, label)

  def stepNode[A, B](
      nodeId: NodeId,
      step: PipelineStep[A, B],
      input: ArtifactRef[A]
  ): Either[PipelineError, PipelineBuilder.Added[B]] =
    stepNode(nodeId, step, input.expr, None)

  def step[A, B](
      nodeId: String,
      step: PipelineStep[A, B],
      input: ArtifactRef[A],
      label: Option[String]
  ): Either[PipelineError, PipelineBuilder.Added[B]] =
    NodeId(nodeId).flatMap(id => stepNode(id, step, input, label))

  def step[A, B](
      nodeId: String,
      step: PipelineStep[A, B],
      input: ArtifactRef[A]
  ): Either[PipelineError, PipelineBuilder.Added[B]] =
    this.step(nodeId, step, input, None)

  def outputPort[A](name: PortName, ref: ArtifactRef[A]): Either[PipelineError, PipelineBuilder] =
    graph.addOutput(name, ref).map(PipelineBuilder.fromGraph)

  def output[A](name: String, ref: ArtifactRef[A]): Either[PipelineError, PipelineBuilder] =
    PortName(name).flatMap(port => outputPort(port, ref))

  def executionPlan: Either[PipelineError, ExecutionPlan] =
    graph.executionPlan

  def describe: Vector[NodeDescription] =
    graph.describe

object PipelineBuilder:
  final case class Added[A](
      builder: PipelineBuilder,
      ref: ArtifactRef[A]
  ):
    def expr: PipelineExpr[A] =
      ref.expr

  def fromId(id: PipelineId): PipelineBuilder =
    new PipelineBuilder(PipelineGraph.empty(id))

  def apply(id: String): Either[PipelineError, PipelineBuilder] =
    PipelineId(id).map(fromId)

  def unsafe(id: String): PipelineBuilder =
    apply(id).fold(error => throw new IllegalArgumentException(error.message), identity)

  private[pipeline] def fromGraph(graph: PipelineGraph): PipelineBuilder =
    new PipelineBuilder(graph)
