package scalafim.pipeline

enum PipelineNodeKind:
  case Input
  case Step

final case class NodeDescription(
    nodeId: NodeId,
    kind: PipelineNodeKind,
    dependencies: Vector[NodeId],
    outputKind: String,
    label: String,
    step: Option[StepDescription]
)

sealed trait PipelineNode:
  def id: NodeId
  def output: ArtifactRef[?]
  def dependencies: Vector[ArtifactRef[?]]
  def description: NodeDescription

  private[pipeline] def execute(table: ArtifactTable, context: RunContext): Either[PipelineError, StoredArtifact]

private[pipeline] final case class InputNode[A](
    id: NodeId,
    kind: ArtifactKind[A],
    label: Option[String]
) extends PipelineNode:
  override val output: ArtifactRef[A] =
    ArtifactRef(id, kind, label)

  override def dependencies: Vector[ArtifactRef[?]] =
    Vector.empty

  override def description: NodeDescription =
    NodeDescription(
      nodeId = id,
      kind = PipelineNodeKind.Input,
      dependencies = Vector.empty,
      outputKind = kind.label,
      label = output.displayName,
      step = None
    )

  override private[pipeline] def execute(table: ArtifactTable, context: RunContext): Either[PipelineError, StoredArtifact] =
    context.input(output).map(value => StoredArtifact(kind, value))

private[pipeline] final case class StepNode[A, B](
    id: NodeId,
    step: PipelineStep[A, B],
    input: PipelineExpr[A],
    label: Option[String]
) extends PipelineNode:
  override val output: ArtifactRef[B] =
    ArtifactRef(id, step.outputKind, label)

  override def dependencies: Vector[ArtifactRef[?]] =
    input.dependencies

  override def description: NodeDescription =
    NodeDescription(
      nodeId = id,
      kind = PipelineNodeKind.Step,
      dependencies = dependencies.map(_.nodeId).distinct,
      outputKind = step.outputKind.label,
      label = output.displayName,
      step = Some(step.description)
    )

  override private[pipeline] def execute(table: ArtifactTable, context: RunContext): Either[PipelineError, StoredArtifact] =
    input.read(table).flatMap { value =>
      step.run(value, context) match
        case Right(out) =>
          Right(StoredArtifact(step.outputKind, out))
        case Left(error) =>
          Left(PipelineError.StepFailed(id, step.id, error.message))
    }

final case class PipelineOutputRef private[pipeline] (
    name: PortName,
    ref: ArtifactRef[?]
):
  def nodeId: NodeId =
    ref.nodeId

  def kind: ArtifactKind[?] =
    ref.kind

  def label: Option[String] =
    ref.label

  private[pipeline] def typedRef[A](expected: ArtifactKind[A]): Either[PipelineError, ArtifactRef[A]] =
    if ref.kind == expected then Right(ArtifactRef(ref.nodeId, expected, ref.label))
    else Left(PipelineError.ArtifactKindMismatch(ref.nodeId, expected.diagnosticName, ref.kind.diagnosticName))

final case class PipelineGraph private (
    id: PipelineId,
    nodes: Vector[PipelineNode],
    outputs: Vector[PipelineOutputRef]
):
  def addInput[A](
      nodeId: NodeId,
      kind: ArtifactKind[A],
      label: Option[String] = None
  ): Either[PipelineError, (PipelineGraph, ArtifactRef[A])] =
    val node = InputNode(nodeId, kind, label)
    append(node).map(graph => (graph, node.output))

  def addStep[A, B](
      nodeId: NodeId,
      step: PipelineStep[A, B],
      input: PipelineExpr[A],
      label: Option[String] = None
  ): Either[PipelineError, (PipelineGraph, ArtifactRef[B])] =
    val node = StepNode(nodeId, step, input, label)
    append(node).map(graph => (graph, node.output))

  def addOutput[A](name: PortName, ref: ArtifactRef[A]): Either[PipelineError, PipelineGraph] =
    if outputs.exists(_.name == name) then Left(PipelineError.DuplicateOutput(name))
    else
      validateRef(NodeId.unsafe(s"output.${name.value}"), ref).map { _ =>
        copy(outputs = outputs :+ PipelineOutputRef(name, ref))
      }

  def executionPlan: Either[PipelineError, ExecutionPlan] =
    ExecutionPlan.fromGraph(this)

  def describe: Vector[NodeDescription] =
    nodes.map(_.description)

  private def append(node: PipelineNode): Either[PipelineError, PipelineGraph] =
    if nodes.exists(_.id == node.id) then Left(PipelineError.DuplicateNode(node.id))
    else
      node.dependencies
        .foldLeft(Right(()): Either[PipelineError, Unit]) { (validated, ref) =>
          validated.flatMap(_ => validateRef(node.id, ref))
        }
        .map(_ => copy(nodes = nodes :+ node))

  private def validateRef(owner: NodeId, ref: ArtifactRef[?]): Either[PipelineError, Unit] =
    nodes.find(_.id == ref.nodeId) match
      case None =>
        Left(PipelineError.UnknownDependency(owner, ref.nodeId))
      case Some(node) if node.output.kind != ref.kind =>
        Left(PipelineError.ArtifactKindMismatch(ref.nodeId, ref.kind.diagnosticName, node.output.kind.diagnosticName))
      case Some(_) =>
        Right(())

object PipelineGraph:
  def empty(id: PipelineId): PipelineGraph =
    PipelineGraph(id, Vector.empty, Vector.empty)

  private[pipeline] def unchecked(
      id: PipelineId,
      nodes: Vector[PipelineNode],
      outputs: Vector[PipelineOutputRef] = Vector.empty
  ): PipelineGraph =
    PipelineGraph(id, nodes, outputs)
