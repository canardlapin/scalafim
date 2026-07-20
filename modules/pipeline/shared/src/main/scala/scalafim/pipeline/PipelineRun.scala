package scalafim.pipeline

import scala.annotation.targetName

enum PipelineStatus:
  case Pending
  case Running
  case Succeeded
  case Failed
  case Skipped

final case class StepReceipt(
    nodeId: NodeId,
    status: PipelineStatus,
    dependencies: Vector[NodeId],
    outputKind: String,
    stepId: Option[StepId],
    message: Option[String]
)

final case class PipelineRun(
    graphId: PipelineId,
    outputs: Vector[PipelineOutputRef],
    status: PipelineStatus,
    values: ArtifactTable,
    receipts: Vector[StepReceipt],
    error: Option[PipelineError],
    nodeDescriptions: Vector[NodeDescription] = Vector.empty,
    runner: RunnerMetadata = RunnerMetadata.local,
    userMetadata: Vector[(String, String)] = Vector.empty
):
  def succeeded: Boolean =
    status == PipelineStatus.Succeeded

  def trace: PipelineTrace =
    PipelineTrace.fromRun(this)

  def get[A](ref: ArtifactRef[A]): Either[PipelineError, A] =
    values.get(ref)

  def outputRef(name: PortName): Either[PipelineError, PipelineOutputRef] =
    outputs.find(_.name == name).toRight(PipelineError.MissingOutput(name))

  @targetName("outputRefByString")
  def outputRef(name: String): Either[PipelineError, PipelineOutputRef] =
    PortName(name).flatMap(outputRef)

  def output[A](name: PortName, kind: ArtifactKind[A]): Either[PipelineError, A] =
    for
      output <- outputRef(name)
      ref <- output.typedRef(kind)
      value <- values.get(ref)
    yield value

  @targetName("outputByString")
  def output[A](name: String, kind: ArtifactKind[A]): Either[PipelineError, A] =
    PortName(name).flatMap(port => output(port, kind))

object LocalPipelineRunner:
  def run(graph: PipelineGraph, context: RunContext = RunContext.empty): PipelineRun =
    graph.executionPlan match
      case Left(error) =>
        PipelineRun(
          graphId = graph.id,
          outputs = graph.outputs,
          status = PipelineStatus.Failed,
          values = ArtifactTable.empty,
          receipts = Vector.empty,
          error = Some(error),
          nodeDescriptions = graph.describe,
          runner = RunnerMetadata.local,
          userMetadata = sortedMetadata(context)
        )
      case Right(plan) =>
        runPlan(plan, context)

  def runPlan(plan: ExecutionPlan, context: RunContext = RunContext.empty): PipelineRun =
    var table = ArtifactTable.empty
    var failedOrSkipped = Set.empty[NodeId]
    var firstError = Option.empty[PipelineError]
    val receipts = Vector.newBuilder[StepReceipt]

    plan.nodesInOrder.foreach { node =>
      val failedDependency = node.dependencies.find(ref => failedOrSkipped.contains(ref.nodeId))
      failedDependency match
        case Some(dep) =>
          failedOrSkipped = failedOrSkipped + node.id
          receipts += receipt(
            node,
            PipelineStatus.Skipped,
            Some(s"dependency '${dep.nodeId.value}' did not succeed")
          )
        case None =>
          PipelineNodeExecution.execute(node, table, context) match
            case Right(stored) =>
              table.putStored(node.output, stored) match
                case Right(updated) =>
                  table = updated
                  receipts += receipt(node, PipelineStatus.Succeeded, None)
                case Left(error) =>
                  if firstError.isEmpty then firstError = Some(error)
                  failedOrSkipped = failedOrSkipped + node.id
                  receipts += receipt(node, PipelineStatus.Failed, Some(error.message))
            case Left(error) =>
              if firstError.isEmpty then firstError = Some(error)
              failedOrSkipped = failedOrSkipped + node.id
              receipts += receipt(node, PipelineStatus.Failed, Some(error.message))
    }

    val finalStatus =
      if firstError.isEmpty && failedOrSkipped.isEmpty then PipelineStatus.Succeeded
      else PipelineStatus.Failed

    PipelineRun(
      graphId = plan.graph.id,
      outputs = plan.graph.outputs,
      status = finalStatus,
      values = table,
      receipts = receipts.result(),
      error = firstError,
      nodeDescriptions = plan.graph.describe,
      runner = RunnerMetadata.local,
      userMetadata = sortedMetadata(context)
    )

  private def sortedMetadata(context: RunContext): Vector[(String, String)] =
    context.metadata.toVector.sortBy(_._1)

  private def receipt(
      node: PipelineNode,
      status: PipelineStatus,
      message: Option[String]
  ): StepReceipt =
    StepReceipt(
      nodeId = node.id,
      status = status,
      dependencies = node.dependencies.map(_.nodeId).distinct,
      outputKind = node.output.kind.label,
      stepId = node.description.step.map(_.stepId),
      message = message
    )
