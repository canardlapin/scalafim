package scalafim.pipeline

final case class RunnerMetadata(
    name: String,
    details: Vector[(String, String)] = Vector.empty
):
  require(name.nonEmpty, "runner metadata name must be non-empty")

  def sortedDetails: Vector[(String, String)] =
    details.sortBy(_._1)

object RunnerMetadata:
  val local: RunnerMetadata =
    RunnerMetadata("local")

  def future(policy: ParallelPolicy): RunnerMetadata =
    RunnerMetadata(
      "future",
      Vector("maxConcurrency" -> policy.maxConcurrency.toString)
    )

final case class PipelineOutputTrace(
    name: PortName,
    nodeId: NodeId,
    artifactKind: String,
    label: String
)

final case class PipelineNodeTrace(
    description: NodeDescription,
    status: PipelineStatus,
    message: Option[String]
):
  def nodeId: NodeId =
    description.nodeId

final case class PipelineTrace(
    graphId: PipelineId,
    status: PipelineStatus,
    runner: RunnerMetadata,
    userMetadata: Vector[(String, String)],
    outputs: Vector[PipelineOutputTrace],
    nodes: Vector[PipelineNodeTrace],
    error: Option[String]
):
  def renderLines: Vector[String] =
    val header =
      Vector(
        s"pipeline ${graphId.value} status=${statusName(status)}",
        renderRunner
      )
    val metadataLines =
      userMetadata.map { case (key, value) => s"metadata $key=$value" }
    val outputLines =
      outputs.map { output =>
        s"output ${output.name.value} node=${output.nodeId.value} kind=${output.artifactKind} label=${output.label}"
      }
    val nodeLines =
      nodes.map(renderNode)
    val errorLines =
      error.toVector.map(message => s"error $message")

    header ++ metadataLines ++ outputLines ++ nodeLines ++ errorLines

  def renderText: String =
    renderLines.mkString("\n")

  private def renderRunner: String =
    val details = runner.sortedDetails
    if details.isEmpty then s"runner ${runner.name}"
    else
      val rendered = details.map { case (key, value) => s"$key=$value" }.mkString(" ")
      s"runner ${runner.name} $rendered"

  private def renderNode(node: PipelineNodeTrace): String =
    val desc = node.description
    val deps =
      if desc.dependencies.isEmpty then "-"
      else desc.dependencies.map(_.value).mkString(",")
    val step =
      desc.step.map(_.stepId.value).getOrElse("-")
    val message =
      node.message.fold("")(value => s" message=$value")
    s"node ${desc.nodeId.value} kind=${nodeKindName(desc.kind)} status=${statusName(node.status)} deps=$deps output=${desc.outputKind} step=$step label=${desc.label}$message"

  private def statusName(status: PipelineStatus): String =
    status match
      case PipelineStatus.Pending   => "pending"
      case PipelineStatus.Running   => "running"
      case PipelineStatus.Succeeded => "succeeded"
      case PipelineStatus.Failed    => "failed"
      case PipelineStatus.Skipped   => "skipped"

  private def nodeKindName(kind: PipelineNodeKind): String =
    kind match
      case PipelineNodeKind.Input => "input"
      case PipelineNodeKind.Step  => "step"

object PipelineTrace:
  def fromRun(run: PipelineRun): PipelineTrace =
    val receiptsByNode =
      run.receipts.map(receipt => receipt.nodeId -> receipt).toMap
    val nodes =
      run.nodeDescriptions.map { description =>
        val receipt = receiptsByNode.get(description.nodeId)
        PipelineNodeTrace(
          description = description,
          status = receipt.map(_.status).getOrElse(PipelineStatus.Pending),
          message = receipt.flatMap(_.message)
        )
      }
    val outputs =
      run.outputs.map { output =>
        PipelineOutputTrace(
          name = output.name,
          nodeId = output.nodeId,
          artifactKind = output.kind.label,
          label = output.label.getOrElse(output.nodeId.value)
        )
      }

    PipelineTrace(
      graphId = run.graphId,
      status = run.status,
      runner = run.runner,
      userMetadata = run.userMetadata,
      outputs = outputs,
      nodes = nodes,
      error = run.error.map(_.message)
    )
