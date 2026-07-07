package scalafim.pipeline

enum PipelineError:
  case InvalidId(kind: String, value: String, reason: String)
  case DuplicateNode(id: NodeId)
  case DuplicateOutput(name: PortName)
  case UnknownDependency(nodeId: NodeId, dependency: NodeId)
  case CyclicGraph(remaining: Vector[NodeId])
  case MissingArtifact(nodeId: NodeId)
  case MissingPipelineInput(nodeId: NodeId)
  case ArtifactKindMismatch(nodeId: NodeId, expected: String, actual: String)
  case StepFailed(nodeId: NodeId, stepId: StepId, reason: String)
  case ExpressionFailed(label: String, reason: String)
  case InvalidGraph(detail: String)

  def message: String =
    this match
      case InvalidId(kind, value, reason) =>
        s"invalid $kind '$value': $reason"
      case DuplicateNode(id) =>
        s"duplicate pipeline node '${id.value}'"
      case DuplicateOutput(name) =>
        s"duplicate pipeline output '${name.value}'"
      case UnknownDependency(nodeId, dependency) =>
        s"node '${nodeId.value}' depends on unknown node '${dependency.value}'"
      case CyclicGraph(remaining) =>
        val ids = remaining.map(_.value).mkString(", ")
        s"pipeline graph contains a cycle among node(s): $ids"
      case MissingArtifact(nodeId) =>
        s"artifact for node '${nodeId.value}' is not available"
      case MissingPipelineInput(nodeId) =>
        s"pipeline input '${nodeId.value}' was not provided"
      case ArtifactKindMismatch(nodeId, expected, actual) =>
        s"artifact '${nodeId.value}' has kind '$actual', expected '$expected'"
      case StepFailed(nodeId, stepId, reason) =>
        s"step '${stepId.value}' at node '${nodeId.value}' failed: $reason"
      case ExpressionFailed(label, reason) =>
        s"input expression '$label' failed: $reason"
      case InvalidGraph(detail) =>
        detail
