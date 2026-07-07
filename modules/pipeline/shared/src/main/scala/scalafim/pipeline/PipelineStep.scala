package scalafim.pipeline

final case class StepDescription(
    stepId: StepId,
    label: String,
    detail: Option[String] = None,
    metadata: Vector[(String, String)] = Vector.empty
):
  require(label.nonEmpty, "step description label must be non-empty")

trait PipelineStep[A, B]:
  def id: StepId
  def outputKind: ArtifactKind[B]

  def description: StepDescription =
    StepDescription(id, id.value)

  def run(input: A, context: RunContext): Either[PipelineError, B]
