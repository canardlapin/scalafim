package scalafim.pipeline

import scala.util.control.NonFatal

private[pipeline] object PipelineNodeExecution:
  def execute(
      node: PipelineNode,
      table: ArtifactTable,
      context: RunContext
  ): Either[PipelineError, StoredArtifact] =
    try node.execute(table, context)
    catch
      case NonFatal(t) =>
        val reason = Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.toString)
        node.description.step match
          case Some(step) => Left(PipelineError.StepFailed(node.id, step.stepId, reason))
          case None       => Left(PipelineError.InvalidGraph(reason))
