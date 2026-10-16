package scalafim.fmri.mvpa.pipeline

import scalafim.fmri.mvpa.execution.{UnitError, UnitEvaluation, UnitInterpreter}
import scalafim.pipeline.{ArtifactKind, LocalPipelineRunner, PipelineBuilder, PipelineError, PipelineStep, RunContext, StepId}

/** Optional ScalaFIM pipeline interpretation of an MVPA work-unit evaluation.
  * The MVPA runner remains the sole owner of resource and commit semantics.
  */
object PipelineInterpreter extends UnitInterpreter:
  private type Evaluation[C, O] = () => Either[UnitError, UnitEvaluation[C, O]]

  private final class ComputeStep[C, O] extends PipelineStep[Evaluation[C, O], Either[UnitError, UnitEvaluation[C, O]]]:
    val id: StepId = StepId.unsafe("compute")
    val outputKind: ArtifactKind[Either[UnitError, UnitEvaluation[C, O]]] =
      ArtifactKind.unsafe("mvpa-unit-evaluation")

    def run(input: Evaluation[C, O], context: RunContext): Either[PipelineError, Either[UnitError, UnitEvaluation[C, O]]] =
      Right(input())

  def evaluate[C, O](
      evaluation: () => Either[UnitError, UnitEvaluation[C, O]]
  ): Either[UnitError, UnitEvaluation[C, O]] =
    val inputKind = ArtifactKind.unsafe[Evaluation[C, O]]("mvpa-unit-work")
    val step = new ComputeStep[C, O]
    val built =
      for
        builder <- PipelineBuilder("mvpa.execution")
        input <- builder.input("work", inputKind)
        output <- input.builder.step("compute", step, input.ref)
      yield (output.builder.build, input.ref, output.ref)
    built match
      case Left(error) => Left(UnitError.Interpreter(error.message))
      case Right((graph, input, output)) =>
        val run = LocalPipelineRunner.run(graph, RunContext.empty.withInput(input, evaluation))
        run.error match
          case Some(error) => Left(UnitError.Interpreter(error.message))
          case None => run.get(output).fold(error => Left(UnitError.Interpreter(error.message)), identity)
