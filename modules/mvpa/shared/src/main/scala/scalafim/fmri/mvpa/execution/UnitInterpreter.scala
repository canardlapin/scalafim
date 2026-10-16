package scalafim.fmri.mvpa.execution

/** Interprets one synchronous evaluation supplied by the finite-family runner.
  * Invoke it exactly once before returning, or refuse without invoking it.
  * Do not retain it, retry it, or transfer it to another worker: scheduling,
  * resources, cancellation and commits belong to [[LocalExecution]].
  */
trait UnitInterpreter:
  def evaluate[C, O](
      evaluation: () => Either[UnitError, UnitEvaluation[C, O]]
  ): Either[UnitError, UnitEvaluation[C, O]]

object UnitInterpreter:
  val direct: UnitInterpreter = new UnitInterpreter:
    def evaluate[C, O](
        evaluation: () => Either[UnitError, UnitEvaluation[C, O]]
    ): Either[UnitError, UnitEvaluation[C, O]] =
      evaluation()
