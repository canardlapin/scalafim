package scalafim.fmri.mvpa

enum MvpaRunError[+Rejection]:
  case Specification(error: ScientificSpecificationError)
  case Binding[Rejection](error: BindError[Rejection]) extends MvpaRunError[Rejection]
  case Planning(error: ExecutionPlanError)
  case Execution(error: AnalysisExecutionError)

  def message: String =
    this match
      case Specification(error) => s"specification stage: ${error.message}"
      case Binding(error)       => s"binding stage: ${error.message}"
      case Planning(error)      => s"planning stage: ${error.message}"
      case Execution(error)     => s"execution stage: ${error.message}"

/** One discoverable entry to the identified-evidence pipeline. Every method delegates to the canonical source, design,
  * frame, estimand, plan, result, and receipt types; the facade introduces no parallel analysis ontology.
  */
object Mvpa:
  def inspect[
      Source <: ScientificSource,
      Design <: EvidenceDesign,
      E <: Estimand[Source, Design],
      Rendition
  ](
      specification: ScientificSpecification[Source, Design, E, Rendition]
  ): ScientificPlanInspection =
    ScientificPlanInspection.from(specification)

  def inspect[
      Source <: ScientificSource,
      Design <: EvidenceDesign,
      E <: Estimand[Source, Design],
      Rendition,
      Prepared
  ](
      plan: BoundScientificPlan[Source, Design, E, Rendition, Prepared]
  ): ScientificPlanInspection =
    inspect(plan.specification)

  def inspect[
      Source <: ScientificSource,
      Design <: EvidenceDesign,
      E <: Estimand[Source, Design],
      Rendition,
      Prepared
  ](
      plan: ExecutionPlan[Source, Design, E, Rendition, Prepared]
  ): ScientificPlanInspection =
    inspect(plan.scientific)

  def specify[
      Source <: ScientificSource,
      Design <: EvidenceDesign,
      E <: Estimand[Source, Design],
      Rendition
  ](
      source: Source
  )(
      design: Design,
      frame: MeasurementFrame[source.Neural, source.NeuralKey, Rendition],
      estimand: E
  ): Either[
    ScientificSpecificationError,
    ScientificSpecification[Source, Design, E, Rendition]
  ] =
    ScientificSpecification(source)(design, frame, estimand)

  def bind[
      Source <: ScientificSource,
      Design <: EvidenceDesign,
      E <: Estimand[Source, Design],
      Rendition
  ](
      specification: ScientificSpecification[Source, Design, E, Rendition]
  )(using
      compiler: Compile[Source, Design, E, Rendition]
  ): Either[
    BindError[specification.Rejection],
    BoundScientificPlan[Source, Design, E, Rendition, compiler.Prepared]
  ] =
    BoundScientificPlan.bind(specification)

  def plan[
      Source <: ScientificSource,
      Design <: EvidenceDesign,
      E <: Estimand[Source, Design],
      Rendition,
      Prepared
  ](
      scientific: BoundScientificPlan[Source, Design, E, Rendition, Prepared],
      strategy: ExecutionStrategy
  )(using
      task: MeasurementTask[Source, Design, E, Rendition, Prepared]
  ): Either[
    ExecutionPlanError,
    ExecutionPlan[Source, Design, E, Rendition, Prepared]
  ] =
    ExecutionPlan(scientific, strategy)

  def execute[
      Source <: ScientificSource,
      Design <: EvidenceDesign,
      E <: Estimand[Source, Design],
      Rendition,
      Prepared
  ](
      plan: ExecutionPlan[Source, Design, E, Rendition, Prepared]
  ): Either[
    AnalysisExecutionError,
    AnalysisExecution[Source, Design, E, Rendition, Prepared]
  ] =
    AnalysisExecution(plan)

  def run[
      Source <: ScientificSource,
      Design <: EvidenceDesign,
      Result0,
      Rejection0,
      Failure0,
      E <: Estimand[Source, Design] {
        type Result = Result0
        type Rejection = Rejection0
        type Failure = Failure0
      },
      Rendition
  ](
      source: Source
  )(
      design: Design,
      frame: MeasurementFrame[source.Neural, source.NeuralKey, Rendition],
      estimand: E,
      strategy: ExecutionStrategy
  )(using
      compiler: Compile[Source, Design, E, Rendition],
      task: MeasurementTask[Source, Design, E, Rendition, compiler.Prepared]
  ): Either[
    MvpaRunError[Rejection0],
    AnalysisResult[Result0, Rejection0, Failure0, Rendition]
  ] =
    specify(source)(design, frame, estimand).left
      .map(MvpaRunError.Specification.apply)
      .flatMap: specification =>
        bind(specification).left
          .map(MvpaRunError.Binding.apply)
          .flatMap: scientific =>
            plan(scientific, strategy).left
              .map(MvpaRunError.Planning.apply)
              .flatMap: executionPlan =>
                execute(executionPlan).left
                  .map(MvpaRunError.Execution.apply)
                  .flatMap: execution =>
                    execution.collect.left.map(MvpaRunError.Execution.apply)
