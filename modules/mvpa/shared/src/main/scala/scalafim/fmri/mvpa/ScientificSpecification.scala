package scalafim.fmri.mvpa

enum ScientificSpecificationError:
  case MissingSourceAxis(name: ScientificAxisName)
  case SourceAxisMismatch(
      name: ScientificAxisName,
      declared: AxisFingerprint,
      provided: AxisFingerprint
  )
  case FrameSourceMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case FrameSourceWitnessMismatch
  case UnknownDesignAxis(name: ScientificAxisName)
  case DesignAxisMismatch(
      name: ScientificAxisName,
      expected: AxisFingerprint,
      actual: AxisFingerprint
  )
  case Identity(error: ScientificIdentityError)

  def message: String =
    this match
      case MissingSourceAxis(name) =>
        s"scientific source does not declare its neural axis '${name.value}'"
      case SourceAxisMismatch(name, declared, provided) =>
        s"scientific source axis '${name.value}' declares ${declared.value}, but exposes ${provided.value}"
      case FrameSourceMismatch(expected, actual) =>
        s"measurement frame source ${actual.value} does not match neural source ${expected.value}"
      case FrameSourceWitnessMismatch =>
        "measurement frame and scientific source use different nominal neural witnesses"
      case UnknownDesignAxis(name) =>
        s"evidence design references unknown source axis '${name.value}'"
      case DesignAxisMismatch(name, expected, actual) =>
        s"evidence design axis '${name.value}' is ${actual.value}, expected ${expected.value}"
      case Identity(error) =>
        error.message

/** A pure scientific request. Execution strategy, solver settings, storage, scheduling, random seeds, and
  * materialization policy cannot be represented here and therefore cannot affect [[identity]].
  */
final class ScientificSpecification[
    Source <: ScientificSource,
    Design <: EvidenceDesign,
    E <: Estimand[Source, Design],
    Rendition
] private (
    val source: Source
)(
    val design: Design,
    val frame: MeasurementFrame[source.Neural, source.NeuralKey, Rendition],
    val estimand: E,
    val normalization: NormalizationIdentity,
    val boundaries: RequestedBoundaries,
    val identity: ScientificPlanIdentity
):
  type Result = estimand.Result
  type Rejection = estimand.Rejection
  type Failure = estimand.Failure

object ScientificSpecification:
  def apply[
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
    requesting(source)(
      design,
      frame,
      estimand,
      NormalizationIdentity.none,
      estimand.defaultBoundaries
    )

  def requesting[
      Source <: ScientificSource,
      Design <: EvidenceDesign,
      E <: Estimand[Source, Design],
      Rendition
  ](
      source: Source
  )(
      design: Design,
      frame: MeasurementFrame[source.Neural, source.NeuralKey, Rendition],
      estimand: E,
      normalization: NormalizationIdentity,
      boundaries: RequestedBoundaries
  ): Either[
    ScientificSpecificationError,
    ScientificSpecification[Source, Design, E, Rendition]
  ] =
    for
      declaredNeural <- source.identity
        .axis(source.neuralAxisName)
        .toRight(ScientificSpecificationError.MissingSourceAxis(source.neuralAxisName))
      _ <-
        if declaredNeural == source.neuralAxis.identity then Right(())
        else
          Left(
            ScientificSpecificationError.SourceAxisMismatch(
              source.neuralAxisName,
              declaredNeural.fingerprint,
              source.neuralAxis.identity.fingerprint
            )
          )
      _ <-
        if frame.source.identity == source.neuralAxis.identity then Right(())
        else
          Left(
            ScientificSpecificationError.FrameSourceMismatch(
              source.neuralAxis.identity.fingerprint,
              frame.source.identity.fingerprint
            )
          )
      _ <-
        if frame.source.evidence eq source.neuralAxis.evidence then Right(())
        else Left(ScientificSpecificationError.FrameSourceWitnessMismatch)
      planIdentity <- ScientificPlanIdentity
        .fromSpecification(
          source.identity,
          design.identity,
          design.referencedAxes,
          frame.identity,
          estimand.identity,
          normalization,
          boundaries
        )
        .left
        .map:
          case ScientificIdentityError.UnknownDesignAxis(name) =>
            ScientificSpecificationError.UnknownDesignAxis(name)
          case ScientificIdentityError.DesignAxisMismatch(name, expected, actual) =>
            ScientificSpecificationError.DesignAxisMismatch(name, expected, actual)
          case error => ScientificSpecificationError.Identity(error)
    yield new ScientificSpecification(source)(
      design,
      frame,
      estimand,
      normalization,
      boundaries,
      planIdentity
    )
