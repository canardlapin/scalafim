package scalafim.fmri.mvpa

enum BindError[+Rejection]:
  case EstimandRejected[Rejection](
      estimand: EstimandIdentity,
      reason: Rejection,
      explanation: String
  ) extends BindError[Rejection]

  def message: String =
    this match
      case EstimandRejected(estimand, _, explanation) =>
        s"estimand '${estimand.kind.value}' rejected the scientific specification: $explanation"

/** Open compiler capability for one compatible source, design, frame rendition, and estimand. The compiler returns only
  * prepared scientific evidence. The central binder alone can turn that value into a bound plan.
  */
trait Compile[
    Source <: ScientificSource,
    Design <: EvidenceDesign,
    E <: Estimand[Source, Design],
    Rendition
]:
  type Prepared

  def prepare(
      specification: ScientificSpecification[Source, Design, E, Rendition]
  ): Either[specification.Rejection, Prepared]

object Compile:
  type Aux[
      Source <: ScientificSource,
      Design <: EvidenceDesign,
      E <: Estimand[Source, Design],
      Rendition,
      Prepared0
  ] = Compile[Source, Design, E, Rendition] { type Prepared = Prepared0 }

/** An always-valid scientific plan. Compiler instances can prepare evidence, but only the validating companion
  * admission can construct a bound plan.
  */
final class BoundScientificPlan[
    Source <: ScientificSource,
    Design <: EvidenceDesign,
    E <: Estimand[Source, Design],
    Rendition,
    Prepared
] private (
    val specification: ScientificSpecification[Source, Design, E, Rendition],
    val prepared: Prepared
):
  type Result = specification.Result
  type Rejection = specification.Rejection
  type Failure = specification.Failure

  def identity: ScientificPlanIdentity =
    specification.identity

object BoundScientificPlan:
  /** Validate estimand-specific capabilities and admit the prepared evidence.
    *
    * Construction and admission deliberately live in the same companion. A downstream source cannot gain construction
    * authority by declaring itself in `scalafim.fmri.mvpa`.
    */
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
    compiler
      .prepare(specification)
      .left
      .map: reason =>
        BindError.EstimandRejected(
          specification.estimand.identity,
          reason,
          specification.estimand.rejectionMessage(reason)
        )
      .map: prepared =>
        new BoundScientificPlan(specification, prepared)
