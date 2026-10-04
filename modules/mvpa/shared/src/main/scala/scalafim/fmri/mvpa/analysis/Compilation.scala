package scalafim.fmri.mvpa.analysis

import multivar.core.{SemanticSpace, ValueIdentity}
import resample4s.core.*
import scalafim.fmri.mvpa.{AxisDigest, AxisSignature, Observations, ValidationDesign}
import scalafim.fmri.mvpa.measurement.MeasurementFrame
import scalafim.response.{ProvenanceEvidence, ProvenanceNode, ProvenanceOperation}

/** An open scientific question. Methods own their result, rejection, failure,
  * and capability vocabulary; adding one never changes a central dispatch
  * table.
  */
trait Estimand[-Source, -Design, -Frame]:
  type Result
  type Rejection
  type Failure

  def id: EstimandId
  def requiredCapabilities: Set[CapabilityId]
  /** Method-owned scientific parameters. These are part of a plan, unlike
    * backend tuning or scheduling details.
    */
  def parameters: Vector[(String, String)] = Vector.empty

/** Metadata-only binding for one method's concrete input triple. The adapter
  * is deliberately open: a method can expose additional source, design, or
  * frame facts without extending a central identity registry. Implementations
  * must use already-certified metadata and must not inspect values.
  */
trait ScientificInputs[-Source, -Design, -Frame]:
  def sourceAxes(source: Source): Vector[AxisSignature]
  def sourceIdentity(source: Source): String
  def sourceCapabilities(source: Source): CapabilitySet
  def designAxis(design: Design): AxisSignature
  def designIdentity(design: Design): String
  def frameAxis(frame: Frame): AxisSignature
  def frameIdentity(frame: Frame): String

object ScientificInputs:
  /** Native evidence/design/frame binding. It reads only their established
    * identity and receipt metadata; it never applies an evidence or map
    * operator. */
  given observationsValidationFrame[S <: SemanticSpace, N <: SemanticSpace, SK, K, Cov <: resample4s.core.Coverage.Exact, R]
      : ScientificInputs[Observations[S, N], ValidationDesign[S, K, Cov], MeasurementFrame[N, SK, R]] with
    def sourceAxes(source: Observations[S, N]): Vector[AxisSignature] =
      Vector(source.identity.rows.coordinateSignature, source.identity.columns.coordinateSignature)

    def sourceIdentity(source: Observations[S, N]): String =
      val identity = source.identity
      metadataId: writer =>
        writer.string("observations")
        writer.string(identity.rows.stableKey)
        writer.string(identity.columns.stableKey)
        writer.string(identity.source.value)
        writeValueIdentity(writer, identity.values)
        writer.intLE(identity.provenanceRoots.length)
        identity.provenanceRoots.foreach(root => writer.string(root.value))
        writer.intLE(identity.provenanceNodes.length)
        identity.provenanceNodes.foreach(writeProvenanceNode(writer, _))
        identity.origins.writeFramed(writer)

    def sourceCapabilities(source: Observations[S, N]): CapabilitySet =
      // Observations may wrap a one-shot matrix-free operator. Representation
      // alone cannot certify replay, so metadata planning conservatively makes
      // no replay promise until an explicit source certificate is introduced.
      CapabilitySet.empty

    def designAxis(design: ValidationDesign[S, K, Cov]): AxisSignature =
      design.samples.descriptor.coordinateSignature

    def designIdentity(design: ValidationDesign[S, K, Cov]): String =
      metadataId(writer => writePlanReceipt(writer, design.receipt))

    def frameAxis(frame: MeasurementFrame[N, SK, R]): AxisSignature =
      frame.identity.source.coordinateSignature

    def frameIdentity(frame: MeasurementFrame[N, SK, R]): String =
      metadataId: writer =>
        writer.string("measurement-frame")
        writer.string(frame.identity.source.stableKey)
        // The declaration owns its canonical ordering, including duplicate
        // parameter names, so retain its authoritative fingerprint as one
        // length-prefixed metadata field.
        writer.string(frame.identity.declaration.fingerprint)

  private def metadataId(write: AxisDigest.Writer => Unit): String =
    AxisDigest.sha256Hex(write)

  private def writeValueIdentity(writer: AxisDigest.Writer, identity: ValueIdentity): Unit =
    identity match
      case ValueIdentity.Source(id) =>
        writer.string("value-source")
        writer.string(id.value)
      case ValueIdentity.Adjoint(of) =>
        writer.string("value-adjoint")
        writeValueIdentity(writer, of)
      case ValueIdentity.Composition(first, second) =>
        writer.string("value-composition")
        writeValueIdentity(writer, first)
        writeValueIdentity(writer, second)
      case ValueIdentity.Derived(operation, inputs) =>
        writer.string("value-derived")
        writer.string(operation)
        writer.intLE(inputs.length)
        inputs.foreach(writeValueIdentity(writer, _))

  private def writeProvenanceNode(writer: AxisDigest.Writer, node: ProvenanceNode): Unit =
    writer.string(node.id.value)
    node.operation match
      case ProvenanceOperation.SourceRead(source) =>
        writer.string("source-read")
        writer.string(source.value)
      case ProvenanceOperation.Selection => writer.string("selection")
      case ProvenanceOperation.Assembly => writer.string("assembly")
      case ProvenanceOperation.Adapter(adapter) =>
        writer.string("adapter")
        writer.string(adapter.value)
      case ProvenanceOperation.Derived(operation) =>
        writer.string("derived")
        writer.string(operation.value)
    writer.intLE(node.parents.length)
    node.parents.foreach(parent => writer.string(parent.value))
    writer.intLE(node.evidence.length)
    node.evidence.foreach:
      case ProvenanceEvidence.Domain(reference) =>
        writer.string("domain")
        writer.string(reference.namespace.value)
        writer.string(reference.value)
      case ProvenanceEvidence.External(reference) =>
        writer.string("external")
        writer.string(reference.namespace.value)
        writer.string(reference.value)
      case ProvenanceEvidence.NoneDeclared => writer.string("none-declared")

  private def writePlanReceipt(writer: AxisDigest.Writer, receipt: PlanReceipt): Unit =
    writer.string("plan-receipt")
    writer.string(receipt.algorithm.value)
    writeContentDigest(writer, receipt.design)
    writeFingerprint(writer, receipt.population)
    receipt.labels match
      case Some(labels) =>
        writer.string("labels-present")
        writeContentDigest(writer, labels)
      case None => writer.string("labels-absent")
    writer.string(java.lang.Long.toUnsignedString(receipt.seed.value))
    writeContentDigest(writer, receipt.assignment)

  private def writeContentDigest(writer: AxisDigest.Writer, digest: ContentDigest): Unit =
    writer.string("content-digest")
    writer.string(digest.algorithm.value)
    val bytes = digest.value.toIArray
    writer.intLE(bytes.length)
    bytes.foreach(byte => writer.intLE(byte & 0xff))

  private def writeFingerprint(writer: AxisDigest.Writer, fingerprint: Fingerprint): Unit =
    fingerprint match
      case digest: ContentDigest => writeContentDigest(writer, digest)
      case source: SourceIdentity =>
        writer.string("source-identity")
        writer.string(source.uri)
        writer.string(source.version)
      case summary: Summary =>
        writer.string("summary")
        writer.string(summary.policyId)
        writer.string(java.lang.Long.toUnsignedString(summary.value))

opaque type EstimandId = String

object EstimandId:
  def apply(value: String): EstimandId =
    require(valid(value), "estimand id must be a non-empty trimmed identifier")
    value

  extension (value: EstimandId) inline def text: String = value

  private def valid(value: String): Boolean =
    value.nonEmpty && value == value.trim && value.forall(character => character.isLetterOrDigit || character == '-' || character == '_')

opaque type PlanId = String

object PlanId:
  private[analysis] def derived(
      estimand: EstimandId,
      sourceAxes: Vector[AxisSignature],
      designAxis: AxisSignature,
      frameAxis: AxisSignature,
      sourceIdentity: String,
      designIdentity: String,
      frameIdentity: String,
      question: String,
      assumptions: Vector[String],
      preparation: Vector[String],
      reduction: String,
      estimandParameters: Vector[(String, String)],
      requiredCapabilities: Set[CapabilityId]
  ): PlanId =
    AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.mvpa.analysis.plan.v2")
      writer.string(estimand.text)
      writer.intLE(sourceAxes.length)
      sourceAxes.foreach(axis => writer.string(axis.value))
      writer.string(designAxis.value)
      writer.string(frameAxis.value)
      writer.string(sourceIdentity)
      writer.string(designIdentity)
      writer.string(frameIdentity)
      writer.string(question)
      writer.intLE(assumptions.length)
      assumptions.foreach(writer.string)
      writer.intLE(preparation.length)
      preparation.foreach(writer.string)
      writer.string(reduction)
      writer.intLE(estimandParameters.length)
      estimandParameters.foreach: (name, value) =>
        writer.string(name)
        writer.string(value)
      val required = requiredCapabilities.toVector.map(_.text).sorted
      writer.intLE(required.length)
      required.foreach(writer.string)

  extension (value: PlanId) inline def text: String = value

opaque type CapabilityId = String

object CapabilityId:
  def apply(value: String): CapabilityId =
    require(value.nonEmpty && value == value.trim, "capability id must be non-empty and trimmed")
    value

  extension (value: CapabilityId) inline def text: String = value

final case class Capability(id: CapabilityId, detail: String):
  require(detail.nonEmpty && detail == detail.trim, "capability detail must be non-empty and trimmed")

final case class CapabilitySet private (values: Map[CapabilityId, Capability]):
  def contains(id: CapabilityId): Boolean = values.contains(id)

  def get(id: CapabilityId): Option[Capability] = values.get(id)

  def missing(required: Set[CapabilityId]): Set[CapabilityId] =
    required.filterNot(contains)

object CapabilitySet:
  val empty: CapabilitySet = CapabilitySet(Map.empty)

  def from(values: Iterable[Capability]): CapabilitySet =
    CapabilitySet(values.iterator.map(value => value.id -> value).toMap)

enum CapabilityAdmission:
  case Admitted(provided: CapabilitySet)
  case Unsupported(missing: Set[CapabilityId], provided: CapabilitySet)

object CapabilityAdmission:
  def evaluate(required: Set[CapabilityId], provided: CapabilitySet): CapabilityAdmission =
    val missing = provided.missing(required)
    if missing.isEmpty then Admitted(provided) else Unsupported(missing, provided)

/** Pure request: changing a backend, schedule, or cache must not change this
  * object or its scientific identity.
  */
final class AnalysisSpecification[Source, Design, Frame, E <: Estimand[Source, Design, Frame]] private[analysis] (
    val source: Source,
    val design: Design,
    val frame: Frame,
    val estimand: E,
    val estimandId: EstimandId,
    val requiredCapabilities: Set[CapabilityId],
    val sourceAxes: Vector[AxisSignature],
    val designAxis: AxisSignature,
    val frameAxis: AxisSignature,
    val sourceIdentity: String,
    val designIdentity: String,
    val frameIdentity: String,
    val sourceCapabilities: CapabilitySet,
    val question: String,
    val assumptions: Vector[String],
    val preparation: Vector[String],
    val reduction: String,
    val estimandParameters: Vector[(String, String)],
    val plan: PlanId
):
  require(sourceAxes.nonEmpty, "scientific source must expose at least one axis signature")
  require(question.nonEmpty && question == question.trim, "scientific question must be non-empty and trimmed")
  require(assumptions.forall(value => value.nonEmpty && value == value.trim), "scientific assumptions must be non-empty and trimmed")
  require(preparation.forall(value => value.nonEmpty && value == value.trim), "preparation steps must be non-empty and trimmed")
  require(reduction.nonEmpty && reduction == reduction.trim, "reduction must be non-empty and trimmed")
  require(estimandParameters.forall((name, value) => name.nonEmpty && name == name.trim && value.nonEmpty && value == value.trim), "estimand parameters must be non-empty and trimmed")

object AnalysisSpecification:
  def from[Source, Design, Frame, E <: Estimand[Source, Design, Frame]](
      source: Source,
      design: Design,
      frame: Frame,
      estimand: E,
      question: String,
      assumptions: Vector[String],
      preparation: Vector[String],
      reduction: String
  )(using inputs: ScientificInputs[Source, Design, Frame]): AnalysisSpecification[Source, Design, Frame, E] =
    val sourceAxes = inputs.sourceAxes(source)
    val designAxis = inputs.designAxis(design)
    val frameAxis = inputs.frameAxis(frame)
    val sourceIdentity = inputs.sourceIdentity(source)
    val designIdentity = inputs.designIdentity(design)
    val frameIdentity = inputs.frameIdentity(frame)
    val estimandParameters = estimand.parameters
    val estimandId = estimand.id
    val requiredCapabilities = estimand.requiredCapabilities
    new AnalysisSpecification(
      source, design, frame, estimand, estimandId, requiredCapabilities, sourceAxes, designAxis, frameAxis,
      sourceIdentity, designIdentity, frameIdentity, inputs.sourceCapabilities(source),
      question, assumptions, preparation, reduction, estimandParameters,
      PlanId.derived(estimandId, sourceAxes, designAxis, frameAxis, sourceIdentity, designIdentity, frameIdentity, question, assumptions, preparation, reduction, estimandParameters, requiredCapabilities)
    )

/** The scientific receipt is immutable evidence of what was requested and
  * bound. It deliberately has no backend or scheduling fields.
  */
final class ScientificReceipt private[analysis] (
    val plan: PlanId,
    val estimand: EstimandId,
    val sourceAxes: Vector[AxisSignature],
    val designAxis: AxisSignature,
    val frameAxis: AxisSignature,
    val sourceIdentity: String,
    val designIdentity: String,
    val frameIdentity: String,
    val question: String,
    val assumptions: Vector[String],
    val preparation: Vector[String],
    val reduction: String,
    val estimandParameters: Vector[(String, String)]
):
  require(sourceIdentity.nonEmpty && designIdentity.nonEmpty && frameIdentity.nonEmpty, "scientific identities must be non-empty")

object ScientificReceipt:
  private[analysis] def from[Source, Design, Frame, E <: Estimand[Source, Design, Frame]](
      specification: AnalysisSpecification[Source, Design, Frame, E]
  ): ScientificReceipt =
    new ScientificReceipt(
      specification.plan, specification.estimandId, specification.sourceAxes,
      specification.designAxis, specification.frameAxis, specification.sourceIdentity,
      specification.designIdentity, specification.frameIdentity, specification.question,
      specification.assumptions, specification.preparation, specification.reduction,
      specification.estimandParameters
    )

/** Provider-reported evidence identity. These are provider assertions, not
  * locally validated verification proofs; a claimed digest is never promoted
  * beyond the assurance reported by the producing provider.
  */
final case class EvidenceReceipt(
    declaredRevision: String,
    verifiedReadBlocks: Vector[String],
    verifiedCompleteContent: Option[String]
):
  require(declaredRevision.nonEmpty && declaredRevision == declaredRevision.trim, "declared revision must be non-empty and trimmed")
  require(verifiedReadBlocks.forall(value => value.nonEmpty && value == value.trim), "verified block ids must be non-empty and trimmed")
  require(verifiedCompleteContent.forall(value => value.nonEmpty && value == value.trim), "complete content identity must be non-empty and trimmed")

/** Concrete code and resource realization. These facts may vary for the same
  * scientific plan and therefore cannot be used as its identity.
  */
final case class RealizationReceipt(
    backend: String,
    implementation: String,
    precision: String,
    randomStreams: Vector[String],
    materializations: Vector[String],
    resourceSummary: String
):
  private val fields = Vector(backend, implementation, precision, resourceSummary) ++ randomStreams ++ materializations
  require(fields.forall(value => value.nonEmpty && value == value.trim), "realization receipt fields must be non-empty and trimmed")

final case class ExecutionReceipt(
    scientific: ScientificReceipt,
    evidence: EvidenceReceipt,
    realization: RealizationReceipt,
    completedUnits: Int,
    expectedUnits: Int
):
  require(completedUnits >= 0 && expectedUnits >= 0 && completedUnits <= expectedUnits, "execution coverage must be in [0, expected]")

enum BindError[+Rejection]:
  case Rejected(reason: Rejection)
  case Unsupported(missing: Set[CapabilityId])
  case Invalid(reason: String)

/** A binding fixes source/design/frame identities and proves capability
  * admission before any numerical work is chosen.
  */
final class BoundScientificPlan[
    Source,
    Design,
    Frame,
    E <: Estimand[Source, Design, Frame],
    B,
    P
] private[analysis] (
    val specification: AnalysisSpecification[Source, Design, Frame, E],
    val bound: B,
    val admission: CapabilityAdmission.Admitted,
    val receipt: ScientificReceipt,
    private[analysis] val compiler: AnalysisCompiler[Source, Design, Frame, E] {
      type Bound = B
      type Program = P
    }
)

/** Method-specific sufficient-statistic and algorithm declaration. This is
  * deliberately separate from binding and execution strategy.
  */
final case class NumericalProgram[
    Source,
    Design,
    Frame,
    E <: Estimand[Source, Design, Frame],
    Bound,
    Program
](
    scientific: BoundScientificPlan[Source, Design, Frame, E, Bound, Program],
    program: Program,
    algorithm: String,
    sufficientStatistics: Vector[String]
):
  require(algorithm.nonEmpty && algorithm == algorithm.trim, "algorithm must be non-empty and trimmed")
  require(sufficientStatistics.forall(value => value.nonEmpty && value == value.trim), "sufficient-statistic descriptions must be non-empty and trimmed")

final case class ExecutionStrategy(
    backend: String,
    scheduling: String,
    chunkSize: Int,
    cancellationRequested: () => Boolean
):
  require(backend.nonEmpty && backend == backend.trim, "backend must be non-empty and trimmed")
  require(scheduling.nonEmpty && scheduling == scheduling.trim, "scheduling must be non-empty and trimmed")
  require(chunkSize > 0, "chunk size must be positive")

final case class ExecutionPlan[
    Source,
    Design,
    Frame,
    E <: Estimand[Source, Design, Frame],
    Bound,
    Program
](
    numerical: NumericalProgram[Source, Design, Frame, E, Bound, Program],
    strategy: ExecutionStrategy
):
  def admit(candidates: Vector[ResourceCandidate], budget: ResourceBudget): Either[ResourceError, ResourceAdmission] =
    ResourceAdmission.choose(numerical.scientific.specification.plan, candidates, budget)

/** The only extension point required for a new method. There is no global
  * method enum, registry, codec, or dispatch branch.
  */
trait AnalysisCompiler[Source, Design, Frame, E <: Estimand[Source, Design, Frame]]:
  type Bound
  type Program

  def bind(
      specification: AnalysisSpecification[Source, Design, Frame, E],
      available: CapabilitySet
  ): Either[BindError[specification.estimand.Rejection], Bound]

  def numerical(
      scientific: BoundScientificPlan[Source, Design, Frame, E, Bound, Program]
  ): NumericalProgram[Source, Design, Frame, E, Bound, Program]

object AnalysisCompiler:
  def bind[Source, Design, Frame, E <: Estimand[Source, Design, Frame]](
      specification: AnalysisSpecification[Source, Design, Frame, E],
      available: CapabilitySet
  )(using
      compiler: AnalysisCompiler[Source, Design, Frame, E]
  ): Either[
    BindError[specification.estimand.Rejection],
    BoundScientificPlan[Source, Design, Frame, E, compiler.Bound, compiler.Program]
  ] =
    val actual = specification.sourceCapabilities
    val admittedCapabilities = CapabilitySet.from(
      specification.requiredCapabilities.toVector.flatMap: capability =>
        if actual.contains(capability) && available.contains(capability) then actual.get(capability) else None
    )
    CapabilityAdmission.evaluate(specification.requiredCapabilities, admittedCapabilities) match
      case CapabilityAdmission.Unsupported(missing, _) => Left(BindError.Unsupported(missing))
      case admitted: CapabilityAdmission.Admitted =>
        compiler.bind(specification, available).map: bound =>
          new BoundScientificPlan(
            specification, bound, admitted,
            ScientificReceipt.from(specification),
            compiler
          )

  def compile[Source, Design, Frame, E <: Estimand[Source, Design, Frame], Bound, Program](
      scientific: BoundScientificPlan[Source, Design, Frame, E, Bound, Program]
  ): NumericalProgram[Source, Design, Frame, E, Bound, Program] =
    scientific.compiler.numerical(scientific)

/** Products are explicit lifecycle states. Numerical zero can appear only in
  * Valid or Partial values, never as a surrogate for a missing product.
  */
enum ProductState[+A, +Unsupported, +NonEstimable, +Failure]:
  case Unrequested(reason: String)
  case Unsupported(reason: Unsupported)
  case NonEstimable(reason: NonEstimable)
  case Failed(error: Failure)
  case Cancelled(reason: String)
  case Partial(product: PartialProduct[A])
  case Valid(value: A)

final case class PartialProduct[+A](value: A, completedContributors: Int, expectedContributors: Int):
  require(completedContributors >= 0 && expectedContributors >= 0 && completedContributors <= expectedContributors, "partial contributor counts must be in [0, expected]")

trait FrameEntry[+Id, +Rendition, Value[_ <: SemanticSpace], +Unsupported, +NonEstimable, +Failure]:
  type Local <: SemanticSpace
  def id: Id
  def rendition: Rendition
  def state: ProductState[Value[Local], Unsupported, NonEstimable, Failure]

object FrameEntry:
  def apply[Id, Rendition, Value[_ <: SemanticSpace], L <: SemanticSpace, Unsupported, NonEstimable, Failure](
      entryId: Id,
      entryRendition: Rendition,
      entryState: ProductState[Value[L], Unsupported, NonEstimable, Failure]
  ): FrameEntry[Id, Rendition, Value, Unsupported, NonEstimable, Failure] { type Local = L } =
    new FrameEntry[Id, Rendition, Value, Unsupported, NonEstimable, Failure]:
      type Local = L
      val id = entryId
      val rendition = entryRendition
      val state = entryState

/** Frame-local outcomes can vary in value type through their method-owned
  * rendering, while their lifecycle status remains visible for every entry.
  */
final case class FrameResult[Id, Rendition, Value[_ <: SemanticSpace], Unsupported, NonEstimable, Failure](
    entries: Vector[FrameEntry[Id, Rendition, Value, Unsupported, NonEstimable, Failure]]
)

/** Global methods return `AnalysisResult[Artifact]` directly. Frame methods
  * return `AnalysisResult[FrameResult[...]]`; a global result is never forced
  * through a synthetic singleton measurement.
  */
final class AnalysisResult[A] private[analysis] (
    val plan: PlanId,
    val value: A,
    val receipt: ExecutionReceipt,
    val exposure: EvidenceExposure
):
  require(plan == receipt.scientific.plan, "analysis result and scientific receipt must name the same plan")
  require(exposure.reference.plan == plan, "analysis result exposure must name the same plan")

object AnalysisResult:
  def complete[Source, Design, Frame, E <: Estimand[Source, Design, Frame], B, P](
      scientific: BoundScientificPlan[Source, Design, Frame, E, B, P],
      value: scientific.specification.estimand.Result,
      receipt: ExecutionReceipt
  ): AnalysisResult[scientific.specification.estimand.Result] =
    require(receipt.scientific.plan == scientific.specification.plan, "result receipt must belong to the bound scientific plan")
    completeWithExposure(scientific, value, receipt, defaultExposure(scientific, receipt))

  /** Carries the exact passed exposure snapshot into the result boundary. */
  def completeWithExposure[Source, Design, Frame, E <: Estimand[Source, Design, Frame], B, P](
      scientific: BoundScientificPlan[Source, Design, Frame, E, B, P],
      value: scientific.specification.estimand.Result,
      receipt: ExecutionReceipt,
      exposure: EvidenceExposure
  ): AnalysisResult[scientific.specification.estimand.Result] =
    require(receipt.scientific.plan == scientific.specification.plan, "result receipt must belong to the bound scientific plan")
    require(exposure.reference.plan == scientific.specification.plan, "result exposure must belong to the bound scientific plan")
    new AnalysisResult(scientific.specification.plan, value, receipt, exposure)

  private def defaultExposure[Source, Design, Frame, E <: Estimand[Source, Design, Frame], B, P](
      scientific: BoundScientificPlan[Source, Design, Frame, E, B, P],
      receipt: ExecutionReceipt
  ): EvidenceExposure =
    val result = ResultIdentity(AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.mvpa.analysis-result.v1")
      writer.string(scientific.specification.plan.text)
      writer.string(receipt.evidence.declaredRevision)
    )
    EvidenceExposure.external(ExposureReference(
      scientific.specification.plan,
      scientific.specification.sourceIdentity,
      scientific.specification.sourceIdentity,
      result
    ))

opaque type ReductionWeight = Double

object ReductionWeight:
  def apply(value: Double): ReductionWeight =
    require(value.isFinite && value >= 0.0, "reduction weight must be finite and non-negative")
    value

  extension (value: ReductionWeight) inline def toDouble: Double = value

final case class ExcludedContributor[Id](id: Id, reason: String):
  require(reason.nonEmpty && reason == reason.trim, "exclusion reason must be non-empty and trimmed")

final case class ReductionEvidence[Id](
    contributors: Vector[(Id, ReductionWeight)],
    excluded: Vector[ExcludedContributor[Id]],
    denominator: Double,
    exactNumerator: Option[BigInt],
    exactDenominator: BigInt
):
  require(denominator.isFinite && denominator >= 0.0, "reduction denominator must be finite and non-negative")
  require(contributors.map(_._1).distinct.length == contributors.length, "reduction contributors must be unique")
  require(exactNumerator.forall(_ >= 0), "exact numerator must be non-negative")
  require(exactDenominator >= 0, "exact denominator must be non-negative")

final case class RunAccuracy[Id](id: Id, correctTrials: Long, trials: Long):
  require(correctTrials >= 0L && trials >= 0L && correctTrials <= trials, "run accuracy counts must satisfy 0 <= correct <= trials")

final case class PooledTrialAccuracy[Id](value: Double, evidence: ReductionEvidence[Id])
final case class MeanRunAccuracy[Id](value: Double, evidence: ReductionEvidence[Id])

enum AccuracyReductionResult[Id, +A]:
  case Valid(value: A)
  case NonEstimable(evidence: ReductionEvidence[Id], reason: String)
  case Overflow(evidence: ReductionEvidence[Id], reason: String)
  case DuplicateContributor(id: Id)

object AccuracyReduction:
  def pooledTrial[Id](runs: Vector[RunAccuracy[Id]]): AccuracyReductionResult[Id, PooledTrialAccuracy[Id]] =
    duplicate(runs) match
      case Some(id) => return AccuracyReductionResult.DuplicateContributor(id)
      case None     => ()
    val valid = runs.filter(_.trials > 0L)
    val excluded = runs.collect:
      case run if run.trials == 0L => ExcludedContributor(run.id, "run contains no assessable trials")
    val denominator = valid.foldLeft(BigInt(0))((total, run) => total + BigInt(run.trials))
    val numerator = valid.foldLeft(BigInt(0))((total, run) => total + BigInt(run.correctTrials))
    val evidence = ReductionEvidence(
      valid.map(run => run.id -> ReductionWeight(run.trials.toDouble)),
      excluded,
      denominator.toDouble,
      Some(numerator),
      denominator
    )
    if denominator > 0 then AccuracyReductionResult.Valid(PooledTrialAccuracy(numerator.toDouble / denominator.toDouble, evidence))
    else AccuracyReductionResult.NonEstimable(evidence, "no assessable trial contributors")

  def meanRun[Id](runs: Vector[RunAccuracy[Id]]): AccuracyReductionResult[Id, MeanRunAccuracy[Id]] =
    duplicate(runs) match
      case Some(id) => return AccuracyReductionResult.DuplicateContributor(id)
      case None     => ()
    val valid = runs.filter(_.trials > 0L)
    val excluded = runs.collect:
      case run if run.trials == 0L => ExcludedContributor(run.id, "run contains no assessable trials")
    val denominator = valid.length.toDouble
    val sum = valid.foldLeft(0.0): (accumulated, run) =>
      accumulated + run.correctTrials.toDouble / run.trials.toDouble
    val evidence = ReductionEvidence(
      valid.map(run => run.id -> ReductionWeight(1.0)),
      excluded,
      denominator,
      None,
      BigInt(valid.length)
    )
    if valid.isEmpty then AccuracyReductionResult.NonEstimable(evidence, "no assessable run contributors")
    else AccuracyReductionResult.Valid(MeanRunAccuracy(sum / denominator, evidence))

  private def duplicate[Id](runs: Vector[RunAccuracy[Id]]): Option[Id] =
    runs.groupBy(_.id).collectFirst { case (id, values) if values.length > 1 => id }
