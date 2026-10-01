package scalafim.fmri.mvpa

import multivar.core.SemanticSpace
import resample4s.core.*

/** Root randomness for a scientific analysis. Provider seeds are derived internally from this root and the complete
  * sample-axis signature, never from worker ids or traversal order.
  */
opaque type ScientificSeed = Seed

object ScientificSeed:
  def fromLong(value: Long): ScientificSeed =
    Seed.fromLong(value)

  private[mvpa] def providerSeed(seed: ScientificSeed): Seed =
    seed

final class ValidationUnit[S <: SemanticSpace, K] private[mvpa] (
    val key: UnitKey,
    val analysis: ReindexingLeg[S, K, Selection],
    val assessment: ReindexingLeg[S, K, Selection]
)

/** Identified validation. `Cov` remains provider-certified exact coverage, including the distinction between one and
  * repeated exact passes.
  */
final class ValidationDesign[
    S <: SemanticSpace,
    K,
    Cov <: Coverage.Exact
] private[mvpa] (
    val samples: AxisRef[K] { type Id = S },
    val design: Design[Split[Selection], Cov],
    private val compiled: Compiled[Split[Selection], Cov],
    val receipt: PlanReceipt
):
  val plan: Plan[Split[Selection], Cov] =
    compiled.plan

  val diagnostics: PlanDiagnostics =
    compiled.diagnostics

  val cost: PlanCost =
    compiled.cost

  def keys: IndexedSeq[UnitKey] =
    plan.keys

  def at(key: UnitKey): Either[EvidenceError, ValidationUnit[S, K]] =
    plan
      .at(key)
      .left
      .map(EvidenceError.UnknownResampleUnit.apply)
      .flatMap: split =>
        for
          analysis <- ReindexingLeg.bind(samples, split.analysis)
          assessment <- ReindexingLeg.bind(samples, split.assessment)
        yield new ValidationUnit(key, analysis, assessment)

object ValidationDesign:
  def bind[K, Cov <: Coverage.Exact](
      samples: AxisRef[K],
      design: Design[Split[Selection], Cov],
      seed: ScientificSeed
  )(using algorithm: DigestAlgorithm): Either[EvidenceError, ValidationDesign[samples.Id, K, Cov]] =
    EvidenceDesignBinding
      .compile(samples, design, seed, EvidenceDesignPurpose.Validation)
      .map: bound =>
        new ValidationDesign(samples, design, bound.compiled, bound.receipt)

final class CrossFitUnit[S <: SemanticSpace, K] private[mvpa] (
    val key: UnitKey,
    val analysis: ReindexingLeg[S, K, Selection],
    val assessment: ReindexingLeg[S, K, Selection]
)

/** Identified single-pass cross-fitting. Construction requires the provider's stronger `ExactOnce` witness. */
final class CrossFitDesign[S <: SemanticSpace, K] private[mvpa] (
    val samples: AxisRef[K] { type Id = S },
    val design: Design[Split[Selection], Coverage.ExactOnce],
    private val compiled: Compiled[Split[Selection], Coverage.ExactOnce],
    val receipt: PlanReceipt
):
  val plan: Plan[Split[Selection], Coverage.ExactOnce] =
    compiled.plan

  val diagnostics: PlanDiagnostics =
    compiled.diagnostics

  val cost: PlanCost =
    compiled.cost

  def keys: IndexedSeq[UnitKey] =
    plan.keys

  def at(key: UnitKey): Either[EvidenceError, CrossFitUnit[S, K]] =
    plan
      .at(key)
      .left
      .map(EvidenceError.UnknownResampleUnit.apply)
      .flatMap: split =>
        for
          analysis <- ReindexingLeg.bind(samples, split.analysis)
          assessment <- ReindexingLeg.bind(samples, split.assessment)
        yield new CrossFitUnit(key, analysis, assessment)

object CrossFitDesign:
  def bind[K](
      samples: AxisRef[K],
      design: Design[Split[Selection], Coverage.ExactOnce],
      seed: ScientificSeed
  )(using algorithm: DigestAlgorithm): Either[EvidenceError, CrossFitDesign[samples.Id, K]] =
    EvidenceDesignBinding
      .compile(samples, design, seed, EvidenceDesignPurpose.CrossFit)
      .map: bound =>
        new CrossFitDesign(samples, design, bound.compiled, bound.receipt)

final class BootstrapUnit[S <: SemanticSpace, K] private[mvpa] (
    val key: UnitKey,
    val analysis: ReindexingLeg[S, K, Draw],
    val assessment: ReindexingLeg[S, K, Selection]
)

/** Identified bootstrap. Repeated analysis rows remain draws and never acquire exact-coverage evidence. */
final class BootstrapDesign[S <: SemanticSpace, K] private[mvpa] (
    val samples: AxisRef[K] { type Id = S },
    val design: Design[Split[Draw], Coverage],
    private val compiled: Compiled[Split[Draw], Coverage],
    val receipt: PlanReceipt
):
  val plan: Plan[Split[Draw], Coverage] =
    compiled.plan

  val diagnostics: PlanDiagnostics =
    compiled.diagnostics

  val cost: PlanCost =
    compiled.cost

  def keys: IndexedSeq[UnitKey] =
    plan.keys

  def at(key: UnitKey): Either[EvidenceError, BootstrapUnit[S, K]] =
    plan
      .at(key)
      .left
      .map(EvidenceError.UnknownResampleUnit.apply)
      .flatMap: split =>
        for
          analysis <- ReindexingLeg.bind(samples, split.analysis)
          assessment <- ReindexingLeg.bind(samples, split.assessment)
        yield new BootstrapUnit(key, analysis, assessment)

object BootstrapDesign:
  def bind[K](
      samples: AxisRef[K],
      design: Design[Split[Draw], Coverage],
      seed: ScientificSeed
  )(using algorithm: DigestAlgorithm): Either[EvidenceError, BootstrapDesign[samples.Id, K]] =
    EvidenceDesignBinding
      .compile(samples, design, seed, EvidenceDesignPurpose.Bootstrap)
      .map: bound =>
        new BootstrapDesign(samples, design, bound.compiled, bound.receipt)

final class RandomizationUnit[S <: SemanticSpace, K] private[mvpa] (
    val key: UnitKey,
    val permutation: ReindexingLeg[S, K, Permutation]
)

/** Identified randomization. Exchangeability policy stays in the concrete provider design rather than a weak shared
  * ScalaFIM enum.
  */
final class RandomizationDesign[S <: SemanticSpace, K] private[mvpa] (
    val samples: AxisRef[K] { type Id = S },
    val design: Design[Permutation, Coverage],
    private val compiled: Compiled[Permutation, Coverage],
    val receipt: PlanReceipt
):
  val plan: Plan[Permutation, Coverage] =
    compiled.plan

  val diagnostics: PlanDiagnostics =
    compiled.diagnostics

  val cost: PlanCost =
    compiled.cost

  def keys: IndexedSeq[UnitKey] =
    plan.keys

  def at(key: UnitKey): Either[EvidenceError, RandomizationUnit[S, K]] =
    plan
      .at(key)
      .left
      .map(EvidenceError.UnknownResampleUnit.apply)
      .flatMap: permutation =>
        ReindexingLeg.bind(samples, permutation).map(new RandomizationUnit(key, _))

object RandomizationDesign:
  def bind[K](
      samples: AxisRef[K],
      design: Design[Permutation, Coverage],
      seed: ScientificSeed
  )(using algorithm: DigestAlgorithm): Either[EvidenceError, RandomizationDesign[samples.Id, K]] =
    EvidenceDesignBinding
      .compile(samples, design, seed, EvidenceDesignPurpose.Randomization)
      .map: bound =>
        new RandomizationDesign(samples, design, bound.compiled, bound.receipt)

private enum EvidenceDesignPurpose(val streamTag: Int):
  case Validation extends EvidenceDesignPurpose(201)
  case CrossFit extends EvidenceDesignPurpose(202)
  case Bootstrap extends EvidenceDesignPurpose(203)
  case Randomization extends EvidenceDesignPurpose(204)

private final case class CompiledEvidenceDesign[A, Cov <: Coverage](
    compiled: Compiled[A, Cov],
    receipt: PlanReceipt
)

private object EvidenceDesignBinding:
  private val AxisSignatureStreamTag = 205

  def compile[K, A, Cov <: Coverage](
      samples: AxisRef[K],
      design: Design[A, Cov],
      root: ScientificSeed,
      purpose: EvidenceDesignPurpose
  )(using algorithm: DigestAlgorithm): Either[EvidenceError, CompiledEvidenceDesign[A, Cov]] =
    for
      space <- IndexSpace
        .of(samples.size)
        .left
        .map(EvidenceError.ResampleFailure.apply)
      seed <- scopedSeed(root, samples, purpose)
      compiled <- design
        .compile(space, seed)
        .left
        .map(EvidenceError.ResampleFailure.apply)
      population <- SourceIdentity
        .of(
          s"scalafim:mvpa-axis:${samples.descriptor.namespace}",
          samples.descriptor.coordinateSignature.value
        )
        .left
        .map(EvidenceError.ResampleFingerprintFailure.apply)
      receipt <- compiled
        .receipt(population)
        .left
        .map(EvidenceError.ResampleDigestFailure.apply)
    yield CompiledEvidenceDesign(compiled, receipt)

  private def scopedSeed[K](
      root: ScientificSeed,
      samples: AxisRef[K],
      purpose: EvidenceDesignPurpose
  ): Either[EvidenceError, Seed] =
    for
      purposeDomain <- StreamDomain
        .custom(purpose.streamTag)
        .left
        .map(EvidenceError.ResampleFailure.apply)
      signatureDomain <- StreamDomain
        .custom(AxisSignatureStreamTag)
        .left
        .map(EvidenceError.ResampleFailure.apply)
      start <- StreamPath
        .of(purposeDomain, 0)
        .left
        .map(EvidenceError.ResampleFailure.apply)
      path <- appendSignature(start, signatureDomain, samples.descriptor.coordinateSignature.value)
    yield ScientificSeed.providerSeed(root).derive(path)

  private def appendSignature(
      initial: StreamPath,
      domain: StreamDomain,
      signature: String
  ): Either[EvidenceError, StreamPath] =
    var path = initial
    var index = 0
    while index < signature.length do
      path.append(domain, signature.charAt(index).toInt) match
        case Left(error)  => return Left(EvidenceError.ResampleFailure(error))
        case Right(value) => path = value
      index += 1
    Right(path)
