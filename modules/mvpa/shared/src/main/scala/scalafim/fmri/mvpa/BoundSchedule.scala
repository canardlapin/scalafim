package scalafim.fmri.mvpa

import multivar.core.SemanticSpace
import multivar.core.SpaceEvidence
import resample4s.core.Compiled
import resample4s.core.ContentDigest
import resample4s.core.Coverage
import resample4s.core.Design
import resample4s.core.DigestAlgorithm
import resample4s.core.DigestError
import resample4s.core.Draw
import resample4s.core.FingerprintError
import resample4s.core.Injection
import resample4s.core.Labels
import resample4s.core.OutOfDomain
import resample4s.core.Permutation
import resample4s.core.PlanCost
import resample4s.core.PlanDiagnostics
import resample4s.core.PlanReceipt
import resample4s.core.PlanShape
import resample4s.core.Reindexing
import resample4s.core.Seed
import resample4s.core.Selection
import resample4s.core.SourceIdentity
import resample4s.core.Split
import resample4s.core.UnitKey
import resample4s.designs.NestedFold

opaque type SeedDomain = String

object SeedDomain:
  val Validation: SeedDomain = "validation"
  val CrossFit: SeedDomain = "cross-fit"
  val NestedTuning: SeedDomain = "nested-tuning"
  val Randomization: SeedDomain = "randomization"
  val Bootstrap: SeedDomain = "bootstrap"

  def apply(value: String): Either[BoundScheduleError, SeedDomain] =
    ScientificIdentityText
      .lowerIdentifier("seed domain", value)
      .left
      .map(BoundScheduleError.InvalidSeedDomain.apply)

  extension (domain: SeedDomain) inline def value: String = domain

final class SeedAuthority private (
    val domain: SeedDomain,
    val seed: Seed
)

object SeedAuthority:
  def apply(domain: SeedDomain, seed: Seed): SeedAuthority =
    new SeedAuthority(domain, seed)

  def fromLong(domain: SeedDomain, value: Long): SeedAuthority =
    new SeedAuthority(domain, Seed.fromLong(value))

/** Resample4s population evidence derived from one exact ordered scientific axis. The receipt value includes the
  * complete axis fingerprint, not only its length.
  */
final class AxisPopulationFingerprint[K, S <: SemanticSpace] private (
    val axis: AxisIdentity,
    val evidence: SpaceEvidence[S],
    val value: SourceIdentity
)

object AxisPopulationFingerprint:
  private val Protocol = "scalafim-mvpa-axis-population/v1"

  def fromAxis[K, S <: SemanticSpace](
      axis: AxisRef.Aux[K, S]
  ): Either[BoundScheduleError, AxisPopulationFingerprint[K, S]] =
    SourceIdentity
      .of(
        s"urn:scalafim:axis:${axis.identity.fingerprint.value}",
        Protocol
      )
      .left
      .map(BoundScheduleError.InvalidPopulationFingerprint.apply)
      .map: fingerprint =>
        new AxisPopulationFingerprint(
          axis.identity,
          axis.evidence,
          fingerprint
        )

/** Exact sample-bound label evidence copied from the same Resample4s design that owns the compiled plan. Its digest is
  * compared with the plan receipt at binding, so a foreign group/stratum authority cannot be substituted.
  */
final class ScheduleLabels[K, S <: SemanticSpace] private (
    val axis: AxisIdentity,
    val evidence: SpaceEvidence[S],
    val values: Vector[Labels],
    val fingerprint: Option[ContentDigest]
):
  def size: Int = values.length

object ScheduleLabels:
  def fromDesign[K, A, Cov <: Coverage, S <: SemanticSpace](
      axis: AxisRef.Aux[K, S],
      design: Design[A, Cov]
  )(using
      algorithm: DigestAlgorithm
  ): Either[
    BoundScheduleError,
    ScheduleLabels[K, S]
  ] =
    val values = Vector.newBuilder[Labels]
    var index = 0
    while index < design.definition.labelCount do
      design.definition.labelAt(index) match
        case Left(error) =>
          return Left(BoundScheduleError.InvalidDesignLabelIndex(error))
        case Right(labels) =>
          if labels.size != axis.size then
            return Left(
              BoundScheduleError.LabelPopulationMismatch(
                index,
                axis.size,
                labels.size
              )
            )
          values += labels
      index += 1

    design.labelsFingerprint.left
      .map(BoundScheduleError.Digest.apply)
      .map: fingerprint =>
        new ScheduleLabels(
          axis.identity,
          axis.evidence,
          values.result(),
          fingerprint
        )

enum ScheduleUnitError:
  case PopulationMismatch(role: String, expected: Int, actual: Int)
  case Axis(error: AxisRefError)

  def message: String =
    this match
      case PopulationMismatch(role, expected, actual) =>
        s"schedule $role population is $actual, expected sample-axis size $expected"
      case Axis(error) => error.message

enum BoundScheduleError:
  case InvalidSeedDomain(error: ScientificIdentityError)
  case InvalidPopulationFingerprint(error: FingerprintError)
  case PopulationAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case PopulationWitnessMismatch
  case LabelAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case LabelWitnessMismatch
  case LabelPopulationMismatch(label: Int, expected: Int, actual: Int)
  case InvalidDesignLabelIndex(error: OutOfDomain)
  case LabelFingerprintMismatch(
      expected: Option[ContentDigest],
      actual: Option[ContentDigest]
  )
  case SeedMismatch(expected: Long, actual: Long)
  case Digest(error: DigestError)
  case UnknownUnit(error: resample4s.core.UnknownUnit)
  case InvalidUnit(key: UnitKey, error: ScheduleUnitError)

  def message: String =
    this match
      case InvalidSeedDomain(error)            => error.message
      case InvalidPopulationFingerprint(error) =>
        s"invalid sample population fingerprint: $error"
      case PopulationAxisMismatch(expected, actual) =>
        s"population fingerprint belongs to sample axis ${actual.value}, expected ${expected.value}"
      case PopulationWitnessMismatch =>
        "population fingerprint and schedule use different nominal sample witnesses"
      case LabelAxisMismatch(expected, actual) =>
        s"schedule labels belong to sample axis ${actual.value}, expected ${expected.value}"
      case LabelWitnessMismatch =>
        "schedule labels and schedule use different nominal sample witnesses"
      case LabelPopulationMismatch(label, expected, actual) =>
        s"schedule label set $label has population $actual, expected $expected"
      case InvalidDesignLabelIndex(error) =>
        s"design exposed an invalid label index ${error.index} for ${error.domain} label sets"
      case LabelFingerprintMismatch(expected, actual) =>
        s"schedule label fingerprint $actual does not match compiled receipt $expected"
      case SeedMismatch(expected, actual) =>
        s"compiled schedule seed $actual does not match seed authority $expected"
      case Digest(error)      => s"schedule receipt digest failed: $error"
      case UnknownUnit(error) =>
        s"unknown schedule unit ${error.key} for shape ${error.shape}"
      case InvalidUnit(key, error) =>
        s"invalid schedule unit $key: ${error.message}"

trait ScheduleUnitBinder[A, K, S <: SemanticSpace]:
  type Bound

  def validate(value: A, populationSize: Int): Either[ScheduleUnitError, Unit]

  def bind(
      axis: AxisRef.Aux[K, S],
      value: A
  ): Either[ScheduleUnitError, Bound]

object ScheduleUnitBinder:
  type Aux[A, K, S <: SemanticSpace, Bound0] =
    ScheduleUnitBinder[A, K, S] { type Bound = Bound0 }

  given selection[K: AxisKeyCodec, S <: SemanticSpace]: Aux[
    Selection,
    K,
    S,
    ReindexingLeg[S, K, K, Selection]
  ] = new ScheduleUnitBinder[Selection, K, S]:
    override type Bound = ReindexingLeg[S, K, K, Selection]

    override def validate(
        value: Selection,
        populationSize: Int
    ): Either[ScheduleUnitError, Unit] =
      validateReindexing(value, populationSize, "selection")

    override def bind(
        axis: AxisRef.Aux[K, S],
        value: Selection
    ): Either[
      ScheduleUnitError,
      ReindexingLeg[S, K, K, Selection]
    ] =
      ReindexingLeg.selection(axis, value).left.map(ScheduleUnitError.Axis.apply)

  given injection[K: AxisKeyCodec, S <: SemanticSpace]: Aux[
    Injection,
    K,
    S,
    ReindexingLeg[S, K, K, Injection]
  ] = new ScheduleUnitBinder[Injection, K, S]:
    override type Bound = ReindexingLeg[S, K, K, Injection]

    override def validate(
        value: Injection,
        populationSize: Int
    ): Either[ScheduleUnitError, Unit] =
      validateReindexing(value, populationSize, "injection")

    override def bind(
        axis: AxisRef.Aux[K, S],
        value: Injection
    ): Either[
      ScheduleUnitError,
      ReindexingLeg[S, K, K, Injection]
    ] =
      ReindexingLeg.injection(axis, value).left.map(ScheduleUnitError.Axis.apply)

  given draw[K: AxisKeyCodec, S <: SemanticSpace]: Aux[
    Draw,
    K,
    S,
    ReindexingLeg[S, K, DrawOccurrence[K], Draw]
  ] = new ScheduleUnitBinder[Draw, K, S]:
    override type Bound =
      ReindexingLeg[S, K, DrawOccurrence[K], Draw]

    override def validate(
        value: Draw,
        populationSize: Int
    ): Either[ScheduleUnitError, Unit] =
      validateReindexing(value, populationSize, "draw")

    override def bind(
        axis: AxisRef.Aux[K, S],
        value: Draw
    ): Either[
      ScheduleUnitError,
      ReindexingLeg[S, K, DrawOccurrence[K], Draw]
    ] =
      ReindexingLeg.draw(axis, value).left.map(ScheduleUnitError.Axis.apply)

  given permutation[K: AxisKeyCodec, S <: SemanticSpace]: Aux[
    Permutation,
    K,
    S,
    ReindexingLeg[S, K, K, Permutation]
  ] = new ScheduleUnitBinder[Permutation, K, S]:
    override type Bound = ReindexingLeg[S, K, K, Permutation]

    override def validate(
        value: Permutation,
        populationSize: Int
    ): Either[ScheduleUnitError, Unit] =
      validateReindexing(value, populationSize, "permutation")

    override def bind(
        axis: AxisRef.Aux[K, S],
        value: Permutation
    ): Either[
      ScheduleUnitError,
      ReindexingLeg[S, K, K, Permutation]
    ] =
      ReindexingLeg.permutation(axis, value).left.map(ScheduleUnitError.Axis.apply)

  given selectionSplit[K: AxisKeyCodec, S <: SemanticSpace]: Aux[
    Split[Selection],
    K,
    S,
    BoundSelectionSplit[K, S]
  ] = new ScheduleUnitBinder[Split[Selection], K, S]:
    override type Bound = BoundSelectionSplit[K, S]

    override def validate(
        value: Split[Selection],
        populationSize: Int
    ): Either[ScheduleUnitError, Unit] =
      for
        _ <- validateReindexing(value.analysis, populationSize, "analysis selection")
        _ <- validateReindexing(value.assessment, populationSize, "assessment selection")
      yield ()

    override def bind(
        axis: AxisRef.Aux[K, S],
        value: Split[Selection]
    ): Either[ScheduleUnitError, BoundSelectionSplit[K, S]] =
      for
        analysis <- ReindexingLeg
          .selection(axis, value.analysis)
          .left
          .map(ScheduleUnitError.Axis.apply)
        assessment <- ReindexingLeg
          .selection(axis, value.assessment)
          .left
          .map(ScheduleUnitError.Axis.apply)
      yield BoundSelectionSplit(analysis, assessment)

  given drawSplit[K: AxisKeyCodec, S <: SemanticSpace]: Aux[
    Split[Draw],
    K,
    S,
    BoundDrawSplit[K, S]
  ] = new ScheduleUnitBinder[Split[Draw], K, S]:
    override type Bound = BoundDrawSplit[K, S]

    override def validate(
        value: Split[Draw],
        populationSize: Int
    ): Either[ScheduleUnitError, Unit] =
      for
        _ <- validateReindexing(value.analysis, populationSize, "analysis draw")
        _ <- validateReindexing(value.assessment, populationSize, "assessment selection")
      yield ()

    override def bind(
        axis: AxisRef.Aux[K, S],
        value: Split[Draw]
    ): Either[ScheduleUnitError, BoundDrawSplit[K, S]] =
      for
        analysis <- ReindexingLeg
          .draw(axis, value.analysis)
          .left
          .map(ScheduleUnitError.Axis.apply)
        assessment <- ReindexingLeg
          .selection(axis, value.assessment)
          .left
          .map(ScheduleUnitError.Axis.apply)
      yield BoundDrawSplit(analysis, assessment)

  given nestedFold[S <: SemanticSpace]: Aux[
    NestedFold,
    SampleId,
    S,
    BoundNestedFold[S]
  ] = new ScheduleUnitBinder[NestedFold, SampleId, S]:
    override type Bound = BoundNestedFold[S]

    override def validate(
        value: NestedFold,
        populationSize: Int
    ): Either[ScheduleUnitError, Unit] =
      BoundNestedFold.validate(value, populationSize)

    override def bind(
        samples: AxisRef.Aux[SampleId, S],
        value: NestedFold
    ): Either[ScheduleUnitError, BoundNestedFold[S]] =
      BoundNestedFold.bind(samples, value)

  private def validateReindexing(
      value: Reindexing,
      populationSize: Int,
      role: String
  ): Either[ScheduleUnitError, Unit] =
    if value.codomain == populationSize then Right(())
    else
      Left(
        ScheduleUnitError.PopulationMismatch(
          role,
          populationSize,
          value.codomain
        )
      )

final case class BoundSelectionSplit[K, S <: SemanticSpace](
    analysis: ReindexingLeg[S, K, K, Selection],
    assessment: ReindexingLeg[S, K, K, Selection]
)

final case class BoundDrawSplit[K, S <: SemanticSpace](
    analysis: ReindexingLeg[S, K, DrawOccurrence[K], Draw],
    assessment: ReindexingLeg[S, K, K, Selection]
)

/** A compiled ordinal schedule, exact semantic sample owner, label authority, seed authority, and receipt admitted as
  * one value. There is no constructor that accepts a free-standing Plan or PlanReceipt.
  */
final class BoundSchedule[
    A,
    Cov <: Coverage,
    K,
    S <: SemanticSpace,
    BoundUnit
] private (
    private val compiled: Compiled[A, Cov],
    private val binder: ScheduleUnitBinder.Aux[A, K, S, BoundUnit],
    val axis: AxisRef.Aux[K, S],
    val population: AxisPopulationFingerprint[K, S],
    val labels: ScheduleLabels[K, S],
    val seedAuthority: SeedAuthority,
    val receipt: PlanReceipt
):
  def shape: PlanShape = compiled.plan.shape
  def keys: IndexedSeq[UnitKey] = compiled.plan.keys
  def diagnostics: PlanDiagnostics = compiled.diagnostics
  def cost: PlanCost = compiled.cost

  /** Exact compiled value retained for package-owned scientific compilers. Public callers consume only bound units and
    * receipts; execution adapters may pass this same value to a dependency interpreter without rebuilding ordinal folds
    * from typed legs.
    */
  private[mvpa] def compiledValue: Compiled[A, Cov] = compiled

  def at(key: UnitKey): Either[BoundScheduleError, BoundUnit] =
    compiled.plan
      .at(key)
      .left
      .map(BoundScheduleError.UnknownUnit.apply)
      .flatMap: value =>
        binder
          .bind(axis, value)
          .left
          .map(BoundScheduleError.InvalidUnit(key, _))

  def iterator: Iterator[(UnitKey, Either[BoundScheduleError, BoundUnit])] =
    keys.iterator.map(key => key -> at(key))

object BoundSchedule:
  def apply[
      A,
      Cov <: Coverage,
      K,
      S <: SemanticSpace,
      BoundUnit
  ](
      compiled: Compiled[A, Cov],
      axis: AxisRef.Aux[K, S],
      population: AxisPopulationFingerprint[K, S],
      labels: ScheduleLabels[K, S],
      seedAuthority: SeedAuthority
  )(using
      binder: ScheduleUnitBinder.Aux[A, K, S, BoundUnit],
      algorithm: DigestAlgorithm
  ): Either[
    BoundScheduleError,
    BoundSchedule[A, Cov, K, S, BoundUnit]
  ] =
    for
      _ <- validatePopulation(axis, population)
      _ <- validateLabels(axis, labels)
      _ <- validateUnits(compiled, axis.size, binder)
      receipt <- compiled
        .receipt(population.value)
        .left
        .map(BoundScheduleError.Digest.apply)
      _ <-
        if receipt.labels == labels.fingerprint then Right(())
        else
          Left(
            BoundScheduleError.LabelFingerprintMismatch(
              receipt.labels,
              labels.fingerprint
            )
          )
      _ <-
        if receipt.seed.value == seedAuthority.seed.value then Right(())
        else
          Left(
            BoundScheduleError.SeedMismatch(
              seedAuthority.seed.value,
              receipt.seed.value
            )
          )
    yield new BoundSchedule(
      compiled,
      binder,
      axis,
      population,
      labels,
      seedAuthority,
      receipt
    )

  private def validatePopulation[K, S <: SemanticSpace](
      axis: AxisRef.Aux[K, S],
      population: AxisPopulationFingerprint[K, S]
  ): Either[BoundScheduleError, Unit] =
    if population.axis != axis.identity then
      Left(
        BoundScheduleError.PopulationAxisMismatch(
          axis.identity.fingerprint,
          population.axis.fingerprint
        )
      )
    else if !(population.evidence eq axis.evidence) then Left(BoundScheduleError.PopulationWitnessMismatch)
    else Right(())

  private def validateLabels[K, S <: SemanticSpace](
      axis: AxisRef.Aux[K, S],
      labels: ScheduleLabels[K, S]
  ): Either[BoundScheduleError, Unit] =
    if labels.axis != axis.identity then
      Left(
        BoundScheduleError.LabelAxisMismatch(
          axis.identity.fingerprint,
          labels.axis.fingerprint
        )
      )
    else if !(labels.evidence eq axis.evidence) then Left(BoundScheduleError.LabelWitnessMismatch)
    else Right(())

  private def validateUnits[
      A,
      Cov <: Coverage,
      K,
      S <: SemanticSpace,
      BoundUnit
  ](
      compiled: Compiled[A, Cov],
      populationSize: Int,
      binder: ScheduleUnitBinder.Aux[A, K, S, BoundUnit]
  ): Either[BoundScheduleError, Unit] =
    val iterator = compiled.plan.iterator
    while iterator.hasNext do
      val (key, value) = iterator.next()
      binder.validate(value, populationSize) match
        case Left(error) =>
          return Left(BoundScheduleError.InvalidUnit(key, error))
        case Right(_) => ()
    Right(())
