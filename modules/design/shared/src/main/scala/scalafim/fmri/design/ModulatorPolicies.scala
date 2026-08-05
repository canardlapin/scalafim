package scalafim.fmri.design

/** What to do when a continuous modulator contributes no estimable variation. */
enum DegenerateModulatorPolicy:
  /** Keep the explicit zero/constant contribution and retain diagnostic evidence. */
  case RetainAndReport
  /** Reject the term before convolution. */
  case Reject

  def label: String =
    this match
      case RetainAndReport => "retain-and-report"
      case Reject          => "reject"

/** A typed scientific classification of a raw modulator before missing-value
  * repair or convolution.
  */
enum DegenerateModulatorOutcome:
  case EmptyScope
  case AllNonFinite
  case AllZero
  case Constant

  def label: String =
    this match
      case EmptyScope   => "empty-scope"
      case AllNonFinite => "all-non-finite"
      case AllZero      => "all-zero"
      case Constant     => "constant"

/** Auditable evidence for one raw degenerate modulator. */
final case class DegenerateModulatorReceipt(
    term: TermId,
    modulator: ModulatorId,
    sourceRows: Vector[Int],
    finiteSourceRows: Vector[Int],
    parentTrials: Vector[TrialId],
    outcome: DegenerateModulatorOutcome,
    policy: DegenerateModulatorPolicy
):
  require(sourceRows.distinct.length == sourceRows.length, "degenerate-modulator source rows must be unique")
  require(sourceRows.forall(_ >= 0), "degenerate-modulator source rows must be non-negative")
  require(
    finiteSourceRows.forall(sourceRows.contains),
    "finite degenerate-modulator rows must be drawn from source rows"
  )
  require(
    parentTrials.isEmpty || parentTrials.length == sourceRows.length,
    "degenerate-modulator parent trials must align with source rows"
  )

  def canonical: String =
    s"term=${term.value};modulator=${modulator.value};rows=${sourceRows.mkString(",")};finite=${finiteSourceRows.mkString(",")};parents=${parentTrials.map(_.value).mkString(",")};outcome=${outcome.label};policy=${policy.label}"

/** The event-row partitions within which ordered orthogonalization is applied. */
enum OrthogonalizationScope:
  case WholeTerm
  case WithinRun
  case WithinCells(factors: Vector[FactorId])

  def validate: Either[DesignError, Unit] =
    this match
      case WithinCells(factors) if factors.isEmpty =>
        Left(DesignError.InvalidSchema("within-cell orthogonalization requires at least one factor"))
      case WithinCells(factors) if factors.distinct.length != factors.length =>
        Left(DesignError.InvalidSchema("within-cell orthogonalization factors must be distinct"))
      case _ => Right(())

  def canonical: String =
    this match
      case WholeTerm            => "whole-term"
      case WithinRun            => "within-run"
      case WithinCells(factors) => s"within-cells:${factors.map(_.value).mkString(",")}"

object OrthogonalizationScope:
  def withinCells(first: String, remaining: String*): Either[DesignError, OrthogonalizationScope] =
    val values = first +: remaining.toVector
    values.foldLeft[Either[DesignError, Vector[FactorId]]](Right(Vector.empty)) { (acc, value) =>
      for
        parsed <- acc
        factor <- FactorId(value)
      yield parsed :+ factor
    }.flatMap { factors =>
      val scope = OrthogonalizationScope.WithinCells(factors)
      scope.validate.map(_ => scope)
    }

/** A deliberately ordered, term-owned serial orthogonalization policy.
  *
  * The first modulator is left unchanged. Each later modulator is residualized
  * against the span of every earlier modulator, within the declared scope.
  * Nothing in formula lowering enables this policy implicitly.
  */
final case class ModulatorOrthogonalization private[design] (
    term: TermId,
    order: Vector[ModulatorId],
    scope: OrthogonalizationScope,
    degenerate: DegenerateModulatorPolicy,
    tolerance: Double
):
  require(order.lengthCompare(2) >= 0, "ordered orthogonalization requires at least two modulators")
  require(order.distinct.length == order.length, "ordered orthogonalization modulators must be unique")
  require(scope.validate.isRight, "ordered orthogonalization scope must be valid")
  require(tolerance >= 0.0 && tolerance.isFinite, "ordered orthogonalization tolerance must be finite and non-negative")

  def canonical: String =
    s"term=${term.value};order=${order.map(_.value).mkString(",")};scope=${scope.canonical};degenerate=${degenerate.label};tolerance=$tolerance"

  /** Refine a validated ordered policy to independent run partitions. */
  def withinRun: ModulatorOrthogonalization =
    copy(scope = OrthogonalizationScope.WithinRun)

  /** Checked public boundary for the common within-cell refinement. */
  def withinCells(first: String, remaining: String*): Either[DesignError, ModulatorOrthogonalization] =
    OrthogonalizationScope.withinCells(first, remaining*).map(value => copy(scope = value))

  /** Make orthogonalization-induced degeneration a typed compilation error. */
  def rejectDegenerate: ModulatorOrthogonalization =
    copy(degenerate = DegenerateModulatorPolicy.Reject)

  def withTolerance(value: Double): Either[DesignError, ModulatorOrthogonalization] =
    if value >= 0.0 && value.isFinite then Right(copy(tolerance = value))
    else Left(DesignError.InvalidSchema("ordered orthogonalization tolerance must be finite and non-negative"))

object ModulatorOrthogonalization:
  val DefaultTolerance: Double = 1e-10

  def ordered(
      term: TermId,
      order: Vector[ModulatorId],
      scope: OrthogonalizationScope = OrthogonalizationScope.WholeTerm,
      degenerate: DegenerateModulatorPolicy = DegenerateModulatorPolicy.RetainAndReport,
      tolerance: Double = DefaultTolerance
  ): Either[DesignError, ModulatorOrthogonalization] =
    if order.lengthCompare(2) < 0 then
      Left(DesignError.InvalidSchema("ordered orthogonalization requires at least two modulators"))
    else if order.distinct.length != order.length then
      Left(DesignError.InvalidSchema("ordered orthogonalization modulators must be unique"))
    else if tolerance < 0.0 || !tolerance.isFinite then
      Left(DesignError.InvalidSchema("ordered orthogonalization tolerance must be finite and non-negative"))
    else scope.validate.map(_ => ModulatorOrthogonalization(term, order, scope, degenerate, tolerance))

  /** Checked string boundary for compact public model specifications. */
  def ordered(
      term: String,
      first: String,
      second: String,
      remaining: String*
  ): Either[DesignError, ModulatorOrthogonalization] =
    for
      termId <- TermId(term)
      ids <- parseModulators(Vector(first, second) ++ remaining)
      policy <- ordered(termId, ids)
    yield policy

  private def parseModulators(values: Vector[String]): Either[DesignError, Vector[ModulatorId]] =
    values.foldLeft[Either[DesignError, Vector[ModulatorId]]](Right(Vector.empty)) { (acc, value) =>
      for
        parsed <- acc
        id <- ModulatorId(value)
      yield parsed :+ id
    }

/** Validated term registry for ordered modulator policies. */
final case class ModulatorOrthogonalizationPlan private[design] (
    policies: Vector[ModulatorOrthogonalization]
):
  require(policies.map(_.term).distinct.length == policies.length, "a term may declare at most one orthogonalization policy")

  def isEmpty: Boolean = policies.isEmpty

  def forTerm(term: Option[String]): Option[ModulatorOrthogonalization] =
    term.flatMap(value => TermId(value).toOption).flatMap(id => policies.find(_.term == id))

  def validateCoverage(realizedTerms: Vector[TermId]): Either[DesignError, Unit] =
    policies.find(policy => !realizedTerms.contains(policy.term)) match
      case Some(unused) =>
        Left(DesignError.InvalidSchema(s"orthogonalization policy names unknown term '${unused.term.value}'"))
      case None => Right(())

object ModulatorOrthogonalizationPlan:
  val None: ModulatorOrthogonalizationPlan = ModulatorOrthogonalizationPlan(Vector.empty)

  def of(policies: ModulatorOrthogonalization*): Either[DesignError, ModulatorOrthogonalizationPlan] =
    val values = policies.toVector
    if values.map(_.term).distinct.length != values.length then
      Left(DesignError.InvalidSchema("a term may declare at most one orthogonalization policy"))
    else Right(ModulatorOrthogonalizationPlan(values))

  def one(policy: ModulatorOrthogonalization): ModulatorOrthogonalizationPlan =
    ModulatorOrthogonalizationPlan(Vector(policy))

enum OrthogonalizationOutcome:
  case Applied
  case DegenerateRetained

/** Numerical evidence for one scoped residualization step. */
final case class OrthogonalizationGroupReceipt(
    key: String,
    sourceRows: Vector[Int],
    parentTrials: Vector[TrialId],
    referenceRank: Int,
    sourceNorm: Double,
    residualNorm: Double,
    outcome: OrthogonalizationOutcome
):
  require(key.nonEmpty, "orthogonalization group key must be non-empty")
  require(sourceRows.nonEmpty, "orthogonalization groups must contain source rows")
  require(sourceRows.distinct.length == sourceRows.length, "orthogonalization source rows must be unique")
  require(sourceRows.forall(_ >= 0), "orthogonalization source rows must be non-negative")
  require(parentTrials.isEmpty || parentTrials.length == sourceRows.length, "orthogonalization parent trials must align with source rows")
  require(referenceRank >= 0, "orthogonalization reference rank must be non-negative")
  require(sourceNorm >= 0.0 && sourceNorm.isFinite, "orthogonalization source norm must be finite and non-negative")
  require(residualNorm >= 0.0 && residualNorm.isFinite, "orthogonalization residual norm must be finite and non-negative")

  def canonical: String =
    s"key=$key;rows=${sourceRows.mkString(",")};parents=${parentTrials.map(_.value).mkString(",")};rank=$referenceRank;sourceNorm=$sourceNorm;residualNorm=$residualNorm;outcome=$outcome"

/** Receipt for one target modulator and all of its scoped groups. */
final case class OrthogonalizationStepReceipt(
    modulator: ModulatorId,
    against: Vector[ModulatorId],
    groups: Vector[OrthogonalizationGroupReceipt]
):
  require(against.nonEmpty, "orthogonalization step must name at least one predecessor")
  require(against.distinct.length == against.length, "orthogonalization predecessors must be unique")
  require(groups.nonEmpty, "orthogonalization step must retain at least one group")

  def canonical: String =
    s"modulator=${modulator.value};against=${against.map(_.value).mkString(",")};groups=${groups.map(_.canonical).mkString("|")}"

/** Complete policy and lowering evidence for one term. */
final case class OrthogonalizationReceipt(
    policy: ModulatorOrthogonalization,
    steps: Vector[OrthogonalizationStepReceipt]
):
  require(steps.length == policy.order.length - 1, "orthogonalization receipt must cover every target after the first")
  require(steps.map(_.modulator) == policy.order.tail, "orthogonalization receipt steps must preserve declared order")

  def canonical: String =
    s"${policy.canonical};steps=${steps.map(_.canonical).mkString("||")}"
