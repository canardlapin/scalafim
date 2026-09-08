package scalafim.fmri.design

import scalafim.fmri.design.contrast.LevelId
import scalafim.fmri.design.linalg.QrDecomposition
import scalafim.fmri.hrf.{BasisElementId, BasisRole, Hrf, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

/** Which compiled model contributed a column. */
enum ModelSource:
  case Event, Baseline

/** Semantic role of a realized column. */
enum ColumnRole:
  case Task, Trial, TrialAggregate, Covariate, Drift, Intercept, Nuisance, Baseline

/** Compatibility/export classification for modulation columns. */
enum ModulationType:
  case Amplitude, Parametric, Covariate

/** A structural factor assignment in one realized design cell.
  *
  * The level is kept as an identifier rather than folded into a rendered
  * condition name.  This is the small piece of provenance that lets clients
  * author hypotheses without parsing names such as `task_recall_valence_high`.
  */
final case class CellAssignment(factor: FactorId, level: LevelId)

/** A canonical, order-independent key for a factorial cell. */
final case class CellKey private (assignments: Vector[CellAssignment]):
  require(
    assignments.map(_.factor.value).distinct.length == assignments.length,
    "a CellKey may contain at most one level for each factor"
  )

  def get(factor: FactorId): Option[LevelId] =
    assignments.find(_.factor.value == factor.value).map(_.level)

  def canonical: String =
    if assignments.isEmpty then "cell:{}"
    else
      assignments
      .sortBy(_.factor.value)
        .map(a => s"${CellKey.encode(a.factor.value)}=${CellKey.encode(a.level.value)}")
        .mkString("cell:{", ",", "}")

object CellKey:
  val empty: CellKey = CellKey(Vector.empty)

  def from(assignments: Seq[CellAssignment]): Either[DesignError, CellKey] =
    val xs = assignments.toVector.sortBy(_.factor.value)
    if xs.map(_.factor.value).distinct.length != xs.length then
      Left(DesignError.InvalidSchema("CellKey contains multiple levels for one factor"))
    else Right(CellKey(xs))

  def unsafe(assignments: Seq[CellAssignment]): CellKey =
    from(assignments).fold(error => throw new IllegalArgumentException(error.message), identity)

  private[design] def encode(value: String): String =
    value
      .replace("%", "%25")
      .replace("=", "%3D")
      .replace(",", "%2C")
      .replace("{", "%7B")
      .replace("}", "%7D")

/** An exhaustive HRF assignment for realized factorial cells.  The mapping
  * is structural: callers provide [[CellKey]] values and never maintain
  * column offsets or parse rendered names.
  */
final case class HrfByCell private (assignments: Vector[(CellKey, Hrf)]):
  require(assignments.nonEmpty, "an HRF-by-cell assignment must not be empty")
  require(assignments.map(_._1).distinct.length == assignments.length, "HRF-by-cell assignments must have unique cells")

  private val byCell: Map[CellKey, Hrf] = assignments.toMap

  def resolve(cells: Vector[CellKey], term: String): Either[DesignError, Vector[Hrf]] =
    val realized = cells.distinct
    val missing = realized.filterNot(byCell.contains)
    val extra = byCell.keySet.filterNot(realized.contains).toVector.sortBy(_.canonical)
    if missing.nonEmpty || extra.nonEmpty then
      Left(
        DesignError.InvalidHrfAssignment(
          term = term,
          missing = missing.map(_.canonical).sortBy(identity),
          extra = extra.map(_.canonical)
        )
      )
    else Right(cells.map(byCell))

  def canonical: String =
    assignments
      .sortBy(_._1.canonical)
      .map { case (cell, hrf) =>
        s"${HrfAssignment.token(cell.canonical)}=hrf(${HrfAssignment.hrfCanonical(hrf)})"
      }
      .mkString("hrf-by-cell[", ";", "]")

object HrfByCell:
  def of(assignments: (CellKey, Hrf)*): Either[DesignError, HrfByCell] =
    val values = assignments.toVector
    if values.isEmpty then Left(DesignError.InvalidSchema("HRF-by-cell assignment must contain at least one cell"))
    else if values.map(_._1).distinct.length != values.length then
      Left(DesignError.InvalidSchema("HRF-by-cell assignments must have unique cells"))
    else HrfAssignment.validateHrfs(values.map(_._2)).map(_ => HrfByCell(values))

  def apply(assignments: (CellKey, Hrf)*): HrfByCell =
    of(assignments*).fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Checked convenience for the common one-factor assignment. */
  def oneFactor(factor: String, assignments: (String, Hrf)*): Either[DesignError, HrfByCell] =
    for
      factorId <- FactorId(factor)
      cells <- assignments.toVector.foldLeft[Either[DesignError, Vector[(CellKey, Hrf)]]](Right(Vector.empty)) {
        case (acc, (level, hrf)) =>
          for
            values <- acc
            levelId <- LevelId(level).left.map(error => DesignError.InvalidId("factor level", level, error.message))
            cell <- CellKey.from(Vector(CellAssignment(factorId, levelId)))
          yield values :+ (cell -> hrf)
      }
      result <- of(cells*)
    yield result


/** Structural meaning of one condition column before HRF basis expansion. */
final case class ConditionProvenance(
    cell: CellKey,
    modulator: Option[ModulatorId]
)

/** A basis component reference used by structural column origins.
  *
  * `index` remains available for numerical lowering, while `id` and `role`
  * retain the semantic identity of the component without stringly-typed role
  * or element identities.
  */
final case class BasisElementRef(
    basisId: String,
    index: BasisIndex,
    role: Option[BasisRole] = None,
    elementId: Option[BasisElementId] = None
):
  require(basisId.trim.nonEmpty, "basisId must be non-empty")

  def id: String =
    val rolePart = role.fold("")(value => "#" + value.stableLabel)
    s"$basisId#${index.oneBased}$rolePart"

  /** Stable HRF-module identity when available; legacy refs retain their
    * deterministic basis-name/index/role identity. */
  def semanticId: String =
    elementId.fold(id)(_.value)

  def canonical: String =
    val rolePart = role.fold("")(value => "|" + encode(value.stableLabel))
    val elementPart = elementId.fold("")(value => s"|element=${encode(value.value)}")
    s"basis(${encode(basisId)}|${index.oneBased}$rolePart$elementPart)"

  private def encode(value: String): String =
    value
      .replace("%", "%25")
      .replace("|", "%7C")
      .replace("#", "%23")

/** The scope in which a realized column is interpreted. */
enum RunScope:
  case Global
  case PerRun
  case Run(index: RunIndex)

  def canonical: String =
    this match
      case Global     => "global"
      case PerRun     => "per-run"
      case Run(index) => s"run:${index.oneBased}"

/** Structural provenance for every supported design column family. */
enum StructuralColumnOrigin:
  case Event(
      term: TermId,
      phase: Option[PhaseId],
      cell: CellKey,
      modulator: Option[ModulatorId],
      basis: Option[BasisElementRef],
      role: ColumnRole,
      runScope: RunScope
  )
  case Sampled(
      regressor: ModulatorId,
      role: ColumnRole,
      runScope: RunScope
  )
  case Intercept(runScope: RunScope)
  case Drift(term: TermId, component: Option[BasisElementRef], runScope: RunScope)
  case Nuisance(term: TermId, regressor: ModulatorId, runScope: RunScope)
  case Baseline(
      term: TermId,
      role: ColumnRole,
      component: Option[BasisElementRef],
      runScope: RunScope
  )
  /** Explicit compatibility origin for hand-built pre-schema models. */
  case Legacy(source: ModelSource, name: String, position: Int)

  /** Re-identify a column for a coefficient estimated independently within a
    * run.  Explicit design-owned run identities (for example a per-run
    * intercept that belongs to run 1) are preserved; global and per-run
    * families become the run-local coefficient identity.  The operation does
    * not change the numerical column or the source design fingerprint.
    */
  def forRunwiseCoefficient(run: RunIndex): StructuralColumnOrigin =
    def scopeFor(existing: RunScope): RunScope =
      existing match
        case RunScope.Run(_) => existing
        case _               => RunScope.Run(run)

    this match
      case Event(term, phase, cell, modulator, basis, role, runScope) =>
        Event(term, phase, cell, modulator, basis, role, scopeFor(runScope))
      case Sampled(regressor, role, runScope) =>
        Sampled(regressor, role, scopeFor(runScope))
      case Intercept(runScope) =>
        Intercept(scopeFor(runScope))
      case Drift(term, component, runScope) =>
        Drift(term, component, scopeFor(runScope))
      case Nuisance(term, regressor, runScope) =>
        Nuisance(term, regressor, scopeFor(runScope))
      case Baseline(term, role, component, runScope) =>
        Baseline(term, role, component, scopeFor(runScope))
      case legacy: Legacy => legacy

  def canonical: String =
    this match
      case Event(term, phase, cell, modulator, basis, role, runScope) =>
        List(
          "event",
          term.value,
          phase.fold("")(_.value),
          cell.canonical,
          modulator.fold("")(_.value),
          basis.fold("")(_.canonical),
          role.toString,
          runScope.canonical
        ).map(StructuralColumnOrigin.encode).mkString("|")
      case Sampled(regressor, role, runScope) =>
        s"sampled|${StructuralColumnOrigin.encode(regressor.value)}|${role.toString}|${runScope.canonical}"
      case Intercept(runScope) => s"intercept|${runScope.canonical}"
      case Drift(term, component, runScope) =>
        s"drift|${StructuralColumnOrigin.encode(term.value)}|${component.fold("")(_.canonical)}|${runScope.canonical}"
      case Nuisance(term, regressor, runScope) =>
        s"nuisance|${StructuralColumnOrigin.encode(term.value)}|${StructuralColumnOrigin.encode(regressor.value)}|${runScope.canonical}"
      case Baseline(term, role, component, runScope) =>
        s"baseline|${StructuralColumnOrigin.encode(term.value)}|${role.toString}|${component.fold("")(_.canonical)}|${runScope.canonical}"
      case Legacy(source, name, position) =>
        s"legacy|${source.toString}|${StructuralColumnOrigin.encode(name)}|$position"

object StructuralColumnOrigin:
  private[design] def encode(value: String): String =
    value
      .replace("%", "%25")
      .replace("|", "%7C")
      .replace("\n", "%0A")

/** A realized design column.  The label is presentation data; the origin is
  * the identity used by semantic consumers and fingerprinting.
  */
final case class StructuralColumn(
    id: ColumnId,
    ordinal: DesignColumnIndex,
    origin: StructuralColumnOrigin,
    label: String,
    prettyLabel: String = "",
    hrfScale: HrfColumnScale = HrfColumnScale.identity
):
  require(label.trim.nonEmpty, "column label must be non-empty")

  def renderedLabel: String = label

  def canonical: String =
    s"${ordinal.oneBased}|${id.value}|${origin.canonical}|hrf-scale=${hrfScale.canonical}"

object StructuralColumn:
  def fromOrigin(
      ordinal: Int,
      origin: StructuralColumnOrigin,
      label: String,
      prettyLabel: String = "",
      hrfScale: HrfColumnScale = HrfColumnScale.identity
  ): Either[DesignError, StructuralColumn] =
    if ordinal < 1 then
      Left(DesignError.InvalidSchema(s"column ordinal must be >= 1, got $ordinal"))
    else
      val idValue = s"${origin.canonical}|ordinal=$ordinal"
      ColumnId(idValue).map { id =>
        StructuralColumn(
          id = id,
          ordinal = DesignColumnIndex.unsafeOneBased(ordinal),
          origin = origin,
          label = label,
          prettyLabel = prettyLabel,
          hrfScale = hrfScale
        )
      }

/** The row-level alignment used to bind a design to a response. */
final case class RowLayout(
    blockIds: Vector[RunIndex],
    acquisitionTimes: Vector[Seconds],
    selectedRows: Vector[ScanIndex]
):
  require(blockIds.length == acquisitionTimes.length, "blockIds and acquisitionTimes must have equal length")
  require(selectedRows.distinct.length == selectedRows.length, "selectedRows must not contain duplicates")
  require(
    selectedRows.forall(ix => ix.oneBased >= 1 && ix.oneBased <= acquisitionTimes.length),
    "selectedRows must point into acquisitionTimes"
  )
  require(acquisitionTimes.forall(t => t.value.isFinite), "acquisitionTimes must be finite")

  def rows: Int = acquisitionTimes.length

  def selectedRowCount: Int = selectedRows.length

  def canonical: String =
    val blocks = blockIds.map(_.oneBased).mkString(",")
    val times = acquisitionTimes.map(t => java.lang.Double.doubleToLongBits(t.value).toString).mkString(",")
    val selected = selectedRows.map(_.oneBased).mkString(",")
    s"rows=$rows;blocks=$blocks;times=$times;selected=$selected"

object RowLayout:
  def fromSamplingFrame(samplingFrame: SamplingFrame): RowLayout =
    val blockIds = samplingFrame.blockIdsPerSample.map(b => RunIndex.unsafeOneBased(b + 1))
    val times = samplingFrame.acquisitionOnsets()
    val selected = (1 to times.length).toVector.map(ScanIndex.unsafeOneBased)
    RowLayout(blockIds, times, selected)

/** A typed diagnostic retained by a compiled design audit. */
enum DesignDiagnosticKind:
  case BasisDegeneracy
  case DegenerateModulator
  case NonFiniteModulator
  case OnsetOutOfBounds
  case Other

final case class DesignDiagnostic(
    kind: DesignDiagnosticKind,
    term: Option[TermId],
    message: String
):
  require(message.trim.nonEmpty, "diagnostic message must be non-empty")

final case class EventExclusion(
    eventIndex: Int,
    reason: String,
    term: Option[TermId] = None
):
  require(eventIndex >= 0, "eventIndex must be non-negative")
  require(reason.trim.nonEmpty, "exclusion reason must be non-empty")

final case class MissingValueResolution(
    modulator: ModulatorId,
    eventIndex: Int,
    policy: String,
    action: String,
    column: Option[String] = None,
    /** Exact source identity when the value belongs to a lowered trial phase. */
    source: Option[EventRowProvenance] = None
):
  require(eventIndex >= 0, "eventIndex must be non-negative")
  require(policy.trim.nonEmpty && action.trim.nonEmpty, "missing-value policy and action must be non-empty")
  require(column.forall(_.trim.nonEmpty), "missing-value column must be non-empty when present")

/** Stable provenance for one source event row before it is lowered to scan rows.
  *
  * Event terms may be convolved into a single scan-space regressor, but the
  * source observation must remain inspectable.  `parent` identifies the
  * conceptual trial, `phase` identifies the phase that produced this event,
  * and `sourceRow` points back to the runtime table row.  The same parent may
  * legitimately occur in several phase terms; identity is therefore scoped to
  * the phase term rather than required to be globally unique.
  */
final case class EventRowProvenance(
    parent: TrialId,
    phase: Option[PhaseId],
    sourceRow: Int,
    blockId: Int,
    onset: Seconds,
    duration: Seconds
):
  require(sourceRow >= 0, "event provenance source row must be non-negative")
  require(blockId >= 0, "event provenance block id must be non-negative")
  require(onset.value.isFinite, "event provenance onset must be finite")
  require(duration.value.isFinite && duration.value >= 0.0, "event provenance duration must be finite and non-negative")

  def canonical: String =
    s"parent=${parent.value}|phase=${phase.fold("")(_.value)}|source=$sourceRow|block=$blockId|onset=${java.lang.Double.doubleToLongBits(onset.value)}|duration=${java.lang.Double.doubleToLongBits(duration.value)}"

/** A retained event's original table row, without assuming a parent trial.
  * Both indices are zero-based: eventIndex addresses the lowered term schedule;
  * sourceRow addresses the input event table before formula filtering.
  */
final case class SourceEventRow(
    term: TermId,
    eventIndex: Int,
    sourceRow: Int,
    blockId: Int,
    onset: Seconds,
    duration: Seconds
):
  require(eventIndex >= 0 && sourceRow >= 0 && blockId >= 0, "event source indices must be non-negative")
  require(onset.value.isFinite, "source event onset must be finite")
  require(duration.value.isFinite && duration.value >= 0.0, "source event duration must be finite and non-negative")

  def canonical: String =
    s"term=${term.value}|event=$eventIndex|source=$sourceRow|block=$blockId|onset=${java.lang.Double.doubleToLongBits(onset.value)}|duration=${java.lang.Double.doubleToLongBits(duration.value)}"

/** Policy for non-finite continuous event/modulator values.
  *
  * This is deliberately separate from fit-time response missingness: it is a
  * design-compilation decision about how a trial's parametric contribution is
  * constructed before convolution.
  */
enum MissingValuePolicy:
  case Reject
  case ZeroContribution
  case DropFromTerm
  case ImputeConstant(value: Double)

  def label: String =
    this match
      case Reject                 => "reject"
      case ZeroContribution      => "zero-contribution"
      case DropFromTerm          => "drop-from-term"
      case ImputeConstant(value) => s"impute-constant($value)"

  def validate: Either[DesignError, Unit] =
    this match
      case ImputeConstant(value) if !value.isFinite =>
        Left(DesignError.InvalidSchema(s"impute constant must be finite, got $value"))
      case _ => Right(())

/** Explicit scope for centering a continuous event/modulator before HRF
  * convolution.  Centering is deliberately not folded into
  * [[MissingValuePolicy]] or column normalization: those operations have
  * different scientific meanings and different audit requirements.
  */
enum CenteringPolicy:
  case None
  case GrandMean
  case WithinRun
  case WithinFactor(factor: FactorId)
  case WithinCells(factors: Vector[FactorId])
  case At(value: Double)

  def label: String =
    this match
      case None                  => "none"
      case GrandMean             => "grand-mean"
      case WithinRun             => "within-run"
      case WithinFactor(factor)  => s"within-factor(${factor.value})"
      case WithinCells(factors)  =>
        s"within-cells(${factors.map(_.value).sorted.mkString(",")})"
      case At(value)             => s"at($value)"

  def canonical: String =
    this match
      case None                 => "none"
      case GrandMean            => "grand-mean"
      case WithinRun            => "within-run"
      case WithinFactor(factor) => s"within-factor:${encode(factor.value)}"
      case WithinCells(factors) =>
        s"within-cells:${factors.map(_.value).distinct.sorted.map(encode).mkString(",")}"
      case At(value)            => s"at:${java.lang.Double.doubleToLongBits(value)}"

  def validate: Either[DesignError, Unit] =
    this match
      case WithinCells(factors) if factors.isEmpty =>
        Left(DesignError.InvalidSchema("within-cells centering requires at least one factor"))
      case WithinCells(factors) if factors.map(_.value).distinct.length != factors.length =>
        Left(DesignError.InvalidSchema("within-cells centering factors must be distinct"))
      case At(value) if !value.isFinite =>
        Left(DesignError.InvalidSchema(s"centering reference value must be finite, got $value"))
      case _ => Right(())

  private def encode(value: String): String =
    value
      .replace("%", "%25")
      .replace(":", "%3A")
      .replace(",", "%2C")

enum CenteringOutcome:
  case Applied
  case Constant
  case AllNonFinite
  case EmptyScope

  def label: String = toString.toLowerCase

/** Per-group evidence for an explicit centering operation.  Event indices
  * refer to the term's input rows, so a receipt remains inspectable even when
  * the resulting matrix has only scan rows.
  */
final case class CenteringGroupReceipt(
    key: String,
    eventIndices: Vector[Int],
    finiteEventIndices: Vector[Int],
    center: Option[Double],
    outcome: CenteringOutcome
):
  require(key.trim.nonEmpty, "centering group key must be non-empty")
  require(eventIndices.distinct.length == eventIndices.length, "centering event indices must be unique")
  require(eventIndices.forall(_ >= 0), "centering event indices must be non-negative")
  require(finiteEventIndices.forall(eventIndices.contains), "finite centering indices must belong to the group")
  require(center.forall(_.isFinite), "centering group mean must be finite when present")

  def canonical: String =
    val rows = eventIndices.sorted.mkString(",")
    val finite = finiteEventIndices.sorted.mkString(",")
    s"$key|rows=$rows|finite=$finite|center=${center.fold("")(_.toString)}|outcome=${outcome.label}"

/** Evidence for one centered continuous modulator. */
final case class CenteringReceipt(
    modulator: ModulatorId,
    source: ColumnId,
    policy: CenteringPolicy,
    groups: Vector[CenteringGroupReceipt],
    affectedEvents: Vector[Int]
):
  require(policy.validate.isRight, "centering receipt policy must be valid")
  require(groups.map(_.key).distinct.length == groups.length, "centering group keys must be unique")
  require(affectedEvents.distinct.length == affectedEvents.length, "affected centering events must be unique")
  require(affectedEvents.forall(_ >= 0), "affected centering events must be non-negative")

  def canonical: String =
    val affected = affectedEvents.sorted.mkString(",")
    s"modulator=${modulator.value}|source=${source.value}|policy=${policy.canonical}|affected=$affected|groups=${groups.sortBy(_.key).map(_.canonical).mkString(";")}"

final case class PolicyReceipt(name: String, detail: String):
  require(name.trim.nonEmpty && detail.trim.nonEmpty, "policy receipt must be named and described")

enum RankPreviewMethod:
  case PivotedQr

enum RankToleranceConvention:
  case ScaleAware
  case Absolute

/**
  * Structural numerical-rank evidence computed from the realized design.
  *
  * The preview uses column-pivoted QR with Gale's matrix-scale-aware cutoff.
  * `conditionEstimate` is the ratio of the largest to smallest accepted
  * diagonal-R magnitude; it is a cheap factorization diagnostic, not a 2-norm
  * condition number.  Stable [[ColumnId]] values make the pivot partition
  * useful before any fit is attempted.
  */
final case class RankPreviewEvidence private[design] (
    method: RankPreviewMethod,
    rows: Int,
    columns: Int,
    numericalRank: Int,
    toleranceConvention: RankToleranceConvention,
    tolerance: Double,
    pivotOrder: Vector[ColumnId],
    independentColumns: Vector[ColumnId],
    aliasedColumns: Vector[ColumnId],
    diagonalR: Vector[Double],
    conditionEstimate: Option[Double]
):
  require(rows >= 0, "rank preview rows must be non-negative")
  require(columns >= 0, "rank preview columns must be non-negative")
  require(numericalRank >= 0 && numericalRank <= columns, "rank preview rank must be within column count")
  require(tolerance >= 0.0 && tolerance.isFinite, "rank preview tolerance must be finite and non-negative")
  require(pivotOrder.length == columns, "rank preview pivot order must cover every column")
  require(pivotOrder.distinct.length == columns, "rank preview pivot order must be unique")
  require(independentColumns == pivotOrder.take(numericalRank), "rank preview independent columns must be the accepted pivots")
  require(aliasedColumns == pivotOrder.drop(numericalRank), "rank preview aliased columns must be the rejected pivots")
  require(diagonalR.length == math.min(rows, columns), "rank preview diagonal-R length must match the factorization")
  require(diagonalR.forall(value => value.isFinite && value >= 0.0), "rank preview diagonal-R values must be finite and non-negative")
  require(conditionEstimate.forall(value => value.isFinite && value >= 1.0), "rank preview condition estimate must be finite and at least one")

  def rank: Int = numericalRank

  def deficient: Boolean = numericalRank < columns

enum RankPreviewUnavailableReason:
  case NonFiniteMatrix

enum RankPreview:
  case Available(report: RankPreviewEvidence)
  case Unavailable(rowCount: Int, columnIds: Vector[ColumnId], reason: RankPreviewUnavailableReason)

  def evidence: Option[RankPreviewEvidence] =
    this match
      case Available(value) => Some(value)
      case Unavailable(_, _, _) => None

  def rows: Int =
    this match
      case Available(value) => value.rows
      case Unavailable(value, _, _) => value

  def columnCount: Int =
    this match
      case Available(value) => value.columns
      case Unavailable(_, values, _) => values.length

object RankPreview:
  private[design] def from(
      matrix: Mat,
      structuralColumns: Vector[StructuralColumn]
  ): RankPreview =
    require(matrix.cols == structuralColumns.length, "rank preview columns must match the realized matrix")
    if !matrix.data.forall(_.isFinite) then
      RankPreview.Unavailable(
        rowCount = matrix.rows,
        columnIds = structuralColumns.map(_.id),
        reason = RankPreviewUnavailableReason.NonFiniteMatrix
      )
    else if matrix.rows == 0 || matrix.cols == 0 then
      val ids = structuralColumns.map(_.id)
      RankPreview.Available(RankPreviewEvidence(
        method = RankPreviewMethod.PivotedQr,
        rows = matrix.rows,
        columns = matrix.cols,
        numericalRank = 0,
        toleranceConvention = RankToleranceConvention.ScaleAware,
        tolerance = 0.0,
        pivotOrder = ids,
        independentColumns = Vector.empty,
        aliasedColumns = ids,
        diagonalR = Vector.empty,
        conditionEstimate = None
      ))
    else
      val qr = QrDecomposition.decomposeScaleAware(matrix.data, matrix.rows, matrix.cols, pivoting = true)
      val pivot = qr.pivotOrder.map(index => structuralColumns(index).id)
      RankPreview.Available(RankPreviewEvidence(
        method = RankPreviewMethod.PivotedQr,
        rows = matrix.rows,
        columns = matrix.cols,
        numericalRank = qr.rank,
        toleranceConvention = RankToleranceConvention.ScaleAware,
        tolerance = qr.rankTolerance,
        pivotOrder = pivot,
        independentColumns = pivot.take(qr.rank),
        aliasedColumns = pivot.drop(qr.rank),
        diagonalR = qr.diagonalR,
        conditionEstimate = qr.conditionEstimate
      ))

/** Structured construction evidence retained with a compiled design. */
final case class DesignAudit(
    eventsSeen: Int = 0,
    eventsUsed: Int = 0,
    excludedEvents: Vector[EventExclusion] = Vector.empty,
    emptyCells: Vector[CellKey] = Vector.empty,
    emptyCellAudits: Vector[EmptyCellAudit] = Vector.empty,
    factorLevels: Vector[FactorLevelAudit] = Vector.empty,
    missingValues: Vector[MissingValueResolution] = Vector.empty,
    eventProvenance: Vector[EventRowProvenance] = Vector.empty,
    centeringReceipts: Vector[CenteringReceipt] = Vector.empty,
    degenerateModulatorReceipts: Vector[DegenerateModulatorReceipt] = Vector.empty,
    orthogonalizationReceipts: Vector[OrthogonalizationReceipt] = Vector.empty,
    policyReceipts: Vector[PolicyReceipt] = Vector.empty,
    rankPreview: Option[RankPreview] = None,
    diagnostics: Vector[DesignDiagnostic] = Vector.empty,
    sourceEvents: Vector[SourceEventRow] = Vector.empty,
    responseSupport: Vector[EventSupportReceipt] = Vector.empty
):
  require(eventsSeen >= 0 && eventsUsed >= 0, "event counts must be non-negative")
  require(eventsUsed <= eventsSeen, "eventsUsed cannot exceed eventsSeen")
  require(
    emptyCellAudits.map(_.canonical).distinct.length == emptyCellAudits.length,
    "empty-cell audit entries must be unique"
  )

  def canonical: String =
    val exclusions = excludedEvents
      .map(e => s"${e.eventIndex}:${e.reason}:${e.term.fold("")(_.value)}")
      .mkString(",")
    val cells = emptyCells.map(_.canonical).mkString(",")
    val cellAudits = emptyCellAudits.map(_.canonical).mkString(",")
    val factors = factorLevels.map(_.canonical).mkString(",")
    val missing = missingValues
      .map { value =>
        s"${value.modulator.value}:${value.eventIndex}:${value.policy}:${value.action}:${value.column.getOrElse("")}:" +
          value.source.fold("")(_.canonical)
      }
      .mkString(",")
    val provenance = eventProvenance.map(_.canonical).mkString(",")
    val centering = centeringReceipts.map(_.canonical).mkString(",")
    val degenerateModulators = degenerateModulatorReceipts.map(_.canonical).mkString(",")
    val orthogonalization = orthogonalizationReceipts.map(_.canonical).mkString(",")
    val policies = policyReceipts.map(p => s"${p.name}:${p.detail}").mkString(",")
    val rank = rankPreview.fold("") {
      case RankPreview.Available(preview) =>
        val pivots = preview.pivotOrder.map(_.value).mkString(",")
        val independent = preview.independentColumns.map(_.value).mkString(",")
        val aliased = preview.aliasedColumns.map(_.value).mkString(",")
        val diagonal = preview.diagonalR.mkString(",")
        s"${preview.method}:${preview.rows}:${preview.columns}:${preview.numericalRank}:${preview.toleranceConvention}:${preview.tolerance}:" +
          s"pivots=$pivots:independent=$independent:aliased=$aliased:diagonal=$diagonal:" +
          s"condition=${preview.conditionEstimate.fold("")(_.toString)}"
      case RankPreview.Unavailable(rows, columns, reason) =>
        s"unavailable:$reason:rows=$rows:columns=${columns.map(_.value).mkString(",")}"
    }
    val diags = diagnostics.map(d => s"${d.kind}:${d.term.fold("")(_.value)}:${d.message}").mkString(",")
    val support = if responseSupport.isEmpty then "" else responseSupport.map(_.canonical).mkString(";response-support=", "|", "")
    val sources = if sourceEvents.isEmpty then "" else sourceEvents.map(_.canonical).mkString(";source-events=", ",", "")
    s"seen=$eventsSeen;used=$eventsUsed;excluded=$exclusions;empty=$cells;empty-audits=$cellAudits;factors=$factors;missing=$missing;provenance=$provenance;centering=$centering;degenerate-modulators=$degenerateModulators;orthogonalization=$orthogonalization;policies=$policies;rank=$rank;diagnostics=$diags" + sources + support

/** A stable, cross-platform identity for a compiled matrix and its semantics. */
final case class DesignFingerprint private (value: String, canonicalEncoding: String)

object DesignFingerprint:
  private val schemaVersion = "design-schema/v1"

  def from(
      matrix: Mat,
      rows: RowLayout,
      columns: Vector[StructuralColumn],
      audit: DesignAudit
  ): DesignFingerprint =
    val encoding = canonicalEncoding(matrix, rows, columns, audit)
    DesignFingerprint(s"$schemaVersion:${hash(encoding)}", encoding)

  private[design] def canonicalEncoding(
      matrix: Mat,
      rows: RowLayout,
      columns: Vector[StructuralColumn],
      audit: DesignAudit
  ): String =
    val values = matrix.data
      .map(v => java.lang.Double.doubleToLongBits(v).toString)
      .mkString(",")
    val cols = columns.map(_.canonical).mkString(";")
    s"$schemaVersion|matrix=${matrix.rows}x${matrix.cols}|rows=${rows.canonical}|columns=$cols|audit=${audit.canonical}|values=$values"

  private def hash(value: String): String =
    // FNV-style 64-bit rolling hash.  It is an identity, not a security hash;
    // the full canonical encoding is retained for diagnostics and receipts.
    var h = -3750763034362895579L
    var i = 0
    while i < value.length do
      h = (h ^ value.charAt(i).toLong) * 1099511628211L
      i += 1
    java.lang.Long.toHexString(h)

/** Matrix plus its structural schema. */
final case class DesignSchema private (
    matrix: Mat,
    rows: RowLayout,
    columns: Vector[StructuralColumn],
    audit: DesignAudit,
    rankPreview: RankPreview,
    fingerprint: DesignFingerprint
):
  require(matrix.rows == rows.rows, "matrix rows must equal RowLayout rows")
  require(matrix.cols == columns.length, "matrix columns must equal structural column count")
  require(audit.rankPreview.contains(rankPreview), "design audit must retain the authoritative rank preview")

  def rowLayout: RowLayout = rows

  def columnIds: Vector[ColumnId] = columns.map(_.id)

  def columnNames: Vector[String] = columns.map(_.renderedLabel)

  /** The immutable coefficient axis carried beyond matrix compilation.
    *
    * The matrix itself remains owned by `DesignSchema`; this value carries
    * only the identity and semantic axis needed by model, fit, and result
    * artifacts.  It is therefore safe to retain without copying the matrix or
    * creating a second source of column truth.
    */
  def coefficientAxis: CoefficientAxis =
    CoefficientAxis.fromSchema(this)

  /** Replace presentation labels after a legacy case-class copy.  Structural
    * identity and the fingerprint intentionally remain unchanged.
    */
  def withRenderedLabels(labels: Vector[String]): Either[DesignError, DesignSchema] =
    if labels.length != columns.length then
      Left(DesignError.InvalidSchema(s"expected ${columns.length} rendered labels, got ${labels.length}"))
    else if labels.exists(_.trim.isEmpty) then
      Left(DesignError.InvalidSchema("rendered labels must be non-empty"))
    else
      Right(
        DesignSchema(
          matrix = matrix,
          rows = rows,
          columns = columns.zip(labels).map { case (column, label) =>
            StructuralColumn(column.id, column.ordinal, column.origin, label, label, column.hrfScale)
          },
          audit = audit,
          rankPreview = rankPreview,
          fingerprint = fingerprint
        )
      )

  def validate: Either[DesignError, Unit] =
    DesignSchema.validate(matrix, rows, columns, audit, rankPreview, fingerprint)

  /** Lower one compiled run into an explicit local coefficient projection.
    *
    * A runwise fit must not factor columns that have no support in that run.
    * In particular, a run-local intercept or drift is represented in the
    * source design as a block-diagonal column and is zero everywhere else.
    * This projection keeps the source column identity, classifies every
    * source column, and selects only columns that can participate in the
    * run-local factorization.
    */
  def runwiseProjection(run: RunIndex): Either[DesignError, RunCoefficientProjection] =
    runwiseProjection(run, rows.selectedRows)

  def runwiseProjection(
      run: RunIndex,
      selectedRows: Vector[ScanIndex],
      supportTolerance: Double = 0.0
  ): Either[DesignError, RunCoefficientProjection] =
    if supportTolerance < 0.0 || !supportTolerance.isFinite then
      Left(DesignError.InvalidSchema("runwise support tolerance must be finite and non-negative"))
    else if selectedRows.isEmpty then
      Left(DesignError.InvalidSchema("runwise selected rows must be non-empty"))
    else if selectedRows.distinct.length != selectedRows.length then
      Left(DesignError.InvalidSchema("runwise selected rows must not contain duplicates"))
    else if selectedRows.exists(row => row.oneBased < 1 || row.oneBased > rows.rows) then
      Left(DesignError.InvalidSchema("runwise selected rows must point into the compiled design"))
    else
      val sourceRows = selectedRows.map(_.oneBased - 1).filter(index => rows.blockIds(index) == run)
      if sourceRows.isEmpty then
        Left(DesignError.InvalidSchema(s"run ${run.oneBased} has no selected rows in the compiled design"))
      else
        for
          runAxis <- coefficientAxis.forRunwiseCoefficient(run)
          decisions <- columnDecisions(run, sourceRows, supportTolerance)
          selected <- selectedColumns(decisions, run)
          localAxis <- runAxis.select(selected)
        yield RunCoefficientProjection(
          run = run,
          selectedRows = selectedRows,
          sourceAxis = coefficientAxis,
          axis = localAxis,
          sourceColumnIndices = selected,
          decisions = decisions,
          supportTolerance = supportTolerance
        )

  /** Lower one compiled run using the existing projection-aware slice. */
  def runwiseSlice(run: RunIndex): Either[DesignError, RunwiseDesignSlice] =
    runwiseSlice(run, rows.selectedRows)

  /** Lower one compiled run using an explicit selected-row view.  This is the
    * seam used when a fit applies censoring or another response selection that
    * is not already encoded in the compiled [[RowLayout]].  The selected scan
    * indices remain in source-matrix coordinates, so a caller can audit the
    * exact observations used for semantic estimability checks.
    */
  def runwiseSlice(
      run: RunIndex,
      selectedRows: Vector[ScanIndex]
  ): Either[DesignError, RunwiseDesignSlice] =
    runwiseProjection(run, selectedRows).map { projection =>
      val sourceRows = selectedRows.map(_.oneBased - 1).filter(index => rows.blockIds(index) == run)
      val out = new Array[Double](sourceRows.length * projection.sourceColumnIndices.length)
      var outRow = 0
      while outRow < sourceRows.length do
        val sourceRow = sourceRows(outRow)
        var outCol = 0
        while outCol < projection.sourceColumnIndices.length do
          val sourceCol = projection.sourceColumnIndices(outCol)
          out(outRow * projection.sourceColumnIndices.length + outCol) = matrix(sourceRow, sourceCol)
          outCol += 1
        outRow += 1
      RunwiseDesignSlice(
        run = run,
        sourceRowIndices = sourceRows,
        sourceColumnIndices = projection.sourceColumnIndices,
        matrix = Mat.unsafe(sourceRows.length, projection.sourceColumnIndices.length, out),
        axis = projection.axis,
        projection = projection
      )
    }

  private def columnDecisions(
      run: RunIndex,
      sourceRows: Vector[Int],
      supportTolerance: Double
  ): Either[DesignError, Vector[RunColumnDecision]] =
    val out = Vector.newBuilder[RunColumnDecision]
    var column = 0
    while column < columns.length do
      val structural = columns(column)
      structural.origin match
        case _: StructuralColumnOrigin.Legacy =>
          return Left(DesignError.InvalidSchema("runwise coefficient projection requires a compiled non-legacy design schema"))
        case origin =>
          var support = 0.0
          var row = 0
          while row < sourceRows.length do
            val value = math.abs(matrix(sourceRows(row), column))
            if value > support then support = value
            row += 1
          val disposition = runColumnDisposition(origin, run, support > supportTolerance)
          out += RunColumnDecision(column, structural, disposition, support)
      column += 1
    Right(out.result())

  private def selectedColumns(
      decisions: Vector[RunColumnDecision],
      run: RunIndex
  ): Either[DesignError, Vector[Int]] =
    val selected = decisions.collect {
      case RunColumnDecision(index, _, RunColumnDisposition.SharedEstimand, _) => index
      case RunColumnDecision(index, _, RunColumnDisposition.RunLocalNuisance, _) => index
      case RunColumnDecision(index, _, RunColumnDisposition.RunLocalEstimand, _) => index
    }
    if selected.isEmpty then
      Left(DesignError.InvalidSchema(s"run ${run.oneBased} has no estimable or supported coefficient columns"))
    else Right(selected)

  private def runColumnDisposition(
      origin: StructuralColumnOrigin,
      run: RunIndex,
      supported: Boolean
  ): RunColumnDisposition =
    def localDisposition: RunColumnDisposition =
      if isNuisance(origin) then RunColumnDisposition.RunLocalNuisance
      else RunColumnDisposition.RunLocalEstimand

    if !supported then
      runEmptyCellAudit(origin, run) match
        case Some(value) if value.disposition == EmptyCellDisposition.Omitted =>
          RunColumnDisposition.Omitted(s"cell omitted in run ${run.oneBased} by policy '${value.policy.label}'")
        case Some(value) =>
          RunColumnDisposition.NonEstimable(s"cell retained without support in run ${run.oneBased} by policy '${value.policy.label}'")
        case None => unsupportedRunColumnDisposition(origin, run)
    else
      originRunScope(origin) match
        case RunScope.Global     => RunColumnDisposition.SharedEstimand
        case RunScope.PerRun     => localDisposition
        case RunScope.Run(index) =>
          if index == run then localDisposition
          else RunColumnDisposition.Omitted(s"column belongs to run ${index.oneBased}")

  private def unsupportedRunColumnDisposition(
      origin: StructuralColumnOrigin,
      run: RunIndex
  ): RunColumnDisposition =
    originRunScope(origin) match
      case RunScope.Global =>
        RunColumnDisposition.NonEstimable("shared column has no support in this run")
      case RunScope.PerRun =>
        RunColumnDisposition.Omitted("per-run column has no support in this run")
      case RunScope.Run(index) =>
        if index == run then RunColumnDisposition.NonEstimable("run-local column has no support in its declared run")
        else RunColumnDisposition.Omitted(s"column belongs to run ${index.oneBased}")

  private def runEmptyCellAudit(
      origin: StructuralColumnOrigin,
      run: RunIndex
  ): Option[EmptyCellAudit] =
    origin match
      case StructuralColumnOrigin.Event(term, _, cell, _, _, _, _) =>
        audit.emptyCellAudits.find { value =>
          value.run.contains(run) && value.term.contains(term) && value.cell == cell
        }
      case _ => None

  private def originRunScope(origin: StructuralColumnOrigin): RunScope =
    origin match
      case StructuralColumnOrigin.Event(_, _, _, _, _, _, scope) => scope
      case StructuralColumnOrigin.Sampled(_, _, scope)            => scope
      case StructuralColumnOrigin.Intercept(scope)               => scope
      case StructuralColumnOrigin.Drift(_, _, scope)              => scope
      case StructuralColumnOrigin.Nuisance(_, _, scope)           => scope
      case StructuralColumnOrigin.Baseline(_, _, _, scope)        => scope
      case StructuralColumnOrigin.Legacy(_, _, _)                 => RunScope.Global

  private def isNuisance(origin: StructuralColumnOrigin): Boolean =
    def roleIsNuisance(role: ColumnRole): Boolean =
      role match
        case ColumnRole.Task | ColumnRole.Trial | ColumnRole.TrialAggregate | ColumnRole.Covariate => false
        case _ => true

    origin match
      case StructuralColumnOrigin.Intercept(_) => true
      case StructuralColumnOrigin.Drift(_, _, _) => true
      case StructuralColumnOrigin.Nuisance(_, _, _) => true
      case StructuralColumnOrigin.Baseline(_, role, _, _) => roleIsNuisance(role)
      case StructuralColumnOrigin.Sampled(_, role, _) =>
        roleIsNuisance(role)
      case StructuralColumnOrigin.Event(_, _, _, _, _, role, _) =>
        roleIsNuisance(role)
      case StructuralColumnOrigin.Legacy(_, _, _) => true

/** A checked run-local design view used to lower run-specific hypotheses. */
final case class RunwiseDesignSlice private[design] (
    run: RunIndex,
    sourceRowIndices: Vector[Int],
    sourceColumnIndices: Vector[Int],
    matrix: Mat,
    axis: CoefficientAxis,
    projection: RunCoefficientProjection
):
  require(sourceRowIndices.nonEmpty, "runwise design slices must contain at least one source row")
  require(sourceColumnIndices.nonEmpty, "runwise design slices must contain at least one source column")
  require(matrix.rows == sourceRowIndices.length, "runwise slice rows must match source row indices")
  require(matrix.cols == axis.predictors, "runwise slice columns must match coefficient axis")
  require(sourceColumnIndices.length == axis.predictors, "runwise source columns must match coefficient axis")

/** The disposition of a source-design column in one run-local estimand. */
enum RunColumnDisposition:
  case SharedEstimand
  case RunLocalNuisance
  case RunLocalEstimand
  case Omitted(reason: String)
  case NonEstimable(reason: String)

/** A source-column decision retained by a run coefficient projection. */
final case class RunColumnDecision(
    sourceColumnIndex: Int,
    sourceColumn: StructuralColumn,
    disposition: RunColumnDisposition,
    support: Double
):
  require(sourceColumnIndex >= 0, "run projection source column index must be non-negative")
  require(support.isFinite && support >= 0.0, "run projection support must be finite and non-negative")

/** Structural and numerical mapping for one independently fitted run. */
final case class RunCoefficientProjection private[design] (
    run: RunIndex,
    selectedRows: Vector[ScanIndex],
    sourceAxis: CoefficientAxis,
    axis: CoefficientAxis,
    sourceColumnIndices: Vector[Int],
    decisions: Vector[RunColumnDecision],
    supportTolerance: Double
):
  require(selectedRows.nonEmpty, "run coefficient projections must retain selected rows")
  require(sourceColumnIndices.nonEmpty, "run coefficient projections must retain source columns")
  require(sourceColumnIndices.distinct.length == sourceColumnIndices.length, "run projection source columns must be unique")
  require(sourceColumnIndices.forall(index => index >= 0 && index < sourceAxis.predictors), "run projection source column out of bounds")
  require(axis.predictors == sourceColumnIndices.length, "run projection axis must match source column count")
  require(decisions.length == sourceAxis.predictors, "run projection must classify every source column")
  require(supportTolerance.isFinite && supportTolerance >= 0.0, "run projection tolerance must be finite and non-negative")

  def sharedSourceColumnIndices: Vector[Int] =
    decisions.collect { case RunColumnDecision(index, _, RunColumnDisposition.SharedEstimand, _) => index }

  def runLocalSourceColumnIndices: Vector[Int] =
    decisions.collect {
      case RunColumnDecision(index, _, RunColumnDisposition.RunLocalNuisance, _) => index
      case RunColumnDecision(index, _, RunColumnDisposition.RunLocalEstimand, _) => index
    }

  def omitted: Vector[RunColumnDecision] =
    decisions.collect { case decision @ RunColumnDecision(_, _, RunColumnDisposition.Omitted(_), _) => decision }

  def nonEstimable: Vector[RunColumnDecision] =
    decisions.collect { case decision @ RunColumnDecision(_, _, RunColumnDisposition.NonEstimable(_), _) => decision }

  def sourceIndexForLocal(localIndex: Int): Option[Int] =
    sourceColumnIndices.lift(localIndex)

  def sourceColumnIds: Vector[ColumnId] = sourceColumnIndices.map(sourceAxis.columns(_).id)

/** Structural coefficient identity detached from the numerical matrix.
  *
  * `designFingerprint` identifies the complete compiled design (including
  * rows and audit).  `columns` is the ordered coefficient axis consumed by a
  * fit result; it may be a validated subset for estimators such as LSS while
  * retaining the fingerprint of the source design.
  */
final case class CoefficientAxis private[design] (
    designFingerprint: DesignFingerprint,
    rowLayout: RowLayout,
    audit: DesignAudit,
    columns: Vector[StructuralColumn]
):
  require(columns.nonEmpty, "coefficient axis must contain at least one column")
  require(columns.map(_.id.value).distinct.length == columns.length, "coefficient axis column ids must be unique")

  def predictors: Int = columns.length
  def columnIds: Vector[ColumnId] = columns.map(_.id)
  def columnNames: Vector[String] = columns.map(_.renderedLabel)

  /** Structural compatibility deliberately ignores display labels. */
  def structurallyCompatible(other: CoefficientAxis): Boolean =
    designFingerprint == other.designFingerprint && columnIds == other.columnIds

  def select(indices: Vector[Int]): Either[DesignError, CoefficientAxis] =
    if indices.isEmpty then
      Left(DesignError.InvalidSchema("coefficient axis selection must be non-empty"))
    else if indices.distinct.length != indices.length then
      Left(DesignError.InvalidSchema("coefficient axis selection must not repeat columns"))
    else if indices.exists(index => index < 0 || index >= columns.length) then
      Left(DesignError.InvalidSchema("coefficient axis selection contains an out-of-bounds column"))
    else Right(copy(columns = indices.map(columns)))

  /** Return the structural axis for coefficients estimated independently in
    * one run.  The numerical design remains the same; only the semantic
    * run-scope of global/per-run column families is lowered.  This distinction
    * prevents a runwise result from being mistaken for a shared-across-runs
    * coefficient vector while preserving explicit design-owned run identities.
    */
  def forRunwiseCoefficient(run: RunIndex): Either[DesignError, CoefficientAxis] =
    if columns.exists(_.origin.isInstanceOf[StructuralColumnOrigin.Legacy]) then
      Left(DesignError.InvalidSchema("runwise structural coefficient identity requires a compiled non-legacy design schema"))
    else
      val scopedColumns = columns.map { column =>
        val origin = column.origin.forRunwiseCoefficient(run)
        StructuralColumn.fromOrigin(
          ordinal = column.ordinal.oneBased,
          origin = origin,
          label = column.label,
          prettyLabel = column.prettyLabel,
          hrfScale = column.hrfScale
        )
      }
      scopedColumns.foldLeft[Either[DesignError, Vector[StructuralColumn]]](Right(Vector.empty)) {
        case (Left(error), _) => Left(error)
        case (Right(values), Right(column)) => Right(values :+ column)
        case (Right(_), Left(error)) => Left(error)
      }.map(columns => copy(columns = columns))

object CoefficientAxis:
  private[design] def fromSchema(schema: DesignSchema): CoefficientAxis =
    CoefficientAxis(
      designFingerprint = schema.fingerprint,
      rowLayout = schema.rows,
      audit = schema.audit,
      columns = schema.columns
    )

object DesignSchema:
  def validated(
      matrix: Mat,
      rows: RowLayout,
      columns: Vector[StructuralColumn],
      audit: DesignAudit = DesignAudit()
  ): Either[DesignError, DesignSchema] =
    validateShape(matrix, rows, columns).map { _ =>
      val rankPreview = RankPreview.from(matrix, columns)
      val verifiedAudit = audit.copy(rankPreview = Some(rankPreview))
      val fingerprint = DesignFingerprint.from(matrix, rows, columns, verifiedAudit)
      DesignSchema(matrix, rows, columns, verifiedAudit, rankPreview, fingerprint)
    }

  def legacy(
      matrix: Mat,
      samplingFrame: SamplingFrame,
      columnNames: Vector[String],
      source: ModelSource
  ): DesignSchema =
    val rows = RowLayout.fromSamplingFrame(samplingFrame)
    val columns = columnNames.zipWithIndex.map { case (name, i) =>
      val origin = StructuralColumnOrigin.Legacy(source, name, i + 1)
      StructuralColumn(
        id = ColumnId.unsafe(s"legacy|${source.toString}|$i|$name"),
        ordinal = DesignColumnIndex.unsafeOneBased(i + 1),
        origin = origin,
        label = if name.trim.isEmpty then s"column_${i + 1}" else name,
        prettyLabel = name
      )
    }
    validated(matrix, rows, columns).fold(
      error => throw new IllegalArgumentException(error.message),
      identity
    )

  def combine(left: DesignSchema, right: DesignSchema): Either[DesignError, DesignSchema] =
    if left.rows != right.rows then
      Left(DesignError.InvalidSchema("cannot combine designs with different RowLayouts"))
    else
      val columns =
        (left.columns ++ right.columns).zipWithIndex.map { case (column, i) =>
          column.copy(ordinal = DesignColumnIndex.unsafeOneBased(i + 1))
        }
      validated(left.matrix ++ right.matrix, left.rows, columns, combineAudits(left.audit, right.audit))

  private def combineAudits(left: DesignAudit, right: DesignAudit): DesignAudit =
    DesignAudit(
      eventsSeen = left.eventsSeen + right.eventsSeen,
      eventsUsed = left.eventsUsed + right.eventsUsed,
      excludedEvents = left.excludedEvents ++ right.excludedEvents,
      emptyCells = (left.emptyCells ++ right.emptyCells).distinct,
      emptyCellAudits = (left.emptyCellAudits ++ right.emptyCellAudits).distinct,
      factorLevels = (left.factorLevels ++ right.factorLevels).distinct,
      missingValues = left.missingValues ++ right.missingValues,
      eventProvenance = left.eventProvenance ++ right.eventProvenance,
      sourceEvents = left.sourceEvents ++ right.sourceEvents,
      responseSupport = left.responseSupport ++ right.responseSupport,
      centeringReceipts = left.centeringReceipts ++ right.centeringReceipts,
      degenerateModulatorReceipts = left.degenerateModulatorReceipts ++ right.degenerateModulatorReceipts,
      orthogonalizationReceipts = left.orthogonalizationReceipts ++ right.orthogonalizationReceipts,
      policyReceipts = left.policyReceipts ++ right.policyReceipts,
      rankPreview = None,
      diagnostics = left.diagnostics ++ right.diagnostics
    )

  private[design] def validate(
      matrix: Mat,
      rows: RowLayout,
      columns: Vector[StructuralColumn],
      audit: DesignAudit,
      rankPreview: RankPreview,
      fingerprint: DesignFingerprint
  ): Either[DesignError, Unit] =
    validateShape(matrix, rows, columns).flatMap { _ =>
      val expectedPreview = RankPreview.from(matrix, columns)
      val expected = DesignFingerprint.from(matrix, rows, columns, audit)
      if rankPreview != expectedPreview || !audit.rankPreview.contains(expectedPreview) then
        Left(DesignError.InvalidSchema("design rank preview does not match the realized matrix and structural columns"))
      else if expected.value != fingerprint.value then
        Left(DesignError.InvalidSchema("design fingerprint does not match matrix, rows, columns, and audit"))
      else Right(())
    }

  private def validateShape(
      matrix: Mat,
      rows: RowLayout,
      columns: Vector[StructuralColumn]
  ): Either[DesignError, Unit] =
    if matrix.rows != rows.rows then
      Left(DesignError.InvalidSchema(s"matrix has ${matrix.rows} rows but RowLayout has ${rows.rows}"))
    else if matrix.cols != columns.length then
      Left(DesignError.InvalidSchema(s"matrix has ${matrix.cols} columns but schema has ${columns.length}"))
    else if columns.map(_.id.value).distinct.length != columns.length then
      Left(DesignError.InvalidSchema("structural column ids must be unique"))
    else if columns.zipWithIndex.exists { case (column, i) => column.ordinal.oneBased != i + 1 } then
      Left(DesignError.InvalidSchema("structural column ordinals must be contiguous and one-based"))
    else Right(())
