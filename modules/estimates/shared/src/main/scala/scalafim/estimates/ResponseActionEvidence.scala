package scalafim.estimates

import scalafim.archive.ContentDigest

/** Text is admissible for canonical UTF-8 encoding only without unpaired
  * surrogates; otherwise encoding would silently substitute characters.
  */
private[estimates] object WellFormed:
  def text(value: String): Boolean =
    var i = 0
    var ok = true
    while ok && i < value.length do
      val c = value.charAt(i)
      if Character.isHighSurrogate(c) then
        ok = i + 1 < value.length && Character.isLowSurrogate(value.charAt(i + 1))
        i += 2
      else
        ok = !Character.isLowSurrogate(c)
        i += 1
    ok

/** A digest derived by a ScalaFIM binder from native provenance. It is identity
  * evidence (two records agree or differ), never scientific truth or an
  * authenticated origin.
  */
final case class ProviderDigest(schema: String, digest: ContentDigest):
  require(Invariants.text(schema) && WellFormed.text(schema), "provider digest schema must be nonempty well-formed text")
  require(digest.algorithm == "sha256" && digest.value.matches("[0-9a-f]{64}"), "provider digests are lowercase SHA-256")

  def render: String = s"$schema@${digest.render}"

final case class ConditionLevelId(value: String):
  require(Invariants.text(value) && WellFormed.text(value), "ConditionLevelId must be nonempty well-formed text without control characters")

final case class ReadoutRowId(condition: ConditionLevelId, bin: Int):
  require(bin >= 0, "readout bins are zero-based")

  def render: String = s"${condition.value}#$bin"

/** Structural defects of a readout row axis or of a raw row permutation. */
enum ReadoutDefect:
  case EmptyAxis
  case DuplicateRow(row: ReadoutRowId)
  /** A condition's rows are not one contiguous block. */
  case NotConditionMajor(condition: ConditionLevelId)
  /** A condition's bins are not exactly 0, 1, ..., b-1 in ascending order. */
  case BinsNotSequential(condition: ConditionLevelId)
  case UnequalBins(condition: ConditionLevelId, expected: Int, actual: Int)
  /** The raw images are not a permutation of the axis rows. */
  case NotRowPermutation
  /** The bins of one condition are sent to more than one condition. */
  case BinsSplit(condition: ConditionLevelId)
  /** Whole blocks move together, but a bin changes its within-condition index. */
  case BinsPermuted(condition: ConditionLevelId)

  def message: String = this match
    case EmptyAxis => "readout axis has no rows"
    case DuplicateRow(row) => s"readout row ${row.render} is repeated"
    case NotConditionMajor(condition) => s"rows of condition ${condition.value} are not contiguous (axis is not condition-major)"
    case BinsNotSequential(condition) => s"bins of condition ${condition.value} are not 0..b-1 in order"
    case UnequalBins(condition, expected, actual) => s"condition ${condition.value} has $actual bins; expected $expected"
    case NotRowPermutation => "raw row images are not a permutation of the axis rows"
    case BinsSplit(condition) => s"bins of condition ${condition.value} are split across conditions"
    case BinsPermuted(condition) => s"bins of condition ${condition.value} change their within-condition index"

/** A checked condition-major readout axis with equal, complete bin blocks.
  * The only constructor is [[ReadoutAxis.parse]]. Deliberately not a case
  * class: a case-class companion exposes a public `fromProduct` (Mirror)
  * that would bypass the check.
  */
final class ReadoutAxis private (val rows: Vector[ReadoutRowId]):
  require(ReadoutAxis.defect(rows).isEmpty, "readout axis must be condition-major with equal complete bins")

  def conditions: Vector[ConditionLevelId] = rows.map(_.condition).distinct
  def bins: Int = rows.size / conditions.size
  def render: String = rows.map(_.render).mkString("[", ",", "]")

  override def equals(other: Any): Boolean = other match
    case that: ReadoutAxis => rows == that.rows
    case _ => false
  override def hashCode: Int = rows.hashCode
  override def toString: String = s"ReadoutAxis($render)"

object ReadoutAxis:
  def parse(rows: Vector[ReadoutRowId]): Either[ReadoutDefect, ReadoutAxis] =
    defect(rows).toLeft(new ReadoutAxis(rows))

  /** One linear pass with hash maps. Defect order and choice: the first
    * repeated row in row order; then, in first-appearance order of
    * conditions, the first non-contiguous condition, the first with bins other
    * than 0..b-1 in order, and the first whose block size differs from the
    * first condition's.
    */
  private[estimates] def defect(rows: Vector[ReadoutRowId]): Option[ReadoutDefect] =
    if rows.isEmpty then Some(ReadoutDefect.EmptyAxis)
    else
      val seen = scala.collection.mutable.HashSet.empty[ReadoutRowId]
      val index = scala.collection.mutable.HashMap.empty[ConditionLevelId, Int]
      val order = Vector.newBuilder[ConditionLevelId]
      val first = scala.collection.mutable.ArrayBuffer.empty[Int]
      val last = scala.collection.mutable.ArrayBuffer.empty[Int]
      val size = scala.collection.mutable.ArrayBuffer.empty[Int]
      val sequential = scala.collection.mutable.ArrayBuffer.empty[Boolean]
      var duplicate: Option[ReadoutRowId] = None
      var r = 0
      while r < rows.length do
        val row = rows(r)
        if duplicate.isEmpty && !seen.add(row) then duplicate = Some(row)
        val c = index.getOrElseUpdate(row.condition, {
          order += row.condition
          first += r; last += r; size += 0; sequential += true
          first.length - 1
        })
        last(c) = r
        if row.bin != size(c) then sequential(c) = false
        size(c) += 1
        r += 1
      val conditions = order.result()
      val n = conditions.length
      def firstWhere(p: Int => Boolean): Option[Int] =
        var c = 0
        while c < n && !p(c) do c += 1
        Option.when(c < n)(c)
      duplicate.map(ReadoutDefect.DuplicateRow.apply)
        .orElse(firstWhere(c => last(c) - first(c) + 1 != size(c)).map(c => ReadoutDefect.NotConditionMajor(conditions(c))))
        .orElse(firstWhere(c => !sequential(c)).map(c => ReadoutDefect.BinsNotSequential(conditions(c))))
        .orElse(firstWhere(c => size(c) != size(0)).map(c => ReadoutDefect.UnequalBins(conditions(c), size(0), size(c))))

/** A condition-level action; it moves whole bin blocks by construction. */
enum ConditionAction:
  case Permute(map: Map[ConditionLevelId, ConditionLevelId])

  def isIdentity: Boolean = this match
    case Permute(map) => map.forall((from, to) => from == to)

object ConditionAction:
  /** Parse a raw readout-row permutation (row `rows(i)` is sent to `images(i)`)
    * into a condition action, refusing any map that is not `P ⊗ I_bins` on a
    * checked condition-major axis.
    */
  def parseRows(rows: Vector[ReadoutRowId], images: Vector[ReadoutRowId]): Either[ReadoutDefect, ConditionAction] =
    ReadoutAxis.parse(rows).flatMap: axis =>
      if images.size != rows.size || images.toSet != rows.toSet then Left(ReadoutDefect.NotRowPermutation)
      else
        val pairs = rows.zip(images)
        val split = axis.conditions.find(c => pairs.collect { case (from, to) if from.condition == c => to.condition }.distinct.size != 1)
        split match
          case Some(condition) => Left(ReadoutDefect.BinsSplit(condition))
          case None =>
            pairs.collectFirst { case (from, to) if from.bin != to.bin => ReadoutDefect.BinsPermuted(from.condition) }
              .toLeft(ConditionAction.Permute(pairs.map((from, to) => from.condition -> to.condition).toMap))

/** Realized noise processing, recorded by the binder from native provenance.
  * A missing native record is `Unrecorded`, never a default.
  */
enum NoiseScope:
  case Global
  case PerRun
  case PerFeature

enum RealizedNoise:
  case White
  /** `exact` covers AR coefficients, MA terms, initialization and run resets;
    * `None` when native provenance does not record all of them.
    */
  case FixedAr(order: Int, pooling: NoiseScope, exact: Option[ProviderDigest])
  case EstimatedAr(order: Int, pooling: NoiseScope)
  case Robust
  case LearnedSubspace
  case Unrecorded

  def arOrder: Option[Int] = this match
    case FixedAr(order, _, _) => Some(order)
    case EstimatedAr(order, _) => Some(order)
    case _ => None

  def render: String = this match
    case White => "White"
    case FixedAr(order, pooling, exact) => s"FixedAr($order,$pooling,${exact.fold("unrecorded")(_.render)})"
    case EstimatedAr(order, pooling) => s"EstimatedAr($order,$pooling)"
    case Robust => "Robust"
    case LearnedSubspace => "LearnedSubspace"
    case Unrecorded => "Unrecorded"

enum RealizedCombination:
  case SingleRun
  case FixedWeights(digest: ProviderDigest)
  case EstimatedWeights
  case Unrecorded

  def render: String = this match
    case SingleRun => "SingleRun"
    case FixedWeights(digest) => s"FixedWeights(${digest.render})"
    case EstimatedWeights => "EstimatedWeights"
    case Unrecorded => "Unrecorded"

/** Source-bound identity of one selected first-level readout; the feature
  * digest covers ordered physical sample IDs plus domain identity. Only
  * ScalaFIM binders issue it (OD-8: procedural `private[scalafim]` trust in
  * v1; a forged value can at worst mislabel a refusal, because v1 never
  * admits). Not a case class, so there is no public `fromProduct`.
  */
final class ResponseSourceBinding private[scalafim] (
    val unit: UnitRevisionId,
    val observation: ObservationId,
    val design: ProviderDigest,
    val preparation: ProviderDigest,
    val noise: ProviderDigest,
    val runCombination: ProviderDigest,
    val readout: ProviderDigest,
    val readoutAxis: ReadoutAxis,
    val columns: Vector[ColumnId],
    val selected: Vector[ColumnId],
    val features: ProviderDigest,
    val realizedNoise: RealizedNoise,
    val realizedCombination: RealizedCombination
):
  require(Invariants.unique(columns), "binding columns must be unique and nonempty")
  require(Invariants.unique(selected) && selected.toSet.subsetOf(columns.toSet), "selected columns must be a unique nonempty subset")
  require(realizedNoise.arOrder.forall(_ >= 1), "AR order must be positive")
  require(WellFormed.text(observation.value) && columns.forall(c => WellFormed.text(c.value)),
    "binding text must be well-formed UTF-16 so that its canonical encoding is lossless")

  private def fields: Product = (unit, observation, design, preparation, noise, runCombination, readout, readoutAxis, columns, selected, features, realizedNoise, realizedCombination)

  private[scalafim] def copy(
      unit: UnitRevisionId = unit,
      observation: ObservationId = observation,
      design: ProviderDigest = design,
      preparation: ProviderDigest = preparation,
      noise: ProviderDigest = noise,
      runCombination: ProviderDigest = runCombination,
      readout: ProviderDigest = readout,
      readoutAxis: ReadoutAxis = readoutAxis,
      columns: Vector[ColumnId] = columns,
      selected: Vector[ColumnId] = selected,
      features: ProviderDigest = features,
      realizedNoise: RealizedNoise = realizedNoise,
      realizedCombination: RealizedCombination = realizedCombination
  ): ResponseSourceBinding = new ResponseSourceBinding(unit, observation, design, preparation, noise, runCombination, readout, readoutAxis, columns, selected, features, realizedNoise, realizedCombination)

  override def equals(other: Any): Boolean = other match
    case that: ResponseSourceBinding => fields == that.fields
    case _ => false
  override def hashCode: Int = fields.hashCode
  override def toString: String = s"ResponseSourceBinding${fields.productIterator.mkString("(", ", ", ")")}"

object ResponseSourceBinding:
  private[scalafim] def apply(
      unit: UnitRevisionId,
      observation: ObservationId,
      design: ProviderDigest,
      preparation: ProviderDigest,
      noise: ProviderDigest,
      runCombination: ProviderDigest,
      readout: ProviderDigest,
      readoutAxis: ReadoutAxis,
      columns: Vector[ColumnId],
      selected: Vector[ColumnId],
      features: ProviderDigest,
      realizedNoise: RealizedNoise,
      realizedCombination: RealizedCombination
  ): ResponseSourceBinding = new ResponseSourceBinding(unit, observation, design, preparation, noise, runCombination, readout, readoutAxis, columns, selected, features, realizedNoise, realizedCombination)

/** The declared temporal law of the response. */
enum DeclaredTemporal:
  case White
  /** Exact AR coefficients, MA terms, initialization and run resets. */
  case FixedSigmaT(spec: ProviderDigest)

  def render: String = this match
    case White => "White"
    case FixedSigmaT(spec) => s"FixedSigmaT(${spec.render})"

enum DeclaredCombination:
  case SingleRun
  case FixedWeights(digest: ProviderDigest)

  def render: String = this match
    case SingleRun => "SingleRun"
    case FixedWeights(digest) => s"FixedWeights(${digest.render})"

enum SpatialClaim:
  /** vec(Y) has covariance Σ_S ⊗ Σ_T with Σ_S an arbitrary PSD matrix. */
  case KroneckerSeparable
  case NonSeparable(description: String)
  case Unspecified

enum DistributionClaim:
  case Gaussian
  case SecondMomentOnly

enum ModelOrigin:
  case Declared(source: String)
  /** Fitted covariance or fitted values restate a model; they never declare one. */
  case InferredFromFit(source: String)

final case class DeclaredResponseModel(
    temporal: DeclaredTemporal,
    combination: DeclaredCombination,
    spatial: SpatialClaim,
    distribution: DistributionClaim,
    origin: ModelOrigin
)

enum NullConstraint:
  case Unencoded(label: String)
  /** B = NΓ for a declared basis N. v1 never evaluates it. */
  case Linear(constraint: ProviderDigest)

/** `version` is raw so that an unknown version is representable. */
final case class ResponseActionRequest(
    version: String,
    expected: ResponseSourceBinding,
    model: Option[DeclaredResponseModel],
    action: ConditionAction,
    nullConstraint: NullConstraint
)

/** Immutable summary of the Fit status plane over the bound in-support
  * features, bound to the scanned unit revision, plane, feature identity and
  * selected columns. `samples` is the number of features read; the counts
  * always sum to it and it is positive, so no summary can claim coverage it
  * did not read. Codes are declarations: `Estimable` implies neither numerical
  * validity nor a response-law contract. Issued only by the ScalaFIM scanner;
  * not a case class, so there is no public `fromProduct`.
  */
final class StatusSummary private[scalafim] (
    val unit: UnitRevisionId,
    val plane: InferenceStatusScope.Fit,
    val features: ProviderDigest,
    val selected: Vector[ColumnId],
    val samples: Long,
    val counts: Map[InferenceStatusCode, Long],
    val conditioning: ScientificFact
):
  require(Invariants.unique(selected), "summary selection must be unique and nonempty")
  require(samples > 0L, "a summary covers at least one feature")
  require(counts.values.forall(_ >= 0L) && counts.values.sum == samples, "status counts must be nonnegative and sum to the scanned features")

  def count(code: InferenceStatusCode): Long = counts.getOrElse(code, 0L)

  /** Selected features whose code is neither `Estimable` nor `OutsideSupport`. */
  def nonEstimable: Long = counts.iterator.collect {
    case (code, n) if code != InferenceStatusCode.Estimable && code != InferenceStatusCode.OutsideSupport => n
  }.sum

  private def fields: Product = (unit, plane, features, selected, samples, counts, conditioning)

  private[scalafim] def copy(
      unit: UnitRevisionId = unit,
      plane: InferenceStatusScope.Fit = plane,
      features: ProviderDigest = features,
      selected: Vector[ColumnId] = selected,
      samples: Long = samples,
      counts: Map[InferenceStatusCode, Long] = counts,
      conditioning: ScientificFact = conditioning
  ): StatusSummary = new StatusSummary(unit, plane, features, selected, samples, counts, conditioning)

  override def equals(other: Any): Boolean = other match
    case that: StatusSummary => fields == that.fields
    case _ => false
  override def hashCode: Int = fields.hashCode
  override def toString: String = s"StatusSummary${fields.productIterator.mkString("(", ", ", ")")}"

object StatusSummary:
  private[scalafim] def apply(
      unit: UnitRevisionId,
      plane: InferenceStatusScope.Fit,
      features: ProviderDigest,
      selected: Vector[ColumnId],
      samples: Long,
      counts: Map[InferenceStatusCode, Long],
      conditioning: ScientificFact
  ): StatusSummary = new StatusSummary(unit, plane, features, selected, samples, counts, conditioning)

enum StatusAbsence:
  case UnitHasNoEvidence
  case NoCoefficientEvidence
  case ColumnsDisagree
  case SelectedNotInferable(cols: Vector[ColumnId])
  case FitPlaneAbsent
  /** The caller supplied no status evidence at all. */
  case NotScanned

enum StatusEvidence:
  case Absent(reason: StatusAbsence)
  case Scanned(summary: StatusSummary)

/** Binding fields in comparison order, then the status scope. */
enum SourceField:
  case Unit, Observation, Design, Preparation, Noise, RunCombination, Readout, ReadoutAxis, Columns, Selected,
    Features, RealizedNoise, RealizedCombination, StatusUnit, StatusFeatures, StatusPlane, StatusSelected,
    StatusOutsideSupport

enum ModelDefect:
  case TemporalMismatch(declared: DeclaredTemporal, realized: RealizedNoise)
  case CombinationMismatch(declared: DeclaredCombination, realized: RealizedCombination)

enum ActionDefect:
  case NotBijection
  case UnknownLevel(level: ConditionLevelId)
  case LevelsIncomplete(missing: Vector[ConditionLevelId])

/** Reasons in declared (first-failure) order. */
enum UnavailableReason:
  case InferenceStatus(reason: StatusAbsence)
  case NoDeclaredModel
  case ModelInferredFromFit
  case NoiseIncomplete
  case CombinationIncomplete
  case EstimatedWhitening
  case EstimatedRunWeights
  case LearnedResponseSubspace
  case ConditioningUnknown
  case NonEstimableFeatures(count: Long)
  case NullNotEncoded
  /** Policy (OD-5): the identity is invariant but calibrates nothing. */
  case IdentityActionPolicy
  case SpatialJointUnrepresented
  case NoPositiveContractInVersion(version: String)

/** The complete v1 codomain. There is deliberately no positive case (OD-2). */
enum ResponseActionRefusal:
  case UnsupportedVersion(found: String)
  case SourceMismatch(field: SourceField, expected: String, actual: String)
  case ModelMismatch(defect: ModelDefect)
  case ActionMismatch(defect: ActionDefect)
  case Unavailable(reason: UnavailableReason)

object ResponseActionEvidence:
  val Version: String = "scalafim.response-action/1"

  /** Pure and total. The first failure wins (OD-7): version, source (binding
    * fields in order, then the status scope), model, action, then unavailable
    * reasons in declared order, ending with `NoPositiveContractInVersion`.
    */
  def evaluate(
      req: ResponseActionRequest,
      actual: ResponseSourceBinding,
      status: Option[StatusEvidence]
  ): ResponseActionRefusal =
    if req.version != Version then ResponseActionRefusal.UnsupportedVersion(req.version)
    else
      sourceMismatch(req.expected, actual, status)
        .orElse(req.model.flatMap(modelMismatch(_, actual)).map(ResponseActionRefusal.ModelMismatch.apply))
        .orElse(actionMismatch(req.action, actual.readoutAxis).map(ResponseActionRefusal.ActionMismatch.apply))
        .getOrElse(ResponseActionRefusal.Unavailable(unavailable(req, actual, status)))

  private def sourceMismatch(
      e: ResponseSourceBinding,
      a: ResponseSourceBinding,
      status: Option[StatusEvidence]
  ): Option[ResponseActionRefusal] =
    def field[A](name: SourceField, expected: A, actual: A)(render: A => String): Option[ResponseActionRefusal] =
      Option.when(expected != actual)(ResponseActionRefusal.SourceMismatch(name, render(expected), render(actual)))
    def columns(ids: Vector[ColumnId]): String = ids.map(_.value).mkString("[", ",", "]")
    val binding: Vector[Option[ResponseActionRefusal]] = Vector(
      field(SourceField.Unit, e.unit, a.unit)(_.value),
      field(SourceField.Observation, e.observation, a.observation)(_.value),
      field(SourceField.Design, e.design, a.design)(_.render),
      field(SourceField.Preparation, e.preparation, a.preparation)(_.render),
      field(SourceField.Noise, e.noise, a.noise)(_.render),
      field(SourceField.RunCombination, e.runCombination, a.runCombination)(_.render),
      field(SourceField.Readout, e.readout, a.readout)(_.render),
      field(SourceField.ReadoutAxis, e.readoutAxis, a.readoutAxis)(_.render),
      field(SourceField.Columns, e.columns, a.columns)(columns),
      field(SourceField.Selected, e.selected, a.selected)(columns),
      field(SourceField.Features, e.features, a.features)(_.render),
      field(SourceField.RealizedNoise, e.realizedNoise, a.realizedNoise)(_.render),
      field(SourceField.RealizedCombination, e.realizedCombination, a.realizedCombination)(_.render)
    )
    def scope(summary: StatusSummary): Option[ResponseActionRefusal] =
      field(SourceField.StatusUnit, a.unit, summary.unit)(_.value)
        .orElse(field(SourceField.StatusFeatures, a.features, summary.features)(_.render))
        .orElse(field(SourceField.StatusPlane, a.observation, summary.plane.observation)(_.value))
        .orElse(field(SourceField.StatusSelected, a.selected, summary.selected)(columns))
        .orElse(field(SourceField.StatusOutsideSupport, 0L, summary.count(InferenceStatusCode.OutsideSupport))(_.toString))
    binding.flatten.headOption.orElse(status match
      case Some(StatusEvidence.Scanned(summary)) => scope(summary)
      case _ => None)

  /** A declared white law admits no temporal processing; a declared fixed Σ_T
    * admits only the fixed whitening with that exact specification. Estimated
    * or learned processing is not a contradiction of the declared law; it is
    * unavailable (OD-6). Unrecorded processing cannot be compared.
    */
  private def modelMismatch(model: DeclaredResponseModel, a: ResponseSourceBinding): Option[ModelDefect] =
    val temporal = (model.temporal, a.realizedNoise) match
      case (_, RealizedNoise.Unrecorded) => false
      case (DeclaredTemporal.White, realized) => realized != RealizedNoise.White
      case (DeclaredTemporal.FixedSigmaT(_), RealizedNoise.White) => true
      case (DeclaredTemporal.FixedSigmaT(spec), RealizedNoise.FixedAr(_, _, exact)) => exact.exists(_ != spec)
      case (DeclaredTemporal.FixedSigmaT(_), _) => false
    val combination = (model.combination, a.realizedCombination) match
      case (_, RealizedCombination.Unrecorded) => false
      case (DeclaredCombination.SingleRun, realized) => realized != RealizedCombination.SingleRun
      case (DeclaredCombination.FixedWeights(_), RealizedCombination.SingleRun) => true
      case (DeclaredCombination.FixedWeights(declared), RealizedCombination.FixedWeights(realized)) => declared != realized
      case (DeclaredCombination.FixedWeights(_), RealizedCombination.EstimatedWeights) => false
    if temporal then Some(ModelDefect.TemporalMismatch(model.temporal, a.realizedNoise))
    else Option.when(combination)(ModelDefect.CombinationMismatch(model.combination, a.realizedCombination))

  private def actionMismatch(action: ConditionAction, axis: ReadoutAxis): Option[ActionDefect] = action match
    case ConditionAction.Permute(map) =>
      val levels = axis.conditions
      val named = (map.keys.toVector.sortBy(_.value) ++ map.values.toVector.sortBy(_.value)).distinct
      named.find(level => !levels.contains(level)).map(ActionDefect.UnknownLevel.apply).orElse:
        val missing = levels.filterNot(map.contains)
        if missing.nonEmpty then Some(ActionDefect.LevelsIncomplete(missing))
        else Option.when(map.values.toSet.size != map.size)(ActionDefect.NotBijection)

  private def unavailable(
      req: ResponseActionRequest,
      a: ResponseSourceBinding,
      status: Option[StatusEvidence]
  ): UnavailableReason =
    import UnavailableReason.*
    val summary = status match
      case Some(StatusEvidence.Scanned(summary)) => Right(summary)
      case Some(StatusEvidence.Absent(reason)) => Left(InferenceStatus(reason))
      case None => Left(InferenceStatus(StatusAbsence.NotScanned))
    summary match
      case Left(reason) => reason
      case Right(summary) =>
        req.model match
          case None => NoDeclaredModel
          case Some(model) =>
            val reasons: Vector[Option[UnavailableReason]] = Vector(
              Option.when(model.origin.isInstanceOf[ModelOrigin.InferredFromFit])(ModelInferredFromFit),
              Option.when(noiseIncomplete(a.realizedNoise))(NoiseIncomplete),
              Option.when(a.realizedCombination == RealizedCombination.Unrecorded)(CombinationIncomplete),
              Option.when(dataDependentWhitening(a.realizedNoise))(EstimatedWhitening),
              Option.when(a.realizedCombination == RealizedCombination.EstimatedWeights)(EstimatedRunWeights),
              Option.when(a.realizedNoise == RealizedNoise.LearnedSubspace)(LearnedResponseSubspace),
              Option.when(summary.conditioning.isInstanceOf[ScientificFact.Unknown])(ConditioningUnknown),
              Option.when(summary.nonEstimable > 0L)(NonEstimableFeatures(summary.nonEstimable)),
              Option.when(req.nullConstraint.isInstanceOf[NullConstraint.Unencoded])(NullNotEncoded),
              Option.when(req.action.isIdentity)(IdentityActionPolicy),
              Option.when(model.spatial != SpatialClaim.KroneckerSeparable)(SpatialJointUnrepresented)
            )
            reasons.flatten.headOption.getOrElse(NoPositiveContractInVersion(req.version))

  private def noiseIncomplete(noise: RealizedNoise): Boolean = noise match
    case RealizedNoise.Unrecorded | RealizedNoise.FixedAr(_, _, None) => true
    case _ => false

  private def dataDependentWhitening(noise: RealizedNoise): Boolean = noise match
    case RealizedNoise.EstimatedAr(_, _) | RealizedNoise.Robust => true
    case _ => false
