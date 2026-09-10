package scalafim.fmri.fit.profile

import scalafim.fmri.hrf.family.NormalizationRule

enum OutputError:
  case EmptyLabel
  case WeightLength(label: String, expected: Int, actual: Int)
  case NonFiniteWeight(label: String, index: Int, value: Double)
  case NonPositiveTolerance(label: String, value: Double)
  case DuplicateLabel(label: String)
  case TrialOutputsNeedTrialBackend(request: String)

  def message: String =
    this match
      case EmptyLabel => "a query needs a non-empty label"
      case WeightLength(label, expected, actual) => s"query '$label' has $actual weights for $expected conditions"
      case NonFiniteWeight(label, index, value) => s"query '$label' weight ${index + 1} is not finite: $value"
      case NonPositiveTolerance(label, value) => s"query '$label' needs a positive absolute tolerance, got $value"
      case DuplicateLabel(label) => s"query label '$label' is repeated"
      case TrialOutputsNeedTrialBackend(request) => s"$request requires the trial backend; the condition backend fits condition means only"

/** A signed linear functional over the condition axis with its own absolute
  * output tolerance: a near-zero contrast is not certified by a relative
  * amplitude error, so each query carries the scale at which its value is
  * meaningful.
  */
final case class SignedQuery private (label: String, weights: Vector[Double], absoluteTolerance: Double):
  def conditions: Int = weights.length

object SignedQuery:
  def make(label: String, weights: Vector[Double], absoluteTolerance: Double): Either[OutputError, SignedQuery] =
    if label.trim.isEmpty then Left(OutputError.EmptyLabel)
    else
      weights.indexWhere(w => !w.isFinite) match
        case i if i >= 0 => Left(OutputError.NonFiniteWeight(label, i, weights(i)))
        case _ =>
          if !(absoluteTolerance > 0.0 && absoluteTolerance.isFinite) then Left(OutputError.NonPositiveTolerance(label, absoluteTolerance))
          else Right(new SignedQuery(label, weights, absoluteTolerance))

/** What a fit should emit. Condition outputs are native to the condition
  * backend; trial outputs are a different contract and are refused there
  * with a typed error rather than expanded from condition means.
  */
enum OutputRequest:
  case ConditionAmplitudes(normalization: NormalizationRule)
  case ConditionQueries(queries: Vector[SignedQuery], normalization: NormalizationRule)
  case TrialAmplitudes(normalization: NormalizationRule)
  case TrialQueries(queries: Vector[SignedQuery], normalization: NormalizationRule)

  def rule: NormalizationRule =
    this match
      case ConditionAmplitudes(n) => n
      case ConditionQueries(_, n) => n
      case TrialAmplitudes(n) => n
      case TrialQueries(_, n) => n

  def isConditionNative: Boolean =
    this match
      case ConditionAmplitudes(_) | ConditionQueries(_, _) => true
      case TrialAmplitudes(_) | TrialQueries(_, _) => false

  /** Validate against a condition axis of `conditions` entries. */
  def validateFor(conditions: Int): Either[OutputError, OutputRequest] =
    this match
      case ConditionAmplitudes(_) => Right(this)
      case ConditionQueries(queries, _) =>
        val labels = queries.map(_.label)
        labels.diff(labels.distinct).headOption match
          case Some(dup) => Left(OutputError.DuplicateLabel(dup))
          case None =>
            queries.find(_.conditions != conditions) match
              case Some(q) => Left(OutputError.WeightLength(q.label, conditions, q.conditions))
              case None => Right(this)
      case TrialAmplitudes(_) => Left(OutputError.TrialOutputsNeedTrialBackend("TrialAmplitudes"))
      case TrialQueries(_, _) => Left(OutputError.TrialOutputsNeedTrialBackend("TrialQueries"))

/** One evaluated query with its float32 conversion audited against the
  * query's own tolerance. `value` is always the float64 result.
  */
final case class QueryValue(label: String, value: Double, float32: Float, conversionError: Double, withinTolerance: Boolean)

object QueryEvaluation:

  def evaluate(amplitudes: Vector[Double], queries: Vector[SignedQuery]): Vector[QueryValue] =
    queries.map { q =>
      var acc = 0.0
      var i = 0
      while i < q.weights.length do
        acc += q.weights(i) * amplitudes(i)
        i += 1
      audit(q.label, acc, q.absoluteTolerance)
    }

  /** Convert one amplitude or query value to float32 and audit the rounding. */
  def audit(label: String, value: Double, absoluteTolerance: Double): QueryValue =
    val f = value.toFloat
    val error = if value.isFinite && f.isFinite then math.abs(value - f.toDouble) else Double.PositiveInfinity
    QueryValue(label, value, f, error, value.isFinite && f.isFinite && error <= absoluteTolerance)
