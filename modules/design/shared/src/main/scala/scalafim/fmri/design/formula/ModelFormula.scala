package scalafim.fmri.design.formula

import scalafim.fmri.design.{ColumnId, HrfColumnScaling, PhaseId, TermId}

import scalafim.fmri.hrf.{EventResponseNormalization, HrfNormalization, PositiveSeconds, TemporalDerivativeConvention}

sealed trait ArgValue

object ArgValue:
  /** A bare identifier.
    *
    * Wherever the grammar admits one it denotes a column of the event table, so
    * it carries the id of that column rather than its name: the build path can
    * hand it straight to `DataTable.get` and get `MissingColumn` /
    * `InvalidColumnType` back instead of a stringly catch-all.
    *
    * The one position that reads it otherwise is `hrf_fun=`, where an
    * identifier is looked up in the generator registry before being tried as a
    * column of HRFs.
    */
  final case class Ident(value: ColumnId) extends ArgValue
  final case class Str(value: String) extends ArgValue
  final case class Num(value: Double) extends ArgValue
  final case class Bool(value: Boolean) extends ArgValue
  final case class Call(fun: String, args: Vector[Arg]) extends ArgValue

final case class Arg(name: Option[String], value: ArgValue)

sealed trait TermCall

/** A phase term's stable identity and the source column that identifies its
  * parent trials.
  *
  * Keeping these values together prevents a formula from representing a phase
  * with no parent axis, or a parent axis whose phase identity is absent.
  */
final case class PhaseRef(id: PhaseId, parent: ColumnId)

/** `basis` and `contrasts` stay `String`: both key user-extensible registries,
  * so an open set is the right model for them.
  */
final case class HrfCall(
    vars: Vector[ArgValue],
    basis: Option[String] = None,
    subset: Option[ArgValue] = None,
    onsets: Option[ArgValue] = None,
    durations: Option[ArgValue] = None,
    phase: Option[PhaseRef] = None,
    hrfFun: Option[ArgValue] = None,
    contrasts: Option[String] = None,
    id: Option[TermId] = None,
    prefix: Option[TermId] = None,
    lag: Option[Double] = None,
    nbasis: Option[Int] = None,
    summate: Option[Boolean] = None,
    scaling: Option[HrfColumnScaling] = None,
    /** Compatibility spelling for scan-space unit-maximum scaling. */
    normalize: Option[Boolean] = None,
    /** Finite support, explicitly distinct from scan-space column scaling. */
    span: Option[PositiveSeconds] = None,
    kernelNormalization: Option[HrfNormalization] = None,
    includeMain: Option[Boolean] = None,
    /** Opt-in serial residualization of ordered modulator siblings within run. */
    orthogonalize: Option[Boolean] = None,
    /** Keep modulator slopes shared while categorical main effects remain split. */
    sharedSlopes: Option[Boolean] = None,
    /** Serially project derivative basis columns off earlier basis columns after convolution. */
    orthogonalizeBasis: Option[Boolean] = None,
    temporalDerivative: Option[TemporalDerivativeConvention] = None,
    eventNormalization: Option[EventResponseNormalization] = None
) extends TermCall

final case class TrialwiseCall(
    basis: Option[String] = None,
    subset: Option[ArgValue] = None,
    onsets: Option[ArgValue] = None,
    durations: Option[ArgValue] = None,
    phase: Option[PhaseRef] = None,
    /** Per-run trial identity column. */
    id: Option[ArgValue] = None,
    lag: Option[Double] = None,
    nbasis: Option[Int] = None,
    addSum: Option[Boolean] = None,
    label: Option[TermId] = None,
    scaling: Option[HrfColumnScaling] = None,
    /** Compatibility spelling for scan-space unit-maximum scaling. */
    normalize: Option[Boolean] = None
) extends TermCall

final case class CovariateCall(
    vars: Vector[ArgValue],
    data: Option[String] = None,
    id: Option[TermId] = None,
    prefix: Option[TermId] = None
) extends TermCall

final case class ModelFormula(onset: ColumnId, terms: Vector[TermCall]):
  def render: Vector[FormulaToken] = FormulaPrinter.render(this)
  def renderEither: Either[FormulaParser.ParseError, Vector[FormulaToken]] = FormulaPrinter.renderEither(this)
  def text: String = render.map(_.text).mkString

object ModelFormula:
  def render(formula: ModelFormula): Vector[FormulaToken] = formula.render
