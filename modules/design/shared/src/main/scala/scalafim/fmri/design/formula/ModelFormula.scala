package scalafim.fmri.design.formula

import scalafim.fmri.design.{ColumnId, TermId}

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

/** `basis` and `contrasts` stay `String`: both key user-extensible registries,
  * so an open set is the right model for them.
  */
final case class HrfCall(
    vars: Vector[ArgValue],
    basis: Option[String] = None,
    subset: Option[ArgValue] = None,
    onsets: Option[ArgValue] = None,
    durations: Option[ArgValue] = None,
    hrfFun: Option[ArgValue] = None,
    contrasts: Option[String] = None,
    id: Option[TermId] = None,
    prefix: Option[TermId] = None,
    lag: Option[Double] = None,
    nbasis: Option[Int] = None,
    summate: Option[Boolean] = None,
    normalize: Option[Boolean] = None
) extends TermCall

final case class TrialwiseCall(
    basis: Option[String] = None,
    durations: Option[ArgValue] = None,
    lag: Option[Double] = None,
    nbasis: Option[Int] = None,
    addSum: Option[Boolean] = None,
    label: Option[TermId] = None,
    normalize: Option[Boolean] = None
) extends TermCall

final case class CovariateCall(
    vars: Vector[ArgValue],
    data: Option[String] = None,
    id: Option[TermId] = None,
    prefix: Option[TermId] = None
) extends TermCall

final case class ModelFormula(onset: ColumnId, terms: Vector[TermCall])
