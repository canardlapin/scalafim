package scalafim.fmri.design.formula

sealed trait ArgValue

object ArgValue:
  final case class Ident(value: String) extends ArgValue
  final case class Str(value: String) extends ArgValue
  final case class Num(value: Double) extends ArgValue
  final case class Bool(value: Boolean) extends ArgValue
  final case class Call(fun: String, args: Vector[Arg]) extends ArgValue

final case class Arg(name: Option[String], value: ArgValue)

sealed trait TermCall

final case class HrfCall(
    vars: Vector[ArgValue],
    basis: Option[String] = None,
    subset: Option[ArgValue] = None,
    onsets: Option[ArgValue] = None,
    durations: Option[ArgValue] = None,
    hrfFun: Option[ArgValue] = None,
    contrasts: Option[ArgValue] = None,
    id: Option[String] = None,
    prefix: Option[String] = None,
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
    label: Option[String] = None,
    normalize: Option[Boolean] = None
) extends TermCall

final case class CovariateCall(
    vars: Vector[ArgValue],
    data: Option[String] = None,
    id: Option[String] = None,
    prefix: Option[String] = None
) extends TermCall

final case class ModelFormula(onset: String, terms: Vector[TermCall])
