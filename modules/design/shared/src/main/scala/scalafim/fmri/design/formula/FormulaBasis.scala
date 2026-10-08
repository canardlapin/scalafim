package scalafim.fmri.design.formula

import scalafim.fmri.hrf.{BasisCount, Cascade34Params, Hrf, HrfKind, Hrfs, PositiveSeconds, Seconds}
import scalafim.fmri.hrf.HrfFunctions.LwuNormalize

/** A literal value of a named HRF kind parameter in formula text. */
enum BasisParamValue:
  case Number(value: Double)
  case Flag(value: Boolean)
  case Word(value: String)
  /** Written `c(v1, v2, ...)`. */
  case Numbers(values: Vector[Double])

  def label: String =
    this match
      case Number(v)   => if v.isFinite then scalafim.fmri.design.PortableNumber.format(v) else v.toString
      case Flag(v)     => if v then "TRUE" else "FALSE"
      case Word(v)     => v
      case Numbers(vs) => vs.map(v => Number(v).label).mkString("c(", ", ", ")")

/** One `name = value` parameter of `basis = kind(name = value, ...)`. */
final case class BasisParam(name: String, value: BasisParamValue)

enum BasisParamType(val label: String):
  case Number extends BasisParamType("a number")
  case NonNegativeInteger extends BasisParamType("a non-negative integer")
  case Flag extends BasisParamType("TRUE or FALSE")
  case Choice(values: Vector[String]) extends BasisParamType(values.mkString("one of ", ", ", ""))
  case Numbers extends BasisParamType("a numeric vector c(...)")

/** A named parameter of one HRF kind. `default` is the constructor default;
  * `None` marks a parameter without one, which `required` then says must be supplied.
  */
final case class BasisParamSpec(name: String, tpe: BasisParamType, default: Option[BasisParamValue], required: Boolean = false)

enum FormulaBasisError:
  case UnknownKind(name: String)
  case ParamsWithoutBasis
  case UnknownParam(kind: HrfKind, name: String, allowed: Vector[String])
  case DuplicateParam(kind: HrfKind, name: String)
  case WrongType(kind: HrfKind, name: String, expected: BasisParamType, found: String)
  case MissingParam(kind: HrfKind, name: String)
  case MissingOneOf(kind: HrfKind, first: String, second: String)
  case ConflictingParams(kind: HrfKind, first: String, second: String)
  case InvalidValue(kind: HrfKind, name: String, detail: String)
  case Construction(kind: HrfKind, detail: String)

  /** The parameter the error is about, used to position parse errors. */
  def param: Option[String] =
    this match
      case UnknownParam(_, name, _)         => Some(name)
      case DuplicateParam(_, name)          => Some(name)
      case WrongType(_, name, _, _)         => Some(name)
      case ConflictingParams(_, _, second)  => Some(second)
      case InvalidValue(_, name, _)         => Some(name)
      case UnknownKind(_) | ParamsWithoutBasis | MissingParam(_, _) | MissingOneOf(_, _, _) | Construction(_, _) => None

  def message: String =
    this match
      case UnknownKind(name) =>
        s"Unknown HRF kind '$name' (available: ${HrfKind.availableNames.mkString(", ")})"
      case ParamsWithoutBasis =>
        "HRF kind parameters require an explicit basis"
      case UnknownParam(kind, name, allowed) =>
        val known = if allowed.isEmpty then "it takes no parameters" else s"known: ${allowed.mkString(", ")}"
        val hint =
          if Set("nbasis", "span").contains(name) then s"; give '$name' on the term, e.g. hrf(x, basis = ${kind.canonicalName}, $name = ...)"
          else ""
        s"basis ${kind.canonicalName}(...) got unknown parameter '$name' ($known)$hint"
      case DuplicateParam(kind, name) =>
        s"basis ${kind.canonicalName}(...) got parameter '$name' more than once"
      case WrongType(kind, name, expected, found) =>
        s"basis ${kind.canonicalName}(...) parameter '$name' must be ${expected.label}, found $found"
      case MissingParam(kind, name) =>
        s"basis ${kind.canonicalName}(...) requires parameter '$name'"
      case MissingOneOf(kind, first, second) =>
        s"basis ${kind.canonicalName}(...) requires '$first' or '$second'"
      case ConflictingParams(kind, first, second) =>
        s"basis ${kind.canonicalName}(...) accepts either '$first' or '$second', not both"
      case InvalidValue(kind, name, detail) =>
        s"basis ${kind.canonicalName}(...) parameter '$name' $detail"
      case Construction(kind, detail) =>
        s"basis ${kind.canonicalName} cannot be built: $detail"

/** Formula text for every admitted [[HrfKind]]: `basis = kind` with constructor
  * defaults, or `basis = kind(name = value, ...)` with named parameters whose
  * names and defaults are those of the matching `Hrfs` constructor.
  *
  * Basis count and support stay on the term (`nbasis =`, `span =`), as they
  * were before kind parameters existed, so one spelling covers every kind.
  * Aliases (`spmg`, `gam`, `bs`, ...) are accepted on input; the parser stores
  * and the printer emits the canonical name.
  */
object FormulaBasis:
  import BasisParamType as T
  import BasisParamValue as V

  private def num(name: String, default: Double): BasisParamSpec =
    BasisParamSpec(name, T.Number, Some(V.Number(default)))

  private val lwuNormalizations = Vector("none", "height", "area")
  private val weightedMethods = Vector("constant", "linear")

  /** The named parameters `basis = kind(...)` admits, in printing order. */
  def params(kind: HrfKind): Vector[BasisParamSpec] =
    kind match
      case HrfKind.Spmg1    => Vector(num("P1", 5.0), num("P2", 15.0), num("A1", 1.0 / 120.0))
      case HrfKind.Spmg2 | HrfKind.Spmg3 => Vector.empty
      case HrfKind.Gamma    => Vector(num("shape", 6.0), num("rate", 1.0))
      case HrfKind.Gaussian => Vector(num("mean", 6.0), num("sd", 2.0))
      case HrfKind.Lwu =>
        Vector(num("tau", 6.0), num("sigma", 2.5), num("rho", 0.35),
          BasisParamSpec("normalize", T.Choice(lwuNormalizations), Some(V.Word("none"))))
      case HrfKind.Cascade34 =>
        val d = Cascade34Params.Default
        Vector(num("kappaP", d.kappaP), num("kappaU", d.kappaU), num("rho", d.rho))
      case HrfKind.Mexhat   => Vector(num("mean", 6.0), num("sd", 2.0))
      case HrfKind.InvLogit =>
        Vector(num("mu1", 6.0), num("s1", 1.0), num("mu2", 16.0), num("s2", 1.0), num("lag", 0.0))
      case HrfKind.HalfCosine =>
        Vector(num("h1", 1.0), num("h2", 5.0), num("h3", 7.0), num("h4", 7.0), num("f1", 0.0), num("f2", 0.0))
      case HrfKind.Fir | HrfKind.Tent | HrfKind.Fourier | HrfKind.Sine => Vector.empty
      case HrfKind.Bspline  => Vector(BasisParamSpec("degree", T.NonNegativeInteger, Some(V.Number(3.0))))
      case HrfKind.Daguerre => Vector(num("scale", 4.0))
      case HrfKind.Boxcar =>
        Vector(BasisParamSpec("width", T.Number, None, required = true), num("amplitude", 1.0),
          BasisParamSpec("normalize", T.Flag, Some(V.Flag(false))))
      case HrfKind.Weighted =>
        Vector(BasisParamSpec("weights", T.Numbers, None, required = true), BasisParamSpec("times", T.Numbers, None),
          BasisParamSpec("width", T.Number, None), BasisParamSpec("method", T.Choice(weightedMethods), Some(V.Word("constant"))),
          BasisParamSpec("normalize", T.Flag, Some(V.Flag(false))))

  /** Default `nbasis` for kinds whose column count the term sets. */
  def defaultBasisCount(kind: HrfKind): Option[Int] =
    kind match
      case HrfKind.Fir      => Some(12)
      case HrfKind.Bspline | HrfKind.Tent | HrfKind.Fourier | HrfKind.Sine => Some(5)
      case HrfKind.Daguerre => Some(3)
      case _                => None

  /** Default support for kinds whose constructor takes `span`; others fix their
    * support from their own parameters (`half_cosine`, `boxcar`, `weighted`) or
    * are the fixed informed sets (`spmg2`, `spmg3`), and reject a term `span`.
    */
  def defaultSpan(kind: HrfKind): Option[Seconds] =
    kind match
      case HrfKind.Cascade34 => Some(Seconds.unsafe(32.0))
      case HrfKind.Spmg2 | HrfKind.Spmg3 | HrfKind.HalfCosine | HrfKind.Boxcar | HrfKind.Weighted => None
      case _ => Some(Seconds.unsafe(24.0))

  def admitsSpan(kind: HrfKind): Boolean =
    defaultSpan(kind).nonEmpty

  def spanKinds: Vector[HrfKind] =
    HrfKind.all.filter(admitsSpan)

  def kind(name: String): Either[FormulaBasisError, HrfKind] =
    HrfKind.fromString(name).left.map(_ => FormulaBasisError.UnknownKind(name))

  /** Check names, types and presence; values a constructor would reject surface from [[build]]. */
  def admit(kind: HrfKind, supplied: Vector[BasisParam]): Either[FormulaBasisError, Unit] =
    val specs = params(kind)
    val byName = specs.map(s => s.name -> s).toMap
    val seen = scala.collection.mutable.HashSet.empty[String]
    val checked = supplied.iterator.map { p =>
      byName.get(p.name) match
        case None => Left(FormulaBasisError.UnknownParam(kind, p.name, specs.map(_.name)))
        case Some(_) if !seen.add(p.name) => Left(FormulaBasisError.DuplicateParam(kind, p.name))
        case Some(spec) => checkValue(kind, spec, p.value)
    }.collectFirst { case Left(error) => error }
    checked match
      case Some(error) => Left(error)
      case None =>
        specs.find(s => s.required && !seen.contains(s.name)) match
          case Some(spec) => Left(FormulaBasisError.MissingParam(kind, spec.name))
          case None =>
            if kind == HrfKind.Weighted && seen.contains("times") && seen.contains("width") then
              Left(FormulaBasisError.ConflictingParams(kind, "times", "width"))
            else if kind == HrfKind.Weighted && !seen.contains("times") && !seen.contains("width") then
              Left(FormulaBasisError.MissingOneOf(kind, "times", "width"))
            else Right(())

  private def checkValue(kind: HrfKind, spec: BasisParamSpec, value: BasisParamValue): Either[FormulaBasisError, Unit] =
    def wrong = Left(FormulaBasisError.WrongType(kind, spec.name, spec.tpe, value.label))
    def finite(v: Double) =
      if v.isFinite then Right(()) else Left(FormulaBasisError.InvalidValue(kind, spec.name, s"must be finite, got $v"))
    (spec.tpe, value) match
      case (T.Number, V.Number(v)) => finite(v)
      case (T.NonNegativeInteger, V.Number(v)) =>
        if v.isValidInt && v == v.toInt.toDouble && v >= 0.0 then Right(()) else wrong
      case (T.Flag, V.Flag(_)) => Right(())
      case (T.Choice(values), V.Word(v)) => if values.contains(v) then Right(()) else wrong
      case (T.Numbers, V.Numbers(vs)) =>
        vs.find(v => !v.isFinite).fold(Right(()))(v => finite(v))
      case _ => wrong

  /** The parameter value supplied, or the constructor default. */
  private final class Values(kind: HrfKind, supplied: Vector[BasisParam]):
    private val byName = supplied.map(p => p.name -> p.value).toMap
    private def value(name: String): Option[BasisParamValue] =
      byName.get(name).orElse(params(kind).find(_.name == name).flatMap(_.default))
    def number(name: String): Double =
      value(name) match
        case Some(V.Number(v)) => v
        case other             => throw IllegalStateException(s"admitted parameter '$name' is not numeric: $other")
    def numberOpt(name: String): Option[Double] =
      value(name).collect { case V.Number(v) => v }
    def numbers(name: String): Option[Vector[Double]] =
      value(name).collect { case V.Numbers(vs) => vs }
    def flag(name: String): Boolean =
      value(name).collect { case V.Flag(v) => v }.getOrElse(false)
    def word(name: String): String =
      value(name).collect { case V.Word(v) => v }.getOrElse("")

  /** Build the kernel for `basis = kind(params...)` with the term's `nbasis` and `span`.
    *
    * `nbasis` is ignored by single-column kinds, as it always has been.
    */
  def build(
      kind: HrfKind,
      supplied: Vector[BasisParam],
      nbasis: Option[Int] = None,
      span: Option[PositiveSeconds] = None
  ): Either[FormulaBasisError, Hrf] =
    for
      _ <- admit(kind, supplied)
      _ <- span match
        case Some(_) if !admitsSpan(kind) =>
          Left(FormulaBasisError.Construction(kind, s"span is fixed by its parameters; term 'span' applies to ${spanKinds.map(_.canonicalName).mkString(", ")}"))
        case _ => Right(())
      count <- defaultBasisCount(kind) match
        case None => Right(1)
        case Some(default) =>
          val n = nbasis.getOrElse(default)
          BasisCount.fromInt(n).left.map(error => FormulaBasisError.Construction(kind, error.message)).map(_.value)
      hrf <- construct(kind, Values(kind, supplied), count, span.map(_.seconds).orElse(defaultSpan(kind)).getOrElse(Seconds.unsafe(24.0)), plain = supplied.isEmpty && span.isEmpty)
    yield hrf

  private def construct(kind: HrfKind, p: Values, nbasis: Int, span: Seconds, plain: Boolean): Either[FormulaBasisError, Hrf] =
    def fail(detail: String) = Left(FormulaBasisError.Construction(kind, detail))
    kind match
      case HrfKind.Spmg1 =>
        Right(if plain then Hrfs.SPMG1 else Hrfs.spmg1(P1 = p.number("P1"), P2 = p.number("P2"), A1 = p.number("A1"), span = span))
      case HrfKind.Spmg2 => Right(Hrfs.SPMG2)
      case HrfKind.Spmg3 => Right(Hrfs.SPMG3)
      case HrfKind.Gamma =>
        Right(if plain then Hrfs.Gamma else Hrfs.gamma(shape = p.number("shape"), rate = p.number("rate"), span = span))
      case HrfKind.Gaussian =>
        Right(if plain then Hrfs.Gaussian else Hrfs.gaussian(mean = p.number("mean"), sd = p.number("sd"), span = span))
      case HrfKind.Lwu =>
        val normalize = p.word("normalize") match
          case "height" => LwuNormalize.Height
          case "area"   => LwuNormalize.Area
          case _        => LwuNormalize.None
        Hrfs.lwuValidated(tau = p.number("tau"), sigma = p.number("sigma"), rho = p.number("rho"), normalize = normalize, span = span)
          .left.map(error => FormulaBasisError.Construction(kind, error.message))
      case HrfKind.Cascade34 =>
        Cascade34Params.make(p.number("kappaP"), p.number("kappaU"), p.number("rho"))
          .left.map(error => FormulaBasisError.Construction(kind, error.message))
          .map(Hrfs.cascade34(_, span))
      case HrfKind.Mexhat => Right(Hrfs.mexhat(mean = p.number("mean"), sd = p.number("sd"), span = span))
      case HrfKind.InvLogit =>
        Right(Hrfs.invLogit(mu1 = p.number("mu1"), s1 = p.number("s1"), mu2 = p.number("mu2"), s2 = p.number("s2"),
          lag = Seconds.unsafe(p.number("lag")), span = span))
      case HrfKind.HalfCosine =>
        Right(Hrfs.halfCosine(h1 = Seconds.unsafe(p.number("h1")), h2 = Seconds.unsafe(p.number("h2")),
          h3 = Seconds.unsafe(p.number("h3")), h4 = Seconds.unsafe(p.number("h4")), f1 = p.number("f1"), f2 = p.number("f2")))
      case HrfKind.Fir     => Right(Hrfs.fir(nBasis = nbasis, span = span))
      case HrfKind.Bspline => Right(Hrfs.bspline(nBasis = nbasis, span = span, degree = p.number("degree").toInt))
      case HrfKind.Tent    => Right(Hrfs.tent(nBasis = nbasis, span = span))
      case HrfKind.Fourier => Right(Hrfs.fourier(nBasis = nbasis, span = span))
      case HrfKind.Sine    => Right(Hrfs.sine(nBasis = nbasis, span = span))
      case HrfKind.Daguerre =>
        Hrfs.daguerreValidated(nBasis = nbasis, scale = p.number("scale"), span = span)
          .left.map(error => FormulaBasisError.Construction(kind, error.message))
      case HrfKind.Boxcar =>
        val width = p.number("width")
        if !(width > 0.0) then Left(FormulaBasisError.InvalidValue(kind, "width", s"must be > 0, got $width"))
        else Right(Hrfs.boxcar(width = Seconds.unsafe(width), amplitude = p.number("amplitude"), normalize = p.flag("normalize")))
      case HrfKind.Weighted =>
        val method = if p.word("method") == "linear" then Hrfs.WeightedMethod.Linear else Hrfs.WeightedMethod.Constant
        p.numbers("weights") match
          case None => fail("weights are required")
          case Some(weights) =>
            Hrfs.weightedValidated(
              weights = weights,
              width = p.numberOpt("width").map(Seconds.unsafe),
              times = p.numbers("times").map(_.map(Seconds.unsafe)),
              method = method,
              normalize = p.flag("normalize")
            ).left.map(error => FormulaBasisError.Construction(kind, error.message))

  /** `basis =` value text: a bare canonical name, or a call with named parameters. */
  def argValue(kind: String, supplied: Vector[BasisParam]): ArgValue =
    val name = HrfKind.fromString(kind).map(_.canonicalName).getOrElse(kind)
    if supplied.isEmpty then ArgValue.Str(name)
    else
      ArgValue.Call(name, supplied.map { p =>
        val value = p.value match
          case V.Number(v)   => ArgValue.Num(v)
          case V.Flag(v)     => ArgValue.Bool(v)
          case V.Word(v)     => ArgValue.Str(v)
          case V.Numbers(vs) => ArgValue.Call("c", vs.map(v => Arg(None, ArgValue.Num(v))))
        Arg(Some(p.name), value)
      })
