package scalafim.fmri.design.formula

import scalafim.fmri.design.{HrfColumnScaling, PortableNumber}
import scalafim.fmri.hrf.{EventResponseNormalization, TemporalDerivativeConvention}

enum FormulaTokenKind:
  case Identifier, Function, Argument, Literal, Punctuation, Whitespace

enum FormulaTokenStatus:
  /** Accepted by the formula parser; binding/build validation is a separate step. */
  case Accepted
  case Proposed(reason: String)

final case class FormulaToken(
    text: String,
    kind: FormulaTokenKind,
    termIndex: Option[Int],
    argPath: Vector[String],
    status: FormulaTokenStatus = FormulaTokenStatus.Accepted
)

/** Character offsets are zero-based, end-exclusive, in the original input. */
final case class FormulaSourceSpan(start: Int, end: Int):
  require(start >= 0 && end >= start)

final case class FormulaSourceNode(termIndex: Option[Int], argPath: Vector[String], span: FormulaSourceSpan)
final case class PositionedFormula(formula: ModelFormula, source: String, nodes: Vector[FormulaSourceNode])

/** Prints formulas and expressions in the admitted grammar.
  *
  * Output is byte-identical on the JVM and Scala.js: numbers are printed with
  * [[PortableNumber.format]] (`4`, `0.25`, `1e-7`, `1e21`; `-0` prints as `0`).
  * Every `...Either` printer re-parses its output and fails unless the result
  * equals the input, so printed text is always a lossless representation.
  */
object FormulaPrinter:
  /** Throwing convenience over [[expressionTextEither]].
    * @throws FormulaParser.ParseError when the expression is not printable losslessly.
    */
  def expressionText(value: ArgValue): String =
    expressionTextEither(value).fold(throw _, identity)

  def expressionTextEither(value: ArgValue): Either[FormulaParser.ParseError, String] =
    nonFinite(value) match
      case Some(number) => Left(FormulaParser.ParseError(s"Numeric literal $number is not finite and has no formula text", 0))
      case None =>
        val out = Vector.newBuilder[FormulaToken]
        def emit(text: String, kind: FormulaTokenKind, path: Vector[String]): Unit =
          out += FormulaToken(text, kind, None, path)
        renderValue(value, Vector.empty, emit)
        val text = out.result().map(_.text).mkString
        FormulaParser.parseExpression(text).flatMap { parsed =>
          if parsed == value then Right(text)
          else Left(FormulaParser.ParseError("Expression cannot be represented losslessly by the admitted grammar", 0))
        }

  def renderEither(formula: ModelFormula): Either[FormulaParser.ParseError, Vector[FormulaToken]] =
    formulaNonFinite(formula) match
      case Some(number) => Left(FormulaParser.ParseError(s"Numeric literal $number is not finite and has no formula text", 0))
      case None =>
        val tokens = unchecked(formula)
        FormulaParser.parseEither(tokens.map(_.text).mkString).flatMap: parsed =>
          if parsed == formula then Right(tokens)
          else Left(FormulaParser.ParseError("Formula cannot be represented losslessly by the admitted grammar", 0))

  /** Throwing convenience over [[renderEither]].
    * @throws FormulaParser.ParseError when the formula is not printable losslessly.
    */
  def render(formula: ModelFormula): Vector[FormulaToken] =
    renderEither(formula).fold(throw _, identity)

  private def nonFinite(value: ArgValue): Option[Double] = value match
    case ArgValue.Num(number) if !number.isFinite => Some(number)
    case ArgValue.Call(_, args) => args.iterator.flatMap(arg => nonFinite(arg.value)).nextOption()
    case _ => None

  private def formulaNonFinite(formula: ModelFormula): Option[Double] =
    def values(term: TermCall): Vector[ArgValue] = term match
      case h: HrfCall =>
        h.vars ++ h.subset ++ h.onsets ++ h.durations ++ h.hrfFun ++ h.lag.map(ArgValue.Num(_))
      case t: TrialwiseCall =>
        t.subset.toVector ++ t.onsets ++ t.durations ++ t.id ++ t.lag.map(ArgValue.Num(_))
      case c: CovariateCall => c.vars
    formula.terms.iterator.flatMap(values).flatMap(nonFinite).nextOption()

  private def quote(value: String, delimiter: Char): String =
    val escaped = value.flatMap:
      case c if c == delimiter || c == '\\' => "\\" + c
      case c => c.toString
    delimiter.toString + escaped + delimiter.toString

  private def identifier(value: String): String =
    if value.nonEmpty && (value.head.isLetter || value.head == '_') &&
        value.tail.forall(c => c.isLetterOrDigit || c == '_' || c == '.') &&
        !Set("true", "false").contains(value.toLowerCase) then value
    else quote(value, '`')

  private def renderValue(
      value: ArgValue,
      path: Vector[String],
      emit: (String, FormulaTokenKind, Vector[String]) => Unit
  ): Unit =
    import FormulaTokenKind.*
    def call(fun: String, args: Vector[Arg], callPath: Vector[String]): Unit =
      emit(identifier(fun), Function, callPath)
      emit("(", Punctuation, callPath)
      args.zipWithIndex.foreach: (arg, index) =>
        if index > 0 then emit(", ", Punctuation, callPath)
        val child = callPath :+ arg.name.getOrElse(index.toString)
        arg.name.foreach: name =>
          emit(identifier(name), Argument, child)
          emit(" = ", Punctuation, child)
        renderValue(arg.value, child, emit)
      emit(")", Punctuation, callPath)
    value match
      case ArgValue.Ident(id) => emit(identifier(id.value), Identifier, path)
      case ArgValue.Str(s) => emit(quote(s, '"'), Literal, path)
      case ArgValue.Num(n) => emit(PortableNumber.format(n), Literal, path)
      case ArgValue.Bool(b) => emit(if b then "TRUE" else "FALSE", Literal, path)
      case ArgValue.Call(op, args) if Set("+", "-", "*", "/", "|", "&", "==", "!=", "<", "<=", ">", ">=").contains(op) &&
          args.size == 2 && args.forall(_.name.isEmpty) =>
        emit("(", Punctuation, path)
        renderValue(args(0).value, path :+ "0", emit)
        emit(s" $op ", Punctuation, path)
        renderValue(args(1).value, path :+ "1", emit)
        emit(")", Punctuation, path)
      // `!` binds looser than comparison (as in R), so the whole negation is
      // parenthesized: `(!(a)) == b` would otherwise re-parse as `!(a == b)`.
      case ArgValue.Call("!", Vector(Arg(None, arg))) =>
        emit("(!", Punctuation, path)
        renderValue(arg, path :+ "0", emit)
        emit(")", Punctuation, path)
      // `-(3)` would re-parse as the literal -3, so a negated literal keeps its call form.
      case ArgValue.Call("-", Vector(Arg(None, arg))) if !arg.isInstanceOf[ArgValue.Num] =>
        emit("-(", Punctuation, path)
        renderValue(arg, path :+ "0", emit)
        emit(")", Punctuation, path)
      case ArgValue.Call(fun, args) => call(fun, args, path)

  private def unchecked(formula: ModelFormula): Vector[FormulaToken] =
    import FormulaTokenKind.*
    val out = Vector.newBuilder[FormulaToken]
    var term = Option.empty[Int]
    def emit(text: String, kind: FormulaTokenKind, path: Vector[String] = Vector.empty): Unit =
      out += FormulaToken(text, kind, term, path)
    def str(name: String, v: Option[String]): Vector[Arg] = v.toVector.map(x => Arg(Some(name), ArgValue.Str(x)))
    def num(name: String, v: Option[Double]): Vector[Arg] = v.toVector.map(x => Arg(Some(name), ArgValue.Num(x)))
    def bool(name: String, v: Option[Boolean]): Vector[Arg] = v.toVector.map(x => Arg(Some(name), ArgValue.Bool(x)))
    def raw(name: String, v: Option[ArgValue]): Vector[Arg] = v.toVector.map(x => Arg(Some(name), x))
    def scaling(v: Option[HrfColumnScaling]): Vector[Arg] = str("scaling", v.map:
      case HrfColumnScaling.AsConvolved => "as-convolved"
      case HrfColumnScaling.UnitMaximumAbsolute => "unit-maximum-absolute"
    )
    emit(identifier(formula.onset.value), Identifier, Vector("onset"))
    emit(" ~ ", Punctuation)
    formula.terms.zipWithIndex.foreach: (t, i) =>
      if i > 0 then emit(" + ", Punctuation)
      term = Some(i)
      t match
        case h: HrfCall =>
          renderValue(ArgValue.Call("hrf", h.vars.map(Arg(None, _)) ++ str("basis", h.basis) ++ raw("subset", h.subset) ++
            raw("onsets", h.onsets) ++ raw("durations", h.durations) ++
            str("phase", h.phase.map(_.id.value)) ++ str("parent", h.phase.map(_.parent.value)) ++
            raw("hrf_fun", h.hrfFun) ++ str("contrasts", h.contrasts) ++ str("id", h.id.map(_.value)) ++
            str("prefix", h.prefix.map(_.value)) ++ num("lag", h.lag) ++ num("nbasis", h.nbasis.map(_.toDouble)) ++
            bool("summate", h.summate) ++ scaling(h.scaling) ++ bool("normalize", h.normalize) ++
            num("span", h.span.map(_.seconds.value)) ++ str("kernel_normalization", h.kernelNormalization.map(_.label)) ++ bool("include_main", h.includeMain) ++
            bool("orthogonalize", h.orthogonalize) ++ bool("shared_slopes", h.sharedSlopes) ++
            bool("orthogonalize_basis", h.orthogonalizeBasis) ++
            str("temporal_derivative", h.temporalDerivative.map {
              case TemporalDerivativeConvention.AnalyticSpmg => "analytic"
              case TemporalDerivativeConvention.SpmOneSecondBackwardDifference => "spm-1s"
            }) ++ str("event_normalization", h.eventNormalization.map {
              case EventResponseNormalization.PreservePulseScale => "as-convolved"
              case EventResponseNormalization.UnitPeak(_) => "unit-peak"
            }) ++ num("event_peak_step", h.eventNormalization.collect { case EventResponseNormalization.UnitPeak(step) => step.value })), Vector.empty, emit)
        case t: TrialwiseCall =>
          renderValue(ArgValue.Call("trialwise", str("basis", t.basis) ++ raw("subset", t.subset) ++ raw("onsets", t.onsets) ++
            raw("durations", t.durations) ++ str("phase", t.phase.map(_.id.value)) ++
            str("parent", t.phase.map(_.parent.value)) ++ raw("id", t.id) ++ num("lag", t.lag) ++
            num("nbasis", t.nbasis.map(_.toDouble)) ++ bool("add_sum", t.addSum) ++ str("label", t.label.map(_.value)) ++
            scaling(t.scaling) ++ bool("normalize", t.normalize)), Vector.empty, emit)
        case c: CovariateCall =>
          renderValue(ArgValue.Call("covariate", c.vars.map(Arg(None, _)) ++ str("data", c.data) ++ str("id", c.id.map(_.value)) ++
            str("prefix", c.prefix.map(_.value))), Vector.empty, emit)
    out.result()
