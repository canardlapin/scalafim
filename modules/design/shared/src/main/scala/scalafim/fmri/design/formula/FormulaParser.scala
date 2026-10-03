package scalafim.fmri.design.formula

import scalafim.fmri.design.{ColumnId, DesignError, HrfColumnScaling, PhaseId, TermId}

import scalafim.fmri.hrf.{EventResponseNormalization, HrfNormalization, PositiveSeconds, TemporalDerivativeConvention}

object FormulaParser:

  final case class ParseError(message: String, pos: Int) extends IllegalArgumentException(s"$message (at char $pos)")

  private enum Tok:
    case Ident(value: String)
    case Str(value: String)
    case Num(value: Double)
    case Bool(value: Boolean)
    case Plus
    case Minus
    case Times
    case Divide
    case Tilde
    case Comma
    case LParen
    case RParen
    case Eq
    case EqEq
    case NotEq
    case Lt
    case Lte
    case Gt
    case Gte
    case And
    case Or
    case Bang
    case EOF

  private final case class Token(tok: Tok, pos: Int)

  def parse(input: String): ModelFormula =
    val toks = tokenize(input)
    val p = new Parser(toks, input)
    val out = p.parseFormula()
    p.expect(Tok.EOF)
    out

  def parseEither(input: String): Either[ParseError, ModelFormula] =
    try Right(parse(input))
    catch
      case error: ParseError => Left(error)

  def parsePositioned(input: String): Either[ParseError, PositionedFormula] =
    try
      val p = new Parser(tokenize(input), input)
      val formula = p.parseFormula()
      p.expect(Tok.EOF)
      Right(PositionedFormula(formula, input, p.sourceNodes))
    catch
      case error: ParseError => Left(error)

  /** Standalone event expressions use the same AST as formula subsets. */
  def parseExpression(input: String): Either[ParseError, ArgValue] =
    try
      val parser = new Parser(tokenize(input), input)
      val expression = parser.expression()
      parser.expect(Tok.EOF)
      Right(expression)
    catch
      case error: ParseError => Left(error)

  private def tokenize(input: String): Vector[Token] =
    val out = Vector.newBuilder[Token]
    val n = input.length
    var i = 0

    def err(msg: String): Nothing = throw ParseError(msg, i)

    while i < n do
      val ch = input.charAt(i)
      if ch.isWhitespace then i += 1
      else
        ch match
          case '+' =>
            out += Token(Tok.Plus, i)
            i += 1
          case '-' | '−' =>
            out += Token(Tok.Minus, i)
            i += 1
          case '*' | '×' =>
            out += Token(Tok.Times, i)
            i += 1
          case '/' | '÷' =>
            out += Token(Tok.Divide, i)
            i += 1
          case '~' =>
            out += Token(Tok.Tilde, i)
            i += 1
          case '!' =>
            val start = i
            if i + 1 < n && input.charAt(i + 1) == '=' then
              out += Token(Tok.NotEq, start)
              i += 2
            else
              out += Token(Tok.Bang, start)
              i += 1
          case '&' =>
            val start = i
            // Support both & and &&.
            if i + 1 < n && input.charAt(i + 1) == '&' then i += 2 else i += 1
            out += Token(Tok.And, start)
          case '|' =>
            val start = i
            // Support both | and ||.
            if i + 1 < n && input.charAt(i + 1) == '|' then i += 2 else i += 1
            out += Token(Tok.Or, start)
          case '<' =>
            val start = i
            if i + 1 < n && input.charAt(i + 1) == '=' then
              out += Token(Tok.Lte, start)
              i += 2
            else
              out += Token(Tok.Lt, start)
              i += 1
          case '>' =>
            val start = i
            if i + 1 < n && input.charAt(i + 1) == '=' then
              out += Token(Tok.Gte, start)
              i += 2
            else
              out += Token(Tok.Gt, start)
              i += 1
          case ',' =>
            out += Token(Tok.Comma, i)
            i += 1
          case '(' =>
            out += Token(Tok.LParen, i)
            i += 1
          case ')' =>
            out += Token(Tok.RParen, i)
            i += 1
          case '=' =>
            val start = i
            if i + 1 < n && input.charAt(i + 1) == '=' then
              out += Token(Tok.EqEq, start)
              i += 2
            else
              out += Token(Tok.Eq, start)
              i += 1
          case '"' | '\'' | '`' =>
            val quote = ch
            val start = i
            i += 1
            val sb = new StringBuilder
            var closed = false
            while i < n && !closed do
              val c = input.charAt(i)
              if c == quote then
                closed = true
                i += 1
              else if c == '\\' then
                if i + 1 >= n then err("Unterminated escape in string literal")
                val nxt = input.charAt(i + 1)
                sb.append(nxt)
                i += 2
              else
                sb.append(c)
                i += 1
            if !closed then throw ParseError("Unterminated string literal", start)
            out += Token(if quote == '`' then Tok.Ident(sb.result()) else Tok.Str(sb.result()), start)
          case c if isIdentStart(c) =>
            val start = i
            i += 1
            while i < n && isIdentPart(input.charAt(i)) do i += 1
            val raw = input.substring(start, i)
            raw.toLowerCase match
              case "true"  => out += Token(Tok.Bool(true), start)
              case "false" => out += Token(Tok.Bool(false), start)
              case _       => out += Token(Tok.Ident(raw), start)
          case c if c.isDigit || (c == '.' && i + 1 < n && input.charAt(i + 1).isDigit) =>
            val start = i
            while i < n && input.charAt(i).isDigit do i += 1
            if i < n && input.charAt(i) == '.' then
              i += 1
              while i < n && input.charAt(i).isDigit do i += 1
            if i < n && (input.charAt(i) == 'e' || input.charAt(i) == 'E') then
              i += 1
              if i < n && (input.charAt(i) == '+' || input.charAt(i) == '-') then i += 1
              while i < n && input.charAt(i).isDigit do i += 1
            val raw = input.substring(start, i)
            val number =
              try raw.toDouble
              catch case _: NumberFormatException => throw ParseError(s"Invalid number literal '$raw'", start)
            if !number.isFinite then throw ParseError("Number literal is outside the finite range", start)
            out += Token(Tok.Num(number), start)
          case other =>
            err(s"Unexpected character '$other'")

    out += Token(Tok.EOF, n)
    out.result()

  /** Lift an identifier token into the column it names.
    *
    * [[isIdentStart]] and [[isIdentPart]] already establish everything
    * [[ColumnId]] requires — non-empty, no whitespace, no control characters —
    * so the token has been parsed by the time it gets here.
    */
  private def columnId(token: String, pos: Int): ColumnId =
    ColumnId(token).fold(error => throw ParseError(idDetail(error), pos), identity)

  private def idDetail(error: DesignError): String =
    error match
      case DesignError.InvalidId(_, value, reason) => s"'$value' $reason"
      case other                                   => other.message

  private def isIdentStart(c: Char): Boolean =
    c.isLetter || c == '_'

  private def isIdentPart(c: Char): Boolean =
    c.isLetterOrDigit || c == '_' || c == '.'

  private final class Parser(tokens: Vector[Token], input: String):
    private val nodes = Vector.newBuilder[FormulaSourceNode]
    private val recordedPaths = scala.collection.mutable.Set.empty[(Option[Int], Vector[String])]
    private val expressionSpans = new java.util.IdentityHashMap[ArgValue, FormulaSourceSpan]()
    private var termIndex = Option.empty[Int]
    private var argPath = Vector.empty[String]
    private var termArgumentPositions = Vector.empty[Int]
    def sourceNodes: Vector[FormulaSourceNode] = nodes.result()
    private def span(start: Int): FormulaSourceSpan =
      var end = cur.pos
      while end > start && input.charAt(end - 1).isWhitespace do end -= 1
      FormulaSourceSpan(start, end)
    private def recordSpan(value: FormulaSourceSpan, path: Vector[String]): Unit =
      if recordedPaths.add(termIndex -> path) then
        nodes += FormulaSourceNode(termIndex, path, value)
    private def record(start: Int, path: Vector[String]): Unit = recordSpan(span(start), path)
    private def located(value: ArgValue, source: FormulaSourceSpan): ArgValue =
      val _ = expressionSpans.put(value, source)
      value
    private def recordChildren(value: ArgValue, path: Vector[String]): Unit = value match
      case ArgValue.Call(_, args) =>
        args.zipWithIndex.foreach: (arg, index) =>
          val child = path :+ arg.name.getOrElse(index.toString)
          recordSpan(expressionSpans.get(arg.value), child)
          recordChildren(arg.value, child)
      case _ => ()
    private var ix: Int = 0

    private def cur: Token = tokens(ix)
    private def tok: Tok = cur.tok

    def expect(t: Tok): Unit =
      if tok != t then throw ParseError(s"Expected $t but found $tok", cur.pos)
      ix += 1

    private def eat(t: Tok): Boolean =
      if tok == t then
        ix += 1
        true
      else false

    private def ident(): String =
      tok match
        case Tok.Ident(v) =>
          ix += 1
          v
        case _ => throw ParseError(s"Expected identifier but found $tok", cur.pos)

    def expression(): ArgValue = value()

    private def value(): ArgValue =
      parseOr()

    private def bin(op: String, left: ArgValue, right: ArgValue): ArgValue =
      located(ArgValue.Call(op, Vector(Arg(None, left), Arg(None, right))),
        FormulaSourceSpan(expressionSpans.get(left).start, expressionSpans.get(right).end))

    private def unary(op: String, expr: ArgValue, start: Int): ArgValue =
      located(ArgValue.Call(op, Vector(Arg(None, expr))), span(start))

    private def parseOr(): ArgValue =
      var left = parseAnd()
      while tok == Tok.Or do
        expect(Tok.Or)
        val right = parseAnd()
        left = bin("|", left, right)
      left

    private def parseAnd(): ArgValue =
      var left = parseNot()
      while tok == Tok.And do
        expect(Tok.And)
        val right = parseNot()
        left = bin("&", left, right)
      left

    /** R precedence (`?Syntax`): `!` binds looser than comparison and
      * arithmetic but tighter than `&`/`&&` and `|`/`||`, so `!a == b` is
      * `!(a == b)` and `!a & b` is `(!a) & b`.
      */
    private def parseNot(): ArgValue =
      if tok == Tok.Bang then
        val start = cur.pos
        expect(Tok.Bang)
        unary("!", parseNot(), start)
      else parseCmp()

    private def parseCmp(): ArgValue =
      val left = parseSum()
      tok match
        case Tok.EqEq =>
          expect(Tok.EqEq)
          bin("==", left, parseSum())
        case Tok.NotEq =>
          expect(Tok.NotEq)
          bin("!=", left, parseSum())
        case Tok.Lt =>
          expect(Tok.Lt)
          bin("<", left, parseSum())
        case Tok.Lte =>
          expect(Tok.Lte)
          bin("<=", left, parseSum())
        case Tok.Gt =>
          expect(Tok.Gt)
          bin(">", left, parseSum())
        case Tok.Gte =>
          expect(Tok.Gte)
          bin(">=", left, parseSum())
        case _ => left

    private def parseSum(): ArgValue =
      var left = parseProduct()
      while tok == Tok.Plus || tok == Tok.Minus do
        val operator = if tok == Tok.Plus then "+" else "-"
        ix += 1
        left = bin(operator, left, parseProduct())
      left

    private def parseProduct(): ArgValue =
      var left = parseUnary()
      while tok == Tok.Times || tok == Tok.Divide do
        val operator = if tok == Tok.Times then "*" else "/"
        ix += 1
        left = bin(operator, left, parseUnary())
      left

    private def parseUnary(): ArgValue =
      tok match
        case Tok.Minus =>
          val start = cur.pos
          expect(Tok.Minus)
          parseUnary() match
            case ArgValue.Num(number) => located(ArgValue.Num(-number), span(start))
            case expression => unary("-", expression, start)
        case Tok.Bang =>
          // An operand position such as `1 + !x == y`: as in R, the negation
          // extends over the following comparison, `1 + !(x == y)`.
          parseNot()
        case _ => parsePrimary()

    private def parsePrimary(): ArgValue =
      val start = cur.pos
      val result =
        tok match
          case Tok.Ident(v) =>
            // Identifier or nested call
            if tokens.lift(ix + 1).exists(_.tok == Tok.LParen) then
              ix += 1
              expect(Tok.LParen)
              val as = args()
              expect(Tok.RParen)
              ArgValue.Call(v, as)
            else
              ix += 1
              ArgValue.Ident(columnId(v, tokens(ix - 1).pos))
          case Tok.Str(v) =>
            ix += 1
            ArgValue.Str(v)
          case Tok.Num(v) =>
            ix += 1
            ArgValue.Num(v)
          case Tok.Bool(v) =>
            ix += 1
            ArgValue.Bool(v)
          case Tok.LParen =>
            expect(Tok.LParen)
            val e = value()
            expect(Tok.RParen)
            e
          case _ =>
            throw ParseError(s"Expected a value but found $tok", cur.pos)
      located(result, span(start))

    private def arg(index: Int): Arg =
      val start = cur.pos
      val parent = argPath
      val name = tok match
        case Tok.Ident(nm) if tokens.lift(ix + 1).exists(_.tok == Tok.Eq) =>
          ix += 1
          expect(Tok.Eq)
          Some(nm)
        case _ => None
      argPath = parent :+ name.getOrElse(index.toString)
      val result = Arg(name, value())
      record(start, argPath)
      recordChildren(result.value, argPath)
      argPath = parent
      result

    private def args(topLevel: Boolean = false): Vector[Arg] =
      if tok == Tok.RParen then Vector.empty
      else
        val out = Vector.newBuilder[Arg]
        val positions = Vector.newBuilder[Int]
        positions += cur.pos
        out += arg(0)
        var index = 1
        while eat(Tok.Comma) do
          positions += cur.pos
          out += arg(index)
          index += 1
        if topLevel then termArgumentPositions = positions.result()
        out.result()

    def parseFormula(): ModelFormula =
      val start = cur.pos
      val onset = columnId(ident(), start)
      record(start, Vector("onset"))
      expect(Tok.Tilde)
      val terms = Vector.newBuilder[TermCall]
      termIndex = Some(0)
      terms += parseTerm()
      while eat(Tok.Plus) do
        termIndex = termIndex.map(_ + 1)
        terms += parseTerm()
      val ts = terms.result()
      if ts.isEmpty then throw ParseError("Formula RHS must have at least one term", cur.pos)
      ModelFormula(onset, ts)

    private def parseTerm(): TermCall =
      val start = cur.pos
      val fun = ident()
      expect(Tok.LParen)
      termArgumentPositions = Vector.empty
      val as = args(topLevel = true)
      expect(Tok.RParen)
      record(start, Vector.empty)
      fun match
        case "hrf"       => buildHrfCall(as)
        case "trialwise" => buildTrialwiseCall(as)
        case "covariate" => buildCovariateCall(as)
        case other       => throw ParseError(s"Unknown term function '$other'", start)

    private final case class TermArgs(fun: String, args: Vector[Arg]):
      private val positions = termArgumentPositions
      def position(name: String): Int =
        val index = args.indexWhere(_.name.contains(name))
        positions.lift(index).getOrElse(cur.pos)
      val positional: Vector[ArgValue] =
        args.iterator.collect { case Arg(None, v) => v }.toVector

      /** Positional values with the source offset of each argument. */
      val positionalAt: Vector[(ArgValue, Int)] =
        args.zipWithIndex.collect { case (Arg(None, v), index) => v -> positions.lift(index).getOrElse(cur.pos) }

      val named: Map[String, ArgValue] =
        val out = scala.collection.mutable.HashMap.empty[String, ArgValue]
        args.zipWithIndex.foreach { (a, index) =>
          a.name.foreach { n =>
            if out.contains(n) then throw ParseError(s"Duplicate argument '$n'", positions(index))
            out.update(n, a.value)
          }
        }
        out.toMap

      def rejectUnknown(allowed: Set[String]): Unit =
        val unknown = named.keySet.diff(allowed)
        if unknown.nonEmpty then
          throw ParseError(s"$fun(...) got unknown named args: ${unknown.toVector.sorted.mkString(", ")}", position(args.flatMap(_.name).find(unknown).get))

      def requireNoPositional(): Unit =
        if positional.nonEmpty then throw ParseError(s"$fun(...) does not take positional arguments", positions(args.indexWhere(_.name.isEmpty)))

      def raw(name: String): Option[ArgValue] =
        named.get(name)

      def stringOrIdent(name: String): Option[String] =
        named.get(name).map {
          case ArgValue.Str(v)   => v
          case ArgValue.Ident(v) => v.value
          case other             => throw ParseError(s"'$name' must be a string/identifier, found $other", position(name))
        }

      /** A term label, parsed here so the AST never carries an unvalidated one. */
      def termId(name: String): Option[TermId] =
        stringOrIdent(name).map { v =>
          TermId(v).fold(error => throw ParseError(idDetail(error), position(name)), identity)
        }

      def phaseId(name: String): Option[PhaseId] =
        stringOrIdent(name).map { v =>
          PhaseId(v).fold(error => throw ParseError(idDetail(error), position(name)), identity)
        }

      def columnId(name: String): Option[ColumnId] =
        named.get(name).map {
          case ArgValue.Ident(id) => id
          case ArgValue.Str(value) =>
            ColumnId(value).fold(error => throw ParseError(idDetail(error), position(name)), identity)
          case other => throw ParseError(s"'$name' must be a string/identifier, found $other", position(name))
        }

      def numeric(name: String): Option[Double] =
        named.get(name).map {
          case ArgValue.Num(v) => v
          case other           => throw ParseError(s"'$name' must be numeric, found $other", position(name))
        }

      def integer(name: String): Option[Int] =
        named.get(name).map {
          case ArgValue.Num(v) if v.isValidInt && v == v.toInt.toDouble => v.toInt
          case ArgValue.Num(v)                                          => throw ParseError(s"'$name' must be an integer, found $v", position(name))
          case other                                                    => throw ParseError(s"'$name' must be an integer, found $other", position(name))
        }

      def boolean(name: String): Option[Boolean] =
        named.get(name).map {
          case ArgValue.Bool(v) => v
          case ArgValue.Ident(v) =>
            v.value.toLowerCase match
              case "true"  => true
              case "false" => false
              case _       => throw ParseError(s"'$name' must be boolean, found ${v.value}", position(name))
          case other => throw ParseError(s"'$name' must be boolean, found $other", position(name))
        }

      def vectorRef(name: String): Option[ArgValue] =
        named.get(name).map {
          case v @ ArgValue.Ident(_) => v
          case v @ ArgValue.Str(_)   => v
          case v @ ArgValue.Num(_)   => v
          case other                 => throw ParseError(s"'$name' must be a string/identifier or numeric scalar, found $other", position(name))
        }

      def stringOrIdentRef(name: String): Option[ArgValue] =
        named.get(name).map {
          case v @ ArgValue.Str(_)   => v
          case v @ ArgValue.Ident(_) => v
          case other                 => throw ParseError(s"'$name' must be a string/identifier, found $other", position(name))
        }

    private def buildHrfCall(args: Vector[Arg]): HrfCall =
      val schema = TermArgs("hrf", args)
      val pos = schema.positional
      if pos.isEmpty then throw ParseError("hrf(...) requires at least one variable", cur.pos)
      schema.positionalAt.foreach {
        case (ArgValue.Ident(_) | ArgValue.Call(_, _) | ArgValue.Num(1.0), _) => ()
        case (other, at) =>
          throw ParseError(s"hrf(...) positional args must be identifiers or calls, found $other", at)
      }

      val basis = schema.stringOrIdent("basis")
      val subset = schema.raw("subset")
      val onsets = schema.vectorRef("onsets")
      val durations = schema.vectorRef("durations")
      val phaseId = schema.phaseId("phase")
      val parent = schema.columnId("parent")
      val phase =
        (phaseId, parent) match
          case (Some(id), Some(parentColumn)) => Some(PhaseRef(id, parentColumn))
          case (None, None)                   => None
          case (Some(_), None) =>
            throw ParseError("hrf(...) 'phase' requires a 'parent' trial-id column", schema.position("phase"))
          case (None, Some(_)) =>
            throw ParseError("hrf(...) 'parent' requires a 'phase' identity", schema.position("parent"))
      val hrfFun = schema.stringOrIdentRef("hrf_fun")
      val contrasts = schema.stringOrIdent("contrasts")
      if schema.named.contains("id") && schema.named.contains("name") then
        throw ParseError("hrf(...) accepts either 'id' or its alias 'name', not both", schema.position("name"))
      val id = schema.termId("id").orElse(schema.termId("name"))
      val prefix = schema.termId("prefix")
      val lag = schema.numeric("lag")
      val nbasis = schema.integer("nbasis")
      val summate = schema.boolean("summate")
      val scaling = schema.stringOrIdent("scaling").map { value =>
        HrfColumnScaling.parse(value).fold(error => throw ParseError(error.message, schema.position("scaling")), identity)
      }
      val normalize = schema.boolean("normalize")
      if scaling.nonEmpty && normalize.nonEmpty then
        throw ParseError("hrf(...) accepts either 'scaling' or compatibility 'normalize', not both", schema.position("normalize"))

      val span = schema.numeric("span").map { value =>
        PositiveSeconds(value, "span").fold(error => throw ParseError(error.message, schema.position("span")), identity)
      }
      val kernelNormalization = schema.stringOrIdent("kernel_normalization").map { value =>
        HrfNormalization.fromString(value).fold(error => throw ParseError(error.message, schema.position("kernel_normalization")), identity)
      }
      val temporalDerivative = schema.stringOrIdent("temporal_derivative").map {
        case "analytic" => TemporalDerivativeConvention.AnalyticSpmg
        case "spm-1s" => TemporalDerivativeConvention.SpmOneSecondBackwardDifference
        case other => throw ParseError(s"unknown temporal derivative convention '$other'", schema.position("temporal_derivative"))
      }
      val peakStep = schema.numeric("event_peak_step").map(value =>
        PositiveSeconds(value, "event_peak_step").fold(error => throw ParseError(error.message, schema.position("event_peak_step")), identity))
      val eventNormalization = schema.stringOrIdent("event_normalization").map {
        case "as-convolved" => EventResponseNormalization.PreservePulseScale
        case "unit-peak" => EventResponseNormalization.UnitPeak(peakStep.getOrElse(PositiveSeconds(0.1, "event_peak_step").toOption.get))
        case other => throw ParseError(s"unknown event normalization '$other'", schema.position("event_normalization"))
      }
      if peakStep.nonEmpty && !eventNormalization.exists(_.isInstanceOf[EventResponseNormalization.UnitPeak]) then
        throw ParseError("event_peak_step requires event_normalization = unit-peak", schema.position("event_peak_step"))
      val allowed = Set("temporal_derivative", "event_normalization", "event_peak_step", "include_main", "orthogonalize", "shared_slopes", "orthogonalize_basis", "span", "kernel_normalization", "basis", "subset", "onsets", "durations", "phase", "parent", "hrf_fun", "contrasts", "id", "name", "prefix", "lag", "nbasis", "summate", "scaling", "normalize")
      schema.rejectUnknown(allowed)

      HrfCall(
        vars = pos,
        basis = basis,
        subset = subset,
        onsets = onsets,
        durations = durations,
        phase = phase,
        hrfFun = hrfFun,
        contrasts = contrasts,
        id = id,
        prefix = prefix,
        lag = lag,
        nbasis = nbasis,
        summate = summate,
        scaling = scaling,
        normalize = normalize,
        span = span,
        kernelNormalization = kernelNormalization,
        includeMain = schema.boolean("include_main"),
        orthogonalize = schema.boolean("orthogonalize"),
        sharedSlopes = schema.boolean("shared_slopes"),
        orthogonalizeBasis = schema.boolean("orthogonalize_basis"),
        temporalDerivative = temporalDerivative,
        eventNormalization = eventNormalization
      )

    private def buildTrialwiseCall(args: Vector[Arg]): TrialwiseCall =
      val schema = TermArgs("trialwise", args)
      schema.requireNoPositional()

      val basis = schema.stringOrIdent("basis")
      val subset = schema.raw("subset")
      val onsets = schema.vectorRef("onsets")
      val durations = schema.vectorRef("durations")
      val phaseId = schema.phaseId("phase")
      val parent = schema.columnId("parent")
      val phase =
        (phaseId, parent) match
          case (Some(id), Some(parentColumn)) => Some(PhaseRef(id, parentColumn))
          case (None, None)                   => None
          case (Some(_), None) =>
            throw ParseError("trialwise(...) 'phase' requires a 'parent' trial-id column", schema.position("phase"))
          case (None, Some(_)) =>
            throw ParseError("trialwise(...) 'parent' requires a 'phase' identity", schema.position("parent"))
      val id = schema.stringOrIdentRef("id")
      val lag = schema.numeric("lag")
      val nbasis = schema.integer("nbasis")
      val addSum = schema.boolean("add_sum")
      val label = schema.termId("label")
      val scaling = schema.stringOrIdent("scaling").map { value =>
        HrfColumnScaling.parse(value).fold(error => throw ParseError(error.message, schema.position("scaling")), identity)
      }
      val normalize = schema.boolean("normalize")
      if scaling.nonEmpty && normalize.nonEmpty then
        throw ParseError("trialwise(...) accepts either 'scaling' or compatibility 'normalize', not both", schema.position("normalize"))

      val allowed = Set("basis", "subset", "onsets", "durations", "phase", "parent", "id", "lag", "nbasis", "add_sum", "label", "scaling", "normalize")
      schema.rejectUnknown(allowed)

      TrialwiseCall(
        basis = basis,
        subset = subset,
        onsets = onsets,
        durations = durations,
        phase = phase,
        id = id,
        lag = lag,
        nbasis = nbasis,
        addSum = addSum,
        label = label,
        scaling = scaling,
        normalize = normalize
      )

    private def buildCovariateCall(args: Vector[Arg]): CovariateCall =
      val schema = TermArgs("covariate", args)
      val pos = schema.positionalAt.map {
        case (v: ArgValue.Ident, _) => v
        case (other, at)            => throw ParseError(s"covariate(...) positional args must be identifiers, found $other", at)
      }
      if pos.isEmpty then throw ParseError("covariate(...) requires at least one variable", cur.pos)

      val data = schema.stringOrIdent("data")
      val id = schema.termId("id")
      val prefix = schema.termId("prefix")

      val allowed = Set("data", "id", "prefix")
      schema.rejectUnknown(allowed)

      CovariateCall(vars = pos, data = data, id = id, prefix = prefix)
