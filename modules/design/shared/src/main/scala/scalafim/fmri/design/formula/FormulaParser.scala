package scalafim.fmri.design.formula

import scalafim.fmri.design.{ColumnId, DesignError, HrfColumnScaling, PhaseId, TermId}

object FormulaParser:

  final case class ParseError(message: String, pos: Int) extends IllegalArgumentException(s"$message (at char $pos)")

  private enum Tok:
    case Ident(value: String)
    case Str(value: String)
    case Num(value: Double)
    case Bool(value: Boolean)
    case Plus
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
    val p = new Parser(toks)
    val out = p.parseFormula()
    p.expect(Tok.EOF)
    out

  def parseEither(input: String): Either[ParseError, ModelFormula] =
    try Right(parse(input))
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
          case '"' | '\'' =>
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
            out += Token(Tok.Str(sb.result()), start)
          case c if isIdentStart(c) =>
            val start = i
            i += 1
            while i < n && isIdentPart(input.charAt(i)) do i += 1
            val raw = input.substring(start, i)
            raw.toLowerCase match
              case "true"  => out += Token(Tok.Bool(true), start)
              case "false" => out += Token(Tok.Bool(false), start)
              case _       => out += Token(Tok.Ident(raw), start)
          case c
              if c.isDigit ||
                (c == '.' && i + 1 < n && input.charAt(i + 1).isDigit) ||
                (c == '-' && i + 1 < n && (input.charAt(i + 1).isDigit || (input.charAt(i + 1) == '.' && i + 2 < n && input.charAt(i + 2).isDigit))) =>
            val start = i
            i += 1
            while i < n && isNumberPart(input.charAt(i)) do i += 1
            val raw = input.substring(start, i)
            val value =
              try raw.toDouble
              catch case _: NumberFormatException => throw ParseError(s"Invalid number literal '$raw'", start)
            out += Token(Tok.Num(value), start)
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
  private def columnId(token: String): ColumnId =
    ColumnId.unsafe(token)

  private def idDetail(error: DesignError): String =
    error match
      case DesignError.InvalidId(_, value, reason) => s"'$value' $reason"
      case other                                   => other.message

  private def isIdentStart(c: Char): Boolean =
    c.isLetter || c == '_'

  private def isIdentPart(c: Char): Boolean =
    c.isLetterOrDigit || c == '_' || c == '.'

  private def isNumberPart(c: Char): Boolean =
    c.isDigit || c == '.' || c == 'e' || c == 'E' || c == '+' || c == '-'

  private final class Parser(tokens: Vector[Token]):
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

    private def value(): ArgValue =
      parseOr()

    private def bin(op: String, left: ArgValue, right: ArgValue): ArgValue =
      ArgValue.Call(op, Vector(Arg(None, left), Arg(None, right)))

    private def unary(op: String, expr: ArgValue): ArgValue =
      ArgValue.Call(op, Vector(Arg(None, expr)))

    private def parseOr(): ArgValue =
      var left = parseAnd()
      while tok == Tok.Or do
        expect(Tok.Or)
        val right = parseAnd()
        left = bin("|", left, right)
      left

    private def parseAnd(): ArgValue =
      var left = parseCmp()
      while tok == Tok.And do
        expect(Tok.And)
        val right = parseCmp()
        left = bin("&", left, right)
      left

    private def parseCmp(): ArgValue =
      val left = parseUnary()
      tok match
        case Tok.EqEq =>
          expect(Tok.EqEq)
          bin("==", left, parseUnary())
        case Tok.NotEq =>
          expect(Tok.NotEq)
          bin("!=", left, parseUnary())
        case Tok.Lt =>
          expect(Tok.Lt)
          bin("<", left, parseUnary())
        case Tok.Lte =>
          expect(Tok.Lte)
          bin("<=", left, parseUnary())
        case Tok.Gt =>
          expect(Tok.Gt)
          bin(">", left, parseUnary())
        case Tok.Gte =>
          expect(Tok.Gte)
          bin(">=", left, parseUnary())
        case _ => left

    private def parseUnary(): ArgValue =
      tok match
        case Tok.Bang =>
          expect(Tok.Bang)
          unary("!", parseUnary())
        case _ => parsePrimary()

    private def parsePrimary(): ArgValue =
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
            ArgValue.Ident(columnId(v))
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

    private def arg(): Arg =
      tok match
        case Tok.Ident(nm) if tokens.lift(ix + 1).exists(_.tok == Tok.Eq) =>
          ix += 1
          expect(Tok.Eq)
          Arg(Some(nm), value())
        case _ =>
          Arg(None, value())

    private def args(): Vector[Arg] =
      if tok == Tok.RParen then Vector.empty
      else
        val out = Vector.newBuilder[Arg]
        out += arg()
        while eat(Tok.Comma) do out += arg()
        out.result()

    def parseFormula(): ModelFormula =
      val onset = columnId(ident())
      expect(Tok.Tilde)
      val terms = Vector.newBuilder[TermCall]
      terms += parseTerm()
      while eat(Tok.Plus) do terms += parseTerm()
      val ts = terms.result()
      if ts.isEmpty then throw ParseError("Formula RHS must have at least one term", cur.pos)
      ModelFormula(onset, ts)

    private def parseTerm(): TermCall =
      val fun = ident()
      expect(Tok.LParen)
      val as = args()
      expect(Tok.RParen)
      fun match
        case "hrf"       => buildHrfCall(as)
        case "trialwise" => buildTrialwiseCall(as)
        case "covariate" => buildCovariateCall(as)
        case other       => throw ParseError(s"Unknown term function '$other'", cur.pos)

    private final case class TermArgs(fun: String, args: Vector[Arg]):
      val positional: Vector[ArgValue] =
        args.iterator.collect { case Arg(None, v) => v }.toVector

      val named: Map[String, ArgValue] =
        val out = scala.collection.mutable.HashMap.empty[String, ArgValue]
        args.foreach { a =>
          a.name.foreach { n =>
            if out.contains(n) then throw ParseError(s"Duplicate argument '$n'", cur.pos)
            out.update(n, a.value)
          }
        }
        out.toMap

      def rejectUnknown(allowed: Set[String]): Unit =
        val unknown = named.keySet.diff(allowed)
        if unknown.nonEmpty then
          throw ParseError(s"$fun(...) got unknown named args: ${unknown.toVector.sorted.mkString(", ")}", cur.pos)

      def requireNoPositional(): Unit =
        if positional.nonEmpty then throw ParseError(s"$fun(...) does not take positional arguments", cur.pos)

      def raw(name: String): Option[ArgValue] =
        named.get(name)

      def stringOrIdent(name: String): Option[String] =
        named.get(name).map {
          case ArgValue.Str(v)   => v
          case ArgValue.Ident(v) => v.value
          case other             => throw ParseError(s"'$name' must be a string/identifier, found $other", cur.pos)
        }

      /** A term label, parsed here so the AST never carries an unvalidated one. */
      def termId(name: String): Option[TermId] =
        stringOrIdent(name).map { v =>
          TermId(v).fold(error => throw ParseError(idDetail(error), cur.pos), identity)
        }

      def phaseId(name: String): Option[PhaseId] =
        stringOrIdent(name).map { v =>
          PhaseId(v).fold(error => throw ParseError(idDetail(error), cur.pos), identity)
        }

      def columnId(name: String): Option[ColumnId] =
        named.get(name).map {
          case ArgValue.Ident(id) => id
          case ArgValue.Str(value) =>
            ColumnId(value).fold(error => throw ParseError(idDetail(error), cur.pos), identity)
          case other => throw ParseError(s"'$name' must be a string/identifier, found $other", cur.pos)
        }

      def numeric(name: String): Option[Double] =
        named.get(name).map {
          case ArgValue.Num(v) => v
          case other           => throw ParseError(s"'$name' must be numeric, found $other", cur.pos)
        }

      def integer(name: String): Option[Int] =
        named.get(name).map {
          case ArgValue.Num(v) if v.isValidInt && v == v.toInt.toDouble => v.toInt
          case ArgValue.Num(v)                                          => throw ParseError(s"'$name' must be an integer, found $v", cur.pos)
          case other                                                    => throw ParseError(s"'$name' must be an integer, found $other", cur.pos)
        }

      def boolean(name: String): Option[Boolean] =
        named.get(name).map {
          case ArgValue.Bool(v) => v
          case ArgValue.Ident(v) =>
            v.value.toLowerCase match
              case "true"  => true
              case "false" => false
              case _       => throw ParseError(s"'$name' must be boolean, found ${v.value}", cur.pos)
          case other => throw ParseError(s"'$name' must be boolean, found $other", cur.pos)
        }

      def vectorRef(name: String): Option[ArgValue] =
        named.get(name).map {
          case v @ ArgValue.Ident(_) => v
          case v @ ArgValue.Str(_)   => v
          case v @ ArgValue.Num(_)   => v
          case other                 => throw ParseError(s"'$name' must be a string/identifier or numeric scalar, found $other", cur.pos)
        }

      def stringOrIdentRef(name: String): Option[ArgValue] =
        named.get(name).map {
          case v @ ArgValue.Str(_)   => v
          case v @ ArgValue.Ident(_) => v
          case other                 => throw ParseError(s"'$name' must be a string/identifier, found $other", cur.pos)
        }

    private def buildHrfCall(args: Vector[Arg]): HrfCall =
      val schema = TermArgs("hrf", args)
      val pos = schema.positional
      if pos.isEmpty then throw ParseError("hrf(...) requires at least one variable", cur.pos)
      pos.foreach {
        case ArgValue.Ident(_) | ArgValue.Call(_, _) => ()
        case other =>
          throw ParseError(s"hrf(...) positional args must be identifiers or calls, found $other", cur.pos)
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
            throw ParseError("hrf(...) 'phase' requires a 'parent' trial-id column", cur.pos)
          case (None, Some(_)) =>
            throw ParseError("hrf(...) 'parent' requires a 'phase' identity", cur.pos)
      val hrfFun = schema.stringOrIdentRef("hrf_fun")
      val contrasts = schema.stringOrIdent("contrasts")
      val id = schema.termId("id").orElse(schema.termId("name"))
      val prefix = schema.termId("prefix")
      val lag = schema.numeric("lag")
      val nbasis = schema.integer("nbasis")
      val summate = schema.boolean("summate")
      val scaling = schema.stringOrIdent("scaling").map { value =>
        HrfColumnScaling.parse(value).fold(error => throw ParseError(error.message, cur.pos), identity)
      }
      val normalize = schema.boolean("normalize")
      if scaling.nonEmpty && normalize.nonEmpty then
        throw ParseError("hrf(...) accepts either 'scaling' or compatibility 'normalize', not both", cur.pos)

      val allowed = Set("basis", "subset", "onsets", "durations", "phase", "parent", "hrf_fun", "contrasts", "id", "name", "prefix", "lag", "nbasis", "summate", "scaling", "normalize")
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
        normalize = normalize
      )

    private def buildTrialwiseCall(args: Vector[Arg]): TrialwiseCall =
      val schema = TermArgs("trialwise", args)
      schema.requireNoPositional()

      val basis = schema.stringOrIdent("basis")
      val durations = schema.vectorRef("durations")
      val lag = schema.numeric("lag")
      val nbasis = schema.integer("nbasis")
      val addSum = schema.boolean("add_sum")
      val label = schema.termId("label")
      val scaling = schema.stringOrIdent("scaling").map { value =>
        HrfColumnScaling.parse(value).fold(error => throw ParseError(error.message, cur.pos), identity)
      }
      val normalize = schema.boolean("normalize")
      if scaling.nonEmpty && normalize.nonEmpty then
        throw ParseError("trialwise(...) accepts either 'scaling' or compatibility 'normalize', not both", cur.pos)

      val allowed = Set("basis", "durations", "lag", "nbasis", "add_sum", "label", "scaling", "normalize")
      schema.rejectUnknown(allowed)

      TrialwiseCall(basis = basis, durations = durations, lag = lag, nbasis = nbasis, addSum = addSum, label = label, scaling = scaling, normalize = normalize)

    private def buildCovariateCall(args: Vector[Arg]): CovariateCall =
      val schema = TermArgs("covariate", args)
      val pos = schema.positional.map {
        case v: ArgValue.Ident => v
        case other             => throw ParseError(s"covariate(...) positional args must be identifiers, found $other", cur.pos)
      }
      if pos.isEmpty then throw ParseError("covariate(...) requires at least one variable", cur.pos)

      val data = schema.stringOrIdent("data")
      val id = schema.termId("id")
      val prefix = schema.termId("prefix")

      val allowed = Set("data", "id", "prefix")
      schema.rejectUnknown(allowed)

      CovariateCall(vars = pos, data = data, id = id, prefix = prefix)
