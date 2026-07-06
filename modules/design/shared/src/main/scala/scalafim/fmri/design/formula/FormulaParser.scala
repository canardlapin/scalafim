package scalafim.fmri.design.formula

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
    val p = new Parser(toks, input)
    val out = p.parseFormula()
    p.expect(Tok.EOF)
    out

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

  private def isIdentStart(c: Char): Boolean =
    c.isLetter || c == '_'

  private def isIdentPart(c: Char): Boolean =
    c.isLetterOrDigit || c == '_' || c == '.'

  private def isNumberPart(c: Char): Boolean =
    c.isDigit || c == '.' || c == 'e' || c == 'E' || c == '+' || c == '-'

  private final class Parser(tokens: Vector[Token], input: String):
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
            ArgValue.Ident(v)
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
      val onset = ident()
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

    private def namedUnique(args: Vector[Arg]): Map[String, ArgValue] =
      val out = scala.collection.mutable.HashMap.empty[String, ArgValue]
      args.foreach { a =>
        a.name.foreach { n =>
          if out.contains(n) then throw ParseError(s"Duplicate argument '$n'", cur.pos)
          out.update(n, a.value)
        }
      }
      out.toMap

    private def positional(args: Vector[Arg]): Vector[ArgValue] =
      args.iterator.collect { case Arg(None, v) => v }.toVector

    private def buildHrfCall(args: Vector[Arg]): HrfCall =
      val pos = positional(args)
      if pos.isEmpty then throw ParseError("hrf(...) requires at least one variable", cur.pos)
      pos.foreach {
        case ArgValue.Ident(_) | ArgValue.Call(_, _) => ()
        case other =>
          throw ParseError(s"hrf(...) positional args must be identifiers or calls, found $other", cur.pos)
      }

      val named = namedUnique(args)
      def strArg(name: String): Option[String] =
        named.get(name).map {
          case ArgValue.Str(v)   => v
          case ArgValue.Ident(v) => v
          case other          => throw ParseError(s"'$name' must be a string/identifier, found $other", cur.pos)
        }
      def doubleArg(name: String): Option[Double] =
        named.get(name).map {
          case ArgValue.Num(v) => v
          case other        => throw ParseError(s"'$name' must be numeric, found $other", cur.pos)
        }
      def intArg(name: String): Option[Int] =
        named.get(name).map {
          case ArgValue.Num(v) if v.isValidInt && v == v.toInt.toDouble => v.toInt
          case ArgValue.Num(v)                                          => throw ParseError(s"'$name' must be an integer, found $v", cur.pos)
          case other                                                    => throw ParseError(s"'$name' must be an integer, found $other", cur.pos)
        }
      def boolArg(name: String): Option[Boolean] =
        named.get(name).map {
          case ArgValue.Bool(v) => v
          case ArgValue.Ident(v) =>
            v.toLowerCase match
              case "true"  => true
              case "false" => false
              case _       => throw ParseError(s"'$name' must be boolean, found $v", cur.pos)
          case other => throw ParseError(s"'$name' must be boolean, found $other", cur.pos)
        }
      def vectorRefArg(name: String): Option[ArgValue] =
        named.get(name).map {
          case v @ ArgValue.Ident(_) => v
          case v @ ArgValue.Str(_)   => v
          case v @ ArgValue.Num(_)   => v
          case other                 => throw ParseError(s"'$name' must be a string/identifier or numeric scalar, found $other", cur.pos)
        }

      val basis = strArg("basis")
      val subset = named.get("subset")
      val onsets = vectorRefArg("onsets")
      val durations = vectorRefArg("durations")
      val hrfFun = named.get("hrf_fun").map {
        case v @ ArgValue.Str(_)   => v
        case v @ ArgValue.Ident(_) => v
        case other                 => throw ParseError(s"'hrf_fun' must be a string/identifier, found $other", cur.pos)
      }
      val contrasts = named.get("contrasts").map {
        case v @ ArgValue.Str(_)   => v
        case v @ ArgValue.Ident(_) => v
        case other                 => throw ParseError(s"'contrasts' must be a string/identifier, found $other", cur.pos)
      }
      val id = strArg("id").orElse(strArg("name"))
      val prefix = strArg("prefix")
      val lag = doubleArg("lag")
      val nbasis = intArg("nbasis")
      val summate = boolArg("summate")
      val normalize = boolArg("normalize")

      val allowed = Set("basis", "subset", "onsets", "durations", "hrf_fun", "contrasts", "id", "name", "prefix", "lag", "nbasis", "summate", "normalize")
      val unknown = named.keySet.diff(allowed)
      if unknown.nonEmpty then throw ParseError(s"hrf(...) got unknown named args: ${unknown.toVector.sorted.mkString(", ")}", cur.pos)

      HrfCall(
        vars = pos,
        basis = basis,
        subset = subset,
        onsets = onsets,
        durations = durations,
        hrfFun = hrfFun,
        contrasts = contrasts,
        id = id,
        prefix = prefix,
        lag = lag,
        nbasis = nbasis,
        summate = summate,
        normalize = normalize
      )

    private def buildTrialwiseCall(args: Vector[Arg]): TrialwiseCall =
      val pos = positional(args)
      if pos.nonEmpty then throw ParseError("trialwise(...) does not take positional arguments", cur.pos)

      val named = namedUnique(args)
      def strArg(name: String): Option[String] =
        named.get(name).map {
          case ArgValue.Str(v)   => v
          case ArgValue.Ident(v) => v
          case other             => throw ParseError(s"'$name' must be a string/identifier, found $other", cur.pos)
        }
      def doubleArg(name: String): Option[Double] =
        named.get(name).map {
          case ArgValue.Num(v) => v
          case other           => throw ParseError(s"'$name' must be numeric, found $other", cur.pos)
        }
      def intArg(name: String): Option[Int] =
        named.get(name).map {
          case ArgValue.Num(v) if v.isValidInt && v == v.toInt.toDouble => v.toInt
          case ArgValue.Num(v)                                          => throw ParseError(s"'$name' must be an integer, found $v", cur.pos)
          case other                                                    => throw ParseError(s"'$name' must be an integer, found $other", cur.pos)
        }
      def boolArg(name: String): Option[Boolean] =
        named.get(name).map {
          case ArgValue.Bool(v) => v
          case ArgValue.Ident(v) =>
            v.toLowerCase match
              case "true"  => true
              case "false" => false
              case _       => throw ParseError(s"'$name' must be boolean, found $v", cur.pos)
          case other => throw ParseError(s"'$name' must be boolean, found $other", cur.pos)
        }
      def vectorRefArg(name: String): Option[ArgValue] =
        named.get(name).map {
          case v @ ArgValue.Ident(_) => v
          case v @ ArgValue.Str(_)   => v
          case v @ ArgValue.Num(_)   => v
          case other                 => throw ParseError(s"'$name' must be a string/identifier or numeric scalar, found $other", cur.pos)
        }

      val basis = strArg("basis")
      val durations = vectorRefArg("durations")
      val lag = doubleArg("lag")
      val nbasis = intArg("nbasis")
      val addSum = boolArg("add_sum")
      val label = strArg("label")
      val normalize = boolArg("normalize")

      val allowed = Set("basis", "durations", "lag", "nbasis", "add_sum", "label", "normalize")
      val unknown = named.keySet.diff(allowed)
      if unknown.nonEmpty then
        throw ParseError(s"trialwise(...) got unknown named args: ${unknown.toVector.sorted.mkString(", ")}", cur.pos)

      TrialwiseCall(basis = basis, durations = durations, lag = lag, nbasis = nbasis, addSum = addSum, label = label, normalize = normalize)

    private def buildCovariateCall(args: Vector[Arg]): CovariateCall =
      val pos = positional(args).map {
        case ArgValue.Ident(v) => ArgValue.Ident(v)
        case other             => throw ParseError(s"covariate(...) positional args must be identifiers, found $other", cur.pos)
      }
      if pos.isEmpty then throw ParseError("covariate(...) requires at least one variable", cur.pos)

      val named = namedUnique(args)
      def strOrIdent(name: String): Option[String] =
        named.get(name).map {
          case ArgValue.Str(v)   => v
          case ArgValue.Ident(v) => v
          case other          => throw ParseError(s"'$name' must be a string/identifier, found $other", cur.pos)
        }

      val data = strOrIdent("data")
      val id = strOrIdent("id")
      val prefix = strOrIdent("prefix")

      val allowed = Set("data", "id", "prefix")
      val unknown = named.keySet.diff(allowed)
      if unknown.nonEmpty then throw ParseError(s"covariate(...) got unknown named args: ${unknown.toVector.sorted.mkString(", ")}", cur.pos)

      CovariateCall(vars = pos, data = data, id = id, prefix = prefix)
