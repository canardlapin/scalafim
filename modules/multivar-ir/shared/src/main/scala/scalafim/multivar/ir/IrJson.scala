package scalafim.multivar.ir

private[ir] enum IrJson:
  case Obj(fields: Vector[(String, IrJson)])
  case Arr(values: Vector[IrJson])
  case Str(value: String)
  case Num(value: Double)
  case Bool(value: Boolean)
  case Null

private[ir] object IrJson:
  def parse(text: String): Either[IrError, IrJson] =
    val parser = Parser(text)
    parser.value().flatMap { parsed =>
      parser.skipWhitespace()
      if parser.atEnd then Right(parsed)
      else Left(parser.error("unexpected trailing input"))
    }

  def render(value: IrJson): String =
    value match
      case Obj(fields) =>
        fields.map { case (key, item) => s"${quote(key)}:${render(item)}" }.mkString("{", ",", "}")
      case Arr(values) => values.map(render).mkString("[", ",", "]")
      case Str(value) => quote(value)
      case Num(value) =>
        if value == Math.rint(value) && Math.abs(value) <= 9007199254740991.0 then value.toLong.toString
        else value.toString
      case Bool(value) => value.toString
      case Null => "null"

  private def quote(value: String): String =
    val out = new StringBuilder
    out.append('"')
    value.foreach { character =>
      character match
        case '"' => out.append("\\\"")
        case '\\' => out.append("\\\\")
        case '\b' => out.append("\\b")
        case '\f' => out.append("\\f")
        case '\n' => out.append("\\n")
        case '\r' => out.append("\\r")
        case '\t' => out.append("\\t")
        case value if value < ' ' => out.append(f"\\u${value.toInt}%04x")
        case value => out.append(value)
    }
    out.append('"')
    out.result()

  private final class Parser(input: String):
    private var index = 0

    def atEnd: Boolean = index >= input.length

    def skipWhitespace(): Unit =
      while !atEnd && input.charAt(index).isWhitespace do index += 1

    def error(detail: String): IrError =
      IrError(RejectionCategory.Malformed, s"json@$index", detail)

    def value(): Either[IrError, IrJson] =
      skipWhitespace()
      if atEnd then Left(error("unexpected end of input"))
      else
        input.charAt(index) match
          case '{' => objectValue()
          case '[' => arrayValue()
          case '"' => stringValue().map(Str.apply)
          case 't' => literal("true", Bool(true))
          case 'f' => literal("false", Bool(false))
          case 'n' => literal("null", Null)
          case '-' => numberValue()
          case character if character.isDigit => numberValue()
          case character => Left(error(s"unexpected character '$character'"))

    private def objectValue(): Either[IrError, IrJson] =
      index += 1
      skipWhitespace()
      if consume('}') then Right(Obj(Vector.empty))
      else
        val fields = Vector.newBuilder[(String, IrJson)]
        val seen = scala.collection.mutable.HashSet.empty[String]
        var done = false
        var failure = Option.empty[IrError]
        while !done && failure.isEmpty do
          stringValue() match
            case Left(value) => failure = Some(value)
            case Right(key) =>
              if seen.contains(key) then failure = Some(error(s"duplicate object field '$key'"))
              else
                seen += key
                skipWhitespace()
                if !consume(':') then failure = Some(error("expected ':'"))
                else
                  value() match
                    case Left(value) => failure = Some(value)
                    case Right(item) => fields += key -> item
          if failure.isEmpty then
            skipWhitespace()
            if consume('}') then done = true
            else if !consume(',') then failure = Some(error("expected ',' or '}'"))
            else skipWhitespace()
        failure match
          case Some(value) => Left(value)
          case None => Right(Obj(fields.result()))

    private def arrayValue(): Either[IrError, IrJson] =
      index += 1
      skipWhitespace()
      if consume(']') then Right(Arr(Vector.empty))
      else
        val values = Vector.newBuilder[IrJson]
        var done = false
        var failure = Option.empty[IrError]
        while !done && failure.isEmpty do
          value() match
            case Left(value) => failure = Some(value)
            case Right(item) => values += item
          if failure.isEmpty then
            skipWhitespace()
            if consume(']') then done = true
            else if !consume(',') then failure = Some(error("expected ',' or ']'"))
        failure match
          case Some(value) => Left(value)
          case None => Right(Arr(values.result()))

    private def stringValue(): Either[IrError, String] =
      skipWhitespace()
      if !consume('"') then Left(error("expected string"))
      else
        val out = new StringBuilder
        var done = false
        var failure = Option.empty[IrError]
        while !done && failure.isEmpty do
          if atEnd then failure = Some(error("unterminated string"))
          else
            val character = input.charAt(index)
            index += 1
            character match
              case '"' => done = true
              case '\\' =>
                escape() match
                  case Left(value) => failure = Some(value)
                  case Right(value) => out.append(value)
              case value if value < ' ' => failure = Some(error("unescaped control character"))
              case value => out.append(value)
        failure match
          case Some(value) => Left(value)
          case None => Right(out.result())

    private def escape(): Either[IrError, Char] =
      if atEnd then Left(error("unterminated escape"))
      else
        val character = input.charAt(index)
        index += 1
        character match
          case '"' => Right('"')
          case '\\' => Right('\\')
          case '/' => Right('/')
          case 'b' => Right('\b')
          case 'f' => Right('\f')
          case 'n' => Right('\n')
          case 'r' => Right('\r')
          case 't' => Right('\t')
          case 'u' => unicodeEscape()
          case value => Left(error(s"invalid escape '$value'"))

    private def unicodeEscape(): Either[IrError, Char] =
      if index + 4 > input.length then Left(error("short unicode escape"))
      else
        val raw = input.substring(index, index + 4)
        index += 4
        try Right(Integer.parseInt(raw, 16).toChar)
        catch case _: NumberFormatException => Left(error(s"invalid unicode escape '$raw'"))

    private def numberValue(): Either[IrError, IrJson] =
      val start = index
      consume('-')
      if consume('0') then ()
      else if !digits() then return Left(error("invalid number"))
      if consume('.') && !digits() then return Left(error("invalid fractional number"))
      if !atEnd && (input.charAt(index) == 'e' || input.charAt(index) == 'E') then
        index += 1
        if !atEnd && (input.charAt(index) == '+' || input.charAt(index) == '-') then index += 1
        if !digits() then return Left(error("invalid exponent"))
      val raw = input.substring(start, index)
      try
        val parsed = raw.toDouble
        if parsed.isFinite then Right(Num(parsed)) else Left(error("non-finite number"))
      catch case _: NumberFormatException => Left(error(s"invalid number '$raw'"))

    private def digits(): Boolean =
      val start = index
      while !atEnd && input.charAt(index).isDigit do index += 1
      index > start

    private def literal(expected: String, value: IrJson): Either[IrError, IrJson] =
      if input.startsWith(expected, index) then
        index += expected.length
        Right(value)
      else Left(error(s"expected '$expected'"))

    private def consume(character: Char): Boolean =
      if !atEnd && input.charAt(index) == character then
        index += 1
        true
      else false
