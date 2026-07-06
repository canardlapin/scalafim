package scalafim.bids

enum JsonValue:
  case Obj(fields: Map[String, JsonValue])
  case Arr(values: Vector[JsonValue])
  case Str(value: String)
  case Num(value: Double)
  case Bool(value: Boolean)
  case Null

  def asObject: Option[Map[String, JsonValue]] =
    this match
      case JsonValue.Obj(fields) => Some(fields)
      case _                     => None

  def asString: Option[String] =
    this match
      case JsonValue.Str(value) => Some(value)
      case _                    => None

  def asNumber: Option[Double] =
    this match
      case JsonValue.Num(value) => Some(value)
      case _                    => None

object JsonValue:
  val EmptyObject: JsonValue.Obj = JsonValue.Obj(Map.empty)

  def merge(left: JsonValue.Obj, right: JsonValue.Obj): JsonValue.Obj =
    JsonValue.Obj(
      right.fields.foldLeft(left.fields) { case (acc, (key, value)) =>
        val merged =
          (acc.get(key), value) match
            case (Some(l: JsonValue.Obj), r: JsonValue.Obj) => merge(l, r)
            case _                                          => value
        acc.updated(key, merged)
      }
    )

object BidsJson:
  def parse(text: String): Either[BidsError, JsonValue] =
    val parser = Parser(text)
    parser.parseValue().flatMap { value =>
      parser.skipWhitespace()
      if parser.atEnd then Right(value)
      else Left(BidsError.InvalidJson(s"unexpected trailing input at offset ${parser.offset}"))
    }

  def parseObject(text: String): Either[BidsError, JsonValue.Obj] =
    parse(text).flatMap {
      case obj: JsonValue.Obj => Right(obj)
      case _                  => Left(BidsError.InvalidJson("top-level JSON value must be an object"))
    }

  private final class Parser(input: String):
    private var index = 0

    def offset: Int = index
    def atEnd: Boolean = index >= input.length

    def skipWhitespace(): Unit =
      while !atEnd && input.charAt(index).isWhitespace do index += 1

    def parseValue(): Either[BidsError, JsonValue] =
      skipWhitespace()
      if atEnd then Left(BidsError.InvalidJson("unexpected end of input"))
      else
        input.charAt(index) match
          case '{' => parseObject()
          case '[' => parseArray()
          case '"' => parseString().map(JsonValue.Str.apply)
          case 't' => parseLiteral("true", JsonValue.Bool(true))
          case 'f' => parseLiteral("false", JsonValue.Bool(false))
          case 'n' => parseLiteral("null", JsonValue.Null)
          case c if c == '-' || c.isDigit => parseNumber()
          case c => Left(BidsError.InvalidJson(s"unexpected character '$c' at offset $index"))

    private def parseObject(): Either[BidsError, JsonValue] =
      index += 1
      skipWhitespace()
      def loop(fields: Map[String, JsonValue]): Either[BidsError, JsonValue] =
        skipWhitespace()
        if consume('}') then Right(JsonValue.Obj(fields))
        else
          for
            key <- parseString()
            _ = skipWhitespace()
            _ <- expect(':', s"expected ':' after object key at offset $index")
            value <- parseValue()
            _ = skipWhitespace()
            result <-
              val next = fields.updated(key, value)
              if consume('}') then Right(JsonValue.Obj(next))
              else if consume(',') then loop(next)
              else Left(BidsError.InvalidJson(s"expected ',' or '}' at offset $index"))
          yield result

      loop(Map.empty)

    private def parseArray(): Either[BidsError, JsonValue] =
      index += 1
      skipWhitespace()
      if consume(']') then Right(JsonValue.Arr(Vector.empty))
      else
        def loop(values: Vector[JsonValue]): Either[BidsError, JsonValue] =
          for
            value <- parseValue()
            _ = skipWhitespace()
            result <-
              val next = values :+ value
              if consume(']') then Right(JsonValue.Arr(next))
              else if consume(',') then loop(next)
              else Left(BidsError.InvalidJson(s"expected ',' or ']' at offset $index"))
          yield result

        loop(Vector.empty)

    private def parseString(): Either[BidsError, String] =
      if !consume('"') then Left(BidsError.InvalidJson(s"expected string at offset $index"))
      else
        val out = new StringBuilder

        def loop(): Either[BidsError, String] =
          if atEnd then Left(BidsError.InvalidJson("unterminated string"))
          else
            val c = input.charAt(index)
            index += 1
            c match
              case '"' => Right(out.toString)
              case '\\' => parseEscape(out).flatMap(_ => loop())
              case other if other < ' ' =>
                Left(BidsError.InvalidJson(s"unescaped control character in string at offset ${index - 1}"))
              case other =>
                out.append(other)
                loop()

        loop()

    private def parseEscape(out: StringBuilder): Either[BidsError, Unit] =
      if atEnd then Left(BidsError.InvalidJson("unterminated escape sequence"))
      else
        val esc = input.charAt(index)
        index += 1
        esc match
          case '"' =>
            out.append('"')
            Right(())
          case '\\' =>
            out.append('\\')
            Right(())
          case '/' =>
            out.append('/')
            Right(())
          case 'b' =>
            out.append('\b')
            Right(())
          case 'f' =>
            out.append('\f')
            Right(())
          case 'n' =>
            out.append('\n')
            Right(())
          case 'r' =>
            out.append('\r')
            Right(())
          case 't' =>
            out.append('\t')
            Right(())
          case 'u' => parseUnicodeEscape(out)
          case other => Left(BidsError.InvalidJson(s"invalid escape '\\$other'"))

    private def parseUnicodeEscape(out: StringBuilder): Either[BidsError, Unit] =
      if index + 4 > input.length then Left(BidsError.InvalidJson("short unicode escape"))
      else
        val hex = input.substring(index, index + 4)
        if !hex.forall(c => c.isDigit || "abcdefABCDEF".contains(c)) then
          Left(BidsError.InvalidJson(s"invalid unicode escape '$hex'"))
        else
          out.append(Integer.parseInt(hex, 16).toChar)
          index += 4
          Right(())

    private def parseNumber(): Either[BidsError, JsonValue] =
      val start = index
      if !atEnd && input.charAt(index) == '-' then index += 1
      val integer =
        if atEnd then Left(BidsError.InvalidJson(s"invalid number at offset $start"))
        else if input.charAt(index) == '0' then
          index += 1
          if !atEnd && input.charAt(index).isDigit then Left(BidsError.InvalidJson(s"invalid number at offset $start"))
          else Right(())
        else
          val integerDigits = consumeDigits()
          if integerDigits == 0 then Left(BidsError.InvalidJson(s"invalid number at offset $start"))
          else Right(())

      integer.flatMap { _ =>
        if !atEnd && input.charAt(index) == '.' then
          index += 1
          val fractionDigits = consumeDigits()
          if fractionDigits == 0 then Left(BidsError.InvalidJson(s"invalid number at offset $start"))
          else parseExponentAndFinish(start)
        else parseExponentAndFinish(start)
      }

    private def parseExponentAndFinish(start: Int): Either[BidsError, JsonValue] =
      if !atEnd && (input.charAt(index) == 'e' || input.charAt(index) == 'E') then
        index += 1
        if !atEnd && (input.charAt(index) == '+' || input.charAt(index) == '-') then index += 1
        val exponentDigits = consumeDigits()
        if exponentDigits == 0 then Left(BidsError.InvalidJson(s"invalid number at offset $start"))
        else finishNumber(start)
      else finishNumber(start)

    private def finishNumber(start: Int): Either[BidsError, JsonValue] =
      val raw = input.substring(start, index)
      raw.toDoubleOption match
        case Some(number) if number.isFinite => Right(JsonValue.Num(number))
        case _ => Left(BidsError.InvalidJson(s"invalid number '$raw'"))

    private def parseLiteral(lit: String, value: JsonValue): Either[BidsError, JsonValue] =
      if input.startsWith(lit, index) then
        index += lit.length
        Right(value)
      else Left(BidsError.InvalidJson(s"expected '$lit' at offset $index"))

    private def consume(c: Char): Boolean =
      if !atEnd && input.charAt(index) == c then
        index += 1
        true
      else false

    private def expect(c: Char, message: => String): Either[BidsError, Unit] =
      if consume(c) then Right(()) else Left(BidsError.InvalidJson(message))

    private def consumeDigits(): Int =
      val start = index
      while !atEnd && input.charAt(index).isDigit do index += 1
      index - start
