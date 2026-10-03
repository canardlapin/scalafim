package scalafim.fmri.design.formula

import scalafim.fmri.design.PortableNumber
import scala.util.control.NonFatal
import ujson.{Arr, Bool, Null, Num, Obj, Str, Value}
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

/** Shared strict JSON reading and canonical writing for portable model
  * specifications ([[ModelJsonCodec]] and the model module's build-spec codec).
  *
  * Reading is total and reports the JSON path of the first problem: duplicate
  * object keys, missing or unknown fields, wrong value kinds, non-finite or
  * non-integral numbers. Writing is byte-identical on the JVM and Scala.js:
  * numbers use [[PortableNumber.format]] and strings use a fixed escape set,
  * independent of the platform's `Double.toString` or ujson's renderer.
  */
private[fmri] object PortableJson:
  type Result[A] = Either[ModelJsonError, A]

  def fail[A](path: String, detail: String): Result[A] = Left(ModelJsonError(path, detail))

  def admit[A, E](value: Either[E, A], path: String)(message: E => String): Result[A] =
    value.left.map(error => ModelJsonError(path, message(error)))

  def traverse[A, B](values: Vector[A])(f: A => Result[B]): Result[Vector[B]] =
    values.foldLeft[Result[Vector[B]]](Right(Vector.empty))((acc, value) => for previous <- acc; next <- f(value) yield previous :+ next)

  def ensure(condition: Boolean, path: String, detail: => String): Result[Unit] =
    if condition then Right(()) else fail(path, detail)

  // ------------------------------------------------------------------ parsing

  private final case class DuplicateKey(path: String, key: String) extends RuntimeException(s"duplicate key '$key'")

  private def childPath(path: String, key: String): String = s"$path.$key"

  /** Builds an ordinary `ujson.Value`, rejecting a repeated key within one object. */
  private final class StrictVisitor(path: String) extends Visitor.Delegate[Value, Value](ujson.Value):
    override def visitObject(length: Int, jsonableKeys: Boolean, index: Int): ObjVisitor[Value, Value] =
      val inner = ujson.Value.visitObject(length, jsonableKeys, index)
      new ObjVisitor[Value, Value]:
        private val seen = scala.collection.mutable.HashSet.empty[String]
        private var key = ""
        def visitKey(index: Int): Visitor[?, ?] = inner.visitKey(index)
        def visitKeyValue(v: Any): Unit =
          key = v.toString
          if !seen.add(key) then throw DuplicateKey(path, key)
          inner.visitKeyValue(v)
        def subVisitor: Visitor[?, ?] = StrictVisitor(childPath(path, key))
        def visitValue(v: Value, index: Int): Unit = inner.visitValue(v, index)
        def visitEnd(index: Int): Value = inner.visitEnd(index)

    override def visitArray(length: Int, index: Int): ArrVisitor[Value, Value] =
      val inner = ujson.Value.visitArray(length, index)
      new ArrVisitor[Value, Value]:
        private var position = 0
        def subVisitor: Visitor[?, ?] = StrictVisitor(s"$path[$position]")
        def visitValue(v: Value, index: Int): Unit =
          inner.visitValue(v, index)
          position += 1
        def visitEnd(index: Int): Value = inner.visitEnd(index)

  private def duplicate(error: Throwable): Option[DuplicateKey] =
    Iterator.iterate(error)(_.getCause).takeWhile(_ != null).take(16).collectFirst { case found: DuplicateKey => found }

  /** Parse JSON text; duplicate keys anywhere in the document are an error. */
  def parse(input: String): Result[Cursor] =
    try Right(Cursor(ujson.transform(ujson.Readable.fromString(input), StrictVisitor("$")), "$"))
    catch
      case NonFatal(error) =>
        duplicate(error) match
          case Some(DuplicateKey(path, key)) => fail(path, s"duplicate key '$key'")
          case None => fail("$", s"invalid JSON: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}")

  private def kind(value: Value): String = value match
    case Str(_) => "string"
    case Num(_) => "number"
    case Bool(_) => "boolean"
    case Null => "null"
    case Arr(_) => "array"
    case Obj(_) => "object"

  /** A JSON value together with its path from the document root. */
  final case class Cursor(value: Value, path: String):
    private def expected[A](what: String): Result[A] = fail(path, s"expected $what, got ${kind(value)}")

    def isNull: Boolean = value == Null

    def obj: Result[collection.Map[String, Value]] = value match
      case Obj(fields) => Right(fields)
      case _ => expected("object")

    /** Exactly these fields: none missing, none unknown. */
    def fields(names: Set[String]): Result[Unit] = obj.flatMap { fields =>
      val actual = fields.keySet.toSet
      val missing = names.diff(actual).toVector.sorted
      val unknown = actual.diff(names).toVector.sorted
      if missing.nonEmpty then fail(path, s"missing field(s) ${missing.mkString(", ")}")
      else if unknown.nonEmpty then fail(path, s"unknown field(s) ${unknown.mkString(", ")}")
      else Right(())
    }

    def field(name: String): Result[Cursor] = obj.flatMap { fields =>
      fields.get(name).map(Cursor(_, childPath(path, name))).toRight(ModelJsonError(path, s"missing field '$name'"))
    }

    def str: Result[String] = value match
      case Str(text) => Right(text)
      case _ => expected("string")

    def bool: Result[Boolean] = value match
      case Bool(flag) => Right(flag)
      case _ => expected("boolean")

    def finite: Result[Double] = value match
      case Num(number) if number.isFinite => Right(number)
      case Num(_) => fail(path, "expected finite number")
      case _ => expected("number")

    def integer: Result[Int] = finite.flatMap { number =>
      if number.isValidInt then Right(number.toInt) else fail(path, s"expected integer, got ${PortableNumber.format(number)}")
    }

    def arr: Result[Vector[Cursor]] = value match
      case Arr(values) => Right(values.toVector.zipWithIndex.map((v, i) => Cursor(v, s"$path[$i]")))
      case _ => expected("array")

    def nullable[A](decode: Cursor => Result[A]): Result[Option[A]] =
      if isNull then Right(None) else decode(this).map(Some(_))

    def entries: Result[Vector[(String, Cursor)]] = obj.map(_.toVector.map((key, v) => key -> Cursor(v, childPath(path, key))))

  // ------------------------------------------------------------------ writing

  def number(value: Double, path: String): Result[Value] =
    if value.isFinite then Right(Num(value)) else fail(path, "non-finite number has no portable JSON representation")

  def numbers(values: Seq[Double], path: String): Result[Value] =
    traverse(values.toVector.zipWithIndex)((value, index) => number(value, s"$path[$index]")).map(Arr.from(_))

  def optional[A](value: Option[A])(encode: A => Result[Value]): Result[Value] =
    value.fold[Result[Value]](Right(Null))(encode)

  /** Canonical text: no insignificant whitespace, fields in insertion order,
    * numbers in [[PortableNumber]] form, fixed string escapes.
    */
  def render(value: Value): Result[String] =
    val out = new StringBuilder
    def string(text: String): Unit =
      out.append('"')
      text.foreach {
        case '"' => out.append("\\\"")
        case '\\' => out.append("\\\\")
        case '\n' => out.append("\\n")
        case '\r' => out.append("\\r")
        case '\t' => out.append("\\t")
        case '\b' => out.append("\\b")
        case '\f' => out.append("\\f")
        case c if c < ' ' =>
          val hex = Integer.toHexString(c.toInt)
          out.append("\\u").append("0" * (4 - hex.length)).append(hex)
        case c => out.append(c)
      }
      out.append('"')
    def loop(value: Value, path: String): Result[Unit] = value match
      case Str(text) => Right(string(text))
      case Num(number) =>
        if number.isFinite then Right(out.append(PortableNumber.format(number))).map(_ => ())
        else fail(path, "non-finite number has no portable JSON representation")
      case Bool(flag) => Right(out.append(if flag then "true" else "false")).map(_ => ())
      case Null => Right(out.append("null")).map(_ => ())
      case Arr(values) =>
        out.append('[')
        val result = values.zipWithIndex.foldLeft[Result[Unit]](Right(())) { case (acc, (item, index)) =>
          acc.flatMap { _ =>
            if index > 0 then out.append(',')
            loop(item, s"$path[$index]")
          }
        }
        out.append(']')
        result
      case Obj(fields) =>
        out.append('{')
        val result = fields.toVector.zipWithIndex.foldLeft[Result[Unit]](Right(())) { case (acc, ((key, item), index)) =>
          acc.flatMap { _ =>
            if index > 0 then out.append(',')
            string(key)
            out.append(':')
            loop(item, childPath(path, key))
          }
        }
        out.append('}')
        result
    loop(value, "$").map(_ => out.result())
