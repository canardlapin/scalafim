package scalafim.fmri.design.formula

import scalafim.fmri.design.{BasisIndex, ColumnId, FactorId, RunContrastCombination}
import scalafim.fmri.design.contrast.*
import scalafim.fmri.hrf.*
import scala.util.control.NonFatal
import ujson.{Arr, Bool, Null, Num, Obj, Str, Value}

final case class ModelJsonError(path: String, detail: String):
  def message: String = s"$path: $detail"

/** Versioned portable specifications. Runtime callbacks are deliberately rejected. */
object ModelJsonCodec:
  private final case class Invalid(path: String, detail: String) extends IllegalArgumentException(detail)
  private def fail(path: String, detail: String): Nothing = throw Invalid(path, detail)
  private def checked[A](body: => A): Either[ModelJsonError, A] =
    try Right(body)
    catch
      case Invalid(path, detail) => Left(ModelJsonError(path, detail))
      case NonFatal(error) => Left(ModelJsonError("$", Option(error.getMessage).getOrElse("invalid JSON")))
  private def admit[A, E](value: Either[E, A], path: String)(message: E => String): A =
    value.fold(e => fail(path, message(e)), identity)
  private def envelope(kind: String, value: Value): Value =
    Obj("schema" -> Str("scalafim.model-spec"), "version" -> Num(1), "kind" -> Str(kind), "value" -> value)
  private def payload(input: String, kind: String): Value =
    val root = ujson.read(input)
    fields(root, Set("schema", "version", "kind", "value"), "$")
    if root("schema").str != "scalafim.model-spec" then fail("$.schema", "unknown schema")
    if root("version").num != 1 then fail("$.version", "unsupported version")
    if root("kind").str != kind then fail("$.kind", s"expected $kind")
    root("value")
  private def fields(value: Value, expected: Set[String], path: String): Unit =
    val actual = value.obj.keySet.toSet
    if actual != expected then fail(path, s"expected fields ${expected.toVector.sorted.mkString(", ")}; got ${actual.toVector.sorted.mkString(", ")}")
  private def finite(value: Value, path: String): Double =
    val number = value.num
    if !number.isFinite then fail(path, "expected finite number")
    number
  private def integer(value: Value, path: String): Int =
    val number = finite(value, path)
    if !number.isValidInt then fail(path, "expected integer")
    number.toInt
  private def nums(values: Seq[Double]): Value =
    if !values.forall(_.isFinite) then fail("$.value", "non-finite weights")
    Arr.from(values)
  private def optional[A](value: Option[A])(encode: A => Value): Value = value.fold[Value](Null)(encode)
  private def readOptional[A](value: Value)(decode: Value => A): Option[A] =
    if value == Null then None else Some(decode(value))

  def encodeFormula(formula: ModelFormula): Either[ModelJsonError, String] = checked:
    val text = admit(formula.renderEither, "$.value")(_.message).map(_.text).mkString
    envelope("formula", Str(text)).render()

  def decodeFormula(input: String): Either[ModelJsonError, ModelFormula] = checked:
    admit(FormulaParser.parseEither(payload(input, "formula").str), "$.value")(_.getMessage)

  def encodeDerivedColumns(columns: Vector[DerivedColumn]): Either[ModelJsonError, String] = checked:
    val declarations = columns.map: column =>
      val text = column.text
      val restored = admit(DerivedColumn.parse(text), "$.value")(_.message)
      if restored != column then fail("$.value", "derived declaration is not losslessly printable")
      Str(text)
    envelope("derived-columns", Arr.from(declarations)).render()

  def decodeDerivedColumns(input: String): Either[ModelJsonError, Vector[DerivedColumn]] = checked:
    payload(input, "derived-columns").arr.toVector.zipWithIndex.map: (value, index) =>
      admit(DerivedColumn.parse(value.str), s"$$.value[$index]")(_.message)

  def encodeRunCombination(combination: RunContrastCombination): Either[ModelJsonError, String] = checked:
    val value = combination match
      case RunContrastCombination.FixedEffects => Obj("type" -> Str("fixed-effects"))
      case RunContrastCombination.Concatenated(columns) =>
        if columns.isEmpty || columns.distinct.size != columns.size then fail("$.value.taskColumns", "provide distinct, nonempty task columns")
        Obj("type" -> Str("concatenated"), "taskColumns" -> Arr.from(columns.map(_.value)))
    envelope("run-combination", value).render()

  def decodeRunCombination(input: String): Either[ModelJsonError, RunContrastCombination] = checked:
    val value = payload(input, "run-combination")
    value("type").str match
      case "fixed-effects" =>
        fields(value, Set("type"), "$.value")
        RunContrastCombination.FixedEffects
      case "concatenated" =>
        fields(value, Set("type", "taskColumns"), "$.value")
        val columns = value("taskColumns").arr.toVector.map(v => admit(ColumnId(v.str), "$.value.taskColumns")(_.message))
        if columns.isEmpty || columns.distinct.size != columns.size then fail("$.value.taskColumns", "provide distinct, nonempty task columns")
        RunContrastCombination.Concatenated(columns)
      case other => fail("$.value.type", s"unknown run combination '$other'")

  def encodeHrf(spec: HrfSpec): Either[ModelJsonError, String] = checked:
    envelope("hrf", Obj(
      "kind" -> Str(spec.kind.canonicalName), "nbasis" -> Num(spec.nbasis),
      "span" -> Num(spec.span.value), "lag" -> Num(spec.lag.value), "width" -> Num(spec.width.value),
      "precision" -> Num(spec.precision.value), "summate" -> Bool(spec.summate),
      "normalize" -> Bool(spec.normalize), "normalization" -> Str(spec.normalization.label)
    )).render()

  def decodeHrf(input: String): Either[ModelJsonError, HrfSpec] = checked:
    val v = payload(input, "hrf")
    fields(v, Set("kind", "nbasis", "span", "lag", "width", "precision", "summate", "normalize", "normalization"), "$.value")
    def seconds(key: String): Seconds = admit(Seconds.fromDouble(finite(v(key), s"$$.value.$key"), key), s"$$.value.$key")(_.message)
    val normalization = admit(HrfNormalization.fromString(v("normalization").str), "$.value.normalization")(_.message)
    admit(HrfSpec.fromName(v("kind").str, integer(v("nbasis"), "$.value.nbasis"), seconds("span"), seconds("lag"),
      seconds("width"), seconds("precision"), v("summate").bool, v("normalize").bool, normalization), "$.value")(_.message)

  def encodeContrast(spec: ContrastSpec): Either[ModelJsonError, String] = checked:
    envelope("contrast", contrast(spec)).render()

  def decodeContrast(input: String): Either[ModelJsonError, ContrastSpec] = checked:
    readContrast(payload(input, "contrast"))

  private def selector(value: CellSelector): Value = value match
    case CellSelector.All => Obj("type" -> Str("all"))
    case CellSelector.Equals(f, l) => Obj("type" -> Str("equals"), "factor" -> Str(f.value), "level" -> Str(l.value))
    case CellSelector.NotEquals(f, l) => Obj("type" -> Str("not-equals"), "factor" -> Str(f.value), "level" -> Str(l.value))
    case CellSelector.In(f, levels) => Obj("type" -> Str("in"), "factor" -> Str(f.value), "levels" -> Arr.from(levels.toVector.map(_.value).sorted))
    case CellSelector.And(a, b) => Obj("type" -> Str("and"), "left" -> selector(a), "right" -> selector(b))
    case CellSelector.Or(a, b) => Obj("type" -> Str("or"), "left" -> selector(a), "right" -> selector(b))
    case CellSelector.Not(a) => Obj("type" -> Str("not"), "selector" -> selector(a))
    case CellSelector.Custom(_, _) => fail("$.value.selector", "custom callback has no portable JSON representation")

  private def readSelector(v: Value): CellSelector =
    def factor = admit(FactorId(v("factor").str), "$.value.selector.factor")(_.message)
    def level = admit(LevelId(v("level").str), "$.value.selector.level")(_.message)
    v("type").str match
      case "all" => fields(v, Set("type"), "$.value.selector"); CellSelector.All
      case "equals" => fields(v, Set("type", "factor", "level"), "$.value.selector"); CellSelector.Equals(factor, level)
      case "not-equals" => fields(v, Set("type", "factor", "level"), "$.value.selector"); CellSelector.NotEquals(factor, level)
      case "in" =>
        fields(v, Set("type", "factor", "levels"), "$.value.selector")
        CellSelector.In(factor, v("levels").arr.map(x => admit(LevelId(x.str), "$.value.selector.levels")(_.message)).toSet)
      case "and" => fields(v, Set("type", "left", "right"), "$.value.selector"); CellSelector.And(readSelector(v("left")), readSelector(v("right")))
      case "or" => fields(v, Set("type", "left", "right"), "$.value.selector"); CellSelector.Or(readSelector(v("left")), readSelector(v("right")))
      case "not" => fields(v, Set("type", "selector"), "$.value.selector"); CellSelector.Not(readSelector(v("selector")))
      case other => fail("$.value.selector.type", s"unknown selector $other")

  private def basis(value: BasisSelection): Value = value match
    case BasisSelection.All => Obj("type" -> Str("all"))
    case BasisSelection.Only(indices) => Obj("type" -> Str("only"), "indices" -> Arr.from(indices.map(_.oneBased)))
    case BasisSelection.WeightedAll(weights) => Obj("type" -> Str("weighted-all"), "weights" -> nums(weights))
    case BasisSelection.Weighted(indices, weights) => Obj("type" -> Str("weighted"), "indices" -> Arr.from(indices.map(_.oneBased)), "weights" -> nums(weights))

  private def readBasis(v: Value): BasisSelection =
    def indices = v("indices").arr.toVector.map(x => admit(BasisIndex.fromOneBased(integer(x, "$.value.basis.indices")), "$.value.basis.indices")(_.message))
    def weights = v("weights").arr.toVector.map(finite(_, "$.value.basis.weights"))
    v("type").str match
      case "all" => fields(v, Set("type"), "$.value.basis"); BasisSelection.All
      case "only" => fields(v, Set("type", "indices"), "$.value.basis"); BasisSelection.Only(indices)
      case "weighted-all" => fields(v, Set("type", "weights"), "$.value.basis"); BasisSelection.WeightedAll(weights)
      case "weighted" => fields(v, Set("type", "indices", "weights"), "$.value.basis"); BasisSelection.Weighted(indices, weights)
      case other => fail("$.value.basis.type", s"unknown basis selection $other")

  private def contrast(spec: ContrastSpec): Value = spec match
    case ContrastSpec.Typed(expr) => expr match
      case ContrastExpr.Pair(id, a, b, where, bs) => Obj("type" -> Str("pair"), "id" -> Str(id.value), "a" -> selector(a), "b" -> selector(b), "where" -> selector(where), "basis" -> basis(bs))
      case ContrastExpr.UnitContrast(id, where, bs) => Obj("type" -> Str("unit"), "id" -> Str(id.value), "where" -> selector(where), "basis" -> basis(bs))
      case ContrastExpr.Oneway(id, f, where, bs) => Obj("type" -> Str("oneway"), "id" -> Str(id.value), "factor" -> Str(f.value), "where" -> selector(where), "basis" -> basis(bs))
      case ContrastExpr.Interaction(id, f1, f2, where, bs) => Obj("type" -> Str("interaction"), "id" -> Str(id.value), "factor1" -> Str(f1.value), "factor2" -> Str(f2.value), "where" -> selector(where), "basis" -> basis(bs))
      case ContrastExpr.ColumnPattern(id, a, b) => Obj("type" -> Str("pattern"), "id" -> Str(id.value), "a" -> Str(a.regex), "b" -> optional(b)(r => Str(r.regex)))
    case ContrastSpec.Difference(name, left, right) => Obj("type" -> Str("difference"), "id" -> Str(name), "left" -> contrast(left), "right" -> contrast(right))
    case ContrastSpec.Mask(name, a, b, bs, weights) => Obj("type" -> Str("mask"), "id" -> Str(name), "a" -> Arr.from(a), "b" -> optional(b)(Arr.from(_)), "basis" -> optional(bs)(Arr.from(_)), "weights" -> optional(weights)(nums))
    case ContrastSpec.Column(name, a, b) => Obj("type" -> Str("legacy-pattern"), "id" -> Str(name), "a" -> Str(a.regex), "b" -> optional(b)(r => Str(r.regex)))
    case _ => fail("$.value", "legacy callback contrast has no portable JSON representation; use ContrastSpec.Typed")

  private def readContrast(v: Value): ContrastSpec =
    val id = admit(ContrastId(v("id").str), "$.value.id")(_.message)
    def factor(key: String) = admit(FactorId(v(key).str), s"$$.value.$key")(_.message)
    def pattern = v("a").str.r
    def patternB = readOptional(v("b"))(_.str.r)
    def check(names: String*): Unit = fields(v, names.toSet ++ Set("type", "id"), "$.value")
    v("type").str match
      case "pair" => check("a", "b", "where", "basis"); ContrastSpec.Typed(ContrastExpr.Pair(id, readSelector(v("a")), readSelector(v("b")), readSelector(v("where")), readBasis(v("basis"))))
      case "unit" => check("where", "basis"); ContrastSpec.Typed(ContrastExpr.UnitContrast(id, readSelector(v("where")), readBasis(v("basis"))))
      case "oneway" => check("factor", "where", "basis"); ContrastSpec.Typed(ContrastExpr.Oneway(id, factor("factor"), readSelector(v("where")), readBasis(v("basis"))))
      case "interaction" => check("factor1", "factor2", "where", "basis"); ContrastSpec.Typed(ContrastExpr.Interaction(id, factor("factor1"), factor("factor2"), readSelector(v("where")), readBasis(v("basis"))))
      case "pattern" => check("a", "b"); ContrastSpec.Typed(ContrastExpr.ColumnPattern(id, pattern, patternB))
      case "legacy-pattern" => check("a", "b"); ContrastSpec.Column(id.value, pattern, patternB)
      case "difference" => check("left", "right"); ContrastSpec.Difference(id.value, readContrast(v("left")), readContrast(v("right")))
      case "mask" =>
        check("a", "b", "basis", "weights")
        ContrastSpec.Mask(id.value, v("a").arr.map(_.bool).toVector,
          readOptional(v("b"))(_.arr.map(_.bool).toVector),
          readOptional(v("basis"))(_.arr.map(integer(_, "$.value.basis")).toVector),
          readOptional(v("weights"))(_.arr.map(finite(_, "$.value.weights")).toVector))
      case other => fail("$.value.type", s"unknown contrast $other")
