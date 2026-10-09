package scalafim.fmri.design.formula

import scalafim.fmri.design.{BasisIndex, ColumnId, FactorId, RunContrastCombination}
import scalafim.fmri.design.contrast.*
import scalafim.fmri.hrf.*
import scala.util.control.NonFatal
import scala.util.matching.Regex
import ujson.{Arr, Bool, Num, Obj, Str, Value}
import PortableJson.{Cursor, Result, admit, ensure, fail, traverse}

final case class ModelJsonError(path: String, detail: String):
  def message: String = s"$path: $detail"

/** Versioned portable specifications. Runtime callbacks are deliberately rejected.
  *
  * Decoding is strict: every object must have exactly its documented fields,
  * duplicate keys are rejected, ids must already be canonical (an id with
  * leading or trailing whitespace is an error, not trimmed), and every error
  * carries the JSON path of the offending value. Encoding rejects anything the
  * decoder could not read back. Encoded text is byte-identical on the JVM and
  * Scala.js.
  *
  * Column-pattern contrasts carry `java.util.regex` pattern source. Scala.js
  * translates those patterns to JavaScript regular expressions, which do not
  * support every Java construct (for example possessive quantifiers or some
  * inline flags); such a pattern is rejected where it cannot compile. Decoded
  * patterns are fresh `Regex` instances, so compare decoded pattern contrasts
  * by pattern source (`regex`), not by `Regex` equality.
  */
object ModelJsonCodec:
  private def envelope(kind: String, value: Value): Value =
    Obj("schema" -> Str("scalafim.model-spec"), "version" -> Num(1), "kind" -> Str(kind), "value" -> value)

  private def payload(input: String, kind: String): Result[Cursor] =
    PortableJson.parse(input).flatMap(unwrap(_, kind))

  private def unwrap(root: Cursor, kind: String): Result[Cursor] =
    def at(name: String) = s"${root.path}.$name"
    for
      _ <- root.fields(Set("schema", "version", "kind", "value"))
      schema <- root.field("schema").flatMap(_.str)
      _ <- ensure(schema == "scalafim.model-spec", at("schema"), "unknown schema")
      version <- root.field("version").flatMap(_.integer)
      _ <- ensure(version == 1, at("version"), "unsupported version")
      actual <- root.field("kind").flatMap(_.str)
      _ <- ensure(actual == kind, at("kind"), s"expected $kind")
      value <- root.field("value")
    yield value

  private def write(kind: String, value: Result[Value]): Either[ModelJsonError, String] =
    value.flatMap(v => PortableJson.render(envelope(kind, v)))

  def encodeFormula(formula: ModelFormula): Either[ModelJsonError, String] =
    write("formula", formula.textEither.left.map(error => ModelJsonError("$.value", error.message)).map(Str(_)))

  def decodeFormula(input: String): Either[ModelJsonError, ModelFormula] =
    for
      value <- payload(input, "formula")
      text <- value.str
      formula <- admit(FormulaParser.parseEither(text), value.path)(_.getMessage)
    yield formula

  /** Declarations only; [[encodeDerivedEvents]] also carries the missing-row policy. */
  def encodeDerivedColumns(columns: Vector[DerivedColumn]): Either[ModelJsonError, String] =
    // DerivedColumn instances are printable by construction.
    write("derived-columns", Right(Arr.from(columns.map(column => Str(column.text)))))

  def decodeDerivedColumns(input: String): Either[ModelJsonError, Vector[DerivedColumn]] =
    for
      value <- payload(input, "derived-columns")
      items <- value.arr
      columns <- traverse(items)(item => item.str.flatMap(text => admit(DerivedColumn.parse(text), item.path)(_.message)))
    yield columns

  /** Declarations together with their missing-row policy, so a saved model
    * reproduces exactly which event rows a build drops.
    */
  def encodeDerivedEvents(plan: DerivedEventPlan): Either[ModelJsonError, String] =
    write("derived-events", Right(derivedEventsValue(plan)))

  def decodeDerivedEvents(input: String): Either[ModelJsonError, DerivedEventPlan] =
    payload(input, "derived-events").flatMap(readDerivedEvents)

  /** A derived-event plan as an embeddable JSON value (model document codec). */
  private[fmri] def derivedEventsValue(plan: DerivedEventPlan): Value =
    // DerivedColumn instances are printable by construction.
    Obj("missingRows" -> Str(plan.missingRows.label), "columns" -> Arr.from(plan.columns.map(column => Str(column.text))))

  private[fmri] def readDerivedEvents(value: Cursor): Result[DerivedEventPlan] =
    for
      _ <- value.fields(Set("missingRows", "columns"))
      policyCursor <- value.field("missingRows")
      policyName <- policyCursor.str
      policy <- DerivedMissingRows.fromLabel(policyName).toRight(ModelJsonError(policyCursor.path,
        s"unknown missing-row policy '$policyName' (expected one of ${DerivedMissingRows.values.map(_.label).mkString(", ")})"))
      list <- value.field("columns")
      items <- list.arr
      columns <- traverse(items)(item => item.str.flatMap(text => admit(DerivedColumn.parse(text), item.path)(_.message)))
      plan <- admit(DerivedEventPlan.from(columns, policy), list.path)(_.message)
    yield plan

  def encodeRunCombination(combination: RunContrastCombination): Either[ModelJsonError, String] =
    write("run-combination", runCombinationValue(combination, "$.value"))

  def decodeRunCombination(input: String): Either[ModelJsonError, RunContrastCombination] =
    payload(input, "run-combination").flatMap(readRunCombination)

  /** A run combination as an embeddable JSON value (model document codec). */
  private[fmri] def runCombinationValue(combination: RunContrastCombination, path: String): Result[Value] =
    combination match
      case RunContrastCombination.FixedEffects => Right(Obj("type" -> Str("fixed-effects")))
      case RunContrastCombination.Concatenated(columns) =>
        if columns.isEmpty || columns.distinct.size != columns.size then fail(s"$path.taskColumns", "provide distinct, nonempty task columns")
        else Right(Obj("type" -> Str("concatenated"), "taskColumns" -> Arr.from(columns.map(_.value))))

  private[fmri] def readRunCombination(value: Cursor): Result[RunContrastCombination] =
    for
      kind <- value.field("type").flatMap(_.str)
      result <- kind match
        case "fixed-effects" => value.fields(Set("type")).map(_ => RunContrastCombination.FixedEffects)
        case "concatenated" =>
          for
            _ <- value.fields(Set("type", "taskColumns"))
            list <- value.field("taskColumns")
            items <- list.arr
            columns <- traverse(items)(item => item.str.flatMap(text => canonical(text, item.path)(ColumnId(_))(_.message)(_.value)))
            _ <- ensure(columns.nonEmpty && columns.distinct.size == columns.size, list.path, "provide distinct, nonempty task columns")
          yield RunContrastCombination.Concatenated(columns)
        case other => fail(s"${value.path}.type", s"unknown run combination '$other'")
    yield result

  def encodeHrf(spec: HrfSpec): Either[ModelJsonError, String] =
    val value =
      for
        span <- PortableJson.number(spec.span.value, "$.value.span")
        lag <- PortableJson.number(spec.lag.value, "$.value.lag")
        width <- PortableJson.number(spec.width.value, "$.value.width")
        precision <- PortableJson.number(spec.precision.value, "$.value.precision")
      yield Obj(
        "kind" -> Str(spec.kind.canonicalName), "nbasis" -> Num(spec.nbasis),
        "span" -> span, "lag" -> lag, "width" -> width,
        "precision" -> precision, "summate" -> Bool(spec.summate),
        "normalize" -> Bool(spec.normalize), "normalization" -> Str(spec.normalization.label)
      )
    write("hrf", value)

  def decodeHrf(input: String): Either[ModelJsonError, HrfSpec] =
    def seconds(v: Cursor, key: String): Result[Seconds] =
      v.field(key).flatMap(c => c.finite.flatMap(n => admit(Seconds.fromDouble(n, key), c.path)(_.message)))
    for
      v <- payload(input, "hrf")
      _ <- v.fields(Set("kind", "nbasis", "span", "lag", "width", "precision", "summate", "normalize", "normalization"))
      kind <- v.field("kind").flatMap(_.str)
      nbasis <- v.field("nbasis").flatMap(_.integer)
      span <- seconds(v, "span")
      lag <- seconds(v, "lag")
      width <- seconds(v, "width")
      precision <- seconds(v, "precision")
      summate <- v.field("summate").flatMap(_.bool)
      normalize <- v.field("normalize").flatMap(_.bool)
      normalizationCursor <- v.field("normalization")
      normalizationName <- normalizationCursor.str
      normalization <- admit(HrfNormalization.fromString(normalizationName), normalizationCursor.path)(_.message)
      spec <- admit(HrfSpec.fromName(kind, nbasis, span, lag, width, precision, summate, normalize, normalization), v.path)(_.message)
    yield spec

  def encodeContrast(spec: ContrastSpec): Either[ModelJsonError, String] =
    write("contrast", contrastValue(spec, "$.value"))

  def decodeContrast(input: String): Either[ModelJsonError, ContrastSpec] =
    payload(input, "contrast").flatMap(readContrastValue)

  /** A versioned contrast envelope as an embeddable JSON value (model build codec). */
  private[fmri] def contrastEnvelope(spec: ContrastSpec, path: String): Result[Value] =
    contrastValue(spec, s"$path.value").map(envelope("contrast", _))

  /** Read an embedded contrast envelope, reporting paths relative to the outer document. */
  private[fmri] def readContrastEnvelope(cursor: Cursor): Result[ContrastSpec] =
    unwrap(cursor, "contrast").flatMap(readContrastValue)

  // ------------------------------------------------------------ ids and regex

  /** Decode an id and require that it is already canonical: the smart
    * constructors trim, and a trimmed id would not re-encode to the same text.
    */
  private def canonical[A, E](raw: String, path: String)(parse: String => Either[E, A])(message: E => String)(show: A => String): Result[A] =
    admit(parse(raw), path)(message).flatMap { id =>
      if show(id) == raw then Right(id) else fail(path, s"id '$raw' is not canonical (leading or trailing whitespace)")
    }

  private def contrastId(raw: String, path: String): Result[ContrastId] =
    canonical(raw, path)(ContrastId(_))(_.message)(_.value)

  private def factorId(raw: String, path: String): Result[FactorId] =
    canonical(raw, path)(FactorId(_))(_.message)(_.value)

  private def levelId(raw: String, path: String): Result[LevelId] =
    canonical(raw, path)(LevelId(_))(_.message)(_.value)

  /** Encoding side: ids built through `unsafe` (or legacy `String` names) must
    * still be readable by the decoder.
    */
  private def encodableId(raw: String, path: String): Result[Value] =
    contrastId(raw, path).map(_ => Str(raw))

  private def pattern(cursor: Cursor): Result[Regex] =
    cursor.str.flatMap { source =>
      try Right(Regex(source))
      catch case NonFatal(error) => fail(cursor.path, s"invalid pattern: ${Option(error.getMessage).getOrElse(source)}")
    }

  // ---------------------------------------------------------------- selectors

  private def selector(value: CellSelector, path: String): Result[Value] = value match
    case CellSelector.All => Right(Obj("type" -> Str("all")))
    case CellSelector.Equals(f, l) => Right(Obj("type" -> Str("equals"), "factor" -> Str(f.value), "level" -> Str(l.value)))
    case CellSelector.NotEquals(f, l) => Right(Obj("type" -> Str("not-equals"), "factor" -> Str(f.value), "level" -> Str(l.value)))
    case CellSelector.In(f, levels) => Right(Obj("type" -> Str("in"), "factor" -> Str(f.value), "levels" -> Arr.from(levels.toVector.map(_.value).sorted)))
    case CellSelector.And(a, b) =>
      for left <- selector(a, s"$path.left"); right <- selector(b, s"$path.right")
      yield Obj("type" -> Str("and"), "left" -> left, "right" -> right)
    case CellSelector.Or(a, b) =>
      for left <- selector(a, s"$path.left"); right <- selector(b, s"$path.right")
      yield Obj("type" -> Str("or"), "left" -> left, "right" -> right)
    case CellSelector.Not(a) => selector(a, s"$path.selector").map(inner => Obj("type" -> Str("not"), "selector" -> inner))
    case CellSelector.Custom(_, _) => fail(path, "custom callback has no portable JSON representation")

  private def readSelector(v: Cursor): Result[CellSelector] =
    def factor = v.field("factor").flatMap(c => c.str.flatMap(factorId(_, c.path)))
    def level = v.field("level").flatMap(c => c.str.flatMap(levelId(_, c.path)))
    def child(name: String) = v.field(name).flatMap(readSelector)
    v.field("type").flatMap(_.str).flatMap {
      case "all" => v.fields(Set("type")).map(_ => CellSelector.All)
      case "equals" => for _ <- v.fields(Set("type", "factor", "level")); f <- factor; l <- level yield CellSelector.Equals(f, l)
      case "not-equals" => for _ <- v.fields(Set("type", "factor", "level")); f <- factor; l <- level yield CellSelector.NotEquals(f, l)
      case "in" =>
        for
          _ <- v.fields(Set("type", "factor", "levels"))
          f <- factor
          items <- v.field("levels").flatMap(_.arr)
          levels <- traverse(items)(item => item.str.flatMap(levelId(_, item.path)))
        yield CellSelector.In(f, levels.toSet)
      case "and" => for _ <- v.fields(Set("type", "left", "right")); a <- child("left"); b <- child("right") yield CellSelector.And(a, b)
      case "or" => for _ <- v.fields(Set("type", "left", "right")); a <- child("left"); b <- child("right") yield CellSelector.Or(a, b)
      case "not" => for _ <- v.fields(Set("type", "selector")); a <- child("selector") yield CellSelector.Not(a)
      case other => fail(s"${v.path}.type", s"unknown selector $other")
    }

  // ------------------------------------------------------------ basis choices

  private def basis(value: BasisSelection, path: String): Result[Value] = value match
    case BasisSelection.All => Right(Obj("type" -> Str("all")))
    case BasisSelection.Only(indices) => Right(Obj("type" -> Str("only"), "indices" -> Arr.from(indices.map(_.oneBased))))
    case BasisSelection.WeightedAll(weights) =>
      PortableJson.numbers(weights, s"$path.weights").map(w => Obj("type" -> Str("weighted-all"), "weights" -> w))
    case BasisSelection.Weighted(indices, weights) =>
      PortableJson.numbers(weights, s"$path.weights").map(w => Obj("type" -> Str("weighted"), "indices" -> Arr.from(indices.map(_.oneBased)), "weights" -> w))

  private def readBasis(v: Cursor): Result[BasisSelection] =
    def indices = v.field("indices").flatMap(_.arr).flatMap(items =>
      traverse(items)(item => item.integer.flatMap(i => admit(BasisIndex.fromOneBased(i), item.path)(_.message))))
    def weights = v.field("weights").flatMap(_.arr).flatMap(items => traverse(items)(_.finite))
    v.field("type").flatMap(_.str).flatMap {
      case "all" => v.fields(Set("type")).map(_ => BasisSelection.All)
      case "only" => for _ <- v.fields(Set("type", "indices")); i <- indices yield BasisSelection.Only(i)
      case "weighted-all" => for _ <- v.fields(Set("type", "weights")); w <- weights yield BasisSelection.WeightedAll(w)
      case "weighted" => for _ <- v.fields(Set("type", "indices", "weights")); i <- indices; w <- weights yield BasisSelection.Weighted(i, w)
      case other => fail(s"${v.path}.type", s"unknown basis selection $other")
    }

  // ---------------------------------------------------------------- contrasts

  private def patternValue(regex: Option[Regex]): Value = regex.fold[Value](ujson.Null)(r => Str(r.regex))

  /** Encode one contrast as a JSON value; shared with the model build codec. */
  private[fmri] def contrastValue(spec: ContrastSpec, path: String): Result[Value] = spec match
    case ContrastSpec.Typed(expr) => expr match
      case ContrastExpr.Pair(id, a, b, where, bs) =>
        for
          i <- encodableId(id.value, s"$path.id")
          av <- selector(a, s"$path.a")
          bv <- selector(b, s"$path.b")
          wv <- selector(where, s"$path.where")
          basisValue <- basis(bs, s"$path.basis")
        yield Obj("type" -> Str("pair"), "id" -> i, "a" -> av, "b" -> bv, "where" -> wv, "basis" -> basisValue)
      case ContrastExpr.UnitContrast(id, where, bs) =>
        for i <- encodableId(id.value, s"$path.id"); wv <- selector(where, s"$path.where"); basisValue <- basis(bs, s"$path.basis")
        yield Obj("type" -> Str("unit"), "id" -> i, "where" -> wv, "basis" -> basisValue)
      case ContrastExpr.Oneway(id, f, where, bs) =>
        for i <- encodableId(id.value, s"$path.id"); wv <- selector(where, s"$path.where"); basisValue <- basis(bs, s"$path.basis")
        yield Obj("type" -> Str("oneway"), "id" -> i, "factor" -> Str(f.value), "where" -> wv, "basis" -> basisValue)
      case ContrastExpr.Interaction(id, f1, f2, where, bs) =>
        for i <- encodableId(id.value, s"$path.id"); wv <- selector(where, s"$path.where"); basisValue <- basis(bs, s"$path.basis")
        yield Obj("type" -> Str("interaction"), "id" -> i, "factor1" -> Str(f1.value), "factor2" -> Str(f2.value), "where" -> wv, "basis" -> basisValue)
      case ContrastExpr.ColumnPattern(id, a, b) =>
        encodableId(id.value, s"$path.id").map(i => Obj("type" -> Str("pattern"), "id" -> i, "a" -> Str(a.regex), "b" -> patternValue(b)))
    case ContrastSpec.Difference(name, left, right) =>
      for i <- encodableId(name, s"$path.id"); l <- contrastValue(left, s"$path.left"); r <- contrastValue(right, s"$path.right")
      yield Obj("type" -> Str("difference"), "id" -> i, "left" -> l, "right" -> r)
    case ContrastSpec.Mask(name, a, b, bs, weights) =>
      for
        i <- encodableId(name, s"$path.id")
        w <- PortableJson.optional(weights)(values => PortableJson.numbers(values, s"$path.weights"))
      yield Obj("type" -> Str("mask"), "id" -> i, "a" -> Arr.from(a), "b" -> b.fold[Value](ujson.Null)(Arr.from(_)),
        "basis" -> bs.fold[Value](ujson.Null)(values => Arr.from(values)), "weights" -> w)
    case ContrastSpec.Column(name, a, b) =>
      encodableId(name, s"$path.id").map(i => Obj("type" -> Str("legacy-pattern"), "id" -> i, "a" -> Str(a.regex), "b" -> patternValue(b)))
    case _ => fail(path, "legacy callback contrast has no portable JSON representation; use ContrastSpec.Typed")

  /** Decode one contrast JSON value; shared with the model build codec. */
  private[fmri] def readContrastValue(v: Cursor): Result[ContrastSpec] =
    def check(names: String*): Result[Unit] = v.fields(names.toSet ++ Set("type", "id"))
    def factor(key: String) = v.field(key).flatMap(c => c.str.flatMap(factorId(_, c.path)))
    def sel(key: String) = v.field(key).flatMap(readSelector)
    def basisChoice = v.field("basis").flatMap(readBasis)
    def patternA = v.field("a").flatMap(pattern)
    def patternB = v.field("b").flatMap(_.nullable(pattern))
    def flags(c: Cursor) = c.arr.flatMap(items => traverse(items)(_.bool))
    for
      kind <- v.field("type").flatMap(_.str)
      id <- v.field("id").flatMap(c => c.str.flatMap(contrastId(_, c.path)))
      spec <- kind match
        case "pair" =>
          for _ <- check("a", "b", "where", "basis"); a <- sel("a"); b <- sel("b"); w <- sel("where"); bs <- basisChoice
          yield ContrastSpec.Typed(ContrastExpr.Pair(id, a, b, w, bs))
        case "unit" =>
          for _ <- check("where", "basis"); w <- sel("where"); bs <- basisChoice
          yield ContrastSpec.Typed(ContrastExpr.UnitContrast(id, w, bs))
        case "oneway" =>
          for _ <- check("factor", "where", "basis"); f <- factor("factor"); w <- sel("where"); bs <- basisChoice
          yield ContrastSpec.Typed(ContrastExpr.Oneway(id, f, w, bs))
        case "interaction" =>
          for _ <- check("factor1", "factor2", "where", "basis"); f1 <- factor("factor1"); f2 <- factor("factor2"); w <- sel("where"); bs <- basisChoice
          yield ContrastSpec.Typed(ContrastExpr.Interaction(id, f1, f2, w, bs))
        case "pattern" =>
          for _ <- check("a", "b"); a <- patternA; b <- patternB
          yield ContrastSpec.Typed(ContrastExpr.ColumnPattern(id, a, b))
        case "legacy-pattern" =>
          for _ <- check("a", "b"); a <- patternA; b <- patternB
          yield ContrastSpec.Column(id.value, a, b)
        case "difference" =>
          for _ <- check("left", "right"); l <- v.field("left").flatMap(readContrastValue); r <- v.field("right").flatMap(readContrastValue)
          yield ContrastSpec.Difference(id.value, l, r)
        case "mask" =>
          for
            _ <- check("a", "b", "basis", "weights")
            a <- v.field("a").flatMap(flags)
            b <- v.field("b").flatMap(_.nullable(flags))
            bs <- v.field("basis").flatMap(_.nullable(_.arr.flatMap(items => traverse(items)(_.integer))))
            w <- v.field("weights").flatMap(_.nullable(_.arr.flatMap(items => traverse(items)(_.finite))))
          yield ContrastSpec.Mask(id.value, a, b, bs, w)
        case other => fail(s"${v.path}.type", s"unknown contrast $other")
    yield spec
