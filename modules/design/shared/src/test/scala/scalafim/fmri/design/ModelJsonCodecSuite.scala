package scalafim.fmri.design

import scalafim.fmri.design.formula.*
import scalafim.fmri.design.contrast.*
import scalafim.fmri.hrf.*

class ModelJsonCodecSuite extends munit.FunSuite:
  test("formula JSON round trip retains all argument types"):
    val formula = FormulaParser.parse("onset ~ hrf(a, modulators(center(rt), gain), basis = spmg3, subset = good & rt > 0, normalize = FALSE)")
    val json = ModelJsonCodec.encodeFormula(formula).toOption.get
    assertEquals(ModelJsonCodec.decodeFormula(json), Right(formula))

  test("HRF JSON round trip preserves physical parameters and normalization"):
    val spec = HrfSpec(HrfKind.Fir, nbasis = 8, span = 16.s, lag = (-2).s, width = 1.s, precision = 0.2.s).toOption.get
    val json = ModelJsonCodec.encodeHrf(spec).toOption.get
    assertEquals(ModelJsonCodec.decodeHrf(json), Right(spec))
    assert(ModelJsonCodec.decodeHrf(json.replace("\"nbasis\":8", "\"nbasis\":8.5")).isLeft)
    assert(ModelJsonCodec.decodeHrf(json.replace("\"span\":16", "\"span\":-16")).isLeft)

  test("typed contrast JSON preserves selectors and basis weighting"):
    val a = CellSelector.Equals(FactorId.unsafe("condition"), LevelId.unsafe("a"))
    val b = CellSelector.In(FactorId.unsafe("condition"), Set(LevelId.unsafe("b"), LevelId.unsafe("c")))
    val spec = ContrastSpec.Typed(ContrastExpr.Pair(ContrastId.unsafe("a-vs-bc"), a, b, CellSelector.Not(a), BasisSelection.WeightedAll(Vector(1.0, 2.0))))
    val json = ModelJsonCodec.encodeContrast(spec).toOption.get
    assertEquals(ModelJsonCodec.decodeContrast(json), Right(spec))
    assertEquals(ModelJsonCodec.encodeContrast(spec).toOption.get, json)

  test("difference and mask contrasts round trip"):
    val left = ContrastSpec.Mask("a", Vector(true, false), basis = Some(Vector(1)))
    val right = ContrastSpec.Mask("b", Vector(false, true), basisWeights = Some(Vector(0.5, 0.5)))
    val spec = ContrastSpec.Difference("a-b", left, right)
    assertEquals(ModelJsonCodec.decodeContrast(ModelJsonCodec.encodeContrast(spec).toOption.get), Right(spec))

  test("callbacks, unknown versions and unknown fields fail explicitly"):
    assert(ModelJsonCodec.encodeContrast(ContrastSpec.UnitContrast("runtime")).isLeft)
    val spec = ContrastSpec.Typed(ContrastExpr.UnitContrast(ContrastId.unsafe("custom"), CellSelector.Custom("x", _ => true)))
    assert(ModelJsonCodec.encodeContrast(spec).isLeft)
    val json = ModelJsonCodec.encodeFormula(FormulaParser.parse("onset ~ trialwise()")).toOption.get
    assert(ModelJsonCodec.decodeFormula(json.replace("\"version\":1", "\"version\":2")).isLeft)
    assert(ModelJsonCodec.decodeFormula(json.dropRight(1) + ",\"extra\":1}").isLeft)

  test("typed derived declarations and run-combination policy round trip"):
    val definitions = Vector("value: number = 2 + 3", "bin: text = cut(value, c(-Inf, 4, Inf), c(\"low\", \"high\"))").map(DerivedColumn.parse(_).toOption.get)
    assertEquals(ModelJsonCodec.encodeDerivedColumns(definitions).flatMap(ModelJsonCodec.decodeDerivedColumns), Right(definitions))
    Vector(RunContrastCombination.FixedEffects, RunContrastCombination.Concatenated(Vector(ColumnId.unsafe("task")))).foreach: policy =>
      assertEquals(ModelJsonCodec.encodeRunCombination(policy).flatMap(ModelJsonCodec.decodeRunCombination), Right(policy))
    assert(ModelJsonCodec.encodeRunCombination(RunContrastCombination.Concatenated(Vector.empty)).isLeft)

  private def errorOf[A](result: Either[ModelJsonError, A]): ModelJsonError =
    result.swap.toOption.getOrElse(fail(s"expected a JSON error, got $result"))

  test("encoded numbers are byte-identical across platforms"):
    val mask = ContrastSpec.Mask("m", Vector(true, false), basisWeights = Some(Vector(1.0, 0.1, 1e-7, 1e21, -0.0, 4.0)))
    val json = ModelJsonCodec.encodeContrast(mask).toOption.get
    assertEquals(json,
      """{"schema":"scalafim.model-spec","version":1,"kind":"contrast","value":{"type":"mask","id":"m","a":[true,false],"b":null,"basis":null,"weights":[1,0.1,1e-7,1e21,0,4]}}""")
    assertEquals(ModelJsonCodec.decodeContrast(json), Right(mask))
    val formula = ModelJsonCodec.encodeFormula(FormulaParser.parse("onset ~ hrf(cond, lag = 4.0, subset = rt > 1E-7)")).toOption.get
    assertEquals(formula,
      """{"schema":"scalafim.model-spec","version":1,"kind":"formula","value":"onset ~ hrf(cond, subset = (rt > 1e-7), lag = 4)"}""")
    assertEquals(ModelJsonCodec.encodeContrast(ContrastSpec.Mask("m", Vector(true), basisWeights = Some(Vector(Double.NaN)))).swap.toOption.map(_.path),
      Some("$.value.weights[0]"))

  test("structural JSON errors report the real path"):
    val json = ModelJsonCodec.encodeFormula(FormulaParser.parse("onset ~ trialwise()")).toOption.get
    assertEquals(errorOf(ModelJsonCodec.decodeFormula(json.replace(""""kind":"formula",""", ""))), ModelJsonError("$", "missing field(s) kind"))
    assertEquals(errorOf(ModelJsonCodec.decodeFormula(json.replace(""""version":1""", """"version":"1""""))).path, "$.version")
    assertEquals(errorOf(ModelJsonCodec.decodeFormula(json.replace(""""version":1""", """"version":1.5"""))).path, "$.version")
    assertEquals(errorOf(ModelJsonCodec.decodeFormula(json.replace(""""kind":"formula"""", """"kind":7"""))).path, "$.kind")
    assertEquals(errorOf(ModelJsonCodec.decodeFormula(json.dropRight(1) + ""","schema":"scalafim.model-spec"}""")),
      ModelJsonError("$", "duplicate key 'schema'"))
    assertEquals(errorOf(ModelJsonCodec.decodeFormula("{not json")).path, "$")

  test("nested contrast errors name the nested path"):
    val a = CellSelector.Equals(FactorId.unsafe("condition"), LevelId.unsafe("a"))
    val spec = ContrastSpec.Typed(ContrastExpr.Pair(ContrastId.unsafe("p"), a, CellSelector.Not(a), CellSelector.And(a, a)))
    val json = ModelJsonCodec.encodeContrast(spec).toOption.get
    assertEquals(errorOf(ModelJsonCodec.decodeContrast(json.replace(""""b":{"type":"not","selector":{"type":"equals",""", """"b":{"type":"not","selector":{"type":"equals","extra":1,"""))).path,
      "$.value.b.selector")
    assertEquals(errorOf(ModelJsonCodec.decodeContrast(json.replace(""""where":{"type":"and","left":{"type":"equals","factor":"condition",""", """"where":{"type":"and","left":{"type":"equals","factor":"condition","factor":"x","""))),
      ModelJsonError("$.value.where.left", "duplicate key 'factor'"))
    assertEquals(errorOf(ModelJsonCodec.decodeContrast(json.replace(""""level":"a"}}""", """"level":7}}"""))).path, "$.value.b.selector.level")

  test("ids are never trimmed: non-canonical or empty ids fail on both sides"):
    val padded = ContrastSpec.Mask(" a ", Vector(true))
    assertEquals(errorOf(ModelJsonCodec.encodeContrast(padded)).path, "$.value.id")
    assertEquals(errorOf(ModelJsonCodec.encodeContrast(ContrastSpec.Mask("", Vector(true)))).path, "$.value.id")
    assertEquals(errorOf(ModelJsonCodec.encodeContrast(ContrastSpec.Typed(ContrastExpr.UnitContrast(ContrastId.unsafe(" u"))))).path, "$.value.id")
    val json = ModelJsonCodec.encodeContrast(ContrastSpec.Mask("a", Vector(true))).toOption.get
    assertEquals(errorOf(ModelJsonCodec.decodeContrast(json.replace(""""id":"a"""", """"id":" a """"))).path, "$.value.id")
    val selector = ModelJsonCodec.encodeContrast(ContrastSpec.Typed(ContrastExpr.UnitContrast(ContrastId.unsafe("u"),
      CellSelector.Equals(FactorId.unsafe("f"), LevelId.unsafe("l"))))).toOption.get
    assertEquals(errorOf(ModelJsonCodec.decodeContrast(selector.replace(""""level":"l"""", """"level":"l """"))).path, "$.value.where.level")

  test("pattern contrasts round trip by pattern source"):
    val typed = ContrastSpec.Typed(ContrastExpr.ColumnPattern(ContrastId.unsafe("pat"), "cond\\[a\\].*".r, Some("^b$".r)))
    val legacy = ContrastSpec.Column("legacy", "x|y".r)
    val typedJson = ModelJsonCodec.encodeContrast(typed).toOption.get
    assertEquals(typedJson,
      """{"schema":"scalafim.model-spec","version":1,"kind":"contrast","value":{"type":"pattern","id":"pat","a":"cond\\[a\\].*","b":"^b$"}}""")
    ModelJsonCodec.decodeContrast(typedJson) match
      case Right(ContrastSpec.Typed(ContrastExpr.ColumnPattern(id, a, b))) =>
        assertEquals((id.value, a.regex, b.map(_.regex)), ("pat", "cond\\[a\\].*", Some("^b$")))
      case other => fail(s"expected a pattern contrast, got $other")
    val legacyJson = ModelJsonCodec.encodeContrast(legacy).toOption.get
    ModelJsonCodec.decodeContrast(legacyJson) match
      case Right(ContrastSpec.Column(name, a, None)) => assertEquals((name, a.regex), ("legacy", "x|y"))
      case other => fail(s"expected a legacy pattern contrast, got $other")
    assertEquals(ModelJsonCodec.decodeContrast(legacyJson).flatMap(ModelJsonCodec.encodeContrast), Right(legacyJson))
    assertEquals(errorOf(ModelJsonCodec.decodeContrast(typedJson.replace(""""a":"cond\\[a\\].*"""", """"a":"(""""))).path, "$.value.a")
