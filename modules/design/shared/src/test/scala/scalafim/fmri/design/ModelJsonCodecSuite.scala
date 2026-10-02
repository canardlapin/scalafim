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
