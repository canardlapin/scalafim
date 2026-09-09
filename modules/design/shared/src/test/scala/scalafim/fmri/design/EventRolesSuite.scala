package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.{ConvolvedTerm, EventModel}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

/** Explicit scientific roles do not depend on CSV numeric inference. */
class EventRolesSuite extends munit.FunSuite:
  private val frame = SamplingFrame(Seq(12), Seq(1.0), Seq(0.0))
  private val times = Vector(1.0, 5.0, 9.0)
  private def table(condition: Column = Column.Ints(Vector(1, 2, 1)),
      amplitude: Column = Column.Doubles(Vector(2.0, 3.0, -1.0))): DataTable =
    DataTable.fromColumns("onset" -> Column.Doubles(times), "condition" -> condition, "amplitude" -> amplitude)
  private val options = EventModelBuilder.BuildOptions(defaultHrf = Hrfs.fir(nBasis = 1, span = 2.s),
    precision = 0.1.s, missingValuePolicy = MissingValuePolicy.Reject)
  private def build(formula: String, data: DataTable = table(),
      settings: EventModelBuilder.BuildOptions = options): Either[DesignError, EventModel] =
    EventModelBuilder.EventDesignRequest.fromText(formula, data, frame, options = settings)
      .flatMap(EventModelBuilder.buildEither)
  private def checked(value: Either[DesignError, EventModel]): EventModel = value.fold(e => fail(e.message), identity)

  test("numeric condition codes remain distinct categorical cells with original identities"):
    val inferred = checked(build("onset ~ hrf(condition, id = task)"))
    val explicit = checked(build("onset ~ hrf(categorical(condition), id = task)"))
    assertEquals(inferred.designMatrix.cols, 1)
    assertEquals(explicit.designMatrix.cols, 2)
    val origins = explicit.designSchema.columns.map(_.origin)
    val levels = origins.collect { case StructuralColumnOrigin.Event(_, _, cell, None, _, _, _) =>
      cell.assignments.map(a => a.factor.value -> a.level.value) }
    assertEquals(levels, Vector(Vector("condition" -> "1"), Vector("condition" -> "2")))
    val sourceRows = explicit.terms.collect { case (_, term: ConvolvedTerm) => term.term.sourceRows }.flatten
    assertEquals(sourceRows, Vector(0, 1, 2))

  test("continuous amplitudes preserve raw values and FIR convolution without scaling"):
    val model = checked(build("onset ~ hrf(categorical(condition), continuous(amplitude), id = task)"))
    val amplitudes = Vector(2.0, 3.0, -1.0)
    val conditions = Vector("1", "2", "1")
    for (column, index) <- model.designSchema.columns.zipWithIndex do
      val (level, modulator) = column.origin match
        case StructuralColumnOrigin.Event(_, _, cell, modulator, _, _, _) => (cell.assignments.head.level.value, modulator)
        case other => fail(s"Unexpected origin $other")
      assertEquals(modulator.map(_.value), Some("amplitude"))
      for scan <- 0 until 12 do
        val expected = times.indices.filter(i => conditions(i) == level && scan >= times(i) && scan < times(i) + 2.0)
          .map(amplitudes).sum
        assertEqualsDouble(model.designMatrix(scan, index), expected, 1e-12)

  test("explicit categories respect declared level order and canonical numeric labels"):
    val registry = FactorLevelRegistry.of("condition" -> Seq("2", "1")).toOption.get
    val model = checked(build("onset ~ hrf(categorical(condition), id = task)",
      table(condition = Column.Doubles(Vector(1.0, 2.0, 1.0))), options.copy(factorLevels = registry)))
    val levels = model.designSchema.columns.collect { case StructuralColumn(_, _,
      StructuralColumnOrigin.Event(_, _, cell, _, _, _, _), _, _, _) => cell.assignments.head.level.value }
    assertEquals(levels, Vector("2", "1"))
    assert(build("onset ~ hrf(categorical(condition))", table(condition = Column.Ints(Vector(1, 3, 1))),
      options.copy(factorLevels = registry)).isLeft)

  test("roles reject wrong types, malformed arity and nonfinite numeric categories"):
    assert(build("onset ~ hrf(continuous(condition))", table(condition = Column.Strings(Vector("a", "b", "a")))).isLeft)
    for expression <- Vector("categorical()", "categorical(condition, amplitude)", "continuous(amplitude, condition)",
      "categorical(missing)", "continuous(missing)") do
      assert(build(s"onset ~ hrf($expression)").isLeft, expression)
    assert(build("onset ~ hrf(categorical(condition))", table(condition = Column.Doubles(Vector(1.0, Double.NaN, 2.0)))).isLeft)
    assert(build("onset ~ hrf(continuous(amplitude))", table(amplitude = Column.Doubles(Vector(1.0, Double.NaN, 2.0)))).isLeft)

  test("continuous roles compose with additive modulators and preserve scientific ids"):
    val model = checked(build("onset ~ hrf(categorical(condition), modulators(continuous(amplitude)), id = slopes)"))
    val ids = model.designSchema.columns.collect { case StructuralColumn(_, _,
      StructuralColumnOrigin.Event(term, _, _, Some(modulator), _, _, _), _, _, _) => term.value -> modulator.value }
    assertEquals(ids, Vector("slopes" -> "amplitude", "slopes" -> "amplitude"))
