package scalafim.fmri.design

import scalafim.fmri.design.fixtures.ObservedModulatorGridFixture

class ObservedModulatorSuite extends munit.FunSuite:
  private def cell(level: String): CellKey =
    CellKey.unsafe(Vector(CellAssignment(FactorId.unsafe("condition"), scalafim.fmri.design.contrast.LevelId.unsafe(level))))
  private val cells = Vector(cell("a"), cell("a"), cell("b"), cell("b"), cell("b"))
  private val values = Vector(1.0, 3.0, 10.0, 14.0, Double.NaN)

  test("by-cell centering uses observed values and one pooled sample scale") {
    val result = ObservedModulator.prepare(values, cells, ObservedModulator.Centering.ByCell,
      ObservedModulator.Scaling.ZScore, MissingValuePolicy.ZeroContribution).toOption.get
    assertEquals(result.retainedIndices, Vector(0, 1, 2, 3, 4))
    assertEquals(result.receipt.observedIndices, Vector(0, 1, 2, 3))
    assertEqualsDouble(result.receipt.observedMean.getOrElse(fail("missing mean")), 0.0, 1e-12)
    assertEqualsDouble(result.receipt.observedSampleStandardDeviation.getOrElse(fail("missing sd")), math.sqrt(10.0 / 3.0), 1e-12)
    assertEqualsDouble(result.receipt.scale, math.sqrt(10.0 / 3.0), 1e-12)
    assert(!result.receipt.degenerateScale)
    assertEquals(result.receipt.groups.map(_.observedIndices), Vector(Vector(0, 1), Vector(2, 3)))
    assertEqualsDouble(result.values(0), -1.0 / math.sqrt(10.0 / 3.0), 1e-12)
    assertEqualsDouble(result.values(3), 2.0 / math.sqrt(10.0 / 3.0), 1e-12)
    assertEqualsDouble(result.values(4), 0.0, 0.0)
  }

  test("drop and zero retain their distinct source-row contracts") {
    val dropped = ObservedModulator.prepare(values, cells, ObservedModulator.Centering.Global,
      ObservedModulator.Scaling.Raw, MissingValuePolicy.DropFromTerm).toOption.get
    assertEquals(dropped.retainedIndices, Vector(0, 1, 2, 3))
    assertEquals(dropped.values.size, 4)
    val zeroed = ObservedModulator.prepare(values, cells, ObservedModulator.Centering.Global,
      ObservedModulator.Scaling.Raw, MissingValuePolicy.ZeroContribution).toOption.get
    assertEquals(zeroed.retainedIndices, Vector(0, 1, 2, 3, 4))
    assertEqualsDouble(zeroed.values.last, 0.0, 0.0)
    assertEqualsDouble(zeroed.receipt.observedMean.getOrElse(fail("missing mean")), 0.0, 1e-12)
  }

  test("zero policy needs centering unless z-score supplies global centering") {
    ObservedModulator.prepare(values, cells, ObservedModulator.Centering.None,
      ObservedModulator.Scaling.Raw, MissingValuePolicy.ZeroContribution) match
      case Left(ObservedModulator.Error.ZeroRequiresCentering) => ()
      case other => fail(s"expected centering error, got $other")
    val z = ObservedModulator.prepare(values, cells, ObservedModulator.Centering.None,
      ObservedModulator.Scaling.ZScore, MissingValuePolicy.ZeroContribution).toOption.get
    assertEqualsDouble(z.receipt.observedMean.getOrElse(fail("missing mean")), 0.0, 1e-12)
    assertEqualsDouble(z.receipt.observedSampleStandardDeviation.getOrElse(fail("missing sd")), math.sqrt(110.0 / 3.0), 1e-12)
  }

  test("reject, unsupported imputation, and degenerate scales are explicit") {
    ObservedModulator.prepare(values, cells, ObservedModulator.Centering.Global,
      ObservedModulator.Scaling.Raw, MissingValuePolicy.Reject) match
      case Left(ObservedModulator.Error.MissingValues(Vector(4))) => ()
      case other => fail(s"expected missing error, got $other")
    ObservedModulator.prepare(Vector(1.0), Vector(cells.head), ObservedModulator.Centering.Global,
      ObservedModulator.Scaling.Raw, MissingValuePolicy.ImputeConstant(0.0)) match
      case Left(ObservedModulator.Error.UnsupportedMissingPolicy(MissingValuePolicy.ImputeConstant(0.0))) => ()
      case other => fail(s"expected unsupported imputation, got $other")
    val constant = ObservedModulator.prepare(Vector(4.0, 4.0), Vector(cells.head, cells.head),
      ObservedModulator.Centering.None, ObservedModulator.Scaling.StandardDeviation, MissingValuePolicy.Reject).toOption.get
    assert(constant.receipt.degenerateScale)
    assertEquals(constant.receipt.scale, 1.0)
    assertEquals(constant.values, Vector(4.0, 4.0))
  }

  test("finite inputs that overflow aggregate receipts fail explicitly") {
    ObservedModulator.prepare(Vector(Double.MaxValue, Double.MaxValue), Vector(cells.head, cells.head),
      ObservedModulator.Centering.None, ObservedModulator.Scaling.Raw, MissingValuePolicy.Reject) match
      case Left(ObservedModulator.Error.NonFiniteStatistic("observed mean")) => ()
      case other => fail(s"expected non-finite aggregate error, got $other")
  }

  test("frozen grid receipts retain their source provenance and expected run summaries") {
    assertEquals(ObservedModulatorGridFixture.dropCellRawRun1, ObservedModulatorGridFixture.Receipt(104, 0.0, 0.102))
    assertEquals(ObservedModulatorGridFixture.dropCellZRun1, ObservedModulatorGridFixture.Receipt(104, 0.0, 1.0))
    assertEquals(ObservedModulatorGridFixture.zeroCellRawRun1, ObservedModulatorGridFixture.Receipt(127, 0.0, 0.102))
    assertEquals(ObservedModulatorGridFixture.zeroCellZRun1, ObservedModulatorGridFixture.Receipt(127, 0.0, 1.0))
    assertEquals(ObservedModulatorGridFixture.dropNoneStandardDeviationRun1, ObservedModulatorGridFixture.Receipt(104, 5.0444, 1.0))
  }

  test("all frozen stop-modulator grid states have independently recomputed partition summaries") {
    import ObservedModulator.{Centering, Scaling}
    val policies = Vector(MissingValuePolicy.Reject, MissingValuePolicy.DropFromTerm, MissingValuePolicy.ZeroContribution)
    val scalings = Vector(Scaling.Raw, Scaling.ZScore, Scaling.StandardDeviation)
    val factorLayouts = Vector(
      Vector(Centering.ByCell, Centering.Global, Centering.None),
      Vector(Centering.Global, Centering.None)
    )
    var checked = 0
    factorLayouts.zipWithIndex.foreach { (centerings, layout) =>
      ObservedModulatorGridFixture.stopRuns.zipWithIndex.foreach { (source, run) =>
        val sourceCells = if layout == 0 then source.cells else Vector.fill(source.values.size)(CellKey.unsafe(Vector.empty))
        policies.foreach { policy =>
          centerings.foreach { centering =>
            scalings.foreach { scaling =>
              checked += 1
              val result = ObservedModulator.prepare(source.values, sourceCells, centering, scaling, policy)
              if policy == MissingValuePolicy.Reject then assert(result.isLeft)
              else if policy == MissingValuePolicy.ZeroContribution && centering == Centering.None && scaling != Scaling.ZScore then
                assertEquals(result, Left(ObservedModulator.Error.ZeroRequiresCentering))
              else
                val actual = result.toOption.get
                val expectedEvents = if policy == MissingValuePolicy.DropFromTerm then Vector(104, 113)(run) else Vector(127, 125)(run)
                assertEquals(actual.retainedIndices.size, expectedEvents)
                val expectedMean =
                  if centering == Centering.None && scaling == Scaling.Raw then Vector(.5149, .5238)(run)
                  else if centering == Centering.None && scaling == Scaling.StandardDeviation then Vector(5.0444, 5.2368)(run)
                  else 0.0
                val expectedSd = if scaling == Scaling.Raw then Vector(.102, .1)(run) else 1.0
                val observedIndices = actual.receipt.observedIndices.toSet
                val observed = actual.retainedIndices.zip(actual.values).collect {
                  case (index, value) if observedIndices.contains(index) => value
                }
                val mean = observed.sum / observed.size
                val sd = math.sqrt(observed.map(value => (value - mean) * (value - mean)).sum / (observed.size - 1))
                assertEqualsDouble(mean, expectedMean, 0.00006)
                assertEqualsDouble(sd, expectedSd, 0.0006)
                if policy == MissingValuePolicy.ZeroContribution then
                  assertEquals(actual.values.count(_ == 0.0) >= Vector(23, 12)(run), true)
            }
          }
        }
      }
    }
    assertEquals(checked, 90)
  }
