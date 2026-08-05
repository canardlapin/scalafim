package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.{ContinuousEvent, EventModel}
import scalafim.fmri.design.fixtures.ModulatorPolicyRFixture
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.design.SamplingFrame

/** Public policy contracts for additive parametric-modulator families. */
class ModulatorPoliciesSuite extends munit.FunSuite:

  test("checked policy refinements hide identifier proof plumbing") {
    val ordered = ModulatorOrthogonalization.ordered("slopes", "x", "y")
      .fold(error => fail(error.message), identity)
    val scoped = ordered.withinCells("group").fold(error => fail(error.message), identity)

    assertEquals(scoped.scope.canonical, "within-cells:group")
    assertEquals(scoped.rejectDegenerate.degenerate, DegenerateModulatorPolicy.Reject)
    assert(ordered.withinCells("group", "group").isLeft)
    assert(ordered.withTolerance(Double.NaN).isLeft)
  }

  test("modulators lower to sibling columns while ordinary arguments remain an interaction") {
    val data = table(
      onsets = Vector(1.0, 5.0, 9.0),
      columns = Vector(
        "x" -> Column.Doubles(Vector(1.0, 2.0, 3.0)),
        "y" -> Column.Doubles(Vector(4.0, 5.0, 6.0))
      )
    )
    val family = build("onset ~ hrf(modulators(x, y), id = family)", data)
    val centeredFamily = build("onset ~ hrf(modulators(center(x), y), id = centeredFamily)", data)
    val interaction = build("onset ~ hrf(x, y, id = interaction)", data)

    assertEquals(family.designMatrix.cols, 2)
    assertEquals(interaction.designMatrix.cols, 1)
    assertEquals(eventModulators(family), Vector("x", "y"))
    assertEquals(eventModulators(interaction), Vector("x+y"))
    assertVectorClose(column(modulatorEvent(centeredFamily), 0), Vector(-1.0, 0.0, 1.0), 1e-12)
    assertEquals(centeredFamily.designSchema.audit.centeringReceipts.map(_.modulator.value), Vector("x"))
  }

  test("whole-term ordered residualization agrees with the checked base-R receipt") {
    val data = table(
      onsets = Vector(1.0, 5.0, 9.0, 13.0, 17.0, 21.0),
      columns = Vector(
        "x" -> Column.Doubles(ModulatorPolicyRFixture.wholeX),
        "y" -> Column.Doubles(ModulatorPolicyRFixture.wholeY)
      )
    )
    val model = build(
      formula = "onset ~ hrf(modulators(x, y), id = modulators)",
      data = data,
      options = policyOptions(scope = OrthogonalizationScope.WholeTerm)
    )
    val event = modulatorEvent(model)

    assertVectorClose(column(event, 0), ModulatorPolicyRFixture.wholeX, 1e-12)
    assertVectorClose(column(event, 1), ModulatorPolicyRFixture.wholeResidual, 1e-12)
    assertEquals(event.modulatorIds.map(_.value), Vector("x", "y"))
    assertEquals(eventModulators(model), Vector("x", "y"))
    assert(
      ModulatorPolicyRFixture.acceptedDifferences.exists(_.contains("does not implicitly apply")) &&
        ModulatorPolicyRFixture.acceptedDifferences.exists(_.contains("MissingValuePolicy"))
    )
  }

  test("within-cell residualization is permutation invariant after event identity alignment") {
    val onsets = Vector(1.0, 5.0, 9.0, 13.0)
    val original = cellModel(Vector(0, 1, 2, 3), onsets)
    val permuted = cellModel(Vector(2, 0, 3, 1), onsets)
    val originalEvent = modulatorEvent(original)
    val permutedEvent = modulatorEvent(permuted)

    assertVectorClose(column(originalEvent, 1), ModulatorPolicyRFixture.cellResidual, 1e-12)
    val originalAligned = alignedResiduals(original, originalEvent)
    val permutedAligned = alignedResiduals(permuted, permutedEvent)
    assertEquals(originalAligned.map(_._1), permutedAligned.map(_._1))
    assertVectorClose(originalAligned.map(_._2), permutedAligned.map(_._2), 1e-12)
    assertVectorClose(original.designMatrix.data.toVector, permuted.designMatrix.data.toVector, 1e-12)

    val groups = original.designSchema.audit.orthogonalizationReceipts.head.steps.head.groups
    assertEquals(groups.map(_.key), Vector("cell:group=A", "cell:group=B"))
    assertEquals(groups.map(_.sourceRows), Vector(Vector(0, 1), Vector(2, 3)))
    assert(groups.forall(_.referenceRank == 1))
  }

  test("raw degeneration has typed outcomes, identities, and a total rejection path") {
    val values = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "trial" -> Column.Strings(Vector("trial-1", "trial-2", "trial-3")),
      "constant" -> Column.Doubles(Vector(2.0, 2.0, 2.0)),
      "zero" -> Column.Doubles(Vector(0.0, 0.0, 0.0)),
      "all_na" -> Column.Doubles(Vector(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity)),
      "partial" -> Column.Doubles(Vector(1.0, Double.NaN, 3.0))
    )
    val formula =
      "onset ~ hrf(constant, phase = probe, parent = trial, id = constant) + " +
        "hrf(zero, id = zero) + hrf(all_na, id = allNa) + hrf(partial, id = partial)"
    val retained = build(formula, values)
    val receipts = retained.designSchema.audit.degenerateModulatorReceipts
    val outcomes = receipts.map(receipt => receipt.modulator.value -> receipt.outcome).toMap

    assertEquals(outcomes("constant"), DegenerateModulatorOutcome.Constant)
    assertEquals(outcomes("zero"), DegenerateModulatorOutcome.AllZero)
    assertEquals(outcomes("all_na"), DegenerateModulatorOutcome.AllNonFinite)
    assert(!outcomes.contains("partial"))
    assert(
      retained.designSchema.audit.missingValues.exists { resolution =>
        resolution.modulator.value == "partial" && resolution.eventIndex == 1 && resolution.action == "zero-contribution"
      }
    )
    val constant = receipts.find(_.modulator.value == "constant").getOrElse(fail("missing constant receipt"))
    assertEquals(constant.sourceRows, Vector(0, 1, 2))
    assertEquals(constant.finiteSourceRows, Vector(0, 1, 2))
    assertEquals(constant.parentTrials.map(_.value), Vector("trial-1", "trial-2", "trial-3"))
    assert(retained.designSchema.fingerprint.canonicalEncoding.contains("degenerate-modulators="))

    buildEither(
      "onset ~ hrf(constant, id = constant)",
      values,
      EventModelBuilder.BuildOptions(degenerateModulatorPolicy = DegenerateModulatorPolicy.Reject)
    ) match
      case Left(DesignError.DegenerateModulator(term, modulator, scope, DegenerateModulatorPolicy.Reject)) =>
        assertEquals(term, "constant")
        assertEquals(modulator.value, "constant")
        assertEquals(scope, "constant")
      case other => fail(s"expected typed degenerate-modulator rejection, found $other")
  }

  test("an empty selected modulator scope has an explicit typed receipt") {
    val data = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0)),
      "x" -> Column.Doubles(Vector(1.0, 2.0)),
      "keep" -> Column.Bools(Vector(false, false))
    )
    val model = build("onset ~ hrf(x, subset = keep, id = empty)", data)
    val receipt = model.designSchema.audit.degenerateModulatorReceipts.head
    assertEquals(receipt.outcome, DegenerateModulatorOutcome.EmptyScope)
    assertEquals(receipt.sourceRows, Vector.empty)
    assertEquals(receipt.finiteSourceRows, Vector.empty)
  }

  private def cellModel(order: Vector[Int], onsets: Vector[Double]): EventModel =
    def reordered[A](values: Vector[A]): Vector[A] = order.map(values)
    val data = table(
      onsets = reordered(onsets),
      columns = Vector(
        "group" -> Column.Strings(reordered(ModulatorPolicyRFixture.cells)),
        "x" -> Column.Doubles(reordered(ModulatorPolicyRFixture.cellX)),
        "y" -> Column.Doubles(reordered(ModulatorPolicyRFixture.cellY))
      )
    )
    build(
      formula = "onset ~ hrf(group, modulators(x, y), id = slopes)",
      data = data,
      options = policyOptions(
        term = "slopes",
        scope = OrthogonalizationScope.WithinCells(Vector(FactorId.unsafe("group")))
      )
    )

  private def policyOptions(
      term: String = "modulators",
      scope: OrthogonalizationScope
  ): EventModelBuilder.BuildOptions =
    val policy = ModulatorOrthogonalization.ordered(
      term = TermId.unsafe(term),
      order = Vector(ModulatorId.unsafe("x"), ModulatorId.unsafe("y")),
      scope = scope
    ).fold(error => fail(error.message), identity)
    EventModelBuilder.BuildOptions(
      orthogonalization = ModulatorOrthogonalizationPlan.one(policy)
    )

  private def table(onsets: Vector[Double], columns: Vector[(String, Column)]): DataTable =
    val allColumns = ("onset" -> Column.Doubles(onsets)) +: columns
    DataTable.fromColumns(allColumns*)

  private def build(
      formula: String,
      data: DataTable,
      options: EventModelBuilder.BuildOptions = EventModelBuilder.BuildOptions()
  ): EventModel =
    buildEither(formula, data, options).fold(error => fail(error.message), identity)

  private def buildEither(
      formula: String,
      data: DataTable,
      options: EventModelBuilder.BuildOptions
  ): Either[DesignError, EventModel] =
    EventModelBuilder.EventDesignRequest.fromText(
      formula = formula,
      data = data,
      samplingFrame = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0)),
      options = options
    ).flatMap(EventModelBuilder.buildEither)

  private def modulatorEvent(model: EventModel): ContinuousEvent =
    model.terms.iterator.flatMap(_._2 match
      case term: scalafim.fmri.design.event.ConvolvedTerm => term.term.events
      case _ => Vector.empty
    ).collectFirst { case event: ContinuousEvent if event.modulatorIds.lengthCompare(2) >= 0 => event }
      .getOrElse(fail("model should retain a multi-column modulator family"))

  private def eventModulators(model: EventModel): Vector[String] =
    model.designSchema.columns.collect {
      case StructuralColumn(_, _, StructuralColumnOrigin.Event(_, _, _, Some(modulator), _, _, _), _, _, _) =>
        modulator.value
    }

  private def column(event: ContinuousEvent, columnIndex: Int): Vector[Double] =
    Vector.tabulate(event.value.rows)(row => event.value(row, columnIndex))

  private def alignedResiduals(model: EventModel, event: ContinuousEvent): Vector[(Double, Double)] =
    val term = model.terms.iterator.collectFirst {
      case (_, convolved: scalafim.fmri.design.event.ConvolvedTerm) if convolved.term.events.contains(event) =>
        convolved.term
    }.getOrElse(fail("modulator event should belong to a convolved term"))
    term.onsets.map(_.value).zip(column(event, 1)).sortBy(_._1)

  private def assertVectorClose(actual: Vector[Double], expected: Vector[Double], tolerance: Double): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).foreach { case (left, right) =>
      assertEqualsDouble(left, right, tolerance)
    }
