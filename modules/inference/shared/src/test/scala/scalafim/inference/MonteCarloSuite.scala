package scalafim.inference

class MonteCarloSuite extends munit.FunSuite:

  import InferenceRReferenceFixtures as R

  private def accepted[A](value: Either[InferenceError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def statistics(values: Vector[Double], offset: Int = 0): Vector[ReplicateStatistic] =
    values.zipWithIndex.map { case (value, index) =>
      accepted(ReplicateStatistic.from(accepted(ReplicateId(offset + index)), value))
    }

  private def alternative(value: String): Alternative =
    value match
      case "greater"   => Alternative.Greater
      case "less"      => Alternative.Less
      case "two_sided" => Alternative.TwoSided
      case other       => fail(s"unknown alternative $other")

  test("fixed Monte Carlo matches independent Phipson-Smyth fixtures") {
    R.fixedMonteCarlo.foreach { fixture =>
      val result = accepted(MonteCarlo.fixed(
        fixture.observed,
        alternative(fixture.alternative),
        statistics(fixture.nullValues)
      ))

      assertEquals(result.exceedances, fixture.exceedances)
      assertEquals(result.consumed.value, fixture.nullValues.length)
      assertEqualsDouble(result.pValue.value, fixture.pValue, 1e-15)
      assertEqualsDouble(result.standardError, fixture.mcSe, 1e-15)
    }
  }

  test("sequential Monte Carlo matches early-stop and full-budget fixtures") {
    R.sequentialMonteCarlo.foreach { fixture =>
      val result = accepted(MonteCarlo.sequential(
        fixture.observed,
        alternative(fixture.alternative),
        statistics(fixture.nullValues),
        accepted(MonteCarloDraws(fixture.maxDraws)),
        accepted(Alpha(fixture.alpha)),
        accepted(BatchSize(fixture.batchSchedule.head)),
        SequentialBoundary.Explicit(accepted(ExceedanceBoundary(fixture.boundary)))
      ))

      assertEquals(result.exceedances, fixture.exceedances)
      assertEquals(result.consumed.value, fixture.drawn)
      assertEquals(result.batchSchedule, fixture.batchSchedule)
      assertEqualsDouble(result.pValue.value, fixture.pValue, 1e-15)
      assertEqualsDouble(result.standardError, fixture.mcSe, 1e-15)
      assertEquals(
        result.stopReason,
        if fixture.stopReason == "non_reject_early" then MonteCarloStopReason.NonRejectionEarly
        else MonteCarloStopReason.Exhausted
      )
    }
  }

  test("replicate-id normalization makes collection order irrelevant") {
    val values = statistics(Vector(0.2, 1.2, 0.7, 1.5, 0.1, 2.0))
    val forward = accepted(MonteCarlo.sequential(
      1.0,
      Alternative.Greater,
      values,
      accepted(MonteCarloDraws(6)),
      accepted(Alpha(0.5)),
      accepted(BatchSize(2)),
      SequentialBoundary.Explicit(accepted(ExceedanceBoundary(2)))
    ))
    val reversed = accepted(MonteCarlo.sequential(
      1.0,
      Alternative.Greater,
      values.reverse,
      accepted(MonteCarloDraws(6)),
      accepted(Alpha(0.5)),
      accepted(BatchSize(2)),
      SequentialBoundary.Explicit(accepted(ExceedanceBoundary(2)))
    ))

    assertEquals(forward, reversed)
    assertEquals(forward.replicateIds.map(_.value), Vector(0, 1, 2, 3))
  }

  test("replicate streams split deterministically by id") {
    val seed = RootSeed(1729L)
    val zero = accepted(ReplicateId(0))
    val one = accepted(ReplicateId(1))
    val stream0a = RandomSource.forReplicate(seed, zero)
    val stream0b = RandomSource.forReplicate(seed, zero)
    val stream1 = RandomSource.forReplicate(seed, one)

    val (p0a, _) = accepted(stream0a.permutation(12))
    val (p0b, _) = accepted(stream0b.permutation(12))
    val (p1, _) = accepted(stream1.permutation(12))
    assertEquals(p0a, p0b)
    assertNotEquals(p0a, p1)
    assertEquals(p0a.sorted, Vector.range(0, 12))
    assertEquals(p1.sorted, Vector.range(0, 12))
  }

  test("replicate plans are disjoint across ladder steps") {
    val capacity = accepted(MonteCarloDraws(39))
    val draws = accepted(MonteCarloDraws(5))
    val first = accepted(ReplicatePlan.forStep(accepted(ComponentIx(0)), capacity, draws))
    val second = accepted(ReplicatePlan.forStep(accepted(ComponentIx(1)), capacity, draws))

    assertEquals(first.map(_.replicate.value), Vector(0, 1, 2, 3, 4))
    assertEquals(second.map(_.replicate.value), Vector(39, 40, 41, 42, 43))
    assertEquals(first.map(_.replicate).toSet.intersect(second.map(_.replicate).toSet), Set.empty)
  }

  test("budget state rolls unused draws forward and rejects grant reuse") {
    val initial = BudgetState.initial(accepted(MonteCarloDraws(10)))
    val first = accepted(initial.checkout(accepted(MonteCarloDraws(6))))
    val afterFirst = accepted(initial.record(first, consumed = 2))
    val second = accepted(afterFirst.checkout(accepted(MonteCarloDraws(9))))
    val afterSecond = accepted(afterFirst.record(second, consumed = 8))

    assertEquals(first.allocated.value, 6)
    assertEquals(second.allocated.value, 8)
    assertEquals(afterSecond.remaining, 0)
    assertEquals(afterSecond.schedule, Vector(2, 8))
    assert(afterSecond.record(first, consumed = 1).isLeft)
  }

  test("ordered ladder reducer matches complete PCA and PLSC fixture receipts") {
    R.ladders.foreach { fixture =>
      val capacity = accepted(MonteCarloDraws(39))
      val rungs = fixture.steps.zipWithIndex.map { case (step, index) =>
        accepted(LadderRung.fromValues(
          accepted(ComponentIx(index)),
          latentUnit(fixture.units(index)),
          step.observed,
          step.nullValues,
          capacity
        ))
      }
      val result = accepted(OrderedLadder.evaluate(
        rungs,
        capacity,
        accepted(MonteCarloDraws(117)),
        accepted(Alpha(0.1)),
        accepted(BatchSize(5))
      ))

      assertEquals(result.rejectedThrough, fixture.rejectedThrough)
      assertEquals(result.steps.length, fixture.lastStepTested)
      assertEquals(result.steps.map(_.selected), fixture.steps.map(_.selected))
      result.steps.zip(fixture.steps).foreach { case (actual, expected) =>
        actual.evidence match
          case Evidence.Computed(receipt) =>
            assertEquals(receipt.exceedances, expected.exceedances)
            assertEquals(receipt.consumed.value, expected.drawn)
            assertEquals(receipt.batchSchedule, expected.batchSchedule)
            assertEqualsDouble(receipt.pValue.value, expected.pValue, 1e-15)
          case other => fail(s"expected computed evidence, got $other")
      }
    }
  }

  test("ordered ladder represents global budget exhaustion explicitly") {
    val capacity = accepted(MonteCarloDraws(2))
    def rung(index: Int): LadderRung =
      val component = accepted(ComponentIx(index))
      accepted(LadderRung.fromValues(
        component,
        LatentUnit.Axis(accepted(UnitId(s"u${index + 1}")), component),
        observed = 5.0,
        values = Vector(0.0, 1.0),
        replicateCapacity = capacity
      ))

    val result = accepted(OrderedLadder.evaluate(
      Vector(rung(0), rung(1)),
      capacity,
      totalBudget = capacity,
      alpha = accepted(Alpha(0.5)),
      batchSize = accepted(BatchSize(1))
    ))

    assertEquals(result.rejectedThrough, 1)
    assertEquals(result.steps.length, 2)
    assertEquals(result.termination, LadderTermination.BudgetExhausted(accepted(UnitId("u2"))))
    result.steps(1).evidence match
      case Evidence.Unavailable(_) => ()
      case other                   => fail(s"expected unavailable evidence, got $other")
  }

  private def latentUnit(value: R.UnitExpectation): LatentUnit =
    val id = accepted(UnitId(value.id))
    val components = value.membersOneBased.map(index => accepted(ComponentIx(index - 1)))
    if components.length == 1 then LatentUnit.Axis(id, components.head)
    else
      val set = accepted(ComponentSet.from(components))
      LatentUnit.Subspace(id, accepted(SubspaceComponents.from(set)))
