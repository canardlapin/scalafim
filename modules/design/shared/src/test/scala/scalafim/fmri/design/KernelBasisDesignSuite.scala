package scalafim.fmri.design

import scalafim.fmri.design.event.{Event, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, ExpandedTrialDesign, HrfKernelBasis, KernelBasisDesignError, KernelBasisSpec, TrialMembership}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, JetLayout, NormalizationRule}

class KernelBasisDesignSuite extends munit.FunSuite:

  private val family = GaussianFamily.Default
  private val step = PositiveSeconds(0.1).fold(e => fail(e.message), identity)
  private lazy val basis =
    HrfKernelBasis.compile(KernelBasisSpec(family, step, Vector(26, 21), tolerance = 1e-4, maxRank = 32)).fold(e => fail(e.message), identity)
  private val precision = Seconds(0.1)
  private val frame = SamplingFrame(blockLens = Seq(60, 60), tr = Seq(1.0))
  private val conditions = Vector("A", "B", "C", "A", "B", "C", "A", "B", "C", "A", "B", "C")
  private val onsets = Vector(2.3, 7.1, 11.6, 19.0, 25.4, 33.7, 4.2, 9.9, 16.5, 22.8, 31.3, 44.1).map(Seconds(_))
  private val blockIds = Vector(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
  private val term = EventTerm(events = Vector(Event.factor(conditions, "cond")), onsets = onsets, blockIds = blockIds, termTag = Some("cond"))

  private def maxAbs(a: Array[Double], b: Array[Double]): Double =
    a.zip(b).map { case (x, y) => math.abs(x - y) }.max

  test("the expanded condition design contracts to the direct family design at a shape"):
    val expanded = ExpandedConditionDesign.lower(term, frame, basis, precision).fold(e => fail(e.message), identity)
    assertEquals(expanded.conditions, Vector("cond.A", "cond.B", "cond.C"))
    assertEquals(expanded.columns, 3 * basis.rank)
    assertEquals(expanded.rows, 120)
    val point = family.chart.point(5.7, math.log(1.4)).fold(e => fail(e.message), identity)
    val contracted = expanded.designAt(point)
    // Direct design: the library kernel divided by its density scale is the unnormalised family kernel.
    val scale = new Array[Double](family.jetComponents)
    family.scaleJetInto(family.libraryNormalization, point, scale)
    val direct = term.convolve(family.toHrf(point), frame, precision = precision)
    assertEquals(direct.data.cols, 3)
    val directUnnormalised = direct.data.data.map(_ / scale(JetLayout.Value))
    val norm = math.sqrt(directUnnormalised.map(x => x * x).sum)
    val err = math.sqrt(contracted.data.zip(directUnnormalised).map { case (a, b) => (a - b) * (a - b) }.sum) / norm
    assert(err <= 5.0 * basis.certificate.valueError(basis.rank - 1), s"relative design error $err vs certificate ${basis.certificate.valueError(basis.rank - 1)}")

  test("precision must match the basis grid"):
    ExpandedConditionDesign.lower(term, frame, basis, Seconds(0.3)) match
      case Left(KernelBasisDesignError.PrecisionMismatch(_, p)) => assertEqualsDouble(p, 0.3, 0.0)
      case other => fail(s"expected PrecisionMismatch, got $other")

  test("trial lowering aggregates to the condition design under one-hot membership"):
    val expanded = ExpandedConditionDesign.lower(term, frame, basis, precision).fold(e => fail(e.message), identity)
    val condIndex = Map("A" -> 0, "B" -> 1, "C" -> 2)
    val membership = TrialMembership.make(conditions.map(condIndex), 3).fold(e => fail(e.message), identity)
    val trials = ExpandedTrialDesign
      .lower(onsets, blockIds, Vector.fill(onsets.length)(Seconds(0.0)), membership, frame, basis, precision)
      .fold(e => fail(e.message), identity)
    assertEquals(trials.columns, 12 * basis.rank)
    val aggregated = trials.aggregateConditions
    assertEquals(aggregated.cols, expanded.columns)
    assert(maxAbs(aggregated.data, expanded.term.data.data) < 1e-12, "sum of trial blocks equals the condition block")
    assertEquals(membership.trialsOf(0), Vector(0, 3, 6, 9))

  test("membership validation"):
    assert(TrialMembership.make(Vector(0, 1, 1), 3).isLeft, "a condition without trials")
    assert(TrialMembership.make(Vector(0, 3), 3).isLeft, "index out of range")
    assert(TrialMembership.make(Vector.empty, 1).isLeft)
    assert(TrialMembership.make(Vector(0, 0), 1).isRight)
    assert(family.supports(NormalizationRule.Density))

  test("interleaved complete records retain trial columns, membership and reversible event-row identity"):
    val trialOnsets = Vector(3.0, 3.0, 12.0, 5.0, 14.0, 31.0).map(Seconds(_))
    val trialRuns = Vector(0, 0, 0, 1, 1, 1)
    val trialDurations = Vector(0.3, 0.5, 0.4, 0.1, 0.7, 0.2).map(Seconds(_))
    val members = Vector(0, 1, 1, 1, 2, 2)
    val order = Vector(3, 0, 4, 1, 5, 2)
    def lower(permutation: Vector[Int]): ExpandedTrialDesign =
      val membership = TrialMembership.make(permutation.map(members), 3).fold(e => fail(e.message), identity)
      ExpandedTrialDesign.lower(permutation.map(trialOnsets), permutation.map(trialRuns),
        permutation.map(trialDurations), membership, frame, basis, precision).fold(e => fail(e.message), identity)
    val original = lower(members.indices.toVector)
    val interleaved = lower(order)
    assertEquals(interleaved.membership.conditionOfTrial, order.map(members))
    assertEquals(interleaved.canonicalToInput, Vector(1, 3, 5, 0, 2, 4))
    order.indices.foreach: input =>
      assertEquals(interleaved.canonicalToInput(interleaved.inputToCanonical(input)), input)
    val source = interleaved.term.term
    assertEquals(source.blockIds, Vector(0, 0, 0, 1, 1, 1))
    assertEquals(source.onsets, interleaved.canonicalToInput.map(order.map(trialOnsets)))
    assertEquals(source.durations, interleaved.canonicalToInput.map(order.map(trialDurations)))
    var row = 0
    while row < original.rows do
      var input = 0
      while input < order.length do
        var component = 0
        while component < basis.rank do
          val actual = interleaved.term.data(row, interleaved.column(input, component))
          assertEqualsDouble(actual, original.term.data(row, original.column(order(input), component)), 1e-12)
          if row / 60 != trialRuns(order(input)) then assertEqualsDouble(actual, 0.0, 0.0)
          component += 1
        input += 1
      row += 1
    assert(maxAbs(interleaved.aggregateConditions.data, original.aggregateConditions.data) < 1e-12)

  test("trial lowering normalizes empty defaults and returns typed malformed-record failures"):
    val xs = Vector(2.0, 6.0, 10.0).map(Seconds(_))
    val membership = TrialMembership.make(Vector(0, 1, 1), 2).fold(e => fail(e.message), identity)
    def lower(runs: Vector[Int], lengths: Vector[Seconds]): Either[KernelBasisDesignError, ExpandedTrialDesign] =
      ExpandedTrialDesign.lower(xs, runs, lengths, membership, frame, basis, precision)
    val defaults = lower(Vector.empty, Vector.empty).fold(e => fail(e.message), identity)
    val explicit = lower(Vector.fill(3)(0), Vector.fill(3)(Seconds(0.0))).fold(e => fail(e.message), identity)
    assertEquals(defaults.canonicalToInput, Vector(0, 1, 2))
    assert(maxAbs(defaults.term.data.data, explicit.term.data.data) <= 1e-15)
    assert(lower(Vector(0, 1), Vector.empty).left.exists(_.message.contains("blockIds has length")))
    assert(lower(Vector.empty, Vector(Seconds(0.0))).left.exists(_.message.contains("durations has length")))
    assert(lower(Vector(1, -1, 0), Vector.empty).left.exists(_.message.contains("trial 2")))
    assert(lower(Vector(1, 2, 0), Vector.empty).left.exists(_.message.contains("trial 2")))
    assert(lower(Vector(1, 0, 0), Vector(Seconds(0.0), Seconds(-0.1), Seconds(0.0)))
      .left.exists(_.message.contains("trial 2 has negative duration")))

  test("declared trial levels retain numeric input order beyond four-digit labels without a large design"):
    val order = (0 until 10001).reverse.toVector
    val event = ExpandedTrialDesign.trialEvent(order)
    assertEquals(event.codes, order)
    assertEquals(event.levels(9998), "trial_9999")
    assertEquals(event.levels(9999), "trial_10000")
    assertEquals(event.levels(10000), "trial_10001")
    assertEquals(event.levels(event.codes.head), "trial_10001")
