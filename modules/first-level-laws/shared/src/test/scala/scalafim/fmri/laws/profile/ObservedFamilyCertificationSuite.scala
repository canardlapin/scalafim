package scalafim.fmri.laws.profile

import gale.linalg.DMat
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.fmri.design.event.{Event, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis, KernelBasisSpec}
import scalafim.fmri.fit.profile.{ObservedFamilyCertification, ObservedFamilyError}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.GaussianFamily

class ObservedFamilyCertificationSuite extends munit.FunSuite:

  private val family = GaussianFamily.Default
  private val step = PositiveSeconds(0.1).fold(e => fail(e.message), identity)
  private val precision = Seconds(0.1)
  private val rows = 90
  private val frame = SamplingFrame(blockLens = Seq(rows), tr = Seq(1.0))
  private val onsets = Vector(1.3, 6.1, 10.6, 17.0, 23.4, 30.7, 36.2, 41.9, 47.5, 53.8, 60.3, 66.1, 71.0, 77.4).map(Seconds(_))
  private val conditions = Vector("A", "B", "C", "A", "B", "C", "A", "B", "C", "B", "A", "C", "A", "B")
  private val term = EventTerm(events = Vector(Event.factor(conditions, "cond")), onsets = onsets, blockIds = Vector.fill(14)(0), termTag = Some("cond"))
  private lazy val basis = HrfKernelBasis.compile(KernelBasisSpec(family, step, Vector(26, 21), tolerance = 1e-4, maxRank = 40)).fold(e => fail(e.message), identity)
  private lazy val expanded = ExpandedConditionDesign.lower(term, frame, basis, precision).fold(e => fail(e.message), identity)
  private val whitening = WhiteningPlan.global(ArmaCoefficients.ar(0.3), Vector(TimeSegment(0, rows, 0)))
  private val nuisance = DMat.tabulate(rows, 3)((t, j) => if j == 0 then 1.0 else if j == 1 then t.toDouble / rows else math.cos(math.Pi * (t + 0.5) / rows))
  private val heldOut = Vector((3.4, math.log(0.9)), (4.8, math.log(1.6)), (6.1, math.log(2.4)), (7.7, math.log(1.1)), (5.5, math.log(2.9)))
    .map { case (t, v) => family.chart.point(t, v).fold(e => fail(e.message), identity) }

  test("the observed family is certified under whitening and nuisance projection"):
    val certificate = ObservedFamilyCertification
      .certify(expanded, term, frame, precision, Some(whitening), Some(nuisance), heldOut, requiredSingularValue = 1e-3)
      .fold(e => fail(e.message), identity)
    assertEquals(certificate.points.length, heldOut.length)
    assertEquals(certificate.nuisanceRank, 3)
    assert(certificate.maxDesignError <= 5.0 * basis.certificate.valueError(basis.rank - 1), s"design error ${certificate.maxDesignError}")
    assert(certificate.maxProjectorError > 0.0)
    assert(certificate.maxProjectorError < 1e-2, s"projector error ${certificate.maxProjectorError}")
    assert(certificate.minSmallestSingularValue > 1e-3)
    assert(certificate.points.forall(p => p.conditionNumber >= 1.0))

  test("an aliased condition cell is refused as rank loss, not certified"):
    // Two conditions with identical onsets are exactly aliased.
    val aliased = EventTerm(
      events = Vector(Event.factor(Vector("A", "B", "A", "B", "A", "B"), "cond")),
      onsets = Vector(5.0, 5.0, 25.0, 25.0, 45.0, 45.0).map(Seconds(_)),
      blockIds = Vector.fill(6)(0),
      termTag = Some("cond")
    )
    val aliasedExpanded = ExpandedConditionDesign.lower(aliased, frame, basis, precision).fold(e => fail(e.message), identity)
    ObservedFamilyCertification.certify(aliasedExpanded, aliased, frame, precision, None, None, heldOut.take(1), requiredSingularValue = 1e-6) match
      case Left(ObservedFamilyError.RankLoss(_, s, _)) => assert(s < 1e-6, s"smallest singular value $s")
      case other => fail(s"expected RankLoss, got $other")
