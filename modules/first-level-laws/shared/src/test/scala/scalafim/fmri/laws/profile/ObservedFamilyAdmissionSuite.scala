package scalafim.fmri.laws.profile

import scalafim.fmri.design.event.{Event, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis, KernelBasisSpec}
import scalafim.fmri.fit.profile.{ObservedFamilyCertification, ObservedFamilyRequirements}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.GaussianFamily

class ObservedFamilyAdmissionSuite extends munit.FunSuite:
  private val family = GaussianFamily.Default
  private val step = PositiveSeconds(0.1).fold(e => fail(e.message), identity)
  private val frame = SamplingFrame(blockLens = Seq(80), tr = Seq(1.0))
  private val term = EventTerm(Vector(Event.factor(Vector("A", "B", "A", "B"), "condition")), Vector(2.0, 12.0, 28.0, 45.0).map(Seconds(_)), blockIds = Vector.fill(4)(0))
  private val basis = HrfKernelBasis.compile(KernelBasisSpec(family, step, Vector(18, 15), 1e-4, 32)).fold(e => fail(e.message), identity)
  private val expanded = ExpandedConditionDesign.lower(term, frame, basis, Seconds(0.1)).fold(e => fail(e.message), identity)
  private val point = Vector(family.chart.point(5.0, math.log(1.4)).fold(e => fail(e.message), identity))
  private val requirements = ObservedFamilyRequirements(1e-2, 1e8, 1e-6)

  test("valid compact admission is bound to exact source, frame, and precision"):
    assert(ObservedFamilyCertification.admitForCompact(expanded, term, frame, Seconds(0.1), None, None, point, requirements).isRight)

  test("swapped condition assignment, changed event weight, frame, and precision refuse before certification"):
    val swapped = term.copy(events = Vector(Event.factor(Vector("B", "A", "B", "A"), "condition")))
    val weighted = EventTerm(Vector(Event.factor(Vector("A", "B", "A", "B"), "condition")), term.onsets.updated(1, Seconds(12.1)), blockIds = term.blockIds0)
    val changedFrame = SamplingFrame(blockLens = Seq(40, 40), tr = Seq(1.0, 1.0))
    assert(ObservedFamilyCertification.certify(expanded, swapped, frame, Seconds(0.1), None, None, point, 1e-6).isLeft)
    assert(ObservedFamilyCertification.certify(expanded, weighted, frame, Seconds(0.1), None, None, point, 1e-6).isLeft)
    assert(ObservedFamilyCertification.certify(expanded, term, changedFrame, Seconds(0.1), None, None, point, 1e-6).isLeft)
    assert(ObservedFamilyCertification.certify(expanded, term, frame, Seconds(0.2), None, None, point, 1e-6).isLeft)
