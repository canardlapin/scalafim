package scalafim.fmri.laws.profile

import gale.linalg.DMat
import scalafim.dataset.{DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.{Event, EventModel, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis, KernelBasisSpec}
import scalafim.fmri.fit.profile.{ConditionProfileFit, ObservedFamilyCertification, ObservedFamilyRequirements}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.GaussianFamily
import scalafim.fmri.model.{FitPlan, FmriModel}
import scalafim.image.SampleSpaces

class ObservedFamilyAdmissionSuite extends munit.FunSuite:
  private val family = GaussianFamily.Default
  private val step = PositiveSeconds(0.1).fold(e => fail(e.message), identity)
  private val frame = SamplingFrame(blockLens = Seq(80), tr = Seq(1.0))
  private val precision = Seconds(0.1)
  private val term = EventTerm(
    Vector(Event.factor(Vector("A", "B", "A", "B"), "condition")),
    Vector(2.0, 12.0, 28.0, 45.0).map(Seconds(_)),
    blockIds = Vector.fill(4)(0)
  )
  private val basis = HrfKernelBasis
    .compile(KernelBasisSpec(family, step, Vector(18, 15), 1e-4, 32))
    .fold(e => fail(e.message), identity)
  private val point = Vector(family.chart.point(5.0, math.log(1.4)).fold(e => fail(e.message), identity))
  private val requirements = ObservedFamilyRequirements(1e-2, 1e8, 1e-6)

  private def lower(source: EventTerm): ExpandedConditionDesign =
    ExpandedConditionDesign.lower(source, frame, basis, precision).fold(e => fail(e.message), identity)

  private def bothAdmissions(
      expanded: ExpandedConditionDesign,
      source: EventTerm,
      actualFrame: SamplingFrame = frame,
      actualPrecision: Seconds = precision
  ): Vector[Boolean] =
    val dataset = FmriDataset.unsafe(
      InMemoryDatasetBackend(
        DatasetId("admission-control"),
        DMat.tabulate(80, 1)((_, _) => 0.0),
        SampleSpaces(Vector(1, 1, 1))
      ),
      frame
    )
    val model = FmriModel(
      EventModel.build(Vector(expanded.term), frame),
      BaselineModel.build(samplingFrame = frame, basis = BaselineBasis.Constant, intercept = Intercept.Global),
      dataset
    )
    val plan = FitPlan(model)
    val structure = ConditionProfileFit.structureFor(plan, expanded.term).fold(e => fail(e.message), identity)
    val taskNames = expanded.term.columnNames.toSet
    val nuisanceIndices =
      plan.model.columnNames.indices.filterNot(i => taskNames.contains(plan.model.columnNames(i))).toVector
    val nuisance =
      DMat.tabulate(80, nuisanceIndices.length)((row, col) => plan.model.designMatrix(row, nuisanceIndices(col)))
    Vector(
      ObservedFamilyCertification
        .admitForCompact(expanded, source, actualFrame, actualPrecision, None, Some(nuisance), point, requirements)
        .isRight,
      ObservedFamilyCertification
        .admitForCondition(
          plan,
          structure,
          expanded,
          source,
          actualFrame,
          actualPrecision,
          None,
          Some(nuisance),
          point,
          requirements
        )
        .isRight
    )

  test("both public admissions accept exactly matched condition geometry"):
    assertEquals(bothAdmissions(lower(term), term), Vector(true, true))

  test("both public admissions refuse a condition-label permutation preserving column space"):
    val swapped = term.copy(events = Vector(Event.factor(Vector("B", "A", "B", "A"), "condition")))
    assertEquals(bothAdmissions(lower(term), swapped), Vector(false, false))

  test("both public admissions refuse changed continuous weights with the same source form"):
    val weighted =
      EventTerm(Vector(Event.variable(Vector(1.0, 1.0, 1.0, 1.0), "weight")), term.onsets, blockIds = term.blockIds0)
    val altered = weighted.copy(events = Vector(Event.variable(Vector(1.0, 2.0, 1.0, 2.0), "weight")))
    val expanded = lower(weighted)
    assertEquals(bothAdmissions(expanded, weighted), Vector(true, true))
    assertEquals(bothAdmissions(expanded, altered), Vector(false, false))

  test("both public admissions refuse changed run frame and convolution precision"):
    val expanded = lower(term)
    val changedFrame = SamplingFrame(blockLens = Seq(40, 40), tr = Seq(1.0, 1.0))
    assertEquals(bothAdmissions(expanded, term, changedFrame), Vector(false, false))
    assertEquals(bothAdmissions(expanded, term, actualPrecision = Seconds(0.2)), Vector(false, false))
