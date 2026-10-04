package scalafim.fmri.fit.profile

import gale.linalg.DMat
import scalafim.fmri.design.hrf.{ExpandedTrialDesign, HrfKernelBasis, KernelBasisSpec, TrialMembership}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, ShapePoint}

class TrialBandedWorkReceiptSuite extends munit.FunSuite:

  private val family = GaussianFamily.Default
  private val step = PositiveSeconds(0.2).fold(error => fail(error.message), identity)
  private lazy val basis = HrfKernelBasis
    .compile(KernelBasisSpec(family, step, Vector(26, 21), tolerance = 1e-4, maxRank = 40))
    .fold(error => fail(error.message), identity)

  private final case class Fixture(
      expanded: ExpandedTrialDesign,
      nuisance: DMat,
      response: Array[Double],
      underflowProbe: ShapePoint)

  private def fixture(): Fixture =
    val rows = 120
    val membership = TrialMembership.make(Vector(0, 1, 1, 2, 2, 0), 3).fold(error => fail(error.message), identity)
    val expanded = ExpandedTrialDesign
      .lower(
        Vector(3.0, 10.0, 18.0, 5.0, 14.0, 31.0).map(Seconds(_)),
        Vector(0, 0, 0, 1, 1, 1),
        Vector.fill(6)(Seconds(0.0)),
        membership,
        SamplingFrame(blockLens = Seq(60, 60), tr = Seq(1.0, 1.0)),
        basis,
        Seconds(0.2))
      .fold(error => fail(error.message), identity)
    val target = ShapePoint.unsafe(Vector(5.3, math.log(1.55)))
    val targetDesign = designAt(expanded, target)
    val nuisance = DMat.tabulate(rows, 1): (row, _) =>
      var sum = 0.0
      var trial = 0
      while trial < expanded.trials do
        if membership.conditionOfTrial(trial) == 0 then sum += targetDesign(row * expanded.trials + trial)
        trial += 1
      sum
    Fixture(expanded, nuisance, Array.fill(rows)(0.0), ShapePoint.unsafe(Vector(-100.0, 0.0)))

  test("shared node setup is immutable while workers record their own response work"):
    val fx = fixture()
    val preparation = TrialBandedPreparation
      .prepare(fx.expanded, None, Some(fx.nuisance), lambda = 2.5)
      .fold(error => fail(error.message), identity)
    val objective = preparation.objective(NodeGrid(family.chart, Vector(2, 2))).fold(error => fail(error.message), identity)
    val setup = objective.setupReceipt
    assertEquals(setup.nodeReferenceAttempts, 4L)
    assertEquals(setup.work.referenceFailures, 0L)
    assertEquals(setup.work.factorFailures, 0L)
    assert(setup.work.factorAttempts > 0L)
    assert(setup.work.solveAttempts > 0L)

    val encoded = preparation.encodeWhitened(fx.response).fold(error => fail(error.message), identity)
    val first = objective.newWorker()
    val second = objective.newWorker()
    assertEquals(first.setupReceipt, setup)
    assertEquals(second.setupReceipt, setup)
    first.pointAt(encoded)
    second.pointAt(encoded)
    val firstOut = new ProfileJetBuffer(family.dimension, fx.expanded.membership.conditionCount)
    val secondOut = new ProfileJetBuffer(family.dimension, fx.expanded.membership.conditionCount)
    assert(first.jetAtNode(0, firstOut))
    assert(second.jetAtNode(0, secondOut))
    assertEquals(first.work.snapshot.attempted.referenceAttempts, 0L)
    assertEquals(second.work.snapshot.attempted.referenceAttempts, 0L)
    assertEquals(objective.setupReceipt, setup)

  test("outside-chart underflow records completed partial reference work"):
    val fx = fixture()
    val preparation = TrialBandedPreparation
      .prepare(fx.expanded, None, Some(fx.nuisance), lambda = 2.5)
      .fold(error => fail(error.message), identity)
    val objective = preparation.objective(NodeGrid(family.chart, Vector(2, 2))).fold(error => fail(error.message), identity)
    val setup = objective.setupReceipt
    val encoded = preparation.encodeWhitened(fx.response).fold(error => fail(error.message), identity)
    objective.pointAt(encoded)
    val out = new ProfileJetBuffer(family.dimension, fx.expanded.membership.conditionCount)

    assert(!objective.jetAt(fx.underflowProbe.coordinates.toArray, out))
    val work = objective.work.snapshot
    assertEquals(work.continuousFactors, 0L)
    assertEquals(work.jetEvaluations, 0L)
    assertEquals(work.bandedSolveCalls, 0L)
    assertEquals(work.bandedRightHandSides, 0L)
    assertEquals(work.attempted.jetAttempts, 1L)
    assertEquals(work.attempted.jetFailures, 1L)
    assertEquals(work.attempted.referenceAttempts, 1L)
    assertEquals(work.attempted.referenceFailures, 1L)
    assertEquals(work.attempted.partialReferenceFailures, 1L)
    assertEquals(work.attempted.releaseFailures, 1L)
    assertEquals(work.attempted.factorAttempts, 2L)
    assertEquals(work.attempted.factorFailures, 1L)
    assertEquals(work.attempted.solveAttempts, 6L)
    assertEquals(work.attempted.solveFailures, 0L)
    assertEquals(work.attempted.rightHandSideAttempts, 24L)
    assertEquals(objective.setupReceipt, setup)

    val rejected = objective.readoutInto(TrialReadoutFactorMode.ExactShape(fx.underflowProbe.coordinates), new Array[Double](fx.expanded.trials))
    assert(rejected.isLeft)
    val afterReadout = objective.work.snapshot
    assertEquals(afterReadout.exactReadoutFactors, 0L)
    assertEquals(afterReadout.attempted.readoutAttempts, 1L)
    assertEquals(afterReadout.attempted.readoutFailures, 1L)
    assertEquals(afterReadout.attempted.exactReadoutFactorAttempts, 1L)
    assertEquals(afterReadout.attempted.exactReadoutFactorFailures, 1L)
    assertEquals(afterReadout.attempted.referenceAttempts, 2L)
    assertEquals(afterReadout.attempted.referenceFailures, 2L)
    assertEquals(afterReadout.attempted.releaseFailures, 2L)
    assertEquals(afterReadout.attempted.factorAttempts, 4L)
    assertEquals(afterReadout.attempted.factorFailures, 2L)
    assertEquals(afterReadout.attempted.solveAttempts, 7L)
    assertEquals(afterReadout.attempted.rightHandSideAttempts, 28L)

  private def designAt(expanded: ExpandedTrialDesign, point: ShapePoint): Array[Double] =
    val coefficients = new Array[Double](family.jetComponents * expanded.rank)
    expanded.basis.coefficientJetInto(
      point,
      new Array[Double](family.jetComponents * expanded.basis.fineCount),
      coefficients,
      family.jetComponents)
    val out = new Array[Double](expanded.rows * expanded.trials)
    val source = expanded.term.data.data
    var row = 0
    while row < expanded.rows do
      var trial = 0
      while trial < expanded.trials do
        var sum = 0.0
        var component = 0
        while component < expanded.rank do
          sum += source(row * expanded.columns + component * expanded.trials + trial) * coefficients(component)
          component += 1
        out(row * expanded.trials + trial) = sum
        trial += 1
      row += 1
    out
