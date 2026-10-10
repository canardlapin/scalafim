package scalafim.fmri.fit.profile

import gale.linalg.DMat
import scalafim.fmri.design.hrf.{ExpandedTrialDesign, HrfKernelBasis, KernelBasisSpec, TrialMembership}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, ShapeChart}

class TrialReferenceBankSuite extends munit.FunSuite:
  private val family = GaussianFamily.Default
  private lazy val prep =
    val step = PositiveSeconds(0.2).toOption.get
    val basis = HrfKernelBasis.compile(KernelBasisSpec(family, step, Vector(26, 21), 1e-4, 40)).toOption.get
    val membership = TrialMembership.make(Vector(0, 1, 0, 1), 2).toOption.get
    val expanded = ExpandedTrialDesign.lower(Vector(3.0, 11.0, 22.0, 35.0).map(Seconds(_)),
      Vector.fill(4)(0), Vector.fill(4)(Seconds(0.0)), membership,
      SamplingFrame(blockLens = Seq(80), tr = Seq(1.0)), basis, Seconds(0.2)).toOption.get
    TrialBandedPreparation.prepare(expanded, None, Some(DMat.tabulate(80, 1)((_, _) => 1.0)), 2.0).toOption.get
  private def point(x: Double, y: Double): Vector[Double] = Vector(x, y).indices.map(i =>
    family.chart.lower(i) + Vector(x, y)(i) * family.chart.width(i)).toVector
  private val points = TrialReferencePoints(family.chart, Vector(point(0.3, 0.5), point(0.7, 0.5)))

  test("explicit points validate and route independently of a regular grid"):
    assertEquals(points.count, 2)
    assertEquals(points.nearest(points.coordinates(1)), Right(1))
    val chart = ShapeChart(("x", 0.0, 1.0))
    val symmetric = TrialReferencePoints(chart, Vector(Vector(0.25), Vector(0.75)))
    assertEquals(symmetric.nearest(Vector(0.5)), Right(0))
    assertEquals(symmetric.nearestAmong(Vector(0.5), Vector(1, 0)), Right(0))
    assertEquals(symmetric.nearestAmong(Vector(0.25), Vector(1)), Right(1))
    assert(symmetric.nearest(Vector(Double.NaN)).isLeft)
    intercept[IllegalArgumentException](TrialReferencePoints(chart, Vector.empty))
    intercept[IllegalArgumentException](TrialReferencePoints(chart, Vector(Vector(0.2), Vector(0.2))))
    intercept[IllegalArgumentException](TrialReferencePoints(chart, Vector(Vector(1.1))))

  test("reconstructed second bands preserve full curvature and charge bounded scratch and work"):
    val full = prep.referenceBank(points).toOption.get
    val compact = prep.referenceBank(points, TrialReferenceStorage.ReconstructSecondBands).toOption.get
    val bandBytes = 8L * prep.trials * (prep.bandwidth + 1)
    assertEquals(full.estimatedReferenceBytes - compact.estimatedReferenceBytes, 3L * bandBytes)
    assertEquals(compact.estimatedWorkerBytes - full.estimatedWorkerBytes, bandBytes)
    val encoded = prep.encodeWhitened(Array.tabulate(80)(i => math.sin(i * 0.17) + 0.2)).toOption.get
    val workers = Vector(full.newWorker(), compact.newWorker(), compact.newWorker())
    workers.foreach(_.pointAt(encoded))
    (0 until points.count).foreach: node =>
      val jets = workers.map: worker =>
        val out = new ProfileJetBuffer(2, 2)
        assert(worker.jetAtNode(node, out))
        out
      jets.tail.foreach: jet =>
        assertEqualsDouble(jet.energy, jets.head.energy, 1e-12)
        jet.gradient.zip(jets.head.gradient).foreach((a, b) => assertEqualsDouble(a, b, 1e-12))
        jet.hessian.zip(jets.head.hessian).foreach((a, b) => assertEqualsDouble(a, b, 1e-12))
        assertEquals(jet.curvature, jets.head.curvature)
    val ledger = workers(1).work.snapshot.attempted
    assertEquals(ledger.reconstructedBands, 6L)
    assert(ledger.reconstructedBandProducts > 0L)
    assertEquals(ledger.factorAttempts, 0L)
    val total = ProfileHrfFit.sumTrialWork(workers.map(_.work.snapshot)).attempted
    assertEquals(total.reconstructedBands, 12L)
    assertEquals(total.reconstructedBandProducts, 2L * ledger.reconstructedBandProducts)
    assertEquals(ledger.solveAttempts, workers.head.work.snapshot.attempted.solveAttempts)
    assertEquals(compact.work.snapshot.attempted.reconstructedBands, 0L)
    assertEquals(compact.setupReceipt, full.setupReceipt)
    assertEquals(compact.retainedReconstructionScratchBytes, 0L)
    assertEquals(workers(1).retainedReconstructionScratchBytes, bandBytes)

  test("explicit compact bank preserves corrected conditional readout without reconstruction"):
    val full = prep.referenceBank(points).toOption.get
    val compact = prep.referenceBank(points, TrialReferenceStorage.ReconstructSecondBands).toOption.get
    val encoded = prep.encodeWhitened(Array.tabulate(80)(i => math.sin(i * 0.17) + 0.2)).toOption.get
    val actual = point(0.42, 0.55)
    val results = Vector(full, compact).map: bank =>
      new TrialConditionalSolve(bank).solve(encoded, 0, actual)
        .fold(e => fail(e.message), identity)
    results(0).trialAmplitudes.zip(results(1).trialAmplitudes).foreach((a, b) => assertEqualsDouble(a, b, 1e-12))
    assertEquals(results(0).work, results(1).work)
    assertEquals(compact.work.snapshot.attempted.reconstructedBands, 0L)
    assertEquals(compact.retainedReconstructionScratchBytes, 0L)
    assertEquals(results(1).work.residualCorrections, 1)
    assertEquals(results(1).work.referenceInverseAttempts, 3L)

  test("reconstruction preserves regular-grid determinant jets and continuous curvature"):
    val grid = NodeGrid(family.chart, Vector(2, 2))
    val full = prep.objective(grid).toOption.get
    val compact = prep.objective(grid, TrialReferenceStorage.ReconstructSecondBands).toOption.get
    val encoded = prep.encodeWhitened(Array.tabulate(80)(i => math.cos(i * 0.23))).toOption.get
    Vector(full, compact).foreach(_.pointAt(encoded))
    (0 until grid.count).foreach: node =>
      val jets = Vector(full, compact).map(_.mlNodeDeterminant(node).toOption.get.outcome.toOption.get)
      assertEqualsDouble(jets(0).value, jets(1).value, 1e-12)
      jets(0).gradient.zip(jets(1).gradient).foreach((a, b) => assertEqualsDouble(a, b, 1e-12))
      jets(0).hessian.zip(jets(1).hessian).foreach((a, b) => assertEqualsDouble(a, b, 1e-12))
    assertEquals(compact.work.snapshot.attempted.reconstructedBands, 12L)
    val continuous = Vector(full, compact).map: bank =>
      val out = new ProfileJetBuffer(2, 2)
      assert(bank.jetAt(point(0.4, 0.6).toArray, out))
      out
    continuous(0).hessian.zip(continuous(1).hessian).foreach((a, b) => assertEqualsDouble(a, b, 1e-12))
    assertEquals(compact.work.snapshot.attempted.reconstructedBands, 12L)

  test("explicit-bank transpose retains the corrected linear operator adjoint"):
    val bank = prep.referenceBank(points, TrialReferenceStorage.ReconstructSecondBands).toOption.get
    val raw = Array.tabulate(80)(i => math.sin(0.17 * i))
    val encoded = prep.encodeWhitened(raw).toOption.get
    val solver = new TrialConditionalSolve(bank)
    val actual = point(0.45, 0.6)
    val solved = solver.solve(encoded, 0, actual).toOption.get
    val weights = Array(0.5, -0.7, 0.1, 0.8, 0.0)
    val transposed = new Array[Double](80)
    val receipt = solver.transposeInto(0, actual, weights, transposed).toOption.get
    val forward = solved.trialAmplitudes.zip(weights.take(4)).map(_ * _).sum
    val backward = raw.zip(transposed).map(_ * _).sum
    assertEqualsDouble(forward, backward, 1e-10)
    assertEquals(receipt.referenceInverseAttempts, 3L)
    assertEquals(bank.work.snapshot.attempted.reconstructedBands, 0L)
