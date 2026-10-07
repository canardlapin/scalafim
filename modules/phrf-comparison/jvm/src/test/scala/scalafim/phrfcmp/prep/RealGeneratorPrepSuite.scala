package scalafim.phrfcmp.prep

import java.nio.file.{Path, Paths}

import scalafim.phrfcmp.ingest.*

/** Preparation on the real generator fixtures (HARNESS root, dataset 0, reduced voxel counts). */
class RealGeneratorPrepSuite extends munit.FunSuite:

  private val GeneratorSha = "b4ba7ac95cc226b7f4132167e67acab885543a0bd157c985bfb51bfd286d3edf"
  private val HarnessRoot = "7a3c91d50b44e2f1"
  private val trialExp = IngestExpectation(
    RootKind.Harness, HarnessRoot, GeneratorSha, CellSpec("T-TX-fast", CellKind.Trial, 3, 2, 1.0, true), 0, Seeds.ProtocolDenylist
  )
  private val condExp = IngestExpectation(
    RootKind.Harness, HarnessRoot, GeneratorSha, CellSpec("C-TX-.5", CellKind.Condition, 3, 0, 1.0, false), 0, Seeds.ProtocolDenylist
  )
  private def fixtureDir: Path =
    Paths.get(getClass.getClassLoader.getResource("fixtures/T-TX-fast__d0000.npz").toURI).getParent

  private def load(stem: String, e: IngestExpectation): FitInputs =
    PhrfDatasetLoader.load(fixtureDir, stem, e).fold(r => fail(r.message), _.fit)

  private def rank(m: Matrix): Int = Linear.projector(Linear.toDMat(m))._2

  for (stem, e, kind, nRuns) <- Seq(("T-TX-fast__d0000", trialExp, CellKind.Trial, 4), ("C-TX-.5__d0000", condExp, CellKind.Condition, 1)) do
    test(s"real $kind data: dropping the generator intercepts gives a full-rank shared baseline with the same span"):
      val in = load(stem, e)
      val split = Nuisance.dropIntercepts(in.nuisance, in.runId).fold(r => fail(r.message), identity)
      assertEquals(split.droppedColumns, Vector.tabulate(nRuns)(_ * 6))
      assertEquals(split.dropped.cols, 5 * nRuns)
      val t = in.nuisance.rows
      val w = 6 * nRuns
      val baseline = Matrix.of(t, w, Array.tabulate(t * w)(k =>
        val (r, j) = (k / w, k % w)
        if j < nRuns then split.runIntercepts(r, j) else split.dropped(r, j - nRuns))).fold(r => fail(r.message), identity)
      assertEquals(rank(baseline), w)
      assertEquals(rank(in.nuisance), w)
      // Constant intercepts on top of the undropped generator nuisance would be rank deficient
      val w2 = w + nRuns
      val doubled = Matrix.of(t, w2, Array.tabulate(t * w2)(k =>
        val (r, j) = (k / w2, k % w2)
        if j < nRuns then split.runIntercepts(r, j) else in.nuisance(r, j - nRuns))).fold(r => fail(r.message), identity)
      assertEquals(rank(doubled), w)

    test(s"real $kind data: common preparation runs end to end and is deterministic"):
      val in = load(stem, e)
      val a = CommonPreparation.prepare(in, kind).fold(r => fail(r.message), identity)
      val b = CommonPreparation.prepare(in, kind).fold(r => fail(r.message), identity)
      assertEquals(a.whitened.sha256, b.whitened.sha256)
      assertEquals(java.lang.Double.doubleToLongBits(a.rho), java.lang.Double.doubleToLongBits(b.rho))
      assert(a.rho.isFinite && math.abs(a.rho) < 1.0)
      assertEquals(a.heterogeneity.perRunRho.length, nRuns)
      assertEquals(a.armHashes.values.toSet.size, 1)
      // standing invariant: the residual-forming QR and the AR module agree on the design rank
      assertEquals(a.prefit.nSamples - a.prefit.provenance.residualDf, a.prefit.rank)
      println(f"REAL-FIXTURE $kind rho=${a.rho}%.4f sigma2=${a.sigma2All.sigma2}%.4f rank=${a.prefit.rank}%d")

  test("real trial data: the 264-column trial design has exactly one exact dependency per condition (rank 261)"):
    // Onsets and sample times are integers (tr_aligned_onsets), so for each condition the sum of its trial columns,
    // sum_e h(lag_e), equals sum_b h(b) * FIR_(c,b): the canonical response at integer lags lies in the span of the 1 s
    // FIR bins. 3 conditions give 3 dependencies. Intended, harmless to the correction (the AR module and QR agree).
    val in = load("T-TX-fast__d0000", trialExp)
    val x = FirPrefit.design(in, CellKind.Trial).fold(r => fail(r.message), identity)
    val nCond = in.evCond.max + 1
    assertEquals(x.cols, 264)
    assertEquals(rank(x), x.cols - nCond)
    assert(in.evOnset.forall(o => o == math.rint(o)) && in.sampleTime.forall(t => t == math.rint(t)))

  for (stem, e, kind) <- Seq(("T-TX-fast__d0000", trialExp, CellKind.Trial), ("C-TX-.5__d0000", condExp, CellKind.Condition)) do
    test(s"real $kind data: the prepared AR path equals the unprepared path exactly (phi, gamma, every recorded value)"):
      val in = load(stem, e)
      val x = FirPrefit.arDesign(in, kind).fold(r => fail(r.message), identity)
      val level = ArDesignLevel.forKind(kind)
      val a = FirPrefit.fit(in, x, level).fold(r => fail(r.message), identity)
      val b = FirPrefit.fitUnprepared(in, x, level).fold(r => fail(r.message), identity)
      def bits(d: Double) = java.lang.Double.doubleToLongBits(d)
      assertEquals(bits(a.rho), bits(b.rho))
      assertEquals(a.provenance.gamma.map(bits), b.provenance.gamma.map(bits))
      assertEquals(a.provenance.canonical, b.provenance.canonical)
      assertEquals(a.heterogeneity.perRunRho.map(bits), b.heterogeneity.perRunRho.map(bits))
      assertEquals(a.heterogeneity.voxelRho.map(bits), b.heterogeneity.voxelRho.map(bits))
      assertEquals(a.heterogeneity.canonical, b.heterogeneity.canonical)
