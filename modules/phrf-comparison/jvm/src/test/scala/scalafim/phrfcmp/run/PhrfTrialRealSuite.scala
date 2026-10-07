package scalafim.phrfcmp.run

import java.nio.file.{Path, Paths}

import scalafim.phrfcmp.ingest.*
import scalafim.phrfcmp.prep.{CommonPrep, CommonPreparation}

/**
  * The PHRF trial arm on the real generator fixture (HARNESS root, dataset 0 of T-TX-fast, 3 scored voxels, 4 runs, 144
  * trials, aligned integer onsets): the whole 37-fit arm, determinism, the real penalty scale, the PHRF-can probe and the
  * endpoint diagnostics. Harness data only: nothing printed here is pilot evidence.
  */
class PhrfTrialRealSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(30, "min")

  private val GeneratorSha = "b4ba7ac95cc226b7f4132167e67acab885543a0bd157c985bfb51bfd286d3edf"
  private val HarnessRoot = "7a3c91d50b44e2f1"
  private val trialExp = IngestExpectation(
    RootKind.Harness, HarnessRoot, GeneratorSha, CellSpec("T-TX-fast", CellKind.Trial, 3, 2, 1.0, true), 0, Seeds.ProtocolDenylist
  )
  private def fixtureDir: Path = Paths.get(getClass.getClassLoader.getResource("fixtures/T-TX-fast__d0000.npz").toURI).getParent

  private lazy val bound: BoundDataset = PhrfDatasetLoader.load(fixtureDir, "T-TX-fast__d0000", trialExp).fold(r => fail(r.message), identity)
  private lazy val inputs: FitInputs = bound.fit
  private lazy val prep: CommonPrep = CommonPreparation.prepare(inputs, CellKind.Trial).fold(e => fail(e.message), identity)
  private val config = PhrfTrialConfig()

  private lazy val outcome: PhrfTrialOutcome =
    val t0 = System.nanoTime()
    val o = PhrfTrialRunner.run(inputs, prep, AlphaGrid.Pilot, config).fold(e => fail(e.message), identity)
    println(f"S5-REAL whole PHRF trial arm (37 fits, 3 voxels): ${(System.nanoTime() - t0) / 1e9}%.1f s wall")
    o

  private def pearson(a: Seq[Double], b: Seq[Double]): Double =
    val ma = a.sum / a.length
    val mb = b.sum / b.length
    val cov = a.zip(b).map((x, y) => (x - ma) * (y - mb)).sum
    cov / math.sqrt(a.map(x => (x - ma) * (x - ma)).sum * b.map(y => (y - mb) * (y - mb)).sum)

  test("real fixture: the 37-fit arm runs, statuses are typed, E-trial is in input trial order"):
    val o = outcome
    val sel = o.tuning.selection
    println(f"S5-REAL rho=${prep.rho}%.4f sigma2All=${prep.sigma2All.sigma2}%.4f selected alpha=${sel.selectedAlpha} (index ${sel.selected}; lower end ${sel.atLowerEnd}, upper end ${sel.atUpperEnd})")
    println(s"S5-REAL mean fold scores ${sel.meanScores.map(x => f"$x%.4f").mkString(", ")}")
    println(s"S5-REAL fold sigma2 ${o.tuning.folds.map(f => f"${f.sigma2}%.4f").mkString(", ")}")
    println(s"S5-REAL final statuses ${o.fit.voxels.map(v => (v.status, v.decode))}")
    assertEquals(o.tuning.folds.length, 4)
    assertEquals(o.tuning.alphasFitted.length, 36)
    assertEquals(o.fit.voxels.length, 3)
    assertEquals(o.fit.runReceipt.route, "trial-banded-ml")
    assertEquals(o.fit.runReceipt.evidenceVoxels, 3)
    assert(o.fit.voxels.forall(v => v.status != TrialArmStatus.Estimated || v.eTrial.exists(_.length == 144)))
    // sanity against the truth (harness data): within-condition Pearson r of E-trial with the true trial peak height
    val truth = bound.truth.trialBeta.getOrElse(fail("trial_beta present"))
    val rs = o.fit.voxels.zipWithIndex.collect { case (v, j) if v.eTrial.isDefined =>
      val cs = inputs.evCond.distinct.sorted.toVector
      cs.map(c => pearson(inputs.evCond.indices.filter(inputs.evCond(_) == c).map(i => v.eTrial.get(i)), inputs.evCond.indices.filter(inputs.evCond(_) == c).map(i => truth(j, i)))).sum / cs.length
    }
    println(s"S5-REAL within-condition r (mean over conditions) per estimated voxel: ${rs.map(x => f"$x%.3f").mkString(", ")}")
    // this fixture: voxel 0 is a Boundary refusal, voxels 1 and 2 are estimated (observed r 0.547, 0.407); fails if an estimate is lost or degrades
    assert(rs.length >= 2, s"estimated voxels: ${rs.length}")
    assert(rs.forall(_ > 0.25), s"within-condition r $rs")

  test("real fixture: a rerun is hash-equal"):
    val again = PhrfTrialRunner.run(inputs, prep, AlphaGrid.Pilot, config).fold(e => fail(e.message), identity)
    assertEquals(again.sha256, outcome.sha256)

  test("real fixture: the penalty scale and the rLSS settings the pilot path uses"):
    val r = PhrfPenaltyScale.derive(inputs, prep, config).fold(e => fail(e.message), identity)
    println(f"S5-REAL penalty scale: canonical tau=${r.shape.coordinates(0)}%.4f sd=${math.exp(r.shape.coordinates(1))}%.4f; kappa2 ${r.scale.shape}%.5f centring ${r.scale.centring}%.5f factor ${r.scale.factor}%.5f; spreads shape ${r.scale.shapeSpread}%.3f centring ${r.scale.centringSpread}%.3f")
    assertEqualsDouble(r.scale.centring, 1.0 - 1.0 / 48.0, 1e-12) // 48 trials per condition
    assert(r.scale.shape > 0.2 && r.scale.shape < 5.0)
    val derived = PhrfTrialRunner.rlssSettings(inputs, prep).fold(e => fail(e.message), identity)
    assertEquals(derived.scale, r.scale)
    val nativeIn = TrialNativeInputs.from(inputs, prep).fold(e => fail(e.code), identity)
    // the scale is a diagnostic; the mapping is on PHRF's TRUE edf, which `rlssSettings` supplies as targets
    val settings = derived
    assert(settings.targets.isDefined)
    val phrfEdf = PhrfEdf.forRuns(inputs, prep, prep.segments.map(_.runIndex), settings.grid, config).fold(e => fail(e.message), identity)
    val q = TrialNativeRunner.prepareRlss(nativeIn, TrialNativeRunner.conditionGrouping(nativeIn)).fold(e => fail(e.message), identity).q.toArray
    val surrogate = Vector.tabulate(settings.grid.size)(k => InformationRatio(q, r.scale.penalty(settings.grid.lambda(k))) * q.length)
    println(s"S5-REAL PHRF TRUE edf by alpha 2^-6..2^2 (N=144, G=3): ${phrfEdf.edf.map(x => f"$x%.3f").mkString(", ")}")
    println(s"S5-REAL S4 q-based surrogate x N (mean q/(q+p), p = scale.penalty):  ${surrogate.map(x => f"$x%.3f").mkString(", ")}")
    assert(phrfEdf.edf.zip(phrfEdf.edf.tail).forall((a, b) => a < b))
    // limits of the edf AT THE CANONICAL SHAPE on the real design (N = 144 trials, G = 3 conditions)
    val basis = PhrfTrialRunner.basisOf(config).fold(e => fail(e.message), identity)
    val problem = PhrfTrialAssembly.problem(inputs, prep, prep.segments.map(_.runIndex), prep.sigma2All.sigma2, config).fold(e => fail(e.message), identity)
    val g = PhrfEdf.gram(problem, basis, r.shape.coordinates).fold(e => fail(e.message), identity)
    assertEqualsDouble(PhrfEdf.amplitudeEdf(g, problem.trialCond, 0.0).fold(m => fail(m), identity), 144.0, 1e-6)
    assertEqualsDouble(PhrfEdf.amplitudeEdf(g, problem.trialCond, 1e9).fold(m => fail(m), identity), 3.0, 1e-2)
    assert(phrfEdf.edf.forall(e => e > 3.0 && e < 144.0))
    // sealed diagnostic: edf at each voxel's decoded shape (not at the canonical shape)
    outcome.decodedShapeEdf.zipWithIndex.foreach((e, j) => println(s"S5-REAL decoded-shape edf voxel $j: ${e.map(_.map(x => f"$x%.2f").mkString(", "))}"))
    val out = TrialNativeRunner.run(scalafim.phrfcmp.prep.NativeArm.Rlss, nativeIn, settings).fold(e => fail(e.message), identity)
    val t = out.tuning.getOrElse(fail("rLSS tuning"))
    println(f"S5-REAL rLSS (derived scale) selected grid index ${t.selection.selected}, ridge ${t.selectedRidge}%.4f; df map flagged ${t.finalMap.flagged}")

  test("real fixture: PHRF-can timing probe with the shared shell, grid and score"):
    val t0 = System.nanoTime()
    val probe = PhrfCanProbe.run(inputs, prep, AlphaGrid.Pilot, config).fold(e => fail(e.message), identity)
    println(f"S5-REAL PHRF-can probe (37 fixed-shape fits): ${(System.nanoTime() - t0) / 1e9}%.1f s wall; selected alpha ${probe.selectedAlpha}; tau=${probe.shape.coordinates(0)}%.3f")
    println(s"S5-REAL PHRF-can mean fold scores ${probe.tuning.selection.meanScores.map(x => f"$x%.4f").mkString(", ")}")
    println(f"S5-REAL PHRF-can per-alpha preparation profile (s): ${probe.timings.alphaPreparationProfile.map(x => f"$x%.3f").mkString(", ")}")
    println(f"S5-REAL PHRF per-alpha preparation profile (s):     ${outcome.timings.alphaPreparationProfile.map(x => f"$x%.3f").mkString(", ")}")
    println(f"S5-REAL PHRF trial-ML core-ms/voxel (fold fits, median): ${outcome.timings.trialMlSecondsPerVoxel.sorted.apply(outcome.timings.trialMlSecondsPerVoxel.length / 2) * 1e3}%.1f")
    assertEquals(probe.tuning.alphasFitted.length, 36)
    assertEquals(probe.finalETrial.length, 3)
    assert(probe.finalETrial.forall(_.length == 144))
    // the probe is scored by the same shell: same folds, same grid
    assertEquals(probe.tuning.folds.map(_.heldOutRun), outcome.tuning.folds.map(_.heldOutRun))
    assertEquals(probe.tuning.selection.grid, outcome.tuning.selection.grid)
