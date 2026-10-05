package scalafim.phrfcmp.prep

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*

import scalafim.phrfcmp.ingest.*

/**
  * Frozen bias criterion (design 2.0.2, P2): |mean(rho_hat) - 0.3| <= 0.02 over 50 HARNESS datasets, per cell kind.
  * Opt-in (heavy: runs the Python generator and 50 datasets per cell, each with the corrected pooled, per-run and per-voxel AR fits):
  * `sbt -Dphrf.s2.bias=true "phrfComparisonJVM/testOnly scalafim.phrfcmp.prep.RhoBiasHeavySuite"`;
  * `-Dphrf.s2.datasets=N` changes the count (the frozen criterion is N = 50).
  * Cells use the pilot voxel count (40 scored voxels; trial cells carry a 2-voxel pool the pre-fit never reads).
  */
class RhoBiasHeavySuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(60, "min")

  private val GeneratorSha = "b4ba7ac95cc226b7f4132167e67acab885543a0bd157c985bfb51bfd286d3edf"
  /** Selection root (rev 1 and rev 2 runs; spent). The VERDICT run uses [[FreshRoot]] only. */
  private val SelectionRootHex = "7a3c91d50b44e2f1"

  /** Rev 3 verdict root: first 8 bytes of SHA-256("phrf-cmp-s2-rev3-fresh-seeds-20261001"), declared in design 2.0.2. */
  private val FreshRootLabel = "phrf-cmp-s2-rev3-fresh-seeds-20261001"
  private val FreshRoot: Long = java.lang.Long.parseUnsignedLong(Digests.sha256Hex(FreshRootLabel.getBytes("UTF-8")).take(16), 16)
  private val HarnessRoot = f"$FreshRoot%016x"
  private val TrueRho = 0.3
  private val Criterion = 0.02
  private val enabled = sys.props.get("phrf.s2.bias").contains("true")
  /** `-Dphrf.s2.cells=trial|condition|both` (default both) selects which cell kinds run; the criterion is per kind. */
  private val cells = sys.props.get("phrf.s2.cells").getOrElse("both")
  private val n = sys.props.get("phrf.s2.datasets").map(_.toInt).getOrElse(50)

  private def findGen(p: Path): Option[Path] =
    if p == null then None
    else
      val g = p.resolve("tools/phrf-comparison/generator")
      if Files.isDirectory(g) then Some(g) else findGen(p.getParent)

  private def generate(gen: Path, out: Path): Unit =
    val script =
      s"""import sys, dataclasses
         |sys.path.insert(0, '$gen')
         |from phrf_gen.cells import CELLS
         |from phrf_gen.io import write_dataset
         |for cid, kw in [('T-TX-fast', dict(n_voxels=40, n_pool=2)), ('C-TX-.5', dict(n_voxels=40))]:
         |    for d in range($n):
         |        write_dataset(dataclasses.replace(CELLS[cid], **kw), 0x$HarnessRoot, 'harness', d, '$out')
         |""".stripMargin
    val p = new ProcessBuilder("python3", "-c", script).redirectErrorStream(true).start()
    val log = new String(p.getInputStream.readAllBytes())
    assertEquals(p.waitFor(), 0, log)

  private def cellRun(out: Path, cellId: String, kind: CellKind, nPool: Int, aligned: Boolean): Double =
    val fits = (0 until n).map { d =>
      val exp = IngestExpectation(RootKind.Harness, HarnessRoot, GeneratorSha, CellSpec(cellId, kind, 40, nPool, 1.0, aligned), d, Seeds.ProtocolDenylist)
      val stem = f"${cellId}__d$d%04d"
      val in = PhrfDatasetLoader.load(out, stem, exp).fold(r => fail(r.message), _.fit)
      FirPrefit.prefit(in, kind).fold(r => fail(r.message), _._1)
    }
    val rhos = fits.map(_.rho)
    val mean = rhos.sum / n
    val sd = math.sqrt(rhos.map(r => (r - mean) * (r - mean)).sum / (n - 1))
    val bias = mean - TrueRho
    // diagnostics only (reported, never used to select anything)
    println(f"S2-RHO-BIAS cell=$cellId diag: mean raw (AR module, uncorrected)=${fits.map(_.provenance.rhoRaw).sum / n}%.5f mean rank=${fits.map(_.rank.toDouble).sum / n}%.1f of ${fits.head.designCols}%d columns, n=${fits.head.nSamples}%d, lag used=${fits.map(_.provenance.lagUsed).distinct}")
    println(f"S2-RHO-BIAS cell=$cellId kind=$kind datasets=$n mean=$mean%.5f bias=$bias%+.5f sd=$sd%.5f min=${rhos.min}%.4f max=${rhos.max}%.4f")
    println(s"S2-RHO-BIAS cell=$cellId values=${rhos.map(r => f"$r%.4f").mkString(",")}")
    val verdict = if math.abs(bias) <= Criterion then "PASS" else "FAIL"
    println(f"S2-RHO-BIAS cell=$cellId criterion |bias|<=$Criterion%.2f verdict=$verdict")
    bias

  test("fresh verdict seeds are disjoint from the selection seeds and from the denylist"):
    val selection = java.lang.Long.parseUnsignedLong(SelectionRootHex, 16)
    assertNotEquals(FreshRoot, selection)
    assertEquals(HarnessRoot, "80dd06a7d4666a7b")
    def streams(root: Long): Set[Long] =
      (for c <- Seq("T-TX-fast", "C-TX-.5"); d <- 0 until 50; p <- 0 until 7 yield Seeds.streamSeed(root, c, d, p)).toSet
    val (sel, fresh) = (streams(selection), streams(FreshRoot))
    assertEquals(sel.size, 2 * 50 * 7)
    assertEquals(fresh.size, 2 * 50 * 7)
    assert((sel intersect fresh).isEmpty, "selection and verdict stream seeds overlap")
    assert(!fresh.exists(Seeds.denylistHit(_, Seeds.ProtocolDenylist)), "a fresh stream seed is denylisted")
    assert(!Seeds.denylistHit(FreshRoot, Seeds.ProtocolDenylist))

  test(s"frozen rho bias criterion over $n harness datasets: trial and condition cells (opt-in)"):
    assume(enabled, "heavy test: pass -Dphrf.s2.bias=true")
    val gen = findGen(Paths.get(sys.props("user.dir")).toAbsolutePath)
    assert(gen.isDefined, "generator directory not found")
    val out = Files.createTempDirectory("phrf-s2-bias")
    try
      generate(gen.get, out)
      val trial = if cells != "condition" then Some(cellRun(out, "T-TX-fast", CellKind.Trial, 2, aligned = true)) else None
      val cond = if cells != "trial" then Some(cellRun(out, "C-TX-.5", CellKind.Condition, 0, aligned = false)) else None
      trial.foreach(b => assert(math.abs(b) <= Criterion, f"trial |bias| ${math.abs(b)}%.5f exceeds $Criterion%.2f"))
      cond.foreach(b => assert(math.abs(b) <= Criterion, f"condition |bias| ${math.abs(b)}%.5f exceeds $Criterion%.2f"))
    finally
      Files.list(out).iterator().asScala.foreach(Files.delete)
      Files.delete(out)
