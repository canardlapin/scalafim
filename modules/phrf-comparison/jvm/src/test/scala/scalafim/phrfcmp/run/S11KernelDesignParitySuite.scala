package scalafim.phrfcmp.run

import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest

import scala.jdk.CollectionConverters.*

import scalafim.fmri.ar.TimeSegment
import scalafim.phrfcmp.score.KernelBasis

/**
  * The remaining S11 fixture consumers: the response kernels (`kernel_*.csv`), the single-trial design (`X_trial.csv`),
  * and the fixture manifest. Relative error is `max|a - b| / max|b|` with b the fixture.
  *
  *  - Kernels: the SPM bases are compared on 0-24 s at 1e-8 and must be exactly zero beyond (owner decision 2026-10-02: the
  *    E-resp basis is truncated at the 24 s design span, whereas fmrireg's raw kernel runs to 32 s). FIR is compared on
  *    the whole grid, piecewise constant and zero from H on (owner decision 2026-10-01).
  *  - Trial design: the exact-lag canonical single-trial regressors against fmrireg `trialwise(basis = "spmg1")` at abs 1e-12,
  *    TR-aligned and 0.1 s cells.
  *  - Manifest: every committed fixture file hashes to `MANIFEST.sha256`, so a hand-edited fixture cannot pass silently.
  */
class S11KernelDesignParitySuite extends munit.FunSuite:

  private val results = scala.collection.mutable.ArrayBuffer.empty[String]

  private def fixtures: Path =
    def find(p: Path): Option[Path] =
      if p == null then None
      else
        val d = p.resolve("tools/phrf-comparison/parity/fixtures")
        if Files.isDirectory(d) then Some(d) else find(p.getParent)
    find(Paths.get(sys.props("user.dir")).toAbsolutePath).getOrElse(fail("S11 fixtures directory not found"))

  private def rows(p: Path): Vector[Array[String]] =
    Files.readAllLines(p).asScala.toVector.drop(1).filter(_.nonEmpty).map(_.split(",", -1))

  private val kernels: Seq[(String, KernelBasis, Boolean)] = Seq(
    ("can", KernelBasis.Canonical, true),
    ("inf3", KernelBasis.InformedThree, true),
    ("fir", KernelBasis.Fir(32, 1.0), false)
  )

  for
    cell <- Seq("cond", "cond_close")
    (id, basis, spm) <- kernels
  do
    test(s"$cell/kernel_$id: response basis matches the fmrihrf kernel (0-24 s at 1e-8 for SPM, zero beyond; FIR everywhere)"):
      val fx = rows(fixtures.resolve(cell).resolve(s"kernel_$id.csv"))
      val nb = basis.size
      assertEquals(fx.head.length, nb + 1)
      val out = new Array[Double](nb)
      var diff = 0.0
      var scale = 0.0
      var beyond = 0.0
      fx.foreach { r =>
        val tau = r(0).toDouble
        basis.valuesAt(tau, out)
        val ref = r.drop(1).map(_.toDouble)
        if spm && tau > KernelBasis.SpmgSpanSeconds then beyond = math.max(beyond, out.map(math.abs).max)
        else
          for k <- 0 until nb do
            diff = math.max(diff, math.abs(out(k) - ref(k)))
            scale = math.max(scale, math.abs(ref(k)))
      }
      val rel = diff / scale
      results += f"S11-KERNEL $cell/$id%-6s rel=$rel%.3e max_abs_beyond_24s=$beyond%.1e"
      assert(rel <= 1e-8, s"$cell/$id kernel relative error $rel")
      assertEquals(beyond, 0.0, s"$cell/$id kernel beyond 24 s")

  for cell <- Seq("tx", "ts") do
    test(s"trial/$cell: canonical single-trial design matches fmrireg trialwise(spmg1) at abs 1e-12"):
      val d = fixtures.resolve("trial").resolve(cell)
      val tr = rows(d.resolve("trials.csv"))
      val n = tr.length
      val runLen = 150
      val events = ConditionEvents(
        tr.map(_(2).toDouble).toArray, Array.tabulate(n)(identity), tr.map(_(1).toInt - 1).toArray, Array.fill(n)(0.0), n)
      val segments = Vector.tabulate(3)(r => TimeSegment(r * runLen, (r + 1) * runLen, r))
      val sampleTime = Array.tabulate(3 * runLen)(i => (i % runLen).toDouble)
      val x = ConditionDesigns.build(scalafim.fmri.hrf.Hrfs.SPMG1, events, sampleTime, segments).fold(k => fail(k.code), identity)
      val fx = rows(d.resolve("X_trial.csv"))
      assertEquals((x.rows, x.cols), (fx.length, fx.head.length))
      var abs = 0.0
      for t <- 0 until x.rows; c <- 0 until x.cols do abs = math.max(abs, math.abs(x(t, c) - fx(t)(c).toDouble))
      results += f"S11-TRIALDESIGN $cell abs=$abs%.3e"
      assert(abs <= 1e-12, s"trial/$cell design abs diff $abs")

  test("MANIFEST.sha256 matches every committed fixture file"):
    val root = fixtures
    val lines = Files.readAllLines(root.resolve("MANIFEST.sha256")).asScala.toVector.filter(_.nonEmpty)
    assert(lines.length >= 100, s"manifest lists only ${lines.length} files")
    def hex(b: Array[Byte]) = b.map(x => f"${x & 0xff}%02x").mkString
    val bad = lines.filter { l =>
      val (h, f) = (l.takeWhile(!_.isWhitespace), l.dropWhile(!_.isWhitespace).trim.stripPrefix("*"))
      hex(MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(root.resolve(f)))) != h
    }
    assertEquals(bad, Vector.empty[String])
    results += s"S11-MANIFEST ${lines.length} files verified"

  override def afterAll(): Unit = results.foreach(println)
