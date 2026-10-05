package scalafim.phrfcmp.run

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*

import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.phrfcmp.ingest.Matrix
import scalafim.phrfcmp.prep.{Whiten, WhitenedArrays}
import scalafim.phrfcmp.score.{EResp, KernelBasis, ResponseGrid}

/**
  * CAN, INF3 and FIR against the S11 fmrireg fixtures (`tools/phrf-comparison/parity`): event design (abs 1e-12),
  * fitted event coefficients and reconstructed E-resp (1e-8 relative, `max|a-b| / max|b|` with b the fixture), in both
  * whitening conventions and both condition cells. The whitening phi is the fixture's given 0.3125.
  */
class S11ConditionParitySuite extends munit.FunSuite:

  private val Phi = 0.3125
  private val GateRel = 1e-8
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

  /** All-numeric CSV; `skipFirst` drops a leading label or tau column. */
  private def num(p: Path, skipFirst: Boolean = false): Matrix =
    val rs = rows(p).map(r => (if skipFirst then r.drop(1) else r).map(_.toDouble))
    Matrix.of(rs.length, rs.head.length, rs.flatten.toArray).fold(e => fail(e.message), identity)

  private def relErr(a: Matrix, b: Matrix): Double =
    assertEquals((a.rows, a.cols), (b.rows, b.cols))
    a.data.indices.map(i => math.abs(a.data(i) - b.data(i))).max / b.data.map(math.abs).max

  private val Nt = 800
  private val segments = Vector.tabulate(4)(r => TimeSegment(r * 200, (r + 1) * 200, r))
  private val sampleTime = Array.tabulate(Nt)(i => (i % 200).toDouble)

  private def transpose(m: Matrix): Matrix =
    Matrix.of(m.cols, m.rows, Array.tabulate(m.rows * m.cols)(k => m(k % m.rows, k / m.rows))).fold(e => fail(e.message), identity)

  private def cols(m: Matrix, from: Int): Matrix =
    Matrix.of(m.rows, m.cols - from, Array.tabulate(m.rows * (m.cols - from))(k => m(k / (m.cols - from), from + k % (m.cols - from))))
      .fold(e => fail(e.message), identity)

  private val arms = Seq(ConditionArm.Can, ConditionArm.Inf3, ConditionArm.Fir)

  for
    cell <- Seq("cond", "cond_close")
    arm <- arms
    exactFirst <- Seq(true, false)
  do
    val tag = s"$cell/${if exactFirst then "exact_first" else "identity_first"}/${arm.id}"
    test(s"$tag: design 1e-12, betas and E-resp at 1e-8 against fmrireg"):
      val d = fixtures.resolve(cell)
      val out = if exactFirst then d else d.resolve("identity_first")
      val ev = rows(d.resolve("events.csv"))
      val events = ConditionEvents(
        ev.map(_(2).toDouble).toArray, ev.map(r => if r(3) == "A" then 0 else 1).toArray,
        ev.map(_(1).toInt - 1).toArray, Array.fill(ev.length)(0.0), 2)
      val hrf = ConditionDesigns.designHrf(arm, 32).get
      val basis = ConditionDesigns.responseBasis(arm, 32).get
      val x = ConditionDesigns.build(hrf, events, sampleTime, segments).fold(k => fail(k.code), identity)
      val ne = 2 * basis.size
      val xFix = num(d.resolve(s"X_${arm.id}.csv"))
      var designAbs = 0.0
      for t <- 0 until Nt; c <- 0 until ne do designAbs = math.max(designAbs, math.abs(x(t, c) - xFix(t, c)))
      assert(designAbs <= 1e-12, s"$tag design abs diff $designAbs")

      val plan = WhiteningPlan.global(ArmaCoefficients.ar(Phi), segments, exactFirstAr1 = exactFirst)
      val y = Whiten.series(plan, transpose(num(d.resolve("Y.csv")))).fold(e => fail(e.message), identity)
      val baseline = Whiten.columns(plan, cols(xFix, ne)).fold(e => fail(e.message), identity)
      val empty = Matrix.of(Nt, 0, Array.empty[Double]).fold(e => fail(e.message), identity)
      val arrays = WhitenedArrays(y, baseline, empty, empty)
      val fit = NativeConditionFit.fit(x, plan, arrays).fold(k => fail(k.code), identity)
      val betaErr = relErr(fit.coefficients, num(out.resolve(s"beta_${arm.id}.csv"), skipFirst = true))

      val tauRows = rows(out.resolve(s"eresp_${arm.id}.csv"))
      val g = ResponseGrid.of(tauRows.map(_(0).toDouble)).fold(e => fail(e.message), identity)
      val ref = num(out.resolve(s"eresp_${arm.id}.csv"), skipFirst = true) // grid x (2 conditions * 3 voxels), condition-major
      val got = Array.ofDim[Double](g.size * 6)
      for v <- 0 until 3 do
        val beta = Array.tabulate(ne)(r => fit.coefficients(r, v))
        val resp = EResp.fromBasis(g, basis, beta, 2).fold(e => fail(e.message), identity)
        for c <- 0 until 2; i <- 0 until g.size do got(i * 6 + c * 3 + v) = resp.curves(c)(i)
      // Parity is asserted on 0-24 s for the SPM arms: their E-resp is deliberately truncated at the 24 s design span
      // (owner decision 2026-10-02) whereas fmrireg's fitted_hrf evaluates the raw kernel to 32 s. FIR: the whole grid.
      val limit = if arm == ConditionArm.Fir then Double.PositiveInfinity else KernelBasis.SpmgSpanSeconds
      val keep = (0 until g.size).filter(i => g.lag(i) <= limit)
      if arm != ConditionArm.Fir then
        for i <- 0 until g.size if g.lag(i) > limit; k <- 0 until 6 do assertEquals(got(i * 6 + k), 0.0, s"$tag E-resp after 24 s")
      def subset(a: Array[Double]) = Matrix.of(keep.length, 6, keep.flatMap(i => (0 until 6).map(k => a(i * 6 + k))).toArray).fold(e => fail(e.message), identity)
      val erespErr = relErr(subset(got), subset(ref.data))
      results += f"S11-COND $tag%-40s design_abs=$designAbs%.2e beta_rel=$betaErr%.3e eresp_rel=$erespErr%.3e"
      assert(betaErr <= GateRel, s"$tag beta relative error $betaErr")
      assert(erespErr <= GateRel, s"$tag E-resp relative error $erespErr")

  override def afterAll(): Unit = results.foreach(println)
