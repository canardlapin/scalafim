package scalafim.fmri.laws.profile

import gale.linalg.{DMat, QROptions, QRPivoting}
import scalafim.fmri.design.event.{Event, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis, KernelBasisSpec}
import scalafim.fmri.fit.profile.{CompactConditionJets, ProfileJetBuffer, ProfileReduction}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, JetLayout}

/** The exact `alpha = 0` law through the real chain: family -> kernel basis ->
  * expanded design -> rank-revealing QR -> compact jets, against a direct
  * time-domain least-squares fit of the family design at the same shape.
  */
class ProfileReductionLawsSuite extends munit.FunSuite:

  private val family = GaussianFamily.Default
  private val step = PositiveSeconds(0.1).fold(e => fail(e.message), identity)
  private val precision = Seconds(0.1)
  private val frame = SamplingFrame(blockLens = Seq(80), tr = Seq(1.0))
  private val conditions = Vector("A", "B", "C", "A", "B", "C", "A", "B", "C", "B", "A", "C")
  private val onsets = Vector(1.3, 6.1, 10.6, 17.0, 23.4, 30.7, 36.2, 41.9, 47.5, 53.8, 60.3, 66.1).map(Seconds(_))
  private val term = EventTerm(events = Vector(Event.factor(conditions, "cond")), onsets = onsets, blockIds = Vector.fill(12)(0), termTag = Some("cond"))

  private lazy val basis = HrfKernelBasis.compile(KernelBasisSpec(family, step, Vector(26, 21), tolerance = 1e-5, maxRank = 40)).fold(e => fail(e.message), identity)
  private lazy val expanded = ExpandedConditionDesign.lower(term, frame, basis, precision).fold(e => fail(e.message), identity)

  private def toDMat(rows: Int, cols: Int, rowMajor: Array[Double]): DMat =
    val b = DMat.newBuilder(rows, cols)
    var i = 0
    while i < rows * cols do
      b.writeLinear(i, rowMajor(i))
      i += 1
    b.result()

  /** Rank-revealing factor `U R` of the expanded design, `R` un-permuted. */
  private lazy val (u, rHat, rank) =
    val t = expanded.rows
    val cm = expanded.columns
    val qr = toDMat(t, cm, expanded.term.data.data).qr(QROptions(pivoting = QRPivoting.Column, rankTolerance = Some(1e-10)))
    val k = qr.diagnostics.rank.getOrElse(fail("no rank"))
    val uArr = new Array[Double](t * k)
    qr.q.slice(0, t, 0, k).copyRowMajorTo(uArr)
    val rPerm = new Array[Double](k * cm)
    qr.r.slice(0, k, 0, cm).copyRowMajorTo(rPerm)
    val perm = qr.columnPermutation.toArray
    val r = new Array[Double](k * cm)
    var i = 0
    while i < k do
      var j = 0
      while j < cm do
        r(i * cm + perm(j)) = rPerm(i * cm + j)
        j += 1
      i += 1
    (uArr, r, k)

  private def directFit(t1: Double, t2: Double, y: Array[Double]): (Double, Array[Double]) =
    val point = family.chart.point(t1, t2).fold(e => fail(e.message), identity)
    val design = expanded.designAt(point)
    val fit = toDMat(design.rows, design.cols, design.data).leastSquares(gale.linalg.DVec.fromSeq(y.toSeq)).fold(e => throw e, identity)
    val beta = Array.tabulate(design.cols)(i => fit(i))
    var residual = 0.0
    var r = 0
    while r < design.rows do
      var pred = 0.0
      var j = 0
      while j < design.cols do
        pred += design(r, j) * beta(j)
        j += 1
      residual += (y(r) - pred) * (y(r) - pred)
      r += 1
    (residual, beta)

  test("compact energy, amplitudes and jets equal the direct time-domain fit at alpha = 0"):
    val rng = new scala.util.Random(21L)
    val t = expanded.rows
    val c = expanded.conditionCount
    val m = basis.rank
    val y = Array.fill(t)(rng.nextGaussian())
    val z = new Array[Double](rank)
    var e = 0.0
    var r = 0
    while r < t do
      e += y(r) * y(r)
      var i = 0
      while i < rank do
        z(i) += u(r * rank + i) * y(r)
        i += 1
      r += 1
    val jets = new CompactConditionJets(rHat, rank, c, m, family.dimension)
    val reduction = new ProfileReduction(family.dimension, c)
    val out = new ProfileJetBuffer(family.dimension, c)
    val scratch = new Array[Double](family.jetComponents * basis.fineCount)
    val coeff = new Array[Double](family.jetComponents * m)
    def compact(t1: Double, t2: Double, components: Int): Double =
      val point = family.chart.point(t1, t2).fold(err => fail(err.message), identity)
      basis.coefficientJetInto(point, scratch, coeff, components)
      jets.assemble(z, e, coeff, components)
      assert(reduction.reduce(jets.s, jets.b, jets.g, out))
      out.energy
    for (t1, t2) <- Seq((4.2, math.log(1.1)), (6.5, math.log(2.2)), (7.4, math.log(0.9))) do
      val energy = compact(t1, t2, family.jetComponents)
      val grad = out.gradient.clone()
      val hess = out.hessian.clone()
      val amplitudes = out.amplitudes.clone()
      val (residual, beta) = directFit(t1, t2, y)
      assertEqualsDouble(energy, residual, 1e-9 * residual, s"energy at ($t1,$t2)")
      var i = 0
      while i < c do
        assertEqualsDouble(amplitudes(i), beta(i), 1e-9 * math.max(1.0, math.abs(beta(i))), s"amplitude $i at ($t1,$t2)")
        i += 1
      val h = 1e-4
      val g1 = (compact(t1 + h, t2, 1) - compact(t1 - h, t2, 1)) / (2 * h)
      val g2 = (compact(t1, t2 + h, 1) - compact(t1, t2 - h, 1)) / (2 * h)
      assertEqualsDouble(grad(0), g1, 1e-5 * math.max(1.0, math.abs(g1)), "dE/dtau")
      assertEqualsDouble(grad(1), g2, 1e-5 * math.max(1.0, math.abs(g2)), "dE/dlogSd")
      val hh = 1e-3
      val f0 = compact(t1, t2, 1)
      val h11 = (compact(t1 + hh, t2, 1) - 2 * f0 + compact(t1 - hh, t2, 1)) / (hh * hh)
      assertEqualsDouble(hess(0), h11, 1e-3 * math.max(1.0, math.abs(h11)), "d2E/dtau2")
      assertEquals(JetLayout.components(family.dimension), 6)
