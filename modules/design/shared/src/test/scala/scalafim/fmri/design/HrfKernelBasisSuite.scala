package scalafim.fmri.design

import scalafim.fmri.design.hrf.{HrfKernelBasis, KernelBasisError, KernelBasisProvenance, KernelBasisSpec}
import scalafim.fmri.hrf.{Lag, PositiveSeconds}
import scalafim.fmri.hrf.family.{GaussianFamily, ShapePoint}

class HrfKernelBasisSuite extends munit.FunSuite:

  private val family = GaussianFamily.Default
  private val step = PositiveSeconds(0.1).fold(e => fail(e.message), identity)
  private val spec = KernelBasisSpec(family, step, Vector(26, 21), tolerance = 1e-3, maxRank = 32)
  private lazy val basis = HrfKernelBasis.compile(spec).fold(e => fail(e.message), identity)

  test("compiles a certified basis within the rank budget"):
    assert(basis.rank <= 32, s"rank ${basis.rank}")
    assert(basis.rank >= 8, s"rank ${basis.rank} suspiciously small")
    val cert = basis.certificate
    assert(cert.valueError(basis.rank - 1) <= 1e-3)
    assert(basis.rank == 1 || cert.valueError(basis.rank - 2) > 1e-3, "rank is the smallest that meets the tolerance")
    assert(cert.valueError.zip(cert.valueError.tail).forall { case (a, b) => b <= a + 1e-15 }, "errors are non-increasing in rank")
    assert(cert.tailEnergyBound < 1e-5, s"tail ${cert.tailEnergyBound}")
    assertEquals(basis.singularValues.length, basis.rank)
    assert(basis.singularValues.zip(basis.singularValues.tail).forall { case (a, b) => b <= a })
    assertEquals(basis.fineCount, 241)

  test("refuses when the budget cannot meet the tolerance"):
    HrfKernelBasis.compile(spec.copy(tolerance = 1e-6, maxRank = 5)) match
      case Left(KernelBasisError.BudgetExceeded(maxRank, tolerance, achieved)) =>
        assertEquals(maxRank, 5)
        assertEqualsDouble(tolerance, 1e-6, 0.0)
        assert(achieved > 1e-6)
      case other => fail(s"expected BudgetExceeded, got $other")
    assert(HrfKernelBasis.compile(spec.copy(nodesPerAxis = Vector(26))).isLeft)
    assert(HrfKernelBasis.compile(spec.copy(nodesPerAxis = Vector(1, 21))).isLeft)
    assert(HrfKernelBasis.compile(spec.copy(tolerance = 0.0)).isLeft)

  test("the kernel exposes the basis as a compact multi-column Hrf and ResponseBasis"):
    val kernel = basis.kernel
    assertEquals(kernel.nbasis, basis.rank)
    assertEquals(kernel.support.horizonOption.map(_.value), Some(24.0))
    var i = 0
    while i < basis.fineCount do
      val values = kernel(Lag(basis.lags(i))).data
      var j = 0
      while j < basis.rank do
        assertEqualsDouble(values(j), basis.value(j, i), 1e-12, s"phi($j) at lag ${basis.lags(i)}")
        j += 1
      i += 1
    val mid = kernel(Lag(0.35)).data
    var j = 0
    while j < basis.rank do
      assertEqualsDouble(mid(j), 0.5 * (basis.value(j, 3) + basis.value(j, 4)), 1e-12)
      j += 1
    assert(kernel(Lag(-0.1)).data.forall(_ == 0.0))
    assert(kernel(Lag(24.5)).data.forall(_ == 0.0))
    assertEquals(basis.responseBasis.dimension, basis.rank)
    assertEquals(basis.responseBasis.elements.length, basis.rank)
    assert(basis.provenance.canonical.contains(s"|rank=${basis.rank}|"))
    assert(basis.provenance.canonical.contains(s"|maxRank=${basis.spec.maxRank}|heldOutPoints=${basis.spec.heldOutPoints}|"))
    assert(basis.provenance.canonical.startsWith("kernel-basis/v2|family=8:gaussian"))

  test("kernel basis provenance has a platform-independent IEEE and string-framed golden"):
    val provenance = KernelBasisProvenance(
      family = "gauss|ian;=",
      chart = Vector(("axis,|[]:=", -0.0, 24.0)),
      horizonSeconds = -0.0,
      fineStepSeconds = 0.1,
      nodesPerAxis = Vector(2, 21),
      includeDerivatives = true,
      tolerance = 1e-3,
      maxRank = 32,
      heldOutPoints = 300,
      rank = 4,
      seed = 11L
    )
    assertEquals(
      provenance.canonical,
      "kernel-basis/v2|family=11:gauss|ian;=|chart=chart(76:axis(10:axis,|[]:=,25:bits:-9223372036854775808,24:bits:4627448617123184640))|horizon=bits:-9223372036854775808|step=bits:4591870180066957722|nodes=nodes(1:2,2:21)|derivatives=true|tolerance=bits:4562254508917369340|maxRank=32|heldOutPoints=300|rank=4|seed=11"
    )

  test("matrix identity is dimensions plus a portable FNV-1a digest of the IEEE bits"):
    val values = Array(1.0, -0.0, 0.1, -1.0, 1e-6, 2.5)
    assertEquals(KernelBasisProvenance.matrix(2, 3, values), "matrix(1:2,1:3,24:fnv1a64:da488cb4783cb320)")
    assertEquals(KernelBasisProvenance.matrix(3, 2, values), "matrix(1:3,1:2,24:fnv1a64:da488cb4783cb320)")
    assertEquals(KernelBasisProvenance.matrix(1, 2, Array(0.0, -0.0)), "matrix(1:1,1:2,24:fnv1a64:881f9fb960fe8ae5)")
    assertEquals(KernelBasisProvenance.matrix(1, 2, Array(-0.0, 0.0)), "matrix(1:1,1:2,24:fnv1a64:d2f570ef13845ae5)")
    assertEquals(KernelBasisProvenance.option(None), "none")
    assertEquals(KernelBasisProvenance.option(Some("")), "some(0:)")

  test("coefficient jets match finite differences and reconstruct within the certificate"):
    val point = family.chart.point(5.3, math.log(1.9)).fold(e => fail(e.message), identity)
    val scratch = new Array[Double](family.jetComponents * basis.fineCount)
    val jet = new Array[Double](family.jetComponents * basis.rank)
    basis.coefficientJetInto(point, scratch, jet, family.jetComponents)
    val h = 1e-4
    val plus = new Array[Double](basis.rank)
    val minus = new Array[Double](basis.rank)
    val kernelScratch = new Array[Double](basis.fineCount)
    basis.coefficientsInto(ShapePoint.unsafe(Vector(5.3 + h, math.log(1.9))), kernelScratch, plus)
    basis.coefficientsInto(ShapePoint.unsafe(Vector(5.3 - h, math.log(1.9))), kernelScratch, minus)
    var j = 0
    while j < basis.rank do
      assertEqualsDouble((plus(j) - minus(j)) / (2 * h), jet(basis.rank + j), 1e-6, s"d c_$j / d tau")
      j += 1
    // reconstruction error at a held-out shape is within the certified bound
    val c = new Array[Double](basis.rank)
    basis.coefficientsInto(point, kernelScratch, c)
    val rebuilt = new Array[Double](basis.fineCount)
    basis.reconstructInto(c, rebuilt)
    val truth = new Array[Double](basis.fineCount)
    family.evalInto(basis.lags, point, truth)
    var num = 0.0
    var den = 0.0
    var i = 0
    while i < basis.fineCount do
      num += (rebuilt(i) - truth(i)) * (rebuilt(i) - truth(i))
      den += truth(i) * truth(i)
      i += 1
    val rel = math.sqrt(num / den)
    assert(rel <= 2.0 * basis.certificate.valueError(basis.rank - 1), s"reconstruction error $rel")

  test("compilation is deterministic"):
    val again = HrfKernelBasis.compile(spec).fold(e => fail(e.message), identity)
    assertEquals(again.rank, basis.rank)
    assertEquals(again.provenance.canonical, basis.provenance.canonical)
    var j = 0
    while j < basis.rank do
      var i = 0
      while i < basis.fineCount do
        assertEquals(again.value(j, i), basis.value(j, i))
        i += 1
      j += 1

  test("tail-grid refusals remain typed and precede basis allocation and family evaluation"):
    val fine = PositiveSeconds(1e-10).fold(e => fail(e.message), identity)
    HrfKernelBasis.compile(spec.copy(fineStep = fine, nodesPerAxis = Vector(2, 2), heldOutPoints = 1)) match
      case Left(KernelBasisError.Summary(error: scalafim.fmri.hrf.family.FamilySummaryError.SampleLimitExceeded)) =>
        assert(error.requested > error.maximum.toDouble)
      case other => fail(s"expected typed tail admission refusal, got $other")
    val unsafe = PositiveSeconds.unsafe(scalafim.fmri.hrf.Seconds.unsafe(Double.NaN))
    assert(HrfKernelBasis.compile(spec.copy(fineStep = unsafe)).swap.exists:
      case KernelBasisError.Summary(_: scalafim.fmri.hrf.family.FamilySummaryError.InvalidPrecision) => true
      case _ => false
    )

  test("certificate diagnostic failures propagate through compile as typed errors"):
    import scalafim.fmri.hrf.family.{FamilySummaryError, NormalizationRule, ParametricHrfFamily, ShapeChart, ShapeSummary}
    import scalafim.fmri.hrf.{Hrf, HrfDescriptor, HrfKind}
    val expected = FamilySummaryError.EvaluationFailed("certificate validation failure")
    val failing = new ParametricHrfFamily:
      def name: String = family.name
      def kind: HrfKind = family.kind
      def chart: ShapeChart = family.chart
      def horizon: PositiveSeconds = family.horizon
      def supports(rule: NormalizationRule): Boolean = family.supports(rule)
      def libraryNormalization: NormalizationRule = family.libraryNormalization
      def evalInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit = family.evalInto(lags, point, out)
      def jetInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit = family.jetInto(lags, point, out)
      def scaleJetInto(rule: NormalizationRule, point: ShapePoint, out: Array[Double]): Unit = family.scaleJetInto(rule, point, out)
      def summaries(point: ShapePoint): ShapeSummary = family.summaries(point)
      def descriptor(point: ShapePoint): HrfDescriptor = family.descriptor(point)
      def toHrf(point: ShapePoint): Hrf = family.toHrf(point)
      override def tailRelativeEnergyEither(point: ShapePoint, precision: PositiveSeconds, extent: Double): Either[FamilySummaryError, Double] = Left(expected)
    assertEquals(HrfKernelBasis.compile(spec.copy(family = failing, nodesPerAxis = Vector(2, 2), fineStep = PositiveSeconds(1.0).toOption.get, heldOutPoints = 1, includeDerivatives = false)), Left(KernelBasisError.Summary(expected)))
