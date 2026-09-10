package scalafim.fmri.design

import scalafim.fmri.design.hrf.{HrfKernelBasis, KernelBasisError, KernelBasisSpec}
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
    assert(basis.provenance.canonical.contains(s"rank=${basis.rank}"))
    assert(basis.provenance.canonical.startsWith("kernel-basis/v1|family=gaussian"))

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
