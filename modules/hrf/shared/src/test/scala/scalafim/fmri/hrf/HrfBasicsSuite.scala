package scalafim.fmri.hrf

import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.HrfCombinators.*
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.hrf.TestUtils.*

class HrfBasicsSuite extends munit.FunSuite:

  test("predefined HRFs have correct nbasis/span") {
    assertEquals(Hrfs.Gamma.nbasis, 1)
    assertEquals(Hrfs.Gaussian.nbasis, 1)
    assertEquals(Hrfs.SPMG1.nbasis, 1)
    assertEquals(Hrfs.SPMG2.nbasis, 2)
    assertEquals(Hrfs.SPMG3.nbasis, 3)
  }

  test("evaluate with duration increases response") {
    val h = Hrfs.SPMG1
    val grid = (0 to 100).map(_ * 0.2)
    val res0 = Evaluate.doubles(h, grid, duration = 0.0, precision = 0.2)
    val res2 = Evaluate.doubles(h, grid, duration = 2.0, precision = 0.2)
    val max0 = res0.data.max
    val max2 = res2.data.max
    assert(max2 > max0)
  }

  test("empirical HRF interpolates and extrapolates to 0") {
    val t = (0 to 40).map(_ * 0.5)
    val y = t.map(x => HrfFunctions.gaussianPdf(x.s, mean = 6.0, sd = 2.0))
    val h = Hrfs.empirical(t.map(_.s), y, name = "test_empirical")
    assertEquals(h.name, "test_empirical")
    assertEquals(h.nbasis, 1)

    val newT = (0 to 60).map(_ * 0.3)
    val res = h.evalDoubles(newT)
    assertEquals(res.rows, newT.length)
    assert(res.data.forall(_ >= 0.0))

    val ext = Seq(-2.0, 0.0, 20.0, 22.0)
    val extRes = h.evalDoubles(ext).data
    assertEquals(extRes.head, 0.0)
    assertEquals(extRes.last, 0.0)
  }

  test("bindBasis combines HRFs") {
    val f1 = Hrf.scalar("linear", span = 10.0.s)(t => t.value)
    val f2 = Hrf.scalar("quad", span = 12.0.s)(t => t.value * t.value)
    val f3 = Hrf.scalar("const", span = 8.0.s)(_ => 1.0)
    val combined = HrfCombinators.bindBasis(Seq(f1, f2, f3))
    assertEquals(combined.nbasis, 3)
    assertEquals(combined.span.value, 12.0)
    val tv = Seq(0.0, 1.0, 2.0, 5.0)
    val res = combined.evalDoubles(tv)
    val expected = Mat.fromRows(tv.map(t => Seq(t, t * t, 1.0)))
    assert(res.approxEquals(expected))
  }

  test("lag decorator shifts HRF") {
    val base = Hrfs.SPMG2
    val lagged = base.lag(2.0.s)
    val t = (0 to 40).map(_ * 0.5)
    val resLag = lagged.evalDoubles(t)
    val resBase = base.evalDoubles(t.map(_ - 2.0))
    assert(resLag.approxEquals(resBase, tol = 1e-6))
    assertEquals(lagged.span.value, base.span.value + 2.0)
  }

  test("block decorator widens HRF and can normalize") {
    val base = Hrfs.SPMG1
    val width = 3.0.s
    val blocked = base.block(width, precision = 0.1.s, summate = true, normalize = false)
    val blockedNorm = base.block(width, precision = 0.1.s, summate = true, normalize = true)
    val t = (0 to 300).map(_ * 0.1)
    val resBase = base.evalDoubles(t).data
    val resBlocked = blocked.evalDoubles(t).data
    val resNorm = blockedNorm.evalDoubles(t).data
    val aucBase = resBase.map(math.abs).sum * 0.1
    val aucBlocked = resBlocked.map(math.abs).sum * 0.1
    assert(aucBlocked > aucBase)
    val maxNorm = resNorm.map(math.abs).max
    assert(math.abs(maxNorm - 1.0) < 1e-6)
  }

  test("normalize scales peak to 1") {
    val unnorm = Hrf.scalar("unnorm_gauss")(t => 5.0 * HrfFunctions.gaussianPdf(t, 6.0, 2.0))
    val norm = unnorm.normalize()
    val t = (0 to 200).map(_ * 0.1)
    val rU = unnorm.evalDoubles(t).data
    val rN = norm.evalDoubles(t).data
    val peak = rU.map(math.abs).max
    assert(math.abs(rN.map(math.abs).max - 1.0) < 1e-6)
    assert(TestUtils.maxAbsDiff(rN, rU.map(_ / peak)) < 1e-6)
  }

  test("withCoefficients reduces to single basis") {
    val h = Hrfs.SPMG3
    val coeffs = Array(1.0, 0.2, -0.1)
    val w = h.withCoefficients(coeffs)
    assertEquals(w.nbasis, 1)
    val t = (0 to 200).map(_ * 0.1)
    val resW = w.evalDoubles(t).data
    val resH = h.evalDoubles(t)
    val manual = Array.tabulate(t.length) { i =>
      val row = resH.row(i).data
      row(0) * coeffs(0) + row(1) * coeffs(1) + row(2) * coeffs(2)
    }
    assert(TestUtils.maxAbsDiff(resW, manual) < 1e-6)
  }

  test("withCoefficients supports custom name (R hrf_from_coefficients)") {
    val t = (0 to 40).map(_ * 0.5).toVector
    val weights = Array(0.5, 2.0)
    val combined = Hrfs.SPMG2.withCoefficients(weights, name = Some("combined"))
    assertEquals(combined.nbasis, 1)
    assertEquals(combined.name, "combined")
    val baseMat = Hrfs.SPMG2.evalDoubles(t)
    val expVals = Array.tabulate(t.length) { i =>
      val row = baseMat.row(i).data
      row(0) * weights(0) + row(1) * weights(1)
    }
    val resVals = combined.evalDoubles(t).data
    assert(TestUtils.maxAbsDiff(resVals, expVals) < 1e-6)
  }

  test("gen/bindBasis combines lagged HRFs into a set") {
    val h1 = HrfCombinators.gen(Hrfs.SPMG1, lag = 0.0.s)
    val h2 = HrfCombinators.gen(Hrfs.SPMG1, lag = 2.0.s)
    val h3 = HrfCombinators.gen(Hrfs.SPMG1, lag = 4.0.s)
    val set = HrfCombinators.bindBasis(Seq(h1, h2, h3), name = Some("test_set"))
    assertEquals(set.nbasis, 3)
    assertEquals(set.name, "test_set")
    val t = (0 to 40).map(_ * 0.5).toVector
    val res = set.evalDoubles(t)
    assertEquals(res.cols, 3)
    val peaks = (0 until 3).map { c =>
      val col = res.col(c).data
      t(col.zipWithIndex.maxBy(_._1)._2)
    }
    val diffs = peaks.sliding(2).collect { case Seq(a, b) => b - a }.toVector
    assert(diffs.forall(d => math.abs(d - 2.0) < 0.6))
  }

  test("evaluate summate=false differs from summate=true") {
    val h = Hrfs.SPMG1
    val t = (0 to 100).map(_ * 0.2).toVector
    val sum = Evaluate.doubles(h, t, duration = 2.0, precision = 0.2, summate = true).data
    val mx  = Evaluate.doubles(h, t, duration = 2.0, precision = 0.2, summate = false).data
    assert(TestUtils.maxAbsDiff(sum, mx) > 1e-8)
  }

  test("evaluate precision affects duration convolution") {
    val h = Hrfs.SPMG1
    val t = (0 to 100).map(_ * 0.2).toVector
    val fine = Evaluate.doubles(h, t, duration = 2.0, precision = 0.1).data
    val coarse = Evaluate.doubles(h, t, duration = 2.0, precision = 0.5).data
    assert(TestUtils.maxAbsDiff(fine, coarse) > 1e-6)
  }

  test("normalize in Evaluate preserves matrix dimensions") {
    val grid = Seq(0.0, 1.0, 2.0)
    val res = Evaluate.doubles(Hrfs.SPMG2, grid, normalize = true)
    assertEquals(res.rows, grid.length)
    assertEquals(res.cols, Hrfs.SPMG2.nbasis)
    val single = Evaluate.doubles(Hrfs.SPMG2, Seq(0.0), normalize = true)
    assertEquals(single.rows, 1)
    assertEquals(single.cols, Hrfs.SPMG2.nbasis)
  }

  test("lag/block reject non-finite parameters") {
    intercept[IllegalArgumentException] {
      Hrfs.SPMG1.lag(Double.PositiveInfinity.s)
    }
    intercept[IllegalArgumentException] {
      Hrfs.SPMG1.block(Double.PositiveInfinity.s, precision = 0.1.s, halfLife = 1.0)
    }
    intercept[IllegalArgumentException] {
      Hrfs.SPMG1.block(1.0.s, precision = Double.PositiveInfinity.s, halfLife = 1.0)
    }
    Hrfs.SPMG1.block(1.0.s, precision = 0.1.s, halfLife = Double.PositiveInfinity)
  }
