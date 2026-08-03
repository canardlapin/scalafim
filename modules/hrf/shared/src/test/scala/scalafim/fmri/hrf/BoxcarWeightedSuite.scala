package scalafim.fmri.hrf

import scalafim.fmri.hrf.Hrfs
import scalafim.fmri.hrf.regressor.Regressor

class BoxcarWeightedSuite extends munit.FunSuite:

  test("boxcar HRF structure") {
    val h = Hrfs.boxcar(5.0.s)
    assertEquals(h.nbasis, 1)
    assertEquals(h.span.value, 5.0)
    assert(h.name.contains("boxcar"))
  }

  test("boxcar evaluates inside/outside window") {
    val h = Hrfs.boxcar(5.0.s, amplitude = 2.0)
    val t = (0 to 24).map(i => -2.0 + i * 0.5).toVector
    val res = h.evalDoubles(t).data.toVector
    val exp = t.map(x => if x >= 0.0 && x < 5.0 then 2.0 else 0.0)
    assertEquals(res, exp)
  }

  test("boxcar normalization approximates unit area") {
    val t = (0 to 100).map(_ * 0.1).toVector
    val dt = 0.1

    val unnorm = Hrfs.boxcar(5.0.s, normalize = false)
    val rU = unnorm.evalDoubles(t).data
    val aucU = rU.sum * dt
    assert(math.abs(aucU - 5.0) < 0.1)

    val norm = Hrfs.boxcar(5.0.s, normalize = true)
    val rN = norm.evalDoubles(t).data
    val aucN = rN.sum * dt
    assert(math.abs(aucN - 1.0) < 0.1)
    assert(math.abs(rN.max - 0.2) < 1e-10)
  }

  test("boxcar validates width") {
    intercept[IllegalArgumentException] {
      Hrfs.boxcar(0.0.s)
    }
    intercept[IllegalArgumentException] {
      Hrfs.boxcar((-5.0).s)
    }
  }

  test("boxcar works with regressor") {
    val h = Hrfs.boxcar(5.0.s)
    val reg = Regressor(Seq(0.0, 20.0, 40.0), h)
    val t = (0 to 120).map(_ * 0.5).toVector
    val res = Regressor.evaluate(reg, t).data.toVector
    def at(x: Double): Double = res(t.indexOf(x))
    assert(at(2.0) > 0.0)
    assert(at(22.0) > 0.0)
    assert(at(42.0) > 0.0)
  }

  test("weighted HRF structure from width") {
    val weights = Vector(0.0, 1.0, 2.0, 2.0, 1.0, 0.0)
    val h = Hrfs.weighted(weights, width = Some(5.0.s))
    assertEquals(h.nbasis, 1)
    assertEquals(h.span.value, 5.0)
    assert(h.name.contains("weighted"))
  }

  test("weighted HRF structure from times") {
    val weights = Vector(0.0, 1.0, 2.0, 2.0, 1.0, 0.0)
    val times: Vector[Seconds] = Vector(0.0, 1.0, 2.0, 3.0, 4.0, 5.0).map(_.s)
    val h = Hrfs.weighted(weights, times = Some(times))
    assertEquals(h.span.value, 5.0)
  }

  test("weighted constant method produces steps") {
    val times: Vector[Seconds] = Vector(0.0, 2.0, 4.0, 6.0).map(_.s)
    val weights = Vector(1.0, 2.0, 3.0, 0.0)
    val h = Hrfs.weighted(weights, times = Some(times), method = Hrfs.WeightedMethod.Constant)
    val t = (0 to 16).map(_ * 0.5).toVector
    val res = h.evalDoubles(t).data.toVector
    def v(x: Double): Double = res(t.indexOf(x))
    assertEquals(v(0.0), 1.0)
    assertEquals(v(1.0), 1.0)
    assertEquals(v(2.0), 2.0)
    assertEquals(v(3.0), 2.0)
    assertEquals(v(4.0), 3.0)
    assertEquals(v(5.0), 3.0)
    assertEquals(v(7.0), 0.0)
  }

  test("weighted linear method interpolates") {
    val times: Vector[Seconds] = Vector(0.0, 2.0, 4.0).map(_.s)
    val weights = Vector(0.0, 1.0, 0.0)
    val h = Hrfs.weighted(weights, times = Some(times), method = Hrfs.WeightedMethod.Linear)
    val t = (0 to 10).map(_ * 0.5).toVector
    val res = h.evalDoubles(t).data.toVector
    def v(x: Double): Double = res(t.indexOf(x))
    assertEquals(v(0.0), 0.0)
    assertEquals(v(1.0), 0.5)
    assertEquals(v(2.0), 1.0)
    assertEquals(v(3.0), 0.5)
    assertEquals(v(4.0), 0.0)
    assertEquals(v(5.0), 0.0)
  }

  test("weighted width infers evenly spaced times") {
    val weights = Vector(1.0, 2.0, 3.0, 0.0)
    val h = Hrfs.weighted(weights, width = Some(6.0.s), method = Hrfs.WeightedMethod.Constant)
    val t = (0 to 16).map(_ * 0.5).toVector
    val res = h.evalDoubles(t).data.toVector
    def v(x: Double): Double = res(t.indexOf(x))
    assertEquals(v(0.0), 1.0)
    assertEquals(v(1.0), 1.0)
    assertEquals(v(2.0), 2.0)
    assertEquals(v(4.0), 3.0)
    assertEquals(v(7.0), 0.0)
  }

  test("weighted normalization constant integrates to ~1") {
    val times: Vector[Seconds] = Vector(0.0, 1.0, 2.0, 3.0, 4.0).map(_.s)
    val weights = Vector(1.0, 2.0, 3.0, 2.0, 1.0)
    val h = Hrfs.weighted(weights, times = Some(times), method = Hrfs.WeightedMethod.Constant, normalize = true)
    val t = (0 to 399).map(_ * 0.01).toVector
    val res = h.evalDoubles(t).data
    val integral = res.sum * 0.01
    assert(math.abs(integral - 1.0) < 0.1)
  }

  test("weighted normalization linear integrates to ~1") {
    val times: Vector[Seconds] = Vector(0.0, 2.0, 4.0).map(_.s)
    val weights = Vector(0.0, 2.0, 0.0)
    val h = Hrfs.weighted(weights, times = Some(times), method = Hrfs.WeightedMethod.Linear, normalize = true)
    val t = (0 to 400).map(_ * 0.01).toVector
    val res = h.evalDoubles(t).data
    val integral = res.sum * 0.01
    assert(math.abs(integral - 1.0) < 0.05)
  }

  test("weighted validates inputs") {
    val times: Vector[Seconds] = Vector(0.0, 1.0, 2.0, 3.0, 4.0).map(_.s)
    intercept[IllegalArgumentException] {
      Hrfs.weighted(Vector(1.0, 2.0, 3.0, 4.0), times = Some(times))
    }
    intercept[IllegalArgumentException] {
      Hrfs.weighted(Vector(1.0), width = Some(5.0.s))
    }
    intercept[IllegalArgumentException] {
      Hrfs.weighted(Vector(1.0, 2.0, 3.0))
    }
    intercept[IllegalArgumentException] {
      Hrfs.weighted(Vector(1.0, 2.0, 3.0), times = Some(Vector(0.0.s, 2.0.s, 2.0.s)))
    }
    intercept[IllegalArgumentException] {
      Hrfs.weighted(Vector(1.0, 2.0, 3.0), times = Some(Vector((-1.0).s, 0.0.s, 1.0.s)))
    }
  }

  test("weighted handles sub-second intervals") {
    val times = (0 to 20).map(_ * 0.25).toVector
    val weights = times.map(t => HrfFunctions.gaussianPdf(Lag(t), 2.5, 1.0))
    val timesSec: Vector[Seconds] = times.map(Seconds(_)).toVector
    val h = Hrfs.weighted(weights.toVector, times = Some(timesSec), method = Hrfs.WeightedMethod.Linear)
    val grid = (0 to 90).map(i => -1.0 + i * 0.1).toVector
    val res = h.evalDoubles(grid).data.toVector
    val peakTime = grid(res.zipWithIndex.maxBy(_._1)._2)
    assert(math.abs(peakTime - 2.5) < 0.5)
    assert(res.zip(grid).forall { case (v, g) => if g < 0.0 || g > 5.0 then v == 0.0 else true })
  }

  test("weighted works with regressor") {
    val h = Hrfs.weighted(
      weights = Vector(0.1, 0.2, 0.3, 0.3, 0.2, 0.1),
      width = Some(6.0.s),
      normalize = true
    )
    val reg = Regressor(Seq(0.0, 30.0), h)
    val t = (0 to 100).map(_ * 0.5).toVector
    val res = Regressor.evaluate(reg, t).data.toVector
    def anyPos(lo: Double, hi: Double): Boolean =
      t.zip(res).exists { case (tt, v) => tt >= lo && tt <= hi && v > 0.0 }
    assert(anyPos(0.0, 6.0))
    assert(anyPos(30.0, 36.0))
  }

  test("boxcar and weighted uniform weights are equivalent") {
    val hBox = Hrfs.boxcar(4.0.s, amplitude = 1.0)
    val times: Vector[Seconds] = Vector(0.0, 1.0, 2.0, 3.0, 4.0).map(_.s)
    val weights = Vector(1.0, 1.0, 1.0, 1.0, 0.0)
    val hWt = Hrfs.weighted(weights, times = Some(times), method = Hrfs.WeightedMethod.Constant)
    val t = (0 to 12).map(_ * 0.5).toVector
    val a = hBox.evalDoubles(t).data
    val b = hWt.evalDoubles(t).data

    def corr(x: Array[Double], y: Array[Double]): Double =
      val mx = x.sum / x.length
      val my = y.sum / y.length
      val num = x.zip(y).map { case (xi, yi) => (xi - mx) * (yi - my) }.sum
      val denx = math.sqrt(x.map(xi => (xi - mx) * (xi - mx)).sum)
      val deny = math.sqrt(y.map(yi => (yi - my) * (yi - my)).sum)
      if denx == 0.0 || deny == 0.0 then 0.0 else num / (denx * deny)

    assert(corr(a, b) > 0.99)
  }
