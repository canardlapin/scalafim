package scalafim.fmri.hrf

import scalafim.fmri.hrf.linalg.Fft

/** The FFT convolution has to agree with the definition it is an optimization
  * of, including at non-power-of-two and degenerate lengths.
  */
class FftSuite extends munit.FunSuite:

  private def naiveConvolve(a: Array[Double], b: Array[Double]): Array[Double] =
    if a.isEmpty || b.isEmpty then Array.empty
    else
      val out = new Array[Double](a.length + b.length - 1)
      var i = 0
      while i < a.length do
        var j = 0
        while j < b.length do
          out(i + j) += a(i) * b(j)
          j += 1
        i += 1
      out

  private def deterministic(n: Int, seed: Int): Array[Double] =
    // A fixed, non-trivial sequence; no RNG so the test is reproducible.
    Array.tabulate(n)(i => math.sin(0.7 * i + seed) + 0.3 * math.cos(0.13 * i * seed))

  test("convolveReal matches the direct definition across lengths"):
    val cases = Seq((1, 1), (1, 8), (8, 1), (5, 3), (16, 16), (17, 9), (64, 33), (100, 7), (129, 128))
    cases.foreach { case (n, m) =>
      val a = deterministic(n, 1)
      val b = deterministic(m, 2)
      val got = Fft.convolveReal(a, b)
      val want = naiveConvolve(a, b)
      assertEquals(got.length, want.length, s"length for ($n, $m)")
      var i = 0
      while i < want.length do
        assertEqualsDouble(got(i), want(i), 1e-9 * (1.0 + math.abs(want(i))), s"($n,$m) at $i")
        i += 1
    }

  test("convolveReal handles empty input"):
    assertEquals(Fft.convolveReal(Array.empty, Array(1.0)).length, 0)
    assertEquals(Fft.convolveReal(Array(1.0), Array.empty).length, 0)

  test("a delta convolution reproduces the signal"):
    val signal = deterministic(40, 3)
    val delta = Array(0.0, 0.0, 1.0)
    val got = Fft.convolveReal(signal, delta)
    var i = 0
    while i < signal.length do
      assertEqualsDouble(got(i + 2), signal(i), 1e-10, s"shifted delta at $i")
      i += 1

  test("the forward transform inverts"):
    val n = 64
    val re = deterministic(n, 5)
    val im = deterministic(n, 7)
    val re0 = re.clone
    val im0 = im.clone
    Fft.transform(re, im, inverse = false)
    Fft.transform(re, im, inverse = true)
    var i = 0
    while i < n do
      assertEqualsDouble(re(i), re0(i), 1e-10, s"re at $i")
      assertEqualsDouble(im(i), im0(i), 1e-10, s"im at $i")
      i += 1

  test("transform rejects a non-power-of-two length"):
    intercept[IllegalArgumentException](Fft.transform(new Array[Double](3), new Array[Double](3), false))
    intercept[IllegalArgumentException](Fft.transform(new Array[Double](4), new Array[Double](8), false))

  test("nextPow2 rounds up"):
    Seq(1 -> 1, 2 -> 2, 3 -> 4, 5 -> 8, 64 -> 64, 65 -> 128).foreach { case (in, want) =>
      assertEquals(Fft.nextPow2(in), want, s"nextPow2($in)")
    }
