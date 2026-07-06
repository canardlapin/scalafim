package scalafim.fmri.hrf.linalg

private[hrf] object Fft:

  private def isPowerOfTwo(n: Int): Boolean = n > 0 && (n & (n - 1)) == 0

  def nextPow2(n: Int): Int =
    var v = n
    var p = 1
    while p < v do p <<= 1
    p

  def fft(input: Array[Complex], inverse: Boolean = false): Array[Complex] =
    val n = input.length
    require(isPowerOfTwo(n), s"FFT length must be power of two, got $n")
    val out = input.clone

    // bit-reversal permutation
    var j = 0
    var i = 1
    while i < n do
      var bit = n >>> 1
      while j >= bit do
        j -= bit
        bit >>>= 1
      j += bit
      if i < j then
        val tmp = out(i)
        out(i) = out(j)
        out(j) = tmp
      i += 1

    var len = 2
    while len <= n do
      val ang = 2.0 * math.Pi / len * (if inverse then 1.0 else -1.0)
      val wlen = Complex.expi(ang)
      var i0 = 0
      while i0 < n do
        var w = Complex(1.0, 0.0)
        var k = 0
        val half = len >>> 1
        while k < half do
          val u = out(i0 + k)
          val v = out(i0 + k + half) * w
          out(i0 + k) = u + v
          out(i0 + k + half) = u - v
          w = w * wlen
          k += 1
        i0 += len
      len <<= 1

    if inverse then
      val invN = 1.0 / n.toDouble
      var k = 0
      while k < n do
        out(k) = out(k).scale(invN)
        k += 1

    out

  def convolveReal(a: Array[Double], b: Array[Double]): Array[Double] =
    val n = a.length
    val m = b.length
    val nFFT = nextPow2(n + m - 1)
    val fa = Array.fill(nFFT)(Complex.zero)
    val fb = Array.fill(nFFT)(Complex.zero)
    var i = 0
    while i < n do
      fa(i) = Complex(a(i), 0.0)
      i += 1
    i = 0
    while i < m do
      fb(i) = Complex(b(i), 0.0)
      i += 1
    val Fa = fft(fa)
    val Fb = fft(fb)
    val Fc = Array.ofDim[Complex](nFFT)
    i = 0
    while i < nFFT do
      Fc(i) = Fa(i) * Fb(i)
      i += 1
    val c = fft(Fc, inverse = true)
    c.map(_.re)
