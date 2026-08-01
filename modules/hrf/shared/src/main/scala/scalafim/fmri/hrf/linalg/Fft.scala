package scalafim.fmri.hrf.linalg

/** Radix-2 FFT on split real/imaginary `Array[Double]`.
  *
  * The arrays are deliberately primitive and split rather than an
  * `Array[Complex]`. `Complex` is a case class, so an array of them is an array
  * of *references*: every butterfly allocates three short-lived objects, and
  * every element access is a pointer chase. Measured on a 4800-scan,
  * 3-basis, precision-0.1 regressor, that cost the FFT path 127 ms against
  * 3.6 ms for the direct convolution it was supposed to beat — an O(n log n)
  * algorithm losing to an O(n·m) one by 35x purely on allocation.
  */
private[hrf] object Fft:

  def nextPow2(n: Int): Int =
    var p = 1
    while p < n do p <<= 1
    p

  /** In-place radix-2 decimation-in-time transform of `(re, im)`.
    *
    * `re.length` must be a power of two. Twiddles are computed per stage by
    * recurrence, so the inner loop neither allocates nor calls trigonometry.
    */
  def transform(re: Array[Double], im: Array[Double], inverse: Boolean): Unit =
    val n = re.length
    require(n > 0 && (n & (n - 1)) == 0, s"FFT length must be a power of two, got $n")
    require(im.length == n, s"real and imaginary parts must match, got $n and ${im.length}")

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
        val tr = re(i); re(i) = re(j); re(j) = tr
        val ti = im(i); im(i) = im(j); im(j) = ti
      i += 1

    val sign = if inverse then 1.0 else -1.0
    var len = 2
    while len <= n do
      val ang = 2.0 * math.Pi / len * sign
      val wlenRe = math.cos(ang)
      val wlenIm = math.sin(ang)
      var i0 = 0
      val half = len >>> 1
      while i0 < n do
        var wRe = 1.0
        var wIm = 0.0
        var k = 0
        while k < half do
          val a = i0 + k
          val b = a + half
          val xr = re(b)
          val xi = im(b)
          val vr = xr * wRe - xi * wIm
          val vi = xr * wIm + xi * wRe
          val ur = re(a)
          val ui = im(a)
          re(a) = ur + vr
          im(a) = ui + vi
          re(b) = ur - vr
          im(b) = ui - vi
          val nextRe = wRe * wlenRe - wIm * wlenIm
          wIm = wRe * wlenIm + wIm * wlenRe
          wRe = nextRe
          k += 1
        i0 += len
      len <<= 1

    if inverse then
      val invN = 1.0 / n.toDouble
      var k = 0
      while k < n do
        re(k) *= invN
        im(k) *= invN
        k += 1

  /** Linear convolution of two real signals, returned at full length
    * `a.length + b.length - 1`.
    */
  def convolveReal(a: Array[Double], b: Array[Double]): Array[Double] =
    val n = a.length
    val m = b.length
    if n == 0 || m == 0 then return new Array[Double](0)
    val full = n + m - 1
    val nFFT = nextPow2(full)

    val ar = new Array[Double](nFFT)
    val ai = new Array[Double](nFFT)
    val br = new Array[Double](nFFT)
    val bi = new Array[Double](nFFT)
    System.arraycopy(a, 0, ar, 0, n)
    System.arraycopy(b, 0, br, 0, m)

    transform(ar, ai, inverse = false)
    transform(br, bi, inverse = false)

    var i = 0
    while i < nFFT do
      val pr = ar(i) * br(i) - ai(i) * bi(i)
      val pi = ar(i) * bi(i) + ai(i) * br(i)
      ar(i) = pr
      ai(i) = pi
      i += 1

    transform(ar, ai, inverse = true)

    val out = new Array[Double](full)
    System.arraycopy(ar, 0, out, 0, full)
    out
