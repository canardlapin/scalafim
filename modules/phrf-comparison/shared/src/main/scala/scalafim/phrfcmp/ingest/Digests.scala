package scalafim.phrfcmp.ingest

/** Portable pure SHA-256 and CRC-32 over byte arrays (identical on JVM and Scala.js). */
object Digests:

  private val K: Array[Int] = Array(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
  )

  /** Lower-case hex SHA-256 of `data[from, until)`. */
  def sha256Hex(data: Array[Byte], from: Int = 0, until: Int = -1): String =
    val end = if until < 0 then data.length else until
    val len = end - from
    val h = Array(
      0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
    )
    val w = new Array[Int](64)
    val padded = ((len + 9 + 63) / 64) * 64
    val tail = new Array[Byte](padded - (len / 64) * 64)
    val tailStart = (len / 64) * 64
    var i = 0
    while i < len - tailStart do
      tail(i) = data(from + tailStart + i)
      i += 1
    tail(len - tailStart) = 0x80.toByte
    val bits = len.toLong * 8L
    var b = 0
    while b < 8 do
      tail(tail.length - 1 - b) = ((bits >>> (8 * b)) & 0xff).toByte
      b += 1

    def block(src: Array[Byte], off: Int): Unit =
      var t = 0
      while t < 16 do
        val o = off + 4 * t
        w(t) = ((src(o) & 0xff) << 24) | ((src(o + 1) & 0xff) << 16) | ((src(o + 2) & 0xff) << 8) | (src(o + 3) & 0xff)
        t += 1
      while t < 64 do
        val s0 = Integer.rotateRight(w(t - 15), 7) ^ Integer.rotateRight(w(t - 15), 18) ^ (w(t - 15) >>> 3)
        val s1 = Integer.rotateRight(w(t - 2), 17) ^ Integer.rotateRight(w(t - 2), 19) ^ (w(t - 2) >>> 10)
        w(t) = w(t - 16) + s0 + w(t - 7) + s1
        t += 1
      var a = h(0)
      var bb = h(1)
      var c = h(2)
      var d = h(3)
      var e = h(4)
      var f = h(5)
      var g = h(6)
      var hh = h(7)
      t = 0
      while t < 64 do
        val S1 = Integer.rotateRight(e, 6) ^ Integer.rotateRight(e, 11) ^ Integer.rotateRight(e, 25)
        val ch = (e & f) ^ (~e & g)
        val t1 = hh + S1 + ch + K(t) + w(t)
        val S0 = Integer.rotateRight(a, 2) ^ Integer.rotateRight(a, 13) ^ Integer.rotateRight(a, 22)
        val maj = (a & bb) ^ (a & c) ^ (bb & c)
        val t2 = S0 + maj
        hh = g
        g = f
        f = e
        e = d + t1
        d = c
        c = bb
        bb = a
        a = t1 + t2
        t += 1
      h(0) += a
      h(1) += bb
      h(2) += c
      h(3) += d
      h(4) += e
      h(5) += f
      h(6) += g
      h(7) += hh

    var off = from
    while off + 64 <= from + tailStart do
      block(data, off)
      off += 64
    var o2 = 0
    while o2 < tail.length do
      block(tail, o2)
      o2 += 64
    val sb = new StringBuilder(64)
    var k = 0
    while k < 8 do
      var sh = 28
      while sh >= 0 do
        sb.append("0123456789abcdef".charAt((h(k) >>> sh) & 0xf))
        sh -= 4
      k += 1
    sb.toString

  private val crcTable: Array[Int] =
    val t = new Array[Int](256)
    var n = 0
    while n < 256 do
      var c = n
      var k = 0
      while k < 8 do
        c = if (c & 1) != 0 then 0xedb88320 ^ (c >>> 1) else c >>> 1
        k += 1
      t(n) = c
      n += 1
    t

  /** CRC-32 (IEEE) of `data[from, until)` as an unsigned value in a Long. */
  def crc32(data: Array[Byte], from: Int, until: Int): Long =
    var c = -1
    var i = from
    while i < until do
      c = crcTable((c ^ data(i)) & 0xff) ^ (c >>> 8)
      i += 1
    (~c).toLong & 0xffffffffL
