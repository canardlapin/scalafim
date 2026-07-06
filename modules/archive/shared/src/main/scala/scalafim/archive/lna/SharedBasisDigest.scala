package scalafim.archive.lna

private[lna] object SharedBasisDigest:
  def sha256Hex(write: Writer => Unit): String =
    val writer = Writer()
    write(writer)
    hex(writer.digest())

  private def hex(bytes: Array[Byte]): String =
    bytes.map(b => f"${b & 0xff}%02x").mkString

  final class Writer private[SharedBasisDigest] ():
    private val sha = Sha256()

    def string(value: String): Unit =
      val bytes = value.getBytes("UTF-8")
      intLE(bytes.length)
      sha.update(bytes)

    def intLE(value: Int): Unit =
      sha.updateByte(value)
      sha.updateByte(value >>> 8)
      sha.updateByte(value >>> 16)
      sha.updateByte(value >>> 24)

    def doubleLE(value: Double): Unit =
      longLE(java.lang.Double.doubleToLongBits(value))

    def byte(value: Int): Unit =
      sha.updateByte(value)

    def digest(): Array[Byte] =
      sha.digest()

    private def longLE(value: Long): Unit =
      sha.updateByte(value.toInt)
      sha.updateByte((value >>> 8).toInt)
      sha.updateByte((value >>> 16).toInt)
      sha.updateByte((value >>> 24).toInt)
      sha.updateByte((value >>> 32).toInt)
      sha.updateByte((value >>> 40).toInt)
      sha.updateByte((value >>> 48).toInt)
      sha.updateByte((value >>> 56).toInt)

  private final class Sha256:
    private val state = Array(
      0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
      0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
    )
    private val block = new Array[Byte](64)
    private val words = new Array[Int](64)
    private var blockLength = 0
    private var totalLength = 0L
    private var finished = false

    def update(bytes: Array[Byte]): Unit =
      var i = 0
      while i < bytes.length do
        updateByte(bytes(i))
        i += 1

    def updateByte(value: Int): Unit =
      require(!finished, "SHA-256 digest is already finalized")
      block(blockLength) = value.toByte
      blockLength += 1
      totalLength += 1L
      if blockLength == 64 then
        compress()
        blockLength = 0

    def digest(): Array[Byte] =
      require(!finished, "SHA-256 digest is already finalized")
      val bitLength = totalLength * 8L
      updateByte(0x80)
      while blockLength != 56 do
        if blockLength == 64 then
          compress()
          blockLength = 0
        else updateByte(0)

      var shift = 56
      while shift >= 0 do
        updateByte((bitLength >>> shift).toInt)
        shift -= 8

      val out = new Array[Byte](32)
      var i = 0
      while i < state.length do
        val value = state(i)
        out(i * 4) = (value >>> 24).toByte
        out(i * 4 + 1) = (value >>> 16).toByte
        out(i * 4 + 2) = (value >>> 8).toByte
        out(i * 4 + 3) = value.toByte
        i += 1
      finished = true
      out

    private def compress(): Unit =
      var i = 0
      while i < 16 do
        val j = i * 4
        words(i) =
          ((block(j) & 0xff) << 24) |
            ((block(j + 1) & 0xff) << 16) |
            ((block(j + 2) & 0xff) << 8) |
            (block(j + 3) & 0xff)
        i += 1
      while i < 64 do
        words(i) = smallSigma1(words(i - 2)) + words(i - 7) + smallSigma0(words(i - 15)) + words(i - 16)
        i += 1

      var a = state(0)
      var b = state(1)
      var c = state(2)
      var d = state(3)
      var e = state(4)
      var f = state(5)
      var g = state(6)
      var h = state(7)

      i = 0
      while i < 64 do
        val t1 = h + bigSigma1(e) + choose(e, f, g) + K(i) + words(i)
        val t2 = bigSigma0(a) + majority(a, b, c)
        h = g
        g = f
        f = e
        e = d + t1
        d = c
        c = b
        b = a
        a = t1 + t2
        i += 1

      state(0) += a
      state(1) += b
      state(2) += c
      state(3) += d
      state(4) += e
      state(5) += f
      state(6) += g
      state(7) += h

    private inline def rotateRight(value: Int, bits: Int): Int =
      (value >>> bits) | (value << (32 - bits))

    private inline def choose(x: Int, y: Int, z: Int): Int =
      (x & y) ^ (~x & z)

    private inline def majority(x: Int, y: Int, z: Int): Int =
      (x & y) ^ (x & z) ^ (y & z)

    private inline def bigSigma0(x: Int): Int =
      rotateRight(x, 2) ^ rotateRight(x, 13) ^ rotateRight(x, 22)

    private inline def bigSigma1(x: Int): Int =
      rotateRight(x, 6) ^ rotateRight(x, 11) ^ rotateRight(x, 25)

    private inline def smallSigma0(x: Int): Int =
      rotateRight(x, 7) ^ rotateRight(x, 18) ^ (x >>> 3)

    private inline def smallSigma1(x: Int): Int =
      rotateRight(x, 17) ^ rotateRight(x, 19) ^ (x >>> 10)

  private val K = Array(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
  )
