package scalafim.multivar.ir

object PayloadIrFactory:
  def inlineDense(rows: Int, columns: Int, values: Vector[Double]): Either[IrError, PayloadIr.InlineDense] =
    if rows <= 0 || columns <= 0 || values.length != rows * columns || values.exists(!_.isFinite) then
      Left(IrError(RejectionCategory.Malformed, "payload", "invalid inline dense shape or values"))
    else
      Right(PayloadIr.InlineDense(rows, columns, values, IrPayloadHash.dense(rows, columns, values)))

  def inlineSparse(
      rows: Int,
      columns: Int,
      rowIndices: Vector[Int],
      columnIndices: Vector[Int],
      values: Vector[Double]
  ): Either[IrError, PayloadIr.InlineSparse] =
    if rows <= 0 || columns <= 0 || rowIndices.length != columnIndices.length ||
        rowIndices.length != values.length || values.exists(!_.isFinite)
    then Left(IrError(RejectionCategory.Malformed, "payload", "invalid inline sparse shape or values"))
    else if rowIndices.indices.exists(index => rowIndices(index) < 0 || rowIndices(index) >= rows) then
      Left(IrError(RejectionCategory.Malformed, "payload.row_indices", "sparse row index is out of bounds"))
    else if columnIndices.indices.exists(index => columnIndices(index) < 0 || columnIndices(index) >= columns) then
      Left(IrError(RejectionCategory.Malformed, "payload.column_indices", "sparse column index is out of bounds"))
    else
      Right(
        PayloadIr.InlineSparse(
          rows,
          columns,
          rowIndices,
          columnIndices,
          values,
          IrPayloadHash.sparse(rows, columns, rowIndices, columnIndices, values)
        )
      )

  def external(
      uri: String,
      mediaType: String,
      rows: Int,
      columns: Int,
      sha256: String
  ): Either[IrError, PayloadIr.External] =
    if uri.trim.isEmpty || mediaType.trim.isEmpty || rows <= 0 || columns <= 0 then
      Left(IrError(RejectionCategory.Malformed, "payload", "external payload metadata is incomplete"))
    else if !IrPayloadHash.isSha256(sha256) then
      Left(IrError(RejectionCategory.Malformed, "payload.sha256", "expected a lowercase SHA-256 digest"))
    else Right(PayloadIr.External(uri, mediaType, rows, columns, sha256))

object IrPayloadHash:
  def dense(rows: Int, columns: Int, values: Vector[Double]): String =
    val body = values.map(java.lang.Double.toHexString).mkString("|")
    Sha256.digestAscii(s"dense|$rows|$columns|$body")

  def sparse(
      rows: Int,
      columns: Int,
      rowIndices: Vector[Int],
      columnIndices: Vector[Int],
      values: Vector[Double]
  ): String =
    val entries = values.indices.map { index =>
      s"${rowIndices(index)}:${columnIndices(index)}:${java.lang.Double.toHexString(values(index))}"
    }.mkString("|")
    Sha256.digestAscii(s"sparse|$rows|$columns|$entries")

  def isSha256(value: String): Boolean =
    value.length == 64 && value.forall(character => character.isDigit || (character >= 'a' && character <= 'f'))

private object Sha256:
  private val Initial = Array(
    0x6a09e667,
    0xbb67ae85,
    0x3c6ef372,
    0xa54ff53a,
    0x510e527f,
    0x9b05688c,
    0x1f83d9ab,
    0x5be0cd19
  )

  private val K = Array(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
  )

  def digestAscii(input: String): String =
    val bytes = input.map { character =>
      require(character <= 0x7f, "SHA-256 canonical payload input must be ASCII")
      character.toByte
    }.toArray
    val bitLength = bytes.length.toLong * 8L
    val padding = (56 - ((bytes.length + 1) % 64) + 64) % 64
    val message = new Array[Byte](bytes.length + 1 + padding + 8)
    System.arraycopy(bytes, 0, message, 0, bytes.length)
    message(bytes.length) = 0x80.toByte
    var index = 0
    while index < 8 do
      message(message.length - 1 - index) = ((bitLength >>> (index * 8)) & 0xffL).toByte
      index += 1

    val hash = Initial.clone
    val words = new Array[Int](64)
    var offset = 0
    while offset < message.length do
      index = 0
      while index < 16 do
        val base = offset + index * 4
        words(index) =
          ((message(base) & 0xff) << 24) |
            ((message(base + 1) & 0xff) << 16) |
            ((message(base + 2) & 0xff) << 8) |
            (message(base + 3) & 0xff)
        index += 1
      while index < 64 do
        val s0 = rotateRight(words(index - 15), 7) ^ rotateRight(words(index - 15), 18) ^ (words(index - 15) >>> 3)
        val s1 = rotateRight(words(index - 2), 17) ^ rotateRight(words(index - 2), 19) ^ (words(index - 2) >>> 10)
        words(index) = words(index - 16) + s0 + words(index - 7) + s1
        index += 1

      var a = hash(0)
      var b = hash(1)
      var c = hash(2)
      var d = hash(3)
      var e = hash(4)
      var f = hash(5)
      var g = hash(6)
      var h = hash(7)
      index = 0
      while index < 64 do
        val sigma1 = rotateRight(e, 6) ^ rotateRight(e, 11) ^ rotateRight(e, 25)
        val choice = (e & f) ^ (~e & g)
        val temp1 = h + sigma1 + choice + K(index) + words(index)
        val sigma0 = rotateRight(a, 2) ^ rotateRight(a, 13) ^ rotateRight(a, 22)
        val majority = (a & b) ^ (a & c) ^ (b & c)
        val temp2 = sigma0 + majority
        h = g
        g = f
        f = e
        e = d + temp1
        d = c
        c = b
        b = a
        a = temp1 + temp2
        index += 1
      hash(0) += a
      hash(1) += b
      hash(2) += c
      hash(3) += d
      hash(4) += e
      hash(5) += f
      hash(6) += g
      hash(7) += h
      offset += 64

    hash.map(value => f"$value%08x").mkString

  private def rotateRight(value: Int, amount: Int): Int =
    (value >>> amount) | (value << (32 - amount))
