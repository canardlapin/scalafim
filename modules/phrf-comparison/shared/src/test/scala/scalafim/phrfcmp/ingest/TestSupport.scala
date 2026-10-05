package scalafim.phrfcmp.ingest

import java.io.ByteArrayOutputStream

/** In-test builders for `.npy`/`.npz` bytes, including deliberately non-conforming variants. */
object TestSupport:

  private def le16(o: ByteArrayOutputStream, v: Int): Unit =
    o.write(v & 0xff)
    o.write((v >>> 8) & 0xff)

  private def le32(o: ByteArrayOutputStream, v: Long): Unit =
    le16(o, (v & 0xffff).toInt)
    le16(o, ((v >>> 16) & 0xffff).toInt)

  def npy(
      data: Array[Byte], descr: String = "<f8", shape: String = "(1,)", fortran: Boolean = false,
      major: Int = 1, minor: Int = 0, magicOk: Boolean = true, pad: Int = 64
  ): Array[Byte] =
    val dict = s"{'descr': '$descr', 'fortran_order': ${if fortran then "True" else "False"}, 'shape': $shape, }"
    val pre = 10
    val total = ((pre + dict.length + 1 + pad - 1) / pad) * pad
    val header = dict + " " * (total - pre - dict.length - 1) + "\n"
    val o = new ByteArrayOutputStream()
    o.write(if magicOk then 0x93 else 0x92)
    o.write("NUMPY".getBytes("US-ASCII"))
    o.write(major)
    o.write(minor)
    le16(o, header.length)
    o.write(header.getBytes("US-ASCII"))
    o.write(data)
    o.toByteArray

  def f64Bytes(xs: Double*): Array[Byte] =
    val o = new ByteArrayOutputStream()
    xs.foreach { x =>
      val b = java.lang.Double.doubleToLongBits(x)
      le32(o, b & 0xffffffffL)
      le32(o, (b >>> 32) & 0xffffffffL)
    }
    o.toByteArray

  def i32Bytes(xs: Int*): Array[Byte] =
    val o = new ByteArrayOutputStream()
    xs.foreach(x => le32(o, x.toLong & 0xffffffffL))
    o.toByteArray

  def npyF64(shape: Seq[Int], xs: Seq[Double]): Array[Byte] = npy(f64Bytes(xs*), "<f8", shapeText(shape))
  def npyI32(shape: Seq[Int], xs: Seq[Int]): Array[Byte] = npy(i32Bytes(xs*), "<i4", shapeText(shape))

  def shapeText(shape: Seq[Int]): String = shape match
    case Seq()  => "()"
    case Seq(n) => s"($n,)"
    case s      => s.mkString("(", ", ", ")")

  /** Knobs that make an otherwise valid zip non-conforming. */
  final case class ZipOpts(
      method: Int = 0, flags: Int = 0, badCrc: Boolean = false, comment: Boolean = false, zip64: Boolean = false,
      localName: Option[String] = None, swapOffsets: Boolean = false, gap: Int = 0, lead: Int = 0
  )

  def zip(members: Seq[(String, Array[Byte])], opts: ZipOpts = ZipOpts()): Array[Byte] =
    val out = new ByteArrayOutputStream()
    (0 until opts.lead).foreach(_ => out.write(0))
    val offsets = members.zipWithIndex.map { (m, i) =>
      val (name, data) = m
      val off = out.size()
      val crc = Digests.crc32(data, 0, data.length) ^ (if opts.badCrc then 1L else 0L)
      val lnm = opts.localName.getOrElse(name).getBytes("US-ASCII")
      le32(out, 0x04034b50L); le16(out, 20); le16(out, opts.flags); le16(out, opts.method); le16(out, 0); le16(out, 0x21)
      le32(out, crc); le32(out, data.length.toLong); le32(out, data.length.toLong)
      le16(out, lnm.length); le16(out, 0)
      out.write(lnm); out.write(data)
      if i == 0 then (0 until opts.gap).foreach(_ => out.write(0))
      off
    }
    val shown = if opts.swapOffsets then offsets.reverse else offsets
    val central = new ByteArrayOutputStream()
    members.zip(shown).foreach { (m, off) =>
      val (name, data) = m
      val crc = Digests.crc32(data, 0, data.length) ^ (if opts.badCrc then 1L else 0L)
      val nm = name.getBytes("US-ASCII")
      le32(central, 0x02014b50L); le16(central, 0x0314); le16(central, 20); le16(central, opts.flags)
      le16(central, opts.method); le16(central, 0); le16(central, 0x21)
      le32(central, crc); le32(central, data.length.toLong); le32(central, data.length.toLong)
      le16(central, nm.length); le16(central, 0); le16(central, 0); le16(central, 0); le16(central, 0)
      le32(central, 0x81a40000L); le32(central, if opts.zip64 then 0xffffffffL else off.toLong)
      central.write(nm)
    }
    val cdOff = out.size()
    out.write(central.toByteArray)
    le32(out, 0x06054b50L); le16(out, 0); le16(out, 0); le16(out, members.size); le16(out, members.size)
    le32(out, central.size().toLong); le32(out, cdOff.toLong)
    le16(out, if opts.comment then 1 else 0)
    if opts.comment then out.write('x')
    out.toByteArray

  /** Canonical rendering of decoded arrays by exact bit pattern, for cross-platform equality. */
  def fingerprint(npz: Npz): String =
    npz.members.map { m =>
      val body = m.array match
        case NpyArray.F64(s, d) => s"f8$s:" + d.map(x => java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(x))).mkString(",")
        case NpyArray.I32(s, d) => s"i4$s:" + d.mkString(",")
      s"${m.name}=$body"
    }.mkString(";")
