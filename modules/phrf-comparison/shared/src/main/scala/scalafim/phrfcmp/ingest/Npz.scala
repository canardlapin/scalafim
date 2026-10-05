package scalafim.phrfcmp.ingest

import scala.util.matching.Regex

/** Why a byte sequence is not an acceptable generator `.npz`. Every case refuses; nothing is coerced. */
enum NpzError:
  case Truncated(what: String)
  case NotZip(detail: String)
  case Zip64OrMultiDisk
  case Encrypted(member: String)
  case CompressedMember(member: String, method: Int)
  case DataDescriptor(member: String)
  case SizeMismatch(member: String)
  case CrcMismatch(member: String)
  case DuplicateMember(member: String)
  case UnknownMember(member: String)
  case LocalHeaderMismatch(member: String)
  case BadMagic(member: String)
  case UnsupportedNpyVersion(member: String, major: Int, minor: Int)
  case MalformedHeader(member: String, detail: String)
  case UnsupportedDtype(member: String, descr: String)
  case FortranOrder(member: String)
  case ShapeOverflow(member: String)
  case DataLengthMismatch(member: String, expected: Long, found: Long)

  def message: String = this match
    case Truncated(w)                  => s"truncated: $w"
    case NotZip(d)                     => s"not an acceptable zip: $d"
    case Zip64OrMultiDisk              => "zip64 or multi-disk archives are not accepted"
    case Encrypted(m)                  => s"encrypted member $m"
    case CompressedMember(m, k)        => s"member $m uses compression method $k (only STORED=0 is accepted)"
    case DataDescriptor(m)             => s"member $m uses a data descriptor"
    case SizeMismatch(m)               => s"member $m has inconsistent sizes"
    case CrcMismatch(m)                => s"member $m fails its CRC-32"
    case DuplicateMember(m)            => s"duplicate member $m"
    case UnknownMember(m)              => s"unknown member $m (only top-level *.npy members are accepted)"
    case LocalHeaderMismatch(m)        => s"local and central headers disagree for $m"
    case BadMagic(m)                   => s"member $m has no .npy magic"
    case UnsupportedNpyVersion(m, a, b) => s"member $m has .npy version $a.$b (only 1.0 is accepted)"
    case MalformedHeader(m, d)         => s"member $m has a malformed .npy header: $d"
    case UnsupportedDtype(m, d)        => s"member $m has dtype '$d' (only <f8 and <i4 are accepted)"
    case FortranOrder(m)               => s"member $m is Fortran-ordered"
    case ShapeOverflow(m)              => s"member $m has an unrepresentable shape"
    case DataLengthMismatch(m, e, f)   => s"member $m has $f data bytes, expected $e"

/** A decoded `.npy` payload: C-order, little-endian, float64 or int32 only. */
enum NpyArray:
  case F64(shape: Vector[Int], data: Array[Double])
  case I32(shape: Vector[Int], data: Array[Int])

  def shape: Vector[Int]
  def dtypeName: String = this match
    case F64(_, _) => "float64"
    case I32(_, _) => "int32"

/** One zip member: its name sans `.npy`, the exact member bytes (what the manifest hashes), and the decoded array. */
final case class NpzMember(name: String, rawNpy: Array[Byte], array: NpyArray)

/** Strict parsed `.npz`: members sorted as stored. */
final class Npz private (val members: Vector[NpzMember]):
  def get(name: String): Option[NpzMember] = members.find(_.name == name)
  def names: Vector[String] = members.map(_.name)

/** Strict reader of STORED-only `.npz` bytes holding `.npy` v1.0 little-endian `<f8`/`<i4` C-order arrays. */
object Npz:

  private def u16(b: Array[Byte], o: Int): Int = (b(o) & 0xff) | ((b(o + 1) & 0xff) << 8)
  private def u32(b: Array[Byte], o: Int): Long =
    (b(o) & 0xffL) | ((b(o + 1) & 0xffL) << 8) | ((b(o + 2) & 0xffL) << 16) | ((b(o + 3) & 0xffL) << 24)

  private val Eocd = 0x06054b50L
  private val Central = 0x02014b50L
  private val Local = 0x04034b50L

  def parse(bytes: Array[Byte]): Either[NpzError, Npz] =
    for
      entries <- zipEntries(bytes)
      members <- decodeAll(bytes, entries)
    yield new Npz(members)

  private final case class Entry(name: String, localStart: Int, dataStart: Int, size: Int):
    def end: Int = dataStart + size

  private def zipEntries(b: Array[Byte]): Either[NpzError, Vector[Entry]] =
    if b.length < 22 then Left(NpzError.Truncated("end of central directory record"))
    else
      val eo = b.length - 22
      if u32(b, eo) != Eocd then
        Left(NpzError.Truncated("end of central directory record"))
      else
        val disk = u16(b, eo + 4)
        val cdDisk = u16(b, eo + 6)
        val nHere = u16(b, eo + 8)
        val nTotal = u16(b, eo + 10)
        val cdSize = u32(b, eo + 12)
        val cdOff = u32(b, eo + 16)
        val commentLen = u16(b, eo + 20)
        if disk != 0 || cdDisk != 0 || nHere != nTotal then Left(NpzError.Zip64OrMultiDisk)
        else if nTotal == 0xffff || cdSize == 0xffffffffL || cdOff == 0xffffffffL then Left(NpzError.Zip64OrMultiDisk)
        else if commentLen != 0 then Left(NpzError.NotZip("archive comment present"))
        else if cdOff + cdSize != eo.toLong then Left(NpzError.NotZip("central directory is not contiguous with end record"))
        else centralEntries(b, cdOff.toInt, nTotal, eo)

  private def centralEntries(b: Array[Byte], start: Int, n: Int, limit: Int): Either[NpzError, Vector[Entry]] =
    val out = Vector.newBuilder[Entry]
    val seen = scala.collection.mutable.HashSet.empty[String]
    var pos = start
    var i = 0
    var err: Option[NpzError] = None
    while err.isEmpty && i < n do
      if pos + 46 > limit then err = Some(NpzError.Truncated("central directory header"))
      else if u32(b, pos) != Central then err = Some(NpzError.NotZip("bad central directory signature"))
      else
        val flags = u16(b, pos + 8)
        val method = u16(b, pos + 10)
        val crc = u32(b, pos + 16)
        val csize = u32(b, pos + 20)
        val usize = u32(b, pos + 24)
        val nameLen = u16(b, pos + 28)
        val extraLen = u16(b, pos + 30)
        val commentLen = u16(b, pos + 32)
        val localOff = u32(b, pos + 42)
        val hdrEnd = pos + 46 + nameLen + extraLen + commentLen
        if hdrEnd > limit then err = Some(NpzError.Truncated("central directory entry"))
        else
          val name = ascii(b, pos + 46, nameLen)
          if name.isEmpty then err = Some(NpzError.NotZip("non-ASCII member name"))
          else
            val nm = name.get
            err =
              if !seen.add(nm) then Some(NpzError.DuplicateMember(nm))
              else if nm.contains('/') || !nm.endsWith(".npy") || nm.length == 4 then Some(NpzError.UnknownMember(nm))
              else if (flags & 1) != 0 then Some(NpzError.Encrypted(nm))
              else if (flags & 8) != 0 then Some(NpzError.DataDescriptor(nm))
              else if method != 0 then Some(NpzError.CompressedMember(nm, method))
              else if csize == 0xffffffffL || usize == 0xffffffffL || localOff == 0xffffffffL then Some(NpzError.Zip64OrMultiDisk)
              else if csize != usize || usize > Int.MaxValue then Some(NpzError.SizeMismatch(nm))
              else localEntry(b, nm, localOff, usize, crc, start)
                .fold(Some(_), e => { out += e; None })
            pos = hdrEnd
      i += 1
    err match
      case Some(e) => Left(e)
      case None if pos != limit => Left(NpzError.NotZip("central directory has trailing bytes or a size mismatch"))
      case None => tile(out.result(), start)

  /** Local entries must tile `[0, cdStart)` exactly: no leading bytes, gaps, overlaps or trailing bytes. */
  private def tile(es: Vector[Entry], cdStart: Int): Either[NpzError, Vector[Entry]] =
    var cursor = 0
    var ok = true
    es.sortBy(_.localStart).foreach { e =>
      if e.localStart != cursor then ok = false
      cursor = e.end
    }
    if ok && cursor == cdStart then Right(es) else Left(NpzError.NotZip("local entries do not tile the archive"))

  private def localEntry(
      b: Array[Byte], name: String, off: Long, size: Long, crc: Long, cdStart: Int
  ): Either[NpzError, Entry] =
    if off + 30 > cdStart then Left(NpzError.Truncated(s"local header of $name"))
    else
      val o = off.toInt
      if u32(b, o) != Local then Left(NpzError.LocalHeaderMismatch(name))
      else
        val flags = u16(b, o + 6)
        val method = u16(b, o + 8)
        val lcrc = u32(b, o + 14)
        val lcs = u32(b, o + 18)
        val lus = u32(b, o + 22)
        val nameLen = u16(b, o + 26)
        val extraLen = u16(b, o + 28)
        val dataStart = o.toLong + 30 + nameLen + extraLen
        if dataStart + size > cdStart then Left(NpzError.Truncated(s"data of $name"))
        else if (flags & 1) != 0 then Left(NpzError.Encrypted(name))
        else if (flags & 8) != 0 then Left(NpzError.DataDescriptor(name))
        else if method != 0 then Left(NpzError.CompressedMember(name, method))
        else if ascii(b, o + 30, nameLen) != Some(name) then Left(NpzError.LocalHeaderMismatch(name))
        else if lcs != size || lus != size || lcrc != crc then Left(NpzError.LocalHeaderMismatch(name))
        else
          val ds = dataStart.toInt
          if Digests.crc32(b, ds, ds + size.toInt) != crc then Left(NpzError.CrcMismatch(name))
          else Right(Entry(name, o, ds, size.toInt))

  private def ascii(b: Array[Byte], from: Int, len: Int): Option[String] =
    val sb = new StringBuilder(len)
    var i = 0
    var ok = true
    while ok && i < len do
      val c = b(from + i) & 0xff
      if c < 0x20 || c > 0x7e then ok = false else sb.append(c.toChar)
      i += 1
    if ok then Some(sb.toString) else None

  private def decodeAll(b: Array[Byte], es: Vector[Entry]): Either[NpzError, Vector[NpzMember]] =
    val out = Vector.newBuilder[NpzMember]
    val it = es.iterator
    var err: Option[NpzError] = None
    while err.isEmpty && it.hasNext do
      val e = it.next()
      val raw = java.util.Arrays.copyOfRange(b, e.dataStart, e.dataStart + e.size)
      val base = e.name.dropRight(4)
      decodeNpy(base, raw) match
        case Left(x)  => err = Some(x)
        case Right(a) => out += NpzMember(base, raw, a)
    err.toLeft(out.result())

  private val HeaderRe: Regex =
    """\{'descr': '([^']*)', 'fortran_order': (True|False), 'shape': (\(\)|\((?:0|[1-9][0-9]*),\)|\((?:0|[1-9][0-9]*)(?:, (?:0|[1-9][0-9]*))+\)), \} *""".r

  /** Decode one `.npy` byte sequence (exposed for tests of individual refusals). */
  def decodeNpy(member: String, raw: Array[Byte]): Either[NpzError, NpyArray] =
    if raw.length < 10 then Left(NpzError.Truncated(s"npy preamble of $member"))
    else if (raw(0) & 0xff) != 0x93 || raw(1) != 'N'.toByte || raw(2) != 'U'.toByte || raw(3) != 'M'.toByte ||
      raw(4) != 'P'.toByte || raw(5) != 'Y'.toByte
    then Left(NpzError.BadMagic(member))
    else
      val major = raw(6) & 0xff
      val minor = raw(7) & 0xff
      if major != 1 || minor != 0 then Left(NpzError.UnsupportedNpyVersion(member, major, minor))
      else
        val hlen = u16(raw, 8)
        val dataStart = 10 + hlen
        if dataStart > raw.length then Left(NpzError.Truncated(s"npy header of $member"))
        else
          ascii(raw, 10, hlen - 1 max 0) match
            case None => Left(NpzError.MalformedHeader(member, "non-ASCII header"))
            case Some(_) if hlen == 0 || raw(10 + hlen - 1) != '\n'.toByte =>
              Left(NpzError.MalformedHeader(member, "header not newline-terminated"))
            case Some(_) if (10 + hlen) % 64 != 0 =>
              Left(NpzError.MalformedHeader(member, "header not padded to a multiple of 64 bytes"))
            case Some(text) =>
              HeaderRe.matches(text) match
                case false => Left(NpzError.MalformedHeader(member, text))
                case true =>
                  val m = HeaderRe.findFirstMatchIn(text).get
                  decodeBody(member, raw, dataStart, m.group(1), m.group(2) == "True", m.group(3))

  private def decodeBody(
      member: String, raw: Array[Byte], dataStart: Int, descr: String, fortran: Boolean, shapeText: String
  ): Either[NpzError, NpyArray] =
    if descr != "<f8" && descr != "<i4" then Left(NpzError.UnsupportedDtype(member, descr))
    else if fortran then Left(NpzError.FortranOrder(member))
    else
      val parts = shapeText.stripPrefix("(").stripSuffix(")").split(',').map(_.trim).filter(_.nonEmpty)
      val dims = parts.map(_.toIntOption)
      if dims.exists(_.isEmpty) then Left(NpzError.ShapeOverflow(member))
      else
        val shape = dims.map(_.get).toVector
        var count = 1L
        var over = false
        shape.foreach { d =>
          count *= d.toLong
          if count > Int.MaxValue then over = true
        }
        if over then Left(NpzError.ShapeOverflow(member))
        else
          val width = if descr == "<f8" then 8 else 4
          val expected = count * width
          val found = (raw.length - dataStart).toLong
          if expected != found then Left(NpzError.DataLengthMismatch(member, expected, found))
          else if descr == "<f8" then
            val out = new Array[Double](count.toInt)
            var i = 0
            while i < out.length do
              val o = dataStart + 8 * i
              out(i) = java.lang.Double.longBitsToDouble(u32(raw, o) | (u32(raw, o + 4) << 32))
              i += 1
            Right(NpyArray.F64(shape, out))
          else
            val out = new Array[Int](count.toInt)
            var i = 0
            while i < out.length do
              out(i) = u32(raw, dataStart + 4 * i).toInt
              i += 1
            Right(NpyArray.I32(shape, out))
