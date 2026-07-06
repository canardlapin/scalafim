package scalafim.image.io

import scalafim.image.*
import narr.NArray

import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.Path
import java.util.zip.GZIPInputStream

final case class NiftiHeader(
  dims: Vector[Int],
  pixdim: Vector[Double],
  datatype: Int,
  bitpix: Int,
  voxOffset: Int,
  slope: Double,
  intercept: Double,
  qoffset: Vector[Double],
  sform: Option[DMat],
  byteOrder: ByteOrder
):
  def space: NeuroSpace =
    val trans = sform.orElse {
      val rows = Vector.tabulate(4)(r =>
        Vector.tabulate(4)(c =>
          if r == c && r < 3 then pixdim(r)
          else if c == 3 && r < 3 then qoffset(r)
          else if r == c then 1.0
          else 0.0
        )
      )
      Some(DMat.fromRows(rows))
    }
    NeuroSpace(dims, spacing = Some(pixdim.take(3)), origin = Some(qoffset), trans = trans)

object Nifti:

  def readHeader(path: Path): NiftiHeader =
    val in = open(path)
    try
      val hdrBytes = in.readNBytes(348)
      if hdrBytes.length != 348 then
        throw new IllegalArgumentException("file too small for NIfTI header")

      val bbLE = ByteBuffer.wrap(hdrBytes).order(ByteOrder.LITTLE_ENDIAN)
      val bbBE = ByteBuffer.wrap(hdrBytes).order(ByteOrder.BIG_ENDIAN)

      val order =
        val szLE = bbLE.getInt(0)
        val szBE = bbBE.getInt(0)
        if szLE == 348 then ByteOrder.LITTLE_ENDIAN
        else if szBE == 348 then ByteOrder.BIG_ENDIAN
        else throw new IllegalArgumentException("not a NIfTI-1 header (sizeof_hdr != 348)")

      val bb = ByteBuffer.wrap(hdrBytes).order(order)

      val dim0 = bb.getShort(40).toInt
      val dims = Vector.tabulate(dim0)(i => bb.getShort(42 + i * 2).toInt).map(_.max(1))

      val datatype = bb.getShort(70).toInt & 0xffff
      val bitpix = bb.getShort(72).toInt & 0xffff

      val pixdim = Vector.tabulate(3)(i => bb.getFloat(76 + (i + 1) * 4).toDouble)

      val voxOffset = bb.getFloat(108).toInt
      val slope = bb.getFloat(112).toDouble
      val intercept = bb.getFloat(116).toDouble

      val qoffset =
        Vector(
          bb.getFloat(268).toDouble,
          bb.getFloat(272).toDouble,
          bb.getFloat(276).toDouble
        )

      val sformCode = bb.getShort(254).toInt
      val sform =
        if sformCode > 0 then
          val rows = Vector(
            Vector.tabulate(4)(i => bb.getFloat(280 + i * 4).toDouble),
            Vector.tabulate(4)(i => bb.getFloat(296 + i * 4).toDouble),
            Vector.tabulate(4)(i => bb.getFloat(312 + i * 4).toDouble),
            Vector(0.0, 0.0, 0.0, 1.0)
          )
          Some(DMat.fromRows(rows))
        else None

      NiftiHeader(
        dims = dims,
        pixdim = pixdim,
        datatype = datatype,
        bitpix = bitpix,
        voxOffset = voxOffset,
        slope = slope,
        intercept = intercept,
        qoffset = qoffset,
        sform = sform,
        byteOrder = order
      )
    finally in.close()

  def readVol(path: Path): NeuroVol[Double] =
    val hdr = readHeader(path)
    require(hdr.dims.length == 3 || (hdr.dims.length == 4 && hdr.dims(3) == 1), "expected 3D nifti")
    val sp =
      if hdr.dims.length == 4 then
        NeuroSpace(hdr.dims.take(3), spacing = Some(hdr.pixdim), origin = Some(hdr.qoffset), trans = hdr.sform)
      else hdr.space
    val data = readDataAsDouble(path, hdr)
    NeuroVol.fromLinear(data, sp)

  def readVec(path: Path): NeuroVec[Double] =
    val hdr = readHeader(path)
    require(hdr.dims.length == 4 && hdr.dims(3) > 1, "expected 4D nifti with time dimension")
    val sp = hdr.space
    val data = readDataAsDouble(path, hdr)
    NeuroVec.fromLinear(data, sp)

  private def open(path: Path): InputStream =
    val base: InputStream = new BufferedInputStream(new FileInputStream(path.toFile))
    if path.toString.endsWith(".gz") then new GZIPInputStream(base) else base

  private def readDataAsDouble(path: Path, hdr: NiftiHeader): NArray[Double] =
    val in = open(path)
    try
      val skipped = in.skip(hdr.voxOffset.toLong)
      if skipped < hdr.voxOffset then
        throw new IllegalArgumentException("unable to seek to vox_offset")

      val nels = hdr.dims.product
      val out = NArray.ofSize[Double](nels)

      val bytesPer = hdr.bitpix / 8
      val buf = Array.ofDim[Byte](bytesPer)
      val bb = ByteBuffer.wrap(buf).order(hdr.byteOrder)

      val slope = if hdr.slope == 0.0 then 1.0 else hdr.slope
      val intercept = hdr.intercept

      var i = 0
      while i < nels do
        val read = in.readNBytes(buf, 0, bytesPer)
        if read != bytesPer then
          throw new IllegalArgumentException("unexpected EOF in data")

        bb.rewind()
        val raw =
          hdr.datatype match
            case 16 => bb.getFloat(0).toDouble
            case 64 => bb.getDouble(0)
            case 2  => (bb.get(0) & 0xff).toDouble
            case 4  => bb.getShort(0).toDouble
            case 8  => bb.getInt(0).toDouble
            case other =>
              throw new UnsupportedOperationException(s"unsupported datatype $other")

        out(i) = raw * slope + intercept
        i += 1
      out
    finally in.close()
