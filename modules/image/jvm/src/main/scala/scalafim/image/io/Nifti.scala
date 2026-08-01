package scalafim.image.io

import scalafim.image.*

import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.Files
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
  qformCode: Int,
  qform: Option[DMat],
  sformCode: Int,
  sform: Option[DMat],
  byteOrder: ByteOrder
):
  def preferredAffine: Option[DMat] =
    sform.orElse(qform)

  def space: NeuroSpace =
    preferredAffine match
      case Some(affine) =>
        val origin = Vector(affine(0, 3), affine(1, 3), affine(2, 3))
        NeuroSpace(dims, spacing = Some(Affine.voxelSizes(affine)), origin = Some(origin), trans = Some(affine))
      case None =>
        NeuroSpace(dims, spacing = Some(pixdim.take(3)), origin = Some(Vector.fill(3)(0.0)))

object Nifti:

  def writeVol(path: Path, volume: NeuroVol[Double]): Path =
    writeBytes(
      path,
      niftiBytes(
        volume.space.dims.take(3),
        volume.space,
        volume.copyLegacyLinear
      )
    )

  def writeVec(path: Path, vec: NeuroVec[Double]): Path =
    writeBytes(
      path,
      niftiBytes(
        vec.space.dims.take(4),
        vec.space,
        vec.copyLegacyLinear
      )
    )

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

      val rawQfac = bb.getFloat(76).toDouble
      val qfac = if rawQfac < 0.0 then -1.0 else 1.0
      val pixdim = Vector.tabulate(3) { i =>
        val value = math.abs(bb.getFloat(76 + (i + 1) * 4).toDouble)
        if value == 0.0 then 1.0 else value
      }

      val voxOffset = bb.getFloat(108).toInt
      val slope = bb.getFloat(112).toDouble
      val intercept = bb.getFloat(116).toDouble

      val qoffset =
        Vector(
          bb.getFloat(268).toDouble,
          bb.getFloat(272).toDouble,
          bb.getFloat(276).toDouble
        )

      val qformCode = bb.getShort(252).toInt
      val qform =
        if qformCode > 0 then
          val b = bb.getFloat(256).toDouble
          val c = bb.getFloat(260).toDouble
          val d = bb.getFloat(264).toDouble
          Some(quaternionAffine(b, c, d, qoffset, pixdim, qfac))
        else None

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
        qformCode = qformCode,
        qform = qform,
        sformCode = sformCode,
        sform = sform,
        byteOrder = order
      )
    finally in.close()

  def readVol(path: Path): NeuroVol[Double] =
    val hdr = readHeader(path)
    require(hdr.dims.length == 3 || (hdr.dims.length == 4 && hdr.dims(3) == 1), "expected 3D nifti")
    val sp = if hdr.dims.length == 4 then hdr.space.spatialSpace else hdr.space
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

  private def writeBytes(path: Path, bytes: Array[Byte]): Path =
    if path.toString.endsWith(".gz") then
      throw new UnsupportedOperationException("writing compressed NIfTI is not supported by the lightweight adapter")
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.write(path, bytes)
    path

  private def quaternionAffine(
      b0: Double,
      c0: Double,
      d0: Double,
      offset: Vector[Double],
      spacing: Vector[Double],
      qfac: Double
  ): DMat =
    val squaredVector = b0 * b0 + c0 * c0 + d0 * d0
    val (a, b, c, d) =
      if squaredVector > 1.0 - 1e-7 then
        val scale = 1.0 / math.sqrt(squaredVector)
        (0.0, b0 * scale, c0 * scale, d0 * scale)
      else
        (math.sqrt(1.0 - squaredVector), b0, c0, d0)

    val r11 = a * a + b * b - c * c - d * d
    val r12 = 2.0 * (b * c - a * d)
    val r13 = 2.0 * (b * d + a * c)
    val r21 = 2.0 * (b * c + a * d)
    val r22 = a * a + c * c - b * b - d * d
    val r23 = 2.0 * (c * d - a * b)
    val r31 = 2.0 * (b * d - a * c)
    val r32 = 2.0 * (c * d + a * b)
    val r33 = a * a + d * d - c * c - b * b

    DMat.fromRows(
      Vector(
        Vector(r11 * spacing(0), r12 * spacing(1), r13 * spacing(2) * qfac, offset(0)),
        Vector(r21 * spacing(0), r22 * spacing(1), r23 * spacing(2) * qfac, offset(1)),
        Vector(r31 * spacing(0), r32 * spacing(1), r33 * spacing(2) * qfac, offset(2)),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def niftiBytes(dims: Vector[Int], space: NeuroSpace, values: Array[Double]): Array[Byte] =
    require(dims.length == 3 || dims.length == 4, "NIfTI writer expects a 3D volume or 4D vector")
    require(values.length == dims.product, "NIfTI data length must match dimensions")
    val bytes = Array.ofDim[Byte](352 + values.length * 8)
    val bb = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)

    bb.putInt(0, 348)
    bb.putShort(40, dims.length.toShort)
    var d = 0
    while d < 7 do
      val dim = if d < dims.length then dims(d) else 1
      bb.putShort(42 + d * 2, dim.toShort)
      d += 1

    bb.putShort(70, 64.toShort)
    bb.putShort(72, 64.toShort)
    bb.putFloat(76, 1.0f)
    val spacing = space.spacing.padTo(3, 1.0)
    var i = 0
    while i < 3 do
      bb.putFloat(80 + i * 4, spacing(i).toFloat)
      i += 1
    if dims.length == 4 then bb.putFloat(92, 1.0f)
    bb.putFloat(108, 352.0f)
    bb.putFloat(112, 1.0f)
    bb.putFloat(116, 0.0f)

    val affine = space.trans
    bb.putShort(254, 1.toShort)
    bb.putFloat(268, affine(0, 3).toFloat)
    bb.putFloat(272, affine(1, 3).toFloat)
    bb.putFloat(276, affine(2, 3).toFloat)
    var c = 0
    while c < 4 do
      bb.putFloat(280 + c * 4, affine(0, c).toFloat)
      bb.putFloat(296 + c * 4, affine(1, c).toFloat)
      bb.putFloat(312 + c * 4, affine(2, c).toFloat)
      c += 1

    val magic = "n+1".getBytes(StandardCharsets.US_ASCII)
    bb.put(344, magic(0))
    bb.put(345, magic(1))
    bb.put(346, magic(2))
    bb.put(347, 0.toByte)

    i = 0
    while i < values.length do
      bb.putDouble(352 + i * 8, values(i))
      i += 1
    bytes

  private def readDataAsDouble(path: Path, hdr: NiftiHeader): Array[Double] =
    val in = open(path)
    try
      val skipped = in.skip(hdr.voxOffset.toLong)
      if skipped < hdr.voxOffset then
        throw new IllegalArgumentException("unable to seek to vox_offset")

      val nels = hdr.dims.product
      val out = Array.ofDim[Double](nels)

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
