package scalafim.fmri.motion.io

import scalafim.fmri.motion.*
import scalafim.image.io.Nifti
import scalafim.image.{DMat, NeuroVec}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.{ByteBuffer, ByteOrder}

object MotionNifti:
  def read(path: Path): Either[MotionIoError, MotionNiftiRun] =
    if !Files.isRegularFile(path) then Left(MotionIoError.MissingFile(path))
    else
      try
        val header = Nifti.readHeader(path)
        val run = Nifti.readVec(path)
        sidecarMetadata(defaultSidecar(path), header.dims, header.pixdim, header.preferredAffine)
          .map(metadata => MotionNiftiRun(run, metadata.copy(path = path)))
      catch
        case e: Exception => Left(MotionIoError.InvalidInput(path, e.getMessage))

  def write(
      path: Path,
      run: NeuroVec[Double],
      metadata: Option[MotionNiftiMetadata] = None
  ): Either[MotionIoError, Path] =
    if path.toString.endsWith(".gz") then
      Left(MotionIoError.WriteFailed(path, "writing compressed NIfTI is not supported by the lightweight adapter"))
    else
      try
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.write(path, niftiBytes(run, metadata))
        metadata match
          case None => Right(path)
          case Some(value) => writeSidecar(defaultSidecar(path), value).map(_ => path)
      catch
        case e: Exception => Left(MotionIoError.WriteFailed(path, e.getMessage))

  def writeSidecar(path: Path, metadata: MotionNiftiMetadata): Either[MotionIoError, Path] =
    try
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.writeString(path, sidecarJson(metadata), StandardCharsets.UTF_8)
      Right(path)
    catch
      case e: Exception => Left(MotionIoError.WriteFailed(path, e.getMessage))

  def defaultSidecar(path: Path): Path =
    val name = path.getFileName.toString
    val stem =
      if name.endsWith(".nii.gz") then name.dropRight(".nii.gz".length)
      else if name.endsWith(".nii") then name.dropRight(".nii".length)
      else name
    path.resolveSibling(s"$stem.json")

  private def sidecarMetadata(
      path: Path,
      dims: Vector[Int],
      voxelSize: Vector[Double],
      affine: Option[DMat]
  ): Either[MotionIoError, MotionNiftiMetadata] =
    if !Files.isRegularFile(path) then
      Right(MotionNiftiMetadata(path, dims, voxelSize, affine, repetitionTime = None, acquisitionTiming = None))
    else
      val text = Files.readString(path, StandardCharsets.UTF_8)
      for
        tr <- parseOptionalNumber(text, "RepetitionTime", path)
        sliceTimes <- parseOptionalNumberArray(text, "SliceTiming", path)
        timing <- sliceTimes match
          case None => Right(None)
          case Some(values) =>
            SliceTiming.make(values).map(t => Some(AcquisitionTiming.Slice(t))).left.map(err => MotionIoError.InvalidSidecar(path, err.message))
      yield MotionNiftiMetadata(path, dims, voxelSize, affine, tr, timing)

  private def niftiBytes(run: NeuroVec[Double], metadata: Option[MotionNiftiMetadata]): Array[Byte] =
    val dims = run.space.dims.take(4)
    require(dims.length == 4, "motion NIfTI writer expects a 4D NeuroVec")
    val nels = dims.product
    val bytes = Array.ofDim[Byte](352 + nels * 8)
    val bb = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(0, 348)
    bb.putShort(40, 4.toShort)
    var d = 0
    while d < 4 do
      bb.putShort(42 + d * 2, dims(d).toShort)
      d += 1
    while d < 7 do
      bb.putShort(42 + d * 2, 1.toShort)
      d += 1
    bb.putShort(70, 64.toShort)
    bb.putShort(72, 64.toShort)
    bb.putFloat(76, 1.0f)
    val spacing = metadata.map(_.voxelSize).getOrElse(run.space.spacing).padTo(3, 1.0)
    var i = 0
    while i < 3 do
      bb.putFloat(80 + i * 4, spacing(i).toFloat)
      i += 1
    bb.putFloat(92, metadata.flatMap(_.repetitionTime).getOrElse(1.0).toFloat)
    bb.putFloat(108, 352.0f)
    bb.putFloat(112, 1.0f)
    bb.putFloat(116, 0.0f)
    val affine = metadata.flatMap(_.affine).getOrElse(run.space.trans)
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
    bb.put(344, 'n'.toByte)
    bb.put(345, '+'.toByte)
    bb.put(346, '1'.toByte)
    bb.put(347, 0.toByte)
    i = 0
    while i < nels do
      bb.putDouble(352 + i * 8, run.values.data(i))
      i += 1
    bytes

  private def sidecarJson(metadata: MotionNiftiMetadata): String =
    val fields = Vector.newBuilder[String]
    metadata.repetitionTime.foreach(tr => fields += s""""RepetitionTime": ${formatDouble(tr)}""")
    metadata.acquisitionTiming.foreach {
      case AcquisitionTiming.Slice(timing) =>
        fields += s""""SliceTiming": [${timing.offsetSeconds.map(formatDouble).mkString(", ")}]"""
      case AcquisitionTiming.Packet(packet) =>
        fields += s""""SliceTiming": [${packet.toSliceTiming.offsetSeconds.map(formatDouble).mkString(", ")}]"""
      case AcquisitionTiming.Volume => ()
    }
    fields.result().mkString("{\n  ", ",\n  ", "\n}\n")

  private def parseOptionalNumber(text: String, key: String, path: Path): Either[MotionIoError, Option[Double]] =
    val pattern = ("\"" + key + "\"\\s*:\\s*(-?[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?)").r
    pattern.findFirstMatchIn(text) match
      case None => Right(None)
      case Some(m) =>
        m.group(1).toDoubleOption match
          case Some(value) if value.isFinite && value > 0.0 => Right(Some(value))
          case _ => Left(MotionIoError.InvalidSidecar(path, s"$key must be a positive finite number"))

  private def parseOptionalNumberArray(text: String, key: String, path: Path): Either[MotionIoError, Option[Vector[Double]]] =
    val pattern = ("\"" + key + "\"\\s*:\\s*\\[([^\\]]*)\\]").r
    pattern.findFirstMatchIn(text) match
      case None => Right(None)
      case Some(m) =>
        val body = m.group(1).trim
        if body.isEmpty then Right(Some(Vector.empty))
        else
          val values = body.split(',').toVector.map(_.trim)
          val parsed = values.map(_.toDoubleOption)
          if parsed.exists(_.isEmpty) then Left(MotionIoError.InvalidSidecar(path, s"$key must contain only numbers"))
          else Right(Some(parsed.flatten))

  private def formatDouble(value: Double): String =
    if value.isWhole then value.toLong.toString else value.toString
