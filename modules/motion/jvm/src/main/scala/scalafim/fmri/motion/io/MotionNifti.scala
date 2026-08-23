package scalafim.fmri.motion.io

import image4s.nifti.NiftiDatatype
import image4s.nifti.NiftiTemporalUnit
import image4s.nifti.NiftiWriteOptions
import scalafim.fmri.motion.*
import scalafim.image.io.Nifti
import scalafim.image.DMat
import scalafim.image.NeuroSeries
import scalafim.image.NeuroSpace
import scalafim.image.NeuroVec
import scalafim.image.SomeNeuroSeries
import scalafim.image.SomeScalarSeries

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

object MotionNifti:
  def read(path: Path): Either[MotionIoError, MotionNiftiRun] =
    if !Files.isRegularFile(path) then Left(MotionIoError.MissingFile(path))
    else
      try
        for
          header <- Nifti
            .readHeader(path)
            .left
            .map(error => MotionIoError.InvalidInput(path, error.message))
          decoded <- Nifti
            .readSeries(path)
            .left
            .map(error => MotionIoError.InvalidInput(path, error.message))
          metadata <- sidecarMetadata(
            defaultSidecar(path),
            header.dims,
            header.pixdim.take(3),
            header.preferredAffine
          )
        yield MotionNiftiRun(
          NeuroVec.fromNative(decoded.image),
          metadata.copy(path = path)
        )
      catch
        case e: Exception => Left(MotionIoError.InvalidInput(path, e.getMessage))

  def write(
      path: Path,
      run: NeuroVec[Double],
      metadata: Option[MotionNiftiMetadata] = None
  ): Either[MotionIoError, Path] =
    try
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      for
        _ <- validateWriteMetadata(path, run, metadata)
        native <- nativeRun(path, run)
        options <- writeOptions(path, metadata)
        _ <- Nifti
          .writeSeries(path, native, options)
          .left
          .map(error => MotionIoError.WriteFailed(path, error.message))
        _ <- metadata match
          case None => Right(path)
          case Some(value) => writeSidecar(defaultSidecar(path), value)
      yield path
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

  private def nativeRun(
      path: Path,
      run: NeuroVec[Double]
  ): Either[MotionIoError, SomeScalarSeries[Double]] =
    for
      sampleSpace <- NeuroSpace
        .requireD3(run.space)
        .left
        .map(error => MotionIoError.WriteFailed(path, error.message))
      native <- NeuroSeries
        .continuous(sampleSpace, run.values, run.sampled.metadata)
        .left
        .map(error => MotionIoError.WriteFailed(path, error.message))
    yield SomeNeuroSeries.eraseSpace(native)

  private def writeOptions(
      path: Path,
      metadata: Option[MotionNiftiMetadata]
  ): Either[MotionIoError, NiftiWriteOptions] =
    NiftiWriteOptions
      .create(
        datatype = NiftiDatatype.Float64,
        slope = 1.0,
        intercept = 0.0,
        nonSpatialPixelDimensions =
          Vector(metadata.flatMap(_.repetitionTime).getOrElse(1.0)),
        temporalUnit = NiftiTemporalUnit.Second
      )
      .left
      .map(error => MotionIoError.WriteFailed(path, error.message))

  private def validateWriteMetadata(
      path: Path,
      run: NeuroVec[Double],
      metadata: Option[MotionNiftiMetadata]
  ): Either[MotionIoError, Unit] =
    metadata match
      case None => Right(())
      case Some(value) =>
        val runDims = run.space.dims.take(4)
        val runSpacing = run.space.spacing.take(3)
        val dimensionsMatch = value.dims == runDims
        val spacingMatches = sameValues(value.voxelSize, runSpacing)
        val affineMatches = value.affine.forall: affine =>
          sameValues(affine.data.toVector, run.space.trans.data.toVector)
        if !dimensionsMatch then
          Left(
            MotionIoError.WriteFailed(
              path,
              s"metadata dimensions ${value.dims.mkString("x")} do not match run dimensions ${runDims.mkString("x")}"
            )
          )
        else if !spacingMatches then
          Left(
            MotionIoError.WriteFailed(
              path,
              s"metadata voxel sizes ${value.voxelSize.mkString("x")} do not match run voxel sizes ${runSpacing.mkString("x")}"
            )
          )
        else if !affineMatches then
          Left(
            MotionIoError.WriteFailed(
              path,
              "metadata affine does not match the run sample space"
            )
          )
        else Right(())

  private def sameValues(
      left: Vector[Double],
      right: Vector[Double],
      tolerance: Double = 1e-9
  ): Boolean =
    left.size == right.size &&
      left.zip(right).forall: (a, b) =>
        math.abs(a - b) <= tolerance

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
