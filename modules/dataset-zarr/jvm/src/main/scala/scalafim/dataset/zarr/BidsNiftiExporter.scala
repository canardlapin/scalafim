package scalafim.dataset.zarr

import java.io.OutputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.util.zip.GZIPOutputStream
import scala.util.Using
import scala.util.control.NonFatal
import gale.linalg.DMat
import image4s.geometry.{Affine, D3}
import scalafim.archive.zarr.*
import bids4s.EntityKey
import scalafim.image.NeuroAffineSyntax.*
import zarr4s.*

final case class BidsExportResult(
    nifti: Path,
    sidecar: Path,
    datasetDescription: Path,
    participants: Path
)

object BidsNiftiExporter:
  def exportRevision(
      opened: OpenedCanonicalBold,
      root: Path,
      acquisition: BidsAcquisition,
      datasetName: String = "NeuroArchive export"
  ): Either[NeuroArchiveZarrError, BidsExportResult] =
    val absoluteRoot = root.toAbsolutePath.normalize()
    val nifti = absoluteRoot.resolve(acquisition.relativePath).normalize()
    if !nifti.startsWith(absoluteRoot) then
      Left(NeuroArchiveZarrError.PublicationFailure("BIDS export path escapes its root"))
    else
      val sidecarName =
        if nifti.getFileName.toString.endsWith(".nii.gz") then nifti.getFileName.toString.dropRight(7) + ".json"
        else nifti.getFileName.toString.dropRight(4) + ".json"
      val sidecar = nifti.resolveSibling(sidecarName)
      val description = absoluteRoot.resolve("dataset_description.json")
      val participants = absoluteRoot.resolve("participants.tsv")
      try
        Files.createDirectories(nifti.getParent)
        writeNifti(opened, nifti)
        Files.writeString(sidecar, sidecarJson(opened, acquisition), StandardCharsets.UTF_8)
        if !Files.exists(description) then
          Files.writeString(
            description,
            s"{\"BIDSVersion\":\"1.11.1\",\"Name\":${JsonValue.Str(datasetName).render}}",
            StandardCharsets.UTF_8
          )
        if !Files.exists(participants) then
          val subject = acquisition.name.entities.get(EntityKey.Subject)
            .getOrElse(throw IllegalArgumentException("BIDS BOLD acquisition requires a subject entity"))
          Files.writeString(participants, s"participant_id\nsub-$subject\n", StandardCharsets.UTF_8)
        Right(BidsExportResult(nifti, sidecar, description, participants))
      catch case NonFatal(error) =>
        Left(NeuroArchiveZarrError.PublicationFailure(s"BIDS/NIfTI export failed: ${error.getMessage}"))

  private def writeNifti(opened: OpenedCanonicalBold, target: Path): Unit =
    val header = niftiHeader(opened)
    val raw = Files.newOutputStream(target)
    val output: OutputStream =
      if target.getFileName.toString.endsWith(".gz") then new GZIPOutputStream(raw)
      else raw
    Using.resource(output): stream =>
      stream.write(header)
      val shape = opened.canonical.array.shape
      val extent = Shape(1L, shape.axis(1), shape.axis(2), shape.axis(3))
        .fold(error => throw IllegalArgumentException(error.message), identity)
      var time = 0L
      while time < shape.axis(0) do
        val origin = Coordinate(time, 0L, 0L, 0L)
          .fold(error => throw IllegalArgumentException(error.message), identity)
        val region = Region.within(shape, origin, extent)
          .fold(error => throw IllegalArgumentException(error.message), identity)
        val block = opened.array.readRegion(region)
          .fold(error => throw IllegalArgumentException(error.message), _.block)
        val encoded = ScalarBytes.encode(
          block,
          opened.canonical.array.dataType,
          Some(Endianness.Little)
        ).fold(error => throw IllegalArgumentException(error.message), identity)
        stream.write(encoded.toArray)
        time += 1L

  private def niftiHeader(opened: OpenedCanonicalBold): Array[Byte] =
    val manifest = opened.canonical.manifest
    val descriptor = opened.canonical.array
    val (datatype, bitpix) = descriptor.dataType.name match
      case "int8" => 256 -> 8
      case "uint8" => 2 -> 8
      case "int16" => 4 -> 16
      case "uint16" => 512 -> 16
      case "int32" => 8 -> 32
      case "uint32" => 768 -> 32
      case "int64" => 1024 -> 64
      case "uint64" => 1280 -> 64
      case "float32" => 16 -> 32
      case "float64" => 64 -> 64
      case found => throw IllegalArgumentException(s"unsupported NIfTI export type $found")
    val shape = descriptor.shape
    val affine = manifest.geometry.voxelToWorld
    val affineMatrix = DMat.tabulate(4, 4)(affine.apply)
    val voxelSizes = Affine
      .fromRowMajor[D3](affineMatrix.valuesRowMajor)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
      .neuroVoxelSizes
    val repetition = manifest.timing match
      case AcquisitionTiming.Regular(_, step, _, TimeUnits.Second) => step
      case AcquisitionTiming.Regular(_, step, _, TimeUnits.Millisecond) => step / 1000.0
      case AcquisitionTiming.Explicit(coordinates, TimeUnits.Second) if coordinates.length > 1 => coordinates(1) - coordinates(0)
      case AcquisitionTiming.Explicit(coordinates, TimeUnits.Millisecond) if coordinates.length > 1 => (coordinates(1) - coordinates(0)) / 1000.0
      case _ => 1.0

    val bytes = new Array[Byte](352)
    val buffer = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
    buffer.putInt(0, 348)
    buffer.putShort(40, 4.toShort)
    buffer.putShort(42, shape.axis(3).toShort)
    buffer.putShort(44, shape.axis(2).toShort)
    buffer.putShort(46, shape.axis(1).toShort)
    buffer.putShort(48, shape.axis(0).toShort)
    var dimension = 4
    while dimension < 7 do
      buffer.putShort(42 + dimension * 2, 1.toShort)
      dimension += 1
    buffer.putShort(70, datatype.toShort)
    buffer.putShort(72, bitpix.toShort)
    buffer.putFloat(76, 1.0f)
    buffer.putFloat(80, voxelSizes(0).toFloat)
    buffer.putFloat(84, voxelSizes(1).toFloat)
    buffer.putFloat(88, voxelSizes(2).toFloat)
    buffer.putFloat(92, repetition.toFloat)
    buffer.putFloat(108, 352.0f)
    buffer.putFloat(112, manifest.calibration.scale.toFloat)
    buffer.putFloat(116, manifest.calibration.offset.toFloat)
    buffer.put(123, 10.toByte)
    buffer.putShort(254, 1.toShort)
    var column = 0
    while column < 4 do
      buffer.putFloat(280 + column * 4, affine(0, column).toFloat)
      buffer.putFloat(296 + column * 4, affine(1, column).toFloat)
      buffer.putFloat(312 + column * 4, affine(2, column).toFloat)
      column += 1
    buffer.put(344, 'n'.toByte)
    buffer.put(345, '+'.toByte)
    buffer.put(346, '1'.toByte)
    buffer.put(347, 0.toByte)
    bytes

  private def sidecarJson(opened: OpenedCanonicalBold, acquisition: BidsAcquisition): String =
    val repetition = opened.canonical.manifest.timing match
      case AcquisitionTiming.Regular(_, step, _, TimeUnits.Second) => step
      case AcquisitionTiming.Regular(_, step, _, TimeUnits.Millisecond) => step / 1000.0
      case AcquisitionTiming.Explicit(coordinates, TimeUnits.Second) if coordinates.length > 1 => coordinates(1) - coordinates(0)
      case AcquisitionTiming.Explicit(coordinates, TimeUnits.Millisecond) if coordinates.length > 1 => (coordinates(1) - coordinates(0)) / 1000.0
      case _ => 1.0
    val task = acquisition.name.entities.get(EntityKey.Task).getOrElse("unknown")
    s"{\"RepetitionTime\":${java.lang.Double.toString(repetition)},\"TaskName\":${JsonValue.Str(task).render}}"
