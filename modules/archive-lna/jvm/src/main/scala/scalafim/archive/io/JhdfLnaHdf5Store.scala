package scalafim.archive.io

import io.jhdf.HdfFile
import io.jhdf.api.{Attribute, Dataset, WritableGroup}
import scalafim.archive.{ArchiveError, ArchivePath, ArchiveStorageFormatId}
import scalafim.archive.lna.*
import gale.linalg.DMat

import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path, StandardCopyOption}
import java.security.MessageDigest
import scala.util.control.NonFatal

object JhdfLnaHdf5Store extends LnaHdf5Store:
  private val FormatId = ArchiveStorageFormatId.unsafe("scalafim-lna-hdf5-0")
  private val ChecksumAlgorithm = "sha256:lna-manifest-null-payloads-v1"
  private val MetaGroup = "__lna__"
  private val PayloadGroup = "payloads"
  private val TransformGroup = "transforms"
  private val ManifestDataset = "manifest_json"
  private val DescriptorDataset = "descriptor_json"

  def write(path: Path, archive: LnaArchive): Either[ArchiveError, Unit] =
    archive.validate.flatMap { valid =>
      try
        val canonical = canonicalArchive(valid)
        val checksum = archiveChecksum(canonical)
        val stored = canonical.copy(manifest = canonical.manifest.copy(checksum = Some(checksum)))
        Option(path.getParent).foreach(Files.createDirectories(_))
        val tmp = tempPath(path)
        Files.deleteIfExists(tmp)

        try
          writeUnchecked(tmp, stored)
          Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
        catch
          case NonFatal(e) =>
            Files.deleteIfExists(tmp)
            throw e

        Right(())
      catch case NonFatal(e) => Left(ArchiveError.UnsupportedStorage(s"jHDF write failed for ${path.toAbsolutePath}: ${e.getMessage}"))
    }

  def read(path: Path): Either[ArchiveError, LnaArchive] =
    try
      val hdf = new HdfFile(path)
      try
        for
          manifestText <- readStringDataset(hdf, s"/$MetaGroup/$ManifestDataset")
          manifest <- LnaManifestCodec.parse(manifestText)
          rootChecksum <- readStringAttribute(hdf.getAttribute("lna_checksum"), "lna_checksum")
          payloadPairs <- readPayloads(hdf, manifest)
          archive <- LnaArchive(manifest, payloadPairs.toMap).validate
          verified <- verifyChecksum(archive, rootChecksum)
        yield verified
      finally hdf.close()
    catch case NonFatal(e) => Left(ArchiveError.UnsupportedStorage(s"jHDF read failed for ${path.toAbsolutePath}: ${e.getMessage}"))

  private def writeUnchecked(path: Path, archive: LnaArchive): Unit =
    val file = HdfFile.write(path)
    try
      file.putAttribute("scalafim_lna_storage", FormatId.value)
      file.putAttribute("lna_spec", archive.manifest.version.id)
      file.putAttribute("creator", archive.manifest.creator)
      file.putAttribute("lna_checksum_algorithm", ChecksumAlgorithm)
      archive.manifest.checksum.foreach(file.putAttribute("lna_checksum", _))

      val meta = file.putGroup(MetaGroup)
      meta.putDataset(ManifestDataset, LnaManifestCodec.render(archive.manifest))

      val transforms = meta.putGroup(TransformGroup)
      archive.manifest.transforms.zipWithIndex.foreach { case (desc, index) =>
        val group = transforms.putGroup(f"$index%04d")
        group.putAttribute("name", desc.name)
        group.putAttribute("kind", desc.kind.value)
        group.putDataset(DescriptorDataset, LnaManifestCodec.renderTransform(desc))
      }

      val payloadRoot = meta.putGroup(PayloadGroup)
      archive.manifest.datasets.sortBy(_.path.value).foreach { ref =>
        val payload = archive.payloads.getOrElse(ref.path, throw new IllegalArgumentException(s"missing payload ${ref.path.value}"))
        writePayload(payloadRoot, ref, payload)
      }
    finally file.close()

  private def readPayloads(hdf: HdfFile, manifest: LnaManifest): Either[ArchiveError, Vector[(ArchivePath, Payload)]] =
    traverse(manifest.datasets) { ref =>
      readPayload(hdf, ref).map(ref.path -> _)
    }

  private def readPayload(hdf: HdfFile, ref: DatasetRef): Either[ArchiveError, Payload] =
    val hdfPath = payloadHdfPath(ref.path)
    ref.dtype match
      case None =>
        Left(ArchiveError.UnsupportedStorage(s"dataset ${ref.path.value} has no dtype in manifest"))
      case Some(dtype @ (LnaDType.Float32 | LnaDType.Float64)) if ref.dims.length == 2 =>
        readDataset(hdf, hdfPath).flatMap(dataset => doubleMatrixPayload(dataset, ref, dtype))
      case Some(dtype @ (LnaDType.Float32 | LnaDType.Float64)) if ref.dims.length == 1 =>
        readDataset(hdf, hdfPath).flatMap(dataset => doubleVectorPayload(dataset, ref, dtype))
      case Some(dtype @ (LnaDType.UInt8 | LnaDType.UInt16 | LnaDType.Int32)) if ref.dims.length == 2 =>
        readDataset(hdf, hdfPath).flatMap(dataset => intMatrixPayload(dataset, ref, dtype))
      case Some(dtype) =>
        Left(ArchiveError.UnsupportedStorage(s"unsupported payload shape ${ref.dims.mkString("x")} for dtype $dtype at ${ref.path.value}"))

  private def writePayload(root: WritableGroup, ref: DatasetRef, payload: Payload): Unit =
    val segments = ref.path.value.stripPrefix("/").split('/').filter(_.nonEmpty).toVector
    require(segments.nonEmpty, "payload archive path must have at least one segment")
    val parent = segments.dropRight(1).foldLeft(root) { case (group, segment) => ensureGroup(group, segment) }
    val dataset = parent.putDataset(segments.last, payloadData(payload))
    val dtype = ref.dtype.getOrElse(payload.dtype)
    dataset.putAttribute("lna_path", ref.path.value)
    dataset.putAttribute("lna_role", ref.role.value)
    dataset.putAttribute("lna_dtype", dtypeName(dtype))
    dataset.putAttribute("lna_unsigned", dtype == LnaDType.UInt8 || dtype == LnaDType.UInt16)

  private def ensureGroup(parent: WritableGroup, name: String): WritableGroup =
    parent.getChild(name) match
      case group: WritableGroup => group
      case null                 => parent.putGroup(name)
      case other                => throw new IllegalArgumentException(s"${other.getPath} already exists and is not a group")

  private def payloadData(payload: Payload): Object =
    payload match
      case Payload.DoubleMatrix(data, LnaDType.Float64) => doubleMatrix(data)
      case Payload.DoubleMatrix(data, LnaDType.Float32) => floatMatrix(data)
      case Payload.DoubleMatrix(_, dtype) =>
        throw new IllegalArgumentException(s"unsupported double matrix dtype $dtype")
      case Payload.DoubleVector(values, LnaDType.Float64) => values.toArray
      case Payload.DoubleVector(values, LnaDType.Float32) => values.map(_.toFloat).toArray
      case Payload.DoubleVector(_, dtype) =>
        throw new IllegalArgumentException(s"unsupported double vector dtype $dtype")
      case Payload.IntMatrix(rows, cols, values, LnaDType.UInt8) =>
        intMatrixAsByteRows(rows, cols, values)
      case Payload.IntMatrix(rows, cols, values, LnaDType.UInt16) =>
        intMatrixAsShortRows(rows, cols, values)
      case Payload.IntMatrix(rows, cols, values, LnaDType.Int32) =>
        intMatrixAsIntRows(rows, cols, values)
      case Payload.IntMatrix(_, _, _, dtype) =>
        throw new IllegalArgumentException(s"unsupported integer matrix dtype $dtype")

  private def readDataset(hdf: HdfFile, path: String): Either[ArchiveError, Dataset] =
    try Right(hdf.getDatasetByPath(path))
    catch case NonFatal(_) => Left(ArchiveError.MissingPayload(archivePathFromPayloadPath(path)))

  private def readStringDataset(hdf: HdfFile, path: String): Either[ArchiveError, String] =
    readDataset(hdf, path).flatMap { dataset =>
      dataset.getData match
        case value: String => Right(value)
        case values: Array[String] if values.length == 1 => Right(values(0))
        case other => Left(ArchiveError.UnsupportedStorage(s"dataset $path is not a string dataset: ${other.getClass.getName}"))
    }

  private def readStringAttribute(attribute: Attribute, name: String): Either[ArchiveError, Option[String]] =
    if attribute == null then Right(None)
    else
      attribute.getData match
        case value: String => Right(Some(value))
        case values: Array[String] if values.length == 1 => Right(Some(values(0)))
        case other => Left(ArchiveError.UnsupportedStorage(s"attribute $name is not a string attribute: ${other.getClass.getName}"))

  private def doubleMatrixPayload(dataset: Dataset, ref: DatasetRef, dtype: LnaDType): Either[ArchiveError, Payload] =
    val rows = ref.dims(0)
    val cols = ref.dims(1)
    flatDoubles(dataset, ref).map { values =>
      Payload.DoubleMatrix(DMat.tabulate(rows, cols)((row, column) => values(row * cols + column)), dtype)
    }

  private def doubleVectorPayload(dataset: Dataset, ref: DatasetRef, dtype: LnaDType): Either[ArchiveError, Payload] =
    flatDoubles(dataset, ref).map(values => Payload.DoubleVector(values.toVector, dtype))

  private def intMatrixPayload(dataset: Dataset, ref: DatasetRef, dtype: LnaDType): Either[ArchiveError, Payload] =
    val rows = ref.dims(0)
    val cols = ref.dims(1)
    flatInts(dataset, ref, dtype).flatMap { values =>
      catchInvalid(s"integer payload ${ref.path.value}") {
        Payload.IntMatrix(rows, cols, values.toVector, dtype)
      }
    }

  private def flatDoubles(dataset: Dataset, ref: DatasetRef): Either[ArchiveError, Array[Double]] =
    try
      val values =
        dataset.getDataFlat match
          case data: Array[Double] => Right(data)
          case data: Array[Float]  => Right(data.map(_.toDouble))
          case other => Left(ArchiveError.UnsupportedStorage(s"dataset ${ref.path.value} is not float data: ${other.getClass.getName}"))
      values.flatMap(data => checkSize(ref, data.length).map(_ => data))
    catch case NonFatal(e) => Left(ArchiveError.UnsupportedStorage(s"could not read float payload ${ref.path.value}: ${e.getMessage}"))

  private def flatInts(dataset: Dataset, ref: DatasetRef, dtype: LnaDType): Either[ArchiveError, Array[Int]] =
    try
      val values =
        dataset.getDataFlat match
          case data: Array[Byte] if dtype == LnaDType.UInt8 || dtype == LnaDType.UInt16 =>
            Right(data.map(java.lang.Byte.toUnsignedInt))
          case data: Array[Byte] =>
            Right(data.map(_.toInt))
          case data: Array[Short] if dtype == LnaDType.UInt8 || dtype == LnaDType.UInt16 =>
            Right(data.map(java.lang.Short.toUnsignedInt))
          case data: Array[Short] =>
            Right(data.map(_.toInt))
          case data: Array[Int] =>
            Right(data)
          case other => Left(ArchiveError.UnsupportedStorage(s"dataset ${ref.path.value} is not integer data: ${other.getClass.getName}"))
      values.flatMap(data => checkSize(ref, data.length).map(_ => data))
    catch case NonFatal(e) => Left(ArchiveError.UnsupportedStorage(s"could not read integer payload ${ref.path.value}: ${e.getMessage}"))

  private def checkSize(ref: DatasetRef, actual: Int): Either[ArchiveError, Unit] =
    val expected = ref.dims.product
    if expected == actual then Right(())
    else Left(ArchiveError.ShapeMismatch(s"${ref.path.value} expected $expected values but read $actual"))

  private def doubleMatrix(data: DMat): Array[Array[Double]] =
    Array.tabulate(data.rows, data.cols)((r, c) => data(r, c))

  private def floatMatrix(data: DMat): Array[Array[Float]] =
    Array.tabulate(data.rows, data.cols)((r, c) => data(r, c).toFloat)

  private def intMatrixAsIntRows(rows: Int, cols: Int, values: Vector[Int]): Array[Array[Int]] =
    Array.tabulate(rows, cols)((r, c) => values(r * cols + c))

  private def intMatrixAsByteRows(rows: Int, cols: Int, values: Vector[Int]): Array[Array[Byte]] =
    Array.tabulate(rows, cols) { (r, c) =>
      val value = values(r * cols + c)
      if value < 0 || value > 255 then throw new IllegalArgumentException(s"UInt8 value $value out of range")
      value.toByte
    }

  private def intMatrixAsShortRows(rows: Int, cols: Int, values: Vector[Int]): Array[Array[Short]] =
    Array.tabulate(rows, cols) { (r, c) =>
      val value = values(r * cols + c)
      if value < 0 || value > 65535 then throw new IllegalArgumentException(s"UInt16 value $value out of range")
      value.toShort
    }

  private def verifyChecksum(archive: LnaArchive, rootChecksum: Option[String]): Either[ArchiveError, LnaArchive] =
    val manifestChecksum = archive.manifest.checksum
    if manifestChecksum.exists(!isSha256Hex(_)) then
      Left(ArchiveError.InvalidArchive("manifest checksum must be a 64-character SHA-256 hex string"))
    else if rootChecksum.exists(!isSha256Hex(_)) then
      Left(ArchiveError.InvalidArchive("root checksum attribute must be a 64-character SHA-256 hex string"))
    else if manifestChecksum.isDefined && rootChecksum.isDefined && manifestChecksum != rootChecksum then
      Left(ArchiveError.InvalidArchive("manifest checksum and root checksum attribute disagree"))
    else
      val expected = manifestChecksum.orElse(rootChecksum)
      expected match
        case None =>
          Right(archive)
        case Some(value) =>
          val canonical = canonicalArchive(archive)
          val actual = archiveChecksum(canonical)
          if actual == value then Right(canonical.copy(manifest = canonical.manifest.copy(checksum = Some(value))))
          else Left(ArchiveError.InvalidArchive(s"checksum mismatch: expected $value but computed $actual"))

  private def canonicalArchive(archive: LnaArchive): LnaArchive =
    val payloads =
      archive.manifest.datasets
        .sortBy(_.path.value)
        .flatMap(ref => archive.payloads.get(ref.path).map(ref.path -> _))
        .toMap
    archive.copy(manifest = archive.manifest.copy(checksum = None), payloads = payloads)

  private def archiveChecksum(archive: LnaArchive): String =
    val digest = MessageDigest.getInstance("SHA-256")
    updateString(digest, ChecksumAlgorithm)
    updateString(digest, LnaManifestCodec.render(archive.manifest.copy(checksum = None)))
    archive.manifest.datasets.sortBy(_.path.value).foreach { ref =>
      updateString(digest, ref.path.value)
      updateString(digest, ref.role.value)
      updateString(digest, ref.dims.mkString("x"))
      updateString(digest, ref.dtype.fold("none")(dtypeName))
      archive.payloads.get(ref.path).foreach(payload => updatePayload(digest, payload))
    }
    hex(digest.digest())

  private def updatePayload(digest: MessageDigest, payload: Payload): Unit =
    payload match
      case Payload.DoubleMatrix(data, dtype) =>
        updateString(digest, "double_matrix")
        updateString(digest, dtypeName(dtype))
        updateInt(digest, data.rows)
        updateInt(digest, data.cols)
        var r = 0
        while r < data.rows do
          var c = 0
          while c < data.cols do
            updateFloatLike(digest, data(r, c), dtype)
            c += 1
          r += 1
      case Payload.DoubleVector(values, dtype) =>
        updateString(digest, "double_vector")
        updateString(digest, dtypeName(dtype))
        updateInt(digest, values.length)
        values.foreach(value => updateFloatLike(digest, value, dtype))
      case Payload.IntMatrix(rows, cols, values, dtype) =>
        updateString(digest, "int_matrix")
        updateString(digest, dtypeName(dtype))
        updateInt(digest, rows)
        updateInt(digest, cols)
        dtype match
          case LnaDType.UInt8 =>
            values.foreach(value => digest.update((value & 0xff).toByte))
          case LnaDType.UInt16 =>
            values.foreach(value => updateShort(digest, value & 0xffff))
          case LnaDType.Int32 =>
            values.foreach(value => updateInt(digest, value))
          case other =>
            throw new IllegalArgumentException(s"unsupported integer checksum dtype $other")

  private def updateFloatLike(digest: MessageDigest, value: Double, dtype: LnaDType): Unit =
    dtype match
      case LnaDType.Float32 => updateInt(digest, java.lang.Float.floatToIntBits(value.toFloat))
      case LnaDType.Float64 => updateLong(digest, java.lang.Double.doubleToLongBits(value))
      case other            => throw new IllegalArgumentException(s"unsupported float checksum dtype $other")

  private def updateString(digest: MessageDigest, value: String): Unit =
    val bytes = value.getBytes(StandardCharsets.UTF_8)
    updateInt(digest, bytes.length)
    digest.update(bytes)

  private def updateShort(digest: MessageDigest, value: Int): Unit =
    val buffer = ByteBuffer.allocate(2).order(ByteOrder.BIG_ENDIAN)
    buffer.putShort(value.toShort)
    digest.update(buffer.array())

  private def updateInt(digest: MessageDigest, value: Int): Unit =
    val buffer = ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN)
    buffer.putInt(value)
    digest.update(buffer.array())

  private def updateLong(digest: MessageDigest, value: Long): Unit =
    val buffer = ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN)
    buffer.putLong(value)
    digest.update(buffer.array())

  private def isSha256Hex(value: String): Boolean =
    value.matches("[A-Fa-f0-9]{64}")

  private def hex(bytes: Array[Byte]): String =
    bytes.map(b => f"${b & 0xff}%02x").mkString

  private def dtypeName(dtype: LnaDType): String =
    dtype match
      case LnaDType.Float32 => "float32"
      case LnaDType.Float64 => "float64"
      case LnaDType.UInt8   => "uint8"
      case LnaDType.UInt16  => "uint16"
      case LnaDType.Int32   => "int32"

  private def payloadHdfPath(path: ArchivePath): String =
    s"/$MetaGroup/$PayloadGroup${path.value}"

  private def archivePathFromPayloadPath(path: String): ArchivePath =
    ArchivePath(path.stripPrefix(s"/$MetaGroup/$PayloadGroup"))

  private def tempPath(path: Path): Path =
    val file = path.getFileName.toString
    val parent = Option(path.getParent).getOrElse(path.toAbsolutePath.getParent)
    parent.resolve(s".$file.tmp")

  private def traverse[A, B](values: Iterable[A])(f: A => Either[ArchiveError, B]): Either[ArchiveError, Vector[B]] =
    val out = Vector.newBuilder[B]
    val it = values.iterator
    var error = Option.empty[ArchiveError]
    while it.hasNext && error.isEmpty do
      f(it.next()) match
        case Left(err) => error = Some(err)
        case Right(ok) => out += ok
    error.fold(Right(out.result()))(Left(_))

  private def catchInvalid[A](label: String)(body: => A): Either[ArchiveError, A] =
    try Right(body)
    catch case NonFatal(e) => Left(ArchiveError.InvalidArchive(s"$label: ${e.getMessage}"))
