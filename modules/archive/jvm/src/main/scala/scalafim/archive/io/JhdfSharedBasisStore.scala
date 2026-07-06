package scalafim.archive.io

import io.jhdf.HdfFile
import io.jhdf.api.{Attribute, Dataset, Group}
import scalafim.archive.ArchiveError
import scalafim.archive.lna.*
import scalafim.image.DMat

import java.nio.file.{Files, Path, StandardCopyOption}
import java.time.Instant
import scala.util.control.NonFatal

final case class SharedBasisWriteResult(
    file: Path,
    entry: SharedBasisRegistryEntry
)

object JhdfSharedBasisStore:
  private val FormatId = "scalafim-lna-basis-hdf5-0"
  private val MetaGroup = "meta"
  private val LoadingsDataset = "loadings"
  private val MaskDataset = "mask"
  private val ParamsDataset = "params"

  def write(path: Path, artifact: SharedBasisArtifact): Either[ArchiveError, Unit] =
    SharedBasisArtifact.validateFinite(artifact).flatMap { valid =>
      try
        Option(path.getParent).foreach(Files.createDirectories(_))
        val tmp = tempPath(path)
        Files.deleteIfExists(tmp)
        try
          writeUnchecked(tmp, valid)
          Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
        catch
          case NonFatal(e) =>
            Files.deleteIfExists(tmp)
            throw e
        Right(())
      catch case NonFatal(e) => Left(ArchiveError.UnsupportedStorage(s"jHDF shared basis write failed for ${path.toAbsolutePath}: ${e.getMessage}"))
    }

  def read(path: Path, verifyChecksum: Boolean = true): Either[ArchiveError, SharedBasisArtifact] =
    try
      val hdf = new HdfFile(path)
      try
        for
          meta <- group(hdf, s"/$MetaGroup")
          kind <- readRequiredStringAttribute(meta, "basis_kind")
          checksum <- readRequiredStringAttribute(meta, "checksum").flatMap(SharedBasisChecksum.apply)
          maskChecksum <- readRequiredStringAttribute(meta, "mask_checksum").flatMap(SharedBasisChecksum.apply)
          created <- readOptionalStringAttribute(meta.getAttribute("created"), "created")
          loadings <- readLoadings(hdf)
          mask <- readMask(hdf)
          params <- readParams(hdf)
          artifact <- catchInvalid("shared basis artifact")(SharedBasisArtifact(loadings, mask, kind, params, created))
          _ <- SharedBasisArtifact.validateFinite(artifact)
          _ <- verify(artifact, checksum, maskChecksum, verifyChecksum)
        yield artifact
      finally hdf.close()
    catch case NonFatal(e) => Left(ArchiveError.UnsupportedStorage(s"jHDF shared basis read failed for ${path.toAbsolutePath}: ${e.getMessage}"))

  def writeContentAddressed(
      dir: Path,
      artifact: SharedBasisArtifact,
      basisId: Option[SharedBasisId] = None,
      created: String = Instant.now().toString
  ): Either[ArchiveError, SharedBasisWriteResult] =
    val stamped = artifact.copy(created = artifact.created.orElse(Some(created)))
    val entry = SharedBasisRegistryEntry.fromArtifact(stamped, stamped.created.getOrElse(created))
    val file = dir.resolve(entry.filename)

    for
      _ <- ensureDir(dir)
      _ <-
        if Files.exists(file) then
          read(file, verifyChecksum = true).flatMap { existing =>
            if existing.checksum == stamped.checksum then Right(())
            else Left(ArchiveError.InvalidArchive(s"existing shared basis artifact checksum mismatch at ${file.toAbsolutePath}"))
          }
        else write(file, stamped)
      _ <- basisId match
        case None => Right(())
        case Some(id) => updateRegistry(dir.resolve("registry.json"), id, entry)
    yield SharedBasisWriteResult(file, entry)

  def readRegistry(path: Path): Either[ArchiveError, SharedBasisRegistry] =
    if !Files.exists(path) then Right(SharedBasisRegistry())
    else
      try SharedBasisRegistryCodec.parse(Files.readString(path))
      catch case NonFatal(e) => Left(ArchiveError.UnsupportedStorage(s"could not read shared basis registry ${path.toAbsolutePath}: ${e.getMessage}"))

  def writeRegistry(path: Path, registry: SharedBasisRegistry): Either[ArchiveError, Unit] =
    try
      Option(path.getParent).foreach(Files.createDirectories(_))
      Files.writeString(path, SharedBasisRegistryCodec.render(registry) + "\n")
      Right(())
    catch case NonFatal(e) => Left(ArchiveError.UnsupportedStorage(s"could not write shared basis registry ${path.toAbsolutePath}: ${e.getMessage}"))

  def updateRegistry(
      path: Path,
      id: SharedBasisId,
      entry: SharedBasisRegistryEntry
  ): Either[ArchiveError, Unit] =
    readRegistry(path).flatMap(registry => writeRegistry(path, registry.updated(id, entry)))

  private def writeUnchecked(path: Path, artifact: SharedBasisArtifact): Unit =
    val file = HdfFile.write(path)
    try
      file.putAttribute("scalafim_lna_basis_storage", FormatId)

      val meta = file.putGroup(MetaGroup)
      meta.putAttribute("basis_kind", artifact.kind)
      meta.putAttribute("checksum", artifact.checksum.value)
      meta.putAttribute("checksum_algorithm", SharedBasisArtifact.ChecksumAlgorithm)
      meta.putAttribute("mask_checksum", artifact.maskChecksum.value)
      meta.putAttribute("mask_checksum_algorithm", SharedBasisArtifact.MaskChecksumAlgorithm)
      meta.putAttribute("n_atoms", artifact.nAtoms)
      meta.putAttribute("n_voxels", artifact.nVoxels)
      meta.putAttribute("created", artifact.created.getOrElse(""))
      meta.putAttribute("loadings_storage", "dense")

      file.putDataset(LoadingsDataset, doubleMatrix(artifact.loadings))
      file.putDataset(MaskDataset, maskData(artifact.mask))
      if artifact.params.nonEmpty then
        file.putDataset(ParamsDataset, SharedBasisParamsCodec.render(artifact.params))
    finally file.close()

  private def readLoadings(hdf: HdfFile): Either[ArchiveError, DMat] =
    readDataset(hdf, s"/$LoadingsDataset").flatMap { dataset =>
      val dims = dataset.getDimensions().map(_.toInt).toVector
      if dims.length != 2 then Left(ArchiveError.ShapeMismatch(s"shared basis loadings must be two-dimensional, got ${dims.mkString("x")}"))
      else
        dataset.getDataFlat match
          case values: Array[Double] =>
            Right(matrixFromFlat(values, dims(0), dims(1)))
          case values: Array[Float] =>
            Right(matrixFromFlat(values.map(_.toDouble), dims(0), dims(1)))
          case other =>
            Left(ArchiveError.UnsupportedStorage(s"shared basis loadings dataset is not floating point: ${other.getClass.getName}"))
    }

  private def readMask(hdf: HdfFile): Either[ArchiveError, SharedBasisMask] =
    readDataset(hdf, s"/$MaskDataset").flatMap { dataset =>
      val dims = dataset.getDimensions().map(_.toInt).toVector
      val values =
        dataset.getDataFlat match
          case data: Array[Byte]  => Right(data.map(_ != 0).toVector)
          case data: Array[Short] => Right(data.map(_ != 0).toVector)
          case data: Array[Int]   => Right(data.map(_ != 0).toVector)
          case other => Left(ArchiveError.UnsupportedStorage(s"shared basis mask dataset is not integer data: ${other.getClass.getName}"))
      values.flatMap(v => catchInvalid("shared basis mask")(SharedBasisMask(dims, v)))
    }

  private def readParams(hdf: HdfFile): Either[ArchiveError, Map[String, String]] =
    optionalDataset(hdf, s"/$ParamsDataset") match
      case None => Right(Map.empty)
      case Some(dataset) =>
        dataset.getData match
          case value: String => SharedBasisParamsCodec.parse(value)
          case values: Array[String] if values.length == 1 => SharedBasisParamsCodec.parse(values(0))
          case other => Left(ArchiveError.UnsupportedStorage(s"shared basis params dataset is not a string: ${other.getClass.getName}"))

  private def verify(
      artifact: SharedBasisArtifact,
      checksum: SharedBasisChecksum,
      maskChecksum: SharedBasisChecksum,
      enabled: Boolean
  ): Either[ArchiveError, Unit] =
    if !enabled then Right(())
    else if artifact.checksum != checksum then
      Left(ArchiveError.InvalidArchive(s"shared basis checksum mismatch: expected ${checksum.value} but computed ${artifact.checksum.value}"))
    else if artifact.maskChecksum != maskChecksum then
      Left(ArchiveError.InvalidArchive(s"shared basis mask checksum mismatch: expected ${maskChecksum.value} but computed ${artifact.maskChecksum.value}"))
    else Right(())

  private def doubleMatrix(data: DMat): Array[Array[Double]] =
    Array.tabulate(data.rows, data.cols)((r, c) => data(r, c))

  private def maskData(mask: SharedBasisMask): Object =
    mask.dims.length match
      case 1 =>
        Array.tabulate(mask.dims(0))(i => if mask.values(i) then 1 else 0)
      case 2 =>
        val d0 = mask.dims(0)
        val d1 = mask.dims(1)
        Array.tabulate(d0, d1)((i, j) => if mask.values(i * d1 + j) then 1 else 0)
      case 3 =>
        val d0 = mask.dims(0)
        val d1 = mask.dims(1)
        val d2 = mask.dims(2)
        Array.tabulate(d0, d1, d2)((i, j, k) => if mask.values((i * d1 + j) * d2 + k) then 1 else 0)
      case other =>
        throw IllegalArgumentException(s"shared basis HDF5 masks support 1D, 2D, or 3D dims, got $other")

  private def matrixFromFlat(values: Array[Double], rows: Int, cols: Int): DMat =
    DMat.fromRows(Vector.tabulate(rows)(r => Vector.tabulate(cols)(c => values(r * cols + c))))

  private def group(hdf: HdfFile, path: String): Either[ArchiveError, Group] =
    hdf.getByPath(path) match
      case group: Group => Right(group)
      case null => Left(ArchiveError.UnsupportedStorage(s"shared basis group $path is missing"))
      case other => Left(ArchiveError.UnsupportedStorage(s"shared basis path $path is not a group: ${other.getClass.getName}"))

  private def readDataset(hdf: HdfFile, path: String): Either[ArchiveError, Dataset] =
    try Right(hdf.getDatasetByPath(path))
    catch case NonFatal(_) => Left(ArchiveError.MissingPayload(scalafim.archive.ArchivePath(path)))

  private def optionalDataset(hdf: HdfFile, path: String): Option[Dataset] =
    try Option(hdf.getDatasetByPath(path))
    catch case NonFatal(_) => None

  private def readRequiredStringAttribute(group: Group, name: String): Either[ArchiveError, String] =
    readOptionalStringAttribute(group.getAttribute(name), name).flatMap {
      case Some(value) if value.nonEmpty => Right(value)
      case _ => Left(ArchiveError.UnsupportedStorage(s"shared basis metadata attribute $name is missing"))
    }

  private def readOptionalStringAttribute(attribute: Attribute, name: String): Either[ArchiveError, Option[String]] =
    if attribute == null then Right(None)
    else
      attribute.getData match
        case value: String if value.isEmpty => Right(None)
        case value: String => Right(Some(value))
        case values: Array[String] if values.length == 1 && values(0).isEmpty => Right(None)
        case values: Array[String] if values.length == 1 => Right(Some(values(0)))
        case other => Left(ArchiveError.UnsupportedStorage(s"shared basis metadata attribute $name is not a string: ${other.getClass.getName}"))

  private def ensureDir(path: Path): Either[ArchiveError, Unit] =
    try
      Files.createDirectories(path)
      Right(())
    catch case NonFatal(e) => Left(ArchiveError.UnsupportedStorage(s"could not create shared basis directory ${path.toAbsolutePath}: ${e.getMessage}"))

  private def tempPath(path: Path): Path =
    val file = path.getFileName.toString
    val parent = Option(path.getParent).getOrElse(path.toAbsolutePath.getParent)
    parent.resolve(s".$file.tmp")

  private def catchInvalid[A](label: String)(body: => A): Either[ArchiveError, A] =
    try Right(body)
    catch case e: IllegalArgumentException => Left(ArchiveError.InvalidArchive(s"$label: ${e.getMessage}"))
