package scalafim.archive.io

import scalafim.archive.{ArchiveError, ArchivePath, RunLabel, RunScopedPath}
import scalafim.archive.lna.*
import scalafim.image.DMat

import java.nio.file.{Files, Path}
import scala.collection.mutable

final case class SharedBasisResolutionContext(
    archivePath: Path,
    datasetRoot: Option[Path] = None,
    searchPaths: Vector[Path] = Vector.empty,
    maxParentLevels: Int = 8
):
  require(maxParentLevels >= 0, "max parent levels must be non-negative")

object LnaSharedBasisResolver:
  def resolve(
      ref: SharedBasisRef,
      context: SharedBasisResolutionContext
  ): Either[ArchiveError, Path] =
    ref.locator match
      case Some(locator) =>
        verifyPath(archiveDir(context).resolve(locator.value).normalize(), ref)
      case None =>
        resolveFromRegistry(ref, context).flatMap {
          case Some(path) => Right(path)
          case None       => resolveContentAddressed(ref, context)
        }

  def reconstruct(
      archive: LnaArchive,
      context: SharedBasisResolutionContext,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, DMat] =
    if archive.manifest.transforms.exists(_.params.isInstanceOf[TransformParams.SharedBasisEmbed]) then
      archive.validate.flatMap { valid =>
        for
          run <- valid.run(runLabel).toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' not found"))
          desc <- sharedBasisDescriptor(valid, run)
          params <- desc.params match
            case p: TransformParams.SharedBasisEmbed => Right(p)
            case _ => Left(ArchiveError.InvalidArchive("shared basis embed descriptor missing typed params"))
          coeffPath <- byRole(desc, DatasetRole.Coefficients)
          coefficients <- doubleMatrixPayload(valid, coeffPath, "coefficients")
          offset <- optionalOffset(valid, desc)
          basisPath <- resolve(params.basis, context)
          basis <- JhdfSharedBasisStore.read(basisPath)
          dense <- reconstruct(coefficients, basis, run, offset)
        yield dense
      }
    else LnaPipeline.reconstruct(archive, runLabel)

  def readAndReconstruct(
      path: Path,
      datasetRoot: Option[Path] = None,
      searchPaths: Vector[Path] = Vector.empty,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, DMat] =
    LnaHdf5Store.default.read(path).flatMap { archive =>
      reconstruct(
        archive,
        SharedBasisResolutionContext(
          archivePath = path,
          datasetRoot = datasetRoot,
          searchPaths = searchPaths
        ),
        runLabel
      )
    }

  private def resolveFromRegistry(
      ref: SharedBasisRef,
      context: SharedBasisResolutionContext
  ): Either[ArchiveError, Option[Path]] =
    val dirs = basisDirs(context)
    val it = dirs.iterator
    while it.hasNext do
      val dir = it.next()
      val registryPath = dir.resolve("registry.json")
      if Files.exists(registryPath) then
        val registry =
          JhdfSharedBasisStore.readRegistry(registryPath) match
            case Left(err) => return Left(err)
            case Right(ok) => ok
        registry.get(ref.basisId) match
          case Some(entry) if entry.checksum != ref.checksum =>
            return Left(
              ArchiveError.InvalidArchive(
                s"shared basis registry entry '${ref.basisId.value}' has checksum ${entry.checksum.value} but archive requires ${ref.checksum.value}"
              )
            )
          case Some(entry) =>
            return verifyPath(dir.resolve(entry.filename).normalize(), ref).map(Some(_))
          case None =>
            ()

    Right(None)

  private def resolveContentAddressed(
      ref: SharedBasisRef,
      context: SharedBasisResolutionContext
  ): Either[ArchiveError, Path] =
    val candidates =
      basisDirs(context).map(_.resolve(ref.checksum.filename)) ++
        context.searchPaths.map { raw =>
          val path = raw.toAbsolutePath.normalize()
          if Files.isDirectory(path) then path.resolve(ref.checksum.filename)
          else path
        }

    val it = dedupe(candidates).iterator
    while it.hasNext do
      val candidate = it.next()
      if Files.exists(candidate) then return verifyPath(candidate, ref)

    Left(ArchiveError.InvalidArchive(s"shared basis artifact ${ref.checksum.filename} was not found"))

  private def verifyPath(path: Path, ref: SharedBasisRef): Either[ArchiveError, Path] =
    val normalized = path.toAbsolutePath.normalize()
    if !Files.exists(normalized) then
      Left(ArchiveError.InvalidPath(normalized.toString, "shared basis artifact does not exist"))
    else
      JhdfSharedBasisStore.read(normalized).flatMap { artifact =>
        if artifact.checksum == ref.checksum then Right(normalized)
        else
          Left(
            ArchiveError.InvalidArchive(
              s"shared basis checksum mismatch at $normalized: expected ${ref.checksum.value} but computed ${artifact.checksum.value}"
            )
          )
      }

  private def reconstruct(
      coefficients: DMat,
      basis: SharedBasisArtifact,
      run: LnaRun,
      offset: Option[Vector[Double]]
  ): Either[ArchiveError, DMat] =
    if coefficients.rows != run.shape.timepoints then
      Left(ArchiveError.ShapeMismatch(s"coefficient rows ${coefficients.rows} do not match run timepoints ${run.shape.timepoints}"))
    else if coefficients.cols != basis.nAtoms then
      Left(ArchiveError.ShapeMismatch(s"coefficients have ${coefficients.cols} columns but shared basis has ${basis.nAtoms} atoms"))
    else if basis.mask.values.length != run.shape.spatialSize then
      Left(ArchiveError.ShapeMismatch(s"shared basis mask has ${basis.mask.values.length} entries but run space has ${run.shape.spatialSize} voxels"))
    else
      offset match
        case Some(values) if values.length != basis.nVoxels =>
          Left(ArchiveError.ShapeMismatch(s"offset has ${values.length} values but shared basis has ${basis.nVoxels} active voxels"))
        case Some(values) if values.exists(value => !value.isFinite) =>
          Left(ArchiveError.InvalidArchive("shared basis offset contains non-finite values"))
        case _ =>
          val active = multiplyCoefficientsByLoadings(coefficients, basis.loadings)
          val centered = offset.fold(active)(values => addOffset(active, values))
          Right(expandMask(centered, basis.mask))

  private def sharedBasisDescriptor(
      archive: LnaArchive,
      run: LnaRun
  ): Either[ArchiveError, TransformDescriptor] =
    archive.manifest.transforms.reverseIterator
      .find { desc =>
        desc.params.isInstanceOf[TransformParams.SharedBasisEmbed] &&
        desc.datasets.exists { ref =>
          ref.role == DatasetRole.Coefficients &&
          (ref.path == run.output || RunScopedPath.from(ref.path, run.label).isDefined)
        }
      }
      .toRight(ArchiveError.InvalidArchive(s"run '${run.label.value}' has no shared basis embed descriptor"))

  private def doubleMatrixPayload(
      archive: LnaArchive,
      path: ArchivePath,
      label: String
  ): Either[ArchiveError, DMat] =
    archive.payload(path) match
      case Some(Payload.DoubleMatrix(data, _)) => Right(data)
      case Some(_) => Left(ArchiveError.ShapeMismatch(s"shared basis $label payload is not a double matrix"))
      case None => Left(ArchiveError.MissingPayload(path))

  private def optionalOffset(
      archive: LnaArchive,
      desc: TransformDescriptor
  ): Either[ArchiveError, Option[Vector[Double]]] =
    desc.datasets.find(_.role == DatasetRole.Offset) match
      case None =>
        Right(None)
      case Some(ref) =>
        archive.payload(ref.path) match
          case Some(Payload.DoubleVector(values, _)) => Right(Some(values))
          case Some(_) => Left(ArchiveError.ShapeMismatch("shared basis offset payload is not a double vector"))
          case None => Left(ArchiveError.MissingPayload(ref.path))

  private def byRole(desc: TransformDescriptor, role: DatasetRole): Either[ArchiveError, ArchivePath] =
    desc.datasets
      .find(_.role == role)
      .map(_.path)
      .toRight(ArchiveError.InvalidArchive(s"${desc.kind.value} descriptor missing ${role.value} dataset"))

  private def multiplyCoefficientsByLoadings(coefficients: DMat, loadings: DMat): DMat =
    DMat.fromRows(
      Vector.tabulate(coefficients.rows) { t =>
        Vector.tabulate(loadings.rows) { voxel =>
          var sum = 0.0
          var atom = 0
          while atom < coefficients.cols do
            sum += coefficients(t, atom) * loadings(voxel, atom)
            atom += 1
          sum
        }
      }
    )

  private def addOffset(data: DMat, offset: Vector[Double]): DMat =
    DMat.fromRows(
      Vector.tabulate(data.rows) { row =>
        Vector.tabulate(data.cols) { col =>
          data(row, col) + offset(col)
        }
      }
    )

  private def expandMask(activeData: DMat, mask: SharedBasisMask): DMat =
    DMat.fromRows(
      Vector.tabulate(activeData.rows) { row =>
        val out = Array.fill(mask.values.length)(0.0)
        var voxel = 0
        var active = 0
        while voxel < mask.values.length do
          if mask.values(voxel) then
            out(voxel) = activeData(row, active)
            active += 1
          voxel += 1
        out.toVector
      }
    )

  private def basisDirs(context: SharedBasisResolutionContext): Vector[Path] =
    val dirs = Vector.newBuilder[Path]
    context.datasetRoot.foreach(root => dirs += root.toAbsolutePath.normalize().resolve("bases"))

    var current: Path = archiveDir(context)
    var depth = 0
    while current != null && depth <= context.maxParentLevels do
      dirs += current.resolve("bases").toAbsolutePath.normalize()
      current = current.getParent
      depth += 1

    dedupe(dirs.result())

  private def archiveDir(context: SharedBasisResolutionContext): Path =
    Option(context.archivePath.toAbsolutePath.normalize().getParent)
      .getOrElse(context.archivePath.toAbsolutePath.normalize())

  private def dedupe(paths: Vector[Path]): Vector[Path] =
    val seen = mutable.LinkedHashSet.empty[String]
    val out = Vector.newBuilder[Path]
    paths.foreach { path =>
      val normalized = path.toAbsolutePath.normalize()
      val key = normalized.toString
      if seen.add(key) then out += normalized
    }
    out.result()
