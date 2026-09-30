package scalafim.fmri.workflow

import bids4s.{BidsFile, BidsPath, BidsProject, BidsValidationReport}
import scalafim.dataset.DatasetShape
import scalafim.image.SomeSampleSpace
import scalafim.image.SampleSpaces.*
import scalafim.image.io.Nifti
import scalafim.image.world.{SpaceEvidence, WorldSpace}

import java.nio.file.Path
import scala.util.control.NonFatal

object BidsStudyCompilerJvm:
  def compile(
      project: BidsProject,
      recipe: DatasetRecipe,
      evidenceFor: BidsFile => Either[String, SpaceEvidence] = defaultEvidence
  ): Either[CatalogCompileReport, StudyCatalog] =
    BidsStudyCompiler.compile(project, recipe, readHeaders(project, recipe, evidenceFor))

  def compileChecked(
      project: BidsValidationReport[BidsProject],
      recipe: DatasetRecipe,
      evidenceFor: BidsFile => Either[String, SpaceEvidence] = defaultEvidence
  ): Either[CatalogCompileReport, CatalogCompilation] =
    BidsStudyCompiler.compileChecked(project, recipe, readHeaders(project.value, recipe, evidenceFor))

  def readHeaders(project: BidsProject, recipe: DatasetRecipe, evidenceFor: BidsFile => Either[String, SpaceEvidence] = defaultEvidence): ImageHeaderCatalog =
    val selectedBold = project.query(recipe.boldQuery)
    val derivativeMasks = project.manifest.files.filter(isMaskImage)
    readHeaders(project, (selectedBold ++ derivativeMasks).distinct, evidenceFor)

  def readHeaders(project: BidsProject): ImageHeaderCatalog =
    readHeaders(project, defaultEvidence)

  def readHeaders(project: BidsProject, evidenceFor: BidsFile => Either[String, SpaceEvidence]): ImageHeaderCatalog =
    val files = project.manifest.files.filter(file => file.extension == "nii" || file.extension == "nii.gz")
    readHeaders(project, files, evidenceFor)

  private def readHeaders(project: BidsProject, files: Vector[BidsFile], evidenceFor: BidsFile => Either[String, SpaceEvidence]): ImageHeaderCatalog =
    val root = Path.of(project.root.value).toAbsolutePath.normalize()
    val headers = Map.newBuilder[BidsPath, ImageHeaderDescriptor]
    val failures = Map.newBuilder[BidsPath, String]

    files.foreach { file =>
      resolve(root, file) match
        case Left(reason) => failures += file.path -> reason
        case Right(path) =>
          describe(file, path, evidenceFor) match
            case Left(reason) => failures += file.path -> reason
            case Right(header) => headers += file.path -> header
    }
    ImageHeaderCatalog(headers.result(), failures.result())

  private def isMaskImage(file: BidsFile): Boolean =
    val nifti = file.extension == "nii" || file.extension == "nii.gz"
    nifti && (
      file.parsed.exists(name => name.kind == "mask" || name.entities.get(bids4s.EntityKey.Description).contains("brain")) ||
        file.fileName.contains("_mask.nii")
    )

  private def describe(file: BidsFile, path: Path, evidenceFor: BidsFile => Either[String, SpaceEvidence]): Either[String, ImageHeaderDescriptor] =
    try
      for
        evidence <- evidenceFor(file)
        entity = spaceEntity(file)
        _ <- entity match
          case Some(value) if evidence.bidsSpace.exists(_ != value) => Left(s"evidence space ${evidence.bidsSpace} contradicts BIDS space-$value for ${file.path.value}")
          case _ => Right(())
        admitted = evidence.copy(bidsSpace = entity.orElse(evidence.bidsSpace))
        header <- Nifti.readHeader(path).left.map(_.message)
        placed <- header.spaceIn(admitted).left.map(_.message)
        descriptor <-
          if header.dims.length < 3 || header.dims.length > 4 then
            Left(s"expected a 3D or 4D NIfTI header, got ${header.dims.mkString("x")}")
          else
            val space = placed.spatialSpace
            val timepoints = if header.dims.length == 4 then header.dims(3) else 1
            DatasetShape.make(space, timepoints).left.map(_.message).map(ImageHeaderDescriptor.apply)
      yield descriptor
    catch
      case NonFatal(error) => Left(error.getMessage)

  private def spaceEntity(file: BidsFile): Option[String] =
    "(?:^|_)space-([^_]+)".r.findFirstMatchIn(file.fileName).map(_.group(1))

  private def defaultEvidence(file: BidsFile): Either[String, SpaceEvidence] =
    spaceEntity(file).toRight(s"NIfTI ${file.path.value} has no explicit BIDS template space entity").flatMap: entity =>
      WorldSpace.template(entity).left.map(_.message).map(_ => SpaceEvidence(bidsSpace = Some(entity)))

  private def resolve(root: Path, file: BidsFile): Either[String, Path] =
    val resolved = root.resolve(file.path.value).normalize()
    if resolved.startsWith(root) then Right(resolved)
    else Left(s"path escapes BIDS project root: ${file.path.value}")
