package scalafim.fmri.workflow

import scalafim.bids.{BidsFile, BidsPath, BidsProject, BidsValidationReport}
import scalafim.dataset.DatasetShape
import scalafim.image.NeuroSpace
import scalafim.image.io.Nifti

import java.nio.file.Path
import scala.util.control.NonFatal

object BidsStudyCompilerJvm:
  def compile(
      project: BidsProject,
      recipe: DatasetRecipe
  ): Either[CatalogCompileReport, StudyCatalog] =
    BidsStudyCompiler.compile(project, recipe, readHeaders(project, recipe))

  def compileChecked(
      project: BidsValidationReport[BidsProject],
      recipe: DatasetRecipe
  ): Either[CatalogCompileReport, CatalogCompilation] =
    BidsStudyCompiler.compileChecked(project, recipe, readHeaders(project.value, recipe))

  def readHeaders(project: BidsProject, recipe: DatasetRecipe): ImageHeaderCatalog =
    val selectedBold = project.query(recipe.boldQuery)
    val derivativeMasks = project.manifest.files.filter(isMaskImage)
    readHeaders(project, (selectedBold ++ derivativeMasks).distinct)

  def readHeaders(project: BidsProject): ImageHeaderCatalog =
    val files = project.manifest.files.filter(file => file.extension == "nii" || file.extension == "nii.gz")
    readHeaders(project, files)

  private def readHeaders(project: BidsProject, files: Vector[BidsFile]): ImageHeaderCatalog =
    val root = Path.of(project.root.value).toAbsolutePath.normalize()
    val headers = Map.newBuilder[BidsPath, ImageHeaderDescriptor]
    val failures = Map.newBuilder[BidsPath, String]

    files.foreach { file =>
      resolve(root, file) match
        case Left(reason) => failures += file.path -> reason
        case Right(path) =>
          describe(path) match
            case Left(reason) => failures += file.path -> reason
            case Right(header) => headers += file.path -> header
    }
    ImageHeaderCatalog(headers.result(), failures.result())

  private def isMaskImage(file: BidsFile): Boolean =
    val nifti = file.extension == "nii" || file.extension == "nii.gz"
    nifti && (
      file.parsed.exists(name => name.kind == "mask" || name.entities.get(scalafim.bids.EntityKey.Description).contains("brain")) ||
        file.fileName.contains("_mask.nii")
    )

  private def describe(path: Path): Either[String, ImageHeaderDescriptor] =
    try
      val header = Nifti.readHeader(path)
      if header.dims.length < 3 || header.dims.length > 4 then
        Left(s"expected a 3D or 4D NIfTI header, got ${header.dims.mkString("x")}")
      else
        val space = header.space.spatialSpace
        val timepoints = if header.dims.length == 4 then header.dims(3) else 1
        DatasetShape.make(space, timepoints).left.map(_.message).map(ImageHeaderDescriptor.apply)
    catch
      case NonFatal(error) => Left(error.getMessage)

  private def resolve(root: Path, file: BidsFile): Either[String, Path] =
    val resolved = root.resolve(file.path.value).normalize()
    if resolved.startsWith(root) then Right(resolved)
    else Left(s"path escapes BIDS project root: ${file.path.value}")
