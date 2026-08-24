package scalafim.fmri.fit.io

import scalafim.fmri.fit.*
import scalafim.fmri.fit.ContrastId.*
import scalafim.fmri.fit.ResultMapName.*
import scalafim.image.io.Nifti
import gale.linalg.DMat

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

opaque type ResultExportStem = String

object ResultExportStem:
  private val Pattern = "^[A-Za-z0-9][A-Za-z0-9._-]*$".r

  def apply(value: String): Either[ResultManifestIoError, ResultExportStem] =
    val trimmed = value.trim
    if trimmed.isEmpty then Left(ResultManifestIoError.InvalidTarget("export stem must be non-empty"))
    else if Pattern.matches(trimmed) then Right(trimmed)
    else Left(ResultManifestIoError.InvalidTarget("export stem must be one BIDS-safe path segment"))

  def unsafe(value: String): ResultExportStem =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (stem: ResultExportStem)
    inline def value: String = stem

enum ResultManifestExportFormat:
  case BidsDirectory
  case Hdf5
  case Gds

  def label: String =
    this match
      case BidsDirectory => "bids-directory"
      case Hdf5          => "hdf5"
      case Gds           => "gds"

enum BidsNiftiMapLayout:
  case Bundled
  case Individual

  def label: String =
    this match
      case Bundled    => "bundled"
      case Individual => "individual"

final case class ResultManifestExportTarget private (
    root: Path,
    stem: ResultExportStem,
    format: ResultManifestExportFormat,
    bidsNiftiMapLayout: BidsNiftiMapLayout
):
  require(
    format == ResultManifestExportFormat.BidsDirectory || bidsNiftiMapLayout == BidsNiftiMapLayout.Bundled,
    "non-BIDS result targets cannot select a NIfTI map layout"
  )

  def stemValue: String =
    stem.value

object ResultManifestExportTarget:
  def bidsDirectory(
      root: Path,
      stem: String,
      niftiMapLayout: BidsNiftiMapLayout = BidsNiftiMapLayout.Bundled
  ): Either[ResultManifestIoError, ResultManifestExportTarget] =
    ResultExportStem(stem).map { value =>
      ResultManifestExportTarget(root, value, ResultManifestExportFormat.BidsDirectory, niftiMapLayout)
    }

  def hdf5(path: Path, stem: String): Either[ResultManifestIoError, ResultManifestExportTarget] =
    ResultExportStem(stem).map { value =>
      ResultManifestExportTarget(path, value, ResultManifestExportFormat.Hdf5, BidsNiftiMapLayout.Bundled)
    }

  def gds(path: Path, stem: String): Either[ResultManifestIoError, ResultManifestExportTarget] =
    ResultExportStem(stem).map { value =>
      ResultManifestExportTarget(path, value, ResultManifestExportFormat.Gds, BidsNiftiMapLayout.Bundled)
    }

enum ResultWrittenArtifactKind:
  case ParameterMaps(kind: ParameterMapKind)
  case ContrastMaps(contrastId: String)
  case CoefficientCovariance
  case ManifestSidecar

  def label: String =
    this match
      case ParameterMaps(kind)      => s"parameter-${kind.label}"
      case ContrastMaps(contrastId) => s"contrast-$contrastId"
      case CoefficientCovariance    => "coefficient-covariance"
      case ManifestSidecar          => "manifest-sidecar"

final case class ResultWrittenArtifact(
    kind: ResultWrittenArtifactKind,
    path: Path,
    labels: Vector[String]
)

final case class ResultManifestWriteResult(
    target: ResultManifestExportTarget,
    artifacts: Vector[ResultWrittenArtifact]
):
  require(artifacts.nonEmpty, "written result must contain at least one artifact")

  def paths: Vector[Path] =
    artifacts.map(_.path)

enum ResultManifestIoError:
  case InvalidTarget(detail: String)
  case UnsupportedFormat(format: ResultManifestExportFormat, detail: String)
  case InvalidManifest(cause: FitError)
  case WriteFailed(path: Path, detail: String)

  def message: String =
    this match
      case InvalidTarget(detail) =>
        s"invalid result export target: $detail"
      case UnsupportedFormat(format, detail) =>
        s"unsupported result export format ${format.label}: $detail"
      case InvalidManifest(cause) =>
        cause.message
      case WriteFailed(path, detail) =>
        s"could not write result artifact '${path.toAbsolutePath}': $detail"

object ResultManifestWriter:
  private final case class PlannedNifti(
      kind: ResultWrittenArtifactKind,
      path: Path,
      labels: Vector[String],
      maps: FitImageMaps
  )

  def write(
      manifest: ResultManifest,
      target: ResultManifestExportTarget
  ): Either[ResultManifestIoError, ResultManifestWriteResult] =
    target.format match
      case ResultManifestExportFormat.BidsDirectory =>
        writeBidsDirectory(manifest, target)
      case ResultManifestExportFormat.Hdf5 =>
        Left(ResultManifestIoError.UnsupportedFormat(ResultManifestExportFormat.Hdf5, "HDF5 result writer is not wired yet"))
      case ResultManifestExportFormat.Gds =>
        Left(ResultManifestIoError.UnsupportedFormat(ResultManifestExportFormat.Gds, "GDS result writer is not wired yet"))

  def writeBidsDirectory(
      manifest: ResultManifest,
      root: Path,
      stem: String,
      niftiMapLayout: BidsNiftiMapLayout = BidsNiftiMapLayout.Bundled
  ): Either[ResultManifestIoError, ResultManifestWriteResult] =
    ResultManifestExportTarget.bidsDirectory(root, stem, niftiMapLayout).flatMap(write(manifest, _))

  private def writeBidsDirectory(
      manifest: ResultManifest,
      target: ResultManifestExportTarget
  ): Either[ResultManifestIoError, ResultManifestWriteResult] =
    for
      niftis <- planNiftiMaps(manifest, target)
      _ <- validateUniquePaths(niftis, manifest, target)
      written <- writePlannedBidsDirectory(manifest, target, niftis)
    yield written

  private def writePlannedBidsDirectory(
      manifest: ResultManifest,
      target: ResultManifestExportTarget,
      niftis: Vector[PlannedNifti]
  ): Either[ResultManifestIoError, ResultManifestWriteResult] =
    try
      Files.createDirectories(target.root)
      val artifacts = Vector.newBuilder[ResultWrittenArtifact]
      niftis.foreach { planned =>
        Nifti
          .writeSeries(planned.path, planned.maps.dense)
          .fold(error => throw new IllegalStateException(error.message), identity)
        artifacts += ResultWrittenArtifact(planned.kind, planned.path, planned.labels)
      }
      manifest.coefficientCovariance.foreach { covariance =>
        artifacts += writeCovariance(covariance, target)
      }

      val withoutSidecar = artifacts.result()
      val sidecar = writeSidecar(manifest, target, withoutSidecar)
      Right(ResultManifestWriteResult(target, withoutSidecar :+ sidecar))
    catch
      case NonFatal(e) =>
        Left(ResultManifestIoError.WriteFailed(target.root, e.getMessage))

  private def planNiftiMaps(
      manifest: ResultManifest,
      target: ResultManifestExportTarget
  ): Either[ResultManifestIoError, Vector[PlannedNifti]] =
    target.bidsNiftiMapLayout match
      case BidsNiftiMapLayout.Bundled =>
        for
          parameters <- planBundledParameterMaps(manifest, target)
          contrasts <- planBundledContrastMaps(manifest, target)
        yield parameters ++ contrasts
      case BidsNiftiMapLayout.Individual =>
        for
          parameters <- planIndividualParameterMaps(manifest, target)
          contrasts <- planIndividualContrastMaps(manifest, target)
        yield parameters ++ contrasts

  private def planBundledParameterMaps(
      manifest: ResultManifest,
      target: ResultManifestExportTarget
  ): Either[ResultManifestIoError, Vector[PlannedNifti]] =
    buildAll(Vector(ParameterMapKind.Coefficient, ParameterMapKind.StandardError)) { kind =>
      val maps = manifest.parameterMaps(kind)
      if maps.isEmpty then Right(None)
      else
        FitImageMaps.fromParameterMaps(maps).left.map(ResultManifestIoError.InvalidManifest.apply).map { imageMaps =>
          val path = target.root.resolve(s"${target.stemValue}_desc-${safeEntity(s"parameter${kind.label}")}_statmap.nii")
          Some(PlannedNifti(ResultWrittenArtifactKind.ParameterMaps(kind), path, imageMaps.names, imageMaps))
        }
    }.map(_.flatten)

  private def planBundledContrastMaps(
      manifest: ResultManifest,
      target: ResultManifestExportTarget
  ): Either[ResultManifestIoError, Vector[PlannedNifti]] =
    val grouped =
      manifest.contrasts
        .groupBy(_.contrastId.value)
        .toVector
        .sortBy(_._1)

    buildAll(grouped) { case (contrastId, maps) =>
      FitImageMaps.fromContrastMaps(maps).left.map(ResultManifestIoError.InvalidManifest.apply).map { imageMaps =>
        val path = target.root.resolve(s"${target.stemValue}_contrast-${safeEntity(contrastId)}_statmap.nii")
        PlannedNifti(ResultWrittenArtifactKind.ContrastMaps(contrastId), path, imageMaps.names, imageMaps)
      }
    }

  private def planIndividualParameterMaps(
      manifest: ResultManifest,
      target: ResultManifestExportTarget
  ): Either[ResultManifestIoError, Vector[PlannedNifti]] =
    buildAll(manifest.parameters) { map =>
      FitImageMaps.fromParameterMaps(Vector(map)).left.map(ResultManifestIoError.InvalidManifest.apply).map { imageMaps =>
        val parameter = safeEntity(map.parameterName.value)
        val statistic = safeEntity(map.statistic.label)
        val path = target.root.resolve(s"${target.stemValue}_desc-${parameter}_stat-${statistic}_statmap.nii")
        PlannedNifti(ResultWrittenArtifactKind.ParameterMaps(map.statistic), path, imageMaps.names, imageMaps)
      }
    }

  private def planIndividualContrastMaps(
      manifest: ResultManifest,
      target: ResultManifestExportTarget
  ): Either[ResultManifestIoError, Vector[PlannedNifti]] =
    buildAll(manifest.contrasts) { map =>
      FitImageMaps.fromContrastMaps(Vector(map)).left.map(ResultManifestIoError.InvalidManifest.apply).map { imageMaps =>
        val contrast = safeEntity(map.contrastId.value)
        val statistic = safeEntity(map.statistic.label)
        val path = target.root.resolve(s"${target.stemValue}_contrast-${contrast}_stat-${statistic}_statmap.nii")
        PlannedNifti(ResultWrittenArtifactKind.ContrastMaps(map.contrastId.value), path, imageMaps.names, imageMaps)
      }
    }

  private def validateUniquePaths(
      niftis: Vector[PlannedNifti],
      manifest: ResultManifest,
      target: ResultManifestExportTarget
  ): Either[ResultManifestIoError, Unit] =
    val fixed =
      Vector(
        Some(target.root.resolve(s"${target.stemValue}_resultmanifest.json")),
        manifest.coefficientCovariance.map(_ => target.root.resolve(s"${target.stemValue}_desc-coefficientCovariance_covariance.tsv"))
      ).flatten
    val paths = niftis.map(_.path) ++ fixed
    paths.groupBy(_.normalize()).collectFirst { case (path, occurrences) if occurrences.length > 1 => path } match
      case Some(path) =>
        Left(ResultManifestIoError.InvalidTarget(s"multiple result artifacts resolve to '${path.getFileName}'"))
      case None =>
        Right(())

  private def writeCovariance(
      artifact: CoefficientCovarianceArtifact,
      target: ResultManifestExportTarget
  ): ResultWrittenArtifact =
    val path = target.root.resolve(s"${target.stemValue}_desc-coefficientCovariance_covariance.tsv")
    Files.writeString(path, covarianceTsv(artifact), StandardCharsets.UTF_8)
    ResultWrittenArtifact(
      kind = ResultWrittenArtifactKind.CoefficientCovariance,
      path = path,
      labels = artifact.parameterNames.map(_.value)
    )

  private def writeSidecar(
      manifest: ResultManifest,
      target: ResultManifestExportTarget,
      artifacts: Vector[ResultWrittenArtifact]
  ): ResultWrittenArtifact =
    val path = target.root.resolve(s"${target.stemValue}_resultmanifest.json")
    Files.writeString(path, sidecarJson(manifest, target, artifacts), StandardCharsets.UTF_8)
    ResultWrittenArtifact(ResultWrittenArtifactKind.ManifestSidecar, path, Vector("manifest"))

  private def covarianceTsv(artifact: CoefficientCovarianceArtifact): String =
    val names = artifact.parameterNames.map(_.value)
    val rows = Vector.newBuilder[String]
    artifact.scope match
      case CoefficientCovarianceScope.Shared =>
        rows += "scope\tparameter_i\tparameter_j\tvalue"
        appendMatrixRows(rows, "shared", None, names, artifact.matrices.head)
      case CoefficientCovarianceScope.Voxelwise =>
        rows += "scope\tvoxel\tparameter_i\tparameter_j\tvalue"
        var voxel = 0
        while voxel < artifact.matrices.length do
          appendMatrixRows(rows, "voxelwise", Some(artifact.voxelIndices(voxel)), names, artifact.matrices(voxel))
          voxel += 1
    rows.result().mkString("", "\n", "\n")

  private def appendMatrixRows(
      rows: VectorBuilder[String],
      scope: String,
      voxel: Option[Int],
      names: Vector[String],
      matrix: DMat
  ): Unit =
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        val prefix =
          voxel match
            case None        => s"$scope\t${names(row)}\t${names(col)}"
            case Some(index) => s"$scope\t$index\t${names(row)}\t${names(col)}"
        rows += s"$prefix\t${formatDouble(matrix(row, col))}"
        col += 1
      row += 1

  private def sidecarJson(
      manifest: ResultManifest,
      target: ResultManifestExportTarget,
      artifacts: Vector[ResultWrittenArtifact]
  ): String =
    val artifactJson =
      artifacts
        .map { artifact =>
          val labels = artifact.labels.map(label => "\"" + escape(label) + "\"").mkString("[", ", ", "]")
          s"""    {"kind": "${escape(artifact.kind.label)}", "path": "${escape(target.root.relativize(artifact.path).toString)}", "labels": $labels}"""
        }
        .mkString("[\n", ",\n", "\n  ]")
    val columns = manifest.provenance.columnNames.map(value => "\"" + escape(value) + "\"").mkString("[", ", ", "]")
    val structuralColumns =
      manifest.provenance.coefficientAxis match
        case None => "[]"
        case Some(axis) =>
          axis.columns
            .map(column =>
              s"{\"id\": \"${escape(column.id.value)}\", \"label\": \"${escape(column.renderedLabel)}\", \"ordinal\": ${column.ordinal.oneBased}, \"origin\": \"${escape(column.origin.canonical)}\"}"
            )
            .mkString("[", ", ", "]")
    val designFingerprint =
      manifest.provenance.coefficientAxis
        .map(axis => s"\"${escape(axis.designFingerprint.value)}\"")
        .getOrElse("null")
    val notes = manifest.provenance.notes.map(value => "\"" + escape(value) + "\"").mkString("[", ", ", "]")
    val inference =
      manifest.provenance.coefficientInference match
        case None =>
          "null"
        case Some(value) =>
          val inferable = value.inferableColumns.map(column => "\"" + escape(column) + "\"").mkString("[", ", ", "]")
          val inferableIds = value.inferableColumnIds.map(column => "\"" + escape(column.value) + "\"").mkString("[", ", ", "]")
          s"""{"method": "${escape(value.method.label)}", "scope": "${escape(value.scopeLabel)}", "inferable_columns": $inferable, "inferable_column_ids": $inferableIds}"""
    val responsePreparation =
      manifest.provenance.responsePreparation match
        case None =>
          "null"
        case Some(value) =>
          value.records
            .map { record =>
              s"""{"step": "${escape(record.step.label)}", "disposition": "${escape(record.disposition.label)}", "detail": "${escape(record.disposition.detailText)}"}"""
            }
            .mkString("[", ", ", "]")
    val rankReports =
      manifest.provenance.rankReports
        .map { report =>
          val pivotIds = report.pivotOrder.map(column => "\"" + escape(column.id.value) + "\"").mkString("[", ", ", "]")
          val independentIds = report.independentColumnIds.map(column => "\"" + escape(column.value) + "\"").mkString("[", ", ", "]")
          val aliasedIds = report.aliasedColumnIds.map(column => "\"" + escape(column.value) + "\"").mkString("[", ", ", "]")
          val diagonal = report.diagonalR.map(formatDouble).mkString("[", ", ", "]")
          val condition = report.conditionEstimate.map(formatDouble).getOrElse("null")
          s"""{"design_fingerprint": "${escape(report.designFingerprint.value)}", "method": "${escape(report.method.toString)}", "predictors": ${report.predictorCount}, "rank": ${report.numericalRank}, "tolerance_convention": "${escape(report.toleranceConvention.toString)}", "tolerance": ${formatDouble(report.tolerance)}, "pivot_column_ids": $pivotIds, "independent_column_ids": $independentIds, "aliased_column_ids": $aliasedIds, "diagonal_r": $diagonal, "condition_estimate": $condition}"""
        }
        .mkString("[", ", ", "]")
    val voxelStatuses =
      manifest.provenance.voxelStatuses match
        case None =>
          "null"
        case Some(records) =>
          records
            .map(record => s"""{"voxel": ${record.voxelIndex}, "status": "${escape(record.status.label)}"}""")
            .mkString("[", ", ", "]")
    val contrastExclusions =
      manifest.contrasts
        .map(_.contrastId.value)
        .distinct
        .flatMap { contrastId =>
          val exclusions = manifest.contrasts.find(_.contrastId.value == contrastId).toVector.flatMap(_.excludedVoxels)
          if exclusions.isEmpty then None
          else
            val voxels =
              exclusions
                .map(exclusion => s"""{"voxel": ${exclusion.voxelIndex}, "status": "${escape(exclusion.status.label)}"}""")
                .mkString("[", ", ", "]")
            Some(s"""{"contrast_id": "${escape(contrastId)}", "voxels": $voxels}""")
        }
        .mkString("[", ", ", "]")
    s"""{
       |  "format": "${target.format.label}",
       |  "stem": "${escape(target.stemValue)}",
       |  "nifti_map_layout": "${target.bidsNiftiMapLayout.label}",
       |  "engine": "${escape(manifest.provenance.engine.toString)}",
       |  "source": "${escape(manifest.provenance.source)}",
       |  "columns": $columns,
       |  "design_fingerprint": $designFingerprint,
       |  "structural_columns": $structuralColumns,
       |  "inference": $inference,
       |  "response_preparation": $responsePreparation,
       |  "rank_reports": $rankReports,
       |  "voxel_statuses": $voxelStatuses,
       |  "contrast_exclusions": $contrastExclusions,
       |  "notes": $notes,
       |  "artifacts": $artifactJson
       |}
       |""".stripMargin

  private def safeEntity(value: String): String =
    val clean = value.filter(_.isLetterOrDigit)
    if clean.nonEmpty then clean else "value"

  private def escape(value: String): String =
    val out = new StringBuilder
    var i = 0
    while i < value.length do
      value.charAt(i) match
        case '"'  => out.append("\\\"")
        case '\\' => out.append("\\\\")
        case '\n' => out.append("\\n")
        case '\r' => out.append("\\r")
        case '\t' => out.append("\\t")
        case c    => out.append(c)
      i += 1
    out.result()

  private def formatDouble(value: Double): String =
    if value.isWhole then value.toLong.toString else value.toString

private type VectorBuilder[A] = scala.collection.mutable.Builder[A, Vector[A]]

private def buildAll[A, B](
    values: Vector[A]
)(
    f: A => Either[ResultManifestIoError, B]
): Either[ResultManifestIoError, Vector[B]] =
  val out = Vector.newBuilder[B]
  var index = 0
  while index < values.length do
    f(values(index)) match
      case Left(error) => return Left(error)
      case Right(value) => out += value
    index += 1
  Right(out.result())
