package scalafim.fmri.workflow

import bids4s.{BidsTable, ConfoundSelectionConfig}
import scalafim.dataset.*
import scalafim.dataset.io.NiftiStagingCache
import scalafim.fmri.model.{FitPlan, FmriModelBuilder, ModelBuildSpec}

import java.net.URI
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

final case class OpenedFirstLevelDataset private[workflow] (
    unit: FirstLevelUnit,
    dataset: SynchronousFmriDataset,
    tables: BoundFirstLevelTables,
    sampling: ResolvedSamplingReference
):
  def buildPlan(spec: ModelBuildSpec): Either[WorkflowError, FitPlan] =
    tables.modelSpec(spec).flatMap { boundSpec =>
      FmriModelBuilder.buildPlanEither(dataset, boundSpec)
        .left.map(e => WorkflowError.InvalidUnit(unit.id.value, e.message))
    }

/** Loads only the catalog unit's explicitly selected companion artifacts. */
object FirstLevelUnitDataset:
  def open(
      unit: FirstLevelUnit,
      datasetId: DatasetId,
      runColumn: String = "run",
      confounds: Option[ConfoundSelectionConfig] = None,
      staging: Option[NiftiStagingCache] = None,
      metadata: DatasetMetadata = DatasetMetadata.Empty,
      eventColumns: Option[Vector[String]] = None,
      samplingReference: SamplingReference = SamplingReference.VolumeMidpoint,
      responseAccess: FirstLevelResponseAccess = FirstLevelResponseAccess.Immediate
  ): Either[WorkflowError, OpenedFirstLevelDataset] =
    for
      sampling <- samplingReference.resolve(unit.runs)
      loaded <- WorkflowValidation.traverse(unit.runs) { run =>
        for
          events <- readTable(run.events.location, run.id)
          nuisance <- confounds match
            case None => Right(None)
            case Some(_) => run.confounds match
              case None => Left(WorkflowError.InvalidRun(run.id.value, "requested confounds have no selected artifact"))
              case Some(artifact) => readTable(artifact.location, run.id).map(Some(_))
        yield run.id -> FirstLevelRunTables(events, nuisance)
      }
      tables <- FirstLevelTables.bind(unit.runs, loaded.toMap, runColumn, confounds, eventColumns)
      source <- FirstLevelUnitSource.open(unit, staging, metadata,responseAccess)
        .left.map(e => WorkflowError.InvalidUnit(unit.id.value, e.message))
      backend <- source.backend(datasetId, metadata)
        .left.map(e => WorkflowError.InvalidUnit(unit.id.value, e.message))
      dataset <- FmriDataset.open(backend,
        sampling.frame,
        unit.runIds, tables.events).left.map(e => WorkflowError.InvalidUnit(unit.id.value, e.message))
    yield OpenedFirstLevelDataset(unit, dataset, tables, sampling)

  private def readTable(location: ArtifactLocation, run: RunId): Either[WorkflowError, BidsTable] =
    try
      val uri = URI.create(location.value)
      if uri.getScheme != "file" then Left(WorkflowError.InvalidRun(run.value, s"table is not a local file URI: ${location.value}"))
      else BidsTable.parse(Files.readString(Path.of(uri)))
        .left.map(e => WorkflowError.InvalidRun(run.value, s"${location.value}: ${e.message}"))
    catch
      case NonFatal(error) => Left(WorkflowError.InvalidRun(run.value, s"cannot read ${location.value}: ${error.getMessage}"))
