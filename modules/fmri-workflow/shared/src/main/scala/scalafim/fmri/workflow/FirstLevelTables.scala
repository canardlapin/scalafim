package scalafim.fmri.workflow

import bids4s.{BidsTable, ConfoundSelection, ConfoundSelectionConfig, ConfoundSelector, EventsTable}
import scalafim.dataset.*
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ModelBuildSpec, NuisanceRegressors}

/** Already selected companions: no filename discovery occurs during binding. */
final case class FirstLevelRunTables(events: BidsTable, confounds: Option[BidsTable] = None)

final case class BoundFirstLevelTables private[workflow] (
    events: DatasetEvents,
    runIds: Vector[RunId],
    runColumn: String,
    nuisance: Option[NuisanceRegressors],
    confoundSelections: Vector[(RunId, ConfoundSelection)],
    sourceTables: Map[RunId, FirstLevelRunTables]
):
  /** Refuse competing configuration rather than silently replacing it. */
  def modelSpec(spec: ModelBuildSpec): Either[WorkflowError, ModelBuildSpec] =
    if spec.nuisance.nonEmpty then Left(WorkflowError.InvalidCatalog("model already supplies nuisance regressors"))
    else if spec.blockColumn.exists(_ != runColumn) then
      Left(WorkflowError.InvalidCatalog(s"model block column must be '$runColumn'"))
    else Right(spec.copy(blockColumn = Some(runColumn), nuisance = nuisance))

object FirstLevelTables:
  /** Bind a complete event table, or explicitly select the model's event inputs.
    * Onset, duration and any source run identity are always validated. Selection
    * never removes rows or replaces missing values; requested columns must exist
    * and be complete. Original companion tables remain available in the result.
    */
  def bind(
      runs: Vector[RunInput],
      tables: Map[RunId, FirstLevelRunTables],
      runColumn: String,
      confounds: Option[ConfoundSelectionConfig] = None,
      eventColumns: Option[Vector[String]] = None
  ): Either[WorkflowError, BoundFirstLevelTables] =
    val ids = runs.map(_.id)
    if runs.isEmpty then Left(WorkflowError.InvalidCatalog("table binding requires at least one run"))
    else if ids.distinct.length != ids.length then Left(WorkflowError.InvalidCatalog("table binding has duplicate runs"))
    else if tables.keySet != ids.toSet then Left(WorkflowError.InvalidCatalog("table keys must exactly match selected run IDs"))
    else for
      runField <- DatasetFieldId.make(runColumn).left.map(e => WorkflowError.InvalidCatalog(e.message))
      bound <- WorkflowValidation.traverse(runs) { run =>
        val input = tables(run.id)
        for
          _ <- EventsTable.from(input.events).left.map(e => WorkflowError.InvalidRun(run.id.value, e.message))
          projected <- eventColumns match
            case None => Right(input.events)
            case Some(columns) =>
              val required = (Vector("onset", "duration") ++ columns ++
                input.events.columns.filter(_ == runColumn)).distinct
              input.events.select(required).left.map(e => WorkflowError.InvalidRun(run.id.value, e.message))
          rows <- WorkflowValidation.traverse(projected.rows.zipWithIndex) { (row, rowIndex) =>
            val cells = projected.columns.zip(row)
            val missing = cells.collect { case (name, None) => name }
            if missing.nonEmpty then Left(WorkflowError.InvalidRun(run.id.value,
              s"event row $rowIndex has missing values in ${missing.mkString(", ")}; resolve them before binding"))
            else if cells.exists { case (name, value) => name == runColumn && !value.contains(run.id.value) } then
              Left(WorkflowError.InvalidRun(run.id.value, s"event row $rowIndex conflicts with selected run identity"))
            else
              DatasetEventRow.fromStrings(cells.collect { case (name, Some(value)) => name -> value }.toMap)
                .flatMap(parsed => DatasetEventRow.fromValues(parsed.values.updated(runField, DatasetValue.Text(run.id.value))))
                .left.map(e => WorkflowError.InvalidRun(run.id.value, s"event row $rowIndex: ${e.message}"))
          }
          selected <- confounds match
            case None => Right(None)
            case Some(config) => input.confounds match
              case None => Left(WorkflowError.InvalidRun(run.id.value, "requested confounds have no selected table"))
              case Some(table) if table.nrows != run.timepoints =>
                Left(WorkflowError.InvalidRun(run.id.value, s"confound rows ${table.nrows} differ from scans ${run.timepoints}"))
              case Some(table) => ConfoundSelector.select(table, config)
                .left.map(e => WorkflowError.InvalidRun(run.id.value, e.message)).map(Some(_))
          matrix <- selected match
            case None => Right(None)
            case Some(selection) =>
              if selection.table.ncols == 0 then Left(WorkflowError.InvalidRun(run.id.value, "confound selection retained no columns"))
              else WorkflowValidation.traverse(selection.table.rows.zipWithIndex) { (row, rowIndex) =>
                WorkflowValidation.traverse(row.zip(selection.table.columns)) { (value, column) =>
                  value.flatMap(_.toDoubleOption).filter(_.isFinite)
                    .toRight(WorkflowError.InvalidRun(run.id.value, s"confound '$column' row $rowIndex is not finite after selection"))
                }
              }.map(rows => Some(Mat.fromRows(rows)))
        yield (rows, selected, matrix)
      }
      events <- DatasetEvents.fromTypedRows(bound.flatMap(_._1)).left.map(e => WorkflowError.InvalidCatalog(e.message))
    yield BoundFirstLevelTables(events, ids, runColumn,
      if confounds.isEmpty then None else Some(NuisanceRegressors(
        bound.flatMap(_._3), Some(bound.flatMap(_._2.map(_.table.columns))))),
      ids.zip(bound.map(_._2)).collect { case (id, Some(selection)) => id -> selection }, tables)
