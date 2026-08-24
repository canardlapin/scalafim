package scalafim.fmri.model

import scalafim.dataset.{DatasetEvents, DatasetValue, FmriDataset}
import scalafim.fmri.design.{ColumnId, DegenerateModulatorPolicy, EmptyCellPolicy, FactorLevelRegistry, FactorSchemaBinding, HrfByCell, HrfByPhase, MissingValuePolicy, ModulatorOrthogonalizationPlan}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept, NaAction, NuisanceCheck}
import scalafim.fmri.design.contrast.ContrastSpec
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.{Hrf, Hrfs, Seconds}
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.linalg.Mat

/** One run of scan-aligned regressors, represented separately from events. */
final class SampledRegressorRun private (
    val matrix: Mat,
    val columnIds: Vector[ColumnId]
):
  def rows: Int = matrix.rows
  def columns: Int = matrix.cols

object SampledRegressorRun:
  /** Construct homogeneous scan columns without transposing them at the call site.
    *
    * Non-finite values are retained so the selected nuisance `NaAction` remains
    * the single owner of missing-value semantics.
    */
  def fromColumns(columns: (String, Seq[Double])*): Either[ModelError, SampledRegressorRun] =
    if columns.isEmpty then
      Left(ModelError.InvalidParameter("sampled regressors", "at least one column is required"))
    else
      val checked = Vector.newBuilder[(ColumnId, Vector[Double])]
      val iterator = columns.iterator
      var error = Option.empty[ModelError]
      while iterator.hasNext && error.isEmpty do
        val (name, values) = iterator.next()
        ColumnId(name) match
          case Left(idError) =>
            error = Some(ModelError.InvalidId("sampled regressor", name, idError.message))
          case Right(id) =>
            checked += id -> values.toVector

      error match
        case Some(value) => Left(value)
        case None =>
          val parsed = checked.result()
          val duplicate = parsed
            .groupMapReduce(_._1.value)(_ => 1)(_ + _)
            .iterator
            .collect { case (name, count) if count > 1 => name }
            .toVector
            .sorted
            .headOption
          duplicate match
            case Some(name) =>
              Left(ModelError.InvalidParameter("sampled regressors", s"duplicate column '$name'"))
            case None =>
              val rowCount = parsed.head._2.length
              if rowCount == 0 then
                Left(ModelError.InvalidParameter("sampled regressors", "columns must contain at least one scan"))
              else
                parsed.find(_._2.length != rowCount) match
                  case Some((id, values)) =>
                    Left(
                      ModelError.InvalidParameter(
                        "sampled regressors",
                        s"column '${id.value}' has ${values.length} scans; expected $rowCount"
                      )
                    )
                  case None =>
                    val data = new Array[Double](rowCount * parsed.length)
                    var row = 0
                    while row < rowCount do
                      var column = 0
                      while column < parsed.length do
                        data(row * parsed.length + column) = parsed(column)._2(row)
                        column += 1
                      row += 1
                    Right(
                      new SampledRegressorRun(
                        matrix = Mat.unsafe(rowCount, parsed.length, data),
                        columnIds = parsed.map(_._1)
                      )
                    )

final case class NuisanceRegressors(
    matrices: Vector[Mat],
    names: Option[Vector[Vector[String]]] = None,
    check: NuisanceCheck = NuisanceCheck.Warn,
    naAction: NaAction = NaAction.Drop,
    tol: Double = BaselineModel.DefaultNuisanceTol,
    duplicateThreshold: Double = 1.0 - BaselineModel.DefaultNuisanceTol
):
  require(matrices.nonEmpty, "nuisance matrices must be non-empty")
  names.foreach(ns => require(ns.length == matrices.length, "nuisance names must match nuisance matrices"))
  names.foreach { ns =>
    ns.zip(matrices).foreach { case (columns, matrix) =>
      require(columns.length == matrix.cols, "nuisance names must match nuisance matrix columns")
    }
  }
  require(tol >= 0.0 && tol.isFinite, "nuisance tolerance must be non-negative and finite")
  require(duplicateThreshold >= 0.0 && duplicateThreshold <= 1.0, "nuisance duplicate threshold must be in [0, 1]")

  val columnIds: Option[Vector[Vector[ColumnId]]] =
    names.map { blocks =>
      blocks.map { block =>
        block.map { name =>
          ColumnId(name).fold(error => throw new IllegalArgumentException(ModelError.InvalidId("nuisance column", name, error.message).message), identity)
        }
      }
    }

object NuisanceRegressors:
  /** Combine validated, scan-aligned runs into a nuisance specification.
    *
    * Dataset/model construction subsequently checks each run's scan count
    * against the sampling frame, so run alignment fails through `ModelError`.
    */
  def fromRuns(
      runs: Seq[SampledRegressorRun],
      check: NuisanceCheck = NuisanceCheck.Warn,
      naAction: NaAction = NaAction.Drop,
      tol: Double = BaselineModel.DefaultNuisanceTol,
      duplicateThreshold: Double = 1.0 - BaselineModel.DefaultNuisanceTol
  ): Either[ModelError, NuisanceRegressors] =
    if runs.isEmpty then
      Left(ModelError.InvalidParameter("nuisance regressors", "at least one run is required"))
    else
      Right(
        NuisanceRegressors(
          matrices = runs.iterator.map(_.matrix).toVector,
          names = Some(runs.iterator.map(_.columnIds.map(_.value)).toVector),
          check = check,
          naAction = naAction,
          tol = tol,
          duplicateThreshold = duplicateThreshold
        )
      )

final case class ModelBuildSpec(
    formula: String,
    blockColumn: Option[String] = None,
    durationColumn: Option[String] = None,
    baselineBasis: BaselineBasis = BaselineBasis.Constant,
    baselineDegree: Int = 1,
    baselineIntercept: Intercept = Intercept.Global,
    strategy: FitStrategy = FitStrategy.Default,
    defaultHrf: Hrf = Hrfs.SPMG1,
    precision: Seconds = 0.3.s,
    dropEmpty: Boolean = true,
    summate: Boolean = true,
    strict: Boolean = false,
    contrastSets: Map[String, ContrastSpec.ContrastSet] = Map.empty,
    nuisance: Option[NuisanceRegressors] = None,
    factorLevels: FactorLevelRegistry = FactorLevelRegistry.empty,
    emptyCellPolicy: EmptyCellPolicy = EmptyCellPolicy.UseDropEmptyFlag,
    factorSchemaBinding: Option[FactorSchemaBinding] = None,
    hrfByCell: Option[HrfByCell] = None,
    hrfByPhase: Option[HrfByPhase] = None,
    missingValuePolicy: MissingValuePolicy = MissingValuePolicy.ZeroContribution,
    degenerateModulatorPolicy: DegenerateModulatorPolicy = DegenerateModulatorPolicy.RetainAndReport,
    orthogonalization: ModulatorOrthogonalizationPlan = ModulatorOrthogonalizationPlan.None
):
  require(formula.trim.nonEmpty, "model formula must be non-empty")
  require(baselineDegree >= 1, "baseline degree must be at least 1")
  val formulaText: FormulaText = FormulaText.unsafe(formula)
  val blockColumnId: Option[ColumnId] = blockColumn.map(ModelBuildSpec.columnId("block column", _))
  val durationColumnId: Option[ColumnId] = durationColumn.map(ModelBuildSpec.columnId("duration column", _))
  def engine: FitEngine = strategy.engine
  def config: FitConfig = strategy.config

object ModelBuildSpec:
  private[model] def columnId(kind: String, name: String): ColumnId =
    ColumnId(name).fold(error => throw new IllegalArgumentException(ModelError.InvalidId(kind, name, error.message).message), identity)

object FmriModelBuilder:

  /** Compatibility wrapper over the total [[buildPlanEither]] entry point. */
  def buildPlan(dataset: FmriDataset, spec: ModelBuildSpec): FitPlan =
    unsafe(buildPlanEither(dataset, spec))

  /** Build an inspectable fit plan without erasing design or baseline errors. */
  def buildPlanEither(dataset: FmriDataset, spec: ModelBuildSpec): Either[ModelError, FitPlan] =
    buildModelEither(dataset, spec).flatMap(FitPlan.make(_, spec.strategy))

  /** Compatibility wrapper over the total [[buildModelEither]] entry point. */
  def buildModel(dataset: FmriDataset, spec: ModelBuildSpec): FmriModel =
    unsafe(buildModelEither(dataset, spec))

  /** Compile dataset events, task design, and baseline into one typed model. */
  def buildModelEither(dataset: FmriDataset, spec: ModelBuildSpec): Either[ModelError, FmriModel] =
    for
      table <- eventsTableEither(dataset.events)
      durations <- durationValuesEither(table, spec)
      blockPlan <- blockPlanEither(table, spec)
      request <- EventModelBuilder.EventDesignRequest.fromText(
        formula = spec.formulaText.value,
        data = table,
        samplingFrame = dataset.samplingFrame,
        blockPlan = blockPlan,
        durationPlan = EventModelBuilder.DurationPlan(durations),
        options = EventModelBuilder.BuildOptions(
          defaultHrf = spec.defaultHrf,
          precision = spec.precision,
          dropEmpty = spec.dropEmpty,
          summate = spec.summate,
          strict = spec.strict,
          factorLevels = spec.factorLevels,
          emptyCellPolicy = spec.emptyCellPolicy,
          factorSchemaBinding = spec.factorSchemaBinding,
          hrfByCell = spec.hrfByCell,
          hrfByPhase = spec.hrfByPhase,
          missingValuePolicy = spec.missingValuePolicy,
          degenerateModulatorPolicy = spec.degenerateModulatorPolicy,
          orthogonalization = spec.orthogonalization
        ),
        extensions = EventModelBuilder.DesignExtensionEnv(
          contrastSets = spec.contrastSets
        )
      ).left.map(ModelError.fromDesignError)
      eventModel <- EventModelBuilder.buildEither(request).left.map(ModelError.fromDesignError)
      baseline <- BaselineModel.buildEither(
        samplingFrame = dataset.samplingFrame,
        basis = spec.baselineBasis,
        degree = spec.baselineDegree,
        intercept = spec.baselineIntercept,
        nuisanceList = spec.nuisance.map(_.matrices),
        nuisanceCheck = spec.nuisance.map(_.check).getOrElse(NuisanceCheck.Warn),
        naAction = spec.nuisance.map(_.naAction).getOrElse(NaAction.Drop),
        nuisanceNames = spec.nuisance.flatMap(_.names),
        nuisanceTol = spec.nuisance.map(_.tol).getOrElse(BaselineModel.DefaultNuisanceTol),
        duplicateThreshold = spec.nuisance.map(_.duplicateThreshold).getOrElse(1.0 - BaselineModel.DefaultNuisanceTol)
      ).left.map(ModelError.fromBaselineError)
      model <- FmriModel.make(eventModel, baseline, dataset)
    yield model

  def eventsTable(events: DatasetEvents): DataTable =
    unsafe(eventsTableEither(events))

  def eventsTableEither(events: DatasetEvents): Either[ModelError, DataTable] =
    if events.isEmpty then
      Left(ModelError.InvalidParameter("dataset events", "at least one event row is required"))
    else
      val allKeys = events.columns
      if allKeys.isEmpty then
        Left(ModelError.InvalidParameter("dataset events", "at least one column is required"))
      else
        val columns = allKeys.map { key =>
          val values = events.column(key)
          key.value -> inferColumn(values)
        }
        Right(DataTable(events.nrows, columns))

  private def durationValuesEither(table: DataTable, spec: ModelBuildSpec): Either[ModelError, Vector[Double]] =
    spec.durationColumnId match
      case Some(columnId) =>
        val column = columnId.value
        if !table.contains(columnId) then
          Left(ModelError.InvalidParameter("duration column", s"'$column' is not present in dataset events"))
        else
          table.get[Double](columnId).left.map(ModelError.fromDesignError).flatMap { values =>
            if values.forall(v => v.isFinite && v >= 0.0) then Right(values)
            else Left(ModelError.InvalidParameter("duration column", s"'$column' must be finite and non-negative"))
          }
      case None => Right(Vector(0.0))

  private def blockPlanEither(
      table: DataTable,
      spec: ModelBuildSpec
  ): Either[ModelError, EventModelBuilder.BlockPlan] =
    spec.blockColumnId match
      case Some(columnId) =>
        val column = columnId.value
        if table.contains(columnId) then Right(EventModelBuilder.BlockPlan.Formula(s"~$column"))
        else Left(ModelError.InvalidParameter("block column", s"'$column' is not present in dataset events"))
      case None =>
        Right(EventModelBuilder.BlockPlan.explicit(Vector.fill(table.nrows)(0)))

  private def inferColumn(values: Vector[DatasetValue]): Column =
    val ints = values.map(_.asInt)
    if ints.forall(_.isDefined) then Column.Ints(ints.flatten)
    else
      val doubles = values.map(_.asDouble)
      if doubles.forall(_.isDefined) then
        // Keep non-finite values in ordinary numeric event columns.  The
        // selected EventModelBuilder.MissingValuePolicy owns their scientific
        // interpretation; rejecting them here would make that policy
        // unreachable through the public DatasetEvents/ModelBuildSpec path.
        Column.Doubles(doubles.flatten)
      else
        val bools = values.map(_.asBoolean)
        if bools.forall(_.isDefined) then Column.Bools(bools.flatten)
        else Column.Strings(values.map(_.asString))

  private def unsafe[A](value: Either[ModelError, A]): A =
    value.fold(error => throw new IllegalArgumentException(error.message), identity)
