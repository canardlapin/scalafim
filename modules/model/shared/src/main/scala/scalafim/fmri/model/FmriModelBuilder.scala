package scalafim.fmri.model

import scalafim.dataset.{DatasetEvents, FmriDataset}
import scalafim.fmri.design.ColumnId
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept, NaAction, NuisanceCheck}
import scalafim.fmri.design.contrast.ContrastSpec
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.{Hrf, Hrfs, Seconds}
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.linalg.Mat

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
    nuisance: Option[NuisanceRegressors] = None
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

  def buildPlan(dataset: FmriDataset, spec: ModelBuildSpec): FitPlan =
    FitPlan(
      model = buildModel(dataset, spec),
      strategy = spec.strategy
    )

  def buildModel(dataset: FmriDataset, spec: ModelBuildSpec): FmriModel =
    val table = eventsTable(dataset.events)
    val durations = durationValues(table, spec)

    val eventModel =
      spec.blockColumn match
        case Some(column) =>
          require(table.contains(column), s"block column '$column' is not present in dataset events")
          EventModelBuilder.buildWithBlockFormula(
            formula = spec.formulaText.value,
            data = table,
            samplingFrame = dataset.samplingFrame,
            block = s"~$column",
            durations = durations,
            defaultHrf = spec.defaultHrf,
            precision = spec.precision,
            dropEmpty = spec.dropEmpty,
            summate = spec.summate,
            contrastSets = spec.contrastSets,
            strict = spec.strict
          )
        case None =>
          EventModelBuilder.build(
            formula = spec.formulaText.value,
            data = table,
            samplingFrame = dataset.samplingFrame,
            blockIds = Vector.fill(table.nrows)(0),
            durations = durations,
            defaultHrf = spec.defaultHrf,
            precision = spec.precision,
            dropEmpty = spec.dropEmpty,
            summate = spec.summate,
            contrastSets = spec.contrastSets,
            strict = spec.strict
          )

    val baseline = BaselineModel.build(
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
    )

    FmriModel(eventModel, baseline, dataset)

  def eventsTable(events: DatasetEvents): DataTable =
    require(!events.isEmpty, "dataset events are required to build an fMRI model")

    val allKeys = events.rows.iterator.flatMap(_.keysIterator).toSet
    require(allKeys.nonEmpty, "dataset events must contain at least one column")

    events.rows.zipWithIndex.foreach { case (row, rowIndex) =>
      val keys = row.keySet
      require(
        keys == allKeys,
        s"dataset event row $rowIndex has columns ${keys.toVector.sorted.mkString(", ")}; expected ${allKeys.toVector.sorted.mkString(", ")}"
      )
    }

    val columns = allKeys.toVector.sorted.map { key =>
      val values = events.rows.map(row => row(key))
      key -> inferColumn(key, values)
    }

    DataTable(events.nrows, columns)

  private def durationValues(table: DataTable, spec: ModelBuildSpec): Vector[Double] =
    spec.durationColumn match
      case None => Vector(0.0)
      case Some(column) =>
        require(table.contains(column), s"duration column '$column' is not present in dataset events")
        val values = table.doubles(column)
        require(values.forall(v => v.isFinite && v >= 0.0), s"duration column '$column' must be finite and non-negative")
        values

  private def inferColumn(name: String, values: Vector[String]): Column =
    val trimmed = values.map(_.trim)

    val ints = trimmed.map(parseInt)
    if ints.forall(_.isDefined) then Column.Ints(ints.map(_.get))
    else
      val doubles = trimmed.map(parseDouble)
      if doubles.forall(_.isDefined) then
        val out = doubles.map(_.get)
        require(out.forall(_.isFinite), s"numeric event column '$name' must contain only finite values")
        Column.Doubles(out)
      else
        val bools = trimmed.map(parseBoolean)
        if bools.forall(_.isDefined) then Column.Bools(bools.map(_.get))
        else Column.Strings(values)

  private def parseInt(value: String): Option[Int] =
    try Some(value.toInt)
    catch case _: NumberFormatException => None

  private def parseDouble(value: String): Option[Double] =
    try Some(value.toDouble)
    catch case _: NumberFormatException => None

  private def parseBoolean(value: String): Option[Boolean] =
    value.toLowerCase match
      case "true" | "t"  => Some(true)
      case "false" | "f" => Some(false)
      case _             => None
