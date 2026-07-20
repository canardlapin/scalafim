package scalafim.fmri.workflow

import scalafim.bids.{BidsQuery, ConfoundSelectionConfig}
import scalafim.dataset.DatasetId
import scalafim.fmri.design.baseline.BaselineSpec
import scalafim.fmri.design.formula.ModelFormula
import scalafim.fmri.fit.{FContrast, TContrast}
import scalafim.fmri.group.{CovariateName, DesignTermName, GroupContrastName, GroupWeighting, InterceptPolicy}
import scalafim.fmri.model.FitStrategy

enum RunGrouping:
  case BySubjectSessionTask
  case OneUnitPerRun

enum MaskPolicy:
  case RequireCommonDerivative
  case IntersectRunMasks
  case Explicit(mask: WorkflowArtifactRef[MaskImageResource])

final case class DatasetRecipe private (
    datasetId: DatasetId,
    project: WorkflowArtifactRef[BidsProjectResource],
    boldQuery: BidsQuery,
    runGrouping: RunGrouping,
    maskPolicy: MaskPolicy,
    confounds: Option[ConfoundSelectionConfig]
):
  require(boldQuery.filename.nonEmpty, "BIDS query must contain filename patterns")
  require(boldQuery.filename.forall(_.trim.nonEmpty), "BIDS query filename patterns must be non-empty")
  require(boldQuery.filters.forall(filter => filter.values.nonEmpty && filter.values.forall(_.trim.nonEmpty)), "BIDS entity filters must contain non-empty values")

object DatasetRecipe:
  def make(
      datasetId: DatasetId,
      project: WorkflowArtifactRef[BidsProjectResource],
      boldQuery: BidsQuery,
      runGrouping: RunGrouping = RunGrouping.BySubjectSessionTask,
      maskPolicy: MaskPolicy = MaskPolicy.IntersectRunMasks,
      confounds: Option[ConfoundSelectionConfig] = None
  ): Either[WorkflowError, DatasetRecipe] =
    if boldQuery.filename.isEmpty then
      Left(WorkflowError.InvalidValue("BIDS query", "filename", "requires at least one filename pattern"))
    else if boldQuery.filename.exists(_.trim.isEmpty) then
      Left(WorkflowError.InvalidValue("BIDS query", "filename", "patterns must be non-empty"))
    else if boldQuery.filters.exists(filter => filter.values.isEmpty || filter.values.exists(_.trim.isEmpty)) then
      Left(WorkflowError.InvalidValue("BIDS query", "filter", "entity filters must contain non-empty values"))
    else Right(new DatasetRecipe(datasetId, project, boldQuery, runGrouping, maskPolicy, confounds))

  def unsafe(
      datasetId: DatasetId,
      project: WorkflowArtifactRef[BidsProjectResource],
      boldQuery: BidsQuery,
      runGrouping: RunGrouping = RunGrouping.BySubjectSessionTask,
      maskPolicy: MaskPolicy = MaskPolicy.IntersectRunMasks,
      confounds: Option[ConfoundSelectionConfig] = None
  ): DatasetRecipe =
    make(datasetId, project, boldQuery, runGrouping, maskPolicy, confounds)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

final case class ModelRecipe private (
    formula: ModelFormula,
    baseline: BaselineSpec,
    fit: FitStrategy
):
  require(formula.onset.trim.nonEmpty, "model onset column must be non-empty")
  require(baseline.name.trim.nonEmpty, "baseline name must be non-empty")

object ModelRecipe:
  def make(
      formula: ModelFormula,
      baseline: BaselineSpec = BaselineSpec(),
      fit: FitStrategy = FitStrategy.Default
  ): Either[WorkflowError, ModelRecipe] =
    if formula.onset.trim.isEmpty then
      Left(WorkflowError.InvalidValue("model onset column", formula.onset, "must be non-empty"))
    else if baseline.name.trim.isEmpty then
      Left(WorkflowError.InvalidValue("baseline name", baseline.name, "must be non-empty"))
    else Right(new ModelRecipe(formula, baseline, fit))

  def unsafe(
      formula: ModelFormula,
      baseline: BaselineSpec = BaselineSpec(),
      fit: FitStrategy = FitStrategy.Default
  ): ModelRecipe =
    make(formula, baseline, fit).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class CoefficientWeight private (
    coefficient: CoefficientName,
    value: Double
):
  require(value.isFinite, "coefficient weight must be finite")

object CoefficientWeight:
  def make(coefficient: String, value: Double): Either[WorkflowError, CoefficientWeight] =
    if !value.isFinite then Left(WorkflowError.InvalidValue("coefficient weight", value.toString, "must be finite"))
    else CoefficientName(coefficient).map(new CoefficientWeight(_, value))

final case class ContrastRow private (weights: Vector[CoefficientWeight]):
  require(weights.nonEmpty, "contrast row must contain weights")
  require(weights.map(_.coefficient).distinct.length == weights.length, "contrast row coefficient names must be unique")
  require(weights.exists(_.value != 0.0), "contrast row must contain a non-zero weight")

  def asMap: Map[String, Double] =
    weights.iterator.map(weight => weight.coefficient.value -> weight.value).toMap

object ContrastRow:
  def make(weights: Vector[(String, Double)]): Either[WorkflowError, ContrastRow] =
    for
      typed <- WorkflowValidation.traverse(weights) { case (coefficient, value) =>
        CoefficientWeight.make(coefficient, value)
      }
      row <- validate(typed)
    yield row

  private def validate(weights: Vector[CoefficientWeight]): Either[WorkflowError, ContrastRow] =
    if weights.isEmpty then Left(WorkflowError.InvalidContrast("row", "must contain at least one coefficient weight"))
    else
      val duplicates = WorkflowValidation.duplicates(weights.map(_.coefficient.value)).sorted
      if duplicates.nonEmpty then
        Left(WorkflowError.InvalidContrast("row", s"duplicate coefficient weights: ${duplicates.mkString(", ")}"))
      else if weights.forall(_.value == 0.0) then
        Left(WorkflowError.InvalidContrast("row", "must contain at least one non-zero coefficient weight"))
      else Right(new ContrastRow(weights.sortBy(_.coefficient.value)))

enum ExecutableContrast:
  case T(value: TContrast)
  case F(value: FContrast)

enum ContrastWorkflow:
  case T(contrastId: WorkflowContrastId, row: ContrastRow)
  case F(contrastId: WorkflowContrastId, rows: Vector[ContrastRow])

  def id: WorkflowContrastId =
    this match
      case T(id, _) => id
      case F(id, _) => id

  def providesSamplingVariance: Boolean =
    this match
      case T(_, _) => true
      case F(_, _) => false

  def executable: ExecutableContrast =
    this match
      case T(id, row) => ExecutableContrast.T(TContrast(id.value, row.asMap))
      case F(id, rows) => ExecutableContrast.F(FContrast(id.value, rows.map(_.asMap)))

object ContrastWorkflow:
  def t(
      id: String,
      weights: Vector[(String, Double)]
  ): Either[WorkflowError, ContrastWorkflow] =
    for
      contrastId <- WorkflowContrastId(id)
      row <- ContrastRow.make(weights).left.map {
        case WorkflowError.InvalidContrast(_, reason) => WorkflowError.InvalidContrast(contrastId.value, reason)
        case other => other
      }
    yield ContrastWorkflow.T(contrastId, row)

  def f(
      id: String,
      rows: Vector[Vector[(String, Double)]]
  ): Either[WorkflowError, ContrastWorkflow] =
    for
      contrastId <- WorkflowContrastId(id)
      _ <-
        if rows.nonEmpty then Right(())
        else Left(WorkflowError.InvalidContrast(contrastId.value, "must contain at least one row"))
      typed <- WorkflowValidation.traverse(rows)(ContrastRow.make).left.map {
        case WorkflowError.InvalidContrast(_, reason) => WorkflowError.InvalidContrast(contrastId.value, reason)
        case other => other
      }
    yield ContrastWorkflow.F(contrastId, typed)

final case class FirstLevelWorkflow private (
    model: ModelRecipe,
    contrasts: Vector[ContrastWorkflow]
):
  require(contrasts.nonEmpty, "first-level workflow must contain contrasts")
  require(contrasts.map(_.id).distinct.length == contrasts.length, "first-level contrast ids must be unique")

  def contrast(id: WorkflowContrastId): Option[ContrastWorkflow] =
    contrasts.find(_.id == id)

object FirstLevelWorkflow:
  def make(
      model: ModelRecipe,
      contrasts: Vector[ContrastWorkflow]
  ): Either[WorkflowError, FirstLevelWorkflow] =
    if contrasts.isEmpty then Left(WorkflowError.InvalidContrast("first-level", "must contain at least one contrast"))
    else
      val duplicates = WorkflowValidation.duplicates(contrasts.map(_.id.value)).sorted
      if duplicates.nonEmpty then Left(WorkflowError.DuplicateValues("first-level contrast ids", duplicates))
      else Right(new FirstLevelWorkflow(model, contrasts))

final case class GroupDesignRecipe private (
    covariates: Vector[CovariateName],
    intercept: InterceptPolicy
):
  require(covariates.distinct.length == covariates.length, "group covariates must be unique")
  require(covariates.nonEmpty || intercept.include, "group design must contain an intercept or covariate")

  def termNames: Vector[DesignTermName] =
    val interceptTerm =
      if intercept.include then Vector(DesignTermName.Intercept)
      else Vector.empty
    interceptTerm ++ covariates.map(name => DesignTermName.unsafe(name.value))

  def terms: Int =
    termNames.length

object GroupDesignRecipe:
  val InterceptOnly: GroupDesignRecipe =
    new GroupDesignRecipe(Vector.empty, InterceptPolicy.Include)

  def make(
      covariates: Vector[String],
      intercept: InterceptPolicy = InterceptPolicy.Include
  ): Either[WorkflowError, GroupDesignRecipe] =
    val typed = covariates.foldLeft[Either[WorkflowError, Vector[CovariateName]]](Right(Vector.empty)) {
      case (acc, value) =>
        for
          collected <- acc
          name <- CovariateName(value).left.map(error => WorkflowError.InvalidValue("group covariate", value, error.message))
        yield collected :+ name
    }
    typed.flatMap { names =>
      val duplicates = WorkflowValidation.duplicates(names.map(_.value)).sorted
      if duplicates.nonEmpty then Left(WorkflowError.DuplicateValues("group covariates", duplicates))
      else if names.isEmpty && !intercept.include then
        Left(WorkflowError.InvalidValue("group design", "empty", "must contain an intercept or at least one covariate"))
      else Right(new GroupDesignRecipe(names, intercept))
    }

final case class GroupTermWeight private[workflow] (
    term: DesignTermName,
    value: Double
):
  require(value.isFinite, "group contrast weight must be finite")

final case class GroupContrastWorkflow private (
    name: GroupContrastName,
    weights: Vector[GroupTermWeight]
):
  require(weights.nonEmpty, "group contrast must contain weights")
  require(weights.map(_.term).distinct.length == weights.length, "group contrast term names must be unique")
  require(weights.exists(_.value != 0.0), "group contrast must contain a non-zero weight")

object GroupContrastWorkflow:
  def make(
      name: String,
      weights: Vector[(String, Double)]
  ): Either[WorkflowError, GroupContrastWorkflow] =
    for
      contrastName <- GroupContrastName(name).left.map(error => WorkflowError.InvalidContrast(name, error.message))
      typed <- weights.foldLeft[Either[WorkflowError, Vector[GroupTermWeight]]](Right(Vector.empty)) {
        case (acc, (term, value)) =>
          for
            collected <- acc
            _ <-
              if value.isFinite then Right(())
              else Left(WorkflowError.InvalidContrast(name, s"weight for '$term' must be finite"))
            termName <- DesignTermName(term).left.map(error => WorkflowError.InvalidContrast(name, error.message))
          yield collected :+ new GroupTermWeight(termName, value)
      }
      out <- validate(contrastName, typed)
    yield out

  private def validate(
      name: GroupContrastName,
      weights: Vector[GroupTermWeight]
  ): Either[WorkflowError, GroupContrastWorkflow] =
    if weights.isEmpty then Left(WorkflowError.InvalidContrast(name.value, "must contain at least one weight"))
    else
      val duplicates = WorkflowValidation.duplicates(weights.map(_.term.value)).sorted
      if duplicates.nonEmpty then Left(WorkflowError.InvalidContrast(name.value, s"duplicate group terms: ${duplicates.mkString(", ")}"))
      else if weights.forall(_.value == 0.0) then Left(WorkflowError.InvalidContrast(name.value, "must contain a non-zero weight"))
      else Right(new GroupContrastWorkflow(name, weights.sortBy(_.term.value)))

opaque type MinimumSubjects = Int

object MinimumSubjects:
  val Two: MinimumSubjects = 2

  def apply(value: Int): Either[WorkflowError, MinimumSubjects] =
    if value >= 2 then Right(value)
    else Left(WorkflowError.InvalidValue("minimum subjects", value.toString, "must be at least 2"))

  def unsafe(value: Int): MinimumSubjects =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (minimum: MinimumSubjects)
    inline def value: Int = minimum

enum GroupUnitPolicy:
  case OneUnitPerSubject
  case IndependentUnits

final case class GroupWorkflow private (
    id: GroupWorkflowId,
    inputs: Vector[WorkflowContrastId],
    design: GroupDesignRecipe,
    weighting: GroupWeighting,
    contrasts: Vector[GroupContrastWorkflow],
    minimumSubjects: MinimumSubjects,
    unitPolicy: GroupUnitPolicy
):
  require(inputs.nonEmpty, "group workflow must consume first-level contrasts")
  require(inputs.distinct.length == inputs.length, "group workflow input contrasts must be unique")
  require(contrasts.nonEmpty, "group workflow must contain group contrasts")
  require(contrasts.map(_.name).distinct.length == contrasts.length, "group contrast names must be unique")
  require(
    contrasts.flatMap(_.weights.map(_.term)).forall(design.termNames.contains),
    "group contrast terms must belong to the group design"
  )

object GroupWorkflow:
  def make(
      id: GroupWorkflowId,
      inputs: Vector[WorkflowContrastId],
      design: GroupDesignRecipe = GroupDesignRecipe.InterceptOnly,
      weighting: GroupWeighting = GroupWeighting.Unweighted,
      contrasts: Vector[GroupContrastWorkflow],
      minimumSubjects: MinimumSubjects = MinimumSubjects.Two,
      unitPolicy: GroupUnitPolicy = GroupUnitPolicy.OneUnitPerSubject
  ): Either[WorkflowError, GroupWorkflow] =
    if inputs.isEmpty then Left(WorkflowError.InvalidGroupWorkflow(id.value, "must consume at least one first-level contrast"))
    else if contrasts.isEmpty then Left(WorkflowError.InvalidGroupWorkflow(id.value, "must contain at least one group contrast"))
    else
      val duplicateInputs = WorkflowValidation.duplicates(inputs.map(_.value)).sorted
      val duplicateContrasts = WorkflowValidation.duplicates(contrasts.map(_.name.value)).sorted
      val knownTerms = design.termNames.map(_.value).toSet
      val unknownTerms = contrasts.flatMap(_.weights.map(_.term.value)).filterNot(knownTerms).distinct.sorted
      if duplicateInputs.nonEmpty then
        Left(WorkflowError.InvalidGroupWorkflow(id.value, s"duplicate first-level inputs: ${duplicateInputs.mkString(", ")}"))
      else if duplicateContrasts.nonEmpty then
        Left(WorkflowError.InvalidGroupWorkflow(id.value, s"duplicate group contrasts: ${duplicateContrasts.mkString(", ")}"))
      else if unknownTerms.nonEmpty then
        Left(WorkflowError.InvalidGroupWorkflow(id.value, s"unknown group design terms: ${unknownTerms.mkString(", ")}"))
      else Right(new GroupWorkflow(id, inputs, design, weighting, contrasts, minimumSubjects, unitPolicy))

final case class StudyAnalysisSpec private (
    id: WorkflowId,
    dataset: DatasetRecipe,
    firstLevel: FirstLevelWorkflow,
    groups: Vector[GroupWorkflow],
    output: ResultOutputPolicy
):
  require(groups.map(_.id).distinct.length == groups.length, "group workflow ids must be unique")
  require(groups == groups.sortBy(_.id.value), "group workflows must have canonical id order")

object StudyAnalysisSpec:
  def make(
      id: WorkflowId,
      dataset: DatasetRecipe,
      firstLevel: FirstLevelWorkflow,
      groups: Vector[GroupWorkflow] = Vector.empty,
      output: ResultOutputPolicy
  ): Either[WorkflowError, StudyAnalysisSpec] =
    val duplicateGroups = WorkflowValidation.duplicates(groups.map(_.id.value)).sorted
    if duplicateGroups.nonEmpty then Left(WorkflowError.DuplicateValues("group workflow ids", duplicateGroups))
    else Right(new StudyAnalysisSpec(id, dataset, firstLevel, groups.sortBy(_.id.value), output))
