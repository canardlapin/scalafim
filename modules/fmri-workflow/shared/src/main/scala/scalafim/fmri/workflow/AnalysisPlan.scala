package scalafim.fmri.workflow

import scalafim.fmri.fit.{ChunkOrdinal, ChunkProgram, ChunkWork}

enum PreflightSeverity:
  case Warning
  case Error

enum PreflightCode:
  case EmptyCatalog
  case DatasetMismatch
  case MissingFirstLevelContrast
  case MissingSamplingVariance
  case RepeatedSubjectUnits
  case InsufficientSubjects
  case InsufficientResidualDegreesOfFreedom
  case NoGroupWorkflow

enum PreflightScope:
  case Study
  case Unit(id: FirstLevelUnitId)
  case Group(id: GroupWorkflowId)

final case class PreflightIssue(
    severity: PreflightSeverity,
    code: PreflightCode,
    scope: PreflightScope,
    message: String
):
  require(message.trim.nonEmpty, "preflight issue message must be non-empty")

final case class PreflightReport private (
    workflowId: WorkflowId,
    checkedUnits: Int,
    issues: Vector[PreflightIssue]
):
  require(checkedUnits >= 0, "checked unit count must be non-negative")

  def errors: Vector[PreflightIssue] =
    issues.filter(_.severity == PreflightSeverity.Error)

  def warnings: Vector[PreflightIssue] =
    issues.filter(_.severity == PreflightSeverity.Warning)

  def canRun: Boolean =
    errors.isEmpty

  def isClean: Boolean =
    issues.isEmpty

object PreflightReport:
  private[workflow] def make(
      workflowId: WorkflowId,
      checkedUnits: Int,
      issues: Vector[PreflightIssue]
  ): PreflightReport =
    new PreflightReport(workflowId, checkedUnits, issues)

final case class SubjectJob private[workflow] (
    id: SubjectJobId,
    ordinal: ChunkOrdinal,
    unit: FirstLevelUnit,
    model: ModelRecipe,
    contrasts: Vector[ContrastWorkflow],
    resultBundle: ResultBundleRef
) extends ChunkWork

final case class GroupInputRef(
    contrast: WorkflowContrastId,
    unitResults: Vector[(FirstLevelUnitId, ResultBundleRef)]
):
  require(unitResults.nonEmpty, "group input must contain first-level result bundles")

final case class GroupJob private[workflow] (
    workflow: GroupWorkflow,
    inputs: Vector[GroupInputRef],
    resultBundle: ResultBundleRef
)

final class AnalysisPlan private (
    val spec: StudyAnalysisSpec,
    val catalog: StudyCatalog,
    val subjectJobs: Vector[SubjectJob],
    val groupJobs: Vector[GroupJob],
    val preflightReport: PreflightReport,
    val jobProgram: ChunkProgram[SubjectJob]
):
  require(subjectJobs.nonEmpty, "analysis plan must contain subject jobs")
  require(preflightReport.canRun, "analysis plan preflight must permit execution")

  def id: WorkflowId =
    spec.id

  def resultBundles: Vector[ResultBundleRef] =
    subjectJobs.map(_.resultBundle) ++ groupJobs.map(_.resultBundle)

object AnalysisPlan:
  def preflight(
      spec: StudyAnalysisSpec,
      catalog: StudyCatalog
  ): PreflightReport =
    val issues = Vector.newBuilder[PreflightIssue]

    if catalog.isEmpty then
      issues += error(
        PreflightCode.EmptyCatalog,
        PreflightScope.Study,
        "the study catalog contains no first-level units"
      )

    if catalog.datasetId != spec.dataset.datasetId then
      issues += error(
        PreflightCode.DatasetMismatch,
        PreflightScope.Study,
        s"catalog dataset '${catalog.datasetId.value}' does not match recipe dataset '${spec.dataset.datasetId.value}'"
      )

    if spec.groups.isEmpty then
      issues += warning(
        PreflightCode.NoGroupWorkflow,
        PreflightScope.Study,
        "the plan has no group workflow and will stop after first-level outputs"
      )

    val firstLevelContrasts = spec.firstLevel.contrasts.map(contrast => contrast.id -> contrast).toMap
    spec.groups.foreach { group =>
      group.inputs.foreach { contrastId =>
        firstLevelContrasts.get(contrastId) match
          case None =>
            issues += error(
              PreflightCode.MissingFirstLevelContrast,
              PreflightScope.Group(group.id),
              s"group input '${contrastId.value}' is not produced by the first-level workflow"
            )
          case Some(contrast) if group.weighting.requiresVariance && !contrast.providesSamplingVariance =>
            issues += error(
              PreflightCode.MissingSamplingVariance,
              PreflightScope.Group(group.id),
              s"group weighting '${group.weighting.label}' requires sampling variances, but '${contrastId.value}' does not provide them"
            )
          case Some(_) =>
            ()
      }

      val repeatedSubjects = WorkflowValidation.duplicates(catalog.units.map(_.subject.value)).sorted
      if group.unitPolicy == GroupUnitPolicy.OneUnitPerSubject && repeatedSubjects.nonEmpty then
        issues += error(
          PreflightCode.RepeatedSubjectUnits,
          PreflightScope.Group(group.id),
          s"one-unit-per-subject group input has repeated subjects: ${repeatedSubjects.mkString(", ")}"
        )

      val observations =
        group.unitPolicy match
          case GroupUnitPolicy.OneUnitPerSubject => catalog.subjects.length
          case GroupUnitPolicy.IndependentUnits => catalog.units.length

      if observations < group.minimumSubjects.value then
        issues += error(
          PreflightCode.InsufficientSubjects,
          PreflightScope.Group(group.id),
          s"group workflow requires at least ${group.minimumSubjects.value} observations, found $observations"
        )

      if observations <= group.design.terms then
        issues += error(
          PreflightCode.InsufficientResidualDegreesOfFreedom,
          PreflightScope.Group(group.id),
          s"group design has ${group.design.terms} terms for $observations observations"
        )
    }

    PreflightReport.make(spec.id, catalog.units.length, issues.result())

  def compile(
      spec: StudyAnalysisSpec,
      catalog: StudyCatalog
  ): Either[PreflightReport, AnalysisPlan] =
    val report = preflight(spec, catalog)
    if !report.canRun then Left(report)
    else
      val jobs = catalog.units.zipWithIndex.map { case (unit, index) =>
        SubjectJob(
          id = SubjectJobId.from(spec.id, unit.id),
          ordinal = ChunkOrdinal.unsafe(index),
          unit = unit,
          model = spec.firstLevel.model,
          contrasts = spec.firstLevel.contrasts,
          resultBundle = spec.output.firstLevel(spec.id, unit.id)
        )
      }
      val groupJobs = spec.groups.map { workflow =>
        val inputs = workflow.inputs.map { contrast =>
          GroupInputRef(
            contrast = contrast,
            unitResults = jobs.map(job => job.unit.id -> job.resultBundle)
          )
        }
        GroupJob(
          workflow = workflow,
          inputs = inputs,
          resultBundle = spec.output.group(spec.id, workflow.id)
        )
      }
      val program = ChunkProgram.unsafe(jobs)
      Right(new AnalysisPlan(spec, catalog, jobs, groupJobs, report, program))

  private def error(
      code: PreflightCode,
      scope: PreflightScope,
      message: String
  ): PreflightIssue =
    PreflightIssue(PreflightSeverity.Error, code, scope, message)

  private def warning(
      code: PreflightCode,
      scope: PreflightScope,
      message: String
  ): PreflightIssue =
    PreflightIssue(PreflightSeverity.Warning, code, scope, message)

type StudyAnalysisPlan = AnalysisPlan
