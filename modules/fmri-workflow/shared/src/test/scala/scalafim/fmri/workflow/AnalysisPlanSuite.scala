package scalafim.fmri.workflow

import scalafim.image.SampleSpaces

import munit.FunSuite
import bids4s.{BidsQuery, BidsScope}
import scalafim.dataset.{DatasetId, DatasetShape, RunId, SessionId, SpaceId, SubjectId, TaskId}
import scalafim.fmri.design.ColumnId
import scalafim.fmri.design.formula.ModelFormula
import scalafim.fmri.fit.SequentialChunkProgramInterpreter
import scalafim.fmri.group.{GroupWeighting, InterceptPolicy}
import scalafim.image.SomeSampleSpace

class AnalysisPlanSuite extends FunSuite:
  test("compile produces deterministic subject and group jobs from pure descriptors") {
    val datasetId = DatasetId("demo")
    val contrast = ContrastWorkflow.t("stim", Vector("stim" -> 1.0)).toOption.get
    val firstLevel = FirstLevelWorkflow.make(
      ModelRecipe.unsafe(ModelFormula(ColumnId.unsafe("onset"), Vector.empty)),
      Vector(contrast)
    ).toOption.get
    val group = interceptGroup("stim", GroupWeighting.Unweighted)
    val spec = analysisSpec(datasetId, firstLevel, Vector(group))
    val catalog = StudyCatalog.make(
      datasetId,
      Vector(
        unit("unit-02", "02", "run-1"),
        unit("unit-01", "01", "run-1")
      )
    ).toOption.get

    val plan = AnalysisPlan.compile(spec, catalog).toOption.get

    assertEquals(plan.subjectJobs.map(_.unit.subject.value), Vector("01", "02"))
    assertEquals(plan.subjectJobs.map(_.ordinal.value), Vector(0, 1))
    assertEquals(
      plan.subjectJobs.map(_.resultBundle.artifact.location.value),
      Vector("file:///analysis/first-level/unit-01", "file:///analysis/first-level/unit-02")
    )
    assertEquals(plan.groupJobs.length, 1)
    assertEquals(plan.groupJobs.head.inputs.head.unitResults.map(_._1.value), Vector("unit-01", "unit-02"))
    assertEquals(plan.groupJobs.head.resultBundle.artifact.location.value, "file:///analysis/group/group-main")
    assert(plan.preflightReport.isClean)

    val interpreted = SequentialChunkProgramInterpreter.execute(plan.jobProgram) { job =>
      Right(job.id.value)
    }
    assertEquals(
      interpreted.toOption.get.map(_.result),
      Vector("study.unit-01", "study.unit-02")
    )
  }

  test("preflight accumulates independent structural errors without reading payloads") {
    val expectedDataset = DatasetId("expected")
    val omnibus = ContrastWorkflow.f(
      "omnibus",
      Vector(Vector("stim" -> 1.0), Vector("stim" -> -1.0))
    ).toOption.get
    val firstLevel = FirstLevelWorkflow.make(
      ModelRecipe.unsafe(ModelFormula(ColumnId.unsafe("onset"), Vector.empty)),
      Vector(omnibus)
    ).toOption.get
    val design = GroupDesignRecipe.make(Vector("age"), InterceptPolicy.Include).toOption.get
    val groupContrast = GroupContrastWorkflow.make("mean", Vector("(Intercept)" -> 1.0)).toOption.get
    val group = GroupWorkflow.make(
      id = GroupWorkflowId.unsafe("group-main"),
      inputs = Vector(omnibus.id, WorkflowContrastId.unsafe("missing")),
      design = design,
      weighting = GroupWeighting.InverseVariance,
      contrasts = Vector(groupContrast)
    ).toOption.get
    val spec = analysisSpec(expectedDataset, firstLevel, Vector(group))
    val catalog = StudyCatalog.make(DatasetId("actual"), Vector(unit("unit-01", "01", "run-1"))).toOption.get

    val report = AnalysisPlan.preflight(spec, catalog)
    val codes = report.errors.map(_.code).toSet

    assertEquals(
      codes,
      Set(
        PreflightCode.DatasetMismatch,
        PreflightCode.MissingFirstLevelContrast,
        PreflightCode.MissingSamplingVariance,
        PreflightCode.InsufficientSubjects,
        PreflightCode.InsufficientResidualDegreesOfFreedom
      )
    )
    assertEquals(report.checkedUnits, 1)
    assert(!report.canRun)
    assert(AnalysisPlan.compile(spec, catalog).isLeft)
  }

  test("one-unit-per-subject policy rejects repeated session units") {
    val datasetId = DatasetId("demo")
    val contrast = ContrastWorkflow.t("stim", Vector("stim" -> 1.0)).toOption.get
    val firstLevel = FirstLevelWorkflow.make(
      ModelRecipe.unsafe(ModelFormula(ColumnId.unsafe("onset"), Vector.empty)),
      Vector(contrast)
    ).toOption.get
    val spec = analysisSpec(datasetId, firstLevel, Vector(interceptGroup("stim", GroupWeighting.Unweighted)))
    val catalog = StudyCatalog.make(
      datasetId,
      Vector(
        unit("unit-ses-1", "01", "run-1", session = Some("1")),
        unit("unit-ses-2", "01", "run-2", session = Some("2"))
      )
    ).toOption.get

    val report = AnalysisPlan.preflight(spec, catalog)

    assert(report.errors.exists(_.code == PreflightCode.RepeatedSubjectUnits))
  }

  test("first-level-only plans compile with an explicit warning") {
    val datasetId = DatasetId("demo")
    val contrast = ContrastWorkflow.t("stim", Vector("stim" -> 1.0)).toOption.get
    val firstLevel = FirstLevelWorkflow.make(
      ModelRecipe.unsafe(ModelFormula(ColumnId.unsafe("onset"), Vector.empty)),
      Vector(contrast)
    ).toOption.get
    val spec = analysisSpec(datasetId, firstLevel, Vector.empty)
    val catalog = StudyCatalog.make(datasetId, Vector(unit("unit-01", "01", "run-1"))).toOption.get

    val plan = AnalysisPlan.compile(spec, catalog).toOption.get

    assert(plan.preflightReport.canRun)
    assert(plan.preflightReport.warnings.exists(_.code == PreflightCode.NoGroupWorkflow))
    assertEquals(plan.groupJobs, Vector.empty)
  }

  test("empty catalogs fail preflight before job construction") {
    val datasetId = DatasetId("demo")
    val contrast = ContrastWorkflow.t("stim", Vector("stim" -> 1.0)).toOption.get
    val firstLevel = FirstLevelWorkflow.make(
      ModelRecipe.unsafe(ModelFormula(ColumnId.unsafe("onset"), Vector.empty)),
      Vector(contrast)
    ).toOption.get
    val spec = analysisSpec(datasetId, firstLevel, Vector.empty)
    val catalog = StudyCatalog.empty(datasetId)

    val report = AnalysisPlan.preflight(spec, catalog)

    assertEquals(report.checkedUnits, 0)
    assert(report.errors.exists(_.code == PreflightCode.EmptyCatalog))
    assert(AnalysisPlan.compile(spec, catalog).isLeft)
  }

  private def analysisSpec(
      datasetId: DatasetId,
      firstLevel: FirstLevelWorkflow,
      groups: Vector[GroupWorkflow]
  ): StudyAnalysisSpec =
    val recipe = DatasetRecipe.unsafe(
      datasetId = datasetId,
      project = WorkflowArtifactRef.unsafe[BidsProjectResource]("file:///bids"),
      boldQuery = BidsQuery.from(
        filename = Vector(".*bold\\.nii(\\.gz)?$"),
        scope = BidsScope.Derivatives
      ).toOption.get
    )
    StudyAnalysisSpec.make(
      id = WorkflowId.unsafe("study"),
      dataset = recipe,
      firstLevel = firstLevel,
      groups = groups,
      output = ResultOutputPolicy(
        ArtifactLocation.unsafe("file:///analysis"),
        layout = ResultMapLayout.IndividualNamedMaps
      )
    ).toOption.get

  private def interceptGroup(
      input: String,
      weighting: GroupWeighting
  ): GroupWorkflow =
    val contrast = GroupContrastWorkflow.make("mean", Vector("(Intercept)" -> 1.0)).toOption.get
    GroupWorkflow.make(
      id = GroupWorkflowId.unsafe("group-main"),
      inputs = Vector(WorkflowContrastId.unsafe(input)),
      weighting = weighting,
      contrasts = Vector(contrast)
    ).toOption.get

  private def unit(
      id: String,
      subject: String,
      run: String,
      session: Option[String] = None
  ): FirstLevelUnit =
    val timepoints = 4
    val runInput = RunInput.unsafe(
      id = RunId(run),
      repetitionTime = RepetitionTime.unsafe(2.0),
      timepoints = timepoints,
      bold = WorkflowArtifactRef.unsafe[BoldImageResource](s"file:///$id/$run/bold.nii.gz"),
      events = WorkflowArtifactRef.unsafe[EventsTableResource](s"file:///$id/$run/events.tsv"),
      confounds = Some(WorkflowArtifactRef.unsafe[ConfoundsTableResource](s"file:///$id/$run/confounds.tsv"))
    )
    FirstLevelUnit.unsafe(
      id = FirstLevelUnitId.unsafe(id),
      subject = SubjectId(subject),
      session = session.map(SessionId(_)),
      task = TaskId("demo"),
      space = SpaceId("MNI152NLin2009cAsym"),
      shape = DatasetShape.unsafe(SampleSpaces(Vector(2, 2, 1)), timepoints),
      runs = Vector(runInput),
      mask = UnitMask.Single(WorkflowArtifactRef.unsafe[MaskImageResource](s"file:///$id/mask.nii.gz"))
    )
