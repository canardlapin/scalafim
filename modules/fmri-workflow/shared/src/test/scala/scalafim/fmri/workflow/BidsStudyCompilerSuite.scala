package scalafim.fmri.workflow

import munit.FunSuite
import scalafim.bids.*
import scalafim.dataset.{DatasetId, DatasetShape}
import scalafim.image.NeuroSpace

class BidsStudyCompilerSuite extends FunSuite:
  test("compiler groups exact fMRIPrep entities into deterministic multi-run units") {
    val fixture = studyFixture(subjects = Vector("02", "01"), runs = Vector("02", "01"))
    val catalog = BidsStudyCompiler.compile(fixture.project, fixture.recipe, fixture.headers).toOption.get

    assertEquals(catalog.units.map(_.subject.value), Vector("01", "02"))
    assertEquals(catalog.units.map(_.runs.map(_.id.value)), Vector(Vector("01", "02"), Vector("01", "02")))
    assertEquals(catalog.units.map(_.shape.timepoints), Vector(6, 6))
    assertEquals(catalog.units.map(_.acquisition), Vector(Some("mb"), Some("mb")))
    assertEquals(catalog.units.map(_.echo), Vector(Some("1"), Some("1")))
    assertEquals(catalog.units.map(_.resolution), Vector(Some("2"), Some("2")))
    assertEquals(catalog.units.map(_.pipeline.map(_.value)), Vector(Some("fmriprep"), Some("fmriprep")))
    assert(catalog.units.forall(_.mask.isInstanceOf[UnitMask.Intersection]))
    assertEquals(
      catalog.units.head.runs.head.bold.location.value,
      "file:///study/derivatives/fmriprep/sub-01/func/sub-01_task-demo_acq-mb_run-01_echo-1_space-MNI_res-2_desc-preproc_bold.nii"
    )
  }

  test("compiler accumulates missing header, metadata, and companion failures") {
    val derivative = DerivativeRoot(BidsPath("derivatives/fmriprep"), PipelineName("fmriprep"))
    val bold = "derivatives/fmriprep/sub-01/func/sub-01_task-demo_run-01_space-MNI_desc-preproc_bold.nii"
    val project = BidsProject(
      root = BidsPath("/study"),
      description = None,
      participants = Vector("01"),
      derivatives = Vector(derivative),
      manifest = BidsManifest.fromRelativePaths(Vector(bold), Vector(derivative))
    )
    val recipe = datasetRecipe()

    val report = BidsStudyCompiler.compile(project, recipe, ImageHeaderCatalog(Map.empty)).left.toOption.get
    val codes = report.issues.map(_.code).toSet

    assertEquals(
      codes,
      Set(
        CatalogIssueCode.MissingHeader,
        CatalogIssueCode.MissingRepetitionTime,
        CatalogIssueCode.MissingEvents,
        CatalogIssueCode.MissingConfounds,
        CatalogIssueCode.MissingMask
      )
    )
    assertEquals(report.matchedBoldFiles, 1)
  }

  test("one-unit-per-run grouping preserves the run in unit identity") {
    val fixture = studyFixture(subjects = Vector("01"), runs = Vector("02", "01"), grouping = RunGrouping.OneUnitPerRun)
    val catalog = BidsStudyCompiler.compile(fixture.project, fixture.recipe, fixture.headers).toOption.get

    assertEquals(catalog.units.length, 2)
    assertEquals(catalog.units.map(_.runs.length), Vector(1, 1))
    assertEquals(
      catalog.units.map(_.id.value),
      Vector(
        "sub-01.task-demo.acq-mb.echo-1.space-MNI.res-2.pipeline-fmriprep.run-01",
        "sub-01.task-demo.acq-mb.echo-1.space-MNI.res-2.pipeline-fmriprep.run-02"
      )
    )
  }

  test("confound-free recipes ignore ambiguous confound tables") {
    val ignored = ambiguousConfoundFixture(confoundsRequested = false)
    val catalog = BidsStudyCompiler.compile(ignored.project, ignored.recipe, ignored.headers).toOption.get

    assertEquals(catalog.units.head.runs.head.confounds, None)

    val requested = ambiguousConfoundFixture(confoundsRequested = true)
    val report = BidsStudyCompiler.compile(requested.project, requested.recipe, requested.headers).left.toOption.get
    assert(report.issues.exists(_.code == CatalogIssueCode.AmbiguousCompanion))
  }

  test("mask policies distinguish common, intersected, and explicit artifacts") {
    val common = studyFixture(
      subjects = Vector("01"),
      runs = Vector("01", "02"),
      maskPolicy = MaskPolicy.RequireCommonDerivative,
      commonMask = true
    )
    val commonCatalog = BidsStudyCompiler.compile(common.project, common.recipe, common.headers).toOption.get
    commonCatalog.units.head.mask match
      case UnitMask.Single(mask) =>
        assert(!mask.location.value.contains("run-"))
      case other => fail(s"expected one common mask, got $other")

    val runSpecific = studyFixture(
      subjects = Vector("01"),
      runs = Vector("01", "02"),
      maskPolicy = MaskPolicy.RequireCommonDerivative
    )
    val report = BidsStudyCompiler.compile(runSpecific.project, runSpecific.recipe, runSpecific.headers).left.toOption.get
    assert(report.issues.exists(_.code == CatalogIssueCode.AmbiguousCompanion))

    val explicitMask = WorkflowArtifactRef.unsafe[MaskImageResource]("file:///declared-mask.nii")
    val explicit = studyFixture(
      subjects = Vector("01"),
      runs = Vector("01", "02"),
      maskPolicy = MaskPolicy.Explicit(explicitMask)
    )
    val explicitCatalog = BidsStudyCompiler.compile(explicit.project, explicit.recipe, explicit.headers).toOption.get
    assertEquals(explicitCatalog.units.head.mask, UnitMask.Single(explicitMask))
  }

  test("compiler reports incompatible geometry and duplicate run identities") {
    val fixture = studyFixture(subjects = Vector("01"), runs = Vector("01", "02"))
    val incompatibleHeaders = fixture.headers.headers.map { case (path, descriptor) =>
      if path.value.contains("run-02") && path.value.endsWith("_bold.nii") then
        path -> ImageHeaderDescriptor(DatasetShape.unsafe(NeuroSpace(Vector(4, 1, 1)), 3))
      else path -> descriptor
    }
    val incompatible = BidsStudyCompiler
      .compile(fixture.project, fixture.recipe, ImageHeaderCatalog(incompatibleHeaders))
      .left
      .toOption
      .get
    assert(incompatible.issues.exists(_.code == CatalogIssueCode.IncompatibleGeometry))

    val duplicate = duplicateRunFixture()
    val duplicateReport = BidsStudyCompiler.compile(duplicate.project, duplicate.recipe, duplicate.headers).left.toOption.get
    assert(duplicateReport.issues.exists(_.code == CatalogIssueCode.DuplicateRun))
  }

  test("catalog reports empty matches, missing entities, and invalid unit identities") {
    val empty = studyFixture(subjects = Vector("01"), runs = Vector("01"))
    val emptyReport = BidsStudyCompiler
      .compile(empty.project.copy(manifest = BidsManifest(Vector.empty)), empty.recipe, ImageHeaderCatalog(Map.empty))
      .left
      .toOption
      .get
    assertEquals(emptyReport.issues.map(_.code), Vector(CatalogIssueCode.NoBoldFiles))

    val valid = identityFixture(task = "demo")
    val filesWithoutTask = valid.project.manifest.files.map { file =>
      if file.fileName.endsWith("_bold.nii") then
        val parsed = file.parsed.map { name =>
          val entities = BidsEntities.from(name.entities.values.filterNot(_._1 == EntityKey.Task)).toOption.get
          name.copy(entities = entities)
        }
        file.copy(parsed = parsed)
      else file
    }
    val malformed = valid.copy(project = valid.project.copy(manifest = BidsManifest(filesWithoutTask)))
    val malformedReport = BidsStudyCompiler.compile(malformed.project, malformed.recipe, malformed.headers).left.toOption.get
    assert(malformedReport.issues.exists(_.code == CatalogIssueCode.MissingEntity))

    val filesWithInvalidTask = valid.project.manifest.files.map { file =>
      if file.parsed.exists(_.entities.contains(EntityKey.Task)) then
        val parsed = file.parsed.map { name =>
          name.copy(entities = name.entities.updated(EntityKey.Task, "demo+invalid").toOption.get)
        }
        file.copy(parsed = parsed)
      else file
    }
    val invalid = valid.copy(project = valid.project.copy(manifest = BidsManifest(filesWithInvalidTask)))
    val invalidReport = BidsStudyCompiler.compile(invalid.project, invalid.recipe, invalid.headers).left.toOption.get
    assert(invalidReport.issues.exists(_.code == CatalogIssueCode.InvalidIdentity))
  }

  test("participant-table coverage reports missing and duplicate subjects") {
    val fixture = studyFixture(subjects = Vector("01"), runs = Vector("01"))
    val missingTable = BidsTable
      .fromRows(Vector("participant_id"), Vector(Vector(Some("sub-02"))))
      .toOption
      .get
    val missing = BidsStudyCompiler
      .compile(fixture.project.copy(participantsTable = Some(missingTable)), fixture.recipe, fixture.headers)
      .left
      .toOption
      .get
    assert(missing.issues.exists(_.code == CatalogIssueCode.MissingParticipant))

    val duplicateTable = BidsTable
      .fromRows(
        Vector("participant_id"),
        Vector(Vector(Some("sub-01")), Vector(Some("sub-01")))
      )
      .toOption
      .get
    val duplicate = BidsStudyCompiler
      .compile(fixture.project.copy(participantsTable = Some(duplicateTable)), fixture.recipe, fixture.headers)
      .left
      .toOption
      .get
    assert(duplicate.issues.exists(_.code == CatalogIssueCode.DuplicateParticipant))
  }

  private final case class Fixture(
      project: BidsProject,
      recipe: DatasetRecipe,
      headers: ImageHeaderCatalog
  )

  private def studyFixture(
      subjects: Vector[String],
      runs: Vector[String],
      grouping: RunGrouping = RunGrouping.BySubjectSessionTask,
      maskPolicy: MaskPolicy = MaskPolicy.IntersectRunMasks,
      commonMask: Boolean = false
  ): Fixture =
    val derivative = DerivativeRoot(BidsPath("derivatives/fmriprep"), PipelineName("fmriprep"))
    val files = Vector.newBuilder[String]
    val sidecars = Map.newBuilder[BidsPath, JsonValue.Obj]
    val headers = Map.newBuilder[BidsPath, ImageHeaderDescriptor]
    val boldShape = DatasetShape.unsafe(NeuroSpace(Vector(2, 2, 1)), 3)
    val maskShape = DatasetShape.unsafe(NeuroSpace(Vector(2, 2, 1)), 1)

    subjects.foreach { subject =>
      if commonMask then
        val mask = s"derivatives/fmriprep/sub-$subject/func/sub-${subject}_task-demo_acq-mb_echo-1_space-MNI_res-2_desc-brain_mask.nii"
        files += mask
        headers += BidsPath(mask) -> ImageHeaderDescriptor(maskShape)
      runs.foreach { run =>
        val prefix = s"sub-${subject}_task-demo_acq-mb_run-${run}_echo-1"
        val derivativePrefix = s"${prefix}_space-MNI_res-2"
        val bold = s"derivatives/fmriprep/sub-$subject/func/${derivativePrefix}_desc-preproc_bold.nii"
        val mask = s"derivatives/fmriprep/sub-$subject/func/${derivativePrefix}_desc-brain_mask.nii"
        val confounds = s"derivatives/fmriprep/sub-$subject/func/${prefix}_desc-confounds_timeseries.tsv"
        val events = s"sub-$subject/func/${prefix}_events.tsv"
        files ++= Vector(bold, confounds, events)
        if !commonMask then files += mask
        sidecars += BidsPath(bold.stripSuffix(".nii") + ".json") -> metadata(2.0)
        files += bold.stripSuffix(".nii") + ".json"
        headers += BidsPath(bold) -> ImageHeaderDescriptor(boldShape)
        if !commonMask then headers += BidsPath(mask) -> ImageHeaderDescriptor(maskShape)
      }
    }

    val project = BidsProject(
      root = BidsPath("/study"),
      description = None,
      participants = subjects.sorted,
      derivatives = Vector(derivative),
      manifest = BidsManifest.fromRelativePaths(files.result(), Vector(derivative)),
      sidecars = sidecars.result()
    )
    Fixture(project, datasetRecipe(grouping, maskPolicy), ImageHeaderCatalog(headers.result()))

  private def datasetRecipe(
      grouping: RunGrouping = RunGrouping.BySubjectSessionTask,
      maskPolicy: MaskPolicy = MaskPolicy.IntersectRunMasks,
      confounds: Option[ConfoundSelectionConfig] = Some(ConfoundSelectionConfig(variables = Vector("motion6")))
  ): DatasetRecipe =
    DatasetRecipe.unsafe(
      datasetId = DatasetId("demo"),
      project = WorkflowArtifactRef.unsafe[BidsProjectResource]("file:///study"),
      boldQuery = BidsQuery(
        filename = Vector("desc-preproc_bold\\.nii$"),
        scope = BidsScope.Derivatives,
        pipeline = Some(PipelineName("fmriprep"))
      ),
      runGrouping = grouping,
      maskPolicy = maskPolicy,
      confounds = confounds
    )

  private def ambiguousConfoundFixture(confoundsRequested: Boolean): Fixture =
    val derivative = DerivativeRoot(BidsPath("derivatives/fmriprep"), PipelineName("fmriprep"))
    val prefix = "sub-01_task-demo_run-01"
    val bold = s"derivatives/fmriprep/sub-01/func/${prefix}_space-MNI_desc-preproc_bold.nii"
    val sidecar = BidsPath(bold.stripSuffix(".nii") + ".json")
    val events = s"sub-01/func/${prefix}_events.tsv"
    val confoundA = s"derivatives/fmriprep/sub-01/func/${prefix}_desc-a_confounds.tsv"
    val confoundB = s"derivatives/fmriprep/sub-01/func/${prefix}_desc-b_confounds.tsv"
    val project = BidsProject(
      root = BidsPath("/study"),
      description = None,
      participants = Vector("01"),
      derivatives = Vector(derivative),
      manifest = BidsManifest.fromRelativePaths(Vector(bold, sidecar.value, events, confoundA, confoundB), Vector(derivative)),
      sidecars = Map(sidecar -> metadata(2.0))
    )
    val recipe = datasetRecipe(
      maskPolicy = MaskPolicy.Explicit(WorkflowArtifactRef.unsafe[MaskImageResource]("file:///mask.nii")),
      confounds = if confoundsRequested then Some(ConfoundSelectionConfig(variables = Vector("motion6"))) else None
    )
    val shape = DatasetShape.unsafe(NeuroSpace(Vector(2, 2, 1)), 3)
    Fixture(project, recipe, ImageHeaderCatalog(Map(BidsPath(bold) -> ImageHeaderDescriptor(shape))))

  private def duplicateRunFixture(): Fixture =
    val derivative = DerivativeRoot(BidsPath("derivatives/fmriprep"), PipelineName("fmriprep"))
    val prefix = "sub-01_task-demo_run-01_space-MNI"
    val preprocessed = s"derivatives/fmriprep/sub-01/func/${prefix}_desc-preproc_bold.nii"
    val denoised = s"derivatives/fmriprep/sub-01/func/${prefix}_desc-denoised_bold.nii"
    val events = "sub-01/func/sub-01_task-demo_run-01_events.tsv"
    val sidecars = Vector(preprocessed, denoised).map(path => BidsPath(path.stripSuffix(".nii") + ".json"))
    val project = BidsProject(
      root = BidsPath("/study"),
      description = None,
      participants = Vector("01"),
      derivatives = Vector(derivative),
      manifest = BidsManifest.fromRelativePaths(Vector(preprocessed, denoised, events) ++ sidecars.map(_.value), Vector(derivative)),
      sidecars = sidecars.map(_ -> metadata(2.0)).toMap
    )
    val recipe = DatasetRecipe.unsafe(
      datasetId = DatasetId("demo"),
      project = WorkflowArtifactRef.unsafe[BidsProjectResource]("file:///study"),
      boldQuery = BidsQuery(
        filename = Vector("desc-(preproc|denoised)_bold\\.nii$"),
        scope = BidsScope.Derivatives,
        pipeline = Some(PipelineName("fmriprep"))
      ),
      maskPolicy = MaskPolicy.Explicit(WorkflowArtifactRef.unsafe[MaskImageResource]("file:///mask.nii")),
      confounds = None
    )
    val shape = DatasetShape.unsafe(NeuroSpace(Vector(2, 2, 1)), 3)
    val headers = ImageHeaderCatalog(Map(
      BidsPath(preprocessed) -> ImageHeaderDescriptor(shape),
      BidsPath(denoised) -> ImageHeaderDescriptor(shape)
    ))
    Fixture(project, recipe, headers)

  private def identityFixture(task: String): Fixture =
    val derivative = DerivativeRoot(BidsPath("derivatives/fmriprep"), PipelineName("fmriprep"))
    val prefix = s"sub-01_task-${task}_run-01"
    val bold = s"derivatives/fmriprep/sub-01/func/${prefix}_space-MNI_desc-preproc_bold.nii"
    val events = s"sub-01/func/${prefix}_events.tsv"
    val sidecar = BidsPath(bold.stripSuffix(".nii") + ".json")
    val project = BidsProject(
      root = BidsPath("/study"),
      description = None,
      participants = Vector("01"),
      derivatives = Vector(derivative),
      manifest = BidsManifest.fromRelativePaths(Vector(bold, events, sidecar.value), Vector(derivative)),
      sidecars = Map(sidecar -> metadata(2.0))
    )
    val shape = DatasetShape.unsafe(NeuroSpace(Vector(2, 2, 1)), 3)
    Fixture(
      project,
      datasetRecipe(
        maskPolicy = MaskPolicy.Explicit(WorkflowArtifactRef.unsafe[MaskImageResource]("file:///mask.nii")),
        confounds = None
      ),
      ImageHeaderCatalog(Map(BidsPath(bold) -> ImageHeaderDescriptor(shape)))
    )

  private def metadata(repetitionTime: Double): JsonValue.Obj =
    BidsJson.parseObject(s"{\"RepetitionTime\":$repetitionTime}").toOption.get
