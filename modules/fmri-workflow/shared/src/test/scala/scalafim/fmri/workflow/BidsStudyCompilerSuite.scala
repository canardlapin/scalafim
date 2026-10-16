package scalafim.fmri.workflow

import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import scalafim.image.SampleSpaces

import munit.FunSuite
import bids4s.*
import scalafim.dataset.{DatasetId, DatasetShape}
import scalafim.image.SomeSampleSpace

class BidsStudyCompilerSuite extends FunSuite:

  private val fixtureWorld = scalafim.image.world.WorldSpace.declare("registered BIDS compiler fixture world")
    .fold(error => fail(error.message), identity)

  private def fixtureSpace(dims: Vector[Int]): image4s.SomeSampleSpace =
    SampleSpaces.inWorld(SampleSpaces(dims), fixtureWorld).fold(error => fail(error.message), identity)
  test("observed slice-timing metadata preserves true, false and unknown separately") {
    val fixture = studyFixture(subjects = Vector("01"), runs = Vector("01"))
    Vector(None, Some(false), Some(true)).foreach { status =>
      val sidecars = fixture.project.sidecars.map { (path, metadata) =>
        val fields = metadata.fields ++ status.map(value => "SliceTimingCorrected" -> JsonValue.Bool(value))
        path -> (JsonValue.Obj(fields): JsonValue.Obj)
      }
      val project = fixture.project.copy(sidecars = sidecars)
      val unit = BidsStudyCompiler.compile(project, fixture.recipe, fixture.headers).toOption.get.units.head
      assertEquals(unit.runs.head.timing.sliceTimingCorrected, status)
      // Metadata must not silently select a sampling reference.
      assertEqualsDouble(SamplingReference.VolumeMidpoint.resolve(unit.runs).toOption.get.runs.head.firstSample.value, 1.0, 0.0)
    }
  }

  test("present malformed slice-timing correction declarations are refused") {
    val fixture = studyFixture(subjects = Vector("01"), runs = Vector("01"))
    Vector(JsonValue.Str("false"), JsonValue.Num(0.0), JsonValue.Null).foreach { value =>
      val project = fixture.project.copy(sidecars = fixture.project.sidecars.map { (path, metadata) =>
        path -> (JsonValue.Obj(metadata.fields.updated("SliceTimingCorrected", value)): JsonValue.Obj)
      })
      val report = BidsStudyCompiler.compile(project, fixture.recipe, fixture.headers).left.toOption.get
      assert(report.issues.exists(issue => issue.code == CatalogIssueCode.InvalidTimingMetadata &&
        issue.field.contains("SliceTimingCorrected") && issue.path.nonEmpty))
    }
  }

  test("raw companion metadata does not assert derivative slice-timing correction") {
    val fixture = studyFixture(subjects = Vector("01"), runs = Vector("01"))
    val raw = "sub-01/func/sub-01_task-demo_acq-mb_run-01_echo-1_bold.json"
    val project = fixture.project.copy(
      manifest = BidsManifest.fromRelativePaths(fixture.project.manifest.files.map(_.path.value) :+ raw, fixture.project.derivatives),
      sidecars = fixture.project.sidecars.updated(BidsPath(raw), JsonValue.Obj(Map("SliceTimingCorrected" -> JsonValue.Bool(true)))))
    val unit = BidsStudyCompiler.compile(project, fixture.recipe, fixture.headers).toOption.get.units.head
    assertEquals(unit.runs.head.timing.sliceTimingCorrected, None)
  }

  test("run and echo index padding selects unique event confound and mask companions") {
    val fixture = studyFixture(subjects = Vector("01"), runs = Vector("01"))
    val padded = "0" * 512 + "1"
    val oldIndex = "run-01_echo-1"
    val paddedIndex = s"run-$padded" + s"_echo-$padded"
    val paths = fixture.project.manifest.files.map(_.path.value).map { path =>
      if path.endsWith("_events.tsv") || path.endsWith("_desc-confounds_timeseries.tsv") || path.endsWith("_mask.nii") then
        path.replace(oldIndex, paddedIndex)
      else path
    }
    val headers = fixture.headers.headers.map { (path, descriptor) =>
      val changed = if path.value.endsWith("_mask.nii") then path.value.replace(oldIndex, paddedIndex) else path.value
      BidsPath(changed) -> descriptor
    }
    val project = fixture.project.copy(manifest = BidsManifest.fromRelativePaths(paths, fixture.project.derivatives))
    val unit = BidsStudyCompiler.compile(project, fixture.recipe, ImageHeaderCatalog(headers)).toOption.get.units.head
    assertEquals(unit.runs.head.id.value, "01")
    assert(unit.runs.head.bold.location.value.contains("run-01_echo-1"))
    assert(unit.runs.head.events.location.value.contains(paddedIndex))
    assert(unit.runs.head.confounds.exists(_.location.value.contains(paddedIndex)))
    assertEquals(unit.mask.artifacts.map(_.location.value),
      paths.filter(_.endsWith("_mask.nii")).map(path => s"file:///study/$path"))
  }

  test("zero-valued run and echo indices match across padding without rewriting run identity") {
    val fixture = studyFixture(subjects = Vector("01"), runs = Vector("01"))
    val originalIndex = "run-01_echo-1"
    val targetIndex = "run-00_echo-00"
    val companionIndex = "run-0_echo-0"
    val paths = fixture.project.manifest.files.map(_.path.value).map { path =>
      val target = path.replace(originalIndex, targetIndex)
      if target.endsWith("_events.tsv") || target.endsWith("_desc-confounds_timeseries.tsv") || target.endsWith("_mask.nii") then
        target.replace(targetIndex, companionIndex)
      else target
    }
    val headers = fixture.headers.headers.map { (path, descriptor) =>
      val target = path.value.replace(originalIndex, targetIndex)
      val changed = if target.endsWith("_mask.nii") then target.replace(targetIndex, companionIndex) else target
      BidsPath(changed) -> descriptor
    }
    val sidecars = fixture.project.sidecars.map { (path, metadata) =>
      BidsPath(path.value.replace(originalIndex, targetIndex)) -> metadata
    }
    val project = fixture.project.copy(
      manifest = BidsManifest.fromRelativePaths(paths, fixture.project.derivatives),
      sidecars = sidecars
    )
    val unit = BidsStudyCompiler.compile(project, fixture.recipe, ImageHeaderCatalog(headers)).toOption.get.units.head
    assertEquals(unit.runs.head.id.value, "00")
    assert(unit.runs.head.bold.location.value.contains(targetIndex))
    assert(unit.runs.head.events.location.value.contains(companionIndex))
  }

  test("padded event and confound duplicates remain equally-specific ambiguities") {
    val fixture = studyFixture(subjects = Vector("01"), runs = Vector("01"))
    val paths = fixture.project.manifest.files.map(_.path.value)
    val event = paths.find(_.endsWith("_events.tsv")).get
    val confound = paths.find(_.endsWith("_desc-confounds_timeseries.tsv")).get
    val padded = "run-1_echo-01"
    val ambiguousEvents = fixture.project.copy(manifest = BidsManifest.fromRelativePaths(
      paths :+ event.replace("run-01_echo-1", padded), fixture.project.derivatives))
    assert(BidsStudyCompiler.compile(ambiguousEvents, fixture.recipe, fixture.headers).left.toOption.get.issues
      .exists(_.code == CatalogIssueCode.AmbiguousCompanion))
    val ambiguousConfounds = fixture.project.copy(manifest = BidsManifest.fromRelativePaths(
      paths :+ confound.replace("run-01_echo-1", padded), fixture.project.derivatives))
    assert(BidsStudyCompiler.compile(ambiguousConfounds, fixture.recipe, fixture.headers).left.toOption.get.issues
      .exists(_.code == CatalogIssueCode.AmbiguousCompanion))
  }

  test("BIDS labels and malformed index values remain exact matches") {
    val fixture = studyFixture(subjects = Vector("01"), runs = Vector("01"))
    val paths = fixture.project.manifest.files.map(_.path.value)
    val event = paths.find(_.endsWith("_events.tsv")).get
    val mask = paths.find(_.endsWith("_mask.nii")).get
    val subjectMismatch = fixture.project.copy(manifest = BidsManifest.fromRelativePaths(
      paths.map(path => if path == event then path.replace("sub-01", "sub-1") else path), fixture.project.derivatives))
    assert(BidsStudyCompiler.compile(subjectMismatch, fixture.recipe, fixture.headers).left.toOption.get.issues
      .exists(_.code == CatalogIssueCode.MissingEvents))
    val sessionPaths = paths.map(_.replace("sub-01_", "sub-01_ses-01_"))
      .map(path => if path.endsWith("_events.tsv") then path.replace("ses-01", "ses-1") else path)
    val sessionHeaders = fixture.headers.headers.map { (path, descriptor) =>
      BidsPath(path.value.replace("sub-01_", "sub-01_ses-01_")) -> descriptor
    }
    val sessionSidecars = fixture.project.sidecars.map { (path, metadata) =>
      BidsPath(path.value.replace("sub-01_", "sub-01_ses-01_")) -> metadata
    }
    val sessionMismatch = fixture.project.copy(
      manifest = BidsManifest.fromRelativePaths(sessionPaths, fixture.project.derivatives),
      sidecars = sessionSidecars
    )
    assert(BidsStudyCompiler.compile(sessionMismatch, fixture.recipe, ImageHeaderCatalog(sessionHeaders)).left.toOption.get.issues
      .exists(_.code == CatalogIssueCode.MissingEvents))
    val acquisitionMismatch = fixture.project.copy(manifest = BidsManifest.fromRelativePaths(
      paths.map(path => if path == event then path.replace("acq-mb", "acq-0mb") else path), fixture.project.derivatives))
    assert(BidsStudyCompiler.compile(acquisitionMismatch, fixture.recipe, fixture.headers).left.toOption.get.issues
      .exists(_.code == CatalogIssueCode.MissingEvents))
    val resolutionPaths = paths.map(path => if path == mask then path.replace("res-2", "res-02") else path)
    val resolutionHeaders = fixture.headers.headers.map { (path, descriptor) =>
      BidsPath(if path.value == mask then path.value.replace("res-2", "res-02") else path.value) -> descriptor
    }
    val resolutionMismatch = fixture.project.copy(manifest = BidsManifest.fromRelativePaths(resolutionPaths, fixture.project.derivatives))
    assert(BidsStudyCompiler.compile(resolutionMismatch, fixture.recipe, ImageHeaderCatalog(resolutionHeaders)).left.toOption.get.issues
      .exists(_.code == CatalogIssueCode.MissingMask))
    val malformedTargetPaths = paths.map(_.replace("run-01", "run-01a"))
      .map(path => if path.endsWith("_events.tsv") then path.replace("run-01a", "run-1a") else path)
    val malformedHeaders = fixture.headers.headers.map { (path, descriptor) =>
      BidsPath(path.value.replace("run-01", "run-01a")) -> descriptor
    }
    val malformedSidecars = fixture.project.sidecars.map { (path, metadata) =>
      BidsPath(path.value.replace("run-01", "run-01a")) -> metadata
    }
    val malformed = fixture.project.copy(
      manifest = BidsManifest.fromRelativePaths(malformedTargetPaths, fixture.project.derivatives),
      sidecars = malformedSidecars
    )
    assert(BidsStudyCompiler.compile(malformed, fixture.recipe, ImageHeaderCatalog(malformedHeaders)).left.toOption.get.issues
      .exists(_.code == CatalogIssueCode.MissingEvents))
  }

  test("numeric-equivalent BOLD aliases are refused under both grouping policies") {
    Vector("run-1_echo-1", "run-01_echo-01").foreach { alias =>
      Vector(RunGrouping.BySubjectSessionTask, RunGrouping.OneUnitPerRun).foreach { grouping =>
        val fixture = studyFixture(subjects = Vector("01"), runs = Vector("01"), grouping = grouping)
        val paths = fixture.project.manifest.files.map(_.path.value)
        val bold = paths.find(_.endsWith("_bold.nii")).get
        val aliasBold = bold.replace("run-01_echo-1", alias)
        val sidecar = BidsPath(bold.stripSuffix(".nii") + ".json")
        val aliasSidecar = BidsPath(aliasBold.stripSuffix(".nii") + ".json")
        val project = fixture.project.copy(
          manifest = BidsManifest.fromRelativePaths(paths ++ Vector(aliasBold, aliasSidecar.value), fixture.project.derivatives),
          sidecars = fixture.project.sidecars.updated(aliasSidecar, fixture.project.sidecars(sidecar))
        )
        val headers = fixture.headers.headers.updated(BidsPath(aliasBold), fixture.headers.headers(BidsPath(bold)))
        val report = BidsStudyCompiler.compile(project, fixture.recipe, ImageHeaderCatalog(headers)).left.toOption.get
        val collision = report.issues.find(_.code == CatalogIssueCode.DuplicateRun).getOrElse(fail("missing alias collision"))
        assertEquals(collision.path, Some(BidsPath(Vector(bold, aliasBold).sorted.head)))
        assert(collision.message.contains(bold) && collision.message.contains(aliasBold))
      }
    }
  }

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
        path -> ImageHeaderDescriptor(DatasetShape.unsafe(fixtureSpace(Vector(4, 1, 1)), 3))
      else path -> descriptor
    }
    val incompatible = BidsStudyCompiler
      .compile(fixture.project, fixture.recipe, ImageHeaderCatalog(incompatibleHeaders))
      .left
      .toOption
      .get
    val runGeometryIssue = incompatible.issues.find: issue =>
      issue.code == CatalogIssueCode.IncompatibleGeometry &&
        issue.path.exists(path => path.value.contains("run-02") && path.value.endsWith("_bold.nii"))
    assertEquals(
      runGeometryIssue.flatMap(_.cause),
      Some(CatalogIssueCause.Geometry(GeometryError.GridsNotCongruent(0.0)))
    )

    val duplicate = duplicateRunFixture()
    val duplicateReport = BidsStudyCompiler.compile(duplicate.project, duplicate.recipe, duplicate.headers).left.toOption.get
    assert(duplicateReport.issues.exists(_.code == CatalogIssueCode.DuplicateRun))
  }

  test("compiler preserves provider frame failures for mask geometry") {
    val fixture = studyFixture(subjects = Vector("01"), runs = Vector("01"))
    val incompatibleHeaders = fixture.headers.headers.map { case (path, descriptor) =>
      if path.value.endsWith("_mask.nii") then
        path -> ImageHeaderDescriptor(ephemeralShape("compiler-mask-ephemeral", 1))
      else path -> descriptor
    }
    val incompatible = BidsStudyCompiler
      .compile(fixture.project, fixture.recipe, ImageHeaderCatalog(incompatibleHeaders))
      .left
      .toOption
      .get
    val maskGeometryIssue = incompatible.issues.find: issue =>
      issue.code == CatalogIssueCode.IncompatibleGeometry &&
        issue.path.exists(_.value.endsWith("_mask.nii"))

    assertEquals(
      maskGeometryIssue.flatMap(_.cause),
      Some(CatalogIssueCause.Geometry(GeometryError.EphemeralFrameMismatch))
    )
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

  test("checked compilation preserves deterministic BIDS diagnostics and valid legacy parity") {
    val fixture = identityFixture(task = "demo")
    val validPaths = fixture.project.manifest.files.map(_.path.value)
    val defects = Vector(
      "sub-03/func/sub-03_task-rest_custom.tsv",
      "sub-02/func/sub-02_bold.nii.gz",
      "notes.txt",
      "sub-01/func/sub-01_task-_bold.nii.gz",
      "../escape.tsv"
    )

    def checked(paths: Vector[String]): Either[CatalogCompileReport, CatalogCompilation] =
      val manifest = BidsManifest.fromRelativePathsChecked(paths, fixture.project.derivatives)
      val project = manifest.map(value => fixture.project.copy(manifest = value))
      BidsStudyCompiler.compileChecked(project, fixture.recipe, fixture.headers)

    val report = checked(validPaths ++ defects).left.toOption.getOrElse(fail("expected validation errors"))
    val reversed = checked((validPaths ++ defects).reverse).left.toOption.getOrElse(fail("expected validation errors"))

    assertEquals(report.issues, reversed.issues)
    assertEquals(report.errors.length, 3)
    assertEquals(report.warnings.length, 2)
    assertEquals(
      report.issues.map(issue => (issue.path.map(_.value), issue.code, issue.severity, issue.field)),
      Vector(
        (Some("../escape.tsv"), CatalogIssueCode.BidsValidation(BidsIssueCode.InvalidPath), BidsIssueSeverity.Error, Some("path")),
        (Some("notes.txt"), CatalogIssueCode.BidsValidation(BidsIssueCode.UnrecognizedFile), BidsIssueSeverity.Warning, None),
        (Some("sub-01/func/sub-01_task-_bold.nii.gz"), CatalogIssueCode.BidsValidation(BidsIssueCode.InvalidName), BidsIssueSeverity.Error, None),
        (Some("sub-02/func/sub-02_bold.nii.gz"), CatalogIssueCode.BidsValidation(BidsIssueCode.MissingRequiredField), BidsIssueSeverity.Error, Some("task")),
        (Some("sub-03/func/sub-03_task-rest_custom.tsv"), CatalogIssueCode.BidsValidation(BidsIssueCode.UnsupportedFileRole), BidsIssueSeverity.Warning, None)
      )
    )

    val legacy = BidsStudyCompiler.compile(fixture.project, fixture.recipe, fixture.headers).toOption.get
    val valid = checked(validPaths).toOption.get
    assertEquals(valid.catalog, legacy)
    assertEquals(valid.issues, Vector.empty)

    val warningOnly = checked(validPaths :+ "notes.txt").toOption.get
    assertEquals(warningOnly.catalog, legacy)
    assertEquals(
      warningOnly.warnings.map(issue => (issue.path.map(_.value), issue.code, issue.severity)),
      Vector(
        (
          Some("notes.txt"),
          CatalogIssueCode.BidsValidation(BidsIssueCode.UnrecognizedFile),
          BidsIssueSeverity.Warning
        )
      )
    )
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
    val boldShape = DatasetShape.unsafe(fixtureSpace(Vector(2, 2, 1)), 3)
    val maskShape = DatasetShape.unsafe(fixtureSpace(Vector(2, 2, 1)), 1)

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
      boldQuery = BidsQuery.from(
        filename = Vector("desc-preproc_bold\\.nii$"),
        scope = BidsScope.Derivatives,
        pipeline = Some(PipelineName("fmriprep"))
      ).toOption.get,
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
    val shape = DatasetShape.unsafe(fixtureSpace(Vector(2, 2, 1)), 3)
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
      boldQuery = BidsQuery.from(
        filename = Vector("desc-(preproc|denoised)_bold\\.nii$"),
        scope = BidsScope.Derivatives,
        pipeline = Some(PipelineName("fmriprep"))
      ).toOption.get,
      maskPolicy = MaskPolicy.Explicit(WorkflowArtifactRef.unsafe[MaskImageResource]("file:///mask.nii")),
      confounds = None
    )
    val shape = DatasetShape.unsafe(fixtureSpace(Vector(2, 2, 1)), 3)
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
    val shape = DatasetShape.unsafe(fixtureSpace(Vector(2, 2, 1)), 3)
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

  private def ephemeralShape(label: String, timepoints: Int): DatasetShape =
    val frame = right(Frame.named[D3](label))
    val grid = right(Grid.in(frame)(Vector(2, 2, 1), Affine.identity[D3]))
    DatasetShape.unsafe(
      SampleSpace.create(grid, NonSpatialAxes.empty),
      timepoints
    )

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(s"expected Right, found Left($error)")
