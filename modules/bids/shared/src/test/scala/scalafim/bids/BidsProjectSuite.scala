package scalafim.bids

class BidsProjectSuite extends munit.FunSuite:
  private def value[A](result: Either[BidsError, A]): A =
    result.fold(err => fail(err.message), identity)

  private def obj(fields: (String, JsonValue)*): JsonValue.Obj =
    JsonValue.Obj(fields.toMap)

  private val project =
    BidsProject(
      root = BidsPath("/tmp/bids"),
      description = None,
      participants = Vector("01", "02"),
      participantsTable = Some(
        value(
          BidsTable.fromRows(
            Vector("participant_id", "age", "group"),
            Vector(
              Vector(Some("sub-01"), Some("25"), Some("control")),
              Vector(Some("sub-02"), Some("30"), Some("patient"))
            )
          )
        )
      ),
      derivatives = Vector(DerivativeRoot(BidsPath("derivatives/fmriprep"), PipelineName("fmriprep"))),
      manifest = BidsManifest.fromRelativePaths(
        Vector(
          "sub-01/ses-A/func/sub-01_ses-A_task-rest_run-01_bold.nii.gz",
          "sub-01/ses-A/func/sub-01_ses-A_task-rest_run-01_bold.json",
          "sub-01/ses-A/func/sub-01_ses-A_task-rest_run-01_events.tsv",
          "sub-01/ses-A/anat/sub-01_ses-A_T1w.nii.gz",
          "sub-02/func/sub-02_task-nback_run-02_bold.nii.gz",
          "sub-02/anat/sub-02_T1w.nii.gz",
          "task-rest_bold.json",
          "task-nback_bold.json",
          "derivatives/fmriprep/sub-01/ses-A/func/sub-01_ses-A_task-rest_run-01_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz",
          "derivatives/fmriprep/sub-01/ses-A/func/sub-01_ses-A_task-rest_run-01_desc-confounds_timeseries.tsv",
          "derivatives/fmriprep/sub-01/ses-A/anat/sub-01_ses-A_space-MNI152NLin2009cAsym_desc-preproc_T1w.nii.gz"
        ),
        derivatives = Vector(DerivativeRoot(BidsPath("derivatives/fmriprep"), PipelineName("fmriprep")))
      ),
      sidecars = Map(
        BidsPath("task-rest_bold.json") -> obj("TaskName" -> JsonValue.Str("Rest"), "RepetitionTime" -> JsonValue.Num(1.5)),
        BidsPath("sub-01/ses-A/func/sub-01_ses-A_task-rest_run-01_bold.json") -> obj("RepetitionTime" -> JsonValue.Num(2.0)),
        BidsPath("task-nback_bold.json") -> obj(
          "TaskName" -> JsonValue.Str("NBack"),
          "VolumeTiming" -> JsonValue.Arr(Vector(JsonValue.Num(0.0), JsonValue.Num(1.2), JsonValue.Num(2.4)))
        )
      )
    )

  test("project summaries expose entity values from the manifest"):
    assertEquals(project.subjects(), Vector("01", "02"))
    assertEquals(project.sessions(), Vector("A"))
    assertEquals(project.tasks(), Vector("nback", "rest"))
    assertEquals(project.runs(), Vector("01", "02"))

  test("participants table preserves dataframe-like participant columns"):
    val table = project.participantsTable.getOrElse(fail("expected participants table"))
    assertEquals(table.columns, Vector("participant_id", "age", "group"))
    assertEquals(value(table.column("participant_id")), Vector(Some("sub-01"), Some("sub-02")))
    assertEquals(value(table.columnNamed("age")).values, Vector(Some("25"), Some("30")))

  test("project summaries can be scoped to derivative pipelines"):
    assertEquals(project.subjects(scope = BidsScope.Derivatives, pipeline = Some(PipelineName("fmriprep"))), Vector("01"))
    assertEquals(project.tasks(scope = BidsScope.Derivatives, pipeline = Some(PipelineName("fmriprep"))), Vector("rest"))

  test("project summaries group sessions, tasks, and runs by subject/task"):
    assertEquals(project.sessionsBySubject(), Map("01" -> Vector("A"), "02" -> Vector.empty[String]))
    assertEquals(project.tasksBySubject(scope = BidsScope.Raw), Map("01" -> Vector("rest"), "02" -> Vector("nback")))
    assertEquals(
      project.runsByTask(scope = BidsScope.Raw),
      Map(
        BidsTaskKey("01", Some("A"), "rest") -> Vector("01"),
        BidsTaskKey("02", None, "nback") -> Vector("02")
      )
    )

  test("scan selectors expose raw functional, derivative preprocessed, and anatomical files"):
    assertEquals(
      project.funcScans(task = "rest").map(_.path.value),
      Vector("sub-01/ses-A/func/sub-01_ses-A_task-rest_run-01_bold.nii.gz")
    )
    assertEquals(
      project.preprocScans(space = "MNI152NLin2009cAsym").map(_.path.value),
      Vector("derivatives/fmriprep/sub-01/ses-A/func/sub-01_ses-A_task-rest_run-01_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz")
    )
    assertEquals(
      project.anatScans().map(_.path.value),
      Vector("sub-01/ses-A/anat/sub-01_ses-A_T1w.nii.gz", "sub-02/anat/sub-02_T1w.nii.gz")
    )
    assertEquals(
      project
        .anatScans(scope = BidsScope.Derivatives, pipeline = Some(PipelineName("fmriprep")), space = "MNI152NLin2009cAsym")
        .map(_.path.value),
      Vector("derivatives/fmriprep/sub-01/ses-A/anat/sub-01_ses-A_space-MNI152NLin2009cAsym_desc-preproc_T1w.nii.gz")
    )

  test("metadata records attach scan identity and inherited metadata"):
    val records = value(project.funcScanMetadata(subid = "01", task = "rest", run = "01", session = "A"))

    assertEquals(records.length, 1)
    assertEquals(records.head.path.value, "sub-01/ses-A/func/sub-01_ses-A_task-rest_run-01_bold.nii.gz")
    assertEquals(records.head.subject, Some("01"))
    assertEquals(records.head.session, Some("A"))
    assertEquals(records.head.task, Some("rest"))
    assertEquals(records.head.run, Some("01"))
    assertEquals(records.head.string("TaskName"), Some("Rest"))
    assertEquals(records.head.number("RepetitionTime"), Some(2.0))

  test("repetition time inference reads RepetitionTime and falls back to VolumeTiming"):
    assertEquals(value(project.inferRepetitionTime(subid = "01", task = "rest", run = "01", session = "A")), Some(2.0))

    val nback = value(project.repetitionTimes(subid = "02", task = "nback", run = "02"))
    assertEquals(nback.map(_.source), Vector(RepetitionTimeSource.VolumeTiming))
    assertEquals(nback.map(_.value), Vector(1.2))
