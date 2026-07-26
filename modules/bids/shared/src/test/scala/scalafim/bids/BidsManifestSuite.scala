package scalafim.bids

class BidsManifestSuite extends munit.FunSuite:
  test("checked manifest construction preserves valid legacy output"):
    val paths =
      Vector(
        "sub-01/func/sub-01_task-rest_run-01_bold.nii.gz",
        "sub-01/func/sub-01_task-rest_run-01_events.tsv",
        "derivatives/fmriprep/sub-01/func/sub-01_task-rest_space-MNI_desc-preproc_bold.nii.gz"
      )
    val derivatives = Vector(DerivativeRoot(BidsPath("derivatives/fmriprep"), PipelineName("fmriprep")))

    val legacy = BidsManifest.fromRelativePaths(paths, derivatives)
    val checked = BidsManifest.fromRelativePathsChecked(paths, derivatives)

    assertEquals(checked.value, legacy)
    assertEquals(checked.issues, Vector.empty)
    assertEquals(checked.enforce(BidsValidationPolicy.Strict), Right(checked))

  test("checked manifest reports independent path and name defects deterministically"):
    val paths =
      Vector(
        "sub-03/func/sub-03_task-rest_custom.tsv",
        "sub-02/func/sub-02_bold.nii.gz",
        "notes.txt",
        "sub-01/func/sub-01_task-_bold.nii.gz",
        "../escape.tsv"
      )

    val report = BidsManifest.fromRelativePathsChecked(paths)
    val reversed = BidsManifest.fromRelativePathsChecked(paths.reverse)

    assertEquals(report.issues, reversed.issues)
    assertEquals(report.errors.length, 3)
    assertEquals(report.warnings.length, 2)
    assertEquals(
      report.issues.map(issue => issue.path.map(_.value) -> issue.code),
      Vector(
        Some("../escape.tsv") -> BidsIssueCode.InvalidPath,
        Some("notes.txt") -> BidsIssueCode.UnrecognizedFile,
        Some("sub-01/func/sub-01_task-_bold.nii.gz") -> BidsIssueCode.InvalidName,
        Some("sub-02/func/sub-02_bold.nii.gz") -> BidsIssueCode.MissingRequiredField,
        Some("sub-03/func/sub-03_task-rest_custom.tsv") -> BidsIssueCode.UnsupportedFileRole
      )
    )
    assertEquals(report.value.files.length, 4)

    val rejected = report.enforce(BidsValidationPolicy.Strict).left.toOption.getOrElse(fail("strict policy should reject errors"))
    assertEquals(rejected.issues, report.issues)
    assertEquals(rejected.primary.code, BidsIssueCode.InvalidPath)
    assertEquals(report.enforce(BidsValidationPolicy.Collect), Right(report))

  test("checked manifest distinguishes auxiliary and unsupported files from invalid BIDS candidates"):
    val report =
      BidsManifest.fromRelativePathsChecked(
        Vector(
          "dataset_description.json",
          "notes.txt",
          "sub-01/func/sub-01_task-rest_events.bad",
          "sub-01/func/sub-01_task-rest_custom.tsv"
        )
      )

    assertEquals(
      report.issues.map(issue => issue.code -> issue.severity),
      Vector(
        BidsIssueCode.UnrecognizedFile -> BidsIssueSeverity.Warning,
        BidsIssueCode.UnsupportedFileRole -> BidsIssueSeverity.Warning,
        BidsIssueCode.InvalidName -> BidsIssueSeverity.Error
      )
    )

  test("checked manifest normalizes and de-duplicates valid relative paths"):
    val report =
      BidsManifest.fromRelativePathsChecked(
        Vector(
          "./sub-01//func/sub-01_task-rest_bold.nii.gz",
          "sub-01/func/sub-01_task-rest_bold.nii.gz"
        )
      )

    assertEquals(report.value.files.map(_.path.value), Vector("sub-01/func/sub-01_task-rest_bold.nii.gz"))
    assertEquals(report.issues, Vector.empty)

  test("checked manifest accepts inherited metadata sidecars above datatype folders"):
    val report =
      BidsManifest.fromRelativePathsChecked(
        Vector(
          "task-rest_bold.json",
          "sub-01/task-rest_bold.json",
          "sub-01/func/sub-01_task-rest_bold.nii.gz"
        )
      )

    assertEquals(report.issues, Vector.empty)
    assertEquals(
      report.value.files.map(_.path.value),
      Vector(
        "task-rest_bold.json",
        "sub-01/task-rest_bold.json",
        "sub-01/func/sub-01_task-rest_bold.nii.gz"
      )
    )

  test("checked manifest accumulates independent entity defects within one filename"):
    val derivative = DerivativeRoot(BidsPath("derivatives/fmriprep"), PipelineName("fmriprep"))
    val path = "derivatives/fmriprep/sub-01/func/sub-01_dir-AP_bold.nii.gz"

    val report = BidsManifest.fromRelativePathsChecked(Vector(path), Vector(derivative))

    assertEquals(
      report.issues.map(issue => (issue.path.map(_.value), issue.code, issue.field)),
      Vector(
        (Some(path), BidsIssueCode.InvalidName, Some("entities")),
        (Some(path), BidsIssueCode.InvalidEntity, Some("dir")),
        (Some(path), BidsIssueCode.MissingRequiredField, Some("task"))
      )
    )
