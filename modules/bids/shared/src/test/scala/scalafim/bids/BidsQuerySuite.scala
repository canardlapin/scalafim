package scalafim.bids

class BidsQuerySuite extends munit.FunSuite:
  private def value[A](e: Either[BidsError, A]): A =
    e.fold(err => fail(err.message), identity)

  private val manifest =
    BidsManifest.fromRelativePaths(
      Vector(
        "participants.tsv",
        "sub-01/anat/sub-01_T1w.nii.gz",
        "sub-01/func/sub-01_task-taskA_run-01_bold.nii.gz",
        "sub-01/func/sub-01_task-taskA_run-01_events.tsv",
        "derivatives/fmriprep/sub-01/func/sub-01_task-taskA_space-MNI_desc-preproc_bold.nii.gz"
      ),
      derivatives = Vector(DerivativeRoot(BidsPath("derivatives/fmriprep"), PipelineName("fmriprep")))
    )

  test("query supports exact entity matching"):
    val hits = manifest.paths(
      BidsQuery(
        filename = Vector("bold\\.nii\\.gz$"),
        matchMode = MatchMode.Exact,
        scope = BidsScope.Raw,
        filters = Vector(EntityFilter(EntityKey.Subject, "01"), EntityFilter(EntityKey.Task, "taskA"))
      )
    )

    assertEquals(hits.map(_.value), Vector("sub-01/func/sub-01_task-taskA_run-01_bold.nii.gz"))

  test("query supports regex entity matching"):
    val hits = manifest.paths(
      BidsQuery(
        filename = Vector("bold\\.nii\\.gz$"),
        matchMode = MatchMode.Regex,
        scope = BidsScope.Raw,
        filters = Vector(EntityFilter(EntityKey.Subject, "0[1]"), EntityFilter(EntityKey.Task, "task.*"))
      )
    )

    assertEquals(hits.map(_.value), Vector("sub-01/func/sub-01_task-taskA_run-01_bold.nii.gz"))

  test("requireEntity excludes files missing wildcard entity"):
    val lax = manifest.paths(
      BidsQuery(
        filename = Vector("T1w\\.nii\\.gz$"),
        filters = Vector(EntityFilter(EntityKey.Task, ".*")),
        requireEntity = false,
        scope = BidsScope.Raw
      )
    )
    val strict = manifest.paths(
      BidsQuery(
        filename = Vector("T1w\\.nii\\.gz$"),
        filters = Vector(EntityFilter(EntityKey.Task, ".*")),
        requireEntity = true,
        scope = BidsScope.Raw
      )
    )

    assertEquals(lax.map(_.value), Vector("sub-01/anat/sub-01_T1w.nii.gz"))
    assertEquals(strict, Vector.empty)

  test("query separates raw and derivative scopes and pipelines"):
    val hits = manifest.paths(
      BidsQuery(
        filename = Vector("bold\\.nii\\.gz$"),
        scope = BidsScope.Derivatives,
        pipeline = Some(PipelineName("fmriprep"))
      )
    )

    assertEquals(
      hits.map(_.value),
      Vector("derivatives/fmriprep/sub-01/func/sub-01_task-taskA_space-MNI_desc-preproc_bold.nii.gz")
    )

  test("glob matching applies to entity filters"):
    val hits = manifest.paths(
      BidsQuery(
        filename = Vector(".*\\.nii\\.gz$"),
        matchMode = MatchMode.Glob,
        filters = Vector(EntityFilter(EntityKey.Task, "task?")),
        strict = true
      )
    )

    assertEquals(
      hits.map(_.value),
      Vector(
        "derivatives/fmriprep/sub-01/func/sub-01_task-taskA_space-MNI_desc-preproc_bold.nii.gz",
        "sub-01/func/sub-01_task-taskA_run-01_bold.nii.gz"
      )
    )

  test("typed filename patterns validate regexes and escape exact names"):
    assert(QueryPattern.regex("[").isLeft)
    assert(BidsQuery.from(filename = Vector("[")).isLeft)

    val exact = value(QueryPattern.exact("sub-01_task-taskA_run-01_bold.nii.gz"))
    val query = value(BidsQuery.fromPatterns(Vector(exact), scope = BidsScope.Raw))
    val hits = manifest.paths(query)

    assertEquals(hits.map(_.value), Vector("sub-01/func/sub-01_task-taskA_run-01_bold.nii.gz"))

    val directInvalid = BidsQuery(filename = Vector("["))
    assertEquals(manifest.paths(directInvalid), Vector.empty)

  test("query and filter validation is total and direct constructors do not throw"):
    assert(EntityFilter.from(EntityKey.Subject, Vector.empty).isLeft)
    assert(BidsQuery.from(filename = Vector.empty).isLeft)

    val emptyFilenameQuery = BidsQuery(filename = Vector.empty)
    assertEquals(manifest.paths(emptyFilenameQuery), Vector.empty)

    val emptyFilterQuery = BidsQuery(filters = Vector(EntityFilter(EntityKey.Subject, Vector.empty)))
    assertEquals(manifest.paths(emptyFilterQuery), Vector.empty)
