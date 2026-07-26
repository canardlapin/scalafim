package scalafim.bids

class BidsQuerySuite extends munit.FunSuite:
  private def value[A](e: Either[BidsError, A]): A =
    e.fold(err => fail(err.message), identity)

  private def filter(key: EntityKey, value0: String): EntityFilter =
    value(EntityFilter.from(key, value0))

  private def query(
      filename: Vector[String] = Vector(".*"),
      filters: Vector[EntityFilter] = Vector.empty,
      matchMode: MatchMode = MatchMode.Regex,
      requireEntity: Boolean = false,
      scope: BidsScope = BidsScope.All,
      pipeline: Option[PipelineName] = None,
      strict: Boolean = true
  ): BidsQuery =
    value(BidsQuery.from(filename, filters, matchMode, requireEntity, scope, pipeline, strict))

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
      query(
        filename = Vector("bold\\.nii\\.gz$"),
        matchMode = MatchMode.Exact,
        scope = BidsScope.Raw,
        filters = Vector(filter(EntityKey.Subject, "01"), filter(EntityKey.Task, "taskA"))
      )
    )

    assertEquals(hits.map(_.value), Vector("sub-01/func/sub-01_task-taskA_run-01_bold.nii.gz"))

  test("query supports regex entity matching"):
    val hits = manifest.paths(
      query(
        filename = Vector("bold\\.nii\\.gz$"),
        matchMode = MatchMode.Regex,
        scope = BidsScope.Raw,
        filters = Vector(filter(EntityKey.Subject, "0[1]"), filter(EntityKey.Task, "task.*"))
      )
    )

    assertEquals(hits.map(_.value), Vector("sub-01/func/sub-01_task-taskA_run-01_bold.nii.gz"))

  test("requireEntity excludes files missing wildcard entity"):
    val lax = manifest.paths(
      query(
        filename = Vector("T1w\\.nii\\.gz$"),
        filters = Vector(filter(EntityKey.Task, ".*")),
        requireEntity = false,
        scope = BidsScope.Raw
      )
    )
    val strict = manifest.paths(
      query(
        filename = Vector("T1w\\.nii\\.gz$"),
        filters = Vector(filter(EntityKey.Task, ".*")),
        requireEntity = true,
        scope = BidsScope.Raw
      )
    )

    assertEquals(lax.map(_.value), Vector("sub-01/anat/sub-01_T1w.nii.gz"))
    assertEquals(strict, Vector.empty)

  test("query separates raw and derivative scopes and pipelines"):
    val hits = manifest.paths(
      query(
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
      query(
        filename = Vector(".*\\.nii\\.gz$"),
        matchMode = MatchMode.Glob,
        filters = Vector(filter(EntityKey.Task, "task?")),
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

  test("query and filter smart constructors reject invalid states"):
    assert(EntityFilter.from(EntityKey.Subject, Vector.empty).isLeft)
    assert(EntityFilter.from(EntityKey.Subject, Vector(" ")).isLeft)
    assert(BidsQuery.from(filename = Vector.empty).isLeft)
    assert(BidsQuery.from(filename = Vector("["), filters = Vector(filter(EntityKey.Task, "["))).isLeft)

  test("checked query construction accumulates independent pattern and filter issues"):
    val report =
      BidsQuery
        .fromChecked(
          filename = Vector("[", "("),
          filters = Vector(filter(EntityKey.Task, "["))
        )
        .left
        .toOption
        .getOrElse(fail("expected validation issues"))

    assertEquals(report.errors.length, 3)
    assertEquals(
      report.issues.flatMap(_.field),
      Vector("filename[0]", "filename[1]", "filters[0].values[0]")
    )
    assert(BidsQuery.from(filename = Vector("[", "(")).isLeft)
