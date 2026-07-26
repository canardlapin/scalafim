package scalafim.bids

class BidsTableSuite extends munit.FunSuite:
  private def value[A](e: Either[BidsError, A]): A =
    e.fold(err => fail(err.message), identity)

  test("events reader preserves tab-delimited columns"):
    val table = value(BidsEvents.readTable("onset\tduration\ttrial_type\n0\t1\tgo\n2\tNA\tstop\n"))

    assertEquals(table.columns, Vector("onset", "duration", "trial_type"))
    assertEquals(table.nrows, 2)
    assertEquals(value(table.column("duration")), Vector(Some("1"), None))

  test("table parser preserves leading and trailing empty tab cells"):
    val table = value(BidsTable.parse("a\tb\tc\n\t2\t\n1\t\t3\n"))

    assertEquals(table.columns, Vector("a", "b", "c"))
    assertEquals(table.rows, Vector(Vector(None, Some("2"), None), Vector(Some("1"), None, Some("3"))))

  test("events reader falls back to whitespace-delimited files"):
    val table = value(BidsEvents.readTable("onset duration trial_type\n0 1 go\n2 n/a stop\n"))

    assertEquals(table.columns, Vector("onset", "duration", "trial_type"))
    assertEquals(table.nrows, 2)
    assertEquals(value(table.column("duration")), Vector(Some("1"), None))

  test("ragged events tables are rejected"):
    assert(BidsEvents.readTable("onset duration\n0 1 2\n").isLeft)

  test("checked table construction accumulates header and row defects"):
    val report =
      BidsTable
        .fromRowsChecked(
          Vector("", "value", "value"),
          Vector(Vector(Some("1")), Vector(None, None, None, None))
        )
        .left
        .toOption
        .getOrElse(fail("expected validation issues"))

    assertEquals(report.issues.length, 4)
    assertEquals(
      report.issues.flatMap(_.field),
      Vector("columns[0]", "rows[0]", "rows[1]", "value")
    )
    assert(BidsTable.fromRows(Vector("", "value", "value"), Vector(Vector(Some("1")))).isLeft)

  test("checked table parsing accumulates header and row defects"):
    val report =
      BidsTable
        .parseChecked("name\tname\t\n1\t2\n1\t2\t3\t4\n")
        .left
        .toOption
        .getOrElse(fail("expected checked parsing defects"))

    assertEquals(report.issues.length, 4)
    assertEquals(
      report.issues.flatMap(_.field),
      Vector("columns[2]", "name", "rows[0]", "rows[1]")
    )
    assertEquals(report.issues.map(_.code).distinct, Vector(BidsIssueCode.InvalidTable))

  test("typed columns expose safe numeric conversion"):
    val table = value(BidsEvents.readTable("onset duration trial_type\n0 1 go\n2 n/a stop\n"))
    val duration = value(table.columnNamed("duration"))

    assertEquals(duration.name, "duration")
    assertEquals(duration.nrows, 2)
    assertEquals(value(duration.numeric), Vector(Some(1.0), None))
    assert(table.columnNamed("missing").isLeft)

  test("typed events table requires numeric onset and duration"):
    val events = value(BidsEvents.readEventsTable("onset duration trial_type\n0 1 go\n2 NA stop\n"))

    assertEquals(events.onsetSeconds, Vector(Some(0.0), Some(2.0)))
    assertEquals(events.durationSeconds, Vector(Some(1.0), None))
    assertEquals(events.trialType.map(_.name), Some("trial_type"))
    assert(ColumnName.from("").isLeft)
    assert(BidsEvents.readEventsTable("onset trial_type\n0 go\n").isLeft)
    assert(BidsEvents.readEventsTable("onset duration\nzero 1\n").isLeft)

  test("table files attach BIDS path and entity context"):
    val file =
      BidsManifest
        .fromRelativePaths(Vector("sub-01/ses-A/func/sub-01_ses-A_task-rest_run-02_events.tsv"))
        .files
        .head
    val table = value(BidsEvents.readTable("onset duration\n0 1\n"))
    val tableFile = BidsTableFile.from(file, table)

    assertEquals(tableFile.path.value, "sub-01/ses-A/func/sub-01_ses-A_task-rest_run-02_events.tsv")
    assertEquals(tableFile.subject, Some("01"))
    assertEquals(tableFile.session, Some("A"))
    assertEquals(tableFile.task, Some("rest"))
    assertEquals(tableFile.run, Some("02"))
    assertEquals(tableFile.context.kind, Some("events"))
