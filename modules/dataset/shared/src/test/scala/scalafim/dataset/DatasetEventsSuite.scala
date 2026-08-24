package scalafim.dataset

import scalafim.fmri.hrf.design.SamplingFrame

class DatasetEventsSuite extends munit.FunSuite:

  test("fromRows parses reserved event fields into typed accessors") {
    val events = DatasetEvents
      .fromRows(
        Vector(
          Map(
            "onset" -> "0.5",
            "duration" -> "1.25",
            "run" -> "run-1",
            "session" -> "ses-1",
            "condition" -> "face",
            "keep" -> "true"
          )
        )
      )
      .fold(error => fail(error.message), identity)

    val row = events.typedRows.head
    assertEquals(row.onset.map(_.value), Some(0.5))
    assertEquals(row.duration.map(_.value), Some(1.25))
    assertEquals(row.run.map(_.value), Some("run-1"))
    assertEquals(row.session.map(_.value), Some("ses-1"))
    assertEquals(row.condition.map(_.value), Some("face"))
    assertEquals(row.get(DatasetFieldId("keep")).flatMap(_.asBoolean), Some(true))
    assertEquals(events.rows.head("run"), "run-1")
    assertEquals(events.columnEither(DatasetFieldId("missing")), Left(DatasetError.DatasetColumnNotFound("missing")))
  }

  test("fromRows reports invalid event fields as typed row errors") {
    val err = DatasetEvents
      .fromRows(Vector(Map("onset" -> "0.0", "duration" -> "-1.0")))
      .swap
      .fold(error => fail(s"expected invalid duration, got ${error.toString}"), identity)

    err match
      case DatasetError.InvalidEventRow(0, detail) =>
        assert(detail.contains("invalid dataset value for 'duration'='-1.0'"))
        assert(detail.contains("must be non-negative"))
      case other =>
        fail(s"unexpected error: $other")
  }

  test("fromRows rejects ragged event tables at construction") {
    val err = DatasetEvents
      .fromRows(
        Vector(
          Map("onset" -> "0.0", "condition" -> "face"),
          Map("onset" -> "1.0")
        )
      )
      .swap
      .fold(error => fail(s"expected ragged rows, got ${error.toString}"), identity)

    assert(err.message.contains("invalid dataset event row 1"))
  }

  test("fromColumns preserves typed values including missing numeric markers") {
    val events = DatasetEvents
      .fromColumns(
        "trial_id" -> DatasetEventColumn.text(Vector("trial-1", "trial-2")),
        "run" -> DatasetEventColumn.text(Vector("run-1", "run-1")),
        "rt" -> DatasetEventColumn.numbers(Vector(0.72, Double.NaN)),
        "correct" -> DatasetEventColumn.booleans(Vector(true, false))
      )
      .fold(error => fail(error.message), identity)

    assertEquals(events.nrows, 2)
    assertEquals(events.typedRows.map(_.run.map(_.value)), Vector(Some("run-1"), Some("run-1")))
    assertEquals(events.column(DatasetFieldId("correct")).flatMap(_.asBoolean), Vector(true, false))
    assert(events.column(DatasetFieldId("rt"))(1).asDouble.exists(_.isNaN))
  }

  test("fromColumns rejects duplicate, empty, and ragged columns") {
    val duplicate = DatasetEvents.fromColumns(
      "condition" -> DatasetEventColumn.text(Vector("face")),
      "condition" -> DatasetEventColumn.text(Vector("scene"))
    )
    val empty = DatasetEvents.fromColumns(
      "condition" -> DatasetEventColumn.text(Vector.empty)
    )
    val ragged = DatasetEvents.fromColumns(
      "condition" -> DatasetEventColumn.text(Vector("face", "scene")),
      "onset" -> DatasetEventColumn.numbers(Vector(0.0))
    )

    assertEquals(duplicate, Left(DatasetError.InvalidEventTable("duplicate column 'condition'")))
    assertEquals(empty, Left(DatasetError.InvalidEventTable("event columns must contain at least one row")))
    assertEquals(ragged, Left(DatasetError.InvalidEventTable("column 'onset' has 1 rows; expected 2")))
  }

  test("validateAgainst checks typed run labels against the dataset time axis") {
    val timeAxis = DatasetTimeAxis
      .fromSamplingFrame(SamplingFrame(blockLens = Seq(2), tr = Seq(1.0)), Vector(RunId("run-1")))
      .fold(error => fail(error.message), identity)
    val ok = DatasetEvents(
      Vector(
        Map("onset" -> "0.0", "duration" -> "0.2", "run" -> "run-1")
      )
    )
    val bad = DatasetEvents(
      Vector(
        Map("onset" -> "0.0", "duration" -> "0.2", "run" -> "run-2")
      )
    )

    assertEquals(ok.validateAgainst(timeAxis), Right(()))
    assertEquals(
      bad.validateAgainst(timeAxis),
      Left(DatasetError.InvalidEventRow(0, "run 'run-2' is not present in the dataset time axis"))
    )
  }
