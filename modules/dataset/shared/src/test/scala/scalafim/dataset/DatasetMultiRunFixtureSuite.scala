package scalafim.dataset

import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.{DMat, NeuroSpace}

class DatasetMultiRunFixtureSuite extends munit.FunSuite:

  test("typed index resolves multi-subject session run datasets into selected series views") {
    val fixtures =
      Vector(
        runFixture("sub-01", Some("ses-01"), "rest", "MNI", "run-1", 10.0),
        runFixture("sub-01", Some("ses-01"), "rest", "MNI", "run-2", 20.0),
        runFixture("sub-01", Some("ses-02"), "nback", "T1w", "run-1", 30.0),
        runFixture("sub-02", None, "rest", "MNI", "run-1", 40.0)
      )
    val index = DatasetIndex
      .fromRuns(
        fixtures.map(_._1)
      )
      .fold(error => fail(error.message), identity)
    val readers =
      SynchronousDatasetReaders
        .build(fixtures.map(_._2)*)
        .fold(error => fail(error.message), identity)

    assertEquals(index.subjects.map(_.value), Vector("sub-01", "sub-02"))

    val restRuns =
      index.resolve(
        DatasetRunQuery(
          subject = Some(SubjectId("sub-01")),
          session = DatasetFieldCriterion.Is(SessionId("ses-01")),
          task = DatasetFieldCriterion.Is(TaskId("rest")),
          space = DatasetFieldCriterion.Is(SpaceId("MNI"))
        )
      )
    assertEquals(restRuns.map(_.key.run.value), Vector("run-1", "run-2"))

    val selected =
      index
        .one(
          DatasetRunQuery(
            subject = Some(SubjectId("sub-01")),
            session = DatasetFieldCriterion.Is(SessionId("ses-01")),
            task = DatasetFieldCriterion.Is(TaskId("rest")),
            run = Some(RunId("run-2"))
          )
        )
        .fold(error => fail(error.message), identity)

    assertEquals(selected.descriptor.timeAxis.runIds.map(_.value), Vector("run-2"))
    assertEquals(selected.dataset.events.typedRows.map(_.run.map(_.value)), Vector(Some("run-2"), Some("run-2")))

    val series =
      selected.series(
        readers,
        DataSelection(
          time = TimepointSelection.indices(0, 2),
          voxels = VoxelSelection.indices(1)
        )
      )

    assertEquals(series.timepoints, Vector(0, 2))
    assertEquals(series.voxelIndices, Vector(1))
    assertMatrixEquals(series.data, Vector(Vector(21.0), Vector(41.0)))
  }

  test("multi-run FmriDataset aligns events and partitions selected rows by run") {
    val samplingFrame = SamplingFrame(blockLens = Seq(2, 2), tr = Seq(1.0))
    val timeAxis =
      DatasetTimeAxis
        .fromSamplingFrame(samplingFrame, Vector(RunId("run-1"), RunId("run-2")))
        .fold(error => fail(error.message), identity)
    val events = DatasetEvents(
      Vector(
        Map("onset" -> "0.0", "duration" -> "0.5", "run" -> "run-1", "condition" -> "face"),
        Map("onset" -> "1.0", "duration" -> "0.5", "run" -> "run-2", "condition" -> "house")
      )
    )
    val dataset =
      FmriDataset
        .open(
          backend = InMemoryDatasetBackend(
            id = DatasetId("sub-01-ses-01-rest"),
            data = DMat.fromRows(
              Vector(
                Vector(1.0, 2.0),
                Vector(3.0, 4.0),
                Vector(5.0, 6.0),
                Vector(7.0, 8.0)
              )
            ),
            space = NeuroSpace(Vector(2, 1, 1))
          ),
          samplingFrame = samplingFrame,
          runIds = timeAxis.runIds,
          events = events
        )
        .fold(error => fail(error.message), identity)

    assertEquals(events.validateAgainst(timeAxis), Right(()))

    val partitions =
      dataset.runPartitions(
        DataSelection(time = TimepointSelection.indices(1, 2, 3))
      )
    assertEquals(partitions.map(_.run.value), Vector("run-1", "run-2"))
    assertEquals(partitions.map(_.rowIndices), Vector(Vector(0), Vector(1, 2)))
    assertEquals(partitions.map(_.localTimepoints), Vector(Vector(1), Vector(0, 1)))

    val badEvents = DatasetEvents(Vector(Map("onset" -> "0.0", "run" -> "run-3")))
    assertEquals(
      badEvents.validateAgainst(timeAxis),
      Left(DatasetError.InvalidEventRow(0, "run 'run-3' is not present in the dataset time axis"))
    )
    val rejected =
      FmriDataset.open(
        backend = dataset.backend,
        samplingFrame = samplingFrame,
        events = badEvents,
        runIds = timeAxis.runIds
      )
    assertEquals(
      rejected,
      Left(DatasetError.InvalidEventRow(0, "run 'run-3' is not present in the dataset time axis"))
    )
  }

  private def runFixture(
      subject: String,
      session: Option[String],
      task: String,
      spaceLabel: String,
      runId: String,
      base: Double
  ): (DatasetRun, SynchronousFmriDataset) =
    val key =
      RunKey
        .fromStrings(
          subject = subject,
          session = session,
          task = Some(task),
          space = Some(spaceLabel),
          run = runId
        )
        .fold(error => fail(error.message), identity)
    val dataset =
      runDataset(
        s"${subject}-${session.getOrElse("nosession")}-$task-$runId",
        runId,
        base
      )
    val run =
      DatasetRun
        .make(key, dataset.dataset)
        .fold(error => fail(error.message), identity)
    run -> dataset

  private def runDataset(
      id: String,
      runId: String,
      base: Double
  ): SynchronousFmriDataset =
    val samplingFrame = SamplingFrame(blockLens = Seq(3), tr = Seq(1.0))
    val timeAxis =
      DatasetTimeAxis
        .fromSamplingFrame(samplingFrame, Vector(RunId(runId)))
        .fold(error => fail(error.message), identity)
    FmriDataset
      .open(
        backend = InMemoryDatasetBackend(
          id = DatasetId(id),
          data = DMat.fromRows(
            Vector.tabulate(3) { time =>
              val value = base + time.toDouble * 10.0
              Vector(value, value + 1.0)
            }
          ),
          space = NeuroSpace(Vector(2, 1, 1))
        ),
        samplingFrame = samplingFrame,
        events = DatasetEvents(
          Vector(
            Map("onset" -> "0.0", "duration" -> "0.5", "run" -> runId, "condition" -> "face"),
            Map("onset" -> "1.0", "duration" -> "0.5", "run" -> runId, "condition" -> "house")
          )
        ),
        runIds = timeAxis.runIds
      )
      .fold(error => fail(error.message), identity)

  private def assertMatrixEquals(
      actual: DMat,
      expected: Vector[Vector[Double]],
      tolerance: Double = 1e-12
  ): Unit =
    assertEquals(actual.rows, expected.length)
    assertEquals(actual.cols, expected.head.length)
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.cols do
        assertEqualsDouble(actual(row, column), expected(row)(column), tolerance)
        column += 1
      row += 1
