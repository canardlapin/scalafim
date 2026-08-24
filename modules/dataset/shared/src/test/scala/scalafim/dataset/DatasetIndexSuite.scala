package scalafim.dataset

import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.{DMat, SampleSpaces, SomeSampleSpace, VoxelCoord}

class DatasetIndexSuite extends munit.FunSuite:

  private val space = SampleSpaces(Vector(1, 1, 1))

  test("dataset keys have safe string constructors") {
    val key =
      DatasetKey
        .fromStrings(
          subject = " sub-01 ",
          session = Some(" ses-01 "),
          task = Some(" rest "),
          space = Some(" MNI ")
        )
        .fold(err => fail(err.message), identity)

    assertEquals(key.subject.value, "sub-01")
    assertEquals(key.session.map(_.value), Some("ses-01"))
    assertEquals(key.task.map(_.value), Some("rest"))
    assertEquals(key.space.map(_.value), Some("MNI"))
    assertEquals(DatasetKey.fromStrings(subject = " ").left.toOption, Some(DatasetError.InvalidLabel("SubjectId", " ", "must be non-empty")))
  }

  test("dataset index enumerates and resolves typed run keys") {
    val restRun1 = run("rest-run-1", "sub-01", Some("ses-01"), Some("rest"), Some("MNI"), "run-1")
    val restRun2 = run("rest-run-2", "sub-01", Some("ses-01"), Some("rest"), Some("MNI"), "run-2")
    val nbackRun1 = run("nback-run-1", "sub-02", None, Some("nback"), Some("T1w"), "run-1")
    val index =
      DatasetIndex
        .fromRuns(Vector(restRun1, restRun2, nbackRun1))
        .fold(err => fail(err.message), identity)

    assertEquals(index.size, 3)
    assertEquals(index.subjects.map(_.value), Vector("sub-01", "sub-02"))
    assertEquals(index.descriptors.map(_.datasetId.value), Vector("rest-run-1", "rest-run-2", "nback-run-1"))

    val restRuns =
      index.resolve(
        DatasetRunQuery(
          subject = Some(SubjectId("sub-01")),
          task = DatasetFieldCriterion.Is(TaskId("rest"))
        )
      )
    assertEquals(restRuns.map(_.key.run.value), Vector("run-1", "run-2"))

    val missingSession =
      index
        .one(DatasetRunQuery(session = DatasetFieldCriterion.Missing))
        .fold(err => fail(err.message), identity)
    assertEquals(missingSession.key.dataset.subject.value, "sub-02")

    val run2 =
      index
        .one(DatasetRunQuery(subject = Some(SubjectId("sub-01")), run = Some(RunId("run-2"))))
        .fold(err => fail(err.message), identity)
    assertEquals(run2.id.value, "rest-run-2")
  }

  test("dataset index rejects duplicate keys and ambiguous one-run queries") {
    val first = run("first", "sub-01", Some("ses-01"), Some("rest"), Some("MNI"), "run-1")
    val duplicate = run("duplicate", "sub-01", Some("ses-01"), Some("rest"), Some("MNI"), "run-1")

    val duplicateError =
      DatasetIndex
        .fromRuns(Vector(first, duplicate))
        .left
        .toOption
        .getOrElse(fail("expected duplicate key"))
    assert(duplicateError.message.contains("duplicate run key"))

    val index =
      DatasetIndex
        .fromRuns(
          Vector(
            first,
            run("second", "sub-01", Some("ses-01"), Some("rest"), Some("MNI"), "run-2")
          )
        )
        .fold(err => fail(err.message), identity)

    val ambiguous =
      index
        .one(DatasetRunQuery(subject = Some(SubjectId("sub-01"))))
        .left
        .toOption
        .getOrElse(fail("expected ambiguous query"))
    assertEquals(ambiguous, DatasetError.AmbiguousDatasetRun("subject=sub-01,session=*,task=*,space=*,run=*", 2))

    val missing =
      index
        .resolveEither(DatasetRunQuery(subject = Some(SubjectId("sub-99"))))
        .left
        .toOption
        .getOrElse(fail("expected missing query"))
    assertEquals(missing, DatasetError.DatasetRunNotFound("subject=sub-99,session=*,task=*,space=*,run=*"))
  }

  test("single-run compatibility wraps an existing FmriDataset") {
    val dataset = fmriDataset("single")
    val key = RunKey.unsafe("sub-01", "run-1", task = Some("rest"))
    val index =
      DatasetIndex
        .single(key, dataset)
        .fold(err => fail(err.message), identity)

    val selected =
      index
        .one(DatasetRunQuery.forKey(key))
        .fold(err => fail(err.message), identity)

    assertEquals(selected.dataset, dataset)
    assertEquals(selected.descriptor.shape.timepoints, dataset.shape.timepoints)
  }

  test("DatasetRun rejects mismatched keys and multi-run datasets expand as zero-copy views") {
    val single = fmriDataset("single", "run-a")
    val mismatch =
      DatasetRun.make(
        RunKey.unsafe("sub-01", "run-b"),
        single
      )
    assert(mismatch.left.exists(_.message.contains("does not match dataset run run-a")))

    val samplingFrame = SamplingFrame(blockLens = Seq(2, 3), tr = Seq(1.0))
    val multi =
      FmriDataset
        .open(
          backend = InMemoryDatasetBackend(
            id = DatasetId("multi"),
            data = DMat.fromRows(
              Vector.tabulate(5)(time => Vector(time.toDouble, time.toDouble + 10.0))
            ),
            space = SampleSpaces(Vector(2, 1, 1))
          ),
          samplingFrame = samplingFrame,
          runIds = Vector(RunId("run-a"), RunId("run-b"))
        )
        .fold(error => fail(error.message), identity)
    val index =
      DatasetIndex
        .fromDataset(
          DatasetKey.unsafe("sub-01", session = Some("ses-01"), task = Some("rest")),
          multi.dataset
        )
        .fold(error => fail(error.message), identity)

    assertEquals(index.keys.map(_.run.value), Vector("run-a", "run-b"))
    assert(index.runs.forall(_.dataset.eq(multi.dataset)))
    assertEquals(index.descriptors.map(_.shape.timepoints), Vector(2, 3))

    val second = index.runs(1)
    val readers = SynchronousDatasetReaders.one(multi)
    val (series, partition) =
      second
        .partitionedSeriesEither(
          readers,
          DataSelection(
            time = TimepointSelection.Window(TimepointIndex.unsafe(0), length = 2),
            voxels = VoxelSelection.indices(1)
          )
        )
        .fold(error => fail(error.message), identity)
    assertEquals(series.timepoints, Vector(2, 3))
    assertEquals(series.data.toRows, Vector(Vector(12.0), Vector(13.0)))
    assertEquals(partition.timepoints, Vector(2, 3))
    assertEquals(partition.localTimepoints, Vector(0, 1))

    assert(second
      .seriesEither(
        readers,
        DataSelection(time = TimepointSelection.indices(3))
      )
      .left
      .exists(_.message.contains("out of bounds for size 3")))
  }

  test("cross-run coordinate reads require exact grids") {
    val translated =
      DMat.fromRows(
        Vector(
          Vector(1.0, 0.0, 0.0, 4.0),
          Vector(0.0, 1.0, 0.0, 0.0),
          Vector(0.0, 0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    val first =
      datasetRun(
        RunKey.unsafe("sub-01", "run-1", space = Some("MNI")),
        "first-grid",
        SampleSpaces(Vector(2, 1, 1))
      )
    val second =
      datasetRun(
        RunKey.unsafe("sub-01", "run-2", space = Some("MNI")),
        "second-grid",
        SampleSpaces(Vector(2, 1, 1), trans = Some(translated))
      )
    val index =
      DatasetIndex
        .fromRuns(Vector(first, second))
        .fold(error => fail(error.message), identity)

    val result =
      index.resolveForRead(
        DatasetRunQuery(subject = Some(SubjectId("sub-01"))),
        DataSelection(voxels = VoxelSelection.coords(VoxelCoord(0, 0, 0)))
      )

    assert(result.left.exists(_.message.contains("requires identical run grids")))
  }

  private def run(
      id: String,
      subject: String,
      session: Option[String],
      task: Option[String],
      spaceLabel: Option[String],
      runId: String
  ): DatasetRun =
    val key =
      RunKey
        .fromStrings(subject = subject, session = session, task = task, space = spaceLabel, run = runId)
        .fold(err => fail(err.message), identity)
    DatasetRun
      .make(key, fmriDataset(id, runId))
      .fold(err => fail(err.message), identity)

  private def fmriDataset(id: String, runId: String = "run-1"): FmriDataset =
    FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        id = DatasetId(id),
        data = DMat.fromRows(Vector(Vector(1.0), Vector(2.0))),
        space = space
      ),
      samplingFrame = SamplingFrame(blockLens = Seq(2), tr = Seq(1.0)),
      runId = RunId(runId)
    ).dataset

  private def datasetRun(
      key: RunKey,
      id: String,
      runSpace: SomeSampleSpace
  ): DatasetRun =
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(
          id = DatasetId(id),
          data = DMat.fromRows(Vector(Vector(1.0, 2.0), Vector(3.0, 4.0))),
          space = runSpace
        ),
        samplingFrame = SamplingFrame(blockLens = Seq(2), tr = Seq(1.0)),
        runId = key.run
      )
    DatasetRun.make(key, dataset.dataset).fold(error => fail(error.message), identity)
