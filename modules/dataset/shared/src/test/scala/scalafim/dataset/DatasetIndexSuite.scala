package scalafim.dataset

import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.{DMat, NeuroSpace}

class DatasetIndexSuite extends munit.FunSuite:

  private val space = NeuroSpace(Vector(1, 1, 1))

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
    val index = DatasetIndex.single(key, dataset)

    val selected =
      index
        .one(DatasetRunQuery.forKey(key))
        .fold(err => fail(err.message), identity)

    assertEquals(selected.dataset, dataset)
    assertEquals(selected.descriptor.shape.timepoints, dataset.shape.timepoints)
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
    DatasetRun(key, fmriDataset(id))

  private def fmriDataset(id: String): FmriDataset =
    FmriDataset(
      backend = InMemoryDatasetBackend(
        id = DatasetId(id),
        data = DMat.fromRows(Vector(Vector(1.0), Vector(2.0))),
        space = space
      ),
      samplingFrame = SamplingFrame(blockLens = Seq(2), tr = Seq(1.0))
    )
