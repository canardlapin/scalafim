package scalafim.dataset

import gale.linalg.DMat
import image4s.geometry.GeometryError
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.{SampleSpaces, SomeSampleSpace, VoxelCoord}

class SegmentedFmriSeriesSuite extends munit.FunSuite:

  test("indexed reads preserve ordered run origins, partitions, and provenance") {
    val fixture = studyFixture()
    val result =
      fixture.index
        .read(
          readers = fixture.readers,
          query = DatasetRunQuery(
            subject = Some(SubjectId("sub-01")),
            task = DatasetFieldCriterion.Is(TaskId("rest"))
          ),
          selection = DataSelection(
            time = TimepointSelection.Window(TimepointIndex.unsafe(0), length = 2),
            voxels = VoxelSelection.coords(VoxelCoord(1, 0, 0))
          )
        )
        .fold(error => fail(error.message), identity)

    assertEquals(result.segments.map(_.key.run.value), Vector("run-1", "run-2", "run-1"))
    assertEquals(
      result.segments.map(_.key.dataset.session.map(_.value)),
      Vector(Some("ses-01"), Some("ses-01"), Some("ses-02"))
    )
    assertEquals(result.segments.map(_.partition.timepoints), Vector(Vector(0, 1), Vector(0, 1), Vector(0, 1)))
    assertEquals(result.segments.map(_.partition.localTimepoints), Vector(Vector(0, 1), Vector(0, 1), Vector(0, 1)))
    assertEquals(
      result.segments.map(segment => GaleTestData.toRows(segment.series.data)),
      Vector(
        Vector(Vector(2.0), Vector(12.0)),
        Vector(Vector(102.0), Vector(112.0)),
        Vector(Vector(202.0), Vector(212.0))
      )
    )
    assertEquals(
      result.segments.map(_.series.metadata.provenance.map(_.source)),
      Vector(Some("fixture:run-1"), Some("fixture:run-2"), Some("fixture:run-1"))
    )
  }

  test("groupBy reorganizes, blockConcatenate relayouts with boundaries, and reduceBy changes values once per group") {
    val fixture = studyFixture()
    val selected =
      fixture.index
        .read(
          fixture.readers,
          DatasetRunQuery(subject = Some(SubjectId("sub-01"))),
          DataSelection(
            time = TimepointSelection.Window(TimepointIndex.unsafe(0), length = 2),
            voxels = VoxelSelection.indices(0)
          )
        )
        .fold(error => fail(error.message), identity)

    val grouped = selected.groupBy(_.key.dataset.session)
    assertEquals(grouped(sessionIdOption("ses-01")).segments.length, 2)
    assertEquals(grouped(sessionIdOption("ses-02")).segments.length, 1)
    assert(grouped(sessionIdOption("ses-01")).segments.head eq selected.segments.head)

    val concatenated =
      selected.blockConcatenate.fold(error => fail(error.message), identity)
    assertEquals(
      GaleTestData.toRows(concatenated.series.data),
      Vector(Vector(1.0), Vector(11.0), Vector(101.0), Vector(111.0), Vector(201.0), Vector(211.0))
    )
    assertEquals(concatenated.series.timepoints, Vector(0, 1, 2, 3, 4, 5))
    assertEquals(concatenated.boundaries.map(_.rowStart), Vector(0, 2, 4))
    assertEquals(concatenated.boundaries.map(_.rowEndExclusive), Vector(2, 4, 6))
    assertEquals(
      concatenated.boundaries.map(_.partition.localTimepoints),
      Vector(Vector(0, 1), Vector(0, 1), Vector(0, 1))
    )

    val calls = scala.collection.mutable.Map.empty[Option[SessionId], Int]
    val reduced =
      selected
        .reduceBy(_.key.dataset.session): group =>
          val session = group.segments.head.key.dataset.session
          calls.update(session, calls.getOrElse(session, 0) + 1)
          Right(group.segments.map(segment => GaleTestData.toRows(segment.series.data).flatten.sum).sum)
        .fold(error => fail(error.message), identity)
    assertEquals(calls.toMap, Map(sessionIdOption("ses-01") -> 1, sessionIdOption("ses-02") -> 1))
    assertEquals(reduced(sessionIdOption("ses-01")), 224.0)
    assertEquals(reduced(sessionIdOption("ses-02")), 412.0)
  }

  test("block concatenation rejects incompatible voxel grids and empty queries stay typed") {
    val translated =
      GaleTestData.matrixFromRows(
        Vector(
          Vector(1.0, 0.0, 0.0, 5.0),
          Vector(0.0, 1.0, 0.0, 0.0),
          Vector(0.0, 0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    val fixtures =
      Vector(
        runFixture("ses-01", "run-1", 0.0, SampleSpaces(Vector(2, 1, 1))),
        runFixture("ses-01", "run-2", 100.0, SampleSpaces(Vector(2, 1, 1), affine = Some(GaleTestData.affineD3(translated))))
      )
    val index =
      DatasetIndex
        .fromRuns(fixtures.map(_._1))
        .fold(error => fail(error.message), identity)
    val readers =
      SynchronousDatasetReaders
        .build(fixtures.map(_._2)*)
        .fold(error => fail(error.message), identity)
    val selected =
      index
        .read(
          readers,
          DatasetRunQuery.All,
          DataSelection(voxels = VoxelSelection.indices(0))
        )
        .fold(error => fail(error.message), identity)

    assertEquals(
      selected.blockConcatenate.left.toOption,
      Some(DatasetError.Geometry(GeometryError.GridsNotCongruent(0.0)))
    )
    assert(index
      .read(
        readers,
        DatasetRunQuery(subject = Some(SubjectId("sub-99")))
      )
      .left
      .toOption
      .contains(DatasetError.DatasetRunNotFound(
        "subject=sub-99,session=*,task=*,space=*,run=*"
      )))
  }

  private final case class StudyFixture(
      index: DatasetIndex,
      readers: SynchronousDatasetReaders
  )

  private def studyFixture(): StudyFixture =
    val fixtures =
      Vector(
        runFixture("ses-01", "run-1", 0.0),
        runFixture("ses-01", "run-2", 100.0),
        runFixture("ses-02", "run-1", 200.0)
      )
    val index =
      DatasetIndex
        .fromRuns(fixtures.map(_._1))
        .fold(error => fail(error.message), identity)
    val readers =
      SynchronousDatasetReaders
        .build(fixtures.map(_._2)*)
        .fold(error => fail(error.message), identity)
    StudyFixture(index, readers)

  private def runFixture(
      session: String,
      run: String,
      base: Double,
      space: SomeSampleSpace = SampleSpaces(Vector(2, 1, 1))
  ): (DatasetRun, SynchronousFmriDataset) =
    val key =
      RunKey.unsafe(
        subject = "sub-01",
        session = Some(session),
        task = Some("rest"),
        space = Some("MNI"),
        run = run
      )
    val metadata =
      DatasetMetadata(Map("session" -> session))
        .withProvenance(FixtureProvenance(run))
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(
          id = DatasetId(s"$session-$run"),
          data = GaleTestData.matrixFromRows(
            Vector.tabulate(3): time =>
              Vector(base + time.toDouble * 10.0 + 1.0, base + time.toDouble * 10.0 + 2.0)
          ),
          space = space,
          metadata = metadata
        ),
        samplingFrame = SamplingFrame(blockLens = Seq(3), tr = Seq(1.0)),
        runId = key.run
      )
    val datasetRun =
      DatasetRun
        .make(key, dataset.dataset)
        .fold(error => fail(error.message), identity)
    datasetRun -> dataset

  private def sessionIdOption(value: String): Option[SessionId] =
    Some(SessionId(value))

  private final case class FixtureProvenance(run: String) extends DatasetProvenance:
    val source: String = s"fixture:$run"
