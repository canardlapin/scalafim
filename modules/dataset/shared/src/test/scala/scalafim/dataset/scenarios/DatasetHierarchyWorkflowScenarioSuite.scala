package scalafim.dataset.scenarios

import scalafim.dataset.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.{DMat, NeuroSpace, VoxelCoord}

class DatasetHierarchyWorkflowScenarioSuite extends munit.FunSuite:

  test("cross-session coordinate window remains segmented until explicitly concatenated") {
    val result = runScenario()
    assert(result.ciPass(ScenarioPolicy.PassOnly), result.render)
  }

  private def runScenario(): ScenarioResult =
    val scenarioId = "dataset.cross-session-coordinate-window.v1"
    val fixtures =
      Vector(
        run("ses-01", "run-01", base = 0.0),
        run("ses-01", "run-02", base = 100.0),
        run("ses-02", "run-01", base = 200.0)
      )
    val index =
      DatasetIndex
        .fromRuns(fixtures.map(_._1))
        .fold(error => fail(error.message), identity)
    val readers =
      SynchronousDatasetReaders
        .build(fixtures.map(_._2)*)
        .fold(error => fail(error.message), identity)
    val query =
      DatasetRunQuery(
        subject = Some(SubjectId("sub-01")),
        task = DatasetFieldCriterion.Is(TaskId("rest")),
        space = DatasetFieldCriterion.Is(SpaceId("MNI"))
      )
    val selection =
      DataSelection(
        time = TimepointSelection.unsafeWindow(start = 1, length = 2),
        voxels = VoxelSelection.coords(VoxelCoord(1, 0, 0))
      )
    val selected =
      index
        .read(readers, query, selection)
        .fold(error => fail(error.message), identity)
    val bySession = selected.groupBy(_.key.dataset.session)
    val concatenated =
      selected.blockConcatenate.fold(error => fail(error.message), identity)
    val sums =
      selected
        .reduceBy(_.key.dataset.session): group =>
          Right(group.segments.flatMap(_.series.data.toRows).flatten.sum)
        .fold(error => fail(error.message), identity)

    ScenarioHarness.result(
      scenarioId,
      Vector(
        ScenarioHarness.fact(
          "segments.keys",
          selected.segments.map(_.key.label) == Vector(
            "subject=sub-01,session=ses-01,task=rest,space=MNI,run=run-01",
            "subject=sub-01,session=ses-01,task=rest,space=MNI,run=run-02",
            "subject=sub-01,session=ses-02,task=rest,space=MNI,run=run-01"
          ),
          selected.segments.map(_.key.label).mkString(";")
        ),
        ScenarioHarness.fact(
          "segments.local-time",
          selected.segments.forall(_.partition.localTimepoints == Vector(1, 2)),
          selected.segments.map(_.partition.localTimepoints).mkString(";")
        ),
        ScenarioHarness.fact(
          "groupBy.preserves-segments",
          bySession(session("ses-01")).segments.length == 2 &&
            bySession(session("ses-02")).segments.length == 1 &&
            (bySession(session("ses-01")).segments.head eq selected.segments.head),
          bySession.iterator.map((key, value) => s"$key=${value.size}").mkString(",")
        ),
        ScenarioHarness.fact(
          "blockConcatenate.boundaries",
          concatenated.boundaries.map(boundary =>
            (boundary.key.run.value, boundary.rowStart, boundary.rowEndExclusive)
          ) == Vector(
            ("run-01", 0, 2),
            ("run-02", 2, 4),
            ("run-01", 4, 6)
          ),
          concatenated.boundaries.map(boundary =>
            s"${boundary.key.run.value}:${boundary.rowStart}-${boundary.rowEndExclusive}"
          ).mkString(",")
        ),
        ScenarioHarness.fact(
          "reduceBy.changes-values-per-session",
          sums == Map(
            session("ses-01") -> 268.0,
            session("ses-02") -> 434.0
          ),
          sums.toString
        )
      ) ++
        ScenarioHarness.matrix(
          "blockConcatenate.values",
          concatenated.series.data,
          Vector(
            Vector(12.0),
            Vector(22.0),
            Vector(112.0),
            Vector(122.0),
            Vector(212.0),
            Vector(222.0)
          ),
          ScenarioTolerance.absolute(0.0)
        )
    )

  private def run(
      sessionId: String,
      runId: String,
      base: Double
  ): (DatasetRun, SynchronousFmriDataset) =
    val key =
      RunKey.unsafe(
        subject = "sub-01",
        session = Some(sessionId),
        task = Some("rest"),
        space = Some("MNI"),
        run = runId
      )
    val dataset =
      FmriDataset
        .open(
          backend = InMemoryDatasetBackend(
            id = DatasetId(s"$sessionId-$runId"),
            data = DMat.fromRows(
              Vector.tabulate(3): time =>
                Vector(
                  base + time.toDouble * 10.0 + 1.0,
                  base + time.toDouble * 10.0 + 2.0
                )
            ),
            space = NeuroSpace(Vector(2, 1, 1)),
            metadata = DatasetMetadata(Map("session" -> sessionId))
          ),
          samplingFrame = SamplingFrame(blockLens = Seq(3), tr = Seq(1.0)),
          runId = key.run
        )
        .fold(error => fail(error.message), identity)
    val datasetRun =
      DatasetRun
        .make(key, dataset.dataset)
        .fold(error => fail(error.message), identity)
    datasetRun -> dataset

  private def session(value: String): Option[SessionId] =
    Some(SessionId(value))
