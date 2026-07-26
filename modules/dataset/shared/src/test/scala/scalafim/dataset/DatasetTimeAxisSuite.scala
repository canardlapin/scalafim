package scalafim.dataset

import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.{DMat, NeuroSpace}

class DatasetTimeAxisSuite extends munit.FunSuite:

  private val samplingFrame =
    SamplingFrame(blockLens = Seq(2, 3, 1), tr = Seq(1.0))

  test("dataset time axis maps global timepoints to run-local positions") {
    val axis =
      DatasetTimeAxis
        .fromSamplingFrame(samplingFrame, Vector(RunId("run-a"), RunId("run-b"), RunId("run-c")))
        .fold(err => fail(err.message), identity)

    assertEquals(axis.timepoints, 6)
    assertEquals(axis.blockLengths, Vector(2, 3, 1))
    assertEquals(axis.runIds.map(_.value), Vector("run-a", "run-b", "run-c"))

    val (run, local) =
      axis
        .localIndex(TimepointIndex.unsafe(3))
        .fold(err => fail(err.message), identity)
    assertEquals(run.value, "run-b")
    assertEquals(local.value, 1)
  }

  test("dataset time axis partitions censored selections by run") {
    val axis =
      DatasetTimeAxis
        .fromSamplingFrame(samplingFrame, Vector(RunId("run-a"), RunId("run-b"), RunId("run-c")))
        .fold(err => fail(err.message), identity)

    val partitions =
      axis
        .partitionsForInts(Vector(0, 2, 4, 5))
        .fold(err => fail(err.message), identity)

    assertEquals(partitions.map(_.run.value), Vector("run-a", "run-b", "run-c"))
    assertEquals(partitions.map(_.rowIndices), Vector(Vector(0), Vector(1, 2), Vector(3)))
    assertEquals(partitions.map(_.timepoints), Vector(Vector(0), Vector(2, 4), Vector(5)))
    assertEquals(partitions.map(_.localTimepoints), Vector(Vector(0), Vector(0, 2), Vector(0)))
  }

  test("dataset time axis rejects duplicate and out-of-bounds selected timepoints") {
    val axis = DatasetTimeAxis.fromSamplingFrame(samplingFrame).fold(err => fail(err.message), identity)

    val negative =
      axis
        .partitionsForInts(Vector(-1))
        .left
        .toOption
        .getOrElse(fail("expected negative selected timepoint"))
    assertEquals(negative, DatasetError.NegativeIndex(DatasetAxis.Timepoint, -1))

    val duplicate =
      axis
        .partitionsForInts(Vector(0, 0))
        .left
        .toOption
        .getOrElse(fail("expected duplicate selected timepoint"))
    assertEquals(duplicate, DatasetError.DuplicateSelection(DatasetAxis.Timepoint, 0))

    val outOfBounds =
      axis
        .partitionsForInts(Vector(6))
        .left
        .toOption
        .getOrElse(fail("expected out-of-bounds selected timepoint"))
    assertEquals(outOfBounds, DatasetError.IndexOutOfBounds(DatasetAxis.Timepoint, 6, 6))
  }

  test("FmriDataset derives a time axis and exposes dataset-backed partitions") {
    val dataset = fmriDataset()

    assertEquals(dataset.timeAxis.runIds.map(_.value), Vector("run-1", "run-2", "run-3"))

    val partitions =
      dataset.runPartitions(
        DataSelection(time = TimepointSelection.indices(1, 2, 5))
      )
    assertEquals(partitions.map(_.run.value), Vector("run-1", "run-2", "run-3"))
    assertEquals(partitions.map(_.rowIndices), Vector(Vector(0), Vector(1), Vector(2)))
    assertEquals(partitions.map(_.localTimepoints), Vector(Vector(1), Vector(0), Vector(0)))
  }

  test("FmriDataset accepts explicit typed run ids when block lengths align") {
    val dataset = fmriDataset(Vector(RunId("baseline"), RunId("task"), RunId("localizer")))

    assertEquals(dataset.timeAxis.runIds.map(_.value), Vector("baseline", "task", "localizer"))
    assertEquals(dataset.timeAxis.blocks(1).globalTimepoints.map(_.value), Vector(2, 3, 4))
  }

  private def fmriDataset(
      runIds: Vector[RunId] = Vector(RunId("run-1"), RunId("run-2"), RunId("run-3"))
  ): FmriDataset =
    FmriDataset
      .open(
        backend = InMemoryDatasetBackend(
          id = DatasetId("time-axis-demo"),
          data = DMat.fromRows(Vector.tabulate(6)(row => Vector(row.toDouble))),
          space = NeuroSpace(Vector(1, 1, 1))
        ),
        samplingFrame = samplingFrame,
        runIds = runIds
      )
      .fold(error => fail(error.message), identity)
