package scalafim.dataset

import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.{DMat, SampleSpaces, SomeSampleSpace}

class DatasetSuite extends munit.FunSuite:

  private def backend: InMemoryDatasetBackend =
    InMemoryDatasetBackend(
      id = DatasetId("demo"),
      data = DMat.fromRows(denseRows),
      space = SampleSpaces(Vector(2, 2, 1))
    )

  test("opaque identifiers reject blank values") {
    assertEquals(DatasetId(" ds01 ").value, "ds01")
    intercept[IllegalArgumentException](DatasetId(" "))
  }

  test("dataset shape validates sampling frame alignment") {
    val ds =
      FmriDataset
        .open(
          backend = backend,
          samplingFrame = SamplingFrame(blockLens = Seq(3), tr = Seq(1.0)),
          runId = RunId("run-1")
        )
        .fold(error => fail(error.message), identity)
    assertEquals(ds.shape.timepoints, 3)
    assertEquals(ds.shape.spatialSize, 4)
  }

  test("checked dataset construction reports temporal incompatibility") {
    val result =
      FmriDataset.open(
        backend = backend,
        samplingFrame = SamplingFrame(blockLens = Seq(2), tr = Seq(1.0)),
        runId = RunId("run-1")
      )

    assertEquals(
      result.left.map(_.message),
      Left("dataset shape mismatch: sampling frame has 2 timepoints but dataset shape has 3")
    )
  }

  test("pure dataset descriptions require an explicit synchronous reader") {
    val synchronous =
      FmriDataset.unsafe(
        backend = backend,
        samplingFrame = SamplingFrame(blockLens = Seq(3), tr = Seq(1.0)),
        runId = RunId("run-1")
      )
    val description =
      FmriDataset
        .describe(
          synchronous.id,
          synchronous.shape,
          synchronous.voxelDomain,
          synchronous.metadata,
          synchronous.samplingFrame,
          synchronous.timeAxis.runIds,
          synchronous.events
        )
        .fold(error => fail(error.message), identity)

    assertEquals(
      description.seriesEither().left.toOption,
      Some(DatasetError.SynchronousReaderNotFound(description.id))
    )
    val explicit =
      SynchronousDatasetReaders
        .one(synchronous)
        .readerFor(description)
        .flatMap(_.seriesEither())
        .fold(error => fail(error.message), identity)
    assertEquals(explicit.data.toRows, denseRows)
  }

  test("dataset shape rejects 4D spaces as spatial-only shapes") {
    val fourD = SampleSpaces(Vector(2, 2, 1, 3))
    val error = DatasetShape.make(fourD, timepoints = 3).left.toOption.getOrElse(fail("expected 4D shape rejection"))
    assert(error.message.contains("requires exactly 3 dimensions"))

    val thrown = intercept[IllegalArgumentException] {
      DatasetShape(fourD, timepoints = 3)
    }
    assert(thrown.getMessage.contains("dataset space must be exactly 3D"))
  }

  test("selection reads timepoints x voxels in canonical orientation") {
    val ds = FmriDataset.unsafe(
      backend = backend,
      samplingFrame = SamplingFrame(blockLens = Seq(3), tr = Seq(1.0))
    )
    val series = ds.series(
      DataSelection(
        time = TimepointSelection.indices(0, 2),
        voxels = VoxelSelection.indices(1, 3)
      )
    )

    assertEquals(series.nTimepoints, 2)
    assertEquals(series.nVoxels, 2)
    assertEquals(series.data.toRows, Vector(Vector(2.0, 4.0), Vector(10.0, 12.0)))
  }

  test("dense dataset default read uses the full spatial voxel domain") {
    val series = backend.read()

    assertEquals(backend.voxelDomain.kind, VoxelDomainKind.FullSpatial)
    assertEquals(backend.voxelDomain.indices, Vector(0, 1, 2, 3))
    assertEquals(series.voxelIndices, Vector(0, 1, 2, 3))
    assertEquals(series.data.toRows, denseRows)
  }

  test("selection reports duplicate and out-of-bounds indices as DatasetError") {
    val duplicate =
      DataSelection(voxels = VoxelSelection.indices(1, 1))
        .resolveEither(backend.shape)
        .left
        .toOption
        .getOrElse(fail("expected duplicate voxel selection"))
    assertEquals(duplicate, DatasetError.DuplicateSelection(DatasetAxis.Voxel, 1))

    val outOfBounds =
      DataSelection(time = TimepointSelection.indices(3))
        .resolveEither(backend.shape)
        .left
        .toOption
        .getOrElse(fail("expected out-of-bounds timepoint selection"))
    assertEquals(outOfBounds, DatasetError.IndexOutOfBounds(DatasetAxis.Timepoint, 3, 3))

    intercept[IllegalArgumentException] {
      DataSelection(voxels = IndexSelection.indices(1, 1)).resolve(backend.shape)
    }
  }

  test("safe series read agrees with the throwing convenience method") {
    val ds = FmriDataset.unsafe(
      backend = backend,
      samplingFrame = SamplingFrame(blockLens = Seq(3), tr = Seq(1.0))
    )
    val failed =
      ds.seriesEither(DataSelection(voxels = VoxelSelection.indices(4)))
        .left
        .toOption
        .getOrElse(fail("expected voxel bounds error"))
    assertEquals(failed, DatasetError.IndexOutOfBounds(DatasetAxis.Voxel, 4, 4))

    val thrown = intercept[IllegalArgumentException] {
      ds.series(DataSelection(voxels = VoxelSelection.indices(4)))
    }
    assert(thrown.getMessage.contains("voxel index 4 out of bounds"))
  }

  test("FmriSeries safe constructor validates matrix and typed index shape") {
    val bad =
      FmriSeries
        .fromIntIndices(
          data = DMat.fromRows(Vector(Vector(1.0))),
          voxelIndices = Vector(0, 1),
          timepoints = Vector(0),
          shape = backend.shape
        )
        .left
        .toOption
        .getOrElse(fail("expected series shape error"))

    assert(bad.message.contains("series has 1 columns but 2 selected voxels"))
  }

  private def denseRows: Vector[Vector[Double]] =
    Vector(
      Vector(1.0, 2.0, 3.0, 4.0),
      Vector(5.0, 6.0, 7.0, 8.0),
      Vector(9.0, 10.0, 11.0, 12.0)
    )
