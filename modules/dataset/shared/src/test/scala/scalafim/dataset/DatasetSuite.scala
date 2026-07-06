package scalafim.dataset

import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.{DMat, NeuroSpace}

class DatasetSuite extends munit.FunSuite:

  private def backend: InMemoryDatasetBackend =
    val rows = Vector(
      Vector(1.0, 2.0, 3.0, 4.0),
      Vector(5.0, 6.0, 7.0, 8.0),
      Vector(9.0, 10.0, 11.0, 12.0)
    )
    InMemoryDatasetBackend(
      id = DatasetId("demo"),
      data = DMat.fromRows(rows),
      space = NeuroSpace(Vector(2, 2, 1))
    )

  test("opaque identifiers reject blank values") {
    assertEquals(DatasetId(" ds01 ").value, "ds01")
    intercept[IllegalArgumentException](DatasetId(" "))
  }

  test("dataset shape validates sampling frame alignment") {
    val ds = FmriDataset(
      backend = backend,
      samplingFrame = SamplingFrame(blockLens = Seq(3), tr = Seq(1.0))
    )
    assertEquals(ds.shape.timepoints, 3)
    assertEquals(ds.shape.spatialSize, 4)
  }

  test("selection reads timepoints x voxels in canonical orientation") {
    val ds = FmriDataset(
      backend = backend,
      samplingFrame = SamplingFrame(blockLens = Seq(3), tr = Seq(1.0))
    )
    val series = ds.series(
      DataSelection(
        time = IndexSelection.indices(0, 2),
        voxels = IndexSelection.indices(1, 3)
      )
    )

    assertEquals(series.nTimepoints, 2)
    assertEquals(series.nVoxels, 2)
    assertEquals(series.data.toRows, Vector(Vector(2.0, 4.0), Vector(10.0, 12.0)))
  }

  test("selection rejects duplicate and out-of-bounds indices") {
    intercept[IllegalArgumentException] {
      DataSelection(voxels = IndexSelection.indices(1, 1)).resolve(backend.shape)
    }
    intercept[IllegalArgumentException] {
      DataSelection(time = IndexSelection.indices(3)).resolve(backend.shape)
    }
  }
