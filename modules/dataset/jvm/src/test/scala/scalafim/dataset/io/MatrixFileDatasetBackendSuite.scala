package scalafim.dataset.io

import scalafim.dataset.{DataSelection, DatasetId, GaleTestData, TimepointSelection, VoxelSelection}
import scalafim.image.{SampleSpaces, SomeSampleSpace}

import java.nio.charset.StandardCharsets
import java.nio.file.Files

class MatrixFileDatasetBackendSuite extends munit.FunSuite:

  private def withMatrixFile[A](contents: String)(f: java.nio.file.Path => A): A =
    val path = Files.createTempFile("scalafim-matrix-", ".csv")
    Files.writeString(path, contents, StandardCharsets.UTF_8)
    try f(path)
    finally Files.deleteIfExists(path)

  test("MatrixFileDatasetBackend reads a concrete matrix file") {
    withMatrixFile("1,2,3,4\n5,6,7,8\n9,10,11,12\n") { path =>
      val backend = MatrixFileDatasetBackend(
        id = DatasetId("matrix-demo"),
        path = path,
        space = SampleSpaces(Vector(2, 2, 1))
      )

      assertEquals(backend.shape.timepoints, 3)
      assertEquals(backend.shape.spatialSize, 4)

      val series = backend.read(
        DataSelection(
          time = TimepointSelection.indices(0, 2),
          voxels = VoxelSelection.indices(1, 3)
        )
      )
      assertEquals(GaleTestData.toRows(series.data), Vector(Vector(2.0, 4.0), Vector(10.0, 12.0)))
      assertEquals(series.timepoints, Vector(0, 2))
      assertEquals(series.voxelIndices, Vector(1, 3))
    }
  }

  test("MatrixFileDatasetBackend rejects non-numeric and shape-mismatched files") {
    withMatrixFile("1,2\n3,nope\n") { path =>
      val backend = MatrixFileDatasetBackend(DatasetId("bad-numeric"), path, SampleSpaces(Vector(2, 1, 1)))
      val safeError = backend.readEither().left.toOption.getOrElse(fail("expected non-numeric read error"))
      assert(safeError.message.contains("non-numeric"))

      val err = intercept[IllegalArgumentException](backend.shape)
      assert(err.getMessage.contains("non-numeric"))
    }

    withMatrixFile("1,2,3\n4,5,6\n") { path =>
      val backend = MatrixFileDatasetBackend(DatasetId("bad-shape"), path, SampleSpaces(Vector(2, 1, 1)))
      val safeError = backend.readEither().left.toOption.getOrElse(fail("expected shape read error"))
      assert(safeError.message.contains("space has 2 voxels"))

      val err = intercept[IllegalArgumentException](backend.shape)
      assert(err.getMessage.contains("space has 2 voxels"))
    }
  }
