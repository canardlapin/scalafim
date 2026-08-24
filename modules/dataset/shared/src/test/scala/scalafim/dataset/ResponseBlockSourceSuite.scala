package scalafim.dataset

import munit.FunSuite
import scalafim.image.{Mask, PrimitiveBuffers, SampleSpaces, SomeSampleSpace}

class ResponseBlockSourceSuite extends FunSuite:
  test("composite source preserves requested global time and voxel order") {
    val first = new RecordingSource(timepoints = 2, base = 0.0)
    val second = new RecordingSource(timepoints = 2, base = 100.0)
    val composite = CompositeResponseBlockSource.unsafe(
      Vector(
        RunResponseBlockSource(RunId("run-1"), first),
        RunResponseBlockSource(RunId("run-2"), second)
      )
    )
    val selection = DataSelection(
      time = TimepointSelection.indices(3, 0, 2),
      voxels = VoxelSelection.indices(2, 0)
    )

    val block = composite.readBlock(selection).toOption.get

    assertEquals(block.timepoints, Vector(3, 0, 2))
    assertEquals(block.voxelIndices, Vector(2, 0))
    assertMatrixEquals(
      block.data.toRows,
      Vector(
        Vector(112.0, 110.0),
        Vector(2.0, 0.0),
        Vector(102.0, 100.0)
      )
    )
    assertEquals(first.reads, Vector(Vector(0) -> Vector(2, 0)))
    assertEquals(second.reads, Vector(Vector(1, 0) -> Vector(2, 0)))
    assertEquals(block.data.rows * block.data.cols, 6)
  }

  test("composite source rejects incompatible geometry and voxel domains") {
    val first = new RecordingSource(timepoints = 2, base = 0.0)
    val differentGeometry = new RecordingSource(
      timepoints = 2,
      base = 100.0,
      space = SampleSpaces(Vector(1, 3, 1))
    )
    val masked = new RecordingSource(
      timepoints = 2,
      base = 200.0,
      domain = VoxelDomain.activeUnsafe(3, Vector(VoxelIndex.unsafe(0), VoxelIndex.unsafe(2)))
    )

    assert(CompositeResponseBlockSource.make(Vector(
      RunResponseBlockSource(RunId("run-1"), first),
      RunResponseBlockSource(RunId("run-2"), differentGeometry)
    )).isLeft)
    assert(CompositeResponseBlockSource.make(Vector(
      RunResponseBlockSource(RunId("run-1"), first),
      RunResponseBlockSource(RunId("run-2"), masked)
    )).isLeft)
  }

  test("source validates selections before invoking storage") {
    val source = new RecordingSource(timepoints = 2, base = 0.0)
    val invalid = DataSelection(
      time = TimepointSelection.indices(2),
      voxels = VoxelSelection.indices(0)
    )

    assert(source.readBlock(invalid).isLeft)
    assertEquals(source.reads, Vector.empty)
  }

  test("dataset backend rejects a mask from incompatible geometry during construction") {
    val source = new RecordingSource(timepoints = 2, base = 0.0)
    val mask = Mask.all(SampleSpaces(Vector(1, 3, 1)))

    val result =
      ResponseBlockDatasetBackend.make(
        DatasetId("geometry-mismatch"),
        source,
        mask
      )

    assert(result.left.exists(_.message.contains("grid mismatch")))
  }

  private final class RecordingSource(
      timepoints: Int,
      base: Double,
      val space: SomeSampleSpace = SampleSpaces(Vector(3, 1, 1)),
      domain: VoxelDomain | Null = null
  ) extends ResponseBlockSource:
    val shape: DatasetShape = DatasetShape.unsafe(space, timepoints)
    val voxelDomain: VoxelDomain =
      if domain == null then VoxelDomain.fullUnsafe(shape)
      else domain.asInstanceOf[VoxelDomain]
    val metadata: DatasetMetadata = DatasetMetadata.Empty
    var reads: Vector[(Vector[Int], Vector[Int])] = Vector.empty

    protected[dataset] def readResolved(
        selection: ResolvedDataSelection
    ): Either[DatasetError, FmriSeries] =
      reads = reads :+ (selection.timepoints -> selection.voxels)
      val values = PrimitiveBuffers.ofSize[Double](selection.nTimepoints * selection.nVoxels)
      var row = 0
      while row < selection.nTimepoints do
        var column = 0
        while column < selection.nVoxels do
          values(row * selection.nVoxels + column) =
            base + selection.timepoints(row).toDouble * 10.0 + selection.voxels(column).toDouble
          column += 1
        row += 1
      FmriSeries.make(
        data = matrixFromRowMajor(selection.nTimepoints, selection.nVoxels, values),
        voxelIndices = selection.voxelIndexValues,
        timepoints = selection.timepointIndices,
        shape = shape,
        metadata = metadata
      )

  private def assertMatrixEquals(
      actual: Vector[Vector[Double]],
      expected: Vector[Vector[Double]],
      tolerance: Double = 1e-12
  ): Unit =
    assertEquals(actual.map(_.length), expected.map(_.length))
    actual.zip(expected).foreach { case (actualRow, expectedRow) =>
      actualRow.zip(expectedRow).foreach { case (actualValue, expectedValue) =>
        assertEqualsDouble(actualValue, expectedValue, tolerance)
      }
    }
