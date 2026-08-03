package scalafim.fmri.workflow

import munit.FunSuite
import scalafim.dataset.*
import scalafim.image.{Axis, DMat, PrimitiveBuffers, NeuroSpace, NeuroVec, NeuroVol}
import scalafim.image.io.Nifti

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class FirstLevelUnitSourceSuite extends FunSuite:
  test("unit source opens run references lazily and applies the exact mask intersection") {
    withFixture { root =>
      val space = NeuroSpace(Vector(2, 2, 1))
      val bold1 = writeBold(root.resolve("run-1.nii"), space, 0.0)
      val bold2 = writeBold(root.resolve("run-2.nii"), space, 100.0)
      val mask1 = writeMask(root.resolve("mask-1.nii"), space, Array(1.0, 1.0, 1.0, 0.0))
      val mask2 = writeMask(root.resolve("mask-2.nii"), space, Array(1.0, 0.0, 1.0, 1.0))
      val runs = Vector(
        runInput("1", bold1),
        runInput("2", bold2)
      )
      val unit = FirstLevelUnit.unsafe(
        id = FirstLevelUnitId.unsafe("sub-01.task-demo.space-MNI"),
        subject = SubjectId("01"),
        session = None,
        task = TaskId("demo"),
        space = SpaceId("MNI"),
        shape = DatasetShape.unsafe(space, 4),
        runs = runs,
        mask = UnitMask.unsafeIntersection(Vector(
          RunId("1") -> WorkflowArtifactRef.unsafe[MaskImageResource](mask1.toUri.toString),
          RunId("2") -> WorkflowArtifactRef.unsafe[MaskImageResource](mask2.toUri.toString)
        ))
      )

      val opened = FirstLevelUnitSource.open(unit).toOption.get
      val block = opened.source.readBlock(
        DataSelection(
          time = TimepointSelection.indices(3, 0),
          voxels = VoxelSelection.All
        )
      ).toOption.get

      assertEquals(opened.source.voxelDomain.indices, Vector(0, 2))
      assertEquals(opened.source.blockLengths, Vector(2, 2))
      assertEquals(block.timepoints, Vector(3, 0))
      assertEquals(block.voxelIndices, Vector(0, 2))
      assertMatrixEquals(block.data, Vector(Vector(104.0, 106.0), Vector(0.0, 2.0)))
    }
  }

  private def runInput(id: String, path: Path): RunInput =
    RunInput.unsafe(
      id = RunId(id),
      repetitionTime = RepetitionTime.unsafe(2.0),
      timepoints = 2,
      bold = WorkflowArtifactRef.unsafe[BoldImageResource](path.toUri.toString),
      events = WorkflowArtifactRef.unsafe[EventsTableResource](path.resolveSibling(s"events-$id.tsv").toUri.toString)
    )

  private def writeBold(path: Path, space: NeuroSpace, offset: Double): Path =
    val values = PrimitiveBuffers.fromArray(Array.tabulate(8)(index => offset + index.toDouble))
    val seriesSpace = space.addDim(2, Some(Axis.Time))
    Nifti.writeVec(path, NeuroVec.fromLinear(values, seriesSpace, "bold"))

  private def writeMask(path: Path, space: NeuroSpace, values: Array[Double]): Path =
    Nifti.writeVol(path, NeuroVol.fromLinear(PrimitiveBuffers.fromArray(values), space, "mask"))

  private def withFixture[A](body: Path => A): A =
    val root = Files.createTempDirectory("scalafim-unit-source-")
    try body(root)
    finally
      val files = Files.walk(root)
      try files.iterator().asScala.toVector.reverse.foreach(Files.deleteIfExists)
      finally files.close()

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
