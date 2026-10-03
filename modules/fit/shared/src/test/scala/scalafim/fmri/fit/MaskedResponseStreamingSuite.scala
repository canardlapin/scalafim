package scalafim.fmri.fit

import scalafim.fmri.fit.GaleTestSyntax.*

import scalafim.dataset.{DataSelection, DatasetError, DatasetId, DatasetSeriesReader, FmriDataset, FmriSeries, IndexSelection, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitConfig, FitEngine, FitPlan, FmriModel, MissingDataPolicy}
import scalafim.image.SampleSpaces

import scala.concurrent.ExecutionContext.Implicits.global

class MaskedResponseStreamingSuite extends munit.FunSuite:

  private val selection = DataSelection(voxels = IndexSelection.indices(3, 0, 2, 1))
  private val chunking = FitChunkingStrategy.unsafeByVoxelCount(2)

  test("discovery rejects a reader returning a different shape even when every voxel is excluded") {
    val sourceDataset = model.dataset
    val reader = new DatasetSeriesReader:
      def dataset: FmriDataset = sourceDataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        selection.resolveEither(dataset.shape, dataset.voxelDomain).flatMap { resolved =>
          FmriSeries.fromIntIndices(
            gale.linalg.Matrix.tabulate(resolved.timepoints.length, resolved.voxels.length)((_, _) => Double.NaN),
            resolved.voxels, resolved.timepoints,
            scalafim.dataset.DatasetShape.unsafe(SampleSpaces(Vector(5, 1, 1)), 8)
          )
        }
    val result = MaskedResponsePlanner.discover(reader, plan, selection, chunking)
    assert(result.left.toOption.exists {
      case FitError.InvalidFitAxis("fit chunk response", _) => true
      case _ => false
    })
  }

  test("chunked row-omission discovery stays bounded and retains global interleaved patterns") {
    val expected = patterned(FitPlanExecutor.fit(plan, selection))
    val actual = patterned(FitPlanExecutor.fitChunked(boundedReader, plan, selection, chunking))

    assertEquals(actual.voxelIndices, Vector(3, 0, 2, 1))
    assertEquals(actual.patternResults.map(_.pattern.sourceVoxels), Vector(Vector(3), Vector(0, 2), Vector(1)))
    assertEquals(actual.patternResults.map(_.pattern.rows), Vector(Vector(0, 1, 2, 3, 4, 5, 6, 7), Vector(0, 2, 3, 4, 5, 6, 7), Vector(0, 1, 2, 4, 5, 6, 7)))
    assertPatternedClose(actual, expected)
  }

  test("parallel chunked row-omission discovery preserves contrasts and covariance") {
    val expected = patterned(FitPlanExecutor.fit(plan, selection))
    FitPlanExecutor
      .fitChunkedFuture(boundedReader, plan, selection, chunking, FitParallelism.unsafe(2))
      .map { result =>
        val actual = patterned(result)
        assertPatternedClose(actual, expected)
        val contrast = TContrast("task", Map("task" -> 1.0))
        val actualStatistics = contrast.evaluate(actual).fold(error => fail(error.message), _.patternResults.map(_._2.statistics.toVector))
        val expectedStatistics = contrast.evaluate(expected).fold(error => fail(error.message), _.patternResults.map(_._2.statistics.toVector))
        assertEquals(actualStatistics.length, expectedStatistics.length)
        actualStatistics.zip(expectedStatistics).foreach { (left, right) =>
          assertVectorClose(left, right)
        }
      }
  }

  private def patterned(result: Either[FitError, FmriFitResult]): PatternedFmriFitResult =
    result.fold(error => fail(error.message), {
      case value: PatternedFmriFitResult => value
      case other => fail(s"expected patterned result, got $other")
    })

  private def boundedReader: DatasetSeriesReader =
    val underlying = SynchronousFmriDataset.readerFor(model.dataset).toOption.get
    new DatasetSeriesReader:
      def dataset: FmriDataset = underlying.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        selection.resolveEither(dataset.shape, dataset.voxelDomain).flatMap { resolved =>
          if resolved.voxels.size > 2 then
            Left(DatasetError.StorageFailure(s"streaming reader refuses ${resolved.voxels.size} voxels"))
          else underlying.seriesEither(selection)
        }

  private def assertPatternedClose(actual: PatternedFmriFitResult, expected: PatternedFmriFitResult): Unit =
    assertEquals(actual.voxelIndices, expected.voxelIndices)
    assertEquals(actual.patternResults.map(_.pattern), expected.patternResults.map(_.pattern))
    actual.voxelIndices.foreach { voxel =>
      val left = actual.resultForVoxel(voxel).get.asInstanceOf[DenseFmriFitResult]
      val right = expected.resultForVoxel(voxel).get.asInstanceOf[DenseFmriFitResult]
      assertMatrixClose(left.coefficients.value, right.coefficients.value)
      assertMatrixClose(left.normalizedCovariance, right.normalizedCovariance)
      assertMatrixClose(left.coefficientCovariance.matrixForVoxelPosition(0).toOption.get,
        right.coefficientCovariance.matrixForVoxelPosition(0).toOption.get)
    }

  private def assertMatrixClose(left: gale.linalg.DMat, right: gale.linalg.DMat): Unit =
    assertEquals(left.rows, right.rows)
    assertEquals(left.cols, right.cols)
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        assertEqualsDouble(left(row, col), right(row, col), 1e-12)
        col += 1
      row += 1

  private def assertVectorClose(left: Vector[Double], right: Vector[Double]): Unit =
    assertEquals(left.length, right.length)
    left.zip(right).foreach { (actual, expected) =>
      assertEqualsDouble(actual, expected, 1e-12)
    }

  private lazy val model: FmriModel =
    val sampling = SamplingFrame(blockLens = Seq(8), tr = Seq(1.0))
    val event = EventModel(
      terms = Vector.empty,
      samplingFrame = sampling,
      designMatrix = Mat.fromRows(Vector.tabulate(8)(i => Vector(i.toDouble))),
      columnNames = Vector("task"),
      termSpans = Vector(0 -> 1),
      colIndices = Map("task" -> Vector(0))
    )
    val baseline = BaselineModel.build(
      samplingFrame = sampling,
      basis = BaselineBasis.Constant,
      intercept = Intercept.Global
    )
    val rows = Vector.tabulate(8) { row =>
      val x = row.toDouble
      def noise(voxel: Int): Double = 0.03 * math.sin((row + 1).toDouble * (voxel + 2).toDouble)
      Vector(
        if row == 1 then Double.NaN else 2.0 + 0.5 * x + noise(0),
        if row == 3 then Double.NaN else 1.0 - 0.25 * x + noise(1),
        if row == 1 then Double.NaN else -1.0 + 0.75 * x + noise(2),
        4.0 - 0.2 * x + noise(3)
      )
    }
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        DatasetId("masked-response-streaming"),
        GaleTestMatrix.fromRows(rows),
        SampleSpaces(Vector(4, 1, 1))
      ),
      samplingFrame = sampling
    )
    FmriModel(event, baseline, dataset)

  private lazy val plan = FitPlan(
    model,
    engine = FitEngine.OrdinaryLeastSquares,
    config = FitConfig(missingData = MissingDataPolicy.OmitRowsPerVoxel)
  )
