package scalafim.fmri.fit

import scalafim.dataset.{DataSelection, DatasetError, DatasetId, DatasetSeriesReader, FmriDataset, FmriSeries, SynchronousFmriDataset}
import scalafim.dataset.io.MatrixFileDatasetBackend
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitEngine, FitPlan, FmriModel}
import scalafim.image.SampleSpaces

import java.nio.charset.StandardCharsets
import java.nio.file.Files

class MatrixFileTraceDiagnosticsSuite extends munit.FunSuite:
  test("file-backed fit capture reads once and traces its immutable selected snapshot"):
    val path = Files.createTempFile("scalafim-trace-", ".csv")
    val x = Vector(0.0, 1.0, 2.0, 3.0, 0.5, 1.5, 2.5, 3.5)
    val noise = Vector(0.2, -0.3, 0.1, -0.2, 0.4, -0.1, 0.2, -0.3)
    val lines = x.indices.map(row => s"${1.5 * x(row) + 0.8 + noise(row)},${-x(row) + 2.0 - noise(row)}").mkString("\n")
    Files.writeString(path, lines + "\n", StandardCharsets.UTF_8)
    try
      val frame = SamplingFrame(blockLens = Vector(4, 4), tr = Vector(1.0, 1.0))
      val dataset = FmriDataset.unsafe(
        MatrixFileDatasetBackend(DatasetId("trace-file"), path, SampleSpaces(Vector(2, 1, 1))), frame)
      val event = EventModel(Vector.empty, frame, Mat.fromRows(x.map(v => Vector(v))),
        Vector("task"), Vector(0 -> 1), Map("task" -> Vector(0)))
      val model = FmriModel(event, BaselineModel.build(samplingFrame = frame, basis = BaselineBasis.Constant, intercept = Intercept.Global), dataset)
      val plan = FitPlan(model, FitEngine.OrdinaryLeastSquares)
      val source = SynchronousFmriDataset.readerFor(dataset).fold(e => fail(e.message), identity)
      var reads = 0
      val reader = new DatasetSeriesReader:
        val dataset: FmriDataset = model.dataset
        def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
          reads += 1
          source.seriesEither(selection)
      val fit = FitPlanExecutor.fitWithTraceCapture(reader, plan, DataSelection.All, 8, 2)
        .fold(e => fail(e.toString), identity)
      assertEquals(reads, 1)
      val trace = fit.trace(FitTraceRequest(fit.traceIdentity.get, 1, (0 until 8).toVector,
        FitTraceSpace.Original, 1)).fold(e => fail(e.toString), identity)
      assertEquals(reads, 1)
      assertEquals(trace.identity.datasetId, DatasetId("trace-file"))
      assertEquals(trace.runIndices, Vector(0, 0, 0, 0, 1, 1, 1, 1))
      trace.rows.foreach(row => assertEqualsDouble(trace.observed(row), trace.fitted(row) + trace.residual(row), 1e-11))
    finally
      val _ = Files.deleteIfExists(path)
