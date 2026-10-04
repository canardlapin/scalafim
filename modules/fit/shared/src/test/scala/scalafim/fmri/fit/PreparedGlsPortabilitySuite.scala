package scalafim.fmri.fit

import scalafim.dataset.{DataSelection, DatasetError, DatasetSeriesReader, FmriSeries, DatasetId, FmriDataset, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.design.{DesignDiagnostic, DesignDiagnosticKind, DesignSchema}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig, FitEngine, FitPlan, FmriModel}
import scalafim.image.SampleSpaces

/** Fixed rational source/design: foreign-platform artifacts must restore against
  * identical scientific inputs, without assuming identical AR-estimation rounding.
  */
class PreparedGlsPortabilitySuite extends munit.FunSuite:
  private val frame = SamplingFrame(blockLens = Seq(6, 6), tr = Seq(1.0))
  private val task = Vector(0.0, 1.0, 0.0, -1.0, 1.0, -1.0)
  private val rawEvent = EventModel(Vector.empty, frame,
    Mat.fromRows(Vector.tabulate(12)(r => Vector(task(r % 6)))),
    Vector("task"), Vector(0 -> 1), Map("task" -> Vector(0)))
  // Default Double formatting differs on JVM and JS; free-text audit messages
  // must not become the portable numeric/structural preparation identity.
  private val schema = rawEvent.designSchema
  private val portableSchema = DesignSchema.validated(rawEvent.designMatrix, schema.rows, schema.columns,
    schema.audit.copy(diagnostics = Vector(DesignDiagnostic(DesignDiagnosticKind.OnsetOutOfBounds,
      None, s"onset ${-1e-12} outside the run")))).toOption.get
  private val event = rawEvent.copy(compiledSchema = Some(portableSchema))
  private val baseline = BaselineModel.build(frame, basis = BaselineBasis.Constant, intercept = Intercept.Global)
  private val response = Vector.tabulate(12) { row =>
    Vector.tabulate(3) { voxel =>
      2.0 + task(row % 6) * (voxel + 1) + ((row * 7 + voxel * 3) % 11 - 5) * 0.125
    }
  }
  private val dataset = FmriDataset.unsafe(
    InMemoryDatasetBackend(DatasetId("portable-gls"), GaleTestMatrix.fromRows(response), SampleSpaces(Vector(3, 1, 1))), frame)
  private val plan = FitPlan(FmriModel(event, baseline, dataset), FitEngine.GeneralizedLeastSquares,
    FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), iterations = 2)))
  private val reference = FitWorkReference.unsafe("portable","rational-plan-v1", "rational-source-v1")
  private var responseReads = 0
  private val resolver = new FitWorkResolver:
    def resolve(request: FitWorkReference): Either[FitError, ResolvedFitWork] =
      if request != reference then Left(FitError.InvalidFitAxis("fixture", "wrong reference"))
      else SynchronousFmriDataset.readerFor(dataset).left.map(e => FitError.InvalidFitAxis("fixture", e.message))
        .map { underlying =>
          val reader = new DatasetSeriesReader:
            def dataset: FmriDataset = underlying.dataset
            def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
              responseReads += 1
              underlying.seriesEither(selection)
          ResolvedFitWork(reference, plan, reader)
        }

  test("foreign-platform completed GLS artifact restores and fits") {
    Vector(jvmArtifact, jsArtifact).foreach { encoded =>
      val artifact = PreparedGlsArtifact.decode(encoded).fold(e => fail(e.message), identity)
      assertEquals(artifact.encode, encoded)
      val before = responseReads
      val work = artifact.restore(resolver).fold(e => fail(e.message), identity)
      assertEquals(responseReads, before)
      val restored = work.fit().fold(e => fail(e.message), identity).asInstanceOf[DenseFmriFitResult]
      assertEquals(responseReads, before + 2)
      val expected = FitPlanExecutor.fit(plan).toOption.get.asInstanceOf[DenseFmriFitResult]
      assertEquals(restored.voxelIndices, expected.voxelIndices)
      assertEquals(restored.autocorrelation.map(_.whitening), expected.autocorrelation.map(_.whitening))
      for row <- 0 until restored.coefficients.predictors; column <- 0 until restored.coefficients.voxels do
        assertEqualsDouble(restored.coefficients.value(row, column), expected.coefficients.value(row, column), 1e-12)
        assertEqualsDouble(restored.standardErrors.value(row, column), expected.standardErrors.value(row, column), 1e-12)
    }
  }

  // Captured from actual JVM and Scala.js producers (focused-03.log, 2026-10-02).
  // Both fixtures are consumed on both platforms; never regenerate from the decoder.
  private val jvmArtifact = "15:prepared-gls-v1185:11:fit-work-v18:portable16:rational-plan-v118:rational-source-v112:portable-gls23:GeneralizedLeastSquares15:GlobalReduction21:PooledAutocorrelation25:0,1,2,3,4,5,6,7,8,9,10,115:0,1,21:26:Shared1218:19:pooled-gls-noise-v116:SharedAcrossRuns5:Error320:rows=12;blocks=1,1,1,1,1,1,2,2,2,2,2,2;times=4602678819172646912,4609434218613702656,4612811918334230528,4615063718147915776,4616752568008179712,4617878467915022336,4619004367821864960,4620130267728707584,4620974692658839552,4621537642612260864,4622100592565682176,4622663542519103488;selected=1,2,3,4,5,6,7,8,9,10,11,12266:93:84:1|legacy|Event|0|task|legacy|Event|task|1|hrf-scale=as-convolved:46071824188000174084:task166:146:2|drift|drift|basis(constant|1|basis-01)|global|ordinal=1|drift|drift|basis(constant|1|basis-01)|global|hrf-scale=as-convolved:460718241880001740813:base_constant572:6:shared399:2:121:21:016:3ff000000000000016:3ff000000000000016:3ff00000000000001:016:3ff000000000000016:bff000000000000016:3ff000000000000016:3ff000000000000016:3ff000000000000016:bff000000000000016:3ff00000000000001:016:3ff000000000000016:3ff000000000000016:3ff00000000000001:016:3ff000000000000016:bff000000000000016:3ff000000000000016:3ff000000000000016:3ff000000000000016:bff000000000000016:3ff00000000000001:11:25:false5:false4:true0:55:0:0,1,2,3,4,5:0,1,2,3,4,5;1:6,7,8,9,10,11:6,7,8,9,10,110:70:15:QrRankRevealing16:strict-full-rank11:scale-aware16:3d719799812dea115:0,1,286:83:6:shared3:Run9:exact-ar115:5:0,6,06:6,12,138:16:bfd867b48bd649b916:bfdb25a3e62c3d25"
  private val jsArtifact = "15:prepared-gls-v1185:11:fit-work-v18:portable16:rational-plan-v118:rational-source-v112:portable-gls23:GeneralizedLeastSquares15:GlobalReduction21:PooledAutocorrelation25:0,1,2,3,4,5,6,7,8,9,10,115:0,1,21:26:Shared1218:19:pooled-gls-noise-v116:SharedAcrossRuns5:Error320:rows=12;blocks=1,1,1,1,1,1,2,2,2,2,2,2;times=4602678819172646912,4609434218613702656,4612811918334230528,4615063718147915776,4616752568008179712,4617878467915022336,4619004367821864960,4620130267728707584,4620974692658839552,4621537642612260864,4622100592565682176,4622663542519103488;selected=1,2,3,4,5,6,7,8,9,10,11,12266:93:84:1|legacy|Event|0|task|legacy|Event|task|1|hrf-scale=as-convolved:46071824188000174084:task166:146:2|drift|drift|basis(constant|1|basis-01)|global|ordinal=1|drift|drift|basis(constant|1|basis-01)|global|hrf-scale=as-convolved:460718241880001740813:base_constant572:6:shared399:2:121:21:016:3ff000000000000016:3ff000000000000016:3ff00000000000001:016:3ff000000000000016:bff000000000000016:3ff000000000000016:3ff000000000000016:3ff000000000000016:bff000000000000016:3ff00000000000001:016:3ff000000000000016:3ff000000000000016:3ff00000000000001:016:3ff000000000000016:bff000000000000016:3ff000000000000016:3ff000000000000016:3ff000000000000016:bff000000000000016:3ff00000000000001:11:25:false5:false4:true0:55:0:0,1,2,3,4,5:0,1,2,3,4,5;1:6,7,8,9,10,11:6,7,8,9,10,110:70:15:QrRankRevealing16:strict-full-rank11:scale-aware16:3d719799812dea115:0,1,286:83:6:shared3:Run9:exact-ar115:5:0,6,06:6,12,138:16:bfd867b48bd649bc16:bfdb25a3e62c3d1a"
