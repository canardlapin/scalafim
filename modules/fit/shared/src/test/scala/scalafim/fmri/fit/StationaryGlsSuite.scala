package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.fit.GaleTestSyntax.*
import scalafim.dataset.{DataSelection, DatasetId, FmriDataset, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.fit.fixtures.FmriAr041GlsFixture as R
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArBiasCorrection, ArCensorTreatment, ArInitialization, AutocorrelationConfig, FitPlan, FitStrategy, FmriModel}
import scalafim.image.SampleSpaces

class StationaryGlsSuite extends munit.FunSuite:
  private def matrix(rows: Vector[Vector[Double]]): DMat = Matrix.tabulate(rows.length,rows.head.length)((r,c) => rows(r)(c))
  private val frame = SamplingFrame(Vector(150,150),Vector(1.0,1.0))
  private val dataset = FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("stationary-tail-oracle"),matrix(R.response),SampleSpaces(Vector(3,1,1))),frame)
  private val event = EventModel(Vector.empty,frame,Mat.fromRows(R.design.map(_.take(13))),Vector.tabulate(13)(i => s"event_$i"),Vector(0 -> 13),Map("events" -> (0 until 13).toVector))
  private val model = FmriModel(event,BaselineModel.build(frame,basis=BaselineBasis.Constant,intercept=Intercept.Runwise),dataset)
  private val reader = SynchronousFmriDataset.readerFor(dataset).toOption.get
  private def close(actual: DMat, expected: Vector[Vector[Double]]): Unit =
    assertEquals(actual.rows,expected.length)
    assertEquals(actual.cols,expected.head.length)
    expected.indices.foreach(r => expected(r).indices.foreach(c => assertEqualsDouble(actual(r,c),expected(r)(c),1e-8)))

  R.cases.foreach: c =>
    test(s"public tail-corrected stationary GLS matches R and bounded fitting: ${c.treatment}"):
      val treatment = if c.treatment == "restart" then ArCensorTreatment.RestartWhitening else ArCensorTreatment.EstimateOnly
      val ar = AutocorrelationConfig.withInitialization(ArInitialization.Stationary,order=2,voxelwise=true,
        censoredTimepoints=R.censor,biasCorrection=ArBiasCorrection.olsTailAnchored().toOption.get,censorTreatment=treatment).toOption.get
      val plan = FitPlan(model,FitStrategy.GeneralizedLeastSquares(ar))
      val dense = FitPlanExecutor.fitDense(plan).toOption.get
      val bounded = FitPlanExecutor.fitChunked(reader,plan,DataSelection.All,FitChunkingStrategy.unsafeByVoxelCount(1)).toOption.get.asInstanceOf[DenseFmriFitResult]
      Vector(dense,bounded).foreach: actual =>
        close(actual.coefficients.value,c.coefficients)
        close(actual.standardErrors.value,c.se)
        actual.residualVariance.toVector.zip(c.variance).foreach((a,e) => assertEqualsDouble(a,e,1e-8))
        assertEquals(actual.residualDegreesOfFreedom.value,285)
        (0 until 3).foreach(v => close(actual.coefficientCovariance.unsafeMatrixForVoxelPosition(v),c.covariance(v)))
      assertEquals(dense.autocorrelation,bounded.autocorrelation)
      if treatment == ArCensorTreatment.EstimateOnly then assertEquals(dense.autocorrelation.get.whitening.segments.length,2)
      else assert(dense.autocorrelation.get.whitening.segments.length > 2)

  test("estimation-only continuity refuses omitted source timepoints and still excludes lag-estimation rows"):
    val partitions = RunPartition.fromSamplingFrame(frame,(0 until 300).toVector)
    val layout = Gls.noiseEstimationLayout(partitions,R.censor,ArCensorTreatment.EstimateOnly).toOption.get
    assertEquals(layout.whiteningSegments.length,2)
    assertEquals(layout.excludedRows,R.censor.sorted)
    val selected = RunPartition.fromSamplingFrame(frame,Vector(0,1,3,4,150,151,152))
    assert(Gls.noiseEstimationLayout(selected,Vector.empty,ArCensorTreatment.EstimateOnly).isLeft)

  test("pooled tail and continuity preparation preserves diagnostics through artifact restoration"):
    val ar = AutocorrelationConfig.withInitialization(ArInitialization.Stationary,order=2,
      censoredTimepoints=R.censor,biasCorrection=ArBiasCorrection.olsTailAnchored().toOption.get,
      censorTreatment=ArCensorTreatment.EstimateOnly).toOption.get
    val plan = FitPlan(model,FitStrategy.GeneralizedLeastSquares(ar))
    val reference = FitWorkReference.unsafe("tail-continuity","plan-v1","source-v1")
    val descriptor = FitWorkDescriptor.compile(reference,plan,ChunkSize.unsafe(1)).toOption.get
    def resolver(recipe: FitPlan): FitWorkResolver = new FitWorkResolver:
      def resolve(request: FitWorkReference): Either[FitError,ResolvedFitWork] =
        if request == reference then Right(ResolvedFitWork(request,recipe,reader))
        else Left(FitError.WorkBindingMismatch("reference differs"))
    val artifact = PreparedGlsArtifact.prepare(descriptor,resolver(plan)).toOption.get
    assert(artifact.encode.contains("prepared-gls-v3"))
    assert(artifact.units.head.corrections.exists(_.isInstanceOf[scalafim.fmri.ar.RunCorrection.AppliedWithTailAnchor]))
    val decoded = PreparedGlsArtifact.decode(artifact.encode).toOption.get
    assertEquals(decoded.encode,artifact.encode)
    val restored = decoded.restore(resolver(plan)).toOption.get.fit().toOption.get.asInstanceOf[DenseFmriFitResult]
    val dense = FitPlanExecutor.fitDense(plan).toOption.get
    close(restored.coefficients.value,dense.coefficients.value.valuesRowMajor.grouped(3).map(_.toVector).toVector)
    assertEquals(restored.autocorrelation,dense.autocorrelation)
    val incompatible = FitPlan(model,FitStrategy.GeneralizedLeastSquares(AutocorrelationConfig.fromLegacy(ar.toLegacy.copy(censorTreatment=ArCensorTreatment.RestartWhitening)).toOption.get))
    assert(decoded.restore(resolver(incompatible)).isLeft)
