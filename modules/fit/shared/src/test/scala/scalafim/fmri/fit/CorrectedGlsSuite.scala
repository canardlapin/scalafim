package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.dataset.{DataSelection, DatasetId, FmriDataset, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.ar.RunCorrection
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.fit.fixtures.FmriArCorrectedGlsFixture as R
import scalafim.fmri.fit.GaleTestSyntax.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArBiasCorrection, ArOptions, ArStructure, AutocorrelationConfig, FitConfig, FitEngine, FitPlan, FitStrategy, FmriModel}
import scalafim.image.SampleSpaces

class CorrectedGlsSuite extends munit.FunSuite:
  private def matrix(rows: Vector[Vector[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((row, col) => rows(row)(col))
  private val frame = SamplingFrame(blockLens = R.runLengths, tr = Vector(1.0, 1.0))
  private val dataset = FmriDataset.unsafe(
    InMemoryDatasetBackend(DatasetId("corrected-gls-oracle"), matrix(R.response), SampleSpaces(Vector(4, 1, 1))), frame)
  private val event = EventModel(Vector.empty, frame, Mat.fromRows(R.design.map(row => Vector(row.head))),
    Vector("task"), Vector(0 -> 1), Map("task" -> Vector(0)))
  private val model = FmriModel(event,
    BaselineModel.build(frame, basis = BaselineBasis.Constant, intercept = Intercept.Runwise), dataset)
  private val reader = SynchronousFmriDataset.readerFor(dataset).toOption.get
  private val tolerance = 1e-10
  private def checked[A](result: Either[FitError, A]): A = result.fold(error => fail(error.message), identity)
  private def plan(c: R.FitCase): FitPlan =
    FitPlan(model, engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(autocorrelation = ArOptions(ArStructure.Ar(c.order), global = c.global,
        voxelwise = c.voxelwise, censoredTimepoints = R.censoredTimepoints, biasCorrection = ArBiasCorrection.Ols)))
  private def assertMatrix(actual: DMat, expected: Vector[Vector[Double]]): Unit =
    assertEquals(actual.rows, expected.length)
    assertEquals(actual.cols, expected.head.length)
    expected.indices.foreach(row => expected(row).indices.foreach { col =>
      assertEqualsDouble(actual(row, col), expected(row)(col), tolerance)
    })
  private def assertVector(actual: Vector[Double], expected: Vector[Double]): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).foreach((a, e) => assertEqualsDouble(a, e, tolerance))

  R.cases.foreach { c =>
    test(s"public corrected AR(${c.order}) GLS matches the pinned R pipeline: global=${c.global}, voxelwise=${c.voxelwise}") {
      val actual = checked(FitPlanExecutor.fit(plan(c))).asInstanceOf[DenseFmriFitResult]
      assertMatrix(actual.coefficients.value, c.coefficients)
      assertVector(actual.residualVariance.toVector, c.variance)
      assertMatrix(actual.standardErrors.value, c.standardErrors)
      assertEquals(actual.residualDegreesOfFreedom.value, c.df)
      val diagnostics = actual.autocorrelation.get
      assertEquals(diagnostics.biasCorrection, ArBiasCorrection.Ols)
      diagnostics.runs.zipWithIndex.foreach { (run, index) =>
        if c.voxelwise then
          run.voxelwiseCoefficients.zipWithIndex.foreach { (phi, voxel) => assertVector(phi, c.phi(voxel)(index)) }
          assert(run.voxelwiseCorrections.forall(_.isInstanceOf[RunCorrection.Applied]))
        else
          assertVector(run.phi, c.phi.head(if c.global then 0 else index))
          assert(run.correction.exists(_.isInstanceOf[RunCorrection.Applied]))
      }
      (0 until actual.voxels).foreach { voxel =>
        assertMatrix(actual.coefficientCovariance.unsafeMatrixForVoxelPosition(voxel), c.covariance(if c.voxelwise then voxel else 0))
      }
      assert(c.phi.flatten.flatten.zip(c.rawPhi.flatten.flatten).exists((a, b) => math.abs(a - b) > 1e-4))
    }
  }

  R.runwiseCases.foreach { c =>
    test(s"public corrected runwise AR(${c.order}) GLS matches run-local R projections") {
      val runwiseModel = FmriModel(event,
        BaselineModel.build(frame, basis = BaselineBasis.Constant, intercept = Intercept.Global), dataset)
      val recipe = FitPlan(runwiseModel, FitStrategy.RunwiseGeneralizedLeastSquares(
        AutocorrelationConfig.unsafe(order = c.order, censoredTimepoints = R.censoredTimepoints,
          biasCorrection = ArBiasCorrection.Ols)))
      val dense = checked(FitPlanExecutor.fit(recipe)).asInstanceOf[RunwiseFmriFitResult]
      val bounded = checked(FitPlanExecutor.fitChunked(reader, recipe, DataSelection.All,
        FitChunkingStrategy.unsafeByVoxelCount(2))).asInstanceOf[RunwiseFmriFitResult]
      Vector(dense, bounded).foreach { result =>
        result.runs.zip(c.runs).foreach { (actual, expected) =>
          assertMatrix(actual.coefficients.value, expected.coefficients)
          assertMatrix(actual.normalizedCovariance, expected.covariance)
          assertVector(actual.residualVariance.toVector, expected.variance)
          assertEquals(actual.residualDegreesOfFreedom.value, expected.df)
          assertVector(actual.autocorrelation.get.runs.head.phi, expected.phi)
          assert(actual.autocorrelation.get.runs.head.correction.exists(_.isInstanceOf[RunCorrection.Applied]))
        }
      }
      assertEquals(bounded.runs.map(_.autocorrelation), dense.runs.map(_.autocorrelation))
    }
  }

  test("corrected pooled and voxelwise public fits are invariant to spatial block size") {
    R.cases.foreach { c =>
      val recipe = plan(c)
      val dense = checked(FitPlanExecutor.fit(recipe)).asInstanceOf[DenseFmriFitResult]
      (1 to 4).foreach { size =>
        val actual = checked(FitPlanExecutor.fitChunked(reader, recipe, DataSelection.All,
          FitChunkingStrategy.unsafeByVoxelCount(size))).asInstanceOf[DenseFmriFitResult]
        assertMatrix(actual.coefficients.value, c.coefficients)
        assertVector(actual.residualVariance.toVector, dense.residualVariance.toVector)
        assertEquals(actual.autocorrelation, dense.autocorrelation)
      }
    }
  }

  test("corrected prepared GLS artifacts restore outcomes and refuse a different correction policy") {
    val recipe = plan(R.cases.find(c => c.global && c.order == 2).get)
    val reference = FitWorkReference.unsafe("corrected-gls", "recipe-v1", "source-v1")
    def resolver(bound: FitPlan): FitWorkResolver = new FitWorkResolver:
      def resolve(request: FitWorkReference): Either[FitError, ResolvedFitWork] =
        if request == reference then Right(ResolvedFitWork(reference, bound, reader))
        else Left(FitError.WorkBindingMismatch("unexpected reference"))
    val descriptor = checked(FitWorkDescriptor.compile(reference, recipe, ChunkSize.unsafe(2)))
    val artifact = checked(PreparedGlsArtifact.prepare(descriptor, resolver(recipe)))
    assert(artifact.units.head.corrections.forall(_.isInstanceOf[RunCorrection.Applied]))
    val decoded = checked(PreparedGlsArtifact.decode(artifact.encode))
    assertEquals(decoded.encode, artifact.encode)
    assertEquals(decoded.units.head.corrections, artifact.units.head.corrections)
    val restored = checked(checked(decoded.restore(resolver(recipe))).fit()).asInstanceOf[DenseFmriFitResult]
    val expected = checked(FitPlanExecutor.fit(recipe)).asInstanceOf[DenseFmriFitResult]
    assertEquals(restored.autocorrelation, expected.autocorrelation)
    assertMatrix(restored.coefficients.value, R.cases.find(c => c.global && c.order == 2).get.coefficients)
    val raw = FitPlan(model, engine = FitEngine.GeneralizedLeastSquares,
      config = recipe.config.copy(autocorrelation = recipe.config.autocorrelation.copy(biasCorrection = ArBiasCorrection.Raw)))
    assert(decoded.restore(resolver(raw)).isLeft)
    val otherBudget = FitPlan(model, engine = FitEngine.GeneralizedLeastSquares,
      config = recipe.config.copy(autocorrelation = recipe.config.autocorrelation.copy(
        biasCorrection = ArBiasCorrection.olsDesign(12).toOption.get)))
    assert(decoded.restore(resolver(otherBudget)).isLeft)
  }

  Vector(1, 2).foreach { order =>
    Vector((true, false), (false, false), (false, true)).foreach { (global, voxelwise) =>
      test(s"fully censored run preserves corrected AR($order) diagnostics: global=$global, voxelwise=$voxelwise") {
        val options = ArOptions(ArStructure.Ar(order), global = global, voxelwise = voxelwise,
          censoredTimepoints = (R.runLengths.head until R.runLengths.sum).toVector, biasCorrection = ArBiasCorrection.Ols)
        val partitions = RunPartition.fromSamplingFrame(frame, (0 until R.runLengths.sum).toVector)
        val prepared = checked(Gls.prepare(DesignMatrix.unsafe(matrix(R.design)),
          ResponseBlock.unsafe(matrix(R.response)), partitions, options))
        val nativePlans = prepared.whitening match
          case GlsWhitening.Shared(plan, _) => Vector(plan)
          case GlsWhitening.Voxelwise(plans, _) => plans
        if !global then nativePlans.foreach { plan =>
          val segment = plan.segments.find(_.runIndex == 1).get
          assertEquals(plan.coefficientsFor(segment).phi, Vector.empty[Double])
          assertEquals(plan.coefficientsFor(segment).arOrder, 0)
        }

        val recipe = FitPlan(model, engine = FitEngine.GeneralizedLeastSquares, config = FitConfig(autocorrelation = options))
        val dense = checked(FitPlanExecutor.fit(recipe)).asInstanceOf[DenseFmriFitResult]
        val censored = dense.autocorrelation.get.runs.last
        if voxelwise then
          assertEquals(censored.voxelwiseCoefficients.length, R.response.head.length)
          assertEquals(censored.voxelwiseCorrections.length, R.response.head.length)
          censored.voxelwiseCoefficients.foreach(phi => assertVector(phi, Vector.fill(order)(0.0)))
          assert(censored.voxelwiseCorrections.forall(_.isInstanceOf[RunCorrection.NotAttempted]))
          assertVector(censored.phi, Vector.fill(order)(0.0))
        else
          assert(censored.correction.exists(_.isInstanceOf[RunCorrection.NotAttempted]))
          if !global then assertVector(censored.phi, Vector.fill(order)(0.0))
        assertEquals(dense.autocorrelation.get, prepared.diagnostics)

        Vector(1, 2, 4).foreach { size =>
          val bounded = checked(FitPlanExecutor.fitChunked(reader, recipe, DataSelection.All,
            FitChunkingStrategy.unsafeByVoxelCount(size))).asInstanceOf[DenseFmriFitResult]
          assertEquals(bounded.autocorrelation, dense.autocorrelation)
          assertMatrix(bounded.coefficients.value, Vector.tabulate(dense.coefficients.predictors)(row =>
            Vector.tabulate(dense.coefficients.voxels)(col => dense.coefficients(row, col))))
          assertVector(bounded.residualVariance.toVector, dense.residualVariance.toVector)
        }
      }
    }
  }
