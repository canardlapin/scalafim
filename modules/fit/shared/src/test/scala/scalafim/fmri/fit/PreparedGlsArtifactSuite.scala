package scalafim.fmri.fit

import scalafim.dataset.{DataSelection, DatasetError, DatasetEvents, DatasetId, DatasetSeriesReader, FmriDataset, FmriSeries, IndexSelection, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.fit.GaleTestSyntax.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArCoefficientSpec, ArOptions, ArStructure, AutocorrelationConfig, FitConfig, FitEngine, FitPlan, FitStrategy, FmriModel, FmriModelBuilder, MissingDataPolicy, ModelBuildSpec}
import scalafim.image.SampleSpaces
import gale.linalg.Matrix
import scalafim.fmri.ar.{ArmaCoefficients, NoisePooling, TimeSegment, WhiteningMethod, WhiteningPlan}

class PreparedGlsArtifactSuite extends munit.FunSuite:

  private val selection = DataSelection(
    time = IndexSelection.indices(0, 1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 14),
    voxels = IndexSelection.indices(2, 0, 3, 1)
  )
  private val blockSize = 2

  test("prepared shared GLS artifact encodes, restores without reads, and fits bounded chunks") {
    val descriptor = descriptorFor(sharedPlan)
    val preparationReader = boundedReader(sharedPlan.model.dataset)
    val artifact = PreparedGlsArtifact.prepare(descriptor, resolver(sharedPlan, preparationReader)).toOption.get
    assertEquals(preparationReader.reads, 4)
    val decoded = PreparedGlsArtifact.decode(artifact.encode).toOption.get
    assertEquals(decoded.encode, artifact.encode)

    val forbidden = denyingReader(sharedPlan.model.dataset)
    assert(decoded.restore(resolver(sharedPlan, forbidden)).isRight)
    assertEquals(forbidden.reads, 0)
    val restoredReader = boundedReader(sharedPlan.model.dataset)
    val restored = decoded.restore(resolver(sharedPlan, restoredReader)).toOption.get
    assertEquals(restoredReader.reads, 0)
    val restoredResult = restored.fit().toOption.get.asInstanceOf[DenseFmriFitResult]
    assertEquals(restoredReader.reads, 2)
    assert(restoredReader.axes.forall(_ == selectedTimepoints))

    val dense = FitPlanExecutor.fit(sharedPlan, selection).toOption.get.asInstanceOf[DenseFmriFitResult]
    val uninterrupted = FitPlanExecutor.fitChunked(boundedReader(sharedPlan.model.dataset), sharedPlan, selection,
      FitChunkingStrategy.unsafeByVoxelCount(blockSize)).toOption.get.asInstanceOf[DenseFmriFitResult]
    assertDenseClose(restoredResult, uninterrupted)
    assertDenseClose(restoredResult, dense)
    val contrast = TContrast("task", Map("task" -> 1.0))
    assertVectorClose(
      contrast.evaluate(restoredResult).toOption.get.statistics.toVector,
      contrast.evaluate(dense).toOption.get.statistics.toVector
    )
  }

  test("prepared GLS artifact codec rejects truncated, unknown-version, and nonfinite state") {
    val artifact = PreparedGlsArtifact.prepare(descriptorFor(sharedPlan), resolver(sharedPlan, boundedReader(sharedPlan.model.dataset))).toOption.get
    Vector(
      artifact.encode.dropRight(1),
      artifact.encode.replace("prepared-gls-v1", "prepared-gls-v2"),
      artifact.encode + "0:",
      replaceFirstWhiteningCoefficient(artifact.encode, "7ff8000000000000"),
      replaceFirstWhiteningCoefficient(artifact.encode, "7ff0000000000000"),
      replaceFirstWhiteningCoefficient(artifact.encode, "3ff0000000000000")
    ).foreach(value => assert(PreparedGlsArtifact.decode(value).isLeft, clues(value)))
  }

  test("prepared GLS artifact uses canonical IEEE-754 number encodings") {
    assertEquals(PreparedGlsArtifact.bits(0.0), "0")
    assertEquals(PreparedGlsArtifact.bits(-0.0), "8000000000000000")
    assertEquals(PreparedGlsArtifact.bits(1.0), "3ff0000000000000")
    assertEquals(PreparedGlsArtifact.bits(-0.25), "bfd0000000000000")
  }

  test("prepared artifact constructors reject invalid retained and source-run metadata") {
    val artifact = PreparedGlsArtifact.prepare(descriptorFor(sharedPlan),
      resolver(sharedPlan, boundedReader(model.dataset))).toOption.get
    intercept[IllegalArgumentException] {
      PreparedGlsUnit(Some(-1), artifact.units.head.whitening)
    }
    intercept[IllegalArgumentException] {
      new PreparedGlsArtifact(
        artifact.descriptor,
        artifact.scope,
        CompletedGlsNoise("test", Vector(2, 2), artifact.units)
      )
    }
  }

  test("prepared artifact preserves propagated exclusions and rejects changed final membership") {
    val original = SynchronousFmriDataset.readerFor(model.dataset).toOption.get
      .seriesEither(DataSelection.All).toOption.get
    val excludedDataset = FmriDataset.unsafe(
      InMemoryDatasetBackend(
        DatasetId("prepared-gls-exclusions"),
        Matrix.tabulate(16, 4) { (row, voxel) =>
          if row == 2 && Set(2, 0, 3).contains(voxel) then Double.NaN else original.data(row, voxel)
        },
        SampleSpaces(Vector(4, 1, 1))
      ),
      model.dataset.samplingFrame
    )
    val plan = FitPlan(
      model.copy(dataset = excludedDataset),
      engine = FitEngine.GeneralizedLeastSquares,
      config = sharedPlan.config.copy(missingData = MissingDataPolicy.Propagate)
    )
    val descriptor = descriptorFor(plan)
    val artifact = PreparedGlsArtifact.prepare(descriptor, resolver(plan, boundedReader(excludedDataset))).toOption.get
    val restored = artifact.restore(resolver(plan, boundedReader(excludedDataset))).toOption.get
    val actual = restored.fit().toOption.get.asInstanceOf[DenseFmriFitResult]
    val expected = FitPlanExecutor.fitChunked(boundedReader(excludedDataset), plan, selection,
      FitChunkingStrategy.unsafeByVoxelCount(blockSize)).toOption.get.asInstanceOf[DenseFmriFitResult]
    assertEquals(actual.voxelIndices, Vector(1))
    assertEquals(actual.fitExclusions, expected.fitExclusions)
    assertDenseClose(actual, expected)

    val stableArtifact = PreparedGlsArtifact.prepare(descriptorFor(propagatingPlan),
      resolver(propagatingPlan, boundedReader(propagatingPlan.model.dataset))).toOption.get
    val changed = stableArtifact.restore(resolver(propagatingPlan, membershipChangingReader(propagatingPlan.model.dataset)))
      .toOption.get.fit()
    assert(changed.left.toOption.exists {
      case FitError.ChunkFailed(_, FitError.PreparationReplayMismatch(detail)) =>
        detail.contains("membership changed")
      case _ => false
    })
  }

  test("prepared runwise GLS artifacts retain run-local results and scope is bound before reads") {
    val plan = FitPlan(projectedModel, FitStrategy.RunwiseGeneralizedLeastSquares(
      AutocorrelationConfig.unsafe(iterations = 2, coefficients = ArCoefficientSpec.Estimate)
    ))
    val descriptor = descriptorFor(plan)
    val preparedReader = boundedReader(projectedModel.dataset)
    val artifact = PreparedGlsArtifact.prepare(descriptor, resolver(plan, preparedReader)).toOption.get
    assertEquals(preparedReader.reads, 4)
    val restoredReader = boundedReader(projectedModel.dataset)
    val decoded = PreparedGlsArtifact.decode(artifact.encode).toOption.get
    val restored = decoded.restore(resolver(plan, restoredReader)).toOption.get
    assertEquals(restoredReader.reads, 0)
    val actual = restored.fit().toOption.get
      .asInstanceOf[RunwiseFmriFitResult]
    assertEquals(restoredReader.reads, 2)
    val expected = FitPlanExecutor.fitChunked(boundedReader(projectedModel.dataset), plan, selection,
      FitChunkingStrategy.unsafeByVoxelCount(blockSize)).toOption.get.asInstanceOf[RunwiseFmriFitResult]
    assertEquals(actual.runs.map(_.projection), expected.runs.map(_.projection))
    actual.runs.zip(expected.runs).foreach { (left, right) =>
      assertMatrixClose(left.coefficients.value, right.coefficients.value)
      assertMatrixClose(left.standardErrors.value, right.standardErrors.value)
      assertMatrixClose(left.normalizedCovariance, right.normalizedCovariance)
      assertEquals(left.residualDegreesOfFreedom, right.residualDegreesOfFreedom)
      val leftAr = left.autocorrelation.getOrElse(fail("missing actual run AR diagnostics"))
      val rightAr = right.autocorrelation.getOrElse(fail("missing expected run AR diagnostics"))
      assertEquals(leftAr.whitening.segments, rightAr.whitening.segments)
      assertEquals(leftAr.whitening.pooling, rightAr.whitening.pooling)
      assertEquals(leftAr.runs.map(_.runIndex), rightAr.runs.map(_.runIndex))
      leftAr.runs.zip(rightAr.runs).foreach { (actualRun, expectedRun) =>
        assertVectorClose(actualRun.phi, expectedRun.phi)
      }
    }

    assert(actual.runs.forall(_.projection.nonEmpty))
    assert(actual.runs.forall(_.projection.exists(_.sourceColumnIndices.nonEmpty)))
    val denied = denyingReader(projectedModel.dataset)
    val sharedScope = FitPlan(projectedModel, FitEngine.GeneralizedLeastSquares, plan.config)
    val swapped = artifact.restore(resolver(sharedScope, denied))
    assert(swapped.left.toOption.exists {
      case FitError.PreparedArtifactInvalid(detail) => detail.endsWith("coefficient scope differs")
      case _ => false
    })
    assertEquals(denied.reads, 0)
  }

  test("prepared artifacts reject stale bindings and design drift before response reads") {
    val artifact = PreparedGlsArtifact.prepare(descriptorFor(sharedPlan),
      resolver(sharedPlan, boundedReader(model.dataset))).toOption.get
    val denied = denyingReader(model.dataset)
    val stale = new FitWorkResolver:
      def resolve(reference: FitWorkReference): Either[FitError, ResolvedFitWork] =
        Left(FitError.InvalidFitAxis("prepared GLS test", "stale source revision"))
    assert(artifact.restore(stale).isLeft)
    assertEquals(denied.reads, 0)

    val wrongReference = new FitWorkResolver:
      def resolve(reference: FitWorkReference): Either[FitError, ResolvedFitWork] =
        Right(ResolvedFitWork(FitWorkReference.unsafe("wrong", "plan-v1", "source-v1"), sharedPlan, denied))
    assert(artifact.restore(wrongReference).isLeft)
    assertEquals(denied.reads, 0)

    val drifted = FitPlan(
      model,
      engine = FitEngine.GeneralizedLeastSquares,
      config = sharedPlan.config.copy(autocorrelation = sharedPlan.config.autocorrelation.copy(censoredTimepoints = Vector(4)))
    )
    assert(artifact.restore(resolver(drifted, denied)).isLeft)
    assertEquals(denied.reads, 0)

    val differentDesign = FitPlan(
      model.copy(eventModel = model.eventModel.copy(
        designMatrix = Mat.fromRows(Vector.tabulate(16)(row => Vector(math.cos((row + 1).toDouble * 0.41))))
      )),
      engine = FitEngine.GeneralizedLeastSquares,
      config = sharedPlan.config
    )
    assert(artifact.restore(resolver(differentDesign, denied)).isLeft)
    assertEquals(denied.reads, 0)
    val renamed = FitPlan(model.copy(eventModel = model.eventModel.copy(columnNames = Vector("other-task"))),
      FitEngine.GeneralizedLeastSquares, sharedPlan.config)
    assert(artifact.restore(resolver(renamed, denied)).isLeft)
    assertEquals(denied.reads, 0)
  }

  test("completed state rejects incompatible whitening and population contracts before reads") {
    val artifact = PreparedGlsArtifact.prepare(descriptorFor(sharedPlan),
      resolver(sharedPlan, boundedReader(model.dataset))).toOption.get
    val unit = artifact.units.head
    val w = unit.whitening
    val altered = Vector(
      WhiteningPlan(w.coefficients, w.segments, w.pooling, !w.exactFirstAr1, WhiteningMethod.Estimated),
      WhiteningPlan(w.coefficients, Vector(TimeSegment(0, w.nTimepoints, 0)), w.pooling,
        w.exactFirstAr1, WhiteningMethod.Estimated),
      WhiteningPlan(Vector(ArmaCoefficients.ar(0.1, 0.05)), w.segments, NoisePooling.Global,
        w.exactFirstAr1, WhiteningMethod.Estimated)
    )
    val denied = denyingReader(model.dataset)
    altered.foreach { value =>
      val changed = new PreparedGlsArtifact(artifact.descriptor, artifact.scope,
        artifact.noise.copy(units = Vector(PreparedGlsUnit(None, value))))
      assert(changed.restore(resolver(sharedPlan, denied)).isLeft)
    }
    val missingVoxel = new PreparedGlsArtifact(artifact.descriptor, artifact.scope,
      artifact.noise.copy(retainedVoxelIndices = artifact.retainedVoxelIndices.tail))
    assert(missingVoxel.restore(resolver(sharedPlan, denied)).isLeft)
    assertEquals(denied.reads, 0)
  }

  test("artifact preparation rejects unsupported fixed AR without a response read") {
    val fixed = FitPlan(model, FitEngine.GeneralizedLeastSquares,
      FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), rho = Some(0.2))))
    val denied = denyingReader(model.dataset)
    assert(PreparedGlsArtifact.prepare(descriptorFor(fixed), resolver(fixed, denied)).isLeft)
    assertEquals(denied.reads, 0)
  }

  private def descriptorFor(plan: FitPlan): FitWorkDescriptor =
    FitWorkDescriptor.compile(
      FitWorkReference.unsafe("prepared-gls", "plan-v1", "source-v1"),
      plan,
      ChunkSize.unsafe(blockSize),
      selection
    ).toOption.get

  private def resolver(plan: FitPlan, reader: DatasetSeriesReader): FitWorkResolver =
    new FitWorkResolver:
      def resolve(reference: FitWorkReference): Either[FitError, ResolvedFitWork] =
        if reference == FitWorkReference.unsafe("prepared-gls", "plan-v1", "source-v1") then
          Right(ResolvedFitWork(reference, plan, reader))
        else Left(FitError.WorkBindingMismatch("unexpected reference"))

  private final class BoundedReader(val dataset: FmriDataset, underlying: DatasetSeriesReader) extends DatasetSeriesReader:
    var reads = 0
    var axes = Vector.empty[Vector[Int]]
    def seriesEither(request: DataSelection): Either[DatasetError, FmriSeries] =
      request.resolveEither(dataset.shape, dataset.voxelDomain).flatMap { resolved =>
        if resolved.voxels.length > blockSize then Left(DatasetError.StorageFailure("read exceeded block size"))
        else
          reads += 1
          axes :+= resolved.timepoints
          underlying.seriesEither(request)
      }

  private def boundedReader(dataset: FmriDataset): BoundedReader =
    new BoundedReader(dataset, SynchronousFmriDataset.readerFor(dataset).toOption.get)

  private def denyingReader(sourceDataset: FmriDataset): BoundedReader =
    new BoundedReader(sourceDataset, new DatasetSeriesReader:
      def dataset: FmriDataset = sourceDataset
      def seriesEither(request: DataSelection): Either[DatasetError, FmriSeries] =
        Left(DatasetError.StorageFailure("response reads are forbidden"))
    )

  private def membershipChangingReader(sourceDataset: FmriDataset): DatasetSeriesReader =
    val underlying = SynchronousFmriDataset.readerFor(sourceDataset).toOption.get
    new DatasetSeriesReader:
      def dataset: FmriDataset = sourceDataset
      def seriesEither(request: DataSelection): Either[DatasetError, FmriSeries] =
        underlying.seriesEither(request).flatMap { series =>
          val values = Array.ofDim[Double](series.nTimepoints * series.nVoxels)
          var row = 0
          while row < series.nTimepoints do
            var column = 0
            while column < series.nVoxels do
              values(row * series.nVoxels + column) =
                if column == 0 then Double.NaN else series.data(row, column)
              column += 1
            row += 1
          FmriSeries.make(
            Matrix.dense(series.nTimepoints, series.nVoxels, values.toIndexedSeq),
            series.voxelIndexValues, series.timepointIndices, series.shape, series.metadata
          )
        }

  private def replaceFirstWhiteningCoefficient(encoded: String, replacement: String): String =
    val fields = unframe(encoded)
    val units = unframe(fields(5))
    val unit = unframe(units.head)
    val coefficients = unframe(unit(4))
    val replacedUnit = frame(unit.updated(4, frame(coefficients.updated(0, replacement))))
    frame(fields.updated(5, frame(units.updated(0, replacedUnit))))

  private def unframe(encoded: String): Vector[String] =
    val fields = Vector.newBuilder[String]
    var offset = 0
    while offset < encoded.length do
      val colon = encoded.indexOf(':', offset)
      if colon <= offset then fail("invalid test framing")
      val length = encoded.substring(offset, colon).toInt
      val start = colon + 1
      if length < 0 || start + length > encoded.length then fail("invalid test framing")
      fields += encoded.substring(start, start + length)
      offset = start + length
    fields.result()

  private def frame(fields: Vector[String]): String =
    fields.map(value => s"${value.length}:$value").mkString

  private def assertDenseClose(actual: DenseFmriFitResult, expected: DenseFmriFitResult): Unit =
    assertEquals(actual.voxelIndices, expected.voxelIndices)
    assertEquals(actual.timepoints, expected.timepoints)
    assertEquals(actual.residualDegreesOfFreedom, expected.residualDegreesOfFreedom)
    assertEquals(actual.inferenceScope, expected.inferenceScope)
    assertEquals(actual.preparationProvenance, expected.preparationProvenance)
    assertMatrixClose(actual.coefficients.value, expected.coefficients.value)
    assertMatrixClose(actual.standardErrors.value, expected.standardErrors.value)
    assertMatrixClose(actual.normalizedCovariance, expected.normalizedCovariance)
    var voxel = 0
    while voxel < actual.voxels do
      assertMatrixClose(
        actual.coefficientCovariance.unsafeMatrixForVoxelPosition(voxel),
        expected.coefficientCovariance.unsafeMatrixForVoxelPosition(voxel)
      )
      voxel += 1
    assertVectorClose(
      actual.autocorrelation.getOrElse(fail("missing actual AR diagnostics")).runs.flatMap(_.phi),
      expected.autocorrelation.getOrElse(fail("missing expected AR diagnostics")).runs.flatMap(_.phi)
    )

  private def assertMatrixClose(left: gale.linalg.DMat, right: gale.linalg.DMat): Unit =
    assertEquals(left.rows, right.rows)
    assertEquals(left.cols, right.cols)
    var row = 0
    while row < left.rows do
      var column = 0
      while column < left.cols do
        assertEqualsDouble(left(row, column), right(row, column), 1e-9)
        column += 1
      row += 1

  private def assertVectorClose(left: Vector[Double], right: Vector[Double]): Unit =
    assertEquals(left.length, right.length)
    left.zip(right).foreach { (a, b) => assertEqualsDouble(a, b, 1e-9) }

  private lazy val selectedTimepoints: Vector[Int] =
    selection.resolveEither(model.dataset.shape, model.dataset.voxelDomain)
      .fold(error => fail(error.message), _.timepoints)

  private lazy val sharedPlan: FitPlan =
    FitPlan(
      model,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(autocorrelation = ArOptions(
        structure = ArStructure.Ar(1),
        global = true,
        iterations = 2,
        censoredTimepoints = Vector(5, 13)
      ))
    )

  private lazy val propagatingPlan: FitPlan =
    FitPlan(
      model,
      engine = FitEngine.GeneralizedLeastSquares,
      config = sharedPlan.config.copy(missingData = MissingDataPolicy.Propagate)
    )

  private lazy val model: FmriModel =
    val sampling = SamplingFrame(blockLens = Seq(8, 8), tr = Seq(1.0, 1.0))
    val task = Vector.tabulate(16)(row => math.sin((row % 8 + 1).toDouble * 0.6))
    val event = EventModel(
      terms = Vector.empty,
      samplingFrame = sampling,
      designMatrix = Mat.fromRows(task.map(value => Vector(value))),
      columnNames = Vector("task"),
      termSpans = Vector(0 -> 1),
      colIndices = Map("task" -> Vector(0))
    )
    val baseline = BaselineModel.build(samplingFrame = sampling, basis = BaselineBasis.Constant, intercept = Intercept.Global)
    val rows = Vector.tabulate(16) { row =>
      Vector.tabulate(4) { voxel =>
        val amplitude = (voxel + 1).toDouble * (if row < 8 then 0.8 else -0.5)
        amplitude * task(row) + 0.1 * math.sin((row + 1).toDouble * (voxel + 2))
      }
    }
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(DatasetId("prepared-gls-artifact"), GaleTestMatrix.fromRows(rows), SampleSpaces(Vector(4, 1, 1))),
      samplingFrame = sampling
    )
    FmriModel(event, baseline, dataset)

  private lazy val projectedModel: FmriModel =
    val sampling = SamplingFrame(blockLens = Seq(8, 8), tr = Seq(1.0, 1.0))
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        DatasetId("prepared-gls-projected"),
        GaleTestMatrix.fromRows(Vector.tabulate(16) { row =>
          Vector.tabulate(4) { voxel =>
            0.3 * (voxel + 1) + 0.1 * math.sin((row + 1).toDouble * (voxel + 2))
          }
        }),
        SampleSpaces(Vector(4, 1, 1))
      ),
      samplingFrame = sampling,
      events = DatasetEvents(Vector(
        Map("onset" -> "2.0", "condition" -> "a", "run" -> "run-1"),
        Map("onset" -> "5.0", "condition" -> "a", "run" -> "run-1"),
        Map("onset" -> "2.0", "condition" -> "a", "run" -> "run-2"),
        Map("onset" -> "5.0", "condition" -> "a", "run" -> "run-2")
      ))
    )
    FmriModelBuilder.buildModel(
      dataset,
      ModelBuildSpec(
        formula = "onset ~ hrf(condition)",
        blockColumn = Some("run"),
        baselineIntercept = Intercept.Runwise,
        defaultHrf = Hrfs.fir(nBasis = 2, span = 3.s),
        precision = 0.25.s,
        strategy = FitStrategy.RunwiseGeneralizedLeastSquares(
          AutocorrelationConfig.unsafe(coefficients = ArCoefficientSpec.Estimate)
        )
      )
    )
