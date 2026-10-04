package scalafim.fmri.fit

import scalafim.fmri.fit.GaleTestSyntax.*

import scalafim.dataset.{DataSelection, DatasetError, DatasetId, DatasetSeriesReader, FmriDataset, FmriSeries, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig, FitEngine, FitPlan, FmriModel, MissingDataPolicy, RobustOptions, RobustPsi}
import scalafim.image.SampleSpaces

class FitWorkDescriptorSuite extends munit.FunSuite:

  private val reference = FitWorkReference.unsafe("unit:α|,", "plan:β|,", "source:γ|,")
  private val selection = DataSelection(voxels = scalafim.dataset.IndexSelection.indices(3, 1, 2, 0))

  test("canonical descriptor encoding round-trips delimiter-bearing Unicode references") {
    val descriptor = compiled(blockSize = 2)
    assertEquals(FitWorkDescriptor.decode(descriptor.encode), Right(descriptor))
  }

  test("descriptor decoder rejects malformed and noncanonical inputs") {
    val descriptor = compiled(blockSize = 2)
    val fields = Vector(
      "fit-work-v1", reference.unitId.value, reference.planRevision.value, reference.sourceRevision.value,
      descriptor.datasetId, descriptor.engine.toString, descriptor.preparation.topology.toString,
      descriptor.preparation.reductions.mkString(","), "0,-1", "3,1,2,0", "2"
    )
    val duplicateAxes = fields.updated(8, "0,1").updated(9, "3,1,1,0")
    val malformed = Vector(
      descriptor.encode.dropRight(1),
      descriptor.encode + "0:",
      descriptor.encode.replace("fit-work-v1", "fit-work-v2"),
      FitWorkDescriptor.frame(Vector("fit-work-v1")),
      FitWorkDescriptor.frame(fields),
      FitWorkDescriptor.frame(duplicateAxes),
      "01:x"
    )
    malformed.foreach(value => assert(FitWorkDescriptor.decode(value).isLeft, clues(value)))
  }

  test("blank registry keys and malformed descriptors are typed refusals") {
    Vector(
      FitWorkReference.of(" ", "plan", "source"),
      FitWorkReference.of("unit", "", "source"),
      FitWorkReference.of("unit", "plan", "\t")
    ).foreach(result => assert(result.left.toOption.exists(_.isInstanceOf[FitError.InvalidWorkDescriptor]), clues(result)))
    val encoded = compiled(blockSize = 2).encode
    assert(encoded.contains("8:unit:α|,"))
    val blankUnit = encoded.replace("8:unit:α|,", "1: ")
    assert(FitWorkDescriptor.decode(blankUnit).left.toOption.exists(_.isInstanceOf[FitError.InvalidWorkDescriptor]))
    assert(FitWorkDescriptor.decode("01:x").left.toOption.exists(_.isInstanceOf[FitError.InvalidWorkDescriptor]))
  }

  test("unit and block identities are canonical and selection-sensitive") {
    val byOne = compiled(blockSize = 1)
    val byTwo = compiled(blockSize = 2)
    val otherSelection = FitWorkDescriptor.compile(reference, plan, ChunkSize.unsafe(2),
      DataSelection(voxels = scalafim.dataset.IndexSelection.indices(0, 1, 2, 3))).toOption.get
    val otherReference = FitWorkDescriptor.compile(
      FitWorkReference(FitUnitId.unsafe("other"), reference.planRevision, reference.sourceRevision), plan, ChunkSize.unsafe(2), selection
    ).toOption.get

    assertEquals(byOne.workId, byTwo.workId)
    assertNotEquals(byOne.workId, otherSelection.workId)
    assertNotEquals(byOne.workId, otherReference.workId)
    assertNotEquals(byTwo.blockWorkIds, otherSelection.blockWorkIds)
    assertNotEquals(byTwo.blockWorkIds, otherReference.blockWorkIds)
    assertEquals(byOne.blockWorkIds.distinct.length, byOne.blockWorkIds.length)
    assertEquals(byTwo.blockWorkIds.distinct.length, byTwo.blockWorkIds.length)
    assertNotEquals(byOne.blockWorkIds, byTwo.blockWorkIds)
  }

  test("fresh resolver execution matches dense OLS with bounded reads") {
    val descriptor = FitWorkDescriptor.decode(compiled(blockSize = 2).encode).toOption.get
    val reader = boundedReader(plan.model.dataset, maxVoxels = 2)
    val resolver = fixedResolver(reference, plan, reader)
    val expected = FitPlanExecutor.fit(plan, selection).toOption.get.asInstanceOf[DenseFmriFitResult]
    val actual = FitWorkExecutor.fit(descriptor, resolver).toOption.get.asInstanceOf[DenseFmriFitResult]

    assertMatrixClose(actual.coefficients.value, expected.coefficients.value)
    assertMatrixClose(actual.normalizedCovariance, expected.normalizedCovariance)
    val contrast = TContrast("task", Map("task" -> 1.0))
    assertVectorClose(
      contrast.evaluate(actual).toOption.get.statistics.toVector,
      contrast.evaluate(expected).toOption.get.statistics.toVector
    )
  }

  test("binding mismatches fail before response reads") {
    val cases = Vector(
      compiled(blockSize = 2) -> fixedResolver(FitWorkReference(FitUnitId.unsafe("wrong"), reference.planRevision, reference.sourceRevision), plan, denyingReader(plan.model.dataset)),
      FitWorkDescriptor.compile(reference, otherPlan, ChunkSize.unsafe(2), selection).toOption.get -> fixedResolver(reference, plan, denyingReader(plan.model.dataset)),
      FitWorkDescriptor.compile(reference, FitPlan(model, engine = FitEngine.GeneralizedLeastSquares,
        config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), rho = Some(0.2)))), ChunkSize.unsafe(2), selection).toOption.get -> fixedResolver(reference, plan, denyingReader(plan.model.dataset)),
      FitWorkDescriptor.compile(reference, FitPlan(model, engine = FitEngine.OrdinaryLeastSquares, config = FitConfig(missingData = MissingDataPolicy.OmitRowsPerVoxel)), ChunkSize.unsafe(2), selection).toOption.get -> fixedResolver(reference, plan, denyingReader(plan.model.dataset))
    )
    cases.foreach { (descriptor, resolver) =>
      assert(FitWorkExecutor.fit(descriptor, resolver).isLeft)
      assertEquals(resolver.asInstanceOf[CountingResolver].reads, 0)
    }
  }

  test("unsupported global preparations are classified and rejected before reads") {
    val pooled = FitPreparationRequirements(FitPreparationTopology.GlobalReduction,
      Vector(FitPreparationReduction.PooledAutocorrelation))
    assert(pooled.supportsBoundedExecution(FitEngine.GeneralizedLeastSquares))
    assert(!pooled.supportsBoundedExecution(FitEngine.RobustLeastSquares))
    val plans = Vector(
      FitPlan(model, engine = FitEngine.RobustLeastSquares,
        config = FitConfig(robust = RobustOptions(psi = RobustPsi.Huber()))) -> FitPreparationReduction.RobustRowWeights,
      FitPlan(model, engine = FitEngine.ReducedRankGls,
        config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), rho = Some(0.2)))) -> FitPreparationReduction.SpatialBasis
    )
    plans.foreach { (globalPlan, reduction) =>
      val descriptor = FitWorkDescriptor.compile(reference, globalPlan, ChunkSize.unsafe(2), selection).toOption.get
      assertEquals(descriptor.preparation.topology, FitPreparationTopology.GlobalReduction)
      assert(descriptor.preparation.reductions.contains(reduction))
      val resolver = fixedResolver(reference, globalPlan, denyingReader(globalPlan.model.dataset))
      assert(FitWorkExecutor.fit(descriptor, resolver).isLeft)
      assertEquals(resolver.asInstanceOf[CountingResolver].reads, 0)
    }
  }

  private def compiled(blockSize: Int): FitWorkDescriptor =
    FitWorkDescriptor.compile(reference, plan, ChunkSize.unsafe(blockSize), selection).toOption.get

  private final class CountingResolver(
      resolvedReference: FitWorkReference,
      resolvedPlan: FitPlan,
      reader: DatasetSeriesReader
  ) extends FitWorkResolver:
    var reads = 0
    private val counted = new DatasetSeriesReader:
      def dataset: FmriDataset = reader.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        reads += 1
        reader.seriesEither(selection)
    def resolve(reference: FitWorkReference): Either[FitError, ResolvedFitWork] =
      Right(ResolvedFitWork(resolvedReference, resolvedPlan, counted))

  private def fixedResolver(reference: FitWorkReference, plan: FitPlan, reader: DatasetSeriesReader): FitWorkResolver =
    new CountingResolver(reference, plan, reader)

  private def boundedReader(dataset: FmriDataset, maxVoxels: Int): DatasetSeriesReader =
    val underlying = SynchronousFmriDataset.readerFor(dataset).toOption.get
    new DatasetSeriesReader:
      def dataset: FmriDataset = underlying.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        selection.resolveEither(dataset.shape, dataset.voxelDomain).flatMap { resolved =>
          if resolved.voxels.length > maxVoxels then Left(DatasetError.StorageFailure("read exceeded configured bound"))
          else underlying.seriesEither(selection)
        }

  private def denyingReader(sourceDataset: FmriDataset): DatasetSeriesReader =
    new DatasetSeriesReader:
      def dataset: FmriDataset = sourceDataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        Left(DatasetError.StorageFailure("binding validation must run before reads"))

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
    left.zip(right).foreach { (actual, expected) => assertEqualsDouble(actual, expected, 1e-12) }

  private lazy val model = makeModel("fit-work-descriptor")
  private lazy val otherPlan = FitPlan(makeModel("fit-work-descriptor-other"))
  private lazy val plan = FitPlan(model)

  private def makeModel(id: String): FmriModel =
    val sampling = SamplingFrame(blockLens = Seq(10), tr = Seq(1.0))
    val event = EventModel(
      terms = Vector.empty,
      samplingFrame = sampling,
      designMatrix = Mat.fromRows(Vector.tabulate(10)(i => Vector(i.toDouble))),
      columnNames = Vector("task"),
      termSpans = Vector(0 -> 1),
      colIndices = Map("task" -> Vector(0))
    )
    val baseline = BaselineModel.build(samplingFrame = sampling, basis = BaselineBasis.Constant, intercept = Intercept.Global)
    val rows = Vector.tabulate(10) { row =>
      val x = row.toDouble
      Vector.tabulate(4) { voxel =>
        1.0 + (voxel + 1) * 0.3 * x + 0.05 * math.sin((row + 1).toDouble * (voxel + 2).toDouble)
      }
    }
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(DatasetId(id), GaleTestMatrix.fromRows(rows), SampleSpaces(Vector(4, 1, 1))),
      samplingFrame = sampling
    )
    FmriModel(event, baseline, dataset)
