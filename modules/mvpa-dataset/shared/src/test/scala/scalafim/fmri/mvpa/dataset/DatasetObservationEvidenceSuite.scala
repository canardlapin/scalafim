package scalafim.fmri.mvpa.dataset

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import gale.linalg.{DMat, Matrix}
import munit.FunSuite
import scalafim.dataset.{
  AcquisitionContext,
  DataSelection,
  DatasetId,
  DatasetKey,
  DatasetMetadata,
  DatasetResponseSchema,
  DatasetRunQuery,
  FmriDataset,
  InMemoryDatasetBackend,
  IndexSelection,
  OpenedDataset,
  ResponseKey
}
import scalafim.fmri.hrf.design.SamplingFrame
import alder.kernel.DataFingerprint
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.dataset.predictive.*
import scalafim.image.SampleSpaces
import scalafim.response.{InMemoryResponseSource, NonFinitePolicy, ResponseSchemaId, SourceId, UnitId}

import scala.concurrent.ExecutionContext.Implicits.global

class DatasetObservationEvidenceSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def matrixRows(matrix: DMat): Vector[Vector[Double]] =
    Vector.tabulate(matrix.rows): row =>
      Vector.tabulate(matrix.cols)(column => matrix(row, column))

  private def sampleKeys(evidence: DatasetObservationEvidence): Vector[String] =
    Vector.tabulate(evidence.samples.size)(index => right(evidence.samples.index.stableKeyAt(index)))

  private def neuralKeys(evidence: DatasetObservationEvidence): Vector[String] =
    Vector.tabulate(evidence.neural.size)(index => right(evidence.neural.index.stableKeyAt(index)))

  private def fixtureDataset(id: String = "mvpa-demo"): FmriDataset =
    val values = Vector(
      Vector(2.0, 2.0, 0.0),
      Vector(-2.0, -2.0, 0.0),
      Vector(3.0, 1.8, 0.5),
      Vector(-3.0, -1.8, -0.5)
    )
    FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        id = DatasetId(id),
        data = Matrix.dense(values.length, values.head.length, values.flatten),
        space = SampleSpaces(Vector(3, 1, 1)),
        metadata = DatasetMetadata.Empty
      ),
      samplingFrame = SamplingFrame(
        blockLens = Seq(4),
        tr = Seq(1.0),
        startTime = Seq(0.0),
        precision = 0.1
      )
    )

  private def selectedRequest: DatasetPatternRequest =
    DatasetPatternRequest(
      selection = DataSelection(
        time = IndexSelection.indices(1, 3),
        voxels = IndexSelection.indices(0, 2)
      ),
      metadata = SampleMetadataRequest.labeled(
        labels = Vector("scene", "scene"),
        blocks = Some(Vector("run-a", "run-b"))
      ),
      featureSpaceId = FeatureSpaceId.unsafe("selected-voxels")
    )

  private def classify(evidence: DatasetObservationEvidence): AlderSwiftCentroidResult =
    given resample4s.core.DigestAlgorithm = resample4s.core.DigestAlgorithm.fnv1a64
    val labels = right(evidence.categoricalLabels)
    val classNames = labels.values.map(_.value).distinct
    val targetAxis = right(
      AxisRef.fromStableKeys(
        "dataset-class-label",
        multivar.core.SpaceRole.Observed,
        Vector("class"),
        "class",
        "none",
        "code"
      )
    )
    val targets = right(
      MultiResponse.fromDense(
        evidence.samples,
        targetAxis,
        Matrix.dense(
          labels.size,
          1,
          labels.values.map(label => classNames.indexOf(label.value).toDouble)
        ),
        multivar.core.ValueIdentity.source(multivar.core.ValueId.unsafe("dataset-class-labels")),
        evidence.observations.source
      )
    )
    val mapping = right(
      NativeAxisMapping.fromAxis(
        evidence.samples,
        Vector.tabulate(evidence.samples.size)(_.toLong),
        DataFingerprint.external(evidence.sourceIdentity)
      )
    )
    val budget = right(MaterializationBudget(10000L))
    val admitted = right(
      AlderPredictiveAdmission.nativeTables(
        evidence.observations,
        targets,
        sampleKeys(evidence),
        DataFingerprint.external(evidence.contentIdentity),
        mapping,
        right(NativeReadPolicy(evidence.neural.size, budget))
      )
    )
    val blocks = evidence.metadata.rows.map(_.block.map(_.value).getOrElse(fail("fixture requires a block")))
    val blockCodes = blocks.distinct.zipWithIndex.toMap
    val validation = right(
      ValidationDesign.bind(
        evidence.samples,
        right(
          resample4s.designs.FixedPartitions.once(
            right(resample4s.core.Labels.retained(IArray.from(blocks.map(blockCodes))))
          )
        ),
        ScientificSeed.fromLong(23)
      )
    )
    val coding = right(SwiftTargetCoding(classNames.zipWithIndex.map((name, index) => index.toDouble -> name)))
    right(AlderSwiftCentroid.crossValidate(admitted, validation, coding))

  test("dataset evidence preserves selected timepoints, nominal axes, mapping, and values") {
    val evidence = right(DatasetObservationEvidence.fromDataset(fixtureDataset(), selectedRequest))

    assertEquals(sampleKeys(evidence), Vector("dataset-row:0", "dataset-row:1"))
    assertEquals(neuralKeys(evidence), Vector("feature:0", "feature:2"))
    assertEquals(evidence.metadata.rows.map(_.timepoint), Vector(Some(1), Some(3)))
    assertEquals(
      evidence.metadata.rows.map(_.origin),
      Vector(right(SampleOrigin.timepoint(1)), right(SampleOrigin.timepoint(3)))
    )
    assertEquals(evidence.metadata.rows.flatMap(_.block).map(_.value), Vector("run-a", "run-b"))
    assertEquals(
      matrixRows(right(evidence.observations.patterns(DMat.eye(2)))),
      Vector(Vector(-2.0, 0.0), Vector(-3.0, -0.5))
    )
    assertEquals(evidence.featureMapping.featureIndices.map(_.value), Vector(0, 2))
    assertEquals(evidence.featureSpace.id.value, "selected-voxels")
    assertEquals(evidence.featureSpace.datasetId.map(_.value), Some("mvpa-demo"))
    assertEquals(evidence.featureSpace.shape.map(_.spatialSize), Some(3))
    assertEquals(evidence.featureSpace.voxelIndices, Vector(0, 2))
    assertEquals(right(evidence.categoricalLabels).values.map(_.value), Vector("scene", "scene"))
  }

  test("selected native evidence enters Alder admission without losing sample or block alignment") {
    val evidence = right(
      DatasetObservationEvidence.fromDataset(
        fixtureDataset(),
        DatasetPatternRequest(
          selection = DataSelection(voxels = IndexSelection.indices(0, 1)),
          metadata = SampleMetadataRequest.labeled(
            labels = Vector("face", "scene", "face", "scene"),
            blocks = Some(Vector("a", "a", "b", "b"))
          ),
          featureSpaceId = FeatureSpaceId.unsafe("classification-voxels")
        )
      )
    )
    val result = classify(evidence)

    assertEqualsDouble(result.assessment.accuracy, 1.0, 1e-12)
    assertEquals(result.assessment.samples, 4L)
    assertEquals(sampleKeys(evidence), Vector("dataset-row:0", "dataset-row:1", "dataset-row:2", "dataset-row:3"))
    assertEquals(evidence.metadata.rows.flatMap(_.block).map(_.value), Vector("a", "a", "b", "b"))
  }

  test("opened-dataset attachment returns native observations at the effectful boundary") {
    val dataset = fixtureDataset()
    val schemaId = ResponseSchemaId.unsafe("mvpa-opened-schema")
    val schema = right(DatasetResponseSchema.fromDataset(dataset, schemaId, UnitId.unsafe("unit"), NonFinitePolicy.Preserve))
    val source = right(
      InMemoryResponseSource.copyFromRowMajor[IO](
        SourceId.unsafe("mvpa-opened-source"),
        schema,
        Array[Double](2.0, 2.0, 0.0, -2.0, -2.0, 0.0, 3.0, 1.8, 0.5, -3.0, -1.8, -0.5)
      )
    )
    val acquisition = right(
      AcquisitionContext.volume(
        dataset,
        DatasetKey.unsafe("sub-01"),
        ResponseKey.unsafe("bold"),
        schemaId,
        UnitId.unsafe("unit"),
        NonFinitePolicy.Preserve,
        alignment = None
      )
    )
    val opened = OpenedDataset.attach(dataset, source, acquisition).toEither.fold(
      issues => fail(issues.toNonEmptyList.toList.map(_.message).mkString("; ")),
      identity
    )

    OpenedDatasetMvpaExecutor
      .observations(opened, DatasetRunQuery.All, selectedRequest)
      .value
      .unsafeToFuture()
      .map: result =>
        val evidence = right(result)
        assertEquals(
          matrixRows(right(evidence.observations.patterns(DMat.eye(2)))),
          Vector(Vector(-2.0, 0.0), Vector(-3.0, -0.5))
        )
        assertEquals(neuralKeys(evidence), Vector("feature:0", "feature:2"))
        assertEquals(evidence.metadata.rows.map(_.timepoint), Vector(Some(1), Some(3)))
  }

  test("derived estimate rows retain estimate origins and abstract feature mapping") {
    val evidence = right(
      DatasetObservationEvidence.fromPatternRows(
        rows = Vector(
          Vector(0.1, 2.0),
          Vector(0.1, -2.0),
          Vector(0.1, 2.5),
          Vector(0.1, -2.5)
        ),
        rowNames = Vector("face_run1", "scene_run1", "face_run2", "scene_run2"),
        voxelIndices = Vector(4, 2),
        labels = Some(Vector("face", "scene", "face", "scene")),
        blocks = Some(Vector("run1", "run1", "run2", "run2")),
        featureSpaceId = FeatureSpaceId.unsafe("trial-betas")
      )
    )

    assertEquals(evidence.metadata.rows.map(_.timepoint), Vector(None, None, None, None))
    assertEquals(
      evidence.metadata.rows.map(_.origin),
      Vector(
        right(SampleOrigin.estimate("face_run1", 0)),
        right(SampleOrigin.estimate("scene_run1", 1)),
        right(SampleOrigin.estimate("face_run2", 2)),
        right(SampleOrigin.estimate("scene_run2", 3))
      )
    )
    assertEquals(
      evidence.metadata.rows.flatMap(_.item).map(_.value),
      Vector("face_run1", "scene_run1", "face_run2", "scene_run2")
    )
    assertEquals(evidence.featureSpace.isVoxelBacked, false)
    assertEquals(evidence.featureSpace.shape, None)
    assertEquals(evidence.featureMapping.featureIndices.map(_.value), Vector(4, 2))
    assertEquals(right(evidence.categoricalLabels).values.map(_.value), Vector("face", "scene", "face", "scene"))
  }

  private def identityEvidence(
      rows: Vector[Vector[Double]] = Vector(Vector(1.0, 2.0), Vector(3.0, 4.0)),
      names: Vector[String] = Vector("estimate-a", "estimate-b"),
      labels: Vector[String] = Vector("left", "right"),
      blocks: Vector[String] = Vector("a", "b"),
      dataset: String = "dataset-a"
  ): DatasetObservationEvidence =
    right(
      DatasetObservationEvidence.fromPatternRows(
        rows,
        names,
        Vector(4, 2),
        labels = Some(labels),
        blocks = Some(blocks),
        featureSpaceId = FeatureSpaceId.unsafe("identity-features"),
        datasetId = Some(DatasetId(dataset))
      )
    )

  test("evidence identities bind each declared source input and labels bind their ordered values") {
    val baseline = identityEvidence()
    val identical = identityEvidence()
    val changedDataset = identityEvidence(dataset = "dataset-b")
    val changedOrigin = identityEvidence(names = Vector("estimate-c", "estimate-d"))
    val changedValues = identityEvidence(rows = Vector(Vector(1.5, 2.0), Vector(3.0, 4.0)))
    val reordered = identityEvidence(
      rows = Vector(Vector(3.0, 4.0), Vector(1.0, 2.0)),
      names = Vector("estimate-b", "estimate-a"),
      labels = Vector("right", "left"),
      blocks = Vector("b", "a")
    )
    val changedLabels = identityEvidence(labels = Vector("right", "left"))

    assertEquals(identical.contentIdentity, baseline.contentIdentity)
    assertEquals(identical.sourceIdentity, baseline.sourceIdentity)
    assertNotEquals(changedDataset.contentIdentity, baseline.contentIdentity)
    assertNotEquals(changedOrigin.contentIdentity, baseline.contentIdentity)
    assertNotEquals(changedValues.contentIdentity, baseline.contentIdentity)
    assertNotEquals(reordered.contentIdentity, baseline.contentIdentity)
    assertNotEquals(changedLabels.contentIdentity, baseline.contentIdentity)
    assertNotEquals(changedDataset.sourceIdentity, baseline.sourceIdentity)
    assertNotEquals(changedOrigin.sourceIdentity, baseline.sourceIdentity)
    assertNotEquals(changedValues.sourceIdentity, baseline.sourceIdentity)
    assertNotEquals(reordered.sourceIdentity, baseline.sourceIdentity)
    assertNotEquals(
      right(changedLabels.categoricalLabels).valueIdentity,
      right(baseline.categoricalLabels).valueIdentity
    )
    assertEquals(sampleKeys(baseline), Vector("dataset-row:0", "dataset-row:1"))
    assertNotEquals(sampleKeys(baseline).head, sampleKeys(baseline).last)
  }

  test("unlabeled evidence and metadata, mapping, row-shape failures are explicit") {
    val unlabeled = right(
      DatasetObservationEvidence.fromPatternRows(
        Vector(Vector(1.0, 0.0), Vector(0.0, 1.0)),
        Vector("a", "b"),
        Vector(10, 11)
      )
    )
    assertEquals(unlabeled.categoricalLabels.left.toOption, Some(MvpaDatasetError.MissingCategoricalLabels))

    val badLength = DatasetObservationEvidence.fromDataset(
      fixtureDataset(),
      DatasetPatternRequest(metadata = SampleMetadataRequest.labeled(Vector("face", "scene", "face")))
    )
    assertEquals(
      badLength.left.toOption,
      Some(MvpaDatasetError.MetadataLengthMismatch("label", expected = 4, actual = 3))
    )
    assertEquals(badLength.left.toOption.map(_.category), Some(MvpaDatasetErrorCategory.Metadata))

    assertEquals(
      SampleTable.build(Vector(SampleRecord.unsafe(0, 0), SampleRecord.unsafe(3, 1))).left.toOption,
      Some(MvpaDatasetError.SampleIndexMismatch(position = 1, actual = 3))
    )
    assertEquals(
      PatternTable.fromRows(Vector(Vector(1.0, 2.0, 3.0)), Vector(0, 1)).left.toOption,
      Some(MvpaDatasetError.PatternFeatureCountMismatch(expected = 2, actual = 3))
    )
    assertEquals(
      PatternTable.fromRows(Vector(Vector(1.0, 2.0), Vector(3.0)), Vector(0, 1)).left.toOption,
      Some(MvpaDatasetError.PatternFeatureCountMismatch(expected = 2, actual = 1))
    )
    assertEquals(
      FeatureMapping.abstractFeatures(Vector(0, 0)).left.toOption,
      Some(MvpaDatasetError.InvalidFeatureMapping("feature mapping indices must be unique"))
    )
    val table = right(PatternTable.fromRows(Vector(Vector(1.0, 2.0)), Vector(0, 1)))
    val samples = right(SampleTable.fromRows(Vector("a", "b")))
    assertEquals(
      DatasetObservationEvidence.build(table, samples).left.toOption,
      Some(MvpaDatasetError.PatternRowCountMismatch(expected = 2, actual = 1))
    )

    val unreadableSelection = DatasetObservationEvidence.fromDataset(
      fixtureDataset(),
      DatasetPatternRequest(selection = DataSelection(time = IndexSelection.indices(9)))
    )
    assertEquals(
      unreadableSelection.left.toOption.map(_.category),
      Some(MvpaDatasetErrorCategory.DatasetRead)
    )
  }
