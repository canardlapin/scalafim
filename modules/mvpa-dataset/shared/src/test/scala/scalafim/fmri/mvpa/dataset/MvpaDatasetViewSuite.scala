package scalafim.fmri.mvpa.dataset

import scalafim.dataset.{DataSelection, DatasetId, FmriDataset, IndexSelection, InMemoryDatasetBackend}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.mvpa.*
import scalafim.image.{DMat, NeuroSpace}

class MvpaDatasetViewSuite extends munit.FunSuite:

  private def rows(matrix: gale.linalg.DMat): Vector[Vector[Double]] =
    Vector.tabulate(matrix.rows)(row => Vector.tabulate(matrix.cols)(col => matrix(row, col)))

  private def timepointOrigin(index: Int): SampleOrigin =
    SampleOrigin.timepoint(index).toOption.get

  private def estimateOrigin(name: String, row: Int): SampleOrigin =
    SampleOrigin.estimate(name, row).toOption.get

  private def dataset: FmriDataset =
    val rows =
      Vector(
        Vector(2.0, 2.0, 0.0),
        Vector(-2.0, -2.0, 0.0),
        Vector(3.0, 1.8, 0.5),
        Vector(-3.0, -1.8, -0.5)
      )
    FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        id = DatasetId("mvpa-demo"),
        data = DMat.fromRows(rows),
        space = NeuroSpace(Vector(3, 1, 1))
      ),
      samplingFrame = SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))
    )

  test("dataset view preserves selected timepoints, matrix orientation, and global feature indices") {
    val request =
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

    val view =
      MvpaDatasetView
        .fromDataset(dataset, request)
        .toOption
        .get

    assertEquals(view.samples.rows.map(_.index.value), Vector(0, 1))
    assertEquals(view.samples.rows.map(_.timepoint), Vector(Some(1), Some(3)))
    assertEquals(view.samples.rows.map(_.origin), Vector(timepointOrigin(1), timepointOrigin(3)))
    assertEquals(view.samples.metadata.blocks.flatten.map(_.value), Vector("run-a", "run-b"))
    assertEquals(rows(view.patterns.value), Vector(Vector(-2.0, 0.0), Vector(-3.0, -0.5)))
    assertEquals(view.patterns.featureIndices.map(_.value), Vector(0, 2))
    assert(view.featureMapping.isVoxelBacked)
    assertEquals(view.featureSpace.id.value, "selected-voxels")
    assertEquals(view.featureSpace.datasetId.map(_.value), Some("mvpa-demo"))
    assertEquals(view.featureSpace.shape.map(_.spatialSize), Some(3))
    assertEquals(view.featureSpace.voxelIndices, Vector(0, 2))
  }

  test("derived pattern rows support beta-like estimates without pretending to be timepoints") {
    val view =
      MvpaDatasetView
        .fromPatternRows(
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
        .toOption
        .get

    assertEquals(view.samples.rows.map(_.timepoint), Vector(None, None, None, None))
    assertEquals(
      view.samples.rows.map(_.origin),
      Vector(
        estimateOrigin("face_run1", 0),
        estimateOrigin("scene_run1", 1),
        estimateOrigin("face_run2", 2),
        estimateOrigin("scene_run2", 3)
      )
    )
    assertEquals(view.samples.rows.flatMap(_.item).map(_.value), Vector("face_run1", "scene_run1", "face_run2", "scene_run2"))
    assert(!view.featureMapping.isVoxelBacked)
    assertEquals(view.featureSpace.shape, None)
    assertEquals(view.patterns.featureIndices.map(_.value), Vector(4, 2))

    val featureSet = FeatureSet.unsafe(RoiId(7), Vector(2, 4), label = Some("signal"))
    val selected = view.source.selectFeatures(featureSet).toOption.get
    assertEquals(
      rows(selected.value),
      Vector(
        Vector(2.0, 0.1),
        Vector(-2.0, 0.1),
        Vector(2.5, 0.1),
        Vector(-2.5, 0.1)
      )
    )

    val plan =
      FeatureSetPlan
        .regional("beta-region", Vector(featureSet))
        .toOption
        .get

    val labeled = view.toLabeled.toOption.get

    val result =
      MvpaEngine
        .runSource(
          labeled.source,
          plan,
          labeled.response,
          CrossValidatedClassifierAnalysis(SwiftCentroidClassifier()),
          Some(labeled.foldsByBlock.toOption.get)
        )
        .toOption
        .get

    assertEquals(result.failures, Vector.empty)
    assertEqualsDouble(result.successes.head.metrics("Accuracy").getOrElse(Double.NaN), 1.0, 1e-12)
  }

  test("sample metadata builds categorical responses and labeled leave-one-block-out folds") {
    val request =
      DatasetPatternRequest(
        metadata = SampleMetadataRequest.labeled(
          labels = Vector("face", "scene", "face", "scene"),
          blocks = Some(Vector("a", "a", "b", "b")),
          runs = Some(Vector("run-1", "run-1", "run-2", "run-2")),
          items = Some(Vector("face-a", "scene-a", "face-b", "scene-b"))
        )
      )

    val view =
      MvpaDatasetView
        .fromDataset(dataset, request)
        .toOption
        .get

    val labeled = view.toLabeled.toOption.get
    assertEquals(labeled.response.length, 4)
    assertEquals(view.samples.metadata.labels.flatten.map(_.value), Vector("face", "scene", "face", "scene"))

    val blockFolds = view.foldsByBlock.toOption.get
    assertEquals(blockFolds.samples, 4)
    assertEquals(blockFolds.folds.map(_.id), Vector("block:a", "block:b"))
    assertEquals(blockFolds.folds.map(_.test.map(_.value)), Vector(Vector(0, 1), Vector(2, 3)))
    assertEquals(blockFolds.folds.map(_.train.map(_.value)), Vector(Vector(2, 3), Vector(0, 1)))

    val runFolds = view.foldsByRun.toOption.get
    assertEquals(runFolds.folds.map(_.id), Vector("run:run-1", "run:run-2"))
  }

  test("labeled dataset view source drives the MVPA engine without losing alignment") {
    val view =
      LabeledMvpaDatasetView
        .fromDataset(
          dataset,
          labels = Vector("face", "scene", "face", "scene"),
          blocks = Some(Vector("a", "a", "b", "b"))
        )
        .toOption
        .get

    val plan =
      FeatureSetPlan
        .regional(
          "selected-region",
          Vector(FeatureSet.unsafe(RoiId(1), Vector(0, 1), label = Some("signal")))
        )
        .toOption
        .get

    val result =
      MvpaEngine
        .runSource(
          view.source,
          plan,
          view.response,
          CrossValidatedClassifierAnalysis(SwiftCentroidClassifier()),
          Some(view.foldsByBlock.toOption.get)
        )
        .toOption
        .get

    assertEquals(result.failures, Vector.empty)
    assertEquals(result.successes.length, 1)
    assertEqualsDouble(result.successes.head.metrics("Accuracy").getOrElse(Double.NaN), 1.0, 1e-12)
    assertEqualsDouble(result.successes.head.metrics("TestedSamples").getOrElse(Double.NaN), 4.0, 1e-12)
  }

  test("labeled view refuses unlabeled samples before classifier workflows") {
    val unlabeled =
      MvpaDatasetView
        .fromDataset(dataset)
        .toOption
        .get

    assertEquals(unlabeled.toLabeled.left.toOption, Some(MvpaDatasetError.MissingCategoricalLabels))

    val labeled =
      LabeledMvpaDatasetView
        .fromPatternRows(
          rows = Vector(Vector(1.0, 0.0), Vector(0.0, 1.0)),
          rowNames = Vector("a", "b"),
          featureIndices = Vector(10, 11),
          labels = Vector("left", "right")
        )
        .toOption
        .get

    assertEquals(labeled.response.length, 2)
    assertEquals(labeled.featureSpace.isVoxelBacked, false)
  }

  test("metadata contract failures are explicit ADT values") {
    val missingLabels =
      MvpaDatasetView
        .fromDataset(dataset)
        .toOption
        .get
        .response

    assertEquals(missingLabels.left.toOption, Some(MvpaDatasetError.MissingCategoricalLabels))

    val badLength =
      MvpaDatasetView.fromDataset(
        dataset,
        labels = Some(Vector("face", "scene", "face"))
      )

    assertEquals(
      badLength.left.toOption,
      Some(MvpaDatasetError.MetadataLengthMismatch("label", expected = 4, actual = 3))
    )
    assertEquals(
      badLength.left.toOption.map(_.category),
      Some(MvpaDatasetErrorCategory.Metadata)
    )

    val badSampleTable =
      SampleTable.build(
        Vector(
          SampleRecord.unsafe(index = 0, timepoint = 0),
          SampleRecord.unsafe(index = 3, timepoint = 1)
        )
      )

    assertEquals(
      badSampleTable.left.toOption,
      Some(MvpaDatasetError.SampleIndexMismatch(position = 1, actual = 3))
    )

    val badFeatureCount =
      PatternTable.fromRows(
        rows = Vector(Vector(1.0, 2.0, 3.0)),
        voxelIndices = Vector(0, 1)
      )

    assertEquals(
      badFeatureCount.left.toOption,
      Some(MvpaDatasetError.PatternFeatureCountMismatch(expected = 2, actual = 3))
    )

    val raggedRows =
      PatternTable.fromRows(
        rows = Vector(Vector(1.0, 2.0), Vector(3.0)),
        voxelIndices = Vector(0, 1)
      )

    assertEquals(
      raggedRows.left.toOption,
      Some(MvpaDatasetError.PatternFeatureCountMismatch(expected = 2, actual = 1))
    )

    val duplicateFeatures =
      FeatureMapping.abstractFeatures(Vector(0, 0))

    assertEquals(
      duplicateFeatures.left.toOption,
      Some(MvpaDatasetError.InvalidFeatureMapping("feature mapping indices must be unique"))
    )
    assertEquals(
      duplicateFeatures.left.toOption.map(_.category),
      Some(MvpaDatasetErrorCategory.FeatureMapping)
    )

    val table =
      PatternTable
        .fromRows(
          rows = Vector(Vector(1.0, 2.0)),
          voxelIndices = Vector(0, 1)
        )
        .toOption
        .get
    val samples =
      SampleTable
        .fromRows(Vector("a", "b"))
        .toOption
        .get

    assertEquals(
      MvpaDatasetView.fromPatternTable(table, samples).left.toOption,
      Some(MvpaDatasetError.PatternRowCountMismatch(expected = 2, actual = 1))
    )
  }
