# scalafim-fmri-mvpa-dataset

Typed dataset adapters for the portable MVPA core.

Package root:

```scala
import scalafim.fmri.mvpa.dataset.*
```

`scalafim-fmri-mvpa` deliberately works over sample-by-feature pattern sources.
`scalafim-dataset` deliberately works over storage-backed fMRI series. This
module is the narrow bridge between the two:

- `MvpaDatasetView` converts an `FmriSeries` or selected `FmriDataset` read into
  a `PatternMatrix` and backend-neutral `DensePatternSource`;
- `LabeledMvpaDatasetView` is the classifier-facing view: it can only be built
  when every sample has a class label and exposes a validated `Response`
  directly;
- `PatternTable` represents any row-by-feature numeric product, including
  fit-result coefficient maps or trialwise beta maps, without depending on the
  `fit` module;
- `FeatureMapping` distinguishes voxel-backed dataset features from abstract
  feature ids while preserving global indices, so atlas regions and searchlight
  plans remain compatible after dataset selection;
- `DatasetPatternRequest` ties a `DataSelection`, `SampleMetadataRequest`, and
  feature-space id into one typed request;
- `SampleTable`, `SampleRecord`, and `SampleMetadata` preserve row-aligned
  sample provenance and aligned label/block/run/item columns;
- failures are represented as categorized `MvpaDatasetError` values rather than
  thrown ingestion exceptions.

Example:

```scala
val view =
  LabeledMvpaDatasetView
    .fromDataset(
      dataset,
      labels = Vector("face", "scene", "face", "scene"),
      selection = DataSelection(voxels = IndexSelection.indices(0, 2, 4)),
      blocks = Some(Vector("run-1", "run-1", "run-2", "run-2"))
    )
    .toOption
    .get

val result =
  MvpaEngine.runSource(
    view.source,
    regions,
    view.response,
    CrossValidatedClassifierAnalysis(SwiftCentroidClassifier()),
    Some(view.foldsByBlock.toOption.get)
  )
```

For lower-level construction, pass a single typed request:

```scala
val request =
  DatasetPatternRequest(
    selection = DataSelection(voxels = IndexSelection.indices(0, 2, 4)),
    metadata = SampleMetadataRequest.labeled(
      labels = Vector("face", "scene", "face", "scene"),
      blocks = Some(Vector("run-1", "run-1", "run-2", "run-2"))
    )
  )

val unlabeledOrLabeled =
  MvpaDatasetView.fromDataset(dataset, request)
```

Derived rows such as condition coefficients or LSS trial betas use the same
view surface:

```scala
val betaView =
  LabeledMvpaDatasetView
    .fromPatternRows(
      rows = betaRows,
      rowNames = trialNames,
      featureIndices = selectedVoxels,
      labels = trialLabels,
      blocks = Some(runLabels),
      featureSpaceId = FeatureSpaceId.unsafe("trial-betas")
    )
    .toOption
    .get
```

`voxelIndices` remain global feature ids. A `FeatureSetPlan` built from an
atlas or searchlight can therefore be used with either raw timepoint patterns or
derived beta rows as long as both refer to the same voxel id space.

This module has no JVM-only dependencies and is built for both JVM and
Scala.js. JVM-specific readers should stay in `dataset` or higher-level
workflow modules; they can produce `FmriSeries` values and then use this bridge.

Run it directly with:

```sh
sbt mvpaDatasetJVM/test
sbt mvpaDatasetJS/test
```
