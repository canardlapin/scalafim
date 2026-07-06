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
  a `PatternMatrix` and backend-neutral `PatternSource`;
- `PatternTable` represents any row-by-feature numeric product, including
  fit-result coefficient maps or trialwise beta maps, without depending on the
  `fit` module;
- `FeatureSpaceRef` records the selected voxel feature space while preserving
  global voxel indices, so atlas regions and searchlight plans remain
  compatible after dataset selection;
- `SampleTable` and `SampleRecord` preserve row-aligned sample provenance,
  distinguishing acquisition timepoints from derived estimate rows;
- typed sample metadata builds categorical `Response` values and leave-one-run
  or leave-one-block-out `FoldPlan`s;
- failures are represented as `MvpaDatasetError` values rather than thrown
  ingestion exceptions.

Example:

```scala
val view =
  MvpaDatasetView
    .fromDataset(
      dataset,
      selection = DataSelection(voxels = IndexSelection.indices(0, 2, 4)),
      labels = Some(Vector("face", "scene", "face", "scene")),
      blocks = Some(Vector("run-1", "run-1", "run-2", "run-2"))
    )
    .toOption
    .get

val result =
  MvpaEngine.runSource(
    view.source,
    regions,
    view.response.toOption.get,
    CrossValidatedClassifierAnalysis(SwiftCentroidClassifier()),
    Some(view.foldsByBlock.toOption.get)
  )
```

Derived rows such as condition coefficients or LSS trial betas use the same
view surface:

```scala
val betaView =
  MvpaDatasetView
    .fromPatternRows(
      rows = betaRows,
      rowNames = trialNames,
      voxelIndices = selectedVoxels,
      labels = Some(trialLabels),
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
