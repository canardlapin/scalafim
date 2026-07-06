# scalafim-fmri-mvpa-spatial

Thin spatial adapters for the portable MVPA core.

Package root:

```scala
import scalafim.fmri.mvpa.spatial.*
```

`scalafim-fmri-mvpa` deliberately owns only sample-by-feature analysis
contracts. This module converts spatial objects into `FeatureSetPlan` values so
regional, searchlight, RSA, classification, crossnobis, and feature-model
analyses can share the same execution engine.

Supported adapters:

- integer volume label maps to regional plans;
- `VolumeAtlas` payloads to regional plans with atlas region ids and labels;
- image ROI/searchlight windows to searchlight plans;
- exhaustive image mask searchlights to searchlight plans;
- surface parcel units or labeled surfaces to regional plans.

Example:

```scala
val regions =
  SpatialFeatureSetPlans.fromVolumeAtlas("atlas", atlas).toOption.get

val searchlights =
  SpatialFeatureSetPlans
    .fromSearchlightMask("search", brainMask, radius = 4.0)
    .toOption
    .get

val analysis =
  CrossValidatedClassifierAnalysis(SwiftCentroidClassifier())

val regionalResult =
  MvpaEngine.run(patterns, regions, labels, analysis, folds = Some(folds))

val searchlightResult =
  MvpaStream.outcomes(source, searchlights, labels, analysis, Some(folds))
```

The adapter keeps feature ids zero-based and deterministic:

- volume labels preserve ascending linear voxel indices within each label;
- atlas regions follow `RegionIndex` order and preserve atlas labels;
- ROI/searchlight windows preserve the coordinate order supplied by the image
  ROI object and use the parent linear voxel index as the searchlight id and
  center;
- surface parcels use deterministic ordinal ROI ids and keep parcel names in
  `FeatureSet.label`.

Run it directly with:

```sh
sbt mvpaSpatialJVM/test
sbt mvpaSpatialJS/test
```
