# scalafim-multivar

Typed multivariate analysis core for ScalaFIM.

This module owns the portable algebra below MVPA and neuroimaging adapters:

- typed finite vector spaces and dimensions;
- ordered feature/sample index sets;
- disjoint block partitions for future multiblock methods;
- matrix-view contracts over dense, sparse, and lazy operator-backed inputs;
- shared error ADTs for estimator, preprocessing, and map layers;
- pure `MultivarPlan` / `FitArtifactShape` descriptions for sample-by-feature
  ROI execution.

`multivar` depends only on `linalg`. Keep dataset, image, MVPA adapter, JVM
solver backend, and Spark-specific code in higher modules.

## Neuroimaging boundary

Neuroimaging modules should translate their own objects into pure multivar
plans instead of pulling dataset/image/Spark types into this module.

- `mvpa` pattern sources map naturally to `SampleByFeatureInput` plus a
  `RoiPlanSet`.
- `dataset` and ROI adapters should carry only serializable source references
  such as `MultivarSourceRef.DatasetSelection(...)` at this layer.
- `LocalMultivarExecutor` is the reference interpreter for ROI/block execution.
  A later JVM adapter can partition by ROI and broadcast small fitted maps using
  the same `MultivarPlan` and `FitArtifactShape` values, without changing the
  shared algebra.

## Release evidence

The shared test suite covers the current core invariants on both JVM and JS:

- typed ids, dimensions, index sets, and complete disjoint block partitions;
- dense, sparse, and affine `MatrixView` algebra without implicit sparse
  densification;
- preprocessing, map/projector algebra, and decoder-capability boundaries;
- SVD/PCA/PLSC/CCA decompositions and generalized eigensolver behavior;
- row/column metrics and generalized PCA/GMD, including dense, diagonal,
  sparse-preserving, rank-deficient PSD, and R-reference-backed paths;
- multiblock projection restrictions and cross-domain transfer maps;
- row-side whitening/projector/effect-operator algebra matching the
  multivarious fixed-effect projector form;
- kernel and Nyström artifacts, including out-of-sample projection;
- pure ROI/sample-by-feature `MultivarPlan` execution that stays independent of
  MVPA, dataset, image IO, Spark, and JVM-only numeric libraries.
