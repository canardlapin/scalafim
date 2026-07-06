# scalafim-dataset

Cross-compiled JVM/Scala.js dataset module for `scalafim`.

Package root:

```scala
import scalafim.dataset.*
```

This module is the rewrite target for `fmridataset`. It defines dataset shapes,
typed identifiers, spatial/temporal selections, fMRI series, and storage-backend
contracts. It deliberately does not emulate R S3 dispatch; new backends implement
a small typed `DatasetBackend` trait.

Backends currently include:

- `InMemoryDatasetBackend` for dense time-by-voxel matrices.
- `LatentArchiveDatasetBackend` for existing archive-backed LNA payloads.
- `LatentResponseDatasetBackend` for direct `LatentResponse` reads; selected
  timepoints and voxels are decoded through the latent response without
  materializing a whole run.
- JVM-only `LnaDataset` directory adapter for neuroarchive-style
  `derivatives/lna` trees: subject discovery, metadata tables, shared-basis
  registries, filtered `.lna.h5` lookup, archive-backed backend creation, and
  opt-in materialized reads that resolve external shared-basis archives through
  the dataset root.

Run it directly with:

```sh
sbt datasetJVM/test
sbt datasetJS/test
```
