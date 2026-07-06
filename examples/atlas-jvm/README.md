# atlas-jvm examples

Runnable JVM examples for `scalafim-atlas`.

These examples cover:

- standard atlas descriptor inspection;
- explicit `loadFromPaths` entry points for Schaefer, Glasser, Brainnetome, and
  FreeSurfer ASEG assets;
- coordinate lookup against a tiny synthetic atlas;
- parcel reduction of a synthetic statistic map;
- conversion from a `VolumeAtlas` to MVPA regional feature sets.

Run the smoke tests:

```sh
sbt atlasExamplesJVM/test
```

Run descriptor inspection:

```sh
sbt "atlasExamplesJVM/runMain scalafim.examples.atlas.describeStandardAtlases"
```

Run synthetic query/reduction demos:

```sh
sbt "atlasExamplesJVM/runMain scalafim.examples.atlas.queryToyAtlas"
sbt "atlasExamplesJVM/runMain scalafim.examples.atlas.reduceToyAtlas"
sbt "atlasExamplesJVM/runMain scalafim.examples.atlas.atlasToMvpaRegions"
```

Load a real Schaefer atlas from files managed by another workflow:

```sh
sbt "atlasExamplesJVM/runMain scalafim.examples.atlas.loadSchaeferAtlas /path/to/Schaefer2018_400Parcels_17Networks_order_FSLMNI152_2mm.nii.gz /path/to/Schaefer2018_400Parcels_17Networks_order.txt"
```

The loader examples accept explicit local paths. They do not download assets.
