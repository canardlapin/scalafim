# scalafim-fmri-mvpa-spatial

Surface composition and functional threshold policy over the portable MVPA providers.

Import the provider measurements separately from the surface adapters:

```scala
import mvpa4s.locus.*
import scalafim.fmri.mvpa.spatial.{SurfaceMeasurementFrames, SurfaceSupportGraphs}
```

`mvpa4s-locus` owns generic frames, locus neighborhoods, scatter, and exact
integer-grid support graphs. `SpatialMeasurementFrames` binds spatial supports to an identified neural
axis and produces typed, lazy `MeasurementFrame` entries. Regions preserve
ambient domain order; explicit selections preserve their supplied order.
Runtime owner checks reject supports from a different live domain before a
numerical read. Atlas labels and centers remain rendition metadata.

Native prediction uses `AlderPredictiveAdmission.nativeMeasurement` and a
method-owned Alder head over each entry. See the executable
[atlas workflow](../../examples/workflows-jvm/src/main/scala/scalafim/examples/workflows/AtlasMvpaWorkflow.scala),
which is tested on JVM and Scala.js.

`SurfaceMeasurementFrames.surfacePatches` composes surface searchlights through
the provider frame API; `SurfaceSupportGraphs.surface` retains anatomical mesh
adjacency. `FamilyThreshold` retains functional inference policy.

Spatial regions, selections, volume neighborhoods, and surface patches are
all expressed as typed measurement-frame entries. Local numerical work is
performed from those measured observations; this module no longer exposes a
legacy feature-set-plan or searchlight execution adapter.

Run it directly with:

```sh
sbt mvpaSpatialJVM/test
sbt mvpaSpatialJS/test
```
