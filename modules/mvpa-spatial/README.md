# scalafim-fmri-mvpa-spatial

Thin spatial adapters for the portable MVPA core.

Package root:

```scala
import scalafim.fmri.mvpa.spatial.*
```

`SpatialMeasurementFrames` binds spatial supports to an identified neural
axis and produces typed, lazy `MeasurementFrame` entries. Regions preserve
ambient domain order; explicit selections preserve their supplied order.
Runtime owner checks reject supports from a different live domain before a
numerical read. Atlas labels and centers remain rendition metadata.

Native prediction uses `AlderPredictiveAdmission.nativeMeasurement` and a
method-owned Alder head over each entry. See the executable
[atlas workflow](../../examples/workflows-jvm/src/main/scala/scalafim/examples/workflows/AtlasMvpaWorkflow.scala),
which is tested on JVM and Scala.js.

`SpatialFeatureSetPlans` and `LocusFeatureSetPlans` remain for inventoried
relational (M2.09) and one-shot/structured (M3.13) consumers. Their legacy
`FeatureSetPlan` engines are not the predictive workflow API.

Run it directly with:

```sh
sbt mvpaSpatialJVM/test
sbt mvpaSpatialJS/test
```
