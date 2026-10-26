# scalafim-fmri-mvpa-dataset

Typed dataset adapters for the portable MVPA core.

Package root:

```scala
import scalafim.fmri.mvpa.dataset.*
```

Generic predictive admission and the Alder method adapters live in
[`mvpa4s-predictive`](https://github.com/canardlapin/mvpa4s/blob/main/docs/predictive.md)
(`mvpa4s.predictive`). This module feeds them dataset-backed observations.
`SoftLda.fit` and `AlderSoftLda` also live in that provider: they preserve
operator products, explicit training-only nuisance and identified validation,
without a dependency on dataset or fMRI fit types. Import `mvpa4s.predictive.*`.

See the executable
[atlas classification workflow](../../examples/workflows-jvm/src/main/scala/scalafim/examples/workflows/AtlasMvpaWorkflow.scala)
for native spatial admission and a bound validation plan. The same source and
suite run on JVM and Scala.js.

`DatasetObservationEvidence` constructs nominal sample and neural domains,
identified observations, selected feature mappings and sample metadata. It
supports synchronous `DatasetSeriesReader` input, a synchronous dataset bridge,
and explicit derived pattern rows. `OpenedDatasetMvpaExecutor.observations`
performs selected reads through the opened dataset's effectful boundary.

```scala
val request =
  DatasetPatternRequest(
    selection = DataSelection(voxels = IndexSelection.indices(0, 2, 4)),
    metadata = SampleMetadataRequest.labeled(
      labels = Vector("face", "scene", "face", "scene"),
      blocks = Some(Vector("run-1", "run-1", "run-2", "run-2"))
    )
  )
val evidence = DatasetObservationEvidence.fromDataset(dataset, request)
```

Derived coefficients use `DatasetObservationEvidence.fromPatternRows`, with
an explicit `DatasetId`, row names, ordered feature indices and optional labels, runs, blocks
and item names. Estimate origins remain distinct from timepoint origins.
`categoricalLabels` returns a column bound to the actual sample domain or a
typed missing-label refusal. Validation binds run or block columns through the
native design; the dataset adapter does not construct a second fold engine.
Regional and searchlight selection uses typed spatial measurement frames.
Serialized axes bind the declared dataset and ordered sample origins or neural
mapping coordinates. Changing labels or numeric values changes value identity;
it preserves the coordinates of that population.

This module has no JVM-only dependencies and is built for both JVM and
Scala.js. JVM-specific readers should stay in `dataset` or higher-level
workflow modules; they can produce `FmriSeries` values and then use this bridge.

Run it directly with:

```sh
sbt mvpaDatasetJVM/test
sbt mvpaDatasetJS/test
```
