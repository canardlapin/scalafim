# workflows-jvm examples

Runnable JVM examples that demonstrate cross-module workflows without requiring
external data.

Current workflows:

- predictive: builds a tiny synthetic `VolumeAtlas`, derives one identified
  neural axis and `MeasurementFrame`, then runs an Alder-backed,
  leave-one-run-out categorical estimand per region;
- relational: reduces the same raw patterns to identified runwise
  condition-by-neural relations, declares the independent run pairs, and runs
  an identity-precision RDM through the same frame.

The shortest public path is the same in both cases:

```text
raw arrays
  -> total axis and EvidenceTable constructors
  -> scientific source + evidence design + MeasurementFrame + estimand
  -> Mvpa.run(source)(design, frame, estimand, strategy)
  -> AnalysisResult with typed MeasurementOutcome values and receipts
```

All ScalaFIM identities in the example use their public `Either`-returning
constructors. The successful relational values retain the exact measurement,
estimand, and fit identities; failures remain local typed outcomes, while a
top-level `MvpaRunError.message` names the failed pipeline stage.

Run the smoke tests:

```sh
sbt workflowExamplesJVM/test
```

Run the workflow:

```sh
sbt "workflowExamplesJVM/runMain scalafim.examples.workflows.runAtlasMvpaWorkflow"
```

The example is deliberately in-memory so both real analyses run without data
downloads. File-backed atlas loading examples live in `examples/atlas-jvm`.
