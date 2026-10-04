# workflows-jvm examples

Runnable JVM examples that demonstrate cross-module workflows without requiring
external data.

Current workflow:

- `AtlasMvpaWorkflow`: builds a tiny synthetic `VolumeAtlas`, converts atlas
  regions into typed spatial measurements, admits identified observations, and
  runs the native Alder Swift centroid head with a sample-bound validation plan.

Run the smoke tests:

```sh
sbt workflowExamplesJVM/test
sbt workflowExamplesJS/test
```

Run the workflow:

```sh
sbt "workflowExamplesJVM/runMain scalafim.examples.workflows.runAtlasMvpaWorkflow"
```

The example is deliberately in-memory. File-backed atlas loading examples live
in `examples/atlas-jvm`.
