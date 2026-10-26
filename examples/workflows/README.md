# Workflow examples

Cross-compiled workflow examples using shared in-memory sources, with native IO
and ProfileHRF preparation confined to the JVM source root.

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

Source layout: `shared` owns the portable atlas workflow and four ecosystem
scenario courts; `jvm` owns ProfileHRF and FreeSurfer IO, and `js` supplies its
platform fixture adapter. The existing `workflowExamplesJVM` and
`workflowExamplesJS` entry points remain available.
