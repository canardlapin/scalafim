# scalafim-pipeline

Cross-compiled JVM/Scala.js generic pipeline graph module for `scalafim`.

Package root:

```scala
import scalafim.pipeline.*
```

This module owns the platform-neutral pipeline algebra: typed artifact
references, immutable graph construction, validation, deterministic
topological staging, local pure execution, and structured receipts. It does not
own neuroimaging algorithms, file IO, container execution, scheduling, or
external command wrappers. Higher-level fMRI, image, BIDS, and spatial
pipelines should adapt their existing typed plans into this graph rather than
putting workflow concerns into lower computational modules.

`ExecutionPlan` validates node identity, dependency existence, and artifact
kinds in pipeline vocabulary, then delegates successful layer construction to
`scalafim.graph.Dag`. Repeated expression dependencies are collapsed at that
boundary. Cycle errors preserve the existing insertion-ordered set of all
blocked nodes, including descendants and self-dependencies, rather than leaking
only a generic cycle witness.

Use `PipelineBuilder` for ordinary authoring. Each call returns a new builder
plus the typed artifact reference produced by that node:

```scala
val built =
  for
    b0 <- PipelineBuilder("first-level")
    bold <- b0.input("bold", boldKind)
    design <- bold.builder.input("design", designKind)
    fit <- design.builder.step("fit", fitStep, bold.expr.zip(design.expr))
    graph <- fit.builder.output("betas", fit.ref).map(_.build)
  yield graph
```

The builder is only an ergonomic facade; validation still lives in
`PipelineGraph`, so low-level and builder-based graphs share the same
invariants.

Completed runs expose declared outputs by name and expected kind:

```scala
val run = LocalPipelineRunner.run(graph, context)
val betas = run.output("betas", betaKind)
val trace = run.trace.renderText
```

`PipelineTrace` is a pure structured provenance value. It records the graph id,
declared outputs, node descriptions, execution statuses, messages, runner
metadata, and sorted user metadata from `RunContext`.

Execution is interpreter-based:

- `LocalPipelineRunner` is the deterministic sequential reference runner.
- `FuturePipelineRunner` executes runnable nodes inside each topological stage
  with `scala.concurrent.Future`, an explicit `ExecutionContext`, and a
  `ParallelPolicy`. Stage boundaries remain barriers, artifact-table updates
  happen only after a stage completes, and receipts are emitted in deterministic
  graph order.

Run it directly with:

```sh
sbt pipelineJVM/test
sbt pipelineJS/test
```
