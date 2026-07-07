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
