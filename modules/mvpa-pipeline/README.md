# MVPA pipeline integration

This optional module runs MVPA units through ScalaFIM's `LocalPipelineRunner`.
MVPA core uses direct synchronous execution by default and has no pipeline
dependency. Both routes use the same core resource cleanup, cancellation,
retry/commit and deterministic reduction implementation.

Add `mvpaPipelineJVM` or `mvpaPipelineJS` to the consuming project, then pass
`scalafim.fmri.mvpa.pipeline.PipelineInterpreter` as the `interpreter` argument
to `LocalExecution.run` or `PreparedVoxelFamily.run`. Scientific plan IDs,
random streams and output ordering do not depend on that choice. Execution
remains synchronous and in memory; no durable resume or distributed execution
is provided.

The former `UnitError.Pipeline` case is now `UnitError.Interpreter`. This is a
source API change; scientific digests and stored schemas are unchanged.

Run `mvpaPipelineJVM/test` and `mvpaPipelineJS/test` for the public-consumer
contract checks. MVPA's own tests also assert that pipeline APIs are absent
from its compile classpath.
