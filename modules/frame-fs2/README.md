# scalafim-frame-fs2

FS2 and Cats Effect execution boundaries for `scalafim-frame`.

`FrameRuntime[F]` interprets a pure `Frame[Schema]` with the always-available
reference backend. `stream` brackets the execution cursor and each emitted
`RecordBatch`; cancellation, early termination, and failures close retained
input batches. `collect` returns `Resource[F, Table[Schema]]` and retains output
buffers only for the resource lifetime.

This adapter does not add a production execution engine or silently fall back
to another backend.

The shared boundary includes capability-described in-memory and CSV
sources/sinks. Requested, accepted, and residual pushdown are explicit. The JVM
side includes an Apache Arrow Java 19 IPC stream adapter; Arrow allocator types
do not cross into shared or core APIs. Java 9+ consumers of that adapter must
open `java.base/java.nio` as documented by Arrow Java.

```sh
sbt frameFs2JVM/test
sbt frameFs2JS/test
```
