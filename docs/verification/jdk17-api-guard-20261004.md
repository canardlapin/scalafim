# JDK 17 API guard (2026-10-04)

CI pins JDK 17 (`.github/workflows/first-level.yml`). Main used a JDK 19 API in a
test source compiled by `mvpaJVM/test`, which would have failed CI compilation.

## Inventory

`git grep` on `main` (2223f13a) for `threadId()`, `Thread.ofVirtual`,
`getFirst()`/`getLast()`/`reversed()`, `Math.clamp`, `.repeat(` over `*.scala`,
`*.java`, `*.sbt`, `project/*.scala`:

- `modules/mvpa/jvm/src/test/scala/scalafim/fmri/mvpa/RsaAllocationProbe.scala:14`
  — `Thread.currentThread().threadId()` (JDK 19+). Fixed.
- `SurfaceWorldIndexBenchmark.scala:17` — comment only.

## Changes

- `RsaAllocationProbe` now uses `com.sun.management.ThreadMXBean`
  `getCurrentThreadAllocatedBytes()` (JDK 14+), matching
  `SurfaceWorldIndexBenchmark`; no deprecated `getId()`.
- `build.sbt` `commonSettings` adds `scalacOptions` `-release:17`. Every project
  except the aggregate `root` uses `commonSettings`.

## Evidence

Host JDK: 22 (javac 22). No JDK 17 installed.

- Java API probe, `javac --release 17`: a class using `ThreadMXBean`
  `isThreadAllocatedMemorySupported` / `setThreadAllocatedMemoryEnabled` /
  `getCurrentThreadAllocatedBytes` compiled (exit 0); negative control calling
  `Thread.currentThread().threadId()` failed (exit 1, `cannot find symbol: method threadId()`).
- Scala guard negative control (earlier scratch worktree, branch
  `fix/jdk17-api-20261004`, not committed): with `-release:17` and a test file
  `ReleaseNegativeControl.scala` containing `Thread.currentThread().threadId()`,
  `sbt-warm mvpaJVM/Test/compile` failed with exactly one error:
  `[E008] Not Found Error ... value threadId is not a member of Thread`. The fixed
  `RsaAllocationProbe` compiled cleanly in the same run.

## Gates (this branch, `sbt-warm`, JDK 22 host)

| Gate | Result |
| --- | --- |
| `mvpaJVM/test` | exit 0; 128 passed, 0 failed, 0 errors; 0 `[warn]` |
| `scalafimCompileAll` | exit 0; 149 compile units, 0 `[error]`, 0 `[warn]` |
| `examplesCompile` | exit 0; 3 compile units, 0 `[error]`, 0 `[warn]` |

No module needed changes beyond `RsaAllocationProbe` under `-release:17`.

## Independent review (Opus, APPROVE-WITH-NITS)

- Negative control reproduced independently at `febb3687` in a different module
  (`hrf`): `hrfJVM/compile` and `hrfJS/compile` both reject `threadId()` under the
  flag; removing the flag restores compilation. The guard covers shared code on JS.
- **Bytecode target changes from 52 (Java 8) to 61 (Java 17)** (`javap` on
  `RsaAllocationProbe$` and `hrf/Reconstruction`). Published JVM artifacts now require
  JDK 17+ at runtime, matching the declared CI baseline. This is an intended
  consequence of `-release:17`.
- Coverage: every root-build project uses `commonSettings` except the source-less
  `root` aggregate. Not covered: the standalone `modules/mvpa-foundation-spike/build.sbt`
  (not in CI) and JMH-generated Java in bench projects (negligible).
- Out of scope, noted for their own repos: reframe4s and locus4s *test* sources use
  `threadId()` and would fail their own JDK 17 CI; gale main's FFM backend
  (`java.lang.foreign`) is not on scalafim's pinned path.
