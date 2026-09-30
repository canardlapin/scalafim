# HDF5 estimate metadata and collection lifecycle hooks

This bounded prerequisite adds `scalafim-estimates-core-hdf5-1` (wire `1.0.0`,
profile `0.2.0`) without changing the scientific unit or old Core1/2/3 encodings.
One product names one closed `.h5` container; its product-local observation axis,
target/pair axis and precision remain scientific descriptor authority. Inference
evidence is refused. The package-visible publication hook validates inventory,
estimability references and container identities before metadata publication.

An explicitly supplied collection inspector uses the shared immutable
collection/pointer/CAS lifecycle. The default NIfTI store refuses HDF metadata,
opening and collection membership; its Core3 status digest check remains intact.
The callback is responsible for its backend's payload admission. The helper
retains pinned identities, dataset/model/catalog consistency, failed and missing
membership, metadata budgets and stale/concurrent CAS behavior.

The test `.h5` leaves are opaque synthetic bytes. These hooks do not establish
physical HDF5 dataset conformance, native slice/handle/memory behavior, platform
qualification, migration, scientific validity or full Core completion.

Exact ownership, source and runtime receipts are retained under
`/private/tmp/scalafim-execution-20260929/hdf5-hooks-*` and
`/private/tmp/scalafim-execution-20260929/logs/hdf5-hooks-*`.


## Actual verification

Base `b11d1909ac36bc4e732a1224605f1f90c5113798`; sole author
`exec-hdf5-probe`, authoritative Mote `bd-01M3RXDEA1SPRMX9SED9KC0KWJ`,
reservation `rv-01M3RXKED8490YDM8QAZCADKSF` on exactly the ten assigned paths.
All required jobs exited 0. Full raw output and actual start/finish/exit metadata
are retained, including unsuccessful attempts. The final source was frozen
before queueing and remained unchanged through every successful job.

| Batch | Actual passes | Exit | Actual execution seconds |
| --- | --- | --- | --- |
| JVM | estimatesIo 68; estimates 17; fitEstimates 32; group 75 (192 total) | 0 | 132.465463 |
| JS contracts | estimatesIo 14; estimates 17 (31 total) | 0 | 96.027891 |
| JS consumers | fitEstimates 24; group 74 (98 total) | 0 | 105.938071 |
| Ordinary CompileAll and classpaths | both platforms; provider property `None` | 0 | 717.293764 |

All four successful logs contain zero compiler warning lines and zero runtime
warning lines. These durations describe check execution, exclude resource-lock
queue time and establish no performance or memory qualification. The JVM IO
suite used the explicitly permitted transient `Test / parallelExecution := false`
setting for its global FD counter; its assertions and build configuration were
unchanged. Every sbt invocation used `ROOT/run-sbt.py` and the shared lock. JS
batches ran in separate JVMs. No native HDF provider property was supplied.

Exact actual sbt commands (cwd: this isolated worktree) were:

hdf5-hooks-jvm-r3.log

```sh
sbt --batch -J-Xmx3g -J-XX:ActiveProcessorCount=4 'set estimatesIoJVM / Test / parallelExecution := false' estimatesIoJVM/test estimatesJVM/test fitEstimatesJVM/test groupJVM/test
```

hdf5-hooks-js-contract.log

```sh
sbt --batch -J-Xmx3g -J-XX:ActiveProcessorCount=4 estimatesIoJS/test estimatesJS/test
```

hdf5-hooks-js-consumers.log

```sh
sbt --batch -J-Xmx3g -J-XX:ActiveProcessorCount=4 fitEstimatesJS/test groupJS/test
```

hdf5-hooks-compile-all.log

```sh
sbt --batch -J-Xmx3g -J-XX:ActiveProcessorCount=4 'eval println("HDF5_HOOKS_PROVIDER_PROPERTY=" + sys.props.get("scalafim.hdf5.provider.dir"))' scalafimCompileAll 'show estimatesIoJVM / Test / fullClasspath' 'show estimatesIoJS / Test / fullClasspath'
```

## Evidence closure and retained failures

`ROOT` is `/private/tmp/scalafim-execution-20260929`. The exact review receipts are:

- `hdf5-hooks-final-check-receipt.json`: actual exits/counts/commands, raw/meta SHA256,
  absent provider property, zero compiler warnings and unchanged 2303-file freeze.
- `hdf5-hooks-source-freeze-r3.json`: all 2303 source/build/resource files frozen
  during checks. Only this verification document was finalized after checks;
  all nine code/test paths remain byte-identical to that tested freeze.
- `hdf5-hooks-runtime-closure.json`: 100 JVM/JS classpath entries and 17249 actual
  runtime/linked JS file hashes. Default classpaths contain no archive-hdf5 module
  or HDF5 binding jar.
- `hdf5-hooks-provider-source-closure.json` and `hdf5-hooks-provider-stability.json`:
  twelve loaded provider source repositories, exact commits/file hashes, unchanged.
- `hdf5-hooks-old-golden-closure.json`: all 99 pre-existing golden resource hashes
  unchanged. Core1/2/3 encoder exact-byte expectations were captured once before
  codec edits (`hdf5-hooks-baseline-authorized.log`, 7/7, exit 0), frozen in the
  shared suite, and passed on both platforms. Expectations were never regenerated.
- `hdf5-hooks-frozen-old-encoder-bytes.json`: the unchanged pre-edit byte specimens.
- `hdf5-hooks-all-job-attempts.json`: all eight actual sbt job attempts and hashes,
  including the first sandbox boot-lock failure and the two failed JVM runs.
- `hdf5-hooks-preflight-receipt.json`, `hdf5-hooks-begin-receipt.json`, and
  `hdf5-hooks-begin-authorized-receipt.json`: zero foreign overlaps, sandbox-denied
  begin, and successful authorized exact-ten begin. No ownership was taken over.
- `hdf5-hooks-diagnostic-failure.json` and `hdf5-hooks-shared-slot-inspection.json`:
  sandbox-denied read-only process diagnostic and authorized targeted lock-holder
  inspection. No other session's process was attached to or terminated.
- `hdf5-hooks-candidate-receipt.json`: exact local commit, canonical Git identity,
  ten-path staged/committed closure and clean status, written after this document.

JVM r1 actually passed 66/68 and failed two new fixture assertions: JSON object
ordering and an empty staging root's existence. The latter was corrected to
assert that it contained no staged files while still requiring no unit metadata
or pointer publication. JVM r2 actually passed 67/68; its remaining literal
comparison exposed five case-object enum spellings. The independently authored
literal was corrected to the unchanged logical wire's string spellings, without
changing the old byte expectations. Exact differences are retained in
`hdf5-hooks-r2-literal-enum-correction.json`. The final 192 JVM/129 JS checks pass;
no failures were suppressed or erased.

The checks ran on macOS arm64 with sbt 1.11.7, Scala 3.7.4, sbt's Homebrew
Java 25.0.1 and Node v26.7.0. The shell `java -version` separately reported Java
22; that was not the sbt runtime. `hdf5-hooks-toolchain-receipt.json` preserves the
actual observations. This is not JDK21/Linux/native-platform qualification.
The isolated clone's origin is the local `ROOT/integration` repository; canonical
Git author/account configuration is `canardlapin` with
`307091466+canardlapin@users.noreply.github.com`. No GitHub operation occurred.

## Review boundary

This candidate provides strict metadata, prepublication closure checks and one
backend-specific collection lifecycle. Independent literal tests exercise both
per-sample Absolute covariance and normalized Float64 shared-U, nonlexical pairs,
reordered product-local observation axes and an unused unit observation. Negative
cases cover inventory, layout, precision, references, aliases, tags, fields,
versions, inference/status disguises and old-profile admission. JVM hooks cover
no metadata on refused closure, immutable retry/conflict, default Unsupported
before absent-container access, explicit callback refusal, pinned catalog/member
checks, missing/failed membership, budgets, Core3 status corruption and concurrent
CAS. Existing independent NIfTI/Core3 golden suites and affected consumers passed.

Parent owns SHA-bound independent review and integration. Mote remains open for
that review; paths are released through normal handoff. No publication, physical
HDF backend, native allocation/handle qualification, legacy migration, fit,
scientific inference or full Core completion is established by this candidate.
