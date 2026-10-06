# Isolate the full-resolution atlas inversion oracle

Mote: `bd-01M448BY154ZN24KRJKSBW3YAH`.

The JVM atlas gate now runs `scalafim.atlas.io.MniTemplateBridgeFilesSuite`
in a fresh subprocess with a 3 GB maximum heap and two active processors.
Other atlas JVM suites remain in process. The atlas groups and their tests
run sequentially; Scala.js settings are unchanged. The grouping preserves
existing fork options, replacing inherited initial/maximum heap flags only.
The standard `test`, `testOnly`, and `testQuick` selection paths remain available.

The full-resolution bridge, inverse lattice, oracle inputs and qualification
gates are unchanged. Pull errors must remain within `1e-8`; push and round-trip
errors must remain within `0.05 mm`.

## Why a separate process

The pinned reframe4s implementation retains large coordinate arrays and creates
inverse and residual workspaces. The independent source audit estimates about
764.76 MiB of retained primitive payloads after inversion, with additional
construction allocations. This is a lower envelope, excluding object/array
headers and transient decoder, workspace and residual-summary allocations.
It is not a measured heap peak. A fresh child makes the oracle independent of
the resident server's previously retained build and test state, and releases
its entire heap at the end of each gate.

This change does not optimize production inversion or bound total process RSS.
Provider allocation improvements belong upstream in reframe4s. The recorded
process measurements are sampled RSS, which includes memory outside the heap.

## Replay

Set `TEMPLATEFLOW_HOME` before starting a new warm server. Supplying it only
to a later client cannot update the resident server's environment. Both pinned
composites must be nonempty and match the `TemplateFlowXfm` hashes.

```sh
python3 tools/build/sbt-warm --shutdown
export TEMPLATEFLOW_HOME=/path/to/verified/templateflow
python3 tools/build/sbt-warm imageJVM/test surfaceJVM/test
python3 tools/build/sbt-warm atlasJVM/test
python3 tools/build/sbt-warm 'atlasJVM/testOnly scalafim.atlas.io.MniTemplateBridgeFilesSuite'
python3 tools/build/sbt-warm 'atlasJVM/testOnly scalafim.atlas.MniTemplateBridgeSuite'
python3 tools/build/sbt-warm atlasJS/test
```

Inspect `receipt.json` for observed counts, child flags, parent/child PIDs,
sampled memory and exact source/artifact hashes. The archive retains raw logs,
the host runner, process samples and the independently derived allocation audit.
Missing assets yield explicit skips and cannot qualify native inversion.

## Results

| Gate | Passed | Skipped |
| --- | ---: | ---: |
| Atlas JVM | 116 | 0 |
| Atlas JS | 79 | 0 |
| Native oracle repeat | 8 | 0 |
| Subsequent ordinary JVM suite | 7 | 0 |
| Image/surface JVM warm-up | 653 | 12 |

The warm-up skips concern unrelated surface assets. Both native runs used
different child PIDs under the same parent, with the verified heap/processor
limits, and both children exited. That parent also ran the subsequent ordinary
JVM and JS gates. It was absent after the enclosing runner exited; no liveness
beyond the command sequence is claimed.

The original host runner's final instrumentation assertion misclassified a
short-lived JVM helper and missed JDK argument-file launches. Supplemental
monitoring verified the children and their flags independently; all test
commands themselves returned zero. The archive includes both the executed
runner and its corrected replay version. Sampling of the first native child
began during the gate, so RSS observations are not full-lifetime peak guarantees.

## Build setup finding

An initial attempt to inject `TEMPLATEFLOW_HOME` through an interactive `set`
after a reload failed during settings reapplication, with up to 98.7% GC time
and 0.02 GB free in the 3 GB resident heap. No explicit OOM stack was emitted.
That setup failure occurred before tests, is archived separately, and does not
count as a passing gate. The final gate starts the server with the environment
already set. Follow-up Mote `bd-01M47MBK0K3VPJ490HDJTQ6Y3C` tracks that
settings failure. No general sbt memory or production inversion improvement is claimed.

## References

- [sbt test grouping and fork controls](https://www.scala-sbt.org/1.x/docs/Testing.html#Forking+tests).
- reframe4s source pin: `5f7152aada60335935843ddc968162f615337415`.
- TemplateFlow composites: `TemplateFlowXfm.Mni6ToMni2009c` and
  `TemplateFlowXfm.Mni2009cToMni6`; the latter is intentionally refused by the oracle.
