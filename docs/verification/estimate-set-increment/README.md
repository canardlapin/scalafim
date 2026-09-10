# Estimate-set implementation evidence — 10 September 2026

This is the retained **first-increment** snapshot. [Continuation evidence](continuation.md)
records subsequent provider publication, covariance and group changes. Source hashes
and test counts below apply to that earlier candidate, not the current working tree.

> Historical qualification record. The provider alignment on 10 September
> merged the canonical image4s implementation into `ec56b34` and replaced the
> temporary provider override with published pins. See the current
> [alignment receipt](../gale-alignment/README.md). The commands and statements
> below describe the original isolated qualification and its then-current pins.

This qualifies the first implementation increment described in
[estimate-sets.md](../../estimate-sets.md), not the complete V0 profile or the
six-stage replacement plan.

## Exact source

ScalaFIM base: `a2d6f95f2a7703b015c156262699138db1d8b70c`.
Image4s base: `18ffdce67fd05f5bcd28336650edcae891da6265`.

- [scalafim-source.patch](scalafim-source.patch) contains the 55 changed
  implementation/build/test files used in the isolated candidate.
- [source-manifest.json](source-manifest.json) records their exact SHA256 and
  byte counts. Documentation is outside this source patch.
- [image4s-canonical-output.patch](image4s-canonical-output.patch) retains the
  prerequisite forward-port onto canonical image4s main.

Both retained patches pass reverse-application checks against their tested
candidate trees. The isolated ScalaFIM candidate was reconstructed from base
source, without concurrent HRF changes or the concurrently added Gale override.
Its implementation files match this working tree byte for byte. Its `build.sbt`
contains only this increment's module and aggregate changes atop the base.
Neither candidate has been published; the ordinary image4s pin still lacks the
new APIs. All qualification below uses the explicit development override
`-Dscalafim.image4s.build=/tmp/estimate-image4s-provider`.

## Gates

| Module | JVM tests | JS tests |
|---|---:|---:|
| image | 305 | 284 |
| dataset | 76 | 62 |
| fit | 295 | 286 |
| archive | 15 | 10 |
| estimates | 8 | 8 |
| estimates-io | 8 | 3 |
| fit-estimates | 4 | 3 |
| **Total** | **711** | **656** |

All passed in bounded sbt invocations in the isolated source candidate. The
final IO/producer JVM run includes catalog consistency, header admission and
resource-close fixes. The last changes were JVM-only; shared/JS sources match
the passing JS runs. The image4s prerequisite independently passed core 96 JVM /
92 JS and NIfTI 55 JVM / 37 JS tests.

`scalafimCompileAll` passed without compiler warnings in the working tree with
the provider override. This full compile included concurrent unrelated work;
it is not an isolated full-repository compile certificate. The final JVM-only
changes were subsequently compiled and tested in isolation. The source-only
candidate has no `.git` directory, so sbt's Git version lookup prints a harmless
`fatal: not a git repository` during project loading.

[test-logs.json.gz](test-logs.json.gz) retains the full successful logs, labelled
by scope. To reproduce the focused gates, apply the patches to separate
checkouts of the bases above and pass the reconciled provider's absolute path:

```sh
sbt -Dscalafim.image4s.build=/absolute/path/to/reconciled-image4s \
  'imageJVM/test' 'datasetJVM/test' 'fitJVM/test' 'archiveJVM/test' \
  'estimatesJVM/test' 'estimatesIoJVM/test' 'fitEstimatesJVM/test'
sbt -Dscalafim.image4s.build=/absolute/path/to/reconciled-image4s \
  'imageJS/test' 'datasetJS/test' 'fitJS/test'
sbt -Dscalafim.image4s.build=/absolute/path/to/reconciled-image4s \
  'archiveJS/test' 'estimatesJS/test' 'estimatesIoJS/test' 'fitEstimatesJS/test'
```

## Bounded payload and relocation

[EstimateProcessProbe](../../../modules/estimates-io/jvm/src/test/scala/scalafim/estimates/io/EstimateProcessProbe.scala)
writes 256 maps on a 64 × 64 × 16 grid in blocks of 1,024 cells. Its 16,777,216
Float64 values occupy 128 MiB, plus validity and the on-disk coverage ledger.
The writer and a separate reader each run with `-Xmx64m`. The archive directory
is renamed between processes; the reader verifies that the fitter is absent
from its classpath and reads reordered cells through the public API.

[heap-and-relocation.json](heap-and-relocation.json) retains both successful
runs. [verify-probe.py](verify-probe.py), an independent Python standard-library
checker, verified every numerical and validity cell, not just the reader's
sampled cells. The numerical checksum is `214459251425280`.

Reproduce by exporting `estimatesIoJVM/Test/fullClasspath`, running
`scalafim.estimates.io.EstimateProcessProbe write <root>` and then `read
<relocated-root>` with that classpath and `java -Xmx64m`. Run
`python3 verify-probe.py <relocated-root>` for the independent check.

This is one local workload. It does not establish full performance conformance,
power-loss durability at every commit boundary, browser IO, HDF5, gzip staging,
pair-axis covariance, pooled-fit persistence, or downstream scientific admission.
