# Published provider adoption — 2026-10-06

The integrated build uses published immutable source revisions:

| Provider | Revision |
| --- | --- |
| Gale | `54e73f8e8f1218c4cb115a850aa80e1a36c6978f` |
| Image4s | `20c9515495e43dff9d17bc1283a8ca1d56355c4a` |
| Reframe4s | `292c9bc7e0c026d893a4101560aa9a46e0fbcf04` |
| Locus4s | `a67bc87c33b5da8a5dc2cad49919c015b59f3050` |

The starting ScalaFIM commit is `1825d121`, including production Gale ExactSum
adoption. No sibling checkout or process-local source override was used. Each
staged provider HEAD matches its declared pin and canonical GitHub origin.

## Fresh integrated gates

`scalafimCompileAll` passed on JVM and Scala.js, with no compiler warnings.
The following fresh consumer gates passed; historical 3,206-test receipts were
not reused:

| Module | JVM passed | Scala.js passed |
| --- | ---: | ---: |
| locusData | 26 | 26 |
| image | 385 | 355 |
| transform | 177 | 147 |
| spatial | 227 | 202 |
| surface | 268 | 223 |
| motion | 105 | 92 |
| atlas | 116 | 79 |
| mvpaSpatial | 20 | 20 |
| Total | 1,324 | 1,144 |

There are **2,468 passing executions**, plus 12 native surface checks skipped
because their external assets are absent and two explicitly opt-in JS timing
checks skipped. Exact skipped names are retained in the archive. These skips
are not presented as executed clean passes.

The JVM image signed INT8 boundary/scaling case, UInt8 ownership/scaling case,
and signed INT8 atlas label-map rejection case passed. Atlas native
`MniTemplateBridgeFilesSuite` passed **8/8 with zero skips**, including the
full-resolution numerical inverse against the committed oracle. Both 204,735,104
byte TemplateFlow composites were checked against the declared SHA256 before
starting a fresh server with
`TEMPLATEFLOW_HOME=/private/tmp/scalafim-templateflow-20261006`. Scientific grids
and tolerances are unchanged; this establishes no general timing or memory
performance claim.

## Dependency and resource scope

Both atlas JVM and JS `Test/fullClasspath` exports contain exactly one source
root each for Image4s 20c951, Locus4s a67bc8, Reframe4s 292c9b and Ravel 9c5669.
The exports and actual checkout HEADs are retained; no parallel stale image or
spatial provider is on those consumer classpaths.

The runtime was macOS ARM64, Temurin JDK 21.0.12.1+1, Scala 3.7.4, sbt 1.11.7 and
Node 24.21.0. Each server requested a 4 GB heap and two active processors. JS
tests ran in batches of three, three and two modules, with explicit successful
server shutdown after every batch. Compile and JVM batches also ended with
successful shutdowns. Separate Linux/JDK 17 and remote CI gates remain root-owned.

No ScalaFIM consumer code or README API migration correction was needed. This
agent made no production edits, Mote mutations, commits or publication in this
task. Root owns the three provider pin changes and their landing.

## Evidence

[receipt.json](receipt.json) records exact pins, environment, counts and limits.
`logs-and-sources.tar.gz` retains raw logs and gate metadata, dependency exports,
the 1,723-file source fingerprint, skipped cases, native oracle JUnit report,
asset identities, runner and build/toolchain source snapshots. Verify without
running sbt or accessing the network:

```sh
python3 -S docs/verification/provider-adoption-20261006/verify.py
```
