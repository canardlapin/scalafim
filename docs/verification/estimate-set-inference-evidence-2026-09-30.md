# Bounded typed inference evidence prerequisite

This change adds portable coefficient applicability declarations, ten closed
status codes, identified fit/hypothesis planes and optional source/sink
capabilities without a fit dependency. It implements an explicit Core-NIfTI-3
unit wire (`3.0.0`) and one bounded UInt8 NIfTI status stack. Core-1/2/default
writer output remains the old route and refuses evidence-bearing inputs.
Catalogs, TSV projections, collections and pointers remain Core-1.

The implementation starts from `bfec921e7f23dd3b8a4e47505e32ccd5c3c8ebd9`
and is owned by `exec-inference-evidence`, Mote
`bd-01M3RMNS9BHHMFN5WGJJFE8M6H`. Fourteen named source/test/document paths
plus the new fixed fixture directory form the scope. No numerical fit kernel,
producer, pooled path, HDF adapter, dependency pin, build definition or legacy
exporter is changed. Independent review and adoption remain separate gates.

## Contract and resource bounds

Coefficient scope retains the full realized ordered binding axis and an explicit
ordered inferable subset. Unknown learned-response conditioning remains Unknown;
no subspace or full-rank certificate is inferred from a restriction. Hypothesis
planes must name catalog hypotheses with statistic semantics at that observation.
Metadata scales with columns/planes, not the number of samples.

The generic physical reader checks plane inventory, all codes, exact support,
identity scaling, declared 3D/singleton-4D or exact multi-plane-4D geometry,
scanner affine, payload length and SHA256 before returning an owned source.
Producer-specific retained/excluded linkage is not inferred here: an estimable
fit can exclude hypothesis A while retaining B, so cross-plane equality is not
a universal invariant. Numerical product validity remains authoritative.

Status is limited to 32 planes and the qualified provider's 8 GiB payload
budget, both checked before staging. Cell/byte counts use Long and block caps are
checked before allocation; the coverage ledger is disk-backed. The writer's
aggregate 96-handle budget charges three per numerical pair and two for status;
the reader's aggregate 64-handle budget charges two per pair and one for status.
The evidence route therefore supports 31 simultaneous numerical pairs, rather
than claiming unchanged total handles. Compact shared-U coexistence is refused
before staging. Status delivery borrows selected arrays, preserves permutations,
rejects duplicate cells and requires all supported plane cells before seal.

Seal closes payloads/coverage and independently rescans staged status geometry,
length/codes before publishing its digest-pinned reference. Failure/abort cleans
only invocation-owned stages and closes all resources. Gzip uses the existing
cumulative disk staging budget. Failed/cancelled reads leave caller arrays
wholly unchanged: the reader stages only the validated selection's bounded bytes
and copies them into the caller array after all reads and checks succeed. Close
is serialized with reads.

## Independent evidence

The [literal fixture](../../modules/estimates-io/jvm/src/test/resources/estimate-golden/inference-evidence/README.md)
is generated solely by Python standard-library JSON/struct/gzip. It freezes the
generator, expected permutation results, catalog, TSV, products and status bytes
in `SHA256SUMS`. There is no production encoder involved in its authorship.
The fixture exercises all ten codes, a physical support hole, two hypotheses
with different exclusions, nontrivial affine and unavailable conditioning.
Numerical validity is independently NotComputed even where status is Estimable.
This is a fitter-free structural/IO oracle, not a native inference qualification.

The shared suites cover constructor linkage, closed codes, ordered subsets,
absent evidence, codec opt-in and strict field/plane refusal. JVM tests cover
literal readback, sparse plane/sample permutations, borrowed deliveries, block
sizes 1/2/40, coverage, duplicate/missing deliveries, exact retry/no-clobber,
unknown/support-mismatched codes, geometry/scaling/plane/digest/length refusals,
singleton 3D, exact gzip budget, cancellation/throwing callback, prepayload caps,
failed construction, poisoned status writes, corrupt staged seal and resource
release after a throwing first close. Descriptor checks exercise repeated failed
constructors; they establish these tested paths, not a general peak-handle claim.

## Verification receipts

The original candidate gates completed with actual exit code 0. These receipts
predate the cancelled-read repair described below. The original full test logs
contain no warnings, errors or skips; each target has a nonzero completed count.

| Gate | Actual result | Raw receipt |
|---|---|---|
| estimatesJVM/test | 17 passed | inference-evidence-jvm-final-r2.log |
| estimatesIoJVM/test | 56 passed | inference-evidence-jvm-final-r2.log |
| fitEstimatesJVM/test | 17 passed | inference-evidence-jvm-final-r2.log |
| groupJVM/test | 75 passed | inference-evidence-jvm-final-r2.log |
| estimatesJS/test | 17 passed | inference-evidence-js-contract-final.log |
| estimatesIoJS/test | 10 passed | inference-evidence-js-contract-final.log |
| fitEstimatesJS/test | 13 passed | inference-evidence-js-consumers-final.log |
| groupJS/test | 74 passed | inference-evidence-js-consumers-final.log |
| scalafimCompileAll | 89/89 tasks, warning-clean | inference-evidence-compile-all-final.log |
| Fresh fitter-free JVM readback | 12 permuted cells, 3 planes | inference-evidence-fresh-readback-final.log |

The [embedded closure and receipts](../../modules/estimates-io/jvm/src/test/resources/estimate-golden/inference-evidence/verification/README.md)
contain full raw outputs, wrapper metadata, source/input hashes (2,214 files),
commits/trees and file hashes for all 12 actually loaded clean provider builds,
and hashes of actual reader classpath jars/classes/resources. All tested source
and fixture inputs remained unchanged through the gates; all 51 old golden files
retain their hashes. The final verification report and generated receipts are
post-gate documentary additions, explicitly excluded from computational identity.

The actual sbt/fresh-reader runtime is Homebrew Java 25.0.1, sbt 1.11.7,
Scala 3.7.4; Scala.js uses Node 26.7.0. The shell default Java 22 is recorded
separately rather than treated as the build runtime. The standalone Java process
prints Scala LazyVals' JDK25 Unsafe deprecation warning and then the successful
readback marker; its raw receipt retains this warning. CompileAll is warning-clean.

Development logs are preserved: two sandbox startup refusals, a misspelled IO
project ID, the new optional-wire representation correction, a no-staging test
helper correction and the repaired old 33-pair refusal-message regression.
None is represented as qualification. The final full suites supersede them.

No performance, native peak-memory, power-loss, legacy scientific parity,
statistical calibration, HDF5 or whole-Core conformance claim follows from these
checks. Native producer migration and SHA-bound independent review remain open.

## Cancelled-read repair after independent review

Independent review of `edd1feafdde5ad783a4d2771c2d3fb841cdbcc85` found that
late failures exposed a caller-buffer prefix. The new sentinel regressions ran
against the unchanged reader first: actual exit 1, 12 tests run, two failed,
with visible prefixes `2,99` and `2,98,97`. The repaired reader allocates only
`selection.cells` bytes after the source validates selection, capacity and block
cap, then copies them into the caller array only on complete success. It does
not cache the full domain. The suite checks late cancellation, callback exception,
invalid code, support mismatch and truncation after a first successful internal
read; all leave the caller array wholly unchanged and the source open for a later
valid read. Successful reads preserve permutations and unused array capacity.

The repair changes exactly three existing paths: `InferenceStatusNifti.scala`,
`InferenceStatusNiftiSuite.scala` (including the standalone probe), and this
report. All new receipts live in the separate
[repair directory](inference-evidence-repair/README.md). The original literal
fixture and its original receipts remain byte-for-byte history.

| Repair gate | Actual result | Raw receipt |
|---|---|---|
| estimatesJVM/test | 17 passed | [JVM r2](inference-evidence-repair/logs/inference-evidence-repair-jvm-r2.log) |
| estimatesIoJVM/test | 57 passed | JVM r2 |
| fitEstimatesJVM/test | 17 passed | JVM r2 |
| groupJVM/test | 75 passed | JVM r2 |
| estimatesJS/test | 17 passed | [JS contracts](inference-evidence-repair/logs/inference-evidence-repair-js-contract.log) |
| estimatesIoJS/test | 10 passed | JS contracts |
| fitEstimatesJS/test | 13 passed | [JS consumers](inference-evidence-repair/logs/inference-evidence-repair-js-consumers.log) |
| groupJS/test | 74 passed | JS consumers |
| scalafimCompileAll | 89 completed tasks, warning-clean | [CompileAll](inference-evidence-repair/logs/inference-evidence-repair-compile.log) |
| Updated fitter-free fresh JVM | 12 cells, 3 planes; late cancel/callback isolation and unchanged tail | [Fresh reader](inference-evidence-repair/logs/inference-evidence-repair-fresh-readback.log) |

All final gates exit 0: 280 tests in total, no warnings/errors/skips in the full
test and CompileAll logs. The standalone JDK25 LazyVals warning remains in its
successful raw receipt. The first affected JVM batch is retained with actual
exit 1: an unchanged shared-covariance suite's global descriptor equality saw
452 decrease to 450. The final JVM batch serializes IO suites using the transient
command-line `parallelExecution := false` setting; no assertions or tracked
build definitions are changed.

The repair closure checks 2,246 frozen inputs, all 12 actually loaded clean
provider source builds and all 51 old golden hashes; no source changes occurred
while jobs were queued/running. It records the actual fitter-free classpath hashes,
exact Java argv, full raw logs and actual wrapper metadata. Final documentation
and new receipts are explicitly outside computational identity. Local commit and
handoff do not substitute for independent SHA-bound review/adoption or close the
whole-Core, native-producer, performance or scientific qualification gates.
