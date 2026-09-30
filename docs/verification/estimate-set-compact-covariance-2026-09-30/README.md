# Compact normalized covariance: bounded local qualification

The opted-in Core-2 unit layout stores shared normalized Float64 covariance U
once per product/observation. Default Core-1 output, historical development
decoding, existing NIfTI representations and frozen fixture bytes remain intact.
The native shared-OLS adapter uses its already prepared covariance matrix; no
new inverse, refit, scientific model or admission policy was introduced.

This is a review candidate for Mote `bd-01KX6G9B8R86MRBZ9S8K8F5G7V`, based on
`43673482d226e43e97af8a7ecbfc2e89ccb557cf` (tree
`4721fb4ad9171364a7ef2ca97aeadf3047647e02`). The accompanying
[qualification.json.gz](qualification.json.gz) contains the exact source-file
manifest, provider revisions, compiled runtime closures, complete raw logs and
command metadata. Its source manifest excludes this README, itself and
SHA256SUMS to avoid a self-hash cycle. The final local commit/tree and all
committed blobs are bound separately by the execution handoff; independent
review and parent integration remain required. No publication or full-ticket
closure is claimed.

## Actual gates

All builds used the serialized execution `run-sbt.py` controller, with a 3 GiB
sbt heap, four active processors and source frozen while queued/running.

| Module | JVM passed | JS passed |
|---|---:|---:|
| estimates | 13 | 13 |
| estimates-io | 44 | 9 |
| fit-estimates | 17 | 13 |
| group | 75 | 74 |

`io-compact-platform-r2.log` completed all eight targets, exit 0 in 69.424389 s.
After strengthening gzip failure cleanup and pair-only sink eligibility,
`io-compact-final-affected.log` reran the affected JVM IO and JVM/JS producer
targets, exit 0 in 48.892723 s. Only the JVM measurement probe changed afterward;
its final test compilation passed in 26.485478 s.
`io-compact-compile-all-final.log` completed all 89 compile targets, exit 0 in
136.593082 s; a scan of the complete raw log found no warning/error records.
The test/build runtime was Homebrew Java 25.0.1, Scala 3.7.4, sbt 1.11.7 and
Node 26.7.0. Direct probes used OpenJDK 22+36-2370 on macOS 14.3 arm64.
These are local platform receipts, not hosted JDK 21 or browser filesystem tests.

## Independent scientific and consumer evidence

[generate_shared_bundle.py](generate_shared_bundle.py) uses Python standard
library literals, without the production encoder or copied production JSON.
It emits a complete Core-2 unit with a Core-1 catalog/TSV projection, scalar
NIfTI payloads and two distinct U tables. Nonlexical Unicode estimand IDs,
repeated display labels, asymmetric signed covariance, two observations,
sparse support and reordered caller axes are intentional. The table upper
triangles are `[4,-1,0.5,9,2,16]` and `[1,0.25,-0.5,2,0.75,3]`.
Stored scale maps use NIfTI slope 2/intercept 1, independently of the variance
multiplier applied to U. Unequal and zero scales have literal rational expected
Sigma values. JVM physical readback checks raw U, support, every supported
matrix and the separate scaling operations. Shared JVM/JS checks exercise the
strict table, axes and representation contracts.

The fixture manifest is 9,348 bytes, SHA256
`d5339bc15aede2aac9f918211127ce6b4d0a683ff7115a1bb4e524e9b7f9adde`.
Regeneration reproduced all 15 artifact files plus SHA256SUMS exactly. All 35
frozen pre-existing fixture files and `NiftiRepresentation.scala` were compared
against the base Git blobs and remain byte-identical.

Tests cover malformed/missing/invalid pairs, nonfinite values and negative
diagonals; scale validity, negativity, nonfiniteness and inconsistent target
scales; indefinite U with zero scale; wrong precision/descriptor/invariance,
identity/ordered-axis/unknown-field/wire conflicts; exact and exceeded cumulative
pair/byte caps before payload access; exact expanded cell budgets; duplicate and
missing writes; callback failure/throw, cancellation and owned abort cleanup.
A malformed later shared table after earlier gzip NIfTI staging is opened 20
times with unchanged descriptor count and temporary-stage inventory. The 32
actual NIfTI-pair/64-handle cap is retained alongside separately bounded shared
observations, including cross-arm observation-invariance checks.

Actual native OLS writes at block sizes 1 and 2 retain named operators, effects,
SE, residual variance, df and outcomes. Compact delivery is counted once per
pair/observation and agrees with the Core-1 route and analytical U
`[0.2,-0.3,0.7]`. A pair-only sink advertises the shared capability without the
old sample-expanded covariance capability. A separate relocated group consumer
reopens two fitted compact units, checks marginal variance 1.6, residual df 2
and joint Sigma `[[5.6,-2.4],[-2.4,1.6]]`, under explicit geometry/method
admission. Its isolated classpath omits both the fitter and producer classes;
only its own entry-point classes are copied into the consumer closure.

## Resource observations

[run_probes.py](run_probes.py) holds the shared build lock and hashes every class
file/jar in the IO, fit-writer and isolated group-consumer closures before and
after the campaign. All 34 direct JVM jobs passed their expected exits. Each
uses `-Xmx64m -XX:ActiveProcessorCount=2`. Resource writers/readers use K=64,
P=2,080, block512 and N=2,048/8,192; every root is relocated before a fresh reader.

| Layout | N | U leaf bytes | Covariance coverage entries | Sampled writer heap bytes | Writer RSS bytes |
|---|---:|---:|---:|---:|---:|
| Core-1 | 2,048 | 38,339,264 | 4,259,840 | 43,326,368 | 172,130,304 |
| Compact | 2,048 | 200,714 | 2,080 | 42,252,160 | 151,257,088 |
| Core-1 | 8,192 | 153,354,944 | 17,039,360 | 51,280,496 | 209,993,728 |
| Compact | 8,192 | 200,714 | 2,080 | 40,183,744 | 166,232,064 |

Heap usage is sampled every 5 ms and can miss shorter peaks; the configured
67,108,864-byte heap limit and successful completion are separately recorded.
RSS is the macOS `/usr/bin/time -l` maximum resident set size, including nonheap
costs. Reader measurements, complete wall times and raw resource counters are
in the receipt. No quiet-machine, repeated-performance or speed-superiority
claim follows from these descriptive timings.

The synthetic storage probe has two scalar products (effects and residual
scale), whose payload sizes are 2,360,704/9,438,592 bytes and coverage entries
262,144/1,048,576. Compact unit metadata is 45,335/131,351 bytes, including an
ordered support array with N entries; catalog 13,895 bytes and TSVs 1,177 bytes
are unchanged. Compact verified numerical payload bytes total
2,561,418/9,639,306. Four value/validity handles and two scalar coverage handles
remain; no sample-by-pair covariance file or ledger is allocated. All completed
writers report zero staging files; resource readers stage no gzip bytes.
Native OLS marginal SE costs are separately exercised by producer/readback tests
and the two-unit group probe, not hidden inside this two-product storage case.

Independent Python streams every scalar/U payload and validity byte with block512,
checks header/axis metadata and every pinned length/digest, then checks all
4,259,840/17,039,360 conceptual upper-triangle Sigma entries from actual U and
scale against rational expected values. Fresh IO readers select six raw cells
and check a full reordered 64-by-64 matrix. All four sources are fitter-free.
These observations establish constant compact U bytes/coverage as N changes,
not constant total metadata, compute cost or whole-job memory.

## Publication and failure history

Retained Core-1 process interruption checks ran seed/halt/fresh-read at four
boundaries (12 jobs). The compact probe ran four more boundaries (12 jobs):
before table publication, a published table leaf with no unit, after actual
compact unit seal, and after actual collection/discovery CAS. Halts use exit 87;
fresh readers see either the complete old unit or the complete new unit.
The table-only action deliberately publishes an orphan leaf through the same
store primitive; it is a reachability check, not an injected interruption inside
the production `seal` method. Existing concurrent/stale CAS and compact exact
retry/no-clobber tests remain green. No power-loss or exhaustive crash proof is
claimed.

All earlier failed raw logs are preserved. An initial build lacked sandbox cache
access; two draft local helpers had forward-reference compile errors; a probe
draft used invalid Scala floating literals. A real regression test found metadata
staging files left by identical/conflicting retries. `LocalEstimateStore.writeText`
now deletes only its own stage and parent on completion/failure; the failing test
and later full passes are retained. The first direct resource attempt completed
its Java checks, but `/usr/bin/time` exited 1 because sandbox sysctl access was
denied. The full campaign reran with those counters available. Final source hashes
match the tested source, and no failed draft is the submitted candidate.

HDF5/provider slice writing, pooled persistence, legacy exporter replacement,
workflow and PLS Neuro W5 adoption, registered-frame expansion, broader access
performance and power-loss qualification remain open. Scientific and consumer
admission gates retain their existing meaning.
