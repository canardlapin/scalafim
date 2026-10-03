# UMVPA M3.E1 adopted whole-call allocation

Status: paired JVM allocation courts and owning JVM/Scala.js regressions pass;
independent source/counter review approves the bounded claims. Immutable commit
verification follows this source receipt.
Packet `bd-01M0Z7JBET61VHWHKSZRJ8HZ7A`; isolated branch
`work/umvpa-finish-20261001`; baseline source parent
`830f35d39ae396ddd119a5fcff6374f5e4331329`. No push/merge/publication.

## Executable court and measurement boundary

`NativeWholeCallAllocationProbe` executes complete adopted scientific calls,
including identity construction, preparation, query/fit and retained results.
The baseline uses the exact committed implementations of `AxisRef.scala` and
`AlderPredictiveAdmission.scala`; optimized calls differ only in the two
profile-proven changes below. Source hashes are retained in
`m3e1-paired-source-hashes.json`. The same probe runs both courts with five
warmup calls then 200 measured calls; comparisons summarize the final 50.
Provider pins are unchanged: Gale
`18d24dbb5056122032b0278f8bad557a9bb1cf23`, Multivar
`f74d631720d65147c51496dcbdd37c01912de1cb`, Alder
`e555bad92307af1c2cbc104aef398cb9d9de88f0`, resample4s
`6bc4172a966c92f1b06811eac64ac2bada9fef9b`.

On Homebrew Java 25.0.1, `com.sun.management.ThreadMXBean` reads cumulative
allocated bytes on the measured caller thread. Numerical checks, result
construction and retained outputs are inside each call; printing and summary
hashing occur after the counter. This measures cumulative allocations, not
peak live bytes, RSS, native backend memory, or other threads. The probe
refuses an unavailable counter. Scala.js has no equivalent counter here; JS
semantic gates do not establish JS allocation numbers.

The ordinary 96-by-64 correlation court constructs nominal observations and
an observation-mean plan with three partitions and four effects, obtains the
actual ordinary relation RDM and consumes six distances. The independent
common-trend correlation oracle gives zero distances.

The native prediction court constructs nominal 96-by-64 observations/targets,
actual native mapping, 16 disjoint four-feature measurements and three grouped
validation partitions. It executes native admission and Swift fits for every
measurement/fold, retaining every result. Analytic balanced-centroid
probabilities, original sample keys, 96-by-four native input reads, 96 targets,
three fits per region and perfect classification are checked. It consumes
1,536 predictions, checksum `1536000000000000`. Foreign and reordered
same-shaped axes are refused before warmup; these checks remain separate from
the allocation total. The printed retained hash demonstrates escaped results;
it is a runtime object hash, not a scientific or cross-run reproducibility hash.

## Measured repairs

JFR sampled allocation stacks in the actual adopted workload found repeated
`NativeAxisMapping.nativeIds` vector construction and per-byte formatter use
in `AxisDigest.sha256Hex`. Sampling weights locate costs; they are not exact
allocated-byte totals. The separately measured caller counter supplies totals.

`NativeAxisMapping` now stores its IDs once in its immutable owning instance.
Construction still validates every ID/key, and there is no global cache or
identity lookup keyed only by shape. Digest hexadecimal conversion now writes
two lowercase characters per byte into one primitive buffer. SHA framing,
input values, SHA-256 implementation and scientific receipts are unchanged.
Existing coordinate digest golden fixtures and independent empty, framed-empty
and multi-block UTF-8 SHA fixtures verify exact identity preservation.

| Court | Before bytes/call | After bytes/call | Allocation reduction |
| --- | ---: | ---: | ---: |
| `ordinary_relation_96x64` | 1,736,248 | 1,059,576 | 38.97% |
| `native_predictive_16x4_96x64` | 110,187,424 | 21,945,944 | 80.08% |

Both runs are unprofiled, use the same warmup/iteration counts and retain all
outputs. `gate122-native-allocation-paired-before.log` and
`gate123-native-allocation-paired-after.log` exit 0. Historical 5.16/61.17 MB
values describe older APIs and are not comparison baselines. Earlier short
probe runs show JVM optimization-dependent variation; no stronger universal
allocation or latency statement is inferred. Elapsed times are retained as
descriptive observations on the shared host, not a qualified speed benchmark.

Before raw-log SHA-256: `c128edb12cb6c189ccc2da82b19ac1806f0de6e44f35969af2fdbf15013df567`.
After raw-log SHA-256: `f8302cd598e5281bc1af91b7bc9d3d4a5621c18106a41f9c9d7c2177d6b34ea2`.
`m3e1-paired-allocation-summary.json` retains per-phase medians, min/max,
iteration completeness and output counts/checksums.

The first profiling attempt, gate119, failed a concurrent confirmation-design
compile and did not execute the court. Gate120 supplied actual sampled
workload stacks; gate121 was an exploratory optimized measurement. Only
unprofiled paired gate122/gate123 establish the table above. Named task-owned
JFR recordings have been stopped.

Evidence directory: `/private/tmp/scalafim-umvpa-finish-evidence-20261001`.
Whole-brain pattern, operational/artifact audit, matched method comparisons,
inference calibration and release retain separate qualification packets.

## Final owning/consumer regression receipt

`gate124-allocation-confirmation-regression.log` exits 0: core 341, readout 45,
dataset 108 and spatial 20 tests per platform; workflow 3 JVM/2 JS, atlas 5 JVM,
and warning-clean `scalafimCompileAll`. Integrated total **522 JVM + 516 JS =
1,038** includes three concurrently developed M4.04 draft tests per platform;
those do not qualify its subsequent repairs. Allocation packet source/consumer
count excludes them: **519 JVM + 513 JS = 1,032**. Existing native parity and
same-shape foreign/reordered identity tests pass, alongside the new digest
framing fixtures.

Raw log SHA-256: `a3b320614ef6a34963dc1b640c97ee80cad934d22dedec94783583d487df0dd4`.
`gate124-allocation-sources.json` freezes 178 owning/consumer Scala sources
(including the adopted M4.03 and JVM probe, excluding the M4.04 draft), SHA-256
`579c69eb98e3fd8bc2a8056b992043443cade13f3a7eee15e4e7ad6f46bf8de8`. These are the exact sources checked in the
local allocation candidate; other source changes retain their own packets.

Independent read-only review recomputed all 200 unique iterations for both
courts, checked original/optimized source hashes, reproduced the UTF-8 digest
fixture, inspected immutable cache ownership, and approved the reported
38.97%/80.08% cumulative-allocation reductions with the stated limitations.
