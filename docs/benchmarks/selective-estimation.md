# Selective first-level performance qualification

The `SelectedFirstLevelBenchmark` JMH fixture measures the public selected and
full-fit routes on the same resident response data. It covers eight condition
coefficients, one contrast and 64 FIR coefficients, with two runs, 320 scans per
run and 16 nuisance columns per run. Shared OLS and inverse-covariance pooling
remain separate scientific routes. Each trial compares every requested estimate
against the existing full-fit route before any measurement.

Run from the native build:

```sh
sbt 'fitBenchJVM/Jmh/run .*SelectedFirstLevelBenchmark.* -p voxels=8192 -p blockVoxels=512 -t 1 -wi 3 -w 1s -i 5 -r 1s -f 2 -prof gc -jvmArgsAppend "-Xmx4g -XX:ActiveProcessorCount=1" -rf json -rff /tmp/selected-first-level.json'
```

Use a new output path. The fixture reports its backend, dimensions, largest full
baseline read and maximum estimate difference. The selected reader enforces the
block bound. The existing shared full-fit route reads all response voxels during
preparation; its larger read is part of the faithful baseline, not allowed in the
selected route. The full baseline retains its usual extra products. Its timing
includes preparation, whereas `selectedPreparedDataset` reuses the compiled
operator; `selectedPreparation` measures that compilation separately.

These are resident-data measurements, including native block conversion and
selected sink delivery to JMH's blackhole. They exclude filesystem reads,
decompression, source-revision verification and durable result publication.
Allocation profiling is not peak RSS or peak live memory. Preparation and run
pooling may require internal variance/covariance even when no uncertainty maps
are retained. Do not label these results whole-brain or end-to-end throughput.

A six-case smoke run at 64 voxels and block size 32 passed with JDK 22, one active
JVM processor and Gale's pure backend. Maximum full/selected difference was
1.71e-14 (rounded upward). This verifies the fixture, not its short-run timing.
A failed JMH fixture can still leave the process exit code zero, so qualification
must also require all expected cases and equivalence records. The consumer
runner `tools/run-selected-estimation-probe.py` in PLS Neuro checks that contract
and captures exact source, patch, logs, JMH data and process receipts.

Native qualification issue: `bd-01M1ZAMKEPXXHF8342H0K70NSH`. File-backed provider execution and resource evidence are recorded below; production scientific publication remains separate.
Independent checks of actual benchmark inputs now pass as recorded below.

## Completed resident-data baseline

Apple M3 Max, macOS 14.3, JDK 22, Gale pure backend, one benchmark thread and
one active JVM processor. Two forks with five measured iterations each cover
18 cases; all 36 trial equivalence checks pass, with maximum difference
`2.80e-14` (rounded upward). Both owned launcher and child processes completed.
These are 8,192-voxel measurements with 512-voxel blocks and 640 total scans.

| Representation | Route | Full baseline ms | Prepare ms | Prepared selected ms | Allocated MiB: full → selected |
| --- | --- | ---: | ---: | ---: | ---: |
| condition | shared | 1432.8 | 2.22 | 114.9 | 732.5 → 340.6 |
| condition | pooled | 928.0 | 1.47 | 950.4 | 525.8 → 451.3 |
| contrast | shared | 1567.2 | 6.85 | 118.6 | 732.5 → 340.1 |
| contrast | pooled | 861.5 | 5.78 | 916.3 | 525.8 → 450.6 |
| FIR | shared | 3556.5 | 17.09 | 161.6 | 753.1 → 344.1 |
| FIR | pooled | 6814.7 | 6.00 | 2635.3 | 3675.4 → 1493.8 |

Values are JMH mean milliseconds and allocated MiB per operation. The full
baseline includes preparation and its normal extra products; the prepared
selected column excludes the separately measured compilation. Allocations are
not peak live heap or RSS. The fixed case order and shared workstation are
limitations; timing differences between the otherwise identical condition and
contrast full baselines illustrate run-to-run variation.

Shared OLS shows a large reduction in these resident-data cases. Pooled condition
and contrast requests show no speed improvement, although allocation decreases.
Pooled FIR improves from roughly 6.81 s to 2.64 s and allocates about 1.46 GiB
instead of 3.59 GiB per operation. Its required variance work and joint pooling
still need profiling; arithmetic selection alone is not sufficient for the small
pooled models. No end-to-end or whole-brain speedup is inferred.

The [machine-readable receipt](receipts/selective-resident-2026-09-08.json.gz) and
[exact source/log archive](receipts/selective-resident-2026-09-08-logs.tar.gz) retain the
production patch, provider contract, benchmark, runner, raw JMH measurements and
smoke/full process receipts. Production patch:
`58ff8af4dd57cfeabff7bc9a59f5e2323c2aa9e3cfd61e35596f502d333d7adf`.
Additional qualification source SHA-256:
`d26f7603972a3ef13c292acc46bfa8e52de4b53b25ca1e21e91e8c931dbea9b2`.
The benchmark is native ScalaFIM code; it is currently an explicitly captured
additional source in the isolated candidate, separate from its production patch.

## Independent benchmark checks

The [independent evidence archive](receipts/selective-numpy-2026-09-08-logs.tar.gz)
contains the actual realized designs, run projections, readout weights and three
response columns (source voxels 0, 31 and 63) for all six baseline benchmark
models. PLS Neuro's `tools/check-selected-estimation-fixtures.py` recomputes
coefficients with NumPy 2.4.3 SVD and performs full joint-precision run pooling
before applying the requested weights. All six comparisons pass; maximum
absolute estimate error is `1.76e-14` (rounded upward). These are selected response
samples from the same designs, not independent checks of every timed voxel.
The archive also includes the hand-calculated rational pooling counterexample,
complete source/output and digests. The original resident timing source and
production patch remain preserved separately.

Gale's existing `83cac90` portable QR multi-RHS improvement now passes its native
gates, Multivar artifact gates, Image4s gates, all 26 affected coherent provider
platform suites and 51 application headless probes. The [coordinated admission
receipt](receipts/selective-gale-83cac90-2026-09-08.json.gz) retains exact revisions,
patches, counts, logs and independent NumPy checks. It includes cross-repository
application references whose relative paths belong to PLS Neuro. No published
remote dependency closure is implied. The table continues to describe the
original `d55fe2` baseline until new measurements are admitted.

The additional native fixture exporter writes realized designs, response samples,
run projections and readouts for independent calculations. Every trial reports a
fingerprint of the actually loaded Gale QR implementation. The benchmark runner
copies and verifies its compiled runtime before launching JMH, keeping concurrent
build output changes separate from performance measurement.

## Resident measurements with the qualified Gale QR update

The same 18 cases and 36 full/selected comparisons pass with Gale `83cac90`;
maximum difference remains below `2.8e-14`. Every fork verifies the loaded QR
implementation. The hardware, JDK, effective 4 GiB heap, one active processor,
block sizes, warmup and measurement counts match the original baseline. Runtime
classes and jars are now copied and verified before JMH starts.

| Representation | Route | Full baseline ms | Prepare ms | Prepared selected ms | Allocated MiB: full → selected |
| --- | --- | ---: | ---: | ---: | ---: |
| condition | shared | 809.9 | 1.89 | 110.9 | 732.5 → 340.6 |
| condition | pooled | 432.0 | 1.46 | 289.7 | 525.7 → 451.2 |
| contrast | shared | 813.4 | 6.20 | 116.0 | 732.5 → 340.1 |
| contrast | pooled | 425.7 | 5.77 | 289.1 | 525.7 → 450.4 |
| FIR | shared | 1732.3 | 8.04 | 170.1 | 753.1 → 344.1 |
| FIR | pooled | 5528.3 | 5.95 | 783.7 | 3675.9 → 1493.6 |

Pooled condition and contrast execution now takes about 289 ms, versus 426–432 ms
for the updated full-fit baseline. Pooled FIR takes about 784 ms versus 5.53 s.
Compared with the archived old-Gale selected route, these pooled cases improve
by approximately 3.2–3.4 times. Shared selected execution stays near its earlier
level, while the full shared baseline also improves substantially.

Allocation changes little: selected condition/contrast pooling still allocates
about 450 MiB per operation, and pooled FIR about 1.46 GiB. These results establish
the benefit of the existing upstream QR improvement and identify remaining
allocation/pooling work. They do not establish a memory bound or file-to-result
throughput. Run order and shared-workstation variation remain limitations.

The [new receipt](receipts/selective-gale83-resident-2026-09-08.json.gz) and
[complete source/log archive](receipts/selective-gale83-resident-2026-09-08-logs.tar.gz)
retain raw JMH samples, copied-runtime inventories, exact scripts and independent
NumPy inputs from the loaded implementation. The executed ScalaFIM candidate
patch is `dd9265ad541ce23fb9c60aa5892d1db2726a790eacbb338ae0ac445919d26359`;
the additional benchmark source is
`9dc647753b6aaaa5729b77e52e47ecc665a70e47c1c9270b34f6f0b00cea4716`.
The benchmark changes since the baseline only add input export and implementation
identity; timed method bodies are unchanged.


## File-backed estimates and resource measurements

A separate native harness now reads 32768 voxels over 640 scans from NIfTI,
uses 512-voxel fit blocks, and writes every requested output to a test-only
little-endian binary sink with `force(true)`. The models retain two runs and 16
nuisance columns per run. It uses the same pure backend, JDK 22 and one active
processor as the resident comparison, with the same 4 GiB maximum heap for both
routes. The reference machine has 36 GiB physical memory. Each scientific case
has two fresh JVM forks, each executing a first and repeated pass.

All 24 estimator processes pass. Every requested value in both passes agrees
between selected and full routes: 24 written-output comparisons, maximum absolute
difference `2.80e-14` (rounded upward). Independent NumPy SVD and full joint
precision calculations read sampled values from the actual timed sink files;
all 24 checks pass, with maximum difference `1.05e-14` (rounded upward). The first
and repeated outputs are byte-identical within each process. All 24 deterministic
cancellation checks stop after one delivered block and one read.

The table reports the two repeated-pass times and the two whole-process RSS
high-water marks as ranges. These are two observations, not confidence intervals.
Times include payload reads/decoding, native conversion and fitting, sink writes
and fsync. Selected preparation is separate; the full route includes its own
preparation. First-pass values, preparation, source verification and every phase
measurement are retained in the receipt.

| Representation | Route | Selected seconds | Full seconds | Selected RSS MiB | Full RSS MiB |
| --- | --- | ---: | ---: | ---: | ---: |
| condition | shared | 0.344–0.357 | 2.321–2.376 | 287.7–288.2 | 640.0–643.7 |
| condition | pooled | 1.037–1.070 | 1.781–1.889 | 305.1–305.2 | 441.0–460.0 |
| contrast | shared | 0.333–0.376 | 2.320–2.355 | 284.2–287.2 | 644.5–648.5 |
| contrast | pooled | 1.021–1.036 | 1.711–1.754 | 305.2–385.5 | 441.3–443.8 |
| FIR | shared | 0.687–0.861 | 6.104–6.169 | 291.1–297.0 | 730.7–745.7 |
| FIR | pooled | 3.094–3.128 | 22.118–22.150 | 311.9–328.4 | 762.6–767.5 |

Selected execution makes exactly 64 reads of at most 512 voxels. Full pooled
execution has the same read count/bound. Full shared OLS makes 65 reads, including
one 32768-voxel preparation read. This explains part of the resource difference;
the selected reader never allows that larger read. For shared condition/contrast
estimates, payload reads and decoding take about 0.25seconds of the 0.33–0.38second
repeated pass. Dense pooled FIR spends about 2.82–2.85seconds in the remaining
fit/conversion work. Those aggregate observations identify where further work
can help; they do not isolate individual QR, variance or pooling kernel times.

The expanded fixture is 160 MiB; gzip sizes are about 154 MiB. Verified staging
misses take 9.09–9.16seconds and verified reuse 0.185–0.216seconds. A missing
staging entry is not a cold operating-system cache. Source hashing also touches
input bytes before each process. This synthetic float64 fixture has deliberately
low compression and does not represent every scanner, mask or storage system.
Staging and source verification are reported separately from estimator execution.
Native issue `bd-01M2092J4Y3D9FRS9VRCKG2EAK` owns the newly reproduced tiny-gzip-read
bottleneck; its candidate is not included in these historical timing numbers.

Each pass records JVM memory-pool peaks. Pool peaks are independent high-water
marks and must not be summed as a simultaneous heap peak. Four further FIR
processes enable JVM Native Memory Tracking, separately from the timed comparison.
Their output hashes exactly match the qualified files. Tracked non-heap committed
memory at exit is 85.0–90.8MiB; raw category records retain their own high-water
marks. NMT reserved/committed memory is not live heap, total process RSS or a
measurement of every external allocation. This pure-backend run does not admit
new vector/FFM dispatch thresholds; the earlier backend qualification remains a
separate claim.

The [file receipt](receipts/selective-file-2026-09-08.json.gz) and [source/log archive](receipts/selective-file-2026-09-08-logs.tar.gz) preserve
all source, commands, logs, raw timings, memory records, output hashes, sampled
oracles and frozen-classpath inventories. Generated bulk payloads and runtime
binaries are omitted; exact generators and digests are retained. The production
candidate was `545cc4aa4a41febef8ef50386d98b619b8e738e549260e930d96fc0611499063`,
with Gale `83cac90`. The additional native qualification sources are recorded
by digest; they are separate from the production patch. The native harness can
be reproduced through the PLS Neuro `tools/run-selected-file-probe.py` runner.

These results establish bounded file-backed selected execution and equivalent
written numerical outputs. They do not establish a production scientific output
format, project save/reopen, PLS handoff or complete desktop throughput. The
remaining per-voxel pooling allocation and exact optimization opportunities have
a bounded native follow-up, `bd-01M208TXSYF5Y5GZQ2DY4GZP8S`. Fixed temporal adjoints
are verified algebraically but transformed model admission remains separate;
no approximate whitening, independent-output-basis optimization, GPU route or
reduced precision is silently enabled.


## Qualified staging improvement and admission decision

Native ScalaFIM issue `bd-01M2092J4Y3D9FRS9VRCKG2EAK` fixes the measured
staging bottleneck by matching the gzip decoder buffer to the existing bounded
64 KiB copy window. The default decoder buffer produced about 315000 small reads
on this input, repeatedly checking usable space. Every integrity, cancellation,
expanded-size, aggregate-quota and pre-write free-space check remains intact.

Across four fresh JVM measurements, verified staging misses now take
**1.39–1.52 seconds**, versus **9.09–9.16 seconds** in the earlier file qualification.
Verified reuse remains 0.17–0.22 seconds. Complete expanded-content hashes and
compressed cache keys match exactly. These timings include the public staging
verification and header-opening work, with the same 160 MiB inputs. They do not
claim cold operating-system caches.

The fix passes 84 native dataset JVM tests and 63 dataset Scala.js tests, including
a new multi-window exact-byte/tight-limit regression. All 51 application headless
probes pass with 108 required PASS records, unchanged execution-time source
snapshots and no owned descendant processes remaining. Only the two qualified
staging source/test files were synced to the native checkout; unrelated work is
preserved. The [staging receipt](receipts/selective-staging-buffer-2026-09-08.json.gz) contains exact source/patch,
compiled-class identity, raw measurements, native tests, consumer logs and the
subsequent prose-only contract-status update. Current production candidate:
`93e2eb33d97c741753a43afc49aec3b355f9612612bda9561da1e3b24c686f80`.
Estimator code is unchanged from the separately recorded file-fit measurements.

SE3 admits the exact pure-backend CPU routes, executable examples, resident and
file-backed resource evidence, plus this verified staging improvement. Remaining
pooling-allocation optimization is explicitly tracked, without changing the
estimator or blocking this admission. Optional transformed/GLS/backend routes
require their own capabilities and evidence; they are not silently selected.
The workbench, optional retained run-local nuisance products, bounded trace
inspection, scientific output lifecycle and PLS handoff remain SE4/W3/W5/SE5 work.
This provider admission is local development evidence, not product completion or
publication.
