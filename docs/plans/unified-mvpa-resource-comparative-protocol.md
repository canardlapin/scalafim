# Unified MVPA resource and comparative benchmark protocol

Status: frozen M0.06 protocol, version 1, 2026-09-13

Mote owner: `bd-01M2BNEJ8CKW74BS0SM5QXQNRE`

Authority: [PRD v0.2](unified-mvpa-prd.md), especially PERF-01, PERF-02,
NUM-01, and OPS-01; [inference and known-truth protocol](unified-mvpa-inference-calibration-protocol.md);
and the [migration ledger](unified-mvpa-migration-ledger.md).

This document freezes reference machines, workloads, measurement boundaries,
budgets, numerical tolerances, baseline fairness, and known-truth comparisons.
It is not a benchmark receipt. No latency, memory, numerical, prediction, or
scientific target passes because the protocol exists.

## Source receipt

The ScalaFIM source baseline inspected for this protocol is
`e071e83b3a23bc6f304b1e72625d6281d9cfc9ba` plus the identified untracked M0
planning artifacts. That is a provenance statement, not a clean-tree claim.
The baseline contains optimized `SearchlightClassifierScanner` Ridge-LDA and
the operator-ridge route.

ScalaFIM pins Multivar at
`c4329fc95688929236c942cca889aa67ad17cbe0`; that revision does **not** contain
PLS regression. SIMPLS first appears later in reachable local Multivar history
at `c7cf380644c2c5e239b859549f67cd33ae816ab8`. Neither that later commit nor the
dirty sibling checkout is a production provider admission. The thresholded-PLS
comparison remains unavailable until an immutable PLS-containing provider
revision and its ScalaFIM consumer adapter pass their own admission gate.

## 1. Evidence classes and pass rules

Resource, numerical, comparative, and scientific evidence are separate:

| Code | Evidence | May establish | Cannot establish |
| --- | --- | --- | --- |
| `R` | Reproducible wall time, peak process memory, allocation census, reads, and fit counts | The named workload met its named budget on its named profile | Numerical or scientific correctness; speed on another machine or workload |
| `N` | Independent dense/analytic oracle and conditioning checks | Numerical agreement within the declared domain and tolerance | Statistical calibration or useful prediction |
| `C` | Matched baseline predictions and costs | A task-specific noninferiority or superiority statement | Searchlight replacement or general superiority |
| `S` | Known-truth recovery and inferential calibration | The declared scientific property in the tested populations | A broader population, error model, or estimand |

A workload passes only if its source and manifest hashes match, every requested
output is retained, the numerical gate passes, all five fresh-process runs
complete, median wall time is at or below the time budget, and the maximum
observed peak resident set is at or below the memory budget. A process failure,
silent output omission, shape overflow, changed scientific workload, or
unmeasured material cost makes the row `Unavailable` or `Fail`, not `Pass`.

Budgets never authorize lowering rank, changing a target, removing voxels,
shrinking a family, reducing randomization count, reading holdout evidence, or
switching to a different estimand. An admitted equivalent execution route may
be selected only before execution and must return the same typed estimand
within the numerical tolerance.

## 2. Reference profiles

### 2.1 JVM reference workstation `JVM-M3MAX-36-v1`

The physical reference recorded at M0.06 is:

- MacBook Pro `Mac15,11`;
- Apple M3 Max, 14 CPU cores (10 performance and 4 efficiency), arm64;
- 36 GB unified memory; and
- macOS 14.3 build `23D56`.

The qualification runtime is OpenJDK 21, Scala 3.7.4, sbt 1.11.7, with
`-Xms1g -Xmx6g -XX:+UseG1GC`. The current interactive shell observed while
writing this protocol uses JDK 22; that observation is not a qualifying
benchmark. A receipt records the full JDK build, VM, collector, Gale backend, native-library versions,
CPU architecture, OS build, and process flags.

Reference runs use AC power with low-power mode disabled, nominal thermal
state, no competing benchmark/build workload, a single benchmark process, and
at most 10 compute workers. The worker count is frozen per row and may not be
increased for one method only. Hardware serial numbers, user names, host names,
and filesystem locations are excluded from public receipts.

### 2.2 Scala.js reference `JS-M3MAX-NODE22-v1`

Scala.js uses the same physical workstation, the repository's Scala.js Node
environment, Node 22.x, one Node process, and
`--max-old-space-size=4096`. The receipt records the exact Node/V8 build and
flags. The current interactive shell observed Node 26.7.0; it is not the frozen
Node 22 qualification profile.

Scala.js must preserve scientific and numerical semantics, but it has smaller
reference shapes and separate time budgets. A JVM timing result cannot stand
in for Scala.js. Conversely, Scala.js need not match JVM wall time.

### 2.3 Alternate and changed profiles

Linux CI, another Apple chip, a different JDK/Node major, a different numerical
backend, or a changed worker count produces a separate comparison receipt. It
may diagnose regressions but cannot be pooled with the reference court. An OS
patch or runtime patch is recorded; if it changes a budget decision, both old
and new receipts remain visible and the profile version is advanced.

## 3. Timing, warm/cold, and repetition boundaries

Compilation, dependency resolution, and sbt startup are not algorithm time and
are reported separately when they occur. Synthetic fixture generation is
outside the timed region but its resident arrays count toward peak process
memory whenever the timed operation retains them.

The following terms are fixed:

- **Process-cold:** a fresh JVM or Node process, no ScalaFIM object/cache from a
  prior workload, classes loaded by the workload driver. OS filesystem cache is
  uncontrolled unless the receipt can prove otherwise and is labeled
  `Unknown`, never `Cold`.
- **Prepared:** the identified source, target, graph, split plan, and fixed
  hyperparameters exist, but no fitted preprocessing, covariance, model,
  factorization, or result from the measured stage exists.
- **Resident fit:** a valid admitted fitted artifact and the outputs named by
  the workload are live; the original neural matrix is absent unless the row
  explicitly requires it.
- **First query:** the first query after a resident fit, including any lazy
  precision products or query cache construction.
- **Steady query:** 10 untimed queries with distinct predeclared identities but
  the same shape have completed; 30 measured queries then use different
  predeclared ROI identities. Values are consumed by a checksum so dead-code
  elimination is impossible.
- **I/O process-cold:** a fresh process opens and reads a new source handle.
  Logical bytes and calls are exact. Filesystem-cache state, physical bytes,
  decompression, and remote transfer are reported independently.

End-to-end rows run in five fresh processes. The receipt stores all five
values, their median, maximum, and failure status; it does not discard warm-up
or slow outliers. Steady-query rows use the median and empirical 95th percentile
of 30 observations. JMH microbenchmarks use at least 3 forks, 5 warm-up and 10
measurement iterations, and the GC allocation profiler; they supplement rather
than replace process-level rows.

Wall, user, and system time plus absolute maximum resident set are collected by
an external process supervisor. Timing runs have allocation tracing disabled.
A separate allocation run uses JFR allocation events on JVM and V8 heap/external
memory accounting on Node. Tracing overhead is not compared to the time budget.
Peak process RSS is the strict total-memory gate because JVM/native/backend
scratch can escape language-level allocation counters.

## 4. Numerical accuracy and conditioning

Every resource workload first passes a downscaled independently calculated
oracle using the same plan. Required comparisons are:

- scalar/objective/prediction values: combined absolute and relative tolerance
  `1e-10` for direct dense routes and `1e-8` for admitted iterative routes;
- fitted subspaces: relative Frobenius error of projection matrices at most
  `1e-8`, not arbitrary component-column equality;
- iterative solve: declared relative residual at most `1e-8`, finite objective,
  and no false convergence;
- probability rows: finite, nonnegative, row sums within `1e-12`, and class
  columns exactly identified; and
- repeated fixed-backend executions: output checksums tolerance-compatible at
  `1e-8`; bitwise identity is not claimed unless separately demonstrated.

The oracle grid has `n=40`, `p=64`, `q=8`, requested rank `r=4`, and residual
rank `h=3`, with target condition numbers `1e2`, `1e6`, and `1e10`. At `1e10`,
either the declared `1e-6` scale-aware tolerance is met with a conditioning
diagnostic or the typed method refuses the case. Arbitrary clipping, silent
rank reduction, or success without a residual/conditioning receipt fails.

All byte and operation arithmetic uses checked 64-bit or wider intermediates.
Dimensions whose required address or byte count overflows are rejected before
allocation or source access.

## 5. JVM resource court

### 5.1 Fixed-hyperparameter global fit `J-FIT-100K`

The prepared inputs are:

- `n=1,000`, `p=100,000`, `q=256`, `r=16`, residual rank `h=16`;
- row-major Float64 `X` and `Y` resident at timing start;
- a deterministic undirected graph connecting each voxel to offsets
  `+/-1`, `+/-31`, and `+/-997` modulo `p`, with duplicate/self edges removed;
- fixed feature/target centering and scaling, fixed hyperparameters, relative
  objective tolerance `1e-6`, and iteration cap 200; and
- retained forward patterns, filters, target relationship, residual diagonal
  and factors, convergence trace, identifiers, and receipt.

The timer begins before preprocessing, supervised initialization, covariance
estimation, factorization, and the first objective evaluation. It stops only
after retained outputs are validated and checksummed. Data generation and I/O
are excluded, but `X`, `Y`, graph storage, preparation copies, worker scratch,
backend scratch, covariance fitting, initialization, and outputs count toward
peak process memory.

Budget: median wall time at most 300 seconds; maximum peak RSS at most 8 GiB;
10 workers. Every run must converge or return the same predeclared typed
nonconvergence. A nonconverged result cannot pass the fit budget merely because
it stops early.

### 5.2 Complete nested selection `J-SELECT-20K`

This is not the fixed-fit workload. It uses `n=600`, `p=20,000`, `q=64`,
`r<=8`, `h<=8`, 10 contiguous acquisition blocks of 60 rows, and the same
degree-six graph construction. Five outer folds test block pairs
`(0,5)`, `(1,6)`, `(2,7)`, `(3,8)`, and `(4,9)`. Inside each outer training
set, the eight remaining sorted blocks are paired first-with-fifth through
fourth-with-eighth to make four inner folds.

Every method receives exactly 18 predeclared configurations per inner fold.
The pattern-first grid is rank `{2,4,8}` by noise rank `{0,8}` by spatial
penalty `{0,0.001,0.01}`. Selection uses sample-pooled inner loss, followed by
one outer-training refit and untouched outer evaluation. All feature/target
preparation, covariance fitting, initialization, early-stopping state, and
warm starts are training-scoped. A warm start may connect ordered candidates
inside one authorized training scope but never crosses an outer fold.

The row counts every one of the 365 candidate/final fits, source/operator reads,
preparations, retained inner summaries, outer predictions, failures, and
selection receipts. Budget: median wall time at most 90 minutes; maximum peak
RSS at most 12 GiB; 10 workers; five fresh-process repetitions. Lowering the
18-candidate grid or returning only successful folds fails the workload.

### 5.3 Resident hard-ROI prediction `J-ROI-5K`

Starting from the resident `J-FIT-100K` artifact with the neural matrix absent,
derive a valid hard-ROI head for 5,000 sorted voxel identities. The ROI consists
of every twentieth voxel beginning at zero. It must restrict the
diagonal-plus-low-rank covariance, not crop a whole-brain decoder.

Budgets: first query at most 2 seconds; steady-query empirical p95 at most
1 second; incremental peak RSS at most 512 MiB. A separate resident held-out
batch of 100 rows reports exact source reads and prediction latency; its steady
prediction p95 is at most 250 milliseconds, excluding but separately reporting
source read time. The first-query relaxation from the PRD's proposed one-second
row makes lazy precision construction visible rather than hiding it in setup.

### 5.4 Gaussian conditional information `J-CINFO-100K`

Starting from resident factors at `p=100,000`, `r=16`, and `h=16`, compute all
voxel conditional-information values, including the required shared precision
products. The first complete map takes at most 10 seconds with incremental peak
RSS at most 1 GiB. A second complete map reusing only documented shared products
takes at most 2 seconds. Both agree with explicit leave-one-voxel-out Gaussian
oracles at the numerical tolerance on the downscaled fixture.

### 5.5 Frozen projected-rank confirmation `J-RANK-2K`

Use confirmation `n=500`, projected dimensions `P=Q=16`, four nuisance columns,
and the frozen M0.05 stepwise closed-testing procedure. Execute the observed
identity plus 1,999 non-identity transforms. The row includes nuisance-basis
construction, observed CCA, every stepwise refit, closed p-values, checksums,
and the full receipt.

Budget: median wall time at most 60 seconds; maximum peak RSS at most 1 GiB;
one worker. This deliberately replaces the PRD's provisional 1,000-transform,
30-second row with the admitted protocol's 2,000 total reference values at a
linearly doubled budget. It is not voxelwise multiplicity timing.

### 5.6 Voxelwise family confirmation `J-VOXEL-2K`

Use confirmation `n=500`, `p=100,000`, `r=16`, four nuisance columns, and one
complete `p*r` component-specific family. Execute the identity plus 1,999
non-identity transforms with one common transform per family replicate. Retain
observed statistics, adjusted p-values, maxima, local failures, and receipt;
do not retain a `B*p` or `B*p*r` null array.

Budget: median wall time at most 15 minutes; maximum peak RSS at most 8 GiB;
10 workers. Every family member and all 2,000 reference values must complete.
Any smaller diagnostic family is labeled a pilot, not this court.

### 5.7 Process-cold evidence I/O `J-IO-800M`

Read a chunked local Float64 evidence source of exactly 1,000 by 100,000 values
(800,000,000 logical payload bytes) plus its identified axes into the same
operator/evidence route used by `J-FIT-100K`. Chunk rows are 20 and the consumer
requests all rows and voxels in ascending identity order.

Budget: median process-cold open-and-read time at most 60 seconds; maximum peak
RSS at most 3 GiB; exactly 50 logical chunk reads and no duplicate payload read.
The receipt separately reports metadata reads, logical bytes, decompressed
bytes, backend calls, and whether the OS page-cache/physical-byte state is
known. An unknown OS cache does not become a disk-throughput claim.

## 6. Scala.js resource court

The portable court uses the same algorithms, identifiers, seeds, and numerical
oracle at reduced shapes:

| ID | Workload | Budget on `JS-M3MAX-NODE22-v1` |
| --- | --- | --- |
| `JS-FIT-10K` | Fixed fit `n=256`, `p=10,000`, `q=32`, `r=8`, `h=8`, degree-six graph, cap 200 | Median <=180 seconds; peak RSS <=4 GiB; one worker |
| `JS-ROI-2K` | First and steady hard-ROI head from resident fit, 2,000 voxels | First <=5 seconds; steady p95 <=2 seconds; incremental RSS <=512 MiB |
| `JS-CINFO-10K` | All conditional-information values from resident `p=10,000`, `r=h=8` factors | First <=10 seconds; incremental RSS <=512 MiB |
| `JS-RANK-2K` | `n=250`, `P=Q=8`, identity plus 1,999 transforms | Median <=180 seconds; peak RSS <=2 GiB |
| `JS-VOXEL-2K` | `n=250`, `p=2,500`, `r=8`, identity plus 1,999 family-complete transforms | Median <=300 seconds; peak RSS <=4 GiB |
| `JS-IO-80M` | Process-cold 20-row chunks for a 400 by 25,000 Float64 source (80,000,000 payload bytes) | Median <=60 seconds; peak RSS <=1.5 GiB; exactly 20 logical chunk reads |

Each row uses five fresh Node processes except steady queries. Oversized inputs
that exceed a declared Node resource policy must refuse before partial
allocation. This court does not claim that a browser tab can hold the JVM
reference dataset.

## 7. Complete live-memory and work accounting

Every receipt provides checked expected-byte arithmetic and observed process
evidence for:

1. neural/target source buffers and decoded/materialized copies;
2. sample, target, feature, graph, ROI, split, and nuisance metadata;
3. training-fitted preprocessing and target-metric state;
4. initialization, covariance diagonal/factors, model factors, solvers, and
   graph workspaces;
5. per-worker and backend/native scratch plus queued work;
6. tuning candidates, warm starts, retained validation predictions/losses, and
   selection state;
7. requested fitted outputs, local failures, checksums, receipts, and
   serialization buffers; and
8. JVM/V8 heap, off-heap/direct buffers, native libraries, code cache, and GC
   reserve as observed by absolute process RSS.

Each category is `Measured`, `Derived`, `IncludedByProcessRSS`, or `Unknown`.
Language allocation counters may leave native scratch
`IncludedByProcessRSS`; they may not label it zero. If a strict in-process
allocator cannot own a backend cost, the budget is enforced by process
isolation and absolute RSS. If neither measures nor bounds a material category,
the memory row is unavailable.

Worker-local storage is multiplied by the maximum simultaneous worker count,
not the average. Retained outputs are present at the RSS measurement. Forced GC
or clearing required results before measuring peak is prohibited. Operator
applications and evidence reads have exact counters; `operator-native` is not
assumed to be cheaper than a small dense block.

## 8. Matched comparative court

### 8.1 Methods and adapters

The minimum court contains:

1. the proposed pattern-first spatial reduced-rank method;
2. optimized Ridge-LDA searchlights for categorical targets, using the current
   `SearchlightClassifierScanner` semantics until its migration replacement;
3. a strong whole-brain linear ridge/logistic head over the same prepared
   evidence, using the admitted Alder/Gale route;
4. Multivar SIMPLS with training-only univariate screening thresholds as the
   low-dimensional thresholded-PLS comparison; and
5. an ablated non-spatial reduced-rank fit with the same rank/covariance family.

Current ScalaFIM contains the searchlight and operator-ridge routes. A later,
not-yet-admitted Multivar revision contains SIMPLS PLS regression, and ScalaFIM
has no thresholded-PLS production adapter at this source receipt. M5.05 may add
a benchmark-only, source-hashed adapter after provider admission; its absence
is `Unavailable`, not a fabricated baseline result or a reason to omit the row.

### 8.2 Matched design and tuning

Comparisons use the `J-SELECT-20K` outer/inner split identities. All methods see
the same training and test sample IDs, raw target coding, nuisance columns,
training-fitted scaling, primary metric, and outer predictions. Each gets 18
candidate configurations per inner fold:

- pattern-first: the frozen rank/noise-rank/spatial grid;
- searchlight Ridge-LDA: penalties `10^(-5 + 9*j/17)` for integer `j=0..17`;
- whole-brain ridge/logistic: the same 18-penalty grid;
- thresholded SIMPLS: component count `{2,4,8}` by retained-feature fraction
  `{0.005,0.01,0.025,0.05,0.10,0.20}`; and
- non-spatial reduced rank: rank `{2,4,8}` by ridge penalty
  `{0,1e-4,1e-3,1e-2,1e-1,1}`.

Screening statistics, scaling, covariance, initialization, and PLS components
are refit inside each inner training split. Searchlight centers and membership
are predeclared from anatomy, not selected using outcomes. Methods may share
immutable raw evidence and split plans, but no fitted preparation. Early
stopping receives the same maximum number of training-loss evaluations and
must report unused evaluations.

Categorical tasks have four balanced classes. Primary loss is sample-pooled
negative log likelihood; accuracy and Brier score are secondary. Continuous
tasks have `q=32`; primary loss is sample-pooled standardized squared error,
with multivariate `R-squared` and per-target correlation secondary. A method
that cannot return the primary target product is unavailable for that court;
accuracy cannot substitute for log loss and a classifier cannot substitute for
multiresponse prediction. Ridge-LDA participates only in the categorical court;
local feature-ridge and SIMPLS participate only in the continuous court unless
an independently calibrated probabilistic head is later frozen.

### 8.3 Known-truth comparison populations

All cases use phase `simulator` of the M0.05 deterministic stream scheme and
the same realized noise/splits across methods. The base continuous population
has `n=600`, `p=20,000`, and `q=32`: the first four unit-variance target factors
carry signal and the remaining targets are noise controls. The categorical
population has four balanced classes repeated within each acquisition block;
its three identified contrasts replace the four continuous factors. Base
support `k` is voxel ordinals `1000*k` through `1000*k+249` for zero-based
`k=0..3`. The base loading magnitude is `0.30` and residual SD is one.

The cases are:

| ID | Truth perturbation | Required separate observation |
| --- | --- | --- |
| `K0-NULL` | No brain/target relationship | Finite predictions, no apparent support-recovery success, inference handled only by the calibration court |
| `K1-COMPACT` | Four compact coherent supports, standardized loading `0.30` | Prediction, forward-pattern and support recovery |
| `K2-SUPPRESSOR` | Zero-forward-loading voxels correlated `0.8` with informative residuals | Forward map stays null; conditional predictive importance and ablation utility remain distinct |
| `K3-REDUNDANT` | Eight exchangeable informative regions plus a diffuse loading over 40% of voxels | Redundancy stability, diffuse recovery, and prediction without arbitrary single-region credit |
| `K4-CHECKER` | Alternating voxel signs inside each coherent anatomical support | Fine-scale signed recovery; a smooth all-positive map fails even if prediction is good |
| `K5-WRONG-GRAPH` | The fit graph permutes 25% of voxel identities across supports while truth/anatomy remain fixed | Sensitivity to wrong adjacency, with no relabeling of the truth graph |
| `K6-COVARIANCE` | Residual covariance is diagonal, admitted diagonal-plus-rank-8, and block-Toeplitz outside the fitted family | Prediction, uncertainty, and pattern sensitivity by covariance specification |
| `K7-RANK-ROI` | True global rank 2 plus a target dimension present only in a 1,000-voxel ROI; fits request rank `{1,2,4,8}` | Global rank misspecification and the locally unique dimension; regional/global estimands remain distinct |
| `K8-TARGET` | Target correlation `0.7` and separate standardized Student-t(5) target/noise variants | Rotation/subspace stability and non-Gaussian sensitivity |
| `K9-SUBJECT` | Subject shifts SD `0.5`, spatial displacement 0/2 voxels, and one deliberately wrong alignment | Subject stability, identity/alignment refusal, and no manufactured agreement |

In `K2`, the first 25 active voxels per support are each paired with a
zero-loading voxel 500 ordinals later at residual correlation `0.8`. In `K3`,
two 250-voxel regions per factor use magnitude `sqrt(22.5/500)`, and the diffuse
variant uses 8,000 voxels at magnitude `sqrt(22.5/8000)`, preserving the base
loading energy `250 * 0.30^2`. `K4` alternates signs by voxel ordinal. `K5`
applies a seed-frozen permutation to exactly 5,000 graph identities for fitting
only. `K6` compares residual identity, diagonal-plus-rank-8, and 32-voxel
Toeplitz blocks with correlation `0.6^distance`. `K7` retains only the first two
global factors and places a third factor of magnitude `0.15` exclusively in
ROI ordinals `15000..15999`. `K8` gives the first four targets equicorrelation
`0.7` and separately standardizes Student-t(5) innovations to unit variance.
`K9` uses the `REPEAT24` layout from M0.05; its wrong alignment is a declared
identity mismatch on the lowest subject identity that must refuse rather than
enter metric aggregation. The `K3` diffuse support is exactly voxel ordinals
`0..7999`.

Each case runs 50 independently generated datasets for comparative uncertainty;
this is not the 10,000-dataset inferential calibration court. Pattern/support
recovery is computed only where truth defines it. Metrics include held-out
primary loss, accuracy or `R-squared`, forward-pattern correlation after valid
subspace alignment, projection-matrix error, support AUPRC, conditional-
importance rank recovery, ROI prediction, component/subspace stability, fit
failure, wall time, fit count, reads, allocation, and peak RSS. No aggregate
score may hide a failed case.

The offline compute ceiling is 100 reference-workstation process-hours per
method and known-truth case, which covers 50 times the 90-minute
`J-SELECT-20K` target plus contingency. Exceeding that ceiling is a resource
failure for the comparative court; it does not authorize fewer datasets,
unmatched tuning, or omission of a slow baseline. Courts may be scheduled on
multiple identical profiles, but each dataset/method process retains its own
complete receipt and no process uses more than 10 workers.

### 8.4 Comparative decisions

Uncertainty resamples independent acquisition blocks for within-subject tasks
and subjects for group tasks; it never resamples overlapping CV folds as units.
All 95% intervals are paired by dataset and outer test unit.

- On `K1-COMPACT`, noninferiority to the best available matched strong baseline
  requires the upper 95% interval for pattern-first minus baseline log loss to
  be at most `0.02` nats and the upper interval for the standardized-MSE ratio
  to be at most `1.05`. Accuracy or `R-squared` may additionally be no worse by
  more than `0.02` absolute.
- A superiority statement requires the entire 95% interval to exceed a
  predeclared meaningful improvement: `0.02` nats lower log loss, `0.02`
  absolute accuracy/`R-squared`, or 5% lower standardized MSE. Statistical
  separation smaller than that is reported as no meaningful superiority.
- Scientific recovery metrics are reported by case. Prediction cannot repair
  a wrong forward-map sign, suppressor interpretation, adjacency identity, or
  ROI-only-dimension claim. A method may be predictively admitted while a
  localization claim remains unavailable.
- Searchlight and shared-subspace results retain their different estimands.
  Winning one synthetic or empirical task never licenses a blanket
  “searchlight replacement,” “best localization,” or “lightning fast” label.

Real-data comparisons may demonstrate usefulness only after an immutable
dataset/license/manifest is filed. They use the same split, preparation, tuning,
and metric rules but do not replace the known-truth court. M0.06 does not select
or download a human dataset.

## 9. Cost-probe, output, and failure discipline

A cost probe can read synthetic data or explicitly authorized training
evidence. Its plan declares maximum reads, operator applications, fits,
workers, allocations, time, and outputs before running. Attempted holdout access
fails and is recorded as an exposure attempt. A training timing sample is not a
global bound, model-quality result, or permission to inspect confirmation data.

Every receipt contains:

- protocol/profile/workload/source/provider/manifest hashes;
- full runtime and hardware profile without personal machine identifiers;
- warm/cold class, worker count, repetitions, raw times, GC, RSS, allocation,
  reads, applications, fits, iterations, and convergence;
- requested/retained output inventory and checksum;
- expected and measured bytes with unknown categories preserved;
- numerical and semantic gate status separate from resource status;
- every baseline configuration, split, prediction, metric, local failure, and
  unavailable reason; and
- evidence exposure plus cancellation/retry status.

Cancellation keeps completed units visible and never reports the row passed.
Retries use immutable work-unit IDs and cannot contribute twice. The court does
not require resume support; if absent, resume is a typed refusal. Benchmark
artifacts use existing archive/estimate facilities when persisted rather than a
new runtime or registry.

## 10. Planned artifacts and exact commands

Implementation packets own these future paths:

- `benchmarks/mvpa-jvm/src/main/scala/scalafim/fmri/mvpa/benchmark/UnifiedMvpaBenchmark.scala`
- `tools/benchmark/umvpa-budgets.json`
- `tools/benchmark/run_umvpa_reference.sh`
- `tools/benchmark/finalize_umvpa_receipt.py`
- `modules/mvpa/js/src/test/scala/scalafim/fmri/mvpa/benchmark/UnifiedMvpaJsResourceSuite.scala`
- `docs/benchmarks/receipts/unified-mvpa-reference-v1.json.gz`
- `docs/audits/unified-mvpa-comparative-v1.json.gz`

The future driver preserves these entry points:

```sh
tools/benchmark/run_umvpa_reference.sh --profile JVM-M3MAX-36-v1 --court resource
tools/benchmark/run_umvpa_reference.sh --profile JVM-M3MAX-36-v1 --court comparison
tools/benchmark/run_umvpa_reference.sh --profile JS-M3MAX-NODE22-v1 --court resource
python3 tools/benchmark/finalize_umvpa_receipt.py --check docs/benchmarks/receipts/unified-mvpa-reference-v1.json.gz
sbt "mvpaJVM/testOnly scalafim.fmri.mvpa.benchmark.UnifiedMvpaNumericalOracleSuite"
sbt "mvpaJS/testOnly scalafim.fmri.mvpa.benchmark.UnifiedMvpaPortableOracleSuite"
```

The shell driver runs one bounded workload at a time and closes every process;
it does not invoke the repository-wide Scala.js test aggregate. Receipt
finalization refuses mismatched hashes, missing rows/repetitions, lowered
dimensions, incomplete outputs, unknown strict costs, or stale profile/runtime
versions.

## 11. Handoff boundary

M0.06 closes the protocol decision only. M3.09 owns enforceable resource
policies and cost probes; M3.12 and M3.14 own fit/operations qualification;
M4.10 owns confirmation/query latency; M5.05 owns matched comparative results.
The M0.05 scientific calibration counts remain authoritative even when their
resource court is expensive.

Changing reference hardware, a workload shape, fit/tuning count, target metric,
split, output inventory, numerical tolerance, worker count, warm/cold class, or
budget requires a versioned protocol update before results are viewed. A failed
target may motivate optimization or an honestly revised future product target;
it may not silently change the measured scientific question.
