# ScalaFIM group-analysis audit — 8 September 2026

**Verdict:** the existing group engine has useful, fast foundations and agrees with independent reference calculations on well-conditioned inputs, but spatial identity defects, fragile numerical preparation, incomplete failure contracts, and poorly calibrated small-sample DL/z inference prevent admission as the default second-level GLM engine.

This audit covers `modules/group` and its public first-level bridge. It does not certify the whole ScalaFIM repository, first-level temporal-noise estimation, a complete BIDS-to-group workflow, or the proposed new group-model UI. Production group sources were not changed. The deliverable is reproducible evidence and six native follow-ups.

## Inputs and verification boundary

- Native tracker audit: `bd-01M20MJY5QG8F7X2ZQENTTWPD7`.
- ScalaFIM HEAD: `fd992a0c85001eb50ba2e78d29c497693ca494dc`; the live repository is dirty. All 31 non-target files in `modules/group` were captured, copied into an isolated provider build, and rechecked against the live source after execution. None changed.
- The isolated build uses the previously prepared PLS Neuro provider candidate: ScalaFIM base above plus candidate patch SHA-256 `5020a0c44433cb4761401521cf3f66c0df5b21d855989598bc7aa00ee6f0e035`, with current group source overlaid. Gale is `83cac90a678d1b8a31c590e0c1b8fc8bf3427161`. This is consumer-candidate evidence, not a claim that the entire mutable native checkout or every published dependency was tested.
- Scala 3.7.4, sbt 1.11.7, MUnit 1.2.1; JVM: Homebrew OpenJDK 25.0.1; Scala.js: Node 26.7.0. The configured CI JDK 17/Node 20 combination was not rerun here, nor was an optimized Scala.js link.
- Independent references: R 4.5.1 `lm`/QR and `metafor::rma.uni(method="FE"|"DL", test="z")`. The receipt contains the complete R session, fixture inputs and output. R warned that metafor and metadat were built under R 4.5.2; reference execution succeeded.
- Existing tests: **50/50 pass on JVM and 50/50 pass on Scala.js**.
- With 16 targeted audit tests: **56 pass / 10 fail on each platform**. Six additional tests pass: three independent-reference tests and three participant-order/sample-block tests. Several failed checks share one underlying cause. These intentionally demanding admission probes live outside ordinary test sources; their failures are evidence, not a passing acceptance gate.
- Logs, commands, source hashes, post-run runtime hashes, simulation counts, and benchmark observations are in [the compressed receipt](group-audit-2026-09-08.json.gz). The test and measurement sources are in [tools/group-audit](../../tools/group-audit/README.md).

## Prioritized findings

### 1. First-level voxel identity can be silently lost

[FirstLevel.scala](../../modules/group/shared/src/main/scala/scalafim/fmri/group/FirstLevel.scala), lines 39–51 and 63–78, checks vector length but copies effects by position without checking `TContrastResult.voxelIndices` against `GroupSpace.VoxelAxis.sampleIndices`.

Reproduction: three subjects represent the same two voxels with values `(1,10)`, `(2,20)`, `(3,30)`. The middle subject supplies its valid reversed representation `(20,2)` at indices `(1,0)`. Against group indices `(0,1)`, the bridge accepts everything and produces means **(8,14)** instead of **(2,20)**. A correct bridge must realign using authoritative identity or return an identified error.

Additional public-API probes show that a result named `different effect` is accepted under an `effect` key, and negative standard errors are squared into positive, accepted variances. An explicit rename operation could be supported, but silent identity substitution supplies no evidence that subjects estimate the same effect. The bridge does not carry first-level degrees of freedom or covariance among jointly modeled responses into `GroupResponse`; those are additional capability gaps for the proposed richer models.

**Follow-up:** `bd-01M20NE2S94V6FQ5D3DXT0C9V6` (priority 1).

### 2. Changing a covariate's units changes fit availability

[GroupGlm.scala](../../modules/group/shared/src/main/scala/scalafim/fmri/group/GroupGlm.scala), lines 50–57 and its weighted counterpart, forms normal equations and chooses Cholesky tolerance from the largest Gram diagonal.

A full-rank 24-participant, four-predictor fixture is fit successfully. Rescale one covariate by `1e-6` and rescale its contrast coefficient correspondingly: the estimand is unchanged. OLS now rejects the design as non-positive-definite. FE and DL return `Right` with NaN estimates. Independent R QR retains rank four and agrees after coefficient back-transformation to **3.33e-16**.

This is a numerical conditioning/unit-handling defect, not a change of the scientific model. Repair should use stable, scale-aware preparation through Gale's public factorization capabilities, with explicit estimability policy. A lower arbitrary Gram tolerance is not sufficient evidence of a repair.

**Follow-up:** `bd-01M20NE4D452Z3E4HVDERQADRP` (priority 1).

### 3. Named contrast and design construction has unsafe edge cases

[GroupContrast.scala](../../modules/group/shared/src/main/scala/scalafim/fmri/group/GroupContrast.scala), lines 66–81:

- `difference("self", "group", "group")` constructs a map with duplicate keys. It collapses to `group -> -1`, so subtracting an effect from itself estimates the **negative effect**. The fixture returns `-0.294623…`, not zero or a refusal.
- `fromStrings` advertises `Either`, but non-finite weights throw `IllegalArgumentException` through the case-class `require`. Empty weights follow the same source path; the combined probe stops at the first non-finite failure, so the empty case is a source finding rather than a separately executed assertion.
- `GroupDesign.twoSample` can accept `(Intercept)` as the non-reference group label and construct two terms with that same name. This violates named binding assumptions. The public API should reject the collision or produce distinct identified terms.

**Follow-up:** `bd-01M20NE5JEN87A0HEKE7AYRK46` (priority 1).

### 4. Weighted numerical failure is represented as successful NaN maps

A globally singular weighted design produces `Right(GroupResult)` containing all-NaN coefficients. The old `GroupInferenceSuite` explicitly requires this behavior, so this is a deliberate legacy contract that needs evolution, rather than an accidental new regression.

A globally invalid design should be rejected before voxel iteration. Any sample-specific failure should retain sample identity and a typed reason, with valid results separately available. Anonymous NaNs cannot support a clear recovery-oriented UI and cannot establish method eligibility.

**Follow-up:** `bd-01M20NE6WF0H5S8244PHCWKXEG` (priority 1).

### 5. DL plus normal-reference inference is poorly calibrated at small sample sizes

The ordinary numerical fixture agrees with `metafor(method="DL", test="z")`: this is not evidence of a faulty port of those formulas. It is evidence that formula agreement and inferential validity are separate gates.

`GroupCalibration` generates independent Gaussian participant effects under a zero population mean, using known first-level variances evenly spaced from 0.04 to 1.0. Total variance is `s_i² + tau²`. Each combination of participant count and between-participant variance has 20,000 independent simulated studies. The output field called `tau` in the probe stores **the variance tau²**, not its standard deviation. There are no nonfinite fits in this experiment.

At between-participant variance **tau² = 0.2**, nominal two-sided alpha = 0.05:

| Participants | OLS/t rejection rate | DL/z rejection rate | 95% Monte Carlo interval for DL/z |
| --- | ---: | ---: | ---: |
| 8 | 4.615% | **11.600%** | 11.164–12.051% |
| 20 | 4.630% | **7.900%** | 7.534–8.282% |
| 80 | 4.805% | 5.580% | 5.270–5.907% |

The receipt also includes tau² = 0 and 1. These are pointwise null-calibration results. They do not certify OLS generally, spatial FWER, robust inference, repeated measurements, estimated first-level variances, misspecified noise models, covariates, or non-Gaussian effects. The intervals above quantify Monte Carlo uncertainty using Wilson binomial intervals; they are not confidence intervals for brain effects.

Evaluate REML/PM and justified small-sample or resampled inference against exact admitted designs. Keeping the current DL/z method for explicitly named reference compatibility is different from making it the default group estimator.

**Follow-up:** `bd-01M20NE80H1DXKQCFQZT15FTTP` (priority 1). Coordinate with existing estimator-extension ticket `bd-01KX2CQZGWBQ3KKX7BWE28X3T2`.

### 6. Dense fitting is promising; weighted allocation and bounded execution need work

Measurements use Apple M3 Max (14 logical CPUs, 36 GiB RAM), macOS 14.3, a fresh JVM per shape/method, 3 GB maximum heap and six active processors. Each process performs two same-shape warmups and three measured fits. Inputs are generated before timing and held constant; each completed fit is followed by a named contrast and BH adjustment, with output consumption and finite-value checks. The same effect and variance matrices are resident for all methods; OLS does not require the variance matrix.

| Method | Participants × terms × voxels | Median fit wall time | Median calling-thread CPU | Calling-thread allocation / fit | Process peak RSS |
| --- | --- | ---: | ---: | ---: | ---: |
| OLS | 100 × 4 × 200,000 | **0.358 s** | 0.181 s | 20.8 MB | 502 MB |
| Fixed-effects WLS | 100 × 4 × 200,000 | **0.961 s** | 0.772 s | 432.0 MB | 1,361 MB |
| DL random effects | 100 × 4 × 200,000 | **2.148 s** | 1.454 s | 859.2 MB | 1,637 MB |
| Fixed-effects WLS | 100 × 12 × 20,000 | 0.799 s | 0.390 s | 162.6 MB | 414 MB |
| DL random effects | 100 × 12 × 20,000 | 1.129 s | 0.739 s | 325.6 MB | 525 MB |

MB are decimal. Allocation is a per-fit calling-thread counter, not peak live memory. RSS covers the entire process including setup, warmups, results, runtime and GC behavior. The receipt includes per-iteration wall/process CPU/thread CPU, GC milliseconds, contrast/FDR timings, checksums, and the successful process exits.

**Limitations:** the shared machine's one-minute load average was approximately 147–160. Early measured iterations show JIT/GC transients, especially for 12 terms. These are exploratory cost measurements, not a clean throughput comparison, SLA, production admission gate, or evidence of superiority to FSL/SPM. Benchmark inputs have only 37 distinct spatial effect patterns and 11 variance patterns; no content cache is used, but this is synthetic dense workload evidence, not real-image execution. Disk IO, spatial correction, GUI interaction, save/reopen and resampling execution are excluded. More warmup, multiple forks and a controlled host are required for stable performance claims.

The implementation allocates small factorization/matrix objects per weighted sample, constructs dense reciprocal/reweighted variance matrices, and retains the first weighted pass during DL reweighting. The existing dense `GroupData` and `GroupEngine` interfaces supply neither streaming execution nor a cancellation contract. A blockwise wrapper and buffer-aware kernels can reduce memory without changing the estimand. Numerical stability and failure semantics must be repaired before selecting optimizations.

**Follow-up:** `bd-01M20NJK4W8RN1TH7EY74WE1KM` (priority 2). Reuse existing manifest-backed block-execution ticket `bd-01KX6G9BFJRERNFRWR17WT0YGN` for IO/planning rather than duplicating it.

## Assurance scorecard

Ratings apply only to the audited module and its dependencies as exercised here.

| Dimension | Rating | Evidence and remaining gap |
| --- | --- | --- |
| ScalaCheck and generators | Missing | Existing group suites use deterministic cases; audit adds explicit generated fixtures and transformations, but no reusable shrinkable generator domain. |
| Reusable laws | Present but incomplete | Shared scenario harness and public-API metamorphic checks exist; no published group conformance law bundle. |
| Test framework / Discipline | Strong | MUnit executes on both supported platforms. Discipline is not needed merely to wrap these numerical contracts. |
| Typeclass lawfulness | Not applicable | This group surface does not define a new algebraic typeclass family. |
| Backend conformance | Present but incomplete | The selected Gale path runs on JVM/JS; alternative backends and optimized links are not certified here. |
| Cross-platform / version CI | Present but incomplete | Both local platforms pass baseline tests. Repository full CI is configured; hosted state and its JDK17/Node20 combination were not run. |
| Numerical assurance | Present but incomplete | Healthy reference and representation checks pass; unit scaling and admission checks fail. |
| Independent oracles | Strong | R QR and metafor are independently executed on the exact same scalar/meta-regression fixture; expected formula agreement is separated from null calibration. |
| Failure / convergence / resources | Present but incomplete | Typed variance capabilities and several validated constructors are useful; exceptions, successful NaNs, missing block/cancellation contracts remain. |
| Work / allocation accounting | Present but incomplete | Audit measures allocation and CPU/wall time. No production per-voxel work or allocation budgets. |
| Compiler discipline | Present but incomplete | Common flags cover deprecation/feature/unchecked; group is outside strict first-level warning settings. |
| Formatting / semantic rewrites | Present but incomplete | Repository has Scalafmt, but this audit did not establish a required group formatting/rewrite gate. No broad formatting changes were made. |
| Compatibility | Unverified | 0.1.0-SNAPSHOT and useful typed interfaces; no group-specific published compatibility baseline or automated guarantee established here. |
| Coverage / mutation | Missing | No group-scoped coverage or mutation receipt was established. Existing first-level facilities do not constitute group coverage. |
| Performance evidence | Present but incomplete | Representative dense measurements completed; controlled-host stability, broader shapes and bounded-memory execution remain. |
| Documentation / release evidence | Present but incomplete | Readable typed model descriptions and module README; no group release closure or complete group workflow qualification. |

No Cats/Cats Effect, Discipline, or sbt-typelevel adoption is required to fix the findings. Improve the existing typed boundaries, independent evidence, stable numerical preparation and resource contracts. Broader tooling decisions should follow a specific assurance need rather than become audit busywork.

## Recommended execution order

1. Repair bridge identity and named contrast construction, with public JVM/JS regressions.
2. Repair scale-aware group preparation and explicit numerical failure reporting through the proper Gale/ScalaFIM boundaries.
3. Admit an explicitly specified, calibrated variance-aware group inference policy; retain compatibility methods under their own names.
4. Qualify bounded execution and allocation improvements using equivalent completed work.
5. Integrate the verified provider contract into the reusable group workbench, including uncertainty availability, participant diagnostics and method-specific results.

The audit is complete. The six linked implementation/admission follow-ups remain open; this record does not claim those defects have been fixed.
