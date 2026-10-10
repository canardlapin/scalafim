# PHRF comparative evaluation: pilot runner design and slicing (v2.1)

Status: design only, no implementation. Mote `bd-01M3TNWW63XC71C3EF2PQME3P8`.
Version: v2.1 (revises fc698d6e after independent review, ACCEPT-WITH-NOTES, then corrects the parity scope; see section 8).
Binds to: protocol v3.2 (`phrf-comparative-evaluation-protocol.md`, commit `1b8c67a5`,
sections 3-9, 11) and integration source `8d5b0e51`, plus the S0 receipt
`docs/verification/phrf-cmp-s0-20261001.md` (commit `256cd1b0`), which supplies the pinned PHRF
call sequences and preview timings used below.

Owner decisions applied: O1 (one global pooled AR(1) for every arm, PHRF included), O7
(custodian backlog-worker-20260930, with a separate runner reviewer), O2-O6 and O9 at their
recommended defaults. **O8 is reversed:** parity against R of every comparator that feeds the
pilot (CAN, INF3, FIR, LSA, LSS, rLSS; pilot sigma sets D) is a **v0 gate before the pilot**
(section 6, slice S11).

## 0. Findings that shape the plan

| # | Finding | Status |
| --- | --- | --- |
| B1 | Trial ML executor reachable. | **Closed by S0**: 69/69 ML suites pass on JVM and JS; condition and trial ML smoke fits run end to end on harness data. |
| B2 | PHRF takes one fixed shared AR plan only. | **Resolved by O1**: one global pooled AR(1), estimated externally from the common pre-fit, passed as `CanonicalTemporalWhitening.Shared` with a matching `FitConfig`. |
| B3 | No LOROCV alpha driver; fold score undefined in the protocol. | Open; new code in S5, definition frozen in v0 (O2 default, section 2.2). |
| B4 | rLSS and native LSA absent; parity fixtures absent. | New code (S4) and v0-gating parity (S11). |
| B5 | Generator defaults differ from the protocol (`n_voxels` 200 vs 40, `n_pool` 100 vs 4000, no baseline 100). | Frozen overrides in v0 (O3, section 1.3). |
| B6 | npz reader scope. | Reader handles **STORED float64/int32 `.npz` only**, for generator output and for GLMsingle output re-emitted by the adaptor (section 1.1). |
| B7 | (S0) The PHRF condition arm must go through `ProfileHrfPlan.fromTrialEvents(alpha = 0)`, the compact route, because `fromFixed` refuses AR whitening. Observed-family admission binds the whitening plan, so it is built **after** rho is known. | Section 2.1. |
| B8 | (S0) Nuisance rank trap: `BaselineBasis.Constant` already emits one intercept per run, so the generator's own per-run intercept columns make every trial preparation rank deficient. | S1/S2 drop the generator's intercept columns (same span); tested. |
| B9 | (S0) The FIR pre-fit overestimates rho in trial cells (0.375 against a truth of 0.3; 0.284 in the condition cell). | S2 specifies the pre-fit (section 2.0). |
| B10 | (S0) No alpha cache through the public API. | Preparation is paid per fit; cost model in section 5.1. |

The v1 design said `fromFixedCondition`; the real name is `ProfileHrfPlan.fromFixed`, and it is
**not used** (B7).

## 1. Data ingestion

### 1.1 Choice: native Scala reader of STORED npz; GLMsingle I/O re-emitted as STORED

The generator `.npz` is a byte-deterministic STORED zip of `.npy` v1.0 members. The Scala
reader (`Npz`, JVM) uses `java.util.zip.ZipFile` and a small header parser, and **refuses
everything else**: compressed members, fortran order, dtypes other than `<f8` and `<i4`,
`.npy` v2/v3, object or string arrays. A raw-float64 export in the generator is rejected
because it changes `generator_code_sha256` and adds a second artifact chain.

**GLMsingle I/O (review F1; choice recorded here and in S1/S6).** `run_glmsingle.py`'s CLI
writes `np.savez_compressed` with a string `meta_json`, which the strict reader would refuse.
Rather than widen the reader to DEFLATE and strings, the new adaptor
`tools/phrf-comparison/glmsingle/from_generator.py` (v0-frozen, section 5.3):

- reads the generator `.npz` itself (it is Python and has numpy), splits runs by `run_id`, adds
  the baseline, and calls `run_glmsingle(...)` in-process;
- writes its result as a **STORED float64/int32 `.npz`** (the generator's `npz_bytes`
  convention: sorted members, fixed timestamps) holding only numeric arrays, plus a JSON
  **sidecar** (`<id>.glmsingle.meta.json`) with the wrapper meta (commit, versions, timing,
  `cpu_s`, onset quantization, log tail, noise-pool size);
- the Scala bridge reads both with the strict `Npz` reader and the repo's JSON codec.
  `run_glmsingle.py` itself is untouched.

Both output files get SHA-256 entries in the dataset ledger.

### 1.2 Binding to the manifest

For every dataset `PhrfDatasetLoader` does the following and refuses on any mismatch with a
typed `IngestRefusal` (no exceptions across the boundary):

1. Parse `<CELL>__dNNNN.manifest.json`; require `schema == "phrf-gen-npz-1"`.
2. Require `root_kind == "pilot"`, `root_hex` equal to the 64-bit pilot root in manifest v0,
   `root_denylisted == false`, and every `stream_denylist_check[*].hit == false`.
3. Require `generator_code_sha256` equal to the value frozen in v0 (supplied to the loader, not
   read from the manifest being validated).
4. SHA-256 the whole `.npz` and compare with `npz_sha256`; for each array hash the raw member
   bytes and compare with `arrays[name].npy_sha256`.
5. Check dtype and shape against the manifest `arrays` table and the v0 `cells.json`
   (V, T, N, runs, `n_pool`, `tr`, `tr_aligned_onsets`).
6. Record `input_sha256` per dataset and a per-arm derived-input hash (section 2.0 requires the
   whitened arrays to be hash-identical across native arms).

Typed views: `FitInputs` (y, y_pool, nuisance, sample_time, run_id, ev_onset, ev_cond, ev_stim,
ev_run, ev_duration) and `ScoreTruth` (everything else, including `ev_onset_true`). Fitters take
`FitInputs` only.

### 1.3 Pilot generator parameters (frozen in v0, O3)

- Scored voxels `n_voxels = 40`; `n_pool = 4000` for T-TX-fast and T-TX-jit, `0` for T-TS-fast.
- `tr_aligned_onsets`: true for T-TX-fast and T-TX-jit, false for T-TS-fast (as `cells.py`). The
  adaptor asserts zero onset quantization for aligned cells.
- **Deviation structure (F4, confirmed in `generate.py`).** All three pilot trial cells use
  stimulus-persistent deviations: `T-TX-fast` and `T-TS-fast` use the default `persistent`
  (per-stimulus draw `0.5 m z_stim` plus per-trial `0.25 m z_trial`); `T-TX-jit` uses `heavy`
  (the same two-term structure with t-distributed draws) plus latency jitter. The generator's
  `Cell.deviation` comment lists `"none"`, but the code treats every value other than `iid` as
  persistent, so **no zero-deviation cell exists**; the alpha-0 check (section 2.2) therefore
  builds its data runner-side (`y - signal + signal_condition_mean`), with no generator change.
- **Baseline.** The adaptor adds `100.0` **per voxel to every sample of `y` and `y_pool`**
  (scored and pool voxels alike) for the GLMsingle input only. Native arms receive `y`
  unmodified; a test asserts native outputs are unchanged when 100 is added (per-run
  intercepts absorb it; see B8). v0b verified baselines 1, 100, 1000.
- Disk: 3 trial cells x 20 datasets x about 40 MB is about 2.4 GB, more than this machine's free
  space. The pilot runs on the custodian host.

## 2. Per-method invocation

### 2.0 Common preparation (shared by every native arm; slice S2)

1. **Nuisance.** Take the generator `nuisance` (6 columns per run: intercept, linear, quadratic,
   3 cosines), **drop the intercept columns** (B8), and let `BaselineBasis.Constant` supply
   them. The span is identical; a test asserts equal projectors and equal rank.
2. **AR(1) pre-fit (specified, B9; revised twice on 2026-10-01 by owner decision; rev 3 supersedes rev 2 and P2 variant A).**
   Purpose: one global rho per dataset, free of any comparator's misspecification.
   - **Rev 3 owner decision (2026-10-01, from the owner directly).** Each cell kind uses its own
     AR design, and the result must be confirmed on fresh seeds. **Condition cells**: pooled
     AR(1) from residuals of the condition design (FIR 1 s bins over `[0, H)` per condition,
     H = 32 s, plus the generator nuisance with intercepts; 102 columns), with correction.
     **Trial cells**: pooled AR(1) from residuals of the variant-A trial design (that design plus
     one canonical-shape column per trial; 264 columns), with correction. One AR per cell,
     shared by every arm (O1 holds). The estimator is
     `ArEstimation.fitNoise(residuals, layout, ArFitOptions(Fixed(1), Global, exactFirstAr1 = true),
     EstimationPolicy.DesignCorrected(design, CorrectionBudget.Adaptive(25)))` (via
     `NoiseFit.estimate`; ceiling 25 = `CorrectionBudget.DefaultMaxLag`); the layout is the
     generator run segments with no censoring. The design is selected by the type `CellKind`
     (`ArDesignLevel`), recorded in `ArFitProvenance`. sigma2 (2.0.7) uses the same design as the
     AR fit in both kinds (for trial cells the trial design, unchanged from rev 1).
   - **History (selection run, not a verdict).** Rev 1 (hand-rolled estimator, trial design):
     trial rho_hat -0.357, condition 0.341 (FAIL). Rev 2 (corrected estimator, condition design
     for every kind), harness root `7a3c91d50b44e2f1`, datasets 0 to 49: condition 0.3012 (PASS),
     trial 0.4743 (FAIL; the condition design leaves trial deviations in the residual), and, as a
     comparison row only, the trial design with the corrected estimator gave 0.2987. **The rev 3
     trial design was chosen after seeing that comparison row**, so the rev 2 seeds are spent for
     selection and cannot be the verdict.
   - **Reason for the correction.** The hand-rolled estimator (lag-1/lag-0 uncentred, times
     `n/(n-p)`) used a scalar factor where the true bias is a linear map on the autocovariance
     vector gamma. R simulations of an S2-like design: with 264 columns (about 12 residual df per
     run) even the exact correction gives 0.27 to 0.29; with the condition design, uncorrected is
     0.283 and corrected is about 0.30.
   - **FRESH-SEED DECLARATION (made before any fresh-seed run).** The verdict run uses a new
     HARNESS-kind root derived deterministically as the first 8 bytes (big-endian) of
     `SHA-256("phrf-cmp-s2-rev3-fresh-seeds-20261001")` = `0x80dd06a7d4666a7b`
     (`root_hex` `80dd06a7d4666a7b`, `root_kind` harness), through the existing SplitMix64
     derivation (`phrf_gen/seeds.py`). It is distinct from the selection root
     `0x7a3c91d50b44e2f1`; the heavy test asserts the two roots differ and that the derived
     stream-seed sets of both roots (cells `T-TX-fast` and `C-TX-.5`, datasets 0 to 49, all seven
     purposes) are disjoint from each other and from the protocol denylist. Cells: `T-TX-fast`
     (2-voxel pool) and `C-TX-.5`, 40 scored voxels, **50 datasets per kind** (indices 0 to 49).
     Criterion: `|mean(rho_hat) - 0.3| <= 0.02` per kind, **unchanged**. **The verdict is final:
     one run, both cells, current branch code, no tuning of design, budget, ceiling or criterion
     follows; if either cell fails the result is reported as a failure.**
   - `FirPrefit` owns no AR estimator. **Provenance** (recorded in the prep output, sealed with
     the ledger): design level, policy, requested budget, requested and used lag, residual df,
     per-run `RunCorrection`, pooled phi and gamma, and the AR module's uncorrected AR(1) as a
     diagnostic (`ArFitProvenance`, canonical form `phrf-cmp-s2-ar-fit-v3`).
   - **Decision: any `IllConditioned` run is a hard refusal for the pilot**
     (`PrepRefusal.ArIllConditioned`): the pre-registration must not silently fall back to the raw
     estimator. A budget capped by residual df is not a refusal but is visible in the provenance.
   - Global pooled, not per voxel or per run (O1). The protocol's "pooled AR" secondary cells
     (C-TX-pooledAR, T-TX-pooledAR) therefore coincide in method with the primary and are
     **relabelled** "per-run AR sensitivity" (or dropped) in v0 (decision P3).
3. **Heterogeneity record (F7).** The scorer also computes, per dataset, the SD across voxels
   of the voxelwise lag-1 autocorrelations of the pre-fit residuals and the per-run rho
   estimates. These are **sealed**, not whitelisted; they document how far the global-AR
   assumption is from the data and feed the post-pilot sensitivity arm.
4. **Whitening plan.** `WhiteningPlan.global(ArmaCoefficients.ar(rho), runSegments,
   exactFirstAr1 = true)`; `FitConfig(autocorrelation = ArOptions(Ar(1), global = true,
   rho = Some(rho)))`. **S2 asserts that the phi in the PHRF `FitConfig` equals the phi used to
   whiten the native arms bit-for-bit** (`doubleToLongBits`), and that segments and
   initial-condition policy agree (the executor refuses otherwise).
5. **Whitened arrays.** One whitened array set per dataset; **a test asserts the whitened-array
   hashes are identical across all native arms** (CAN, INF3, FIR, LSA, LSS, rLSS, PHRF input),
   recorded per arm in the ledger.
6. **Dataset assembly** through `InMemoryDatasetBackend`, so PHRF reads through the production
   `DatasetSeriesReader` path.
7. **sigma2** (PHRF's frozen noise variance, "independent preparation on training runs"):
   whitened-refit RSS / (n - rank), pooled, computed from **training runs only** within each
   LOROCV fold (4 per dataset) and from all runs for the final fit (1). It is a condition-level
   computation (about 0.3 s), counted explicitly in section 5.1.

### 2.1 Condition methods (4 cells x 20 datasets)

Scored on the 0.1 s grid over `[0, H]`, H = 32 s (E-resp, protocol section 3).

| Arm | Entry points (S0-pinned) | Configuration | Output |
| --- | --- | --- | --- |
| PHRF (Gaussian primary) | 1. `ObservedFamilyCertification.admitForCompact(expanded, term, frame, 0.2 s, Some(plan), Some(nuisance), heldOutPoints, ObservedFamilyRequirements(0.5, 1e10, 1e-9))`, **built after rho is known** (its fingerprint binds the whitening plan). 2. `ProfileHrfPlan.fromTrialEvents(dataset, drive, baseline, config, basis, alpha = 0.0, ProfileCriterion.PenalizedProfile(sigma2))`. 3. `ProfileHrfFit.prepare(plan, DataSelection.All, whitening, policy(observedAdmission = Some(admission)))`. 4. `prepared.run(reader, sink)`. Route reported: `direct-condition-compact`. | Gaussian family, frozen shape policy, v0 decoder budget, admission rule and prior. | `ProfileVoxelResult`: `status`, `coordinates`, `conditionMeans`, `conditionalSd`; E-resp rebuilt on the grid. |
| CAN | `FitPlan` with `HrfKind.Spmg1`, GLS (`Gls.fit` or plan executor) | SPM double gamma, peak-normalized. *Note 2026-10-02 (S3 review): the implementation uses the raw `spmg1` kernel (peak 0.1754), not peak-normalized: E-resp is scale invariant and the S11 fmrireg parity fixtures are on the raw kernel, so betas are compared raw. E-resp is truncated at the 24 s design span (see below).* | E-resp |
| INF3 | same with `HrfKind.Spmg3` | three columns; E-resp from all three. *Owner decision 2026-10-02: the CAN and INF3 E-resp is truncated at 24 s, the span of their design kernels, so each arm is scored on exactly what its fitted design represents; it is exactly zero in 24-32 s. fmrireg parity is asserted on 0-24 s; the 24-32 s difference from fmrireg `fitted_hrf` (which evaluates the raw kernel to 32 s) is a documented deliberate deviation.* | E-resp |
| FIR | same with `Hrfs.fir(H/1 s, H)` | 1 s bins, unpenalized; E-resp is **piecewise constant** on each bin `[k, k+1)` and 0 from H on. *Owner decision 2026-10-01, superseding the earlier "piecewise-linear interpolation through bin centres" text: it is the response the FIR model actually estimates, it equals fmrireg `fitted_hrf` (S11 parity at 1e-8), and it is the pre-registered rule with no secondary.* | E-resp |

S0 preview on harness C-TX-.5: 33 of 40 voxels `Accepted`, 7 `Boundary` (17.5 % non-Accepted)
*[2026-10-02: computed under a half-TR-shifted SamplingFrame (the spike used the library default start of TR/2, the generator samples at k*TR); superseded, re-measure in S10]*;
these count as typed refusals (2.3). `fromFixed` is **not** used.
*Note 2026-10-02 (S3): the PHRF arm lowers events on a 0.1 s grid, not the 0.2 s shown in step 1, and sets the frame start from `sample_time`. The compact route splits each impulse linearly between lowering nodes, which is exact only for onsets on a node; at 0.2 s an odd-0.1 onset cost PHRF about 3e-3 of the peak response that the exact-lag native arms do not pay (3e-6 at 0.1 s). See `docs/verification/phrf-cmp-s3-20261001.md`.*

### 2.2 Trial methods (3 cells x 20 datasets)

Endpoint: within-condition Pearson r of estimated vs true E-trial (48 trials per condition),
Fisher z, averaged (protocol section 6).

**LOROCV tuning (D11), equal budget, nested.** The grid is 9 points `2^-6 .. 2^2` in each
method's own units. For one dataset (4 runs):

- 4 folds x 9 alphas = **36 fold fits**, each trained on 3 runs and scored on the held-out run;
- plus **1 final fit** at the selected alpha on all 4 runs;
- **37 fits per dataset**. (The protocol's 75 is 37 for full-data tuning plus 2 halves x
  (9 x 2 + 1 = 19) for split-half E-rel. **The pilot runs the 37 only:** split-half fits are
  not a pilot output; recommended default, decision F12 (iii).)
- **Fold score (O2 default, frozen in v0).** Held-out predicted R2 of the held-out run's data
  from the training-fold fit, each held-out trial's amplitude predicted by the training-run
  amplitude of the **same stimulus** (stimulus-persistent deviations repeat across runs; 12
  stimuli x 4 repeats), with the condition mean for a stimulus unseen in training; pooled over
  the 40 scored voxels, in whitened space. Same score, folds and tie rule (smaller alpha wins)
  for PHRF, PHRF-can and rLSS. Held-out data never enter fitting, `sigma2` or the df mapping
  (poisoned-run test in S5).
- **Pre-fit rho** is estimated once per dataset from all runs. It uses no truth and no
  evaluation split, and no held-out score enters it; this is declared.
- **Endpoint-selection check.** Persistent deviations make a large alpha rarely optimal, and
  alpha-0 truth should prefer the weakest-shrinkage end. The S10 rehearsal (not the pilot) runs
  a **no-deviation check on harness data**: 10 datasets with `y0 = y - signal +
  signal_condition_mean` (`alpha_true = 0`) plus the harness persistent cells, and asserts that
  the selected alpha is not the same grid endpoint in every dataset, recording endpoint counts.
  The pilot whitelist carries no endpoint rates (decision F12 (ii)).

| Arm | Entry points | Configuration |
| --- | --- | --- |
| PHRF trial, ML | `ProfileHrfPlan.fromTrialEvents(dataset, drive, baseline, config, basis, alpha, ProfileCriterion.TrialRandomEffectsML(sigma2))`, `ProfileHrfFit.prepare(...)` (route `trial-banded-ml`, provenance `criterion-form=J=E+sigma2*D`), `prepared.run(reader, sink)` for fold fits; for the final fit `prepared.trialOutputs` then `view.run(reader, OutputRequest.TrialAmplitudes(Density), ProfileTrialReadoutMode.ExactShape, sink)`. Evidence stays at the default `PreparedBasisResidual`; any other mode returns `TrialOutputMlIntent`. Sequential `run`, not `ProfileHrfFitParallel`. | positive alpha from the grid (ML refuses alpha 0); decode budget as frozen in v0 (S0: 16 Newton steps, 20 jets, 40 exact evaluations, grid 9x9). |
| LSA | native composition: trialwise canonical (`Spmg1`) design plus F, `Gls.fit` | shared whitening; no tuning |
| LSS | `LeastSquaresSeparate.fit(LssTrialDesign, ResponseBlock, LssFixedDesign(F), LssOptions())` on whitened inputs | canonical; defaults; no tuning |
| rLSS | new `RidgeLeastSquaresSeparate` (`modules/fit/shared`), below | same grid via the df mapping, same folds and score |
| GLMs (T-G, type D) | `from_generator.py` (section 1.1): `run_glmsingle` at pinned commit `1ab54a65`, defaults, `types = BD`, `threads = 1`, with `y + 100` and 4 000 pool voxels | GLMsingle's internal CV with defaults on the same runs; `d_beta_data` reordered by `trial_order` to input order |
| PHRF-can | **timing probe only**, 2 datasets: shape fixed at canonical, same grid | not scored in the pilot |

**rLSS definition (F4; resolves decision F12 (iv)).** rLSS shrinks each trial toward its
**condition mean**, like-for-like with PHRF's condition-centred deviation. For trial i in
condition c(i), on whitened data with nuisance F:

`y = sum_c m_c X_c + delta_i x_i + F gamma + e`, with `X_c` the sum of the canonical single-trial
regressors of condition c (including trial i), `m_c` and `gamma` unpenalized, and only
`delta_i` penalized with ridge `r`. With `x~_i` the residual of `x_i` on `[X_1..X_C, F]`,
`delta_hat_i = x~_i' y~ / (x~_i' x~_i + r)`, and the trial amplitude is
`m_hat_{c(i)} + delta_hat_i` (E-trial through the canonical peak). Closed form, one small solve
per trial, tested against a dense penalized solve to 1e-10. As `r -> 0` this is "LSS with other
trials grouped by condition", not identical to LSS (single rest column); S4 provides a flag that
reproduces LSS exactly for the parity test, and the tuned arm uses the condition-grouped form.

**df mapping (frozen in v0).** The effective degrees of freedom of the deviation block at
penalty `p` is `edf(p) = (1/N) sum_i q_i / (q_i + p)`, with `q_i = x~_i' x~_i` computed from the
whitened canonical single-trial design (shape-free, no data). For each of PHRF's nine alphas
the native penalty `p_k` on the same scale is the deviation prior weight implied by
`AmplitudeStructure.ConditionCenteredTrials(alpha).lambda` at the canonical shape (S4 derives
and unit-tests the scale factor against `TrialBanded`'s `lambda` use); the rLSS ridge for grid
point k is the unique `r_k` with `edf(r_k) = edf(p_k)`, found by bisection (edf is strictly
decreasing). The mapping is computed per fold design from the design alone; the nine `r_k` and
the edf values go to the ledger. If edf ranges fail to overlap by more than 5 % at the grid
ends the dataset is flagged (sealed), not altered.

**Owner decision (2026-10-02): the df mapping is on TRUE effective df.** The `edf` above, `(1/N) sum_i q_i/(q_i + p)`, is a
q-based surrogate (it makes `r_k = p_k` by construction and its end-overlap flag cannot fire); it is now only a diagnostic
(the "information ratio"). The effective df is `tr(S X) = sum_i d ahat_i / d a_i`, the trace of the trial-amplitude smoother
(condition means included, nuisance projected out): rLSS closed form `sum_i [phi_i + (1 - phi_i) q_i/(q_i + r)]`
(from `N` at `r = 0` down to the number of conditions), PHRF at a fixed shape `tr(W^-1 X~'X~)`, `W = X~'X~ + lambda (I - M)`.
`r_k` solves `edf_R(r_k) = edf_P(lambda_k)` by bisection, from the design alone, per training fold and for the final design;
the 5 % end-overlap flag now compares the achieved and target edf at the grid ends. The `kappa^2 (1 - 1/n_c)` penalty scale is a
reported diagnostic, never the mapping. Definition and code: `run/DfMapping.scala`; receipt `docs/verification/phrf-cmp-s4-20261001.md`.

### 2.3 Failure and refusal capture (symmetric, protocol section 7)

`enum ArmStatus: Estimated | Refused(kind) | Failed(kind) | NotRun`, per voxel per arm.

- PHRF: `DecodeStatus` other than `Accepted` (S0: `Boundary`, `BudgetExceeded`,
  `CurvatureNotPositive` *[2026-10-02: this status inventory was observed under a half-TR-shifted SamplingFrame; superseded, re-measure in S10]*) is a refusal; trial public outputs return typed `DecodeRefused`;
  dataset-level `ProfileFitError.*` marks all 40 voxels with the constructor name.
- Native comparators: rank-deficient or non-finite fit is `Failed`, `FitError` name recorded.
- GLMsingle: non-zero exit, `ValueError` (two trials in one volume), timeout, missing output or
  non-finite beta is `Failed`; exit metadata and log tail retained (sealed).
- Every attempt retained with exit metadata, wall and CPU seconds, output hash. Retries at most
  2; the first completed attempt is scored.
- **Complete-case versus imputed sigma (F12 (i)).** The scorer applies the primary
  worst-in-family imputation in the sealed side. The whitelist gives sigma under the primary
  rule only; a complete-case sigma is computed and **sealed**, not emitted (section 7).
- **Non-finite voxel in a PHRF fold fit (owner decision 2026-10-02).** That voxel is `Failed` for the fit and the rest proceed; a voxel non-finite at any alpha of a fold is excluded from that fold's LOROCV pool at every alpha (recorded, hashed), a fold with no usable voxel refuses, and the final fit is independent. rLSS always pools all voxels: a known, accepted asymmetry.

## 3. Pilot outputs and structural restriction

### 3.1 The whitelist (protocol section 8, step 4), exactly

1. sigma of the **centred** paired differences for every gating pair, with the realized `df`:
   - condition (`lambda_d = log(MISE_PHRF / MISE_comp)`): PHRF vs CAN, INF3, FIR in C-TX-.5,
     C-TS-1, C-TS-.5, C-TG-.5 (12);
   - trial (`delta z_d`): PHRF vs LSA, LSS, rLSS in T-TX-fast, T-TX-jit, T-TS-fast (9);
   - T-G: PHRF vs GLMs-D in T-TX-fast, T-TX-jit (2).
2. ICC of errors and of coverage indicators, **PHRF only**, in C-TG-.5 (the only cell with
   coverage): signed relative E-peak error, tau error, and the 1-sigma coverage indicator. The
   v1 draft's third item, the voxel-level ICC of **paired differences**, is **dropped** (F5):
   D is sized from dataset-level sigma (the endpoints are already dataset-level means), so that
   ICC has no consumer and would add an unwhitelisted comparative quantity.
3. Refusal and failure rates **pooled across cells, one number per method** (GLMsingle: over its
   two cells; the denominator is stated). Nothing per arm pair, per cell or per dataset.
4. The five timing quantities (a)-(e) of protocol section 9, pooled medians and ranges in
   core-seconds: (a) trial ML throughput at 144 trials over 4 runs (S0 preview 23 core-ms per
   voxel); (b) trial preparation per alpha (preview 0.5 s); (c) alpha cache, reported as "not
   possible through the public API" with the per-alpha flatness ratio; (d) GLMsingle on a
   4 040-voxel dataset (v0b: 9.7 CPU s for 200 + 4 000 voxels, one thread); (e) cold condition
   preparation (preview 0.24 s). *[2026-10-02: the S0 PHRF previews (a), (b), (e) were computed under a half-TR-shifted SamplingFrame and at a 0.2 s lowering grid; provisional, superseded, re-measure in S10. The GLMsingle figure (d) is unaffected.]*

Nothing else leaves the custodian: no means, no accuracy medians, no per-cell rates, no plots,
no per-dataset values.

### 3.2 Structural prevention

These layers prevent accidental emission and make bypass a visible, reviewable code change. They
are not a defence against a hostile custodian.

1. **Raw results have no serializer.** `SealedRaw` is written only through `SealedStore.append`
   (binary blob, returns a hash); no `toString`, no codecs, package-private fields.
2. **Centred-differences-only API.** `Centred.of(values)` stores `values - mean` and discards
   the mean; operations are `sd(df)`, `n`, variance components. No function returns a mean,
   median or signed difference.
3. **Closed output ADT.** `PilotWhitelist(sigmas, icc, pooledRefusals, timing)`; one JSON writer
   over a fixed key set; canonical-bytes SHA-256 at the end.
4. **Information channels closed explicitly (F5).**
   - *Imputation channel.* Worst-in-family imputation couples each method's score to the other
     methods' errors. It runs inside the scorer on `SealedRaw`, and its outputs enter only
     `Centred`. The count of imputed voxels is not emitted per method or per cell; the pooled
     refusal rate is computed from raw status counts.
     *Owner decision, 2026-10-02:* trial imputation at a refused voxel is the voxel-level minimum, the lowest
     condition-averaged z among the family's gating arms at that voxel.
   - *Coverage-ICC channel.* Coverage indicators exist only for PHRF in C-TG-.5. The only
     emitted function of them is the ICC and its limit; the coverage proportion (a mean) is
     **not emitted**. Because the ICC of a binary indicator near proportion 0 or 1 is a
     deterministic function of the mean, the writer emits the ICC only when the pooled coverage
     proportion lies in [0.05, 0.95] and otherwise `"degenerate"`, so the field cannot encode
     which side of .95 the method is on.
   - *Counts.* **No per-arm or per-cell retry counts, progress counts, attempt counts, failure
     counts or completion counts leave the custodian.** Authors see only "run complete" and the
     total CPU hours, summed over all arms and cells.
   - *Metadata.* **Authors, reviewers and the runner reviewer must not inspect sealed-tree
     metadata** (file sizes, timestamps, entry counts, directory listings, listing lengths)
     beyond the two published digests. Size patterns (for example a smaller blob because an arm
     refused) are a side channel.
5. **Tests (S8, S9).**
   - location invariance: a constant added to a comparator's scores leaves the whitelist
     byte-identical;
   - schema closure: reflection/golden test over the whitelist ADT;
   - cell pooling: rates have no signature accepting a cell id;
   - **planted-mean scan, extended:** a seeded synthetic corpus with a recognizable planted
     difference (for example exactly 0.123456) runs end to end, then the **whole output
     surface** is scanned: the whitelist, runner and aggregator **logs, stack traces (including
     forced failures), exception messages, progress output, every ledger file that leaves the
     custodian, and the sealed tree** (binary blobs are searched for the planted value's IEEE
     bytes; text channels for its decimal renderings);
   - redaction: all log and exception text passes through `SafeMessage` (constructor names and
     totals only);
   - the run-complete message and total-CPU line are byte-identical across two corpora with
     different planted differences.
6. **Process separation.** Raw sealed outputs, caches and runner logs live in the custodian's
   directory (mode 0700).

### 3.3 Variance components

- **sigma.** Through `Centred`: `sqrt(sum (e_d - e_bar)^2 / (D - 1))`, with `D` the number of
  datasets that completed (5.2). The author derives the 80 % UCL from sigma and the realized
  `df`: `sigma * sqrt(df / chi2_{0.20, df})` (about 1.18 at 19 df). A pair with `df < 14` is
  refused, not extrapolated.
- **ICC(1).** One-way random effects, cluster = dataset, `k = 40` voxels:
  `(MSB - MSW) / (MSB + (k - 1) MSW)`, truncated at 0, F-based 80 % upper limit; the grand mean
  is never an output. Closed-form reference fixtures in the S8 tests.
- **Pooled refusal rate.** Refused-or-failed voxels over attempted voxels per method over all
  cells it ran in; dataset-wide failures count all 40 voxels.
- **Timings.** Single-thread CPU per stage (preparation, per-fold fit, final fit, sigma2
  preparation, GLMsingle `cpu_s` from the sidecar), pooled.

## 4. Custody and seeds

### 4.1 Beacon A and the pilot root

- **v0 fixes the drand chain hash and round A, the relays, and the 64-bit reduction (F10):**
  `root64 = first 8 bytes, big-endian, of SHA-256(v0_hash || beacon_A_randomness)`, where
  `beacon_A_randomness = SHA-256(signature)`. The 32-byte value stays in the record.
- v0 also fixes the **denylist rule**: root64 and every stream seed for the pilot cells and
  datasets (140 datasets x 7 purposes) are checked against the protocol denylist after the root
  exists, by a script frozen in v0; a hit aborts the pilot and requires an amended v0 (no
  silent re-derivation). The v0 commit precedes the round's time.
- After the round the custodian fetches it from at least two relays, verifies the BLS signature
  against the chain public key, and logs the raw responses (hashed).
- Streams per `seeds.py` (SplitMix64 on `(root, fnv1a64(cell id), dataset index, purpose)`); the
  loader re-verifies the recorded checks. GLMsingle's `seed` is the `audit` stream seed for the
  (cell, dataset). LOROCV folds are deterministic, so no `split` seed is used.

### 4.2 Custodian (D12)

- Custodian: **backlog-worker-20260930** (O7 decided). A **separate runner reviewer** reviews
  the runner commit; the custodian is not an author of the runner, generator, scorer or
  aggregator.
- **The custodian builds from a clean checkout of the reviewed SHA** (fresh clone at that
  commit, clean `git status`, no `galeRevision` or `-Dscalafim.gale.build` override) and records
  the SHA, `build.sbt` hash and toolchain identities in the stamp.
- The custodian runs and holds: beacon fetch and root derivation, generation of the 140
  datasets, the runner and scorer, the sealed store, the aggregator and the custody log.
- Authors and reviewers see only `pilot-whitelist.json`, its SHA-256, the sealed-tree digest,
  the stamp, "run complete" and total CPU hours; no sealed-tree metadata (3.2).

### 4.3 Hashing and sealing

- Per dataset: input hash, per-arm input hash, per-arm output blob hash, attempt metadata hash.
- Sealed-tree digest: SHA-256 of the sorted `sha256sum`-style listing, written as `sealed.sha256`
  **before** the aggregator runs and again after (must be equal; the aggregator opens the store
  read-only). Both digests and the whitelist hash go into manifest v1.
- Unsealing: not before the confirmatory report is closed. Nothing is deleted.

## 5. Compute, ceiling and freeze

### 5.1 Budget (recomputed on 37 fits per dataset, from S0 previews)

Cap **60 core-hours** (protocol after A1). PHRF per trial dataset, no alpha cache (B10), one
thread, S0 preview unit costs *[2026-10-02: provisional; the S0 unit costs (23 core-ms, 0.5 s, 0.3 s) were computed under a half-TR-shifted SamplingFrame and a 0.2 s lowering grid; superseded, re-measure in S10; section 5.2 projections inherit this]*:

| Item | Count | Unit | Per dataset |
| --- | ---: | --- | ---: |
| fold-fit preparations | 36 | 0.5 s | 18 s |
| final-fit preparation | 1 | 0.5 s | 0.5 s |
| fold fits (40 voxels x 23 ms; 3-run training is smaller, so conservative) | 36 | 0.93 s | 34 s |
| final fit plus ExactShape outputs | 1 | 0.93 + 1.16 s | 2.1 s |
| **sigma2 preparations** (4 folds + full) | 5 | 0.3 s | 1.5 s |
| **PHRF per trial dataset** | | | **about 56 s** |

60 trial datasets: about 0.93 core-h. Protocol section 9 assumed 75 x 12.4 s = 930 s per
dataset (15.5 core-h); the preview is about 16x lower and must be re-measured in the rehearsal
before it replaces the protocol figure. At the protocol's range (preparation 3-30 s per alpha)
the PHRF trial cells cost 2 to 14 core-h.

Other items (core-h, central): GLMsingle 40 datasets x 10-17 s about 0.2; condition arms
(80 datasets) about 0.1; native trial arms including rLSS tuning (37 cheap solves per dataset)
about 0.2; generation and scoring about 0.5; PHRF-can timing probe (2 datasets) small. **Pilot
about 2 core-h central, about 20 plausible upper**; the protocol's 22 / 50 stay as the planning
figures until the rehearsal replaces them. The **60 core-hour ceiling is unchanged**. Accounting
is JVM process CPU plus **child-process CPU** (GLMsingle `cpu_s` from the sidecar, and the
Python generator if the custodian runs it).

### 5.2 Runtime CPU guard, stop behaviour, stamp

Pattern: bootstrap `PilotRunner` (`PilotRefusal` enum, stamp, resume, atomic writes, ceilings
not stamped).

- **Stamp** (`stamp.json`): reviewed git SHA, v0 manifest hash, pilot root, `generator_code_sha256`,
  GLMsingle commit, lockfile hash, **`from_generator.py` hash**, **GLMsingle subprocess timeout
  and thread pinning** (`threads = 1`; `OMP_NUM_THREADS`, `MKL_NUM_THREADS`,
  `OPENBLAS_NUM_THREADS` = 1), JVM and OS identity, PHRF source digest, `build.sbt`, runner
  configuration digest, whitelist schema version, df-mapping rule version. Resume needs an exact
  match; ceilings are not stamped.
- **Projection before launch:** the first 2 datasets of every cell run first; project to 20;
  refuse if the projection exceeds 60.
- **Soft stop (45 core-h) and partial pilots (F9).** At the soft stop the runner stops
  dispatching and finishes in-flight datasets. Datasets are dispatched in index order
  **round-robin across cells** (index 0 of every cell, then index 1, ...). A partial pilot is
  accepted only under a **uniform reverse-index drop**: completed datasets above the smallest
  per-cell completed count are dropped from the highest index down until every cell has the same
  `D`; never selectively, never by any result. The aggregator takes `D` as that common count,
  recomputes `df = D - 1` and the UCL factor, and **refuses** if `D < 15`. Manifest v1 records
  `D`. *[S7 review, D2: `D` is the smallest per-cell **longest completed prefix**. A cell keeps
  exactly `0..D-1`, and a hole is never kept. On resume, incomplete jobs below the highest completed
  index were in flight when the run was interrupted; they are finished before the soft stop is
  honoured. 2026-10-10 (blocker probe M2): the completed set alone misses a job in flight at or above
  the highest completed index, so the runner writes a durable constant-content dispatch marker
  before a job's first arm, and a resume past the soft stop finishes every dispatched incomplete job.]*
- **Hard stop (60):** `CpuCeilingReached`, resumable only with an owner-approved raised ceiling.
  *[S7: checked before every unit attempt and when an arm polls `shouldAbort`; an attempt that
  ends past the ceiling is discarded. The overshoot is bounded by threads x the longest unit
  attempt.]*
- Atomic writes (temp file plus `ATOMIC_MOVE`); a dataset is complete only when all arm blobs,
  their `.sha256` files and the ledger exist; resume regenerates only incomplete datasets.
  *[S7 review, D1: the invocation that reaches an accepted decision recomputes, once, the kept
  jobs completed by earlier invocations, so the in-process scorer sees every kept dataset. The
  recomputation is sealed under attempt-unique `rerun/<runId>/` names, and its CPU counts toward
  the guard. Writes are fsynced (file and directory), and a missing `cost.json` next to existing
  work refuses the resume.]*
- One dataset single-threaded, datasets in parallel up to cores minus one, fixed block order.
- Nondeterminism: if a rerun on the same platform does not reproduce hashes, retain both, score
  the first completed attempt, at most 2 retries, then "unresolved". *[S7 rev 4 (H1, F2, F4):
  "retain both" holds literally. Every unit commit, payload included, is sealed under a name
  carrying the invocation's run id, so both attempts are kept and none can collide. Only
  `meta/root-check` and `meta/stamp` are deterministic. The owner scores the scheduled ledger
  record with the smallest invocation, and detects nondeterminism per unit by comparing
  `payload_sha256` across that unit's records; a mismatch is recorded for the unit and never
  destroys the store. The aggregate lists, per kept unit, the commit the scorer consumed. Any
  difference from the first attempt invalidates the released whitelist and names the units for
  the deviation report; see format spec section 10.]*

### 5.3 Manifest freeze before launch

Gates checked at start (`ManifestNotFrozen` otherwise):

1. Manifest v0 committed and reviewed, containing: protocol hash; pilot cells with the 1.3
   parameters; whitelist schema; TX parametrization and distance receipt; GLMsingle receipt
   **including the `from_generator.py` converter receipt**; **parity receipts for CAN, INF3, FIR, LSA,
   LSS (against R) and rLSS (dense solve, 1e-10)** (S11); source and environment identities;
   denylist, **root reduction and denylist rule** (4.1); beacon A chain and round; alpha and rLSS
   grids with the df-mapping rule; sigma2 rule; LOROCV fold score; the AR pre-fit specification
   with its bias receipt (<= 0.02); the heterogeneity-record spec.
2. No change to any frozen file since v0 (`generator`, `glmsingle` including `from_generator.py`,
   `tx_gate`, PHRF source digest).
3. Beacon A has elapsed; the custody log holds the verified randomness.
4. Root derived and equal in custody log and stamp; denylist check clean.
5. Harness rehearsal passed with the same binary (S10), **including a forced low-ceiling run that
   exercises the CPU guard** (soft and hard stop, resumable partial state, reverse-index drop).
6. **S6 (GLMsingle bridge), S9 (custody tooling) and S11 (parity) gate v0**: v0 cannot be
   frozen without them.

## 6. Slices

Effort in person-days (excluding review). `shared` is tested on JVM and JS; IO, subprocess and
PHRF/GLMsingle drivers are JVM-only (declared). Module `modules/phrf-comparison/`
(crossProject: `shared` scorer, aggregator, whitelist, `Centred`, ICC; `jvm` loader, npz reader,
drivers, ledger, guard, bridge; `js` empty, compiling shared). It depends on `fit`, `dataset`,
`hrf`, `model`, `ar`; nothing depends on it.

| Slice | Content | Depends on | Effort | Gates v0 | Main files | Test obligations |
| --- | --- | --- | ---: | --- | --- | --- |
| **S0** | Source qualification. **DONE** at `256cd1b0` (receipt `docs/verification/phrf-cmp-s0-20261001.md`). | none | 0 (1 spent) | | | done |
| **S1** | Ingestion: strict STORED npz reader (generator output and adaptor output), manifest binding, `FitInputs`/`ScoreTruth`, ledger. | none | 2 | | `modules/phrf-comparison/jvm/.../ingest/*`; `build.sbt`; README | tamper tests (byte flip, root, code hash, fortran order, compressed member, denylisted seed, non-f8 dtype) each refuse; round-trip on committed small fixtures (1 condition, 1 trial, harness root); adaptor-output fixture read. |
| **S2** | Common preparation (2.0): intercept drop, **pre-fit specification** (per-kind AR design with design-corrected estimate; revised 2026-10-01 rev 3, see 2.0.2), global AR(1), plan and `FitConfig`, whitened arrays, sigma2, heterogeneity record. | S1 | 2.5 | yes | `.../prep/{Nuisance,FirPrefit,CommonWhitening,Sigma2}.scala` | rho bias <= 0.02 over 50 harness datasets per kind; **phi bit-for-bit equal between PHRF `FitConfig` and the whitening plan**; **whitened-array hash identical across native arms**; projector equals generator span minus intercepts; admission built after rho; determinism. |
| **S3** | Condition runner: PHRF compact route, CAN, INF3, FIR, E-resp. | S1, S2 | 3 | | `.../run/{ConditionRunner,ConditionArms}.scala`; `shared/.../score/EResp.scala` | PHRF E-resp vs direct evaluation noise-free; typed refusal capture on constructed rank-deficient and `Boundary` inputs; `EResp` JVM+JS. |
| **S4** | Trial native runner: LSA, LSS, **rLSS (condition-centred)**, df mapping, alpha grid, LOROCV shell and shared fold score. | S1, S2 | 3 | yes (rLSS) | `modules/fit/shared/.../RidgeLss.scala`; `.../run/{TrialNativeRunner,AlphaGrid,Lorocv,DfMapping}.scala` | rLSS vs dense penalized solve at 1e-10 (JVM+JS); LSS-equivalence flag reproduces LSS; df mapping solves `edf(r_k) = edf(p_k)` and the scale factor matches `TrialBanded` lambda; folds leave exactly one run out. |
| **S5** | PHRF trial runner: ML plan, 37-fit LOROCV, per-fold sigma2, final fit with ExactShape outputs, E-trial conversion, work receipts, **PHRF-can probe**. | S0, S2, S4 | **6** | yes | `.../run/{PhrfTrialRunner,PhrfTrialTuning,PhrfCanProbe}.scala`; held-out prediction helper (in `modules/fit` if absent) | determinism (hash-equal reruns); poisoned held-out run; alpha 0 never routed to ML; typed refusals; terminal ML evidence present; both-platform parity for pure helpers; endpoint diagnostics (harness). |
| **S6** | **GLMsingle bridge:** `from_generator.py` (per-voxel +100 including pool, STORED output plus JSON sidecar, in-process wrapper), Scala bridge, timeout, thread pinning, CPU capture, E-trial in input order. | S1 | 2 | **yes** | `tools/phrf-comparison/glmsingle/from_generator.py`; `.../run/GlmSingleBridge.scala`; receipt `tools/phrf-comparison/glmsingle/results/from_generator_converter.json` | converter receipt (known amplitude recovers E-trial; baseline on scored and pool voxels; v0b units); reorder test; failures (two trials in a volume, kill, timeout); `cpu_s` reaches the guard; STORED output read by `Npz`. |
| **S7** | Ledger, guard, scheduler: stamp, resume, atomic writes, projection, soft and hard stop, round-robin dispatch, reverse-index drop, retries. | S1 | 2 | | `.../exec/*` | mirror bootstrap `PilotRunnerSuite`; stamp mismatch refuses resume; ceilings not stamped; fake CPU clock; crash between blob and `.sha256`; **partial-pilot rule (equal D, drop from highest index, df recomputed, refusal below 15)**; **whole-runner determinism under parallel datasets: identical sealed-tree digests across runs with different interleavings**. |
| **S8** | Scorer and aggregator: `lambda_d`, `delta z_d`, worst-in-family imputation, complete-case (sealed), `Centred`, ICC, pooled rates, timing, whitelist ADT and writer, tightened channels (3.2). | S1; types from S3-S6 (synthetic inputs first) | 3.5 | | `modules/phrf-comparison/shared/.../*` | location invariance; schema closure; extended planted-mean scan; coverage-ICC degenerate rule; UCL from realized df, refusal at D < 15; zero-MISE floor counted; JVM+JS. |
| **S9** | **Custody tooling:** drand fetch and BLS verify, root64 derivation and denylist check, custody log, sealed store and digest, runbook (clean-checkout build, no-metadata-peeking rule). | S7, S8 | 2 | **yes** | `tools/phrf-comparison/custody/*`; `docs/plans/phrf-pilot-custody-runbook.md` | root derivation test vector; corrupted signature rejected; digest stable and sensitive to one byte; clean-checkout script refuses a dirty tree or a gale override. |
| **S10** | Freeze and rehearsal: manifest v0 assembly; harness-root rehearsal with the same binary (2 datasets per cell, the no-deviation alpha-0 check, endpoint counts); **forced low-ceiling CPU-guard exercise**; independent review; custodian launch. | all | 2 | | manifest, rehearsal receipt | rehearsal output refused by the launcher under a pilot stamp; guard refuses and resumes at a low ceiling; endpoint selection not constant. |
| **S11** | **Parity gates (O8 reversed):** CAN, **INF3 and FIR** against fmrireg (1e-8 relative on fitted betas and reconstructed E-resp); LSA and LSS against `fmrilss` (1e-8, with a generator for LSS); rLSS vs dense solve (1e-10, with S4). | S2, S4 | 4 | **yes** | `tools/phrf-comparison/parity/*` (R scripts using `~/code/fmrireg`, `~/code/fmrilss`), `modules/phrf-comparison/jvm/src/test/.../parity/*` | fixtures committed with R session info; relative error <= 1e-8 (1e-10 rLSS); a failure labels the arm "parity-failed" and blocks v0. |

Total remaining effort (S0 done): 2 + 2.5 + 3 + 3 + 6 + 2 + 2 + 3.5 + 2 + 2 + 4 = **32
person-days** (v1: 24; v2: 31). v0-gating: S2, S4 (rLSS), S5, S6, S9, S11 and the S10 rehearsal.
Critical path: S1, S2, S4, S5, S8, S10; S11 follows S2 and S4; S1, S6, S7 parallelize.

## 7. Risks and decisions

### Risks

1. **Pre-fit bias (B9).** If neither variant meets bias <= 0.02 in trial cells, v0 needs an owner
   decision; whitening mismatch affects all arms equally but weakens the "common pre-fit" claim.
2. **LOROCV scoring (B3).** Held-out prediction machinery may not exist; S5's 6 days include it.
3. **Disk.** About 2.4 GB of trial data; the custodian host needs room.
4. **GLMsingle on aligned onsets.** Two trials per volume raise `ValueError`; recorded as `Failed`.
5. **Pilot leakage.** Sealing and types reduce, not eliminate (protocol section 10); whitelisted
   sigmas are themselves informative.
6. **Python outside provenance** except via the lockfile, the commit and the `from_generator.py`
   hash.
7. **Preview timings** are single-dataset previews; the 16x gap to the protocol's per-dataset
   cost needs the rehearsal to confirm.
8. **Non-Accepted fraction** (S0: 17-20 %) makes the imputation rule consequential for sigma.
   *[2026-10-02: the 17-20 % was computed under a half-TR-shifted SamplingFrame; superseded, re-measure in S10.]*
9. **JS/JVM.** Runner is JVM-only; the shared scorer is tested on both.
10. **Partial pilots** change `df`, the UCL factor and D (5.2).

### Decisions

Decided: O1, O7, O2-O6 and O9 defaults; O8 reversed (parity of every pilot-feeding comparator, including INF3 and FIR, is a v0 gate; P1 closed).

| ID | Decision | Recommended default |
| --- | --- | --- |
| F12 (i) | Complete-case versus imputed sigma. | Emit sigma under the primary imputed rule only; seal complete-case sigma. |
| F12 (ii) | Policy when the selected alpha is at a grid endpoint. | Record in the sealed ledger and the rehearsal; no whitelist field, no re-tuning; a pooled endpoint rate is emitted only if the owner amends the whitelist. |
| F12 (iii) | Partial pilots; split-half fits in the pilot. | Partial pilots only under the uniform reverse-index drop with `D >= 15`; the pilot runs **37 fits per dataset, no split-half fits**. |
| F12 (iv) | rLSS target. | **Resolved:** shrinks toward the condition mean (2.2). |
| F12 (v) | Ban on peeking at sealed-tree metadata. | Adopt: authors, reviewers and runner reviewers see only the two digests, "run complete" and total CPU hours. |
| F12 (vi) | Root reduction. | `root64` = first 8 bytes big-endian of the SHA-256, with the denylist rule, fixed in v0 (4.1). |
| P2 | Pre-fit variant for trial cells. | ~~(A) LSA plus condition FIR, subject to the bias criterion.~~ **Superseded 2026-10-01 (owner, rev 3):** per-kind AR design (condition design in condition cells, trial design in trial cells) with the design-corrected AR estimate, verdict on fresh seeds (2.0.2). |
| P3 | The "pooled AR" secondary cells. | Relabel "per-run AR sensitivity". |

## 8. Revision log

### v1 (fc698d6e) to v2

- **F1:** npz reader scoped to STORED f8/i4; GLMsingle I/O re-emitted by `from_generator.py` as
  STORED npz plus JSON sidecar (1.1); recorded in S1 and S6.
- **F2:** `fromFixedCondition` replaced by S0's call sequences: condition via
  `fromTrialEvents(alpha = 0)` with admission after rho; trial ML via `fromTrialEvents` +
  `TrialRandomEffectsML`, `ExactShape`/`PreparedBasisResidual` (2.1, 2.2).
- **F3:** fit count corrected to 36 + 1 = 37; split-half fits not in the pilot; cost table
  recomputed from S0 previews with per-fold sigma2 counted (5.1).
- **F4:** rLSS defined as condition-centred ridge on the deviation; df-mapping formula; pilot
  trial cells confirmed persistent; alpha-0 deviation-free check added in the rehearsal using
  `signal_condition_mean` (no generator change) (1.3, 2.2).
- **F5:** voxel-level paired-difference ICC dropped; imputation and coverage-ICC channels
  stated; no per-arm or per-cell counts; planted-mean scan extended to logs, stack traces,
  ledgers and the sealed tree; metadata-peeking ban (3.1, 3.2).
- **F6, F7:** bit-for-bit phi equality; whitened-array hash identity across native arms;
  "pooled AR" cells relabelled; AR-heterogeneity record (2.0).
- **F8:** per-voxel +100 including pool; `from_generator.py` in the v0 frozen set with a
  converter receipt; timeout and thread pinning in the stamp (1.3, 5.2, 5.3).
- **F9:** soft-stop and partial-pilot rule: round-robin dispatch, uniform reverse-index drop,
  df and UCL recomputed, refusal at D < 15 (5.2, 3.3).
- **F10:** 64-bit root reduction and denylist rule in v0; clean-checkout build by the custodian
  (4.1, 4.2).
- **F11:** whole-runner determinism test under parallel datasets (S7); rehearsal exercises the
  CPU guard at a low ceiling (S10); S5 raised to 6 days; total recomputed to 31; S6, S9 and S11
  named v0-gating.
- **F12:** decisions (i)-(vi) added with defaults (7).
- **Owner decisions applied:** O1, O7, O2-O6, O9; **O8 reversed**, parity moved to a v0 gate
  (new slice S11; rLSS parity in S4/S11).
- **S0 findings applied:** B1 closed; condition route; nuisance rank trap; pre-fit
  specification; no alpha cache; measured costs (23 core-ms per voxel, 0.5 s per alpha).

### v2 to v2.1

- Owner correction to O8: INF3 and FIR are gating condition comparators that feed pilot sigma,
  so their R parity (against fmrireg, 1e-8) joins S11 before the pilot. S11 rises from 3 to 4
  days; total remaining effort 31 to **32 person-days**. Open decision P1 removed; the v0 freeze
  list (5.3) now names INF3 and FIR parity receipts.
