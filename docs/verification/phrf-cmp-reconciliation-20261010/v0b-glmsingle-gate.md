# PHRF-CMP v0b reconciliation: GLMsingle install, units and normalization gate

Mote: `bd-01M3TA9DAM0SX1NWAH059C88ZP`. Date: 2026-10-10. Worktree branch
`work/phrf-cmp-reconcile`, base origin/main `8e85ef61`.

This record reconciles the delivered gate record
(`docs/verification/phrf-cmp-v0b-20261001.md`) with the exact upstream, the
runtime, the units and the owner-amended bound. The original literal-bound
failure and the unseen-design evidence are preserved unchanged in that
receipt. Nothing here re-tunes or relaxes the bound.

## Exact upstream and runtime

| Item | Evidence |
| --- | --- |
| Pinned upstream | `tools/phrf-comparison/glmsingle/pinned_commit.txt` = `1ab54a65edd3ea41a6133d4b4ecb78a9c7296684`. A fresh clone checked out at that SHA reports `2025-11-09 Merge branch 'main' of https://github.com/kendrickkay/GLMsingle into main`. `setup.py:13` reads `version='1.2'`. |
| Hash lock | `tools/phrf-comparison/glmsingle/requirements.lock` (sha256 `383160e9b5a5fbf87a88cee2a72f4f2e5c29a8c474c0d4e6739329d99536e3ac`, 786 hash lines). It equals `requirements_sha256` in `docs/verification/consolidation-20261005/glm-environment.json`. |
| Runtime recreated from the lock (2026-10-10) | `python3.12 -m venv`, then `pip install --require-hashes -r requirements.lock`, then `pip install --no-deps <clone at 1ab54a65>`. Python 3.12.13, macOS arm64. No repository `.venv` link was used. The stale shared path `/private/tmp/scalafim-phrf-v0-20261001/...` no longer exists. |
| Freeze identity | The recreated `pip freeze` equals `tools/phrf-comparison/glmsingle/environment.freeze.txt` on every third-party line. It also equals `docs/verification/consolidation-20261005/glm-environment-freeze.txt` on every line except the local `GLMsingle @ file://` path. Copy: `runtime/glmsingle-freeze-20261010.txt`. |
| Licence | `LICENSE` at the pin is BSD 3-Clause (Copyright (c) 2021, Kendrick Kay). `setup.py:21` still carries the stale LGPLv3 classifier the receipt notes. |

## Source citations re-checked at the pin

Spot checks of the receipt's file:line citations in the fresh clone:

| Receipt citation | At `1ab54a65` |
| --- | --- |
| `defaults.py:22` `wantpercentbold = 1` | confirmed; `n_pcs` 10 (l.11), `wantlibrary`/`wantglmdenoise`/`wantfracridge` 1 (l.14-16), `wantlss` 0 (l.23), `brainthresh` [99, 0.1] (l.24), `pcstop` 1.05 (l.29), `fracs` linspace(1, 0.05, 20) (l.30), `wantautoscale` 1 (l.31), `seed` time.time() (l.32) |
| `ols/glm_estimatemodel.py:523` meanvol over all volumes | confirmed |
| `ols/glm_estimatemodel.py:945-960` psc = beta * 100 / abs(meanvol) | confirmed |
| `ols/glm_estimatemodel.py:790` cast betas to float32 | confirmed |
| `glmsingle.py:469, 474` `normalisemax` of `hrftoassume` / per-column library | confirmed |
| `glmsingle.py:1342, 1534` `wantpercentbold: 0` for C/D; `:1639` final psc conversion | confirmed |
| `check_inputs.py:53, 69` data and design to float32 | confirmed |
| `ols/fit_model.py:230` `olsmatrix2` | confirmed |
| `gmm/findtailthreshold.py:55` GMM with `reg_covar=0` | confirmed |
| `hrf/gethrf.py:94` global-maximum library normalization | **line drift:** the file has 93 lines; the statement is `hrfs = hrfs / np.max(hrfs)` at **line 91**. The fact holds; only the line number in the receipt is off. |

## Acceptance mapping

| Acceptance (mote body) | Evidence |
| --- | --- |
| Pin official GLMsingle at an exact commit; isolated venv, no global installs | Pin and lock above. The venv is git-ignored. The runtime was recreated from the lock with an identical freeze. |
| Beta units verified from source and numerically | Source: citations above. Numeric: `results/units_and_norm.json`. psc equals data/meanvol x 100 to 6.1e-5 (float32). `wantpercentbold=0` returns raw units. meanvol ranges 99.77-100.21; substituting 100 would bias betas by 0.23%. |
| HRF library normalization | `results/units_and_norm.json`: raw library peaks 0.349-1.0, peaks as used exactly 1.0. Source: `gethrf.py:91` (global) then `glmsingle.py:474` (per column). |
| Type B/C/D outputs | Documented with source lines in the receipt, "Outputs of types A-D". |
| Low-noise (SNR 100) parity for types C/D | `results/lownoise_snr100_repeat.json`: index 100%; relative error max 0.0084 (C and D, criterion <= 1e-2); `pass_C`/`pass_D` true. |
| Gate failure => D5 'not run' | The original verdict is retained verbatim: **literal type-B 1e-6 FAIL** (`units_and_norm.json` `pass_B_beta_1e-6: false`; L2 max 9.1e-6, elementwise max 6.1e-5). |
| Owner decision 2026-10-01 (1): replace the literal tolerance with a float32-derived bound fixed in advance | `precision_bound.py`, `results/precision_bound_frozen.json` (sha256 `c2b9e01f17e2d4e35f59e1d74c7e7a1e595484ddca645a9e9205cfc02c21088d`). `results/precision_parity.json`: all 60 voxels within the bound at baselines 1/100/1000 (min margin 62x/14.5x/5.2x). Negative controls exceed the bound by >= 175x. |
| Review F1: confirm on >= 5 pre-listed unseen designs with the bound unchanged | `results/unseen_design_plan.json` (6 seeds 20261-20266, `written_before_running: true`, previously used seeds listed). `results/unseen_design_confirmation.json`: 48 cells, 2,880 voxel fits, `voxels_exceeding` 0 at every baseline, `hrf_index_exact_all` true, min margins 19.0x/6.5x/3.5x/2.9x, `pass` true. |
| Owner decision (2): T-G cells use TR-aligned onsets | Generator per-cell flag `tr_aligned_onsets` (v0a receipt; 6 generator tests). `from_generator.py` refuses a cell without it and any non-zero onset quantization (S6 converter refusal tests). The v0b receipt said this was "not verified here"; it is now covered by the S6 evidence. |
| Provenance and licence recorded | Receipt "Pinned install" plus the identities above. |

## Fresh reproduction of the frozen bound

`precision_bound.py` takes the design alone (the original diagnosis seed 11).
It was rerun in a scratch copy under the recreated runtime. The output
`precision_bound_frozen.json` is **byte-identical** to the committed file
(`c2b9e01f...088d`). The kappa table matches to relative difference 0.0
(kappa 3.97-7.96). This reproduces a design-derived constant. It is not a new
confirmation, and seed 11 is not used as confirmation evidence.

`precision_parity.py` was also rerun once in the same scratch copy, against
real pinned GLMsingle (8 s). This is a runtime reproduction on the examined
design. It is **not** new confirmation; the unseen-design JSON remains the
confirmation. Result (`logs/v0b-precision-parity-rerun.log.gz`): `PASS_ALL
True`, HRF index exact, all voxels within the frozen bound at baselines
1/100/1000, and min margins 62.3x/14.5x/5.21x, the same as the committed
JSON. The observed errors agree with `results/precision_parity.json` only to
about 1e-11 relative (for example, baseline-1 max 2.1531576041061776e-06
against 2.153157604161893e-06). Today's GLMsingle runtime on this host is
therefore numerically equivalent but **not bit-identical** to the 10-01
run. The same low-order difference breaks the S6 output-bytes pin
(`s6-glmsingle-bridge.md`). It does not affect this gate, whose criterion is
the float32 bound.

The committed result JSONs remain the gate evidence.

## Ordering evidence not re-verifiable from main

The receipt and the 2026-10-01 review notes cite these commits: bound frozen
`0349a1d` before the rerun; unseen plan `1a930f5` before results `dde65860`;
reviewed `9b943c4f`; original FAIL `34d575d4`. None is reachable from main.
The PHRF branch was recovered from working-tree bytes without history (see
`phrf-pilot-integration-20261004.md`). The ordering therefore rests on the
mote's 2026-10-01 independent-review note ("freeze ordering ... verified";
"The bound is unchanged since 0349a1da (verified)") and on the plan's
self-declared `written_before_running`. This record does not claim to have
re-proven it.

## Scope limits (not claimed)

- **Type-D scientific equivalence is not claimed.** Under per-trial
  independent amplitudes type D shrinks (slope 0.71, FRACvalue near the floor)
  by design. The gate passes C/D only on repeat-consistent amplitudes.
- The bound is a derived estimate with c = 1, not a proof. Its margin is about
  3x at baseline 10000 on slow designs. Larger baselines, longer runs or
  denser designs need recomputation, and an overshoot is a FAIL.
- Shipped-example parity is self-consistency only; no reference outputs ship.
- macOS arm64 only. No Linux custody admission, no pilot admission.

## Verdict

The exact upstream, runtime, units and normalization are reconciled. The
owner-amended bound is bound to its frozen file, and the original FAIL and the
unseen-design confirmation are preserved. **Close**, with the receipt
line-number drift (`gethrf.py:94` should be `:91`) recorded here rather than
edited into the historical receipt.
