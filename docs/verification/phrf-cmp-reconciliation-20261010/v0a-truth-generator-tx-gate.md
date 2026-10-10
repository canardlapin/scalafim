# PHRF-CMP v0a reconciliation: truth generator and TX distance gate

Mote: `bd-01M3TA9BANBE56NJ45H5M86RHK`. Date: 2026-10-10. Worktree branch
`work/phrf-cmp-reconcile`, base origin/main `8e85ef61`.

This record binds the delivered v0a source and its immutable TX receipt to the
mote's acceptance. It is a reconciliation, not a new truth campaign. No pilot
or confirmatory root exists or was used. The checks below are reproductions on
the harness root or on the seed-free Sobol set. They are not fresh
confirmation of any scientific claim.

## Delivered source on main

All files arrived in `bfa66bd5` (PHRF branch recovery, 2026-10-04). The
recovery manifest
`docs/verification/phrf-pilot-integration-20261004/source-manifest.json`
lists the same SHA-256 values. The only later change in scope is `723bc27d`,
which edits `generator/requirements.lock` alone (see nit N1).

The SHA-256 of every file listed in the v0a receipt
(`docs/verification/phrf-cmp-v0a-20261001.md`, "File hashes") was recomputed at
`8e85ef61`. All 13 match, including:

| File | SHA-256 |
| --- | --- |
| `tools/phrf-comparison/tx_gate/tx_gate.py` | `b7cd6f953fffd0fcc67b27118ed507f39e82fa518fb967e652f206b10f4f4c97` |
| `tools/phrf-comparison/tx_gate/receipt.json` | `ff56fcd9336143a1fdd6f54726adde27c5c2c59c12defb23780e13e48d18e8c6` |
| `tools/phrf-comparison/tx_gate/receipt_distances.csv` | `3f414554aaeb60f4753283092207fc52252d37a6859b0563b191b889d58d84f7` |
| `tools/phrf-comparison/generator/phrf_gen/kernels.py` | `0fe4a187a7d603e7e0aa7b46fc2a12b2da19f851e91ee5607e5cf3af1a3c1d43` |
| `tools/phrf-comparison/generator/phrf_gen/seeds.py` | `c306e56b2e7156162defb7a3ac23efa971759544f45ac27f02906d6ef1130a53` |

`receipt.json` records `script_sha256` = `b7cd6f953fffd0fcc67b27118ed507f39e82fa518fb967e652f206b10f4f4c97`,
the hash of the delivered `tx_gate.py`.

## Acceptance mapping

| Acceptance (mote body) | Evidence |
| --- | --- |
| Independent Python generator for the pilot cells: families TG/TL/TS/TX, amplitudes in data units, TR 1 s, sub-TR onsets, AR noise, per-voxel SNR | `tools/phrf-comparison/generator/` (`phrf_gen`, `SCHEMA.md`). The test file covers HRF shape and area per family, sub-TR onset grid, AR(1)/AR(2) recovery, per-voxel SNR exact to 1e-10 in the 7 pilot cells, and data-unit amplitudes. **45 passed** (fresh run, below). |
| Shares no normalization, drive builder or basis compiler with PHRF | Generator is numpy/scipy only and imports nothing from `modules/`. The receipt's "Independence notes" state that Scala was read only to confirm conventions. |
| TX gate: 1024 unscrambled Sobol points | `receipt.json`: `n_points` 1024 of `sobol_candidates_examined` 2408, `points_sha256` reproduced exactly. |
| Amplitude-free relative L2 on a 0.1 s grid over [0, 48] s | `receipt.json` `grid`: `dt` 0.1, `t_max` 48.0. |
| Fit over the Gaussian C0 and Cascade34 PHRF-04 charts: 41-point grid, then Nelder-Mead from the best 5 | `receipt.json` `grid`: `grid_points_per_axis` 41, `refine_from_best` 5. The 2026-09-30 independent review checked the chart definitions at file:line and cross-checked the optimizer with differential evolution to 1e-14 (mote note). |
| Pass when the median is >= 0.05 against both charts; 10th percentile reported | Gaussian median 0.1304 (p10 0.0688), Cascade34 median 0.3768 (p10 0.2395). `verdict` PASS. |
| No pilot/confirmatory seeds; tests use harness seeds only | The CLI refuses pilot or confirmatory roots unless `--root-hex` is given, and refuses the harness root for them (`phrf_gen/__main__.py:18-24`). Tests import `HARNESS_ROOT` only. The TX gate uses no seed. |

## Fresh reproduction (2026-10-10)

The runtime was recreated from `generator/requirements.lock` (numpy 2.4.3,
scipy 1.17.1, pytest 7.4.4) under Python 3.12.13 in a scratch venv. A second
venv used the receipt's original numpy 2.2.6 and scipy 1.15.3. No `.venv` link
in the repository was used.

| Check | Result |
| --- | --- |
| `generator`: `pytest -q -p no:cacheprovider` | 45 passed, exit 0 (`logs/v0a-generator-pytest.log.gz`) |
| `tx_gate`: `pytest -q test_tx_gate.py` | 7 passed, exit 0 (`logs/v0a-txgate-pytest.log.gz`) |
| `tx_gate.py` rerun (output in scratch, receipt untouched), both runtimes | Gaussian and Cascade34 distance columns are **bit-identical** to the committed CSV in all 1024 rows. The CAN column is identical too. The INF3 column (reported only, no threshold) differs in 242 of 1024 rows by at most 1.1e-16 absolute (3.8e-16 relative). `points_sha256` is identical, as are every summary statistic except the INF3 p10, which changes in the 17th significant digit. Verdict PASS. |
| Receipt sample datasets (harness root, dataset 0), both runtimes | `C-TX-.5` `5d44787a...638fe` and `T-TX-jit` `10ddc323...4730b`, byte-identical to the receipt. |

The committed `receipt.json` is therefore not byte-reproducible on this host
(its `distances_sha256` covers the INF3 column). The gate quantities are, bit
for bit. The INF3 difference is floating-point last-bit noise in a column that
carries no threshold. The committed receipt stays the immutable record. It is
not rewritten.

## Receipt nits from the 2026-09-30 review

| Nit | Status on main |
| --- | --- |
| Document the undershoot distribution and the stratified distances | Done in the v0a receipt, "TX distribution: review notes" (49% below 0.05, 31% below 0.01; stratified medians table). |
| 'smooth' wording is too strong (189/1024 have small wiggles) | Done: the receipt says IL is not uniformly smooth and quantifies the worst ripple. |
| Keep the condition-amplitude convention explicit in manifest v0 | Done in the receipt ("Condition amplitude convention (explicit)"). Carrying it into manifest v0 is S10 freeze work. |

New nits found here, recorded without editing the historical receipt:

- **N1.** The receipt says byte reproducibility is conditional on numpy 2.2.6
  and scipy 1.15.3. `723bc27d` moved `generator/requirements.lock` to 2.4.3
  and 1.17.1 to match the committed fixture manifests. Both runtimes reproduce
  the sample dataset hashes and the gate columns exactly, so the statement is
  stale but harmless.
- **N2.** `receipt.json` is not byte-identical when rerun here (INF3 last bits,
  above). Any future "byte-identical" claim for this receipt should name the
  host BLS/libm.

## Scope limits (not claimed)

- The scalafim cross-check at 1e-10 (protocol section 5) was not run. The v0a
  receipt says so, and it is not part of this mote's acceptance.
- The IL sum-to-zero reading is checked against the 2009 eq. A10 only. It is
  not checked against the 2007 HBM text.
- The TX box leans Gaussian-like. That makes the pooled-median gate
  conservative, but it does not show that TX is far from PHRF at every
  undershoot level. This matters for tier-2 interpretation.
- Protocol v3.1 (`1dd49723`) and the original commits `a576b106` and
  `34d575d4` are not reachable from main (recovered without history). The
  acceptance text is taken from the mote body.
- No pilot admission, no Linux custody and no type-D equivalence are claimed.

## Verdict

All v0a acceptance criteria are evidenced by delivered source on main, by the
immutable receipt, and by a hash-bound reproduction. **Close.**
