# phrf_gen output schema (`phrf-gen-npz-1`)

One dataset = `<CELL>__d<NNNN>.npz` + `<CELL>__d<NNNN>.manifest.json`.

The `.npz` is a byte-deterministic zip (members sorted by name, STORED, fixed 1980-01-01
timestamps, `.npy` format v1.0, little-endian, C order). Identical (root, cell, dataset) on the
same numpy/scipy versions gives identical bytes. The manifest carries `npz_sha256` (whole file)
and `npy_sha256` per array, the stream seeds and their denylist check, the root kind
(`harness|pilot|confirmatory`), the root, generator-code hash and library versions.

All arrays are float64 unless noted. V voxels, T samples (runs concatenated), N events,
C=3 conditions, P nuisance columns, G=481 truth-grid points (t = 0, 0.1, ..., 48 s).

| array | shape | meaning |
| --- | --- | --- |
| `y` | (V,T) | data = `signal` + `nuisance @ nuisance_coef` + noise. Noise marginal SD is 1 (data units). |
| `signal` | (V,T) | noiseless drive-convolved signal, before nuisance. Condition cells: per voxel `std_t(signal)/1 = SNR` exactly (ddof 0); trial cells: see `signal_condition_mean`. |
| `nuisance` | (T,P) | C0 nuisance design: per run 6 columns (intercept, linear, quadratic (demeaned), cos(pi k (n+.5)/L), k=1..3), block-diagonal over runs (P = 6 x runs). |
| `nuisance_coef` | (V,P) | true nuisance coefficients (N(0,1)). |
| `sample_time` | (T) | time of sample n within its run, `n*TR` s. |
| `run_id` | (T) int32 | run index of each sample. |
| `ev_onset` | (N) | nominal onset (s, within run, 0.1 s grid). |
| `ev_onset_true` | (N) | onset used to generate the signal (differs only under latency jitter). |
| `ev_cond`, `ev_stim`, `ev_run` | (N) int32 | condition 0..2; stimulus id = cond*12+stim (trial cells; -1 for condition cells); run. |
| `ev_duration` | (N) | neural duration (s); 0 = delta. |
| `truth_t` | (G) | truth grid. |
| `truth_kernel` | (V,G) | true HRF per voxel, UNIT PEAK (max abs = 1 on a 0.01 s grid), causal. 0 for NULL cells. |
| `truth_params` | (V,k) | per-voxel family parameters; names in manifest `truth_param_names`. |
| `cond_coef` | (V,C) | coefficient on the unit-peak-kernel condition regressor (condition cells), or condition-mean amplitude on a unit-peak kernel (trial cells). |
| `cond_peak_amp` | (V,C) | condition amplitude in data units. Condition cells: peak height (max abs over samples) of the drive-convolved condition regressor. Trial cells: peak height of a single-trial response (= `cond_coef`). |
| `signal_scale` | (V) | per-voxel rescale that hit the target SNR. |
| `trial_beta` | (V,N) | trial cells only: true per-trial amplitude (data units, single-trial peak height), including stimulus-persistent / repeat deviations. |
| `signal_condition_mean` | (V,T) | trial cells only: noiseless signal with every trial at its condition amplitude (no deviations). The SNR target applies to this: `std_t = SNR` exactly. `signal` (with deviations) is what generated `y`. |
| `y_pool`, `nuisance_coef_pool` | (Vp,T), (Vp,P) | trial cells only: pure-noise pool voxels sharing the rank-3 structured noise. |

`meta` in the manifest also records the full cell spec, SNR definition and truth grid.
Sample n of a run is at time `n*TR` relative to run start; events are (nominal or true) onsets on
the same clock. The response of an event is the unit-peak kernel evaluated at `sample_time - onset`
(exact continuous time, no resampling), so a runner can rebuild regressors from `ev_*`.
