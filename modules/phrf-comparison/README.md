# phrf-comparison

Pilot runner for the PHRF comparative evaluation (design: `docs/plans/phrf-pilot-runner-design.md`, v2.1).

**Platform scope.** The *runner* (file loading, drivers, ledger, guard, bridges) is **JVM-only**.
The *shared* part (byte-level parsing, manifest binding, typed records, and the later
aggregator and scorer) is portable and is tested on **both JVM and Scala.js**. The `js` platform
contains no sources of its own; it only compiles and tests `shared`.

## Slice S1: ingestion (`scalafim.phrfcmp.ingest`)

- `Npz.parse(bytes)`: strict reader of generator `.npz` bytes. Accepts only ZIP STORED members
  (method 0, no data descriptor, no encryption, no zip64, CRC-32 checked) holding `.npy` v1.0,
  little-endian `<f8` or `<i4`, C order. Everything else is refused with a typed `NpzError`.
- `Digests`: pure SHA-256 and CRC-32, identical on JVM and JS.
- `PhrfDatasetBinding.bind(npzBytes, manifestJson, IngestExpectation)`: parses the generator
  manifest (`phrf-gen-npz-1`) and refuses with a typed `IngestRefusal` unless the root kind, root,
  denylist checks, frozen generator-code hash, cell spec (including `tr_aligned_onsets`),
  `npz_sha256`, per-array `npy_sha256`, dtypes and shapes all agree. On success it yields
  `BoundDataset(manifest, inputSha256, arrayHashes, fit: FitInputs, truth: ScoreTruth)`.
- `FitInputs` carries only what a fitter may see; truth arrays exist only in `ScoreTruth`. The
  constructors are `private[ingest]`, and a compile-level test asserts that `FitInputs` exposes no
  truth field. Tuning and fitting code must receive a `FitInputs` only, never the `BoundDataset`.
- Stream seeds are re-derived in Scala (`Seeds`, a port of `seeds.py`) from the frozen root, cell id and
  dataset index, compared with the manifest as exact digit strings (read from the raw JSON, never via
  `Double`), and the denylist hits are recomputed against a denylist pinned in `IngestExpectation`.
- JVM only: `PhrfDatasetLoader.load(dir, stem, expectation)` reads `<stem>.npz` and
  `<stem>.manifest.json`.

## Slice S4: trial-native arms (`scalafim.phrfcmp.run`)

- `TrialNativeRunner`: LSA (OLS on the shared whitened arrays), LSS (`LeastSquaresSeparate`) and
  condition-centred rLSS (`RidgeLeastSquaresSeparate` in `modules/fit`; a pooled group at ridge 0 reproduces LSS).
  Inputs are `TrialNativeInputs.from(fitInputs, commonPrep)`: the whitened canonical single-trial regressors are the
  leading columns of the S2 pre-fit design. Failures are typed (`NativeTrialRefusal`, `status` is `Failed` or `Refused`).
- `AlphaGrid`: the nine-point `2^-6 .. 2^2` grid in PHRF units (`lambda = 1 / alpha`).
- `DfMapping`: rLSS is matched to PHRF on TRUE effective df (owner decision 2026-10-02), `edf = tr(S X)`, the trace of the
  trial-amplitude smoother (condition means included, nuisance projected out); rLSS closed form
  `sum_i [phi_i + (1 - phi_i) q_i/(q_i + r)]` (`RidgeLssPrepared.amplitudeEdf`). `DfMapping.map(grid, targetEdf, EdfCurve.rlss(..))`
  solves the ridge by bisection from PHRF's edf targets (`EdfTargets`, supplied by the PHRF side) and flags the dataset when an end
  of the grid misses by more than 5 %. `PenaltyScale` (`kappa^2 (1 - 1/n_c)`) and the q-based `InformationRatio` are diagnostics only.
- `Lorocv`: leave-one-run-out folds, the shared fold score (`FoldScore.predictedR2`: whitened, held-out nuisance
  estimated in-sample and removed, pooled over ALL delivered scored voxels), `FoldScore.predictedSignal` and
  `AmplitudePrediction.byStimulus`; selection takes the largest mean score, ties go to the smaller alpha.
- JVM parity against the S11 R fixtures: `S11TrialParitySuite` (LSA and LSS 1e-8, rLSS 1e-10).

## Slice S8: scorer and aggregator (`scalafim.phrfcmp.score`)

- Input contract for the arm adapters: `ConditionDataset` (voxel MISE per arm), `TrialDataset` with `TrialTruth` and
  `TrialEstimate` (E-trial per trial), `CoverageDataset` (PHRF, C-TG-.5), `TimingSamples`, assembled as a
  `PilotCorpus` (equal D, indices `0 until D`, D >= 15). Every voxel is `Estimated`, `Refused` or `Failed`.
- `Imputation` (worst-in-family over the gating arms only), `Endpoints` (`lambda_d`, `delta z_d`, floor, Fisher z),
  `Centred` (deviations only), `Icc` (ICC(1) with unbalanced k0 and an F-based 80 % limit), `Dist` (portable `qf`,
  `qchisq`).
- Output is the closed `PilotWhitelist` (23 `GatingPair` sigmas with df, PHRF ICCs, one pooled rate per method,
  timing) written by `WhitelistWriter` with a fixed key set. A trial cell with constant true amplitudes renders its
  sigmas as `"degenerate"`. Complete-case sigma and all counts go only to `SealedDiagnostics`.
- Messages are `SafeMessage` values: a constructor token, plus integers for most constructors; the count-carrying
  errors print the token only.
- `PilotAggregator.aggregate` and `aggregateGuarded`; both platforms test it (`DistIccSuite`,
  `ImputationEndpointsSuite`, `WhitelistAggregatorSuite`, `FollowUpSuite`).

## Test fixtures

`jvm/src/test/resources/fixtures/` holds two real generator outputs (HARNESS root, dataset 0),
produced from `tools/phrf-comparison/generator` with reduced voxel counts:

```
dataclasses.replace(CELLS["T-TX-fast"], n_voxels=3, n_pool=2)
dataclasses.replace(CELLS["C-TX-.5"],   n_voxels=3)
```

`RealGeneratorSuite` regenerates them when `python3` with numpy and scipy is available and asserts
byte equality. The embedded `GoldenBytes` in the shared tests is real `phrf_gen.io.npz_bytes`
output as well.

## Commands

```
sbt phrfComparisonJVM/test phrfComparisonJS/test
```
