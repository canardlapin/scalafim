# Retained FGLS diagnosis

The long-cell bias failure is explained by a defect in the **cross-dataset seed schedule**, not by a discrepancy in the known-coefficient GLS calculation. The legacy generator starts each Park–Miller stream 7,919 states apart. At a particular draw position, that linear seed spacing concentrates the Box–Muller angle across the entire retained ensemble. Distinct seed integers did not establish independent Monte Carlo datasets.

This is exploratory diagnosis of previously observed inputs. The original failed confirmation, its source, selection, margins, and spent-stream receipt remain unchanged. No replacement confirmation was generated and no scientific admission is granted. The scope and export harness were committed at `ed581353` before the independent reconstruction.

## Independent reconstruction

`tools/scenarios/fgls-diagnosis/diagnose.R` reconstructs both the null and signal voxel from the declared seed/state arithmetic in all five cells and all 2,048 retained datasets. It solves GLS on retained rows using a dense stationary AR covariance built with base R `ARMAacf`, Cholesky and matrix solves. It does not use Scala's whitening factor or censor-indicator implementation.

Every known-control coefficient estimate, reported variance, t/F/coverage probability and decision agrees with the existing JVM and Scala.js records. The maximum scaled numerical difference is **2.04e-14**, across **10,240 known-control records per platform**. Forty exported retained inputs also reproduce the exact generated noise. Noiseless truth follows from the sign-symmetry check, and arbitrary changes to censored values leave fitted coefficients and variance unchanged. The generated independent R fixture is a permanent portable regression.

## Why the long-cell mean is shifted

For the long cell, null voxel, second run, zero-based time 140:

- The Gaussian draw uses uniform number 1,690 after the dataset seed.
- Consecutive datasets' angular states differ by only **254,773 modulo 2,147,483,647**. The 2,048 angles cover a short circular arc where cosine is positive.
- **All 2,048 innovations are positive**, with mean **0.982888492484109**.
- The GLS coefficient's linear weight on this innovation is **0.0781603717076934**. Its contribution to the null coefficient mean is **0.0768229299197724**.
- The total observed null mean is **0.0787861495591386**, so this single innovation accounts for **97.508%** of it. The standardized mean is **0.175550168757588**, reproducing the failed known-control criterion.

The retained CSVs decompose every time point, including burn-in, for both voxels in every cell. Contributions sum to the observed coefficient mean. This is an algebraic explanation of the realized shift; it is not a post hoc removal of an outlier or a repaired confirmation result. The regression deliberately records this historical defect without changing the legacy generator used to reproduce earlier evidence.

## Coverage remains unresolved

In the run-heterogeneous cell, the candidate covers 1,901 datasets and the control covers 1,924: 1,883 are covered by both, 41 only by the control, 18 only by the candidate, and 106 by neither. The candidate's mean reported signal variance is **0.96803** times the control's, while its empirical signal-estimate variance is **1.03434** times the control's. These paired diagnostics identify an uncertainty-calibration concern; they do not establish its cause or a valid corrected margin.

The original counts and nominal intervals are preserved as descriptive outputs. Because the affine seed schedule does not supply the intended independent replicate ensemble, those intervals cannot establish the asserted calibration qualification. This concern applies to other empirical studies using the same generator/seed recipe, including earlier apparently passing cells. It does not negate deterministic numerical fixtures or FitPlan integration replay, and it does not turn the failed shrinkage method into a qualified method.

## Prospective follow-up

1. Version a new external-input simulator, leaving the legacy replay path intact. Use R's L'Ecuyer-CMRG with separately allocated nonoverlapping streams for datasets and declared substreams for voxel/run noise. Persist the complete six-word state, assignment path, input hashes, R/runtime identity and source locks. Generate a dataset once and give identical bytes to JVM and JS. Different integer seeds alone are not an independence argument.
2. Before fitting development or confirmation outcomes, qualify the simulator on a separate fixture stream: across-dataset innovation means/covariances at every draw position, run/voxel independence, stationary covariance, exact truth/design alignment and censor invariance. Predeclare simultaneous diagnostic bounds and failure accounting; include this affine-seed counterexample as a rejected negative control.
3. Keep the existing five cells and original scientific margins. Requalify the known-coefficient control first. Define an independent development packet for uncertainty diagnostics (per-run AR estimation, shrinkage bias, reported versus empirical signal variance and KR scale/df). Any estimator or uncertainty change must be selected and committed before a fresh confirmation stream is opened.
4. Freeze the complete candidate, source/input/stream inventories, resource cap, independent oracle, failure policy and one-use confirmation receipt. Retain 2,048 confirmation datasets per cell and both-platform execution unless a separately justified prospective protocol changes that inventory. No old dataset becomes fresh confirmation.
5. Keep run-varying inference experimental until that new packet passes. This tranche diagnoses the failure and specifies the next study; it does not execute or pre-approve a scientific qualification.

## Reproduction and validation

- Export: set `SCALAFIM_FGLS_DIAGNOSIS_EXPORT=true` and `SCALAFIM_GLS_STUDY_LOG=<absolute prefix>`, then run `firstLevelLawsJVM/testOnly *FglsDiagnosisSuite` and the JS equivalent. Outputs are retained under `inputs-{jvm,js}.jsonl.gz`.
- Reconstruction: `Rscript tools/scenarios/fgls-diagnosis/diagnose.R <inputs-jvm.jsonl.gz> <original-records-jvm.jsonl.gz> <original-records-js.jsonl.gz> <new-output-directory>`.
- Fixtures: `python3 tools/scenarios/fgls-diagnosis/generate_scala_fixtures.py` renders the retained independent R outputs; run the touched-file Scala formatter afterward.
- Custody and platform audit: `python3 tools/scenarios/fgls-diagnosis/verify_packet.py` checks every original source/artifact byte against the integration baseline, complete design/noise export parity, known-control agreement and the dominant contribution arithmetic.

`analysis/` retains the first successful reconstruction; `analysis-final/` adds angular-state and sign-count diagnostics without changing its outcomes. Validation: seven regression tests pass on each of JVM and Scala.js; six initial export tests pass on each platform. The module formatting check passes. Test/export logs and the verification receipt are retained alongside the records. No production numeric source was changed.
