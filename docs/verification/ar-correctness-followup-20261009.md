# AR correctness and voxelwise GLS follow-up, 2026-10-09

The implementation follows the immutable fmriAR 0.4.1 reference
`de42cd9dc3e290d0653c5145749c27e03085f7d6`. Correctness gates and statistical
qualification are separate decisions. The historical negative qualification
receipts remain unchanged.

## Implemented behavior

- Standard BIC; normalized per-voxel ACFs aggregated after computation.
- Exact stationary AR/MA/ARMA whitening and adjoint using Gale banded factors.
- Low-rank bias-map expansion with the original run/censor/centering target.
- Explicit fixed-budget short-memory tail anchoring, including actual anchor
  counts and typed fallback diagnostics; the adaptive exact default is retained.
- Explicit estimation-only censoring: exclude rows from lag estimation, retain
  responses and continuous covariance within each run. Physical observation
  omission is refused under this policy.
- Dense/bounded GLS, contrast preparation, profile identities, JSON/text recipes
  and saved GLS artifacts carry the policies. Robust re-estimation refuses
  unsupported new policies rather than silently ignoring them.

The new `ar.fmriar-041.v1.r.json` receipt is generated in R 4.5.1 from the exact
0.4.1 source. It covers BIC, ACF, eight stationary AR/MA/ARMA cases, the tail
estimator and both censor treatments through final GLS coefficients,
standard errors and covariance. Continuous R-oracle application clears the
copied plan's censor mask explicitly: an empty application mask otherwise falls
back to the plan mask. Original 0.3.3 locks and historical artifacts are retained.
The nested AR identity record is now `ar_options/v3` for every recipe,
including unchanged legacy/raw/IID configurations. Preparation/profile
identities therefore differ from earlier versions even when fitted numerics
are unchanged; regenerate cached preparation under the new identity.
Existing profile/condition identity goldens pin this compatibility boundary.
Artifact v1/v2 encoding and decoding remain supported; this does not promise
reuse of an old preparation under a changed source/policy identity.

Global pooling still averages run coefficient vectors; the upstream change to
averaging normalized ACVFs before Yule-Walker is a distinct scientific policy.

## Retained experiments

All campaigns were declared before outcomes, with separate roots and seed
domains. Each records actual source hashes, a frozen source archive, both
platforms and independent R analysis. None changes the original thresholds.

| Campaign | Root / domains | Evidence and conclusion |
| --- | --- | --- |
| Exact start-up | 1283197733 / 31–33 | [Pilot/screen](gls-remediation-20261009/). Stationary versus legacy start-up does not change one-pass OLS AR-estimation RMSE. The 64-dataset short AR2 RMSEs remain .1039 (complete) and .1077 (censored), above .10. Long RMSE .0716. No admission from screening. |
| Tail25 plus continuity | 1339142671 / 41–43 | [Pilot](gls-tail-continuity-20261009/pilot/analysis/summary.json). Tail25 refuses 9/16 complete short AR2 datasets and 12/16 censored short AR2/AR1 datasets. The complete long tail fits worsen RMSE to .0992 versus adaptive .0664. No admission. |
| Adaptive exact plus continuity | 1427738621 / 51–53 | [Declaration](gls-continuity-20261009/protocol-declaration.json). All four adverse bases are retained. Only the long AR2 case is eligible for fresh confirmation; both corrected and known-phi controls must pass every original margin. |

The tail25 refusals are real estimator fallbacks, not a status-recognition bug.
A direct reproduction of censored AR2 pilot replicates 2 and 3 finds
`SolveFallback(NonPositiveVariance(...))` at corrected lag-zero variances
-.7226, -4.2538, -1.0946 and -2.2601. The public fitter preserves these outcomes
and the qualification harness refuses to count them as successfully corrected
fits. Partial-result RMSEs cannot qualify that recipe. These deterministic
reproductions are diagnostic evidence only; they do not replace the frozen
pilot source or its observed failure records.

The stationary screen agrees across platforms within 1.20e-14 scaled numeric
error; the tail pilot within 5.46e-12, with identical datasets, refusal keys and
t/F/coverage decisions. The wider correction's single-voxel instability cannot
be inferred away from a selected pooled-R parity case.

## Fresh confirmation verdict

[Independent R summary](gls-continuity-20261009/confirmation/analysis/summary.json):
restricted long-cell admission **passes**; all-domain admission **fails**.
Each cell has 2,048 independent datasets per engine, with no refusals. Actual
source hashes are unchanged throughout both platform runs. Maximum scaled
JVM/JS discrepancy is 1.58e-14; t/F/coverage decisions are identical.

| Corrected voxelwise case | AR RMSE | Joint F rejection (90% exact interval) | 95% interval coverage (90% exact interval) | Retained verdict |
| --- | --- | --- | --- | --- |
| AR2 complete, 96/144 | .1027 | 147/2048, .07178 [.06261,.08187] | 1915/2048, .93506 [.92539,.94379] | F and AR RMSE fail |
| AR2 censored, 96/144 | .1072 | 144/2048, .07031 [.06124,.08031] | 1909/2048, .93213 [.92227,.94105] | F and AR RMSE fail |
| AR2 censored, 192/288 | .0700 | 123/2048, .06006 [.05165,.06942] | 1915/2048, .93506 [.92539,.94379] | Every original margin passes |
| AR1 censored, 96/144 | .1058 | 142/2048, .06934 [.06032,.07928] | 1918/2048, .93652 [.92694,.94516] | AR RMSE fails; no admission |

The long AR2 corrected variance ratio is .95993 with 90% dataset-bootstrap
interval [.91424,1.01225]. Its known-phi control passes every margin too:
F 93/2048, .04541 [.03809,.05372], coverage 1938/2048, .94629
[.93736,.95425], variance ratio .98816 [.94395,1.03933]. The bootstrap uses
1,999 declared resamples. t rejection, mean AR bias, standardized effect bias,
whiteness and excess-over-known-filter gates also pass for the eligible case.

This confirms the new continuous stationary recipe on the declared long-run
domain. The spent historical recipe remains negative. The historical and fresh
streams are independent, so their different counts are not a paired estimate
of a causal improvement. [Calibration figure](gls-continuity-20261009/confirmation/analysis/calibration.png).

## Qualification limits

Gaussian stationary pure AR truth; two independent continuous runs;
homogeneous AR across four independent response columns; twelve run-local DCT
nuisance terms; full response axis, with specified lag-estimation exclusions.
The datasets, not response columns, are independent sampling units.

The previous short-run voxelwise failures remain adverse evidence. A restricted
long-run admission would not qualify short runs, heterogeneous AR, physical
observation removal, long-memory noise, or robust re-estimation. No effective-df
or standard-error scaling is introduced. If plug-in covariance uncertainty
still fails calibration, the follow-up requires a separately derived inference
policy with fresh held-out confirmation.


Study logs, trial JSONL and redundant replicate JSON are stored with lossless
gzip compression. Each `storage.json` records both decoded and stored hashes;
`analysis-receipt.json` retains the independent analysis input/output hashes.
To reproduce analysis, pass decompressed trial logs (or R-readable gzip
connections) to the declared R script. Each frozen source archive was checked
entry by entry against its run manifest before packaging.


The continuity summary has its own schema namespace because its admission is
restricted to one predeclared cell. After analysis, this metadata tag was
renamed from the generic qualification-summary namespace; every statistic,
gate and admission decision was checked to be unchanged. The migration's old
and new source/output hashes are retained in each analysis receipt. Frozen
archives preserve the original run-time source.


## Code verification

Full AR suites: 172 JVM / 170 Scala.js tests; model: 99 each; fit: 744 JVM /
686 Scala.js. Affected PHRF preparation: 19 each; targeted AR/GLS laws: 18 each.
One empty historical opt-in qualification suite reports ignored on Scala.js;
its retained negative campaign is not an ordinary regression gate. Both
harness formatting checks pass. Fourteen external oracle receipts and twelve
Python receipt/manifest regressions pass. The unrelated long PHRF benchmark
started in the broad JVM laws invocation was stopped; that invocation is not
counted as a passed gate. Complete logs and dispositions are in
[the validation receipt](ar-correctness-followup-20261009/validation.json).


`scalafimCompileAll` passed across every module on JVM and Scala.js, with no
compiler warnings. The inherited external build's unused-key notices remain
in the log. The current Scala source bytes match the confirmation snapshot.
