# UMVPA rank method: prospective plan v2

Status: review-backed proposal, 2026-10-07. **This is not a frozen v2 calibration protocol and changes no v1 acceptance criterion.**

Owner packets: M4.07 `bd-01M2BNHG03Y1EV95H0CMQ86NRN` for rank semantics and the Multivar seam; M4.09 `bd-01M2BNHMAR9S58QCMT2DEM16VN` for independent mathematical/numerical/statistical checks; M4.10 for reference performance and final claim boundaries. No new assignee or completed qualification is implied.

The [mathematical review](../verification/umvpa-rank-math-review-20261007/README.md) changes the recommended order of work. The immediate problem is not just sample size: Euclidean completion of canonical coefficients makes higher-rank permutations depend on an equivalent change of feature coordinates, and the fitted-tail method still needs a composite-null justification. The [existing power ceiling](../verification/umvpa-rank-diagnosis-20261007/README.md) remains a separate constraint.

**Recommendation:** establish an explicitly Gaussian, conservative rank lower-bound route with a finite-sample error-control argument. Repair score-space geometry and retain the stepwise method as a research comparator while its statistical status is resolved. Evaluate efficiency after validity has a clear basis. Keep the remaining UMVPA claim boundaries and dependency gates explicit while the rank method is reviewed.

## 1. Repair the geometry upstream

Deliver a small Multivar change using its existing Gale QR/SVD capabilities:

- Complete the canonical **scores in whitened coordinates**, retaining every candidate dimension, instead of appending a Euclidean complement to the original coefficient matrix. Preserve all initial roots, including zero roots.
- State the covariance metric and return/record the method identity. The old construction remains reproducible through its pinned historical revision and fixtures; do not introduce a permanent compatibility implementation in ScalaFIM.
- Establish behavior for repeated roots and nearly singular inputs. When a tail cut splits an unidentified tied eigenspace, specify a justified policy or explicit unavailability; do not silently rely on an arbitrary SVD orientation.
- Add independently generated numerical fixtures for invertible shears, positive rescaling, rotations, column order, block interchange, unequal/equal dimensions, identity actions and complete small groups. Require observed roots and corresponding transformed tail statistics to agree within declared tolerances for the supported well-conditioned cases. Verify the full retained score space and orthogonality, not just a single p-value.

Exit: upstream and consuming JVM/JS gates pass; the new construction is source-bound and differs from the historical method by an explicit version. The review's 642-versus-672 counterexample must become invariant. This milestone changes numerical geometry only and does not admit significance.

## 2. Implement a Gaussian reference with an explicit validity argument

Before selecting a more powerful approximation, implement the conservative interlacing reference derived in the review. For Hk set `s=k-1`, compute the **observed unscaled Wilks tail from the full projected blocks**, and compare it with the full Wilks statistic of independent Gaussian blocks of dimensions `(a-s,b)`, where `a=min(p,q)` and `b=max(p,q)`. This retains the observed statistic while changing its reference law. Remove s variables from only the smaller side, a convention fixed before reading outcomes. Use `m=n-rank(Z)` zero-mean residual rows; do not subtract another intercept. Rootwise interlacing and monotonicity of `-log(1-r^2)` give the bound. Do not introduce unequal Bartlett scaling factors on the two sides of this inequality. The original largest-root bound can serve as a secondary independent check.

Use independently generated finite Gaussian reference draws or a validated CDF with a controlled numerical error. A Tracy–Widom approximation and a chi-squared approximation are not exact substitutes in this small-dimensional implementation. A finite Monte Carlo probability must include the observed comparison through the inclusive plus-one rule, bind its own independent stream, and retain every failure. Reusing one random cutoff table across all calibration datasets would change the dependence/uncertainty calculation; the initial implementation should instead give each dataset/hypothesis its own reference stream.

Write and independently check the short argument: rank-null interlacing gives stochastic domination; Gaussian projection preserves the reference model; the Monte Carlo rank test preserves the conservative inequality; sequential closure then controls false rank overestimation. Keep the assumptions conditional on the fixed discovery representation and nuisance design.

Exit: an explicit theorem/assumption record, small-dimensional analytic checks (including the one-column beta case), independent R fixtures, platform parity, and a result that distinguishes a conservative Gaussian reference from an empirically calibrated approximation. Numerical/statistical kernels stay upstream; ScalaFIM owns the scientific claim, evidence bindings and refusal policy. Keep `admittedDetectableRank` unavailable until its complete admission packet passes.

## 3. Compare mechanisms before choosing sample size or a final procedure

Compare three explicitly named methods on paired datasets: the historical construction, the score-completed stepwise candidate, and the Gaussian interlacing reference. The oracles that know true directions remain diagnostic ceilings, never competitors that could replace the actual estimand.

The following is a concrete proposed exploratory inventory, to be turned into a committed manifest only after Steps 1–2 and the resource probe:

| Purpose | Populations | n | Dimensions | Nuisance | Cells |
| --- | --- | --- | --- | --- | ---: |
| Method and boundary diagnosis | R0, R1, R2, R3 from v1; plus R3 weak `(.50,.30,.02,0)`, strong `(.80,.60,.40,0)` and near-unit `(.98,.95,.90,0)` | 80, 160 | (6,4), (4,6) | intercept, existing three-column design | 56 |
| Sizing the .20-root claim | Original R3 `(.50,.30,.20,0)` | 320, 640 | (6,4), (4,6) | both above | 8 |

Start with 200 datasets per cell and B199 for each stochastic method/reference: **12,800 unique exploratory datasets**, shared across methods for paired comparison. Record raw and closed decisions for every stage; first-true-null error, false rank overestimation, closed H3 power in standard R3, and all failures. Report lower-rank boundaries as well as exactly rank-(k-1) nulls. The .02 preceding root challenges a near-zero boundary; the near-unit family checks the opposite boundary where deflation can leave both blocks in a shared estimated residual row space. Analyze method differences as paired comparisons and retain all cells. These are mechanism/sizing diagnostics, not the 10,000/5,000 confirmation studies.

Before execution, probe at most eight separate fixture cases covering n80/n640, both dimension orientations and both nuisance designs. Measure the complete public adapter, including dense residual-basis construction, validation, reference generation and receipt storage. A tiny-SVD timing alone is insufficient. Proposed campaign limits are one worker, four configured processors, a 3 GiB JVM heap, a 4 GiB measured resident-JVM ceiling and a one-hour wall cap. Verify host headroom first. If the measured inventory does not fit, revise and commit the resource/partition plan before starting; do not silently shorten cells, drop failures or substitute a smaller sample size. These are proposed limits, not an existing resource admission.

All new method-comparison streams must use a new, committed namespace with separate fixture/pilot/confirmation roots and separate outer-dataset, method and hypothesis children. Paired methods share input data deliberately; per-dataset reference randomness remains independently assigned. Existing v1 streams and raw artifacts remain immutable and cannot become fresh validation evidence for the new method.

Exit: a complete comparison receipt identifying which difference is due to geometry, remaining conservatism, and achievable power. If the corrected stepwise method still has no adequate validity argument, it remains a research candidate regardless of attractive power. If the conservative reference is too weak for useful rank detection, that limitation is reported rather than hidden by changing the estimand.

## 4. Pursue efficiency only if the reference route is inadequate

The next research candidate would be a **rank-constrained Gaussian likelihood-ratio/bootstrap procedure**. Fit the composite null while preserving its earlier associations; generate from that null; refit nuisance, marginal covariance and all canonical directions within each replicate. Start from a derived/reference-checked null MLE, not an assumed recipe for truncating a covariance matrix.

This candidate would be approximate unless a stronger result is established. Weak/zero preceding roots and repeated roots must be covered explicitly, because they are nonregular boundaries. Compare to the valid conservative reference and retain any inflation or conservatism. A naive global permutation, an ordinary fitted-covariance bootstrap without rank restriction, and independent-but-imperfect frozen deflation are not replacements for this work. Do not add this fourth method to the initial campaign without a separate source, algorithm and resource freeze.

Exit: either a justified, explicitly bounded-scope improvement, or a documented decision to retain the conservative route. There is no requirement to manufacture a near-nominal or high-power result where the data do not support one.

## 5. Freeze the actual v2 qualification protocol

Keep the original v1 record as an unsuccessful qualification. The proposed new protocol must make two distinct scientific requirements explicit:

1. **Validity:** the intended level remains alpha=.05. For a theorem-backed conservative route, confirmation checks the upper side of error control: propose a one-sided 95% binomial upper bound at most .065, alongside the formal level-.05 argument and complete failure accounting. Retain the old CP90 `[.035,.065]` assessment as a separately labeled near-nominality/efficiency diagnostic. This is a prospective change in admission policy, not retroactive success under v1 and not an assertion that 6.5% is the intended level.
2. **Usefulness:** retain the one-sided 95% lower power bound of at least .80 for the declared powered design, if that claim is pursued. Select a feasible sample size/effect scope from the completed pilot before opening new confirmation data. Keep n80/.20 and the other original designs as reported sensitivity cases; the n80 target remains infeasible. The n151 oracle boundary is not fitted-CCA sizing, and a synthetic n640 study does not imply that an actual application has 640 independent subjects or units.

Retain 10,000 null datasets, 5,000 alternative datasets and B1999 where applicable for final confirmation; any change requires an explicit protocol revision and its reason. Specify all populations, primary/secondary metrics, Monte Carlo rules, source/reference locks, exact method identity, failure policy, host profile and resource inventory before that run. Do not claim uniform finite-sample validity for an approximate method merely because finitely many cells pass.

Exit: independent review accepts the method-specific assumptions and protocol; the bounded confirmation campaign passes its declared requirements; M4.10 establishes the reference performance/claim boundary; only then can a typed rank-admission result be enabled. Unsupported temporal, repeated-subject, non-Gaussian and full-selection claims remain unavailable. Nonrejection never becomes an upper bound on total brain/task association or a count of anatomical networks.

The immediate next implementation packet is **Steps 1 and 2**, with their mathematical and numerical checks. The large campaign comes after those decisions. Group summaries, heterogeneity and held-out-subject work keep their existing estimands and dependency gates; this rank review supplies no new qualification for them.
