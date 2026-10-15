# UMVPA qualification progress, 2026-10-07

This packet advances the user-authorized M4/M5 sequence after M5.03. It does
not close M4.07–M4.10, M5.04–M5.07, or the final release audit. M5.03 and its
original execution logs are committed at `0b6eaa77` and `bab3af77`.

## Source and executed work

| Commit | Change |
| --- | --- |
| `39fe611b` | Schema-2 raw/closed rank recorder, strict Python controls, explicit nuisance population construction and algebraic stream checks. |
| `7819ab14` | Pre-execution source/resource manifest for all 64 rank generator QA cells. |
| `8b5f9dde` | Independent adverse group oracles, normal-tail and PM precision repairs, claim matrix and native workflow guide. |
| `341534b7` | Frozen unchanged historical fixture and bounded cross-platform recorder replay. |

[`execution.json`](execution.json) binds final source hashes and evidence
artifacts. Raw test outputs, including failed attempts, are compressed without
editing their bytes. `raw-output-locks.json` records compressed and original
hashes. This work ran in an isolated worktree; PHRF source and its build server
were not changed.

## Rank recording and generator QA

The recorder now keeps actual raw exceedance counts, raw p-values, closed
cumulative maxima, both decision vectors, ordered family members and both
truth classifications. The historical `p_values`/`reject` fields keep their
closed sequential meaning. Failed records carry no partial inferential output.
Python checks reject omitted stages, changed identities, inconsistent plus-one
fractions, incorrect closure, nonfinite probabilities and substituted raw
power/family decisions. Old closed-only records cannot establish raw metrics or
serve as schema-2 confirmation records.

Primary metric bindings can explicitly select raw or closed pointwise decisions;
family error and detectable-rank power require closed decisions. No choice has
been silently frozen. The requested user clarification remains pending. The
earlier R2/H3 closed pilot result, 2/200 with CP90 upper bound below .035,
remains in the [prior evidence](../umvpa-inference-calibration-20261007/preconfirmation-metrics.md).
No historical pilot was rerun or reclassified.

The [nuisance construction](../../plans/unified-mvpa-rank-population-v1.md)
specifies two independent covariates, each correlated .4 with each observed
score, while preserving the exact conditional canonical roots. Its algebraic
checks preserve the old intercept draws and verify the full covariance's
Schur complement. This resolves the previously missing three-column population
definition without treating marginal correlations as the tested roots.

The committed QA manifest specified 64 cells, each with exactly 10,000
independent datasets. All **640,000** datasets completed generator moment QA:

| Check | Maximum observed | Frozen limit |
| --- | ---: | ---: |
| Absolute mean error | .00414885924 | .02 |
| Absolute covariance error | .00548761005 | .03 |
| Noiseless coefficient error | 5.55112e-16 | 1e-12 |
| R worker peak RSS | 165,707,776 bytes | 2 GiB |

The full serial QA took about 277 seconds. Every cell retains its exact seed
assignment TSV (gzip), plan, process receipt, moments, R session and coordinator
output under `rank-qa/`. These are generator checks: **no permutation
calibration, power, coverage, or confirmation campaign was executed**.

The unchanged fixture with B=1999 was replayed through the production rank
binding on JVM and Scala.js. Both returned identical raw counts
`[1442,1730,1684,1273]`, raw p-values `[.7215,.8655,.8425,.637]`, and closed
p-values `[.7215,.8655,.8655,.8655]`. Closed values match the historical fixture
exactly. `recorder-parity.json` records the check. The supervisor bounds and
observes sbt-server RSS; it is not complete Node/whole-process benchmark evidence.

## Group numerical qualification

Seven deterministic adverse quantile fixtures cover four explicit policies
(FE/z, DL/z, DL/mKH, PM/mKH), including n=8 heterogeneity, three-regressor n=80
known/estimated controls, df 8/25/50 and quarter-leverage n=20. Independent base
R uses linear solves, `uniroot`, and `pnorm`/`pt`/`qt`; the feasible fit receives
only reported variances. The original truth variances and first-level df stay
separate. These deterministic vectors are not population draws or coverage
evidence. Historical adverse simulations remain required.

The first execution exposed normal p-value errors up to roughly 8e-8, consistent
with the old documented 1e-7 approximation but outside M0.05's 1e-10 combined
oracle gate. The repaired existing `Distributions.erfc` evaluates the
incomplete-gamma series and upper-tail continued fraction; references are
[NIST DLMF 8.7.1](https://dlmf.nist.gov/8.7.E1) and
[8.9.2](https://dlmf.nist.gov/8.9.E2). R log-tail fixtures also test subnormal
normal tails and the series/fraction boundary.

The stricter oracle then exposed PM stopping at a Q residual of `1e-9*df`
before coefficients/intervals reached the required precision. Tightening that
existing solver criterion to `1e-12*df` fixes the tested discrepancies while
retaining the iteration cap and explicit nonconvergence. No numeric admission
tolerance was relaxed. The second failure log also preserves an oracle-tail
issue: direct R `pnorm(-38)` underflows to zero; the final independent fixture
uses `exp(pnorm(..., log.p=TRUE))` to retain the representable tail.

Final owning-module gates:

| Module | JVM passes | Scala.js passes | Skips |
| --- | ---: | ---: | --- |
| mvpa | 458 | 458 | One opt-in campaign test per platform; separately exercised by the fixture replay. |
| group | 105 | 104 | None; JVM has an additional classpath-boundary test. |
| mvpa-group | 27 | 27 | None. |

That is 1,179 passes plus two default opt-in skips, with no warnings. The
separate recorder replay passed once per platform. Python controls pass 14
tests, and the R population algebra checks pass. The group suite adds 28
independent case/policy comparisons plus two control/tail tests. Original
failure logs and XML remain retained alongside the successful run.

## Workflows, cost boundaries and remaining gates

The [workflow guide](../../examples/unified-mvpa-workflows.md) connects the real
classification, axis repair, fold change, retained RSA, confirmation, group and
held-out-subject paths. The atlas classification command ran successfully:
three regions, four features and eight assessed samples each. Its perfect
synthetic accuracy is a wiring result, not comparative performance.

The [claim matrix](../../plans/unified-mvpa-claim-boundaries.md) records the
scientific and cost distinctions. Inspection found Mac14,12, 10 physical CPUs,
32 GiB, macOS 15.1.1 and Node 24.21.0; M0.06 freezes Mac15,11/M3 Max, 36 GB,
macOS 14.3 and Node 22. Current results cannot be pooled into that reference
performance court. No benchmark was substituted or admitted.

The remaining work is explicit:

1. Resolve and freeze the primary raw-versus-closed pointwise metric decision
   before confirmation, retaining the original conservative pilot observation.
2. Complete fresh pilot feasibility and independent rank oracles across the
   expanded grid. The defined 64-cell rank confirmation subset alone requires
   480,000 datasets and 959,520,000 nonidentity draws. This exact count is in
   `rank-confirmation-resource-plan.json`; it supplies no runtime bound.
3. Finish the voxel/component/refusal population inventory and scientific
   admission. The voxel omnibus candidate cannot replace the required
   component-specific family or unsupported FDR/selection-aware methods.
4. Run M5.04's predeclared group simulations and retain every historical
   adverse control, alignment refusal and first-level uncertainty qualification.
5. Supply complete benchmark drivers/baselines and an admitted reference
   profile, then run the frozen matched comparisons; complete independent
   analyst usability review and the native M4/M5/final release gates.

No missing gate above is represented as passed by a numerical fixture, source
commit, clean build, generator QA, proposal, or this report.
