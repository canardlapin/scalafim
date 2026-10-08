# Rank calibration diagnosis

The n=80 standard-root power requirement is infeasible for the frozen population. Even the optimal test with the population directions, signs, nuisance coefficients and scales known has **56.2932% power at size .05** for adding the .20 third root. Its ceiling is **61.4408% at size .065**, still below the required 80%. The diagnosis also finds evidence of conservatism from estimating and ordering directions; closure does not explain the observed third-root power loss.

This packet completes a diagnosis, not qualification. Production inference, the v1 populations, seed namespace and acceptance criteria are unchanged. No confirmation stream was opened. M4.07/M4.09 and their downstream scientific gates remain unresolved.

## Evidence and scope

[The diagnostic plan](diagnostic-plan.json) was committed at `0065c6d6` before re-analysis and replay. It specifies all 12,200 retained pilot inputs, deterministic calculations, and ordinal zero from each of the 16 R3 null/alternative cells for JVM/JS replay. These are exposed inputs, not additional independent datasets. The power integration uses no RNG.

All 175 source locks and 593 artifact locks from the [preceding pilot packet](../umvpa-rank-pilot-20261007/README.md) still match. Each extracted input matches its retained hash. That packet's `execution.json` remains SHA256 `e960350e1e0da8540e3721c350dc8ebe1c9d380cf2e158fa2405b855ba929cce`. The three historical closed-only pilot cells remain unchanged; no missing raw values were reconstructed.

## A power ceiling independent of the CCA implementation

Compare the specified R2 population, with roots (.5, .3, 0, 0), to R3, with roots (.5, .3, .2, 0). Give an oracle the true coordinates and all distributional parameters. For the three-column nuisance design, subtract the known `.4 * (z1 + z2)` and divide by `sqrt(.68)`. The intercept design already has known zero means and unit marginal variances. All other pairs and the observed nuisance variables have identical distributions under the two hypotheses and provide no additional likelihood-ratio information.

Let the third standardized pair be `(x_i, y_i)`, let `rho=.2`, and put

```
u_i = (x_i + y_i) / sqrt(2)
v_i = (x_i - y_i) / sqrt(2)
W   = (1-rho) sum(u_i^2) - (1+rho) sum(v_i^2)
log(f1/f0) = -n/2 log(1-rho^2) + rho W / (2(1-rho^2))
```

Thus larger W is the likelihood-ratio ordering. For independent `A,B ~ chi-squared(n)`,

```
H0: W = (1-rho) A - (1+rho) B
H1: W = (1-rho^2) (A-B)
```

The [Neyman–Pearson result for simple hypotheses](https://doi.org/10.1098/rsta.1933.0009) makes this the most powerful size-alpha test of this pair of populations. Any size-alpha test of the composite rank null must also control size at this particular R2 population, so its power at R3 cannot exceed this bound. The bound uses **n**, not the estimated residual degrees of freedom, because the oracle knows the nuisance parameters. Additional independent permutation randomness cannot improve the optimum.

The [R integration](../../../tools/mvpa-inference/rank_power_bound.R) solves `P0(W>c)=alpha` and then evaluates `P1(W>c)`. It integrates once against the chi-squared density and separately against a uniform variable transformed by its quantile function. Density-tail truncation omits at most `1e-12` probability. The two parameterizations agree within `3e-12`; achieved size, a symmetry identity and the integer sample-size boundary also pass numerical checks. These are numerical integrations of the exact distributions, not asymptotic or Monte Carlo power estimates.

| n | Size | Optimal power | Critical W |
| ---: | ---: | ---: | ---: |
| 80 | .05 | .5629319394 | -2.6949465925 |
| 160 | .05 | .8210724648 | -22.2552671349 |
| 80 | .065 | .6144081515 | -4.9487369642 |
| 160 | .065 | .8537332910 | -25.4879867322 |

The first integer n at which this oracle reaches 80% power at size .05 is **151** (n150: .7986655; n151: .8010128). This is a necessary lower bound for this ideal experiment, **not a sufficient sample size or a recommendation for fitted CCA**. At n160 the best-case ceiling of 82.1% leaves little room for estimating unknown directions. Full results are in [oracle-power-bound.tsv](oracle-power-bound.tsv) and [oracle-minimum-n.tsv](oracle-minimum-n.tsv).

## Closure and estimation effects on the retained pilots

The [closure audit](closure-audit.json) covers every recorded hypothesis in all 61 fresh cells. In all eight R3 alternative cells, **raw H3 and closed H3 decisions are identical**. Removing closure would recover no third-root detections on these inputs.

Independent R calculations then compare the fitted procedure with two oracles on exactly the same datasets. The signed-pair oracle knows the third population axes and positive direction, but estimates residual association. The likelihood-ratio oracle additionally knows all means, scales and nuisance coefficients. Neither is a proposed replacement rank test, and neither can be used to qualify the fitted procedure.

| R3 alternative shape | Fitted closed H3 | Known third pair | Full-information LR |
| --- | ---: | ---: | ---: |
| n80.p4.q6.intercept | 6/200 | 104/200 | 105/200 |
| n80.p4.q6.three-column | 9/200 | 117/200 | 120/200 |
| n80.p6.q4.intercept | 6/200 | 111/200 | 113/200 |
| n80.p6.q4.three-column | 10/200 | 103/200 | 107/200 |
| n160.p4.q6.intercept | 33/200 | 160/200 | 165/200 |
| n160.p4.q6.three-column | 35/200 | 158/200 | 164/200 |
| n160.p6.q4.intercept | 39/200 | 167/200 | 171/200 |
| n160.p6.q4.three-column | 48/200 | 164/200 | 169/200 |

For R3 null cells, H4 is the first true null. With **known** population axes its tail has one variable on one side and three on the other. Conditional on the nuisance design, the m residual coordinates have isotropic independent Gaussian errors. The known-axis squared multiple correlation therefore has distribution `Beta(3/2, (m-3)/2)`, where `m=n-rank(Z)`. Its mean is `3/m`. This supplies an exact reference without permutations.

| R3 null shape | Fitted raw H4 | Known-axis H4 | Mean known-axis R² | Mean fitted-tail R² |
| --- | ---: | ---: | ---: | ---: |
| n80.p4.q6.intercept | 0/200 | 5/200 | .03216 | .02146 |
| n80.p4.q6.three-column | 0/200 | 9/200 | .03664 | .02204 |
| n80.p6.q4.intercept | 1/200 | 13/200 | .03860 | .02366 |
| n80.p6.q4.three-column | 1/200 | 11/200 | .03680 | .02259 |
| n160.p4.q6.intercept | 1/200 | 10/200 | .01869 | .01447 |
| n160.p4.q6.three-column | 1/200 | 9/200 | .02000 | .01494 |
| n160.p6.q4.intercept | 5/200 | 8/200 | .01878 | .01497 |
| n160.p6.q4.three-column | 3/200 | 13/200 | .01879 | .01345 |

The fitted-tail mean is 60–67% of the known-axis mean at n80 and 72–80% at n160. Feeding the fitted statistic into the same beta tail gives a useful reference value, **not a calibrated p-value for fitted directions**. Its difference from the recorded raw H4 permutation probability has cellwise mean between -.000034 and .00514, and RMS .0285–.0314 in these eight cells. This is consistent with a broadly similar randomized-tail scale and an observed statistic reduced by estimating and ordering the directions, rather than an arithmetic failure in the Monte Carlo probability. This exploratory comparison does not prove a unique cause or establish a finite-sample error law for every partial null.

All 61 cells and all dataset-level observed statistics remain in [diagnostic-summary.tsv](diagnostic-summary.tsv) and [independent-observed.tsv.gz](independent-observed.tsv.gz). The displayed subsets follow the predeclared R3 diagnostic scope. Counts from different cells are not pooled into a qualification result.

## Implementation checks and limits

The [published procedure](https://pmc.ncbi.nlm.nih.gov/articles/PMC7573815/) uses completed canonical coefficient spaces, tail refits, nuisance residual coordinates and cumulative-max closure. Its all-null simulations also show decreasing rejection at later roots. That result does not establish near-5% rejection for the first true null after weak nonzero roots. The original implementation was inspected at [PermCCA commit 5046146](https://github.com/andersonwinkler/PermCCA/blob/5046146c4882a68896cac95f92132e62c5628669/permcca.m); its source hash and license are recorded in [reference-source.json](reference-source.json). No external implementation was copied or vendored.

The production replay covers ordinal zero of every R3 null/alternative shape on both platforms, with three explicit nonidentity actions: reversal, cyclic shift and swapping the first two coordinates. It consumes no new random draws. Base R independently checks:

- Residual-basis orthonormality, nuisance orthogonality and its full residual projector.
- All initial roots and observed tail statistics, both in exported residual coordinates and using R's original-space residualization without the exported basis.
- All four tail statistics for each of the three explicit actions. Sharing the checked basis fixes the action's coordinates; the reference QR, coefficient solves, complements and SVD are computed independently.

All **32 platform/case exports** agree within the declared `1e-10` tolerance. Maximum geometry discrepancy is `3.09e-14`; maximum statistic discrepancy is `2.89e-15`. See [replay-checks.tsv](replay-checks.tsv). This finds no implementation discrepancy on these cases and complements the preceding complete-group fixture checks; it is not a universal correctness proof or statistical qualification.

Full owning gates pass: **465 tests plus one opt-in skip on each of JVM and JavaScript**, with no compiler warnings. The new replay test runs only when explicitly given exposed inputs; the existing confirmation adapter remains skipped. No production Scala code changed.

## Execution, retained failures, and reproduction

The successful R re-analysis took 57.61 seconds with a 600-second timeout and peak child RSS 238,452,736 bytes (about 227.4 MiB). JVM and JS gates ran serially using the worktree's warm sbt server with a 3 GiB heap and four configured processors. Their wrapper times were 104.7 and 40.6 seconds. These are diagnostic execution measurements, not reference benchmark admission.

Two diagnostic-tool failures are retained under `attempts/`. The first R run completed its calculations, but `/usr/bin/time -l` exited 1 because the sandbox denied `sysctl kern.clockrate`. A direct bounded subprocess run exited 0 and reproduced both output tables byte-for-byte after decompression. The first independent replay checker had an R `vapply` return-shape declaration error; correcting that declaration yielded the passing comparisons above. No pilot input, production calculation, threshold or locked pre-analysis source changed in either repair.

For a fresh worktree at this packet's revision, run `prepare.py` to validate the old receipts and extract the retained inputs, then `run_diagnostics.py`. The committed power table is reproducible with `Rscript tools/mvpa-inference/rank_power_bound.R OUTPUT_DIRECTORY`. To reproduce the platform replay, set `SCALAFIM_RANK_DIAGNOSTIC_CASE_LIST` to the newly written `replay-case-files.txt` and `SCALAFIM_RANK_DIAGNOSTIC_OUTPUT` to a fresh `scala-replay.jsonl`, then run `python3 tools/build/sbt-warm mvpaJVM/test mvpaJS/test` with `SBT_WARM_HEAP=3g SBT_WARM_CPUS=4`. `check_replay.py` checks the resulting 32 records (or the archived export after restoring its input paths through `prepare.py`). Preserve fresh-run outputs separately from this committed evidence. The original archived index includes the original worktree paths; `prepare.py` replaces them with paths for the new checkout.

`execution.json` binds this packet's source and artifact bytes, preserved prior locks, process status, test totals, replay discrepancies and raw-output equality. Compressed logs and test reports retain the original bytes.

## Next scientific decision

Do not launch the v1 rank confirmation campaign to try to recover an unattainable power target. Preserve v1 and these failures as the historical specification and evidence.

The next concrete work is a separately versioned rank qualification proposal. It must distinguish control of false positives from the stronger requirement of near-nominal rejection, retain the .20/n80 cells as difficult sensitivity cases, and define any additional powered design with justified sample size and allowance for estimating directions. Neither alpha, the current lower size band, the current 80% target, nor the current populations are changed by this diagnosis. The n151 oracle boundary must not be treated as sufficient design sizing. Any revised method or population needs fresh simulator QA and pilot streams under a new namespace, independent validation, explicit resource admission and a committed protocol before confirmation. The existing n160 pilot losses still need to be addressed; the power ceiling alone does not resolve them. Broader component, voxel, group and performance qualification remains with its existing owners.
