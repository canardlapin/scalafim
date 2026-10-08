# Mathematical review of UMVPA rank confirmation

**The current procedure is a coherent published candidate, but it is not ready for scientific admission.** The Gaussian residual geometry, complete-null test and sequential closure have sound arguments under explicit assumptions. Higher-rank calibration has a separate unresolved justification. This review additionally demonstrates a concrete defect relative to the intended inference about subspaces: the coefficient-completion rule makes later permutation probabilities depend on an arbitrary invertible change of coordinates.

The next action should therefore be a geometry repair and a reference test with an explicit error-control argument, followed by focused method comparison. Increasing sample size alone is premature. The [prospective plan](../../plans/unified-mvpa-rank-method-plan-v2.md) specifies that sequence. It is a proposal, not an amendment to the frozen v1 protocol or an admitted new procedure.

## Scope and verdict

Reviewed baseline: ScalaFIM `65cee783`, Multivar `ab811e257dd67f77e8c3b70cb1ea600f274429a3`, the frozen rank protocol and metric addendum, and the retained pilot/diagnostic packets. Production files and prior evidence are unchanged. New calculations here are deterministic mathematical examples, including exhaustive finite groups; they are not another rate study.

| Part | Assessment | Consequence |
| --- | --- | --- |
| Independently frozen candidate subspaces | Sound conditional estimand | The tested rank concerns those projected population blocks. Independent discovery does not make confirmation-fitted canonical directions known. |
| Common Gaussian nuisance projection | Exact under the conditional matrix-normal model below | Retain the joint-Gaussian restriction and one row per independent unit. |
| H1 complete-null permutation | Valid under that model and the stated sampling rule | Its argument does not automatically extend to H2–H4. |
| QR/SVD and cached cross-basis refits | Correct algebra on the checked cases | Existing numerical parity is useful, but cannot establish a sampling law. |
| Euclidean coefficient complement | Demonstrably coordinate-dependent for unequal dimensions | Repair in the covariance/score metric before developing a new candidate. |
| Fitted higher-rank permutation tails | Uniform finite-sample error control is not established by the current argument | Keep their inferential status unavailable; a geometry fix alone does not close this gap. |
| Plus-one probabilities and cumulative-max closure | Sound conditional on valid individual reference laws | Neither operation repairs an invalid reference distribution. |
| Non-Gaussian or dependent rows | Outside the implemented rank claim | Keep the existing refusals. |

## What the Gaussian argument actually establishes

Condition on the independently frozen projections and the nuisance matrix Z. Write

```
X = Z Bx + Ex,       Y = Z By + Ey
[Ex,Ey] ~ MatrixNormal(0, I_n, Sigma)
Q'Q = I_m,          Q'Z = 0,          m = n-rank(Z).
```

Here Sigma is the joint covariance of the projected blocks and the design is fixed, or conditioned on under this error law. Then `[Q'X,Q'Y]` has m independent, identically distributed, zero-mean Gaussian rows with the same Sigma. This follows directly from linear Gaussian transformation and `Q'I_n Q=I_m`. Do not center these m coordinates again: centering was already included in Z.

Under H1, the cross-block covariance is zero, hence the two Gaussian blocks are independent. A permutation of one residual block preserves their joint law. At k=1, completing the coefficient spaces spans each original block, so the cached computation is algebraically the ordinary statistic on the permuted data. The usual group-randomization argument therefore applies. [Hemerik and Goeman's finite-group result](https://doi.org/10.1007/s11749-017-0571-1) makes clear that the null invariance and the sampling design are essential; the plus-one formula alone is not sufficient.

The code already requires a separate joint-Gaussian declaration and refuses dependent designs in [RankConfirmation.scala](../../../modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/pattern/RankConfirmation.scala). That restriction is mathematically meaningful. With iid Rademacher errors and an intercept, for example, an orthonormal residual basis can have columns

```
q1 = (1,-1, 0, 0)/sqrt(2)
q2 = (1, 1,-1,-1)/2
q3 = (0, 0, 1,-1)/sqrt(2).
```

All three residual coordinates have variance one, but their fourth moments are 2, 2.5 and 2. Thus they are not exchangeable. [Exact enumeration of all 16 sign vectors](non-gaussian-residual-moments.tsv) verifies the calculation. Gaussianity is sufficient here; covariance whitening by itself is not a general exchangeability proof. This is a limitation of extensions, not a defect in the current Gaussian-only gate.

## A demonstrated coordinate-dependence defect

The larger block Y has canonical coefficients B and covariance metric `S=Y'Y`. The implementation completes B with `N=null(B')`, then uses `Y[B,N]`. The [pinned Multivar implementation](https://github.com/canardlapin/multivar/blob/ab811e257dd67f77e8c3b70cb1ea600f274429a3/modules/inference/shared/src/main/scala/multivar/inference/StepwiseCanonicalRank.scala#L144) uses precisely this coefficient-space complement. It agrees with the [pinned original PermCCA implementation](https://github.com/andersonwinkler/PermCCA/blob/5046146c4882a68896cac95f92132e62c5628669/permcca.m#L123). Thus this is a method-level issue, not a divergence introduced by the Scala port.

Although `B'N=0`, the score cross-product is `B'SN`, which generally is not zero. Under the equivalent representation `Y*=YM`, for invertible M, the canonical coefficients become `B*=M^-1 B`. A coefficient complement then spans `M' N`, so its score space becomes

```
col(Y* null(B*')) = col(Y M M' N).
```

This need not equal `col(Y N)`. This calculation uses corresponding canonical directions, as obtained up to signs for distinct roots; tied-root choices need an additional policy. At k=1 the full spans still agree. After discarding earlier canonical variables, the retained larger-block subspace can differ, so permuted tail statistics can differ even though all original canonical correlations agree.

[mathematical_checks.R](mathematical_checks.R) constructs a six-row, two-by-three example with roots (.8,.2), applies an explicit invertible shear to the larger block, and enumerates all 720 row permutations. A common fixed orthogonal rotation avoids degenerate unit-root actions. No matrices or actions were drawn randomly.

| Completion rule | Original H2 count | Sheared H2 count | Maximum H2 statistic change |
| --- | ---: | ---: | ---: |
| Current Euclidean coefficient complement | 642/720 | 672/720 | 2.80424 |
| Score-orthogonal complement | 642/720 | 642/720 | 1.65e-14 |

The original and sheared H2 probabilities are .8916667 and .9333333. H1 counts remain 460/720; observed roots and observed Wilks statistics agree to floating-point precision. All actions and matrices are retained in [affine-actions.tsv](affine-actions.tsv), [affine-input.tsv](affine-input.tsv) and [affine-completion.tsv](affine-completion.tsv). Counts include the identity, with a `1e-12` tolerance only for algebraic ties.

The natural invariant completion is `N_S=null(B'S)`, or, more stably, a complement of the right singular vectors in the already whitened QR coordinates, mapped back to score space. Then `B'S N_S=0`, and under a coordinate change `N_S*=M^-1 N_S`, so the score span is unchanged. The diagnostic implementation uses the QR construction; no covariance inverse is needed. This restores the geometric property in the example. It **does not prove valid higher-rank p-values**. Coordinate dependence alone also does not prove that the current test inflates type-I error, nor does this example measure its contribution to the earlier power loss.

## Why the higher-rank validity question remains

For `Hk: rank(SigmaXY) <= k-1`, the earlier nonzero population modes are nuisance parameters. Canonical directions estimated on confirmation data depend on both blocks and their observed association. Removing their fitted columns does not by itself establish that the retained random matrices have the product/exchangeability law required by a permutation test. One needs a valid conditional symmetry argument or a uniform stochastic bound over the entire rank null, including weaker and lower-rank boundaries.

The [Winkler et al. procedure](https://pmc.ncbi.nlm.nih.gov/articles/PMC7573815/) motivates the fitted-tail construction and reports simulation support. Its all-null results do not establish uniform first-true-null calibration for our weak nonzero roots. The distinction is substantive: [Nielsen's bivariate rank analysis](https://doi.org/10.1093/biomet/86.2.279) shows different asymptotic behavior at complete independence and at a nonzero leading correlation. A generic chi-squared approximation or fitted-null bootstrap therefore also needs boundary-specific justification. We have not established a universal invalidity theorem for the current stepwise test; we have established that the simple complete-null permutation proof is insufficient for its higher-rank claim.

A useful limiting example is `X=[g,e]`, `Y=[g,f]`, with independent standard Gaussian m-vectors g,e,f. After the perfectly shared mode is removed, both tails lie in the **same random (m-1)-dimensional row subspace**, perpendicular to g. Their squared correlation has law `Beta(1/2,(m-2)/2)`. Arbitrarily permuting one tail in the ambient m coordinates does not preserve that conditional row-space constraint. This singular, unit-root example is outside the numerical adapter's admitted inputs, so it is not an end-to-end type-I counterexample for the code. It identifies why near-unit leading roots need explicit analysis, rather than assuming that deflated scores inherit the original m-row exchangeability law.

Two tempting changes do not settle this:

- Refit ordinary CCA after globally shuffling the raw blocks: that destroys all cross-block association and references H1, not the composite Hk.
- Learn the discarded directions on an independent sample: independence removes selection reuse but not estimation error. For identity marginal covariance and `SigmaXY=diag(.5,0)`, an estimated leading direction rotated by 30 degrees leaves association `.5*sin(30 degrees)^2=.125` between the two retained tails. [The exact calculation](frozen-direction-leakage.tsv) shows that a tail-independence test could reject while the population still has rank one. Fixed projected association and additional population rank are different hypotheses.

Closure remains useful. If the true rank is r, any false closed rejection implies rejection of H(r+1), so

```
P(any false closed rejection) <= P(p_(r+1) <= alpha) <= alpha,
```

provided that raw p-value is valid throughout its composite null. No independence between stages is required. Conversely, closure cannot supply that missing raw-stage guarantee. A universal `alpha^k` rejection rule does not follow from this argument. The retained pilots already show that closure removed no standard-root H3 detections.

## A reference route with a finite-sample guarantee

[Johnstone, Lemma 1 and its appendix](https://arxiv.org/pdf/1009.5854), gives a Gaussian rank bound by interlacing: under rank at most s, the (s+1)st sample root is stochastically bounded by the largest root after removing s variables from one block to leave it independent of the entire other block. This permits a conservative rank test without estimating the removed directions. Use the finite Gaussian reference, not the paper's optional Tracy–Widom approximation, for small dimensions.

We can retain **our existing unscaled Wilks statistic** by applying the same rootwise interlacing. Here is the derivation for the smaller block of dimension `a=min(p,q)` and the other of dimension `b=max(p,q)`. Under rank at most s, a population change of coordinates makes the first `a-s` variables of the smaller block independent of the entire larger block. QR in that column order makes the reduced cross-basis matrix C0 a row submatrix of C. Consequently, for `j=1..a-s`,

```
r_(s+j)(C) <= r_j(C0)
W_(s+1)(C) = -sum(j=1..a-s, log(1-r_(s+j)(C)^2))
           <= -sum(j=1..a-s, log(1-r_j(C0)^2)).
```

The scalar function `-log(1-r^2)` is increasing. The right-hand side has the ordinary complete-null Wilks law for independent Gaussian blocks of dimensions `(a-s,b)` with **m zero-mean rows**. The unknown population coordinate change is needed only in the proof; the proposed test computes the observed full-block roots and never estimates that change. [Deterministic checks](interlacing-checks.tsv) verify the inequalities in fixed examples. This argument extends the largest-root bound to our statistic; it is not an assertion that the fitted-tail permutation law equals this reference.

On all 12,200 retained pilot inputs, the direct sum of the remaining root contributions matches the independently calculated observed tail-refit Wilks statistic, with maximum discrepancy `8.22e-15`; see [observed-wilks-identity.json](observed-wilks-identity.json). No new p-values or rejection rates were computed in this identity check.

Generate independent Gaussian reference blocks of those dimensions and compute their full unscaled Wilks statistic. Conditional on Z, the projected errors retain the required matrix-normal law. Independent Monte Carlo draws with an inclusive plus-one probability preserve the conservative bound by stochastic ordering. A reused random reference table requires additional treatment of its uncertainty; it cannot silently be treated as a deterministic exact CDF in binomial calibration.

This is a proposed validity baseline, not a promised power improvement. In particular it removes s variables from only one side. Using a `(p-s) by (q-s)` independent reference without another theorem is not licensed by this bound. Closure can combine these conservative individual tests into a rank lower bound within the fixed candidate spaces.

## Implications and evidence limits

The earlier numerical checks remain correct: they demonstrated agreement with the implemented published construction. They did not test coordinate invariance or prove composite-null calibration. The new finding narrows the next implementation task, while the validity gap requires a separate reference/claim decision.

The [previous power ceiling](../umvpa-rank-diagnosis-20261007/README.md) also still holds: the n80/.20 standard-power requirement cannot reach .80 at size .05 even with known population parameters. Correcting geometry cannot overcome that information limit. Neither this review nor the prospective plan edits the frozen v1 size band, target, populations, namespace or evidence.

The review-only [Scala suite](RankMethodReviewSuite.scala) checks this finite-group example directly against the pinned production kernel on JVM and JavaScript, using independent R expectations. Both platforms reproduce counts 642 and 672. Full owning gates pass **465 tests with two opt-in skips per platform**, without compiler warnings. [run_review_tests.py](run_review_tests.py) temporarily copies the review suite into test sources, runs both owning gates and removes it. It documents current behavior; it is not a regression contract requiring a future corrected algorithm to retain this defect.

The first attempt to inject test sources by reapplying sbt settings stalled at the 3 GiB heap limit. Its log is retained under `attempts/`; only the verified server for this worktree was stopped. The ordinary-build retry completed both gates in 95.54 seconds and removed the temporary source. Exact commands, source hashes and receipts are recorded in `execution.json`. No production method was changed or admitted by these checks.
