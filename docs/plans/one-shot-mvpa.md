# One-shot MVPA through a typed trial readout

Status: **partially implemented target architecture**

Planning bead: `bd-01KXZGWMKYHXXQMNEEY230XJNT`

Implementation status (2026-07-20): Phase 1 is implemented in the current
checkout under bead `bd-01KXZJ2JJGWPD3SDT0JNB22J7Q`. The shared `fit` kernel
now exposes a cached, Gale-backed LSS `TrialReadout` with a named estimability
axis and true adjoint; ordinary LSS fitting goes through the same operator.
Dataset-index enrichment and full temporal-preparation provenance remain
higher-level integration work rather than being invented by the low-level
matrix API.

Phase 2 is implemented under bead `bd-01KXZK2WX45BGK26ETKZNNHADA` and its typed
integration was hardened under `bd-01KXZMPDDSGAKH02A9B7264692`. `mvpa` now owns
dense/composed `PatternOperator` values through its canonical representation-
polymorphic analysis boundary, and the shared `mvpa-fit` bridge composes and
stacks run-local `A_r Y_r` operators while retaining run, trial, feature, and
estimability metadata. ROI selection is pushed into each run's `Y_r` before composition.
The materialized dense-analysis adapter remains the legacy-analysis parity
path.

Phase 3a is implemented under bead `bd-01KXZVNQBYVMS8G0YG6Z1AQPFN`.
`CrossValidatedOperatorRidgeAnalysis` fits centered hard or simplex targets
directly through operator products with Gale LSQR, applies a positive penalty
only to feature weights, and recovers an unpenalized intercept. Its typed
payload distinguishes class scores from probabilities and records convergence,
regularization, solver limits, operator provenance, normal residuals, and
forward/transpose application counts for every fold.
Soft multinomial logistic regression, fold-local feature scaling, nuisance
actions, alternate execution strategies, and crossover benchmarks remain open
Phase 3 work.

Related plans: [`mvpa-engine.md`](mvpa-engine.md),
[`multivar-operator-core.md`](multivar-operator-core.md), and
[`distributed-fit-interpreter.md`](distributed-fit-interpreter.md)

This plan expands the fit-specific convenience-wrapper item in
`mvpa-engine.md` into a distinct operator path; it does not change the existing
dense `PatternMatrix` contract.

## Decision

ScalaFIM should implement one-shot MVPA as a composition of two linear maps and
one trial-space loss:

\[
X = \mathcal A Y,
\qquad
\eta(W) = \mathcal A(YW),
\]

where:

- \(Y\) is the timepoints-by-features fMRI response;
- \(\mathcal A\) is a typed `TrialReadout` from timepoints to trials, initially
  the exact LSS readout already implicit in `LeastSquaresSeparate`;
- \(W\) contains spatial decoding weights; and
- the objective is evaluated on trial scores \(\eta\), not on HRF-convolved
  scan labels.

The first implementation should optimize through the composed operator without
making a whole-brain trial-by-feature beta matrix a public or required
intermediate. The public result is a decoder, predictions, and receipts for the
readout, folds, nuisance handling, optimization, and execution strategy.

This has two deliberately separate meanings of "one shot":

1. **Operator one-shot**: fit the decoder through \(\mathcal A(YW)\), with no
   required beta artifact. This is algebraically equivalent to fitting the same
   decoder to explicit \(X=\mathcal A Y\). It improves composition, memory
   control, and provenance, but does not by itself change the plug-in estimand.
2. **Statistical one-shot**: propagate temporal noise through \(\mathcal A\), or
   marginalize the latent trial responses, so the decoder no longer treats
   estimated trial patterns as independent error-free observations. This is the
   phase that can improve statistical efficiency.

Do not claim the first as the second. The exact-equivalence implementation is a
necessary architecture and correctness gate; the uncertainty-aware model is the
scientific destination.

## Why this architecture

The current LSS path is already a batched linear kernel. It prepares the design
once and computes all trial-by-voxel coefficients together; it is not literally
running a full GLM fit once per trial and voxel. A fused decoder therefore has
to earn its place on one of four grounds:

- no compulsory whole-brain beta materialization;
- a single typed path from time series to cross-validated predictions;
- correct separation of time-domain and trial-domain adjustments; or
- an uncertainty-aware estimand unavailable to a naive beta-then-classify
  pipeline.

For a single dense analysis, explicitly computing \(\mathcal A Y\) once may be
faster than repeatedly applying the operator during optimization. For many
overlapping searchlights, a bounded readout cache may also win. Materialization
is therefore an execution choice, not a semantic taboo. The objective and
provenance stay the same whether the runtime uses a fused operator, a bounded
block cache, or an explicit dense parity path.

## The estimand

For run \(r\), let:

- \(Y_r \in \mathbb R^{T_r \times P}\) be timepoints by features;
- \(A_r \in \mathbb R^{N_r \times T_r}\) be the prepared trial readout;
- \(C_r \in \mathbb R^{N_r \times K}\) be hard or probabilistic trial targets;
- \(Q_r \in \mathbb R^{N_r \times q}\) be optional trial-level covariates; and
- \(W \in \mathbb R^{P \times K}\) be shared spatial weights.

The basic multiclass model is:

\[
\eta_r = A_r(Y_rW) + \mathbf 1 b^\top,
\qquad
p_{rik} = \operatorname{softmax}(\eta_r)_{ik}.
\]

For probabilistic memberships, use simplex-valued rows of \(C\) in the proper
cross-entropy:

\[
\mathcal L(W,b)
= -\sum_{r,i,k} \omega_{ri} C_{rik}\log p_{rik}
  + \frac{\lambda}{2}\lVert W\rVert_F^2.
\]

The forward and adjoint operations are the whole computational core:

\[
W \mapsto A(YW),
\qquad
G \mapsto Y^\top A^\top G.
\]

This is the trial-readout version of decode-then-deconvolve. It keeps label
semantics in trial space while allowing all expensive feature operations to run
on scan-level scores.

### What is not a valid shortcut

Do not convolve class indicators with an HRF and treat the resulting scan rows
as soft class memberships. BOLD superposition is not a probability simplex:
overlapping responses add, HRF bases may contain negative lobes, and baseline
rows need not describe any class. The temporal design belongs inside
`TrialReadout`; uncertainty about a trial's class belongs in a trial-space target
type.

## Two nuisance domains, three distinct actions

The word "confound" currently hides several different operations. The API
should prevent them from being substituted for one another.

### 1. Temporal nuisance

Temporal nuisance has \(T_r\) rows. Motion traces, drift bases, censoring,
whitening, and fixed first-level regressors belong to `fit` and are recorded in
the readout preparation receipt. For LSS, the readout must include the same
fixed-design residualization currently applied before `computeBetas`.

Target type: `TemporalNuisanceDesign` or the existing typed fit-preparation
equivalent.

### 2. Trial-feature adjustment

Trial covariates have \(N_r\) rows. If the goal is to remove trial-covariate
variation from neural patterns, fit that adjustment on training trials only.
For a training covariate matrix \(Q_{tr}\):

\[
R_{tr}=I-Q_{tr}(Q_{tr}^{\top}Q_{tr})^{-}Q_{tr}^{\top},
\qquad
X^{*}_{tr}=R_{tr}X_{tr}.
\]

Held-out patterns use coefficients estimated from training trials:

\[
X^{*}_{te}
=X_{te}-Q_{te}(Q_{tr}^{\top}Q_{tr})^{-}Q_{tr}^{\top}X_{tr}.
\]

Both expressions remain linear in a spatial weight vector, so they can wrap the
composed readout without materializing \(X\).

Target type: `TrialFeatureAdjustment.FoldwiseResidualize`.

### 3. Conditional prediction control

If the scientific question is whether neural patterns add predictive value
conditional on trial covariates, fit:

\[
\eta = A(YW) + Q\Delta + \mathbf 1 b^\top.
\]

This is not feature residualization. The result should report incremental
held-out log loss or deviance against the \(Q\)-only model. Accuracy from the
combined \(A(YW)+Q\Delta\) predictor must not be labeled "neural decoding
accuracy" because it may be driven entirely by \(Q\).

Target type: `PredictionControl.Conditional`, with an explicit baseline-model
result.

Fold construction and stratification are a fourth concern already represented
by `FoldPlan`; they should not be encoded as a regression transform.

### No implicit cross-domain conversion

A temporal trace may be summarized into a trial covariate, but only through a
named transformation such as `TemporalToTrialSummary` carrying the event
window, aggregation rule, missingness policy, and provenance. There is no
default conversion from temporal nuisance to trial nuisance and no shared
untyped `confounds` argument.

## Module boundary

The dependency graph should remain acyclic:

```text
Gale operator and solver contracts
          |                 |
         fit              mvpa
   TrialReadout      PatternOperator + decoders
          \                 /
           \               /
            mvpa-fit (new bridge)
       run/fold/readout composition
                 |
       existing ROI/searchlight results

multivar -- optional later edge into mvpa-fit for pulled-back LDA
```

### `fit`

`fit` owns the time-to-trial statistical readout because it owns LSS, temporal
projection, whitening, and fit diagnostics.

Target enriched types after the dataset-aware bridge is available:

```scala
final case class TrialReadout private[fit] (
    linear: DoubleLinearOperator,
    timepoints: SelectedTimepointIndices,
    trials: TrialCoefficientAxis,
    receipt: TrialReadoutReceipt
)

final case class TrialReadoutReceipt(
    method: TrialReadoutMethod,
    temporalPreparation: ResponsePreparationProvenance,
    estimability: Vector[TrialEstimability],
    covarianceCapability: TrialCovarianceCapability
)
```

`LssPreparedDesign` should expose an LSS `TrialReadout`. The existing
`LeastSquaresSeparate.fit` should then call the same readout implementation so
there is one numerical truth. The operator must implement both `applyTo` and
`transposeApplyTo`; a forward-only wrapper is insufficient for optimization and
covariance propagation.

The trial axis should use stable trial identities and carry estimability. It
must not infer trial columns from names or positions; the existing
`EventTermColumnRole.Trial` metadata remains authoritative.

The first readout is design-only LSS. A fixed shared whitening operator can be
added later. Robust weights, response-estimated whitening, and voxel-specific
AR fits are only conditionally linear after preparation and do not define one
shared \(A\) over all features. They require an explicit prepared/readout scope
or a block-specific operator; they must not silently enter the initial
`TrialReadout` contract.

### `mvpa`

`mvpa` owns a generic sample-by-feature linear view and estimators that need
only forward and adjoint products.

```scala
final class PatternOperator private (
    sampleAxis: SampleAxis,
    sampleIndices: Vector[SampleIndex],
    featureIndices: Vector[FeatureIndex],
    linear: DoubleLinearOperator,
    provenance: PatternOperatorProvenance
)
```

Its orientation is features to sample scores: a \(N\)-by-\(P\) pattern table is
an operator \(\mathbb R^P \to \mathbb R^N\). A dense `PatternMatrix` adapter is
the reference implementation. `PatternSource[P]` and `RoiAnalysis[P]` make the
existing task, stream, and engine boundaries representation-polymorphic; the
source and analysis representations must agree at compile time. Dense and
operator aliases keep signatures readable. `RoiAnalysis.materializing` adapts
existing dense analyses explicitly for parity, while operator-native decoders
live on the same `MvpaTask`/`MvpaStream`/`MvpaEngine` path. There is no parallel
linear runner hierarchy.

### `mvpa-fit`

Add `scalafim-fmri-mvpa-fit` as a shared JVM/Scala.js bridge depending on
`fit` and `mvpa`. It should own:

- `RunTrialReadout`: one run's time-by-feature source, readout, and trial rows;
- `OneShotDataset`: validated run blocks with one common feature axis;
- run-row concatenation and fold restrictions;
- trial-feature and conditional-control plans;
- `OneShotMvpaTask`: the single-feature-set distributed boundary; and
- `OneShotMvpaEngine`: the deterministic local collector returning ordinary
  MVPA outcomes.

It should not own QR, eigensolvers, Cholesky, LSQR, nonlinear optimizer
implementations, or a second feature-set abstraction.

`fmri-workflow` integration is deliberately deferred. Its ownership boundary is
under separate revision, and the numerical bridge does not need workflow
authority to become correct.

### `multivar`

Generic hard/soft LDA belongs in `multivar`, not in `mvpa-fit`. The existing
operator-core plan already reserves the primitive:

\[
\operatorname{secondOrder}(X,L,X)=X^\top L X.
\]

Once the open multivar LDA work lands, `mvpa-fit` can supply
\(X=A Y\) as an operator-backed table. No private generalized eigenproblem or
duplicate `soft_lda` implementation should appear in the bridge.

## Run and fold semantics

The basic unit is a run-local readout and a study-level shared spatial decoder:

\[
\eta_r=A_r(Y_rW).
\]

The default fold policy is leave-one-run or leave-one-independent-block out.
Each run retains its own temporal design, nuisance projection, whitening, and
readout. Training operators are row-stacked over training runs; the test
operator is built from the held-out run without using its labels.

The Phase 2 API records `ReadoutFitScope.DesignOnly`: all current readout state
follows from the design and fixed parameters. Response-adaptive readouts are not
represented yet, so the type does not advertise fit scopes the implementation
cannot honor. A future response-adaptive extension must add explicit typed
alternatives for training-fold fits and declared run-local unsupervised fits;
the latter is transductive preprocessing and must be reported.

Arbitrary within-run trial folds are not a safe default when HRFs overlap.
They should be rejected unless an explicit `WithinRunOverlapPolicy` records why
the fold is scientifically valid. Event timings for test trials may be used to
construct the test readout; test labels may not influence readout preparation,
feature scaling, nuisance fits, hyperparameter choice, or convergence decisions.

All data-dependent feature centering, scaling, nuisance adjustment, and tuning
are fold-local. Fixed centering/scaling can still be expressed as operator
composition:

\[
X_s=(I-\mathbf 1\mathbf 1^\top/n)XD^{-1}.
\]

The fitted means, scales, and zero-variance policy belong in the fold receipt.

## Estimator sequence

### 1. One-shot ridge decoder: correctness slice

Start with regularized multivariate squared loss over hard or simplex targets.
Use a Gale augmented operator and LSQR rather than normal equations:

\[
\begin{bmatrix}X\\ \sqrt{\lambda}I\end{bmatrix}W
\approx
\begin{bmatrix}C\\0\end{bmatrix},
\qquad X=AY.
\]

This slice proves the readout, adjoint, composition, folds, and result plumbing.
It is not the final probabilistic classifier. Its predictions must match a
dense explicit-\(AY\) reference within tolerance. Fit the intercept through
training-fold centering or an explicit unpenalized column; it must not be folded
into the \(\sqrt{\lambda}I\) penalty.

Implementation status: delivered in Phase 3a. The public
`ClassMembership` contract validates hard one-hot and soft simplex targets;
`OperatorRidgePrediction` deliberately returns class scores rather than
mislabeling them as probabilities. The current execution mode consumes only
forward and transpose operator products.

### 2. One-shot soft multinomial logistic decoder: primary MVP

Implement the cross-entropy objective above with a positive ridge penalty by
default. Use a stable log-sum-exp, an identifiable parameterization (reference
class or sum-to-zero), batched class products, and matrix-free gradients.

Do not add a private optimizer loop to `mvpa`. Add or consume a portable generic
differentiable-optimization capability at the Gale numerical layer, with L-BFGS
as the first-order baseline and Newton-CG only if Hessian-vector products show a
measured benefit.

The public result must include convergence, termination reason, objective and
gradient histories, regularization, iterations, and any line-search failure.
An unregularized separable fit has no finite MLE; `lambda = 0` must either be
rejected for the public estimator or return a typed separation/non-convergence
result rather than a large finite coefficient vector labeled converged.

The updated `~/code/discursive/R/em_soft_lda.R` `soft_lr` path is a useful
small-matrix numerical oracle. Generate committed fixtures from it; do not add a
runtime dependency on Discursive.

### 3. Pulled-back soft LDA: multivar-backed estimator

For trial membership matrix \(C\), let \(L_W(C)\) and \(L_B(C)\) be the within-
and between-class row links used by soft LDA. With \(X=AY\):

\[
S_W=Y^\top A^\top L_W(C)AY,
\qquad
S_B=Y^\top A^\top L_B(C)AY.
\]

This is exactly the multivar `secondOrder` construction over a composed table.
Use the generic multivar shrinkage and LDA policies after their existing tracker
dependencies land. The implementation should be limited initially to ROIs or a
declared low-rank/dual strategy; a dense \(P\)-by-\(P\) covariance is not a
whole-brain algorithm.

`~/code/discursive/R/soft_lda.R` is the small explicit-matrix parity oracle.
Trace-ratio LDA is a later objective over the same scatters, not a separate
readout path.

### 4. Covariance-aware integrated decoder: scientific target

For temporal noise covariance \(\Sigma_{t,r}\), the readout induces correlated
trial-estimation noise:

\[
U_r=A_r\Sigma_{t,r}A_r^\top.
\]

Expose this as a typed `TrialNoiseCovariance` with its assumptions: IID,
whitened, run-specific AR, estimated versus fixed, and any separability claim.
The next estimator should derive its loss from a measurement-error or marginal
time-series model and consume \(U_r\). It should be compared with the ITEM
family of integrated linear decoders.

Do not merely prewhiten \(AY\) and the class labels by \(U^{-1/2}\). That treats
readout uncertainty as ordinary response noise and can change label semantics.
The exact likelihood or estimating equation must be written down first, then
validated against an independent implementation and simulation oracle.

This phase earns the name statistical one-shot. Its gate is better held-out log
loss/calibration or parameter recovery under correlated, short-ISI trial
estimation noise—not simply equality to the beta pipeline.

### 5. Joint latent trial model: research extension

Only after the covariance-aware inverse model is stable, consider:

\[
Y_r=H_rB_r+C_{t,r}\Gamma_r+E_r,
\qquad
B_{ri}\mid z_{ri}=k\sim\mathcal N(\mu_k,\Sigma_B).
\]

Integrate or infer \(B\) rather than plugging in trial estimates. Start with
diagonal, shared-low-rank, or score-space covariance; full voxel covariance and
dense EM are not scalable. The updated Discursive `em_soft_lda` is a useful
small-dimensional oracle for likelihood monotonicity and returned-state
self-consistency, not a whole-brain implementation template.

This model needs explicit diagnostics for covariance jitter, effective rank,
objective monotonicity, backtracking, initialization, termination, and label
evidence. Probabilistic memberships and evidential plausibilities must remain
different target types with different estimator capabilities.

## Execution strategies

The estimand is independent of storage strategy. Make execution explicit:

```scala
enum OneShotExecutionStrategy:
  case Fused
  case CachedReadoutBlocks(maxBytes: CacheBytes)
  case ExplicitDenseReference
  case Auto(policy: OneShotPlanningPolicy)
```

- `Fused` applies \(Y\), \(A\), \(A^\top\), and \(Y^\top\) during optimization
  and never builds a whole-brain \(N\)-by-\(P\) trial table.
- `CachedReadoutBlocks` computes bounded \(AY\) feature blocks and reuses them
  across folds or overlapping searchlights.
- `ExplicitDenseReference` is the parity oracle and a legitimate small-data
  execution path.
- `Auto` may be enabled only after benchmarks establish crossover rules from
  timepoints, trials, features, classes, folds, optimizer iterations, and
  feature-set reuse.

The receipt records the resolved strategy, peak cache size, operator-application
counts, and whether any trial patterns were materialized. Searchlight execution
should batch weight columns or reuse readout blocks where profiling supports it;
it should not recompute the same voxel readout independently for every
overlapping neighborhood by accident.

## Result contract

`OneShotMvpaResult` should expose:

- class order and the exact target semantics;
- fold-specific spatial weights and intercepts;
- held-out trial predictions and metrics;
- optional scan-level score traces;
- readout, temporal-preparation, trial-adjustment, and fold receipts;
- conditional-control baseline metrics when applicable;
- optimization and numerical diagnostics;
- covariance assumptions for uncertainty-aware fits; and
- execution-strategy and allocation receipts.

Weights from different folds are not silently averaged. Any consensus map needs
a named aggregation and sign/class alignment rule. Predictions remain the
primary cross-validated result.

## Validation program

Internal equivalence is necessary but insufficient. Each layer gets a different
oracle.

### Operator and LSS laws

1. `TrialReadout.applyTo(Y)` equals current `LeastSquaresSeparate.fit` for fixed
   nuisance, zero trials, non-estimable trials, and ordinary designs.
2. The adjoint identity
   \(\langle Ax,z\rangle=\langle x,A^\top z\rangle\) holds on random and
   adversarial fixtures.
3. `A(YW)` equals `(AY)W` for vectors and class batches.
4. Run stacking, row restriction, and feature restriction match dense block
   references.
5. Every shared law runs on JVM and Scala.js.

### Decoder oracles

1. Fused ridge coefficients, scores, and predictions match explicit dense
   \(AY\) plus a direct ridge oracle.
2. Soft-logistic objectives and gradients match central finite differences;
   small fixtures match Discursive `soft_lr`.
3. Hard one-hot targets reduce to the ordinary hard-label objective.
4. Permuting trials, classes, runs, or feature columns with the corresponding
   metadata permutation leaves the estimand invariant.
5. Separable unregularized data produce a typed non-finite-MLE outcome.
6. Pulled-back soft-LDA scatters and predictions match explicit
   `soft_lda(A %*% Y, C)` fixtures before any matrix-free optimization is trusted.

### Property and adversarial families

1. Generate valid random run/readout/fold shapes with fixed cross-platform
   seeds and check forward/adjoint, composition, restriction, and permutation
   laws over typical and ill-conditioned regimes.
2. Exercise near-collinear trial designs, rank-deficient trial covariates,
   zero/constant features, extreme but finite scales, severe class imbalance,
   missing classes in a training fold, and non-finite boundary inputs.
3. Assert finite values explicitly at every public success boundary and require
   typed failures for invalid shapes, ranks, memberships, and convergence.
4. Run the same scientific contract against `Fused`, cached-block, and explicit
   dense paths; no optimized path is covered only by a checksum benchmark.
5. Any implementation defect found during development gets a minimal fixture
   recording the triggering input, violated invariant, and corrected outcome.

Floating-point assertions use combined absolute and relative tolerances. The
tolerance is recorded with each fixture and scaled to the conditioning and
expected operation count; one global epsilon is not a numerical contract.

### Nuisance and leakage sentinels

1. Construct a temporal nuisance with no trial-level equivalent and a trial
   nuisance with no temporal equivalent; verify that swapping their roles either
   fails construction or changes the expected estimand.
2. Perturb held-out labels and verify that every fitted weight, scale, nuisance
   coefficient, and hyperparameter is unchanged.
3. When response-adaptive fit scopes are introduced, perturb held-out time
   series and distinguish training-fold fits from declared run-local
   unsupervised behavior.
4. Compare foldwise trial residualization with an explicit train-fit/test-apply
   reference and reject whole-dataset residualization.
5. Verify that a conditional \(Q\)-only signal raises combined accuracy but not
   neural incremental log loss.

### Independent scientific simulations

Generate raw time series from known HRFs, class patterns, trial amplitudes,
temporal nuisance, AR noise, and controllable ISI. Pre-register regimes with:

- nearly orthogonal trials, where the integrated model should agree with the
  conventional pipeline;
- short-ISI overlap, where readout covariance is large;
- temporal nuisance correlated with condition;
- trial covariates correlated with condition but absent from temporal nuisance;
- null neural information with a strong behavioral confound; and
- misspecified HRF/noise models.

Score parameter recovery, held-out log loss, calibration, balanced accuracy,
and false-positive behavior. The statistical one-shot phase advances only if it
improves a predeclared uncertainty-sensitive target without inflating the null.

### Scenario and performance gates

Add one shared scenario returning a single clean `ScenarioResult` for the public
time-series-to-predictions path. Known gaps are declared caveats under the
scenario policy, not loose skipped assertions.

Benchmark at least:

- one medium ROI;
- a large parcel/whole-mask decoder;
- many overlapping searchlights; and
- multiple folds and classes.

Record wall time, peak live bytes, allocation count, operator passes, cache
reuse, and prediction checksum for explicit, fused, and cached strategies. Do
not assume fused is faster. Publish the observed crossover before making `Auto`
the default.

Use a computational-test pyramid:

- every pull request runs contracts, analytic fixtures, critical regressions,
  small metamorphic/property samples, and both JVM/JS suites;
- external R differential fixtures run on pull requests when cheap and on a
  scheduled job otherwise; and
- larger fuzz, ill-conditioned stress, raw-time simulations, and performance
  budgets run on a scheduled lane with versioned seeds and workload metadata.

Performance checks use soft pull-request guardrails and a tighter scheduled
baseline. A timing change cannot change a correctness tolerance, and a faster
path does not advance if its oracle, convergence, or allocation contract fails.

## Delivery sequence

### Phase 0 — contracts and fixtures

- Freeze the equations, target semantics, fold policy, and result receipt.
- Commit Discursive-generated soft-logistic and soft-LDA fixtures.
- Add raw-time simulation fixtures with independent expected values.

Gate: reviewers can identify the estimand and every data-adaptive fit scope
without reading an implementation.

### Phase 1 — `TrialReadout` in `fit`

- Extract the exact LSS forward map and adjoint.
- Route existing LSS fit through it.
- Add law, parity, degeneracy, and allocation tests on JVM and JS.

Gate: no LSS numerical behavior changes, and the current beta path has one
implementation.

### Phase 2 — pattern operator and `mvpa-fit` bridge

- [x] Add dense/operator pattern views.
- [x] Add the new cross-project module and run-local dataset types.
- [x] Compose \(A\) and \(Y\), restrict ROIs, stack runs, and preserve metadata.
- [x] Return existing MVPA result types.

Gate: explicit and composed \(AY\) are identical across folds and feature sets.

### Phase 3 — usable operator one-shot decoder

- [x] Ship centered operator-native ridge for hard and simplex targets.
- [x] Add typed LSQR convergence and operator-application receipts.
- [x] Match an independent dense normal-equation oracle on JVM and JS.
- [ ] Ship soft multinomial logistic regression.
- [ ] Add fold-local scaling and the three nuisance actions.
- [ ] Add explicit/fused/cache strategies and the crossover benchmark.

Gate: complete JVM/JS tests, R fixture parity, leakage sentinels, clean scenario,
and published crossover evidence. Phase 3a satisfies the ridge correctness
gate, not this full release gate.

### Phase 4 — multivar soft LDA

- Depend on the existing multivar operator-core and LDA tracker items.
- Express soft scatters only through `secondOrder`.
- Add explicit R parity and low-rank/ROI scope limits.

Gate: no private eigensolver or dense whole-brain covariance path.

### Phase 5 — uncertainty-aware statistical one-shot

- Add temporal covariance propagation and assumptions.
- Derive and implement the integrated objective.
- Add ITEM/differential parity where possible and independent simulations.

Gate: a predeclared scientific benefit under readout uncertainty, calibrated
null behavior, and no claim based only on internal equivalence.

### Phase 6 — latent/evidential research

- Add score-space or low-rank latent trial models only after Phase 5.
- Treat evidential plausibilities as a separate capability.
- Use Discursive EM as a small-matrix oracle with full numerical diagnostics.

Gate: demonstrable value beyond the covariance-aware inverse model at a bounded
and documented computational cost.

## First three implementation slices

1. **LSS `TrialReadout` plus adjoint** — the reusable seam on which every later
   estimator depends.
2. **Composed `PatternOperator` plus ridge parity** — proves the end-to-end
   time-series-to-prediction path without introducing nonlinear optimization.
3. **Fold/nuisance contract plus soft logistic** — makes the path scientifically
   usable and anchors it to the updated Discursive oracle.

Do not begin the latent EM model first. It combines the hardest statistical,
optimization, and scaling questions before the operator, fold, and nuisance
contracts have independent evidence.

## Candidate audit

The following 30 candidates were considered before selecting the architecture.
"Select" means part of the staged target; "baseline" means retained as an
oracle or execution strategy, not presented as the destination.

| # | Candidate | Value | Effort/risk | Decision |
| ---: | --- | --- | --- | --- |
| 1 | Existing LSS betas followed by existing MVPA | Medium | Low | Baseline oracle |
| 2 | Lazy LSS beta matrix view | Medium | Low | Keep as bounded-cache strategy |
| 3 | Gale-backed `TrialReadout` composed with time data | High | Medium | Select foundation |
| 4 | Decode scan scores, then apply the trial readout | High | Medium | Select core estimand |
| 5 | Direct scan classifier on HRF-convolved hard labels | Low | Medium | Reject label mismatch |
| 6 | Treat HRF amplitudes as soft class memberships | Low | Medium | Reject probability mismatch |
| 7 | Matrix-free ridge decoder over `A(YW)` | High | Low/medium | Select correctness slice |
| 8 | Matrix-free soft multinomial logistic decoder | High | Medium | Select primary MVP |
| 9 | One-versus-rest logistic decoder | Medium | Medium | Defer unless multinomial blocks |
| 10 | Pulled-back hard LDA | High | Medium | Select through generic multivar LDA |
| 11 | Pulled-back soft LDA | High | Medium/high | Select after operator core |
| 12 | Trace-ratio soft LDA | Medium | High | Defer as alternate objective |
| 13 | Existing ridge LDA on cached trial blocks | Medium | Low | Baseline/cached execution |
| 14 | Direct class-template encoding GLM in raw time | Medium | Medium | Research comparator |
| 15 | Time-domain PLS or CCA against the design | Medium | High | Reject for first estimand |
| 16 | Kernelized trial-readout decoder | Medium | High | Defer until linear evidence |
| 17 | Generic end-to-end temporal neural network | Low | Very high | Reject from core plan |
| 18 | HRF-constrained temporal neural network | Medium | Very high | Defer research only |
| 19 | State-space latent trial decoder | Medium | High | Defer after covariance model |
| 20 | Joint Gaussian latent-beta LDA/EM | High | Very high | Select late research phase |
| 21 | Variational Bayesian latent-beta classifier | Medium | Very high | Defer after latent likelihood |
| 22 | Bayesian probit/logit with beta marginalization | Medium | Very high | Defer after inverse model |
| 23 | ITEM-style covariance-aware inverse decoder | High | High | Select scientific target |
| 24 | Naively whiten trial patterns and labels by readout covariance | Low | Medium | Reject without derivation |
| 25 | Foldwise trial-feature residualization | High | Medium | Select nuisance action |
| 26 | Conditional outcome model with trial covariates | High | Medium | Select as distinct action |
| 27 | Adversarial nuisance-invariant decoder | Medium | High | Defer until baselines |
| 28 | Double/debiased machine-learning residualization | Medium | High | Defer until estimand demands it |
| 29 | Run-specific readouts with shared spatial weights | High | Medium | Select default CV structure |
| 30 | Hierarchical subject/run shared decoder | Medium | Very high | Defer to multi-subject extension |

## Open questions to settle at the named gates

- Which covariance-aware likelihood most faithfully recovers ITEM semantics
  while supporting categorical and probabilistic trial targets? Settle this in
  Phase 5 from equations and differential fixtures, not during Phase 1 API work.
- Which data-adaptive temporal preparations can honestly be frozen as a
  conditional linear readout, and which need feature-block-specific operators?
  Settle this before adding anything beyond design-only LSS and fixed shared
  whitening.
- Should the portable nonlinear optimizer land directly in Gale core or in a
  small Gale optimization module? Settle ownership before soft logistic code;
  the `mvpa` module must not become its accidental home.
- At what feature-set reuse does a bounded \(AY\) cache beat fused application?
  Settle the default only from the Phase 3 crossover benchmark.
- Which existing event/dataset identities should form `TrialCoefficientAxis`
  without duplicating `EventId`, `RunId`, and MVPA sample metadata? Settle this
  during Phase 0 as a type-boundary review.

## Dependencies and non-goals

Existing tracker dependencies to respect:

- `bd-01KXSGZ2A6F9DA2HG7TB7CT0A4`: unified multivar operator core;
- `bd-01KXSGZ3E48W9X80199PS5FHA8`: multivar LDA via `secondOrder`;
- `bd-01KXYJN7C4DNFB4QGY96S5W6HF`: Gale sparse/operator migration; and
- `bd-01KXYJNQ1TKES0D4GPVP42F8HW`: Gale fit/factorization migration.

Non-goals for the first usable release:

- a direct port of Discursive R objects or list-shaped configuration;
- full voxel covariance or dense whole-brain LDA;
- within-run random trial CV as a default;
- automatic temporal-to-trial confound conversion;
- deep learning, joint multi-subject fitting, or spatial regularization;
- workflow/scheduler ownership; and
- claiming statistical gains from an algebraically equivalent fused path.

## Literature anchors

- Loula, Varoquaux, and Thirion,
  [Decoding fMRI activity in the time domain improves classification performance](https://pubmed.ncbi.nlm.nih.gov/28801250/),
  motivates decode-then-deconvolve.
- Soch and colleagues,
  [Inverse transformed encoding models](https://openaccess.city.ac.uk/id/eprint/23430/),
  motivate integrated linear decoding with design-induced trial covariance.
- Abdulrahman and Henson,
  [Effect of trial-to-trial variability on optimal event-related fMRI design](https://pmc.ncbi.nlm.nih.gov/articles/PMC4692520/),
  provide the LSA/LSS trial-estimation context.

The literature motivates the model classes; the acceptance evidence remains the
ScalaFIM laws, external fixtures, raw-time simulations, and JVM/Scala.js results
specified above.
