# Unified Pattern-First MVPA: Design Notes

Status: **intake notes, Part 1; not yet an implementation plan**

Companion: [Part 2 architecture notes](unified-mvpa-architecture-notes.md)

Synthesis: [Unified analysis PRD sketch](unified-mvpa-prd.md). It proposes the
product requirements and acceptance gates; this file preserves the detailed
scientific intake and derivations.

Recorded: 2026-09-12

These notes capture the proposed scientific method and its intended contracts.
They intentionally preserve assumptions, inferential meanings, limitations, and
claims requiring validation. The companion Part 2 record captures the proposed
replacement architectural waist. The PRD reconciles the note set into a proposed
delivery scope; implementation planning remains a separate next step.

## Governing objective

Develop one whole-brain method that supports:

- ordinary condition classification;
- multivariate prediction of continuous or multiresponse targets, including
  encoding-decoding analyses in which each image has an associated feature
  vector;
- sparse, spatially coherent localization without requiring searchlights;
- interpretable task-related spatial patterns with a valid Haufe
  interpretation;
- maximally interpretable components, allowing rotations and not requiring the
  final explanatory axes to remain orthogonal;
- voxel-, component-, and rank-level inference;
- group analysis of subject-level solutions;
- whole-brain predictive performance and optional local or sub-ROI predictive
  performance from a single fitted model; and
- enough computational efficiency to make discovery and conditional
  confirmation practical at whole-brain scale.

The proposed method is a **pattern-first, spatially structured reduced-rank
model**. It should learn a small task-related spatial representation first and
derive prediction, interpretation, localization, and inference from that one
relationship. It should not be assembled as an unrelated sequence of sparse
classification, post-hoc Haufe transformation, and rotation.

The intended contribution is stronger than “a faster searchlight.” It is a
unified estimand and computational architecture that could make searchlights
unnecessary for a broad class of questions. Superiority in speed, localization,
or prediction remains an empirical claim to establish by benchmarking.

## Scientific distinction that organizes the method

Task-related signal expression and decoding utility are different quantities.

For the illustrative system

\[
x_1=s+n, \qquad x_2=n,
\]

the decoder \(x_1-x_2\) recovers \(s\). Its backward weights are
\((1,-1)\), whereas the task-linked signal pattern is \((1,0)\). The second
measurement is useful because it cancels noise, not because it expresses the
signal.

Therefore:

- sparsifying or smoothing decoder weights may regularize the wrong object for
  anatomical interpretation;
- spatial structure should primarily constrain the forward task-pattern map;
  and
- a covariance-aware decoder may legitimately use voxels outside the forward
  pattern support for noise cancellation.

The method must expose at least two maps rather than collapsing them into a
single ambiguous “importance” image:

| Output | Scientific question | Interpretation |
|---|---|---|
| Task-pattern map | Where is a fitted task-related dimension expressed? | A forward, task-associated signal pattern |
| Conditional predictive-importance map | How much predictive information is lost if this measurement is withheld while all others remain? | Covariance-aware predictive utility, including suppressor/noise-cancelling variables |

Neither output is automatically causal.

## Core joint model

For observation \(i\), let:

- \(x_i\in\mathbb R^p\): the brain image;
- \(y_i\in\mathbb R^q\): a target vector, such as condition coding or stimulus
  features;
- \(r\): the predictive rank;
- \(A\in\mathbb R^{p\times r}\): spatial brain patterns;
- \(C\in\mathbb R^{q\times r}\): target-side/task dimensions; and
- \(\Psi\succ0\): residual brain covariance.

Use the forward observation model

\[
\boxed{
x_i=A t_i+\epsilon_i,
\qquad
t_i=C^\top y_i,
\qquad
\epsilon_i\sim N(0,\Psi).
}
\]

Equivalently,

\[
\mathbb E[x_i\mid y_i]=Fy_i,
\qquad
F=AC^\top,
\qquad
\operatorname{rank}(F)\le r.
\]

This is a structured reduced-rank forward model. Low rank, sparsity, spatial
regularization, and rotation each have precedents; the proposed contribution is
their organization around a common pattern-first model and its downstream
estimands.

The linear-Gaussian formulation is a working model, not a claim that neural
coding is fully Gaussian. Model adequacy and predictive performance must be
checked out of sample. Under misspecification, an optimum for forward
likelihood need not minimize decoding error. Rank, spatial penalties, and
covariance regularization should therefore be selected using held-out decoding
performance rather than reconstruction quality alone.

### Whole-brain sufficient representation

Define

\[
u=A^\top\Psi^{-1}x,
\qquad
G=A^\top\Psi^{-1}A.
\]

Expanding the Gaussian likelihood gives

\[
\boxed{
\log p(y\mid x)
=
\log p(y)
+t(y)^\top u
-\frac12t(y)^\top Gt(y)
+\operatorname{constant}(x).
}
\]

The fitted model therefore reduces the entire brain image to the \(r\)-vector
\(u\) for prediction. Target coding and the target prior determine which
prediction problem is solved from that representation.

### Classification

For class \(c\), let \(m_c=C^\top y_c\). Then

\[
p(c\mid x)
=
\operatorname{softmax}_c
\left[
m_c^\top u
-\frac12m_c^\top Gm_c
+\log\pi_c
\right].
\]

Ordinary condition classification is thus one prediction head over the common
model rather than a separate estimator family.

### Continuous or multiresponse target prediction

With the working target prior

\[
y\sim N(0,\Sigma_y),
\qquad
\Phi=C^\top\Sigma_y C,
\]

the conditional mean is

\[
\boxed{
\hat y
=
\Sigma_y C\,(I+G\Phi)^{-1}u.
}
\]

The only inverse is \(r\times r\), regardless of voxel or target dimension.
Forward prediction is also immediate:

\[
\hat x=AC^\top y.
\]

Encoding and decoding are therefore conditional views of one fitted
relationship, not independently learned systems with unrelated components.

## Spatial structure belongs to the signal model

For data matrices \(X\in\mathbb R^{n\times p}\) and
\(Y\in\mathbb R^{n\times q}\), a basic estimator for fixed regularized
\(\Psi\) is

\[
\begin{aligned}
\min_{A,C:\,C^\top C=I}\quad
&
\frac{1}{2n}
\left\|
(X-YCA^\top)\Psi^{-1/2}
\right\|_F^2
\\
&+
\lambda_s\sum_v\|A_{v:}\|_2
\\
&+
\lambda_{\mathrm{TV}}
\sum_{(v,w)\in E}
\omega_{vw}\|A_{v:}-A_{w:}\|_2
\\
&+
\frac{\lambda_2}{2}\|A\|_F^2.
\end{aligned}
\]

Interpretation of the terms:

- the row-group penalty selects voxels jointly across components;
- the graph penalty encourages spatial coherence without defining searchlight
  centers or fixed parcels;
- the ridge term stabilizes estimation; and
- \(C^\top C=I\) fixes a computational scaling ambiguity but does not imply
  that the task scores \(YC\) are uncorrelated or that latent brain signals are
  independent.

The anatomical graph must respect the measurement domain. Cortex should use
surface adjacency; subcortex may use appropriate volumetric adjacency. Nearby
points across sulci must not be connected merely because their Euclidean
coordinates are close.

Target geometry must also be explicit. Feature blocks with many coordinates
must not dominate only because of dimensionality. The method should accept a
target metric or block weights and preserve their training provenance.

The rank-constrained fit is nonconvex. Expected implementation requirements
include supervised spectral initialization, warm starts, multiple or
diagnostic starts where justified, objective/convergence receipts, and no
general claim of a globally optimal fit.

### Separate smooth support from smooth signed loadings

Strong smoothing of signed coefficients could erase the fine-grained sign
structure that motivates MVPA. “The signal occupies a coherent anatomical
territory” is not the same assumption as “neighboring voxels have similar
signed loadings.”

Introduce a nonnegative spatial envelope \(g_v\) satisfying

\[
\|A_{v:}\|_2\le g_v,
\]

and penalize

\[
\lambda_s\sum_v g_v
+
\lambda_g\sum_{(v,w)\in E}|g_v-g_w|.
\]

A separate, optionally weak penalty may control smoothness of \(A\) itself.
This allows a coherent selected territory to contain positive and negative
fine-scale loadings. Conditional on the task factors, these envelope
constraints and spatial penalties remain convex.

Provisional default: encourage smooth anatomical support and tune signed-pattern
smoothness separately. Spatial coherence must not silently make fine-scale
heterogeneous codes undiscoverable.

## Haufe-valid pattern interpretation

Define calibrated brain scores

\[
z=W^\top x,
\qquad
W=\Psi^{-1}A G^{-1},
\qquad
G=A^\top\Psi^{-1}A.
\]

Assuming \(A\) has full column rank,

\[
W^\top A=I,
\qquad
z=t+W^\top\epsilon.
\]

If \(\Phi=\operatorname{Cov}(t)\), then

\[
\operatorname{Cov}(z)=\Phi+G^{-1}
\]

and

\[
\operatorname{Cov}(x,z)=A(\Phi+G^{-1}).
\]

Consequently, the Haufe forward pattern of these calibrated scores is

\[
\boxed{
A_{\mathrm H}
=
\operatorname{Cov}(x,z)
\operatorname{Cov}(z)^{-1}
=A.
}
\]

Thus the spatial patterns being regularized are exactly the model-implied
Haufe patterns of the calibrated component scores. This identity depends on
the stated covariance model and full-rank conditions, although this covariance
identity itself does not require Gaussianity.

Required qualifications:

- calibrated component scores are not identical to posterior-shrunken target
  predictions;
- Haufe-transforming final predicted features may produce different scaling or
  mixing;
- every returned pattern must identify the score or coordinate system it
  describes;
- an estimated covariance model is not the true covariance; and
- the result should include both the structured estimate \(A\) and an empirical
  Haufe diagnostic computed on independent data.

The held-out empirical diagnostic should not be forced to share the sparse
appearance of \(A\). Disagreement is evidence about finite-sample behavior or
model adequacy and should remain visible.

## Components and rotation

Interpretability should come from rotating the fitted relationship rather than
decorating singular vectors after the fact.

For

\[
F=USV^\top,
\]

allow separate rotations on the brain and target sides:

\[
\boxed{
F
=
\underbrace{UR_b}_{\text{brain factors}}
\underbrace{R_b^\top S R_t}_{\text{relationship matrix}}
\underbrace{(VR_t)^\top}_{\text{task factors}}.
}
\]

The middle relationship matrix need not be diagonal. This permits several
interpretable spatial factors and several interpretable target factors without
forcing a scientifically artificial one-to-one correspondence.

The motivating reference is Karl Rohe and Muzhe Zeng, “Vintage factor analysis
with Varimax performs statistical inference,” *Journal of the Royal Statistical
Society Series B* 85 (2023), 1037–1060,
<https://doi.org/10.1093/jrsssb/qkad029>. Its relevance is the statistical
identification of Varimax axes under particular factor-model and leptokurtic
conditions, including a non-diagonal relation between two factor sides. It does
not establish that an arbitrary rotated neuroimaging decomposition recovers
neural sources.

Varimax is itself orthogonal, but prediction-preserving oblique coordinates are
also possible. For invertible \(R\),

\[
T^\star=TR,
\qquad
A^\star=AR^{-\top}
\]

leaves \(TA^\top\) unchanged when all associated quantities are transformed
consistently.

For an oblique result, require explicit scale conventions, condition-number
limits, and a reported factor-correlation matrix. Component interpretation also
requires stability analysis across repeated training-block fits. If only the
subspace is stable, the method must report subspace stability rather than imply
that individual axes are reproducible.

Comparisons and maps should be coordinate-invariant unless a coordinate system
has been explicitly frozen. Arbitrary loading columns should not be compared
entrywise across fits.

### Predictive rank is not anatomical component count

The predictive rank is bounded by target rank:

\[
r\le q.
\]

For \(K\)-class linear mean discrimination,

\[
r\le K-1.
\]

A binary contrast or one continuous outcome therefore identifies at most one
linear predictive dimension even if its spatial pattern occupies many
disconnected regions. Extra anatomical atoms may be introduced through a
spatial dictionary, shared multi-task structure, or a model of additional brain
variation, but they must not be presented as independently identified
predictive dimensions of that single target.

The API and reports must distinguish:

- predictive rank;
- subdivisions of spatial support; and
- latent-source hypotheses.

## Local prediction from a global fit

For ROI \(R\), restrict the measurement model:

\[
x_R,\qquad A_R,\qquad\Psi_{RR}.
\]

Then compute

\[
u_R=A_R^\top\Psi_{RR}^{-1}x_R,
\qquad
G_R=A_R^\top\Psi_{RR}^{-1}A_R.
\]

Use the same classification or continuous-target equations with \(u_R,G_R\)
in place of \(u,G\). In particular,

\[
\boxed{
\hat y_R
=
\Sigma_y C\,(I+G_R\Phi)^{-1}u_R.
}
\]

Under the fitted model, this is conditional prediction using only ROI
measurements. It is not equivalent to cropping or zeroing the whole-brain
decoder because, in general,

\[
(\Psi^{-1})_{RR}\ne(\Psi_{RR})^{-1}.
\]

Locality contract:

- fit one whole-brain signal model;
- restrict the residual covariance and recompute regional precision;
- make no new high-dimensional spatial fit for an ordinary regional query; and
- evaluate local accuracy, log loss, or \(R^2\) on held-out observations.

Interpretive limits:

- a regional predictor inherits the learned whole-brain task subspace and may
  miss a predictive dimension that an independently fitted local model could
  discover;
- its performance means “information accessible locally under this shared
  representation,” not the maximum information extractable by any local
  method;
- strict test-time locality forbids preprocessing that imports measurements
  from outside the ROI, including cross-boundary spatial smoothing or global
  reconstruction; and
- selecting a favorable ROI after inspecting test performance requires fresh
  evaluation or an explicit multiplicity correction.

### Conditional predictive information without voxelwise refitting

For Gaussian targets, let

\[
P=\Psi^{-1},
\qquad
K=(\Phi^{-1}+G)^{-1},
\]

and define

\[
h_v=\frac{(PA)_{v:}^\top}{\sqrt{P_{vv}}}.
\]

A Schur-complement identity yields

\[
G_{-v}=G-h_vh_v^\top.
\]

The information lost by withholding voxel \(v\), while retaining all others,
is

\[
\boxed{
I(y;x_v\mid x_{-v})
=
-\frac12\log\left(1-h_v^\top K h_v\right).
}
\]

This result is in nats and is a Gaussian-model-derived conditional-importance
measure, not an assumption-free empirical estimator. Once \(PA\) and the small
matrix \(K\) are available, the whole map requires only small quadratic forms;
it does not require a separate leave-one-voxel-out fit.

The conditional-information map and \(A\) answer different questions and may
differ substantially. In particular, a zero-pattern-loading suppressor voxel
may have positive conditional information.

## Inference architecture

Fast inference depends on stating the inferential contract explicitly.

Default separation:

1. Discovery data learn preprocessing, target geometry, spatial support,
   covariance structure, predictive rank, penalties, and rotations.
2. Independent confirmation data test a frozen representation.

This enables resampling or closed-form work in low-dimensional projected
problems instead of rerunning the whole spatial optimization for each test.

Inference conditional on a frozen selected representation is not the same as
inference that propagates uncertainty from every model-selection stage. The
latter should be offered, if at all, as a distinct and more expensive contract.

### Voxel-level inference

Freeze the target transformation learned in discovery. On confirmation data,
use target-derived scores

\[
T_{\mathrm{confirm}}=Y_{\mathrm{confirm}}C
\]

or their consistently rotated coordinates, and fit

\[
X_{\mathrm{confirm}}
=
T_{\mathrm{confirm}}A_{\mathrm{confirm}}^\top+E.
\]

Ignoring nuisance columns in the displayed equation,

\[
\boxed{
\hat A_{\mathrm{confirm}}
=
X_{\mathrm{confirm}}^\top T_{\mathrm{confirm}}
(T_{\mathrm{confirm}}^\top T_{\mathrm{confirm}})^{-1}.
}
\]

This supports:

- a component-specific voxel hypothesis \(H_0:A_{vk}=0\); and
- an omnibus voxel hypothesis \(H_0:A_{v:}=0\).

These test task-associated forward projections. They do not test that a voxel
is uniquely necessary for decoding.

The confirmation score must be constructed from held-out targets, not from
\(X_{\mathrm{confirm}}W\). Regressing a brain image on a score constructed from
that same image can generate associations under a target-null model. An
empirical Haufe map by itself is therefore not a test of task information.

Confirmation maps should be unpenalized, or explicitly labeled as arising from
a different inferential model. Sparse selection frequencies and thresholded
penalized loadings are not voxelwise p-values.

Error and multiplicity handling must follow the actual design:

- ordinary multivariate-design t/F calculations may apply to independent
  Gaussian observations;
- temporally dependent fMRI, repeated measures, runs, and subjects require the
  corresponding covariance model, cluster-robust inference, or design-valid
  resampling;
- nuisance handling and permutation exchangeability must be explicit; and
- max-statistic resampling or a declared error-rate procedure should cover the
  intended voxel-by-component family.

### Component-level inference

Provide two distinct tests:

1. **Association:** does a frozen brain score generalize in its relationship to
   the corresponding target dimension?
2. **Incremental prediction:** does a model using a component outperform a
   reduced model without it?

For incremental prediction, evaluate held-out loss difference

\[
\Delta_k
=
\mathbb E\left[
\ell(y,\hat y_{-k})-\ell(y,\hat y)
\right].
\]

The small reduced prediction head should be refit, especially with correlated
factors. Otherwise the estimand is the effect of a particular parameter
ablation rather than remaining predictive value after adjustment.

The inferential units are trials, blocks, runs, or subjects as justified by the
design. Overlapping cross-validation folds do not create independent units.

### Rank inference

Validation-based rank selection and statistical rank testing are separate.

For rank testing, freeze a modest maximum-dimensional brain and target
projection in discovery data. On confirmation data, test sequential rank or
canonical association in the resulting small cross-covariance problem.

The supported conclusion is:

> There are at least \(k\) detectable dimensions of linear association within
> the independently learned candidate subspaces.

Rejecting a projected rank-\(k\) null is evidence against full-relationship rank
\(k\). Failure to reject is not a global upper-bound proof.

Higher-rank null resampling must preserve the lower-rank relationship. A naive
permutation that destroys all association is generally not a valid test for
every ordered dimension beyond the first.

## Group analysis

Support both separately fitted subject models and a partially pooled model.
Common target coordinates must be established before spatial alignment.
Aligning subjects solely by a rotation chosen to maximize spatial similarity
can manufacture apparent homogeneity.

An interpretable hierarchy is

\[
A_s=M_sA_0+\Delta_s,
\]

where \(M_s\) maps a common anatomical representation into subject \(s\)'s
space and \(\Delta_s\) represents individual departures. Each subject may use
its native spatial graph.

Group outputs should include:

- the population mean task pattern;
- between-subject variability and heterogeneity;
- subject-level component expression;
- predictive performance; and
- the coordinate/subspace transport and uncertainty provenance used to make
  subjects comparable.

A shared target basis should come from an independent discovery sample or an
appropriately separated training stage. Subject-specific confirmation loadings
and their uncertainties can then enter a mixed-effects analysis.

If predictive subspaces are stable but component axes are not, compare
subspaces or task-linked forward operators rather than force one-to-one
component matches. Group inference must also distinguish “some subjects carry
information” from a prevalence claim about the population; a conventional
second-level test of an information measure does not automatically establish
population prevalence.

## Computational design

The implementation must avoid dense \(p\times p\) covariance and dense
\(p\times q\) coefficient matrices.

Store the factors \(A,C\) and begin with a structured residual covariance

\[
\Psi=D+UU^\top,
\]

where \(D\) is positive diagonal and \(U\) has modest width \(h\). Use
Woodbury:

\[
\Psi^{-1}
=
D^{-1}
-
D^{-1}U
(I+U^\top D^{-1}U)^{-1}
U^\top D^{-1}.
\]

Precision applications then require diagonal operations and low-rank products.
The representation restricts naturally to an ROI:

\[
\Psi_{RR}=D_R+U_RU_R^\top.
\]

Residual covariance must be estimated from unexplained variation rather than
indiscriminately from total covariance. Covariance estimation cost and
sensitivity to covariance-model choice belong in validation and benchmarks.

Expected fitting machinery:

- supervised spectral initialization;
- matrix-free products such as \(X^\top(YC)\);
- proximal spatial updates;
- warm-started rank and regularization paths; and
- cached additive spatial/noise summaries for interactive ROI queries.

The main dense passes should scale approximately as

\[
O\{n(p+q)r\},
\]

plus graph operations and low-rank covariance work. This is a design target,
not yet a measured performance result.

All learned preprocessing must remain inside training splits, including
scaling, nuisance models, response compression, covariance estimates, target
metrics, and spatial tuning. For naturalistic data, temporal alignment,
hemodynamic modeling, and train-test buffers are part of the split contract.

“Lightning fast” is most defensible for confirmation inference and local
queries after the global fit: both should operate on cached quantities and
small projected systems. End-to-end speed, including covariance estimation and
model selection, must be benchmarked.

## Searchlight replacement boundary

The method could replace searchlight mapping as the default workflow for the
question:

> Which spatially organized dimensions of brain activity relate to these
> conditions or features, how well do they predict, and where can their
> information be accessed?

It does not make every searchlight estimand identical to a global low-rank
estimand. A searchlight fits local models without requiring local useful
dimensions to survive a shared whole-brain rank constraint. Regional
predictions from this method inherit that constraint.

The basic observation model captures task-related conditional-mean changes. It
does not automatically detect arbitrary nonlinear codes or covariance-only
codes. Extensions may change the meaning of native-voxel patterns and require a
new interpretation contract.

Searchlights therefore remain:

- an important benchmark;
- a possible sensitivity analysis for locally unique dimensions; and
- a distinct method for scientific questions not preserved by the global
  representation.

## Required validation program

Prediction accuracy alone is insufficient. Known-truth simulations should vary
at least:

- pure signal voxels;
- noise suppressors with zero forward loading;
- redundant informative regions;
- diffuse task-related signal;
- fine-scale sign changes inside coherent support;
- misleading Euclidean proximity across cortical folds;
- residual-covariance misspecification;
- rank misspecification and locally unique dimensions;
- correlated and non-Gaussian target factors;
- component-axis instability with stable subspaces; and
- subject-specific spatial variation and alignment error.

Evaluate separately:

| Domain | Required evidence |
|---|---|
| Prediction | Held-out accuracy, calibrated loss, and/or multiresponse \(R^2\) |
| Pattern recovery | Recovery of forward pattern/operator and spatial support under known truth |
| Importance | Recovery of suppressor-variable utility without mislabeling it signal expression |
| Local access | Restricted-covariance regional prediction versus independently fitted local models and cropped-weight anti-baseline |
| Components | Rotation invariance of predictions; axis and subspace stability; interpretability diagnostics |
| Voxel inference | Null calibration, power, multiplicity, temporal/repeated-measures validity |
| Component inference | Association and refitted incremental-prediction calibration |
| Rank inference | Sequential-null calibration, especially beyond the first dimension |
| Group inference | Population-mean and heterogeneity calibration; sensitivity to mappings and unstable axes |
| Computation | Fit, tuning, covariance, confirmation, and interactive ROI costs measured separately |

Speed comparisons must include strong whole-brain low-dimensional baselines,
especially thresholded PLS, as well as sparse/spatial reduced-rank methods and
searchlights. Comparisons must align target, cross-validation, preprocessing,
locality, and inference estimands rather than compare unlike tasks.

## Provisional product contract

The eventual user-facing analysis should make the following outputs available
without conflating them:

1. fitted forward task patterns and their support envelope;
2. target factors, brain factors, and the possibly non-diagonal relationship
   between them;
3. calibrated brain scores and target predictions;
4. held-out whole-brain prediction metrics;
5. restricted-covariance local/ROI predictions and held-out metrics;
6. model-derived conditional predictive-information maps;
7. independent empirical Haufe diagnostics;
8. rotation, axis-stability, and subspace-stability diagnostics;
9. discovery and selection receipts;
10. conditional confirmation results for voxels, components, and rank; and
11. subject- and group-level pattern, uncertainty, heterogeneity, and predictive
    summaries.

Every output should state its estimand, coordinate system, fitted-data scope,
assumptions, and whether it is descriptive, predictive, model-derived, or an
independent confirmation result.

## Non-negotiable conceptual guardrails

- Do not interpret backward decoder weights as task-expression maps.
- Do not crop a whole-brain precision-weighted decoder to claim regional
  prediction.
- Do not force empirical Haufe diagnostics to match sparse model patterns.
- Do not treat selection frequency or penalized loadings as voxelwise p-values.
- Do not construct confirmation targets from the confirmation brain data.
- Do not count overlapping cross-validation folds as independent inferential
  units.
- Do not test higher ordered ranks against a null that erases all lower-rank
  association.
- Do not equate predictive rank, spatial regions, and neural-source count.
- Do not compare arbitrary component columns across fits when only the subspace
  is identified.
- Do not claim searchlight replacement, localization superiority, or end-to-end
  speed before matched benchmarks establish it.

## Decisions deliberately deferred until all intake parts arrive

- public method and artifact names;
- exact target-coding and target-metric API;
- categorical-prior and continuous-prior families;
- covariance families beyond diagonal plus low rank;
- whether the support envelope is mandatory or optional in the first method;
- rotation objectives and the admission criteria for oblique rotations;
- discovery/confirmation split strategies for low-subject or low-trial data;
- precise nuisance and exchangeability contracts;
- group coordinate transport and partial-pooling implementation;
- baseline suite, datasets, numerical oracles, and acceptance thresholds;
- placement relative to the current ScalaFIM `mvpa`, `mvpa-fit`, spatial,
  dataset, group, and Multivar/Gale boundaries; and
- delivery phases and which pieces constitute a scientifically honest minimum
  viable method.

## Sources and provenance

- Primary source: user-supplied Part 1 design statement, received 2026-09-12.
- Rotation reference: Rohe, K. and Zeng, M. (2023), “Vintage factor analysis
  with Varimax performs statistical inference,” *JRSS B* 85:1037–1060,
  <https://doi.org/10.1093/jrsssb/qkad029>.
- Related prior project work, to reconcile only after intake is complete:
  covariance-restricted regional prediction, invariant maps and rotations,
  held-out Haufe diagnostics, frozen-basis confirmation, and covariance-aware
  group inference in the rMVPA pattern-model work.

No implementation, benchmark, or inferential result is claimed by this note.
