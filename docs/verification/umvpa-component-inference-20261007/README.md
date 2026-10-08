# UMVPA M4.06 component inference procedures

Mote: `bd-01M2BNHDZT6TN086GDNEA6VK4K`. This packet completes the implemented component procedures under their explicit reference contracts. Corrected-family integration (M4.08), frozen scientific release qualification (M4.09), and released C1 claims remain unavailable. The [frozen calibration protocol](../../plans/unified-mvpa-inference-calibration-protocol.md) separates these stages in §11.

## Implemented association reference

The representation, component coordinates, nuisance design and complete named family are discovery-frozen. `FrozenComponentAssociationReference` additionally binds the actual confirmation source identities, a separately declared spherical joint-Gaussian independent-row law, untouched confirmation exposure, seed, fixed draw count and budgets before execution. A known voxel or response covariance alone cannot license this score law.

Association retains the ordinary signed nuisance-adjusted correlations. Pinned Multivar constructs the Huh–Jhun nuisance-residual coordinates; each corresponding brain/target pair receives the upstream rank-one permutation calculation. All pairs use the same residual-row action, seed and accepted candidate IDs. Distinct non-identity transformations, inclusive extremeness, actual completed counts and plus-one p-values are retained in provider receipts. No source projection is repeated for each member, and no raw time-row shuffling is admitted. Dependent association and association intervals return typed unavailability. A failed member refuses the complete calculation.

The supplied prepared scores, residual basis and sampled IDs allow independent reconstruction of the candidate calculations. Provider rank-one `adjustedPValues` concern that single pair's rank sequence; they are not corrections for the complete association-plus-prediction family.

Prepared scores contain the `r` brain columns followed by the `r` target columns. Their rows are Gaussian residual-basis coordinates, not original confirmation sample IDs.

## Implemented incremental prediction reference

The full and every reduced head are separately fitted on authorized training data. Confirmation uses those frozen heads, the original target metric and equal rows within each actual unit, followed by equal unit weights.

`FrozenComponentPredictionReference` binds a known full covariance of row-major `vec(Y)` conditional on discovery, all heads and the actual confirmation predictors. It checks exact source/axis order, symmetry, positive definiteness, and zero covariance across declared independent units. It owns a covariance copy. Cross-target and within-unit terms are retained. Estimated or unknown covariance, marginal-outcome conditioning, mismatched sources and contaminated exposure refuse before outcome projections.

For full prediction `f`, reduced prediction `r`, target metric `M` and row weight `w_i=1/(U n_unit)`, the observed difference is computed stably as

`sum_i w_i (f_i-r_i)' M [(Y_i-r_i)+(Y_i-f_i)]`.

The separate full/reduced squared-loss diagnostics remain available. Their direct subtraction can lose a small effect under a large shared offset; the independent `f=1, r=0, Y=1e8` control establishes the corrected value `199999999` rather than `200000000`.

Each component's affine coefficient is `A[(i,j),k]=2 w_i M[j] (f[i,j]-r_k[i,j])`. The complete mean-effect covariance is `A' Gamma A`, including cross-component covariance. Known covariance of the outcome does not require identically distributed unit loss differences.

The fixed domain policy tests `H0: conditional mean improvement <= 0` against a positive improvement at nominal 5%, and supplies a two-sided 95% interval. R-qualified critical constants are `1.6448536269514722` and `1.959963984540054`; these are fixed policy literals, with no private CDF or quantile implementation. [R's Normal reference](https://www.stat.math.ethz.ch/R-manual/R-patched/RHOME/library/stats/html/Normal.html) documents `qnorm`. Zero variance and nonfinite calculations refuse. Prediction p-values and unsupported significance/coverage levels are unavailable.

## Family, exposure and resource boundaries

A complete candidate bundle retains all `r` association and `r` incremental members in the original order, bound to the same plan and confirmation sources. It provides no corrected-family result or released C1 artifact. Candidate point calculations cannot be converted into an admitted inference result through this API.

Source, covariance and exposure bindings validate caller declarations; they cannot authenticate falsely renamed physical evidence. Frozen choices must precede holdout access. Cross-validation folds do not become independent units through naming.

Preflight sums complete arithmetic, retained covariance and inference numeric workspaces before source reads. BigInt plans also bound individual arrays. Nonzero 1000-cell prediction and 3000-cell association controls catch the earlier independent-subbudget admission error. Borrowed evidence/heads, private provider/Gale scratch, collection/object overhead and process RSS are excluded; this is not a whole-process memory guarantee or a benchmark.

## Independent and engineering evidence

The [independent review receipt](independent-review.md) reports no remaining blocking implementation findings and distinguishes source review from parent execution evidence.

The retained [base-R oracle](oracle.R) uses `lm`, `predict`, matrix algebra and `qnorm`, without invoking Scala helpers. It verifies full cross-target covariance, equal/unequal unit weights, known within-unit covariance, critical constants, and high-offset centering. [Raw output](oracle.log) includes a 10,000-dataset known-Gaussian response smoke study at seed `20261007`. Its streams and scope are exploratory engineering validation, not the frozen protocol confirmation campaign.

Twelve main controls plus three public-pipeline numerical controls pass on each platform. The 720-transformation scalar-dot oracle supplies strict/inclusive tolerance brackets around the upstream calculation and checks the exact plus-one receipt identity. It establishes numerical agreement, not exact mathematical-tie qualification or type-I calibration.

Full owning-module integration gates pass **433 JVM and 433 JS tests**, with zero failures, errors or warnings. This includes concurrent M5.01 work; it does not turn M5.01 arithmetic into group admission. [Integration receipts](integration-gates.json) bind source revisions, commands, toolchains and raw logs. Initial compiler failures and corrected runs are retained under `logs/`.

No provider pins, generic distribution implementations or publication were added by M4.06. Pinned Multivar `ab811e257dd67f77e8c3b70cb1ea600f274429a3` supplies the permutation machinery; the existing provider CDF/quantile gap remains explicit. M4.09 still requires the frozen 10,000-null/5,000-alternative campaign and its prescribed actions before scientific release admission.
