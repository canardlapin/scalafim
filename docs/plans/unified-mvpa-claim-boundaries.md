# Unified MVPA interpretation and inference boundaries

Current implementation inventory, 2026-10-07. This is the M4.10 claim matrix;
it is not an admission receipt for its pending scientific or resource gates.
The [calibration protocol](unified-mvpa-inference-calibration-protocol.md) and
[resource protocol](unified-mvpa-resource-comparative-protocol.md) remain binding.

| Product | Meaning and required conditioning | Boundary |
| --- | --- | --- |
| Forward task pattern | `PatternArtifact.factors.neuralByComponent` describes the fitted task-linked forward contribution in named coordinates. | A loading is not a significance map or an identified neural source. |
| Raw or calibrated filter | `PatternPrediction` and `PatternInterpretation.calibrated` map measurements to a named score with the declared covariance and centering. | Suppressor weights may be nonzero where forward loading is zero. Changing score scaling changes the filter. |
| Empirical Haufe diagnostic | Empirical `Cov(x,score) Cov(score)^-1` on independently declared diagnostic rows. The score kind and covariance denominator are retained. | A diagnostic may be dense. Training, selection or tuning overlap refuses; it supplies no p-value. |
| Gaussian conditional information | A model-based conditional-information query under the fitted Gaussian covariance. | It is not a causal effect or a measured held-out gain. Covariance misspecification remains relevant. |
| Component association | Nuisance-adjusted association between paired frozen score coordinates. | Association and incremental predictive utility are distinct estimands. |
| Incremental prediction | Full versus separately refitted reduced frozen heads, assessed on untouched units with the declared target-metric loss. | Negative gains remain visible. A population zero effect need not imply a zero conditional contrast for finite trained heads. |
| Projected rank candidate arithmetic | Huh–Jhun nuisance reduction, remaining-root CCA statistics, actual nonidentity permutations, plus-one raw p-values, cumulative-max closed p-values. | `admittedDetectableRank` remains unavailable pending M4.07/M4.09. Raw stage rejection cannot replace closed decisions for rank. |
| Voxel omnibus candidate family | One shared residual action per complete frozen voxel family under the declared Gaussian row law; streamed maxima and failures. | This tests a whole loading vector per voxel. It does not supply the resource protocol's `p*r` component-specific family, FDR, or unrestricted temporal shuffling. |
| Subject mean and heterogeneity | Frozen task/anatomical transport, subject-bound effects and full covariance. Common-effect known-Gaussian and approximate mixed-effects policies retain distinct scope. | Pointwise inference only. Estimated variance remains estimated; covariance transport does not establish calibration. No prevalence or joint spatial/component test. |
| Held-out-subject prediction | Frozen shared learning, separate head training/adaptation and untouched assessment; equal subject weighting in the declared cohort. | Descriptive conditional utility and observed variability. Population inference and prevalence remain unavailable. |
| Stable subspace comparison | Task-linked forward operators retain meaning when individual component axes are unstable. | Do not manufacture one-to-one component matches or choose alignment on confirmation data. |

The tests distinguish these products through suppressor fixtures, changed
score scalings, correlated components, negative prediction gains, oblique task
transport, and confirmation-exposure refusals. Relevant suites are
`PatternInterpretationSuite`, `ConditionalInformationSuite`,
`ComponentConfirmationSuite`, `RankConfirmationSuite`,
`VoxelFamilyRandomizationSuite`, `SubjectCoordinatesSuite`,
`SubjectGroupSummarySuite`, and `HeldOutSubjectPredictionSuite`.

## Cost claims still requiring measured admission

| Frozen workload | Required observation | Current boundary |
| --- | --- | --- |
| `J-RANK-2K` | n=500, P=Q=16, four nuisance columns, B=1999; five fresh processes, median <=60s, peak RSS <=1GiB. | Record nuisance preparation and every compact refit. Pilot kernel extrapolation is not whole-call timing. |
| `JS-RANK-2K` | n=250, P=Q=8, B=1999; five fresh Node 22 processes, median <=180s, peak RSS <=2GiB. | JVM timing and a different Node major do not qualify this row. |
| `J-VOXEL-2K` | n=500, p=100000, r=16, four nuisance columns, complete p*r family, B=1999; median <=900s, peak RSS <=8GiB. | Current voxel omnibus candidate cannot substitute for this component-specific family. |
| `JS-VOXEL-2K` | n=250, p=2500, r=8, B=1999; median <=300s, peak RSS <=4GiB. | Same family distinction; all requested outputs must remain retained. |
| Conditional-information and ROI queries | The protocol's full dimensions, first/steady boundaries, resident artifacts and peak incremental plus absolute process costs. | Numeric-cell planning excludes object/native/backend overhead. It is not an RSS certificate. |
| Matched comparison K0–K9 | 50 datasets per case, matched splits, 18 configurations per inner fold, every method's primary predictions and failures. | Missing thresholded-PLS consumer qualification and missing primary target products stay unavailable; omit neither methods nor failed cases. |

The confirmation fast path reuses fixed spatial projections and performs
compact score-space work. That work claim can be inspected through actual
compact-fit counters. A speed claim additionally needs external wall/CPU/RSS
measurement, the exact retained output inventory and the frozen reference
profile. Learning and compilation costs must be reported separately wherever
excluded. Scientific and resource failures remain separate decisions.

No blanket superiority, searchlight-replacement, neural-source, prevalence or
speed claim follows from the existing numerical tests. Final M4/M5 and release
gates remain open until their native dependency packets have their own evidence.
