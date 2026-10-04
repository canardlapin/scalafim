# UMVPA M1.12 predictive cutover

Candidate source base: `b72acb36967d40860008d0c9e9327434a8d9256d`.
Isolated branch: `work/umvpa-finish-20261001`; worktree:
`/private/tmp/scalafim-umvpa-finish-20261001`.
Mote packet: `bd-01M2BNFCWA46ZY6J17E78G7W9F`.

## Change and scientific boundary

Predictive validation now runs through the native Alder method heads. Matrix
categorical kernels accept hard labels and return concrete models; their
`CategoricalProbabilities` smart constructor validates shape, class identity,
finiteness, probability range and row normalization before deriving labels.
Correlation enforces its two-feature minimum. Feature encoding/decoding returns
`FeatureModelMetricSet`, `RidgePenalty` and optional `FeatureModelPrediction`
directly. Operator ridge/soft-LDA numerical fits remain behind native adapters.

Removed: ordinary classifier/feature-model/operator-ridge/soft-LDA CV analyses,
specialized classification and cross-decoding scanners, the cross-domain
engine, CV-only ridge/soft-LDA ADTs and predictive universal payload variants.
No compatibility facade or new analysis registry was added. The migration
ledger enumerates remaining shared boundaries and M2.09/M3.13 expiry.

Repeated exact categorical assessment averages each row's probabilities across
all declared assessments. Each row records ordered `(unit, trainingKeys)`
contributors; each contributes weight `1 / assessments.length` and resolves to
a retained fold fit. Cross-domain rows have a separate result type, with their
single source fit audit retained on the containing result. Partial assessment
families remain outside the admitted exact-validation contract.

## Independent expectations and regressions

`NativePredictiveParitySuite` runs immutable R expectations through native
heads: unequal Swift fold sizes and pooled accuracy; both regional feature
orientations and every feature metric; selected-column feature encoding; all
regional/searchlight cross-domain class/probability tables. The earlier local
rank-score caveat for exact tied R decode distances remains explicit (0.005
only for RdmCorrelation; other R expectations use 1e-10). R fixture source and
JSON provenance are retained in the original fixture files, unchanged.

`AlderFeatureModelSuite` also freezes independently computed base-R
standardized-ridge predictions for a separate two-predictor example. Its scalar
training coefficient oracle is 3/4 and excludes held-out target perturbations.
`NativePredictiveLawsSuite` checks source-affine laws, high-dynamic-range
near-collinearity, selected-table finite/nonfinite behavior, coordinated feature
permutations and equal-prototype probability ties. Declared materialized feature
correspondence is not provider-authenticated alignment evidence.

Independent review found four concrete regressions and root repaired them:
first-contribution-only repeated provenance; successful NaN probabilities from
finite overflowing scores; lost one-feature correlation refusal; contradictory
user-supplied prediction labels. Kernel/native overflow tests, ordinary and
cross-domain width refusals, validated result construction, and repeat-to-fit
links now exercise the repairs. The reviewer accepted the repairs at source
review level, conditional on owning-platform gates.

The atlas example uses identified axes, native observations/targets, typed
spatial measurements and bound validation. JVM/JS compile the same source and
suite. Exact expected parcel voxel ordinals, labels, counts and synthetic
accuracy are asserted. Canonical atlas/neural ordinal correspondence is an
explicit property of this closed in-memory fixture, not a general atlas
alignment adapter. Old scanner-vs-engine and scanner benchmark tests were
removed as dead implementation tests; no performance claim follows.

## Verification

Post-deletion module tests passed on both platforms:

| Module | JVM | Scala.js |
| --- | ---: | ---: |
| mvpa | 309 | 309 |
| mvpa-fit | 45 | 45 |
| mvpa-dataset | 107 | 107 |
| mvpa-spatial | 33 | 32 |
| Total | 494 | 493 |

The shared workflow example passed 3 JVM and 2 Scala.js tests, and the atlas
examples passed 5 JVM tests. `scalafimCompileAll` then passed on both platforms.
These logs contain no compiler warnings. Total passing tests: 997.

The owning module tasks in gate69-fresh and gate70 passed; each enclosing run
subsequently failed an example check. The workflow's run-label binding and its
independent parcel-index expectation were corrected. Gate71 reran the affected
examples and all-module compilation and exited 0. Module source hashes remained
unchanged after their respective passing gates. Initial compilation failures and
the first gate69 resident-server reload failure remain preserved; they are not
counted as passes or resource qualification.

Evidence directory: `/private/tmp/scalafim-umvpa-finish-evidence-20261001`.
Final numerical source manifest: `gate71-sources.sha256` (39 files, including
unchanged R fixture tables), verified against the final candidate. Removed-symbol
source scan: `m112-removed-source-reference-scan.json`, ripgrep exit 1 with no
matches in Scala module/example sources. `git diff --check` passed.

| Evidence log | SHA-256 |
| --- | --- |
| gate69-predictive-cutover-jvm-fresh.log | f4dacf467ac90a209f8daf0fd78ad87323ff0220356ee6dd7e38fba2bf117225 |
| gate70-predictive-cutover-js-examples-compileall.log | 7c4d0c5c36f6f40ebe58834ff45006653c5d6394bb5030d22b9565390b605652 |
| gate71-predictive-cutover-examples-compileall.log | 2d8729c60589e46749b8c24f2151f7ed087e7bd3417456dc4731c8dfbd7681ea |

Provider pins remain Gale `18d24dbb5056122032b0278f8bad557a9bb1cf23`,
Multivar `f74d631720d65147c51496dcbdd37c01912de1cb`, Alder
`e555bad92307af1c2cbc104aef398cb9d9de88f0`, resample4s
`6bc4172a966c92f1b06811eac64ac2bada9fef9b`.

This packet does not qualify release publication, scientific calibration,
process-peak resources, full atlas-provider alignment or the remaining epic
milestones. No push or merge has been performed.
