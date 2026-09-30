# Unified MVPA M0.01: ownership constitution and migration ledger

Status: adopted M0 planning contract; this is not implementation, numerical
qualification, provider admission, or release evidence.

Mote packet: `bd-01M2BNE82Q1NJADVWB3ECXRXWD`.

Authority: [PRD v0.2](unified-mvpa-prd.md), the
[implementation epic](unified-mvpa-epic.md), the
[architecture notes](unified-mvpa-architecture-notes.md), and the
[supplementary review](unified-mvpa-supplementary-review.md). Where an older
note suggests a compatibility facade, the PRD's rapid replacement policy wins.

## Source receipt

The tracked MVPA source inventory below is bound to published ScalaFIM revision
`528c302e454697055bc9af31c9a6eca684f019e3` (2026-09-12). Before admission,
`git diff --quiet` established that the originally inspected local descendant
had no changes under `modules/mvpa`, `modules/mvpa-fit`,
`modules/mvpa-dataset`, `modules/mvpa-spatial`, or `examples/workflows-jvm`.
The ledger is therefore rebound to the reachable remote revision whose relevant
trees were actually equivalent, rather than requiring an unpublished commit.

The unified-MVPA plans and foundation spike are identified working-tree
artifacts. Their presence does not make the spike a production module or admit
its provider candidates. M0.02 owns immutable provider admission. Historical
MVPA candidates such as `4f079da`, `574db30`, and `81a0408` remain historical
references: this ledger does not treat their APIs as current merely because
the commits are reachable in local refs.

Any migration owner must refresh this receipt at claim time. If a listed path
has changed, the owner records the new revision and updates the row before
deleting or replacing its route.

## Constitution

1. **Identity is nominal and layered.** Source identity, representation
   identity, sample/effect/feature spaces, repeated occurrences, preparation,
   and realization are distinct. Equal dimensions or equal values do not make
   spaces interchangeable. Reordering requires an explicit, checked reindexing.
2. **Composition is axis bound.** Restrictions, resampling legs, measurement
   legs, pairing, and reductions bind to declared axes and preserve occurrence
   identity. A composed plan is valid only at the population of the stage where
   it executes.
3. **Leakage is an access property.** Training, holdout, tuning, inspection,
   cost probing, and adaptive analyst access are explicit scopes. Preparation
   learned from data is fitted only on its authorized training scope. A digest
   or apparently harmless diagnostic may not read held-out payloads implicitly.
4. **Inspection is bounded and honest.** `describe`, `inspect`, `explain`, and
   plan-diff operations use declared metadata unless a separately authorized
   read is requested. Unknown data-dependent facts remain unknown.
5. **Estimands and results stay distinct.** Classification, regression,
   RDM/crossnobis, RSA, canonical effects, MANOVA, global pattern fitting, and
   confirmation return method-owned typed results. A universal result enum or
   metric bag does not define their scientific meaning. Local failures remain
   visible rather than being dropped from summaries.
6. **Representation is behind evidence.** Dense matrices, operators, lazy
   readers, and sufficient statistics may share a scientific route only when
   their laws and materialization behavior agree. `PatternMatrix` and
   `PatternOperator` may survive as kernels, not as competing scientific
   identities.
7. **Ownership follows the dependency boundary.** Gale owns generic numerical
   kernels; Multivar owns general multivariate semantic algebra; resample4s owns
   ordinal design construction; Alder owns predictive lifecycle; ScalaFIM owns
   fMRI evidence, estimands, spatial frames, domain validation, scattering, and
   receipts. Generic `fit` never depends on MVPA.
8. **Replacement is rapid and complete.** No new `RoiPayload` case, old enum
   case, public compatibility wrapper, duplicate registry, or `legacy/` tree is
   permitted. A private bridge names its callers and expires by the immediately
   following migration gate. Every old orchestration route is gone by M3.13.

## Ownership map

| Concern | Long-term owner | ScalaFIM use |
| --- | --- | --- |
| Matrix/vector/operator primitives, solves, decompositions, optimization kernels | Gale | Typed adapters and numerical receipts; no private replacement families. |
| General PCA/CCA/canonical and multivariate semantic algebra | Multivar | fMRI-specific estimands and evidence adapters only. |
| Ordinal selections, coverage, deterministic split/random plans | resample4s | Bind plans to identified ScalaFIM axes and scientific policies. |
| Predictive preparation, learners, validation, tuning, fitted artifacts | Alder | Translate identified fMRI evidence and return typed domain results. |
| fMRI series/design/readouts, estimability, temporal scope | ScalaFIM response/fit and future evidence seams | Build observations or relations without a second response system. |
| Voxel, surface, atlas, ROI, searchlight, basis and alignment frames | ScalaFIM image/surface/atlas/locus and future frame seams | Compose measurements without extending `FeatureSetKind`. |
| Predictive, relational, canonical/global and confirmation estimands | ScalaFIM unified analysis | Open typed estimands and method-owned results. |
| Persistence and exported estimates | Existing ScalaFIM archive/estimates facilities | Add admitted typed profiles; do not create an MVPA-only archive. |

Physical modules are not frozen by this table. Owners first prove the logical
direction within the current modules; a later split requires a demonstrated
independent boundary.

## Method and consumer ledger

The “consumer/fixture” column names every in-repository production consumer
class and the fixture families that protect useful behavior. Files defining a
route are listed even when their only current callers are tests and docs,
because they are public Scala surfaces. Documentation consumers are grouped in
the final row.

| ID | Current route at the source receipt | In-repository consumers and preserved fixtures | Destination and responsible packet | Cutover and deletion gate |
| --- | --- | --- | --- | --- |
| F1 | `SampleAxis`, `ResponseContext`, `FoldedResponseContext`; MVPA `Response` | `Classification`, `Rsa`, `MvpaEngine`, `CrossDecoding`, `OneShotDataset`, all predictive/relational/one-shot suites | Identified axes, axis-bound columns and multiresponse targets: M1.01; purpose-specific designs: M1.02 | Predictive users removed in M1.12; relational users in M2.09; definitions and residual users in M3.13. The separate `modules/response` module is retained. |
| F2 | `Fold`, `FoldPlan`, `leaveOneBlockOut`, `alignTo` | `MvpaEngine`, classifiers, operator ridge/RSA, `OneShotDataset.leaveOneRunOut`, `SoftLda`, canonical/MANOVA/signed/constrained routes, `MvpaDatasetView`, `AtlasMvpaWorkflow`; `MvpaCoreSuite`, `ClassificationSuite`, one-shot suites | Axis-bound resample4s validation/cross-fit/pairing designs: M1.02, then method-specific M1/M2 compilers | Per predictive/relational slice; last definitions and callers in M3.13. |
| F3 | `FeatureSet`, `FeatureSetPlan`, `FeatureSetKind` | `MvpaEngine`, cross-decoding, scanners, every one-shot collector; `SpatialFeatureSetPlans`, `LocusFeatureSetPlans`, atlas/workflow examples; core/spatial suites | Selection measurements and lazy/dependent `MeasurementFrame`s: M1.03 | Predictive builders in M1.12; relational-only helpers in M2.09; remaining enum/helpers in M3.13. Add no enum cases. |
| F4 | `PatternSource`, `InMemoryPatternSource`, `PatternMatrix`, `PatternOperator` | All `MvpaEngine` and cross-domain execution, `OneShotDataset.patternSource`, dataset adapters; dense/operator parity in `OperatorMvpaSuite`, `OperatorRsaSuite`, `OperatorRidgeSuite`, `OneShotDatasetSuite` | Identified evidence plus representation adapters: M1.01/M1.03/M1.06; operator-native relations: M2.01/M2.03 | `PatternSource` primary API in M1.12 and final facade in M3.13. Keep only useful matrix/operator kernels behind the new evidence model. |
| F5 | `RoiAnalysis`, folded variants, `RoiContext`, `MvpaEngine`, `MvpaTask`, `MvpaStream` | Every current analysis, scanners, `OneShotMvpaTask`/`OneShotMvpaEngine`, dataset executor, workflow example; `MvpaCoreSuite`, `OperatorMvpaSuite` | Open estimands, bind/compile/execute, bounded traversal and typed results: M1.04/M1.05; deterministic work units: M3.10 | Slice-local routes disappear in M1.12/M2.09; shared hierarchy and engine disappear in M3.13. |
| F6 | `RoiAnalysisResult`, six-case `RoiPayload`, `RoiOutcome`, `MvpaResult`, `MetricVector` | All analysis implementations, scanners, `OneShotMvpaEngine`; examples and all result assertions | Method-owned `AnalysisResult` products and optional export/render views: M1.04; archive/estimate profiles: M3.11 | Freeze the existing six payload cases now. Delete cases with each route and the universal result contract in M3.13; keep `MetricVector` only as a non-scientific view if still useful. |
| P1 | `SwiftCentroidClassifier` with `None`, training-fitted sample-SD `ZScore`, or diagonal shrinkage; priors enter linear centroid scores; softmax emits probabilities | `CrossValidatedClassifierAnalysis`, `SearchlightClassifierScanner`, `OneShotMvpaEngine`, `AtlasMvpaWorkflow`; `ClassificationSuite`, `SearchlightClassificationSuite`, `MvpaParitySuite`, independent `MvpaMigrationParitySuite` | Keyed leave-one-run-out Alder pipeline over identified evidence: M1.07, with provider/fixture prerequisites M0.02/M0.04 and identity gate M1.11 | Switch all callers and delete old classifier/analysis/scanner branches in M1.12. Preserve first-occurrence class order, training-only scaling, sample-SD denominator, priors, score/softmax columns, repeated-test aggregation and sample ordering. |
| P2 | `CorrelationCentroidClassifier`; row-correlation template scores and softmax | Cross-domain default classifier and ordinary CV; `ClassificationSuite`, `MvpaParitySuite`, `CrossDecodingSuite` | Remaining predictive-head migration: M1.10 over the M1.06 lifecycle | M1.12 after M1.11. Preserve row-offset invariance, class alignment and degenerate finite normalization. |
| P3 | `RidgeLdaClassifier`; pooled residual cross-product plus diagonal penalty, class priors and softmax | Ordinary CV, specialized searchlight scanner, one-shot example; `ClassificationSuite`, `SearchlightClassificationSuite`, `OneShotDatasetSuite`, `MvpaParitySuite` | Remaining predictive heads: M1.10; reuse admitted general solves from Gale | M1.12 after M1.11. Preserve pooled denominator, penalty meaning, priors, probability column alignment and typed non-finite/fold failures. |
| P4 | `CrossDomainDataset`, `CrossDecodingDesign`, paired feature sets, `CrossDomainMvpaEngine`, `NaiveCrossDecodingScanner` | `CrossDomainClassifierAnalysis`; `CrossDecodingSuite`, `CrossDecodingScannerSuite`, `MvpaPropertySuite`, `MvpaAdversarialSuite`, `MvpaParitySuite` | Identified source/target evidence and cross-decoding estimand: M1.10; exposure/origin checks M1.09/M1.11 | M1.12. Preserve target-row alignment, source class order, unknown-target refusal, paired ROI identity, feature permutations, local failures and scanner/reference parity. |
| P5 | `FeatureModelDesign`, bidirectional `FeaturePredictionDirection`, `FeatureRidgeEstimator`, `FeatureModelAnalysis` | Regional and searchlight feature-model routes; `FeatureModelSuite`, `MvpaPropertySuite`, `MvpaAdversarialSuite`, `MvpaParitySuite` | Scalar/multiresponse Alder integration M1.08 and remaining caller cutover M1.10 | M1.12 after M1.11. Preserve item averaging, encode/decode orientation, standardization, affine laws, predictions and feature-name identity. |
| P6 | `OperatorRidge`, `CrossValidatedOperatorRidgeAnalysis`, iterative execution receipts | One-shot composed operators and ordinary engine; `OperatorRidgeSuite`, `OneShotDatasetSuite`, `MvpaParitySuite` | Alder matrix/operator-native learner and scoped preparation: M1.06/M1.08; exact execution exposure: M1.09 | M1.12 after M1.11. Preserve hard/soft targets, direct/iterative agreement, no dense adapter, convergence receipts and held-out perturbation noninterference. |
| P7 | `SoftLda`, `CrossValidatedSoftLdaAnalysis` | One-shot operator path; `SoftLdaSuite`, `OneShotDatasetSuite` | Remaining predictive head using existing Multivar/Gale capabilities: M1.10 | M1.12 after M1.11. Preserve component selection, nuisance scope, fitted-fold receipts, scores/probabilities and typed failures. |
| R1 | `RdmMethod` squared Euclidean, Euclidean and correlation; `RdmAnalysis`; `LabeledRdm` lower-triangle order | Ordinary ROI analysis; `RdmSuite`, `RsaSuite`, `MvpaParitySuite` | Typed relation/query compiler with explicit metric and item/pair identity: M2.01/M2.03/M2.04 | M2.09 after M2.08. Preserve lower-triangle labeling, normalization choice, correlation semantics and non-finite refusal. |
| R2 | `PartitionMeansBuilder`, dense `CrossnobisAnalysis`, optional feature-count normalization | Folded ROI engine; `RdmSuite`, `RsaSuite`, `MvpaParitySuite`, independent `MvpaMigrationParitySuite` | Ordered partition relations and identity-metric signed squared-Euclidean query: M2.01/M2.02/M2.04 | M2.09 after M2.08. Do not upgrade the baseline's identity metric to an estimated-noise claim. Preserve negative values and explicit normalization. |
| R3 | `RdmScorer` Pearson/Spearman/partial-Pearson and `RsaAnalysis` | Dense RDM/RSA workflow; `RsaSuite`, `MvpaParitySuite` | Typed first/second-order queries and model/nuisance dependencies: M2.03/M2.06/M2.07 | M2.09 after M2.08. Preserve label alignment, tie ranking, nuisance residualization and typed undefined/zero-residual cases. |
| R4 | `SamplewiseRsaDesign`, `SamplewiseRsaAnalysis` | ROI execution; `RsaSuite` and `MvpaParitySuite` | Explicit samplewise relational estimand with block-excluded pairing and origin tracking: M2.02/M2.07 | M2.09 after M2.08. Preserve consistent-reordering invariance and per-row undefined results. |
| R5 | `OperatorCrossvalidatedGeometry`, `OperatorCrossnobisAnalysis`, `OperatorCrossnobisRsaAnalysis` | One-shot beta-free relation path and ordinary operator engine; `OperatorRsaSuite`, `BetaFreeRsaAcceptanceSuite`, `OperatorMvpaSuite` | Operator-native relations, separately admitted residual precision, and reusable queries: M2.01/M2.03/M2.04/M2.05/M2.06/M2.07 | M2.09 after M2.08. Preserve adjoint sufficient-statistic execution, fold-independent bound, held-out exclusion, negative geometry and identity-metric baseline. |
| G1 | `OneShotDataset`, `OneShotMvpaTask`, `OneShotMvpaEngine` | All `mvpa-fit` estimands and `OneShotDatasetSuite`; older one-shot plan docs | fMRI evidence construction plus operator compilation and typed estimands: M1.06/M2.01/M3.01 | Method routes cut over in M1/M2; generic one-shot wrappers and parallel result collection gone in M3.13. |
| G2 | `CanonicalEffectDataset`, schedules/moments, `CanonicalEffectMvpa`, parallel canonical payload/outcome/result types | `CanonicalEffectMvpaSuite`, `CanonicalEffectAcceptanceSuite`, downstream constrained/MANOVA routes | Typed canonical/global decomposition artifact using Multivar semantics: M3.01 | M3.13 after M3.01 and relational/predictive cutovers. Preserve training-frozen geometry, sufficient-statistic scaling, row/run/feature invariances, null/degeneracy behavior and large-time-axis compactness. |
| G3 | `ManovaDataset`, schedules/moments, `ManovaMvpa`, parallel spectrum payload/outcome/result types | `ManovaMvpaSuite`; `generate_one_shot_manova_fixtures.R` | Typed multivariate effect-spectrum estimand/artifact: M3.01 | M3.13. Preserve generalized-root spectrum, hypothesis rank, training scope and reference fixture. |
| G4 | `NonnegativeCanonicalModelSpec`, `NonnegativeCanonicalMvpa`, parallel payload/outcome/result types | `ConstrainedCanonicalMvpaSuite`; `generate_constrained_canonical_fixtures.R` | Open constrained canonical estimand over admitted canonical artifact: M3.01 | M3.13. Preserve the nonnegative canonical-root estimand, selection rule, held-out score and constraint diagnostics; do not turn it into a placeholder probability model. |
| G5 | `SignedCrossRunRayleighMvpa`, signed estimator/orientation/exchangeability ADTs, parallel payload/outcome/result types | `SignedCrossRunRayleighMvpaSuite`; committed R fixture and `generate_signed_cross_run_rayleigh_fixtures.R` | Typed signed cross-run relational/global estimand using shared evidence and artifact contracts: M3.01 | M3.13. Preserve sign/orientation, run-pair scope, training-frozen fit, scale/order invariances, negative values and explicit exchangeability receipt. |
| D1 | `MvpaDatasetView`, synchronous readers, opened-dataset adapters and `OpenedDatasetMvpaExecutor` | `MvpaDatasetViewSuite`, `modules/mvpa-dataset/README.md` | Identified lazy source/axis adapter M1.01/M1.03 and Alder lifecycle bridge M1.06 | Predictive executor in M1.12; remaining old source/fold adapters in M3.13. Preserve explicit sync/effectful boundaries, sample metadata, folds-by-run/block, selected reads and failures. |
| D2 | `SpatialFeatureSetPlans`, `LocusFeatureSetPlans`, `SpatialFeatureDomain` | Atlas/image/surface/locus region and searchlight callers; spatial suites, `AtlasToMvpaRegions`, `AtlasMvpaWorkflow` | Measurement-frame and spatial scattering adapters M1.03; operator relation adapter M2.01 | Predictive old builders in M1.12; relational old-only helpers in M2.09; residual feature-set ontology in M3.13. ROI/searchlight behavior remains supported. |
| D3 | `AtlasMvpaWorkflow`, `AtlasToMvpaRegions`; root/example/module READMEs; `mvpa-engine.md`, `finite-indexed-spaces.md`, `one-shot-mvpa.md`, `beta-free-rsa.md`, `one-shot-manova.md`, `cca-one-shot-mvpa.md`, `one-shot-constrained-canonical.md`, `signed-cross-run-rayleigh.md`; `mvpa-fixtures.md` and R generators | Analyst workflows, documentation links and fixture generation | Migrate examples and documents with each owning method packet; M5.06 supplies final workflows and old-to-new guidance | Each owning M1/M2/M3 cutover updates its docs/examples/generator paths. M3.13 source/reference scan must find no active old API calls; immutable historical links may remain labeled as history. |

### Public surface freeze

The six currently permitted `RoiPayload` cases are `Classification`,
`OperatorRidge`, `Rdm`, `Rsa`, `SamplewiseRsa`, and `FeatureModel`. They are a
closed migration inventory, not extension points. New functionality introduced
by this epic must use a method-owned result rather than adding a seventh case.

No private compatibility bridge is registered at M0.01. If one becomes
unavoidable, its owning row must name the exact declaration and callers, the
next-milestone expiry, and the approving packet before it is merged.

## Verification and future evidence commands

M0.01 is a source and protocol audit. It makes no numerical passing claim. Its
static read-back is:

```sh
git rev-parse HEAD
git status --short -- build.sbt modules/mvpa modules/mvpa-fit modules/mvpa-dataset modules/mvpa-spatial examples/workflows-jvm
rg --files modules/mvpa modules/mvpa-fit modules/mvpa-dataset modules/mvpa-spatial
rg -l 'MvpaEngine|MvpaTask|MvpaStream|RoiAnalysis|RoiPayload|RoiOutcome|MvpaResult|FoldPlan|FeatureSetPlan|PatternSource' modules examples benchmarks docs
rg -l 'SwiftCentroidClassifier|CorrelationCentroidClassifier|RidgeLdaClassifier|CrossDecoding|OperatorRidge|FeatureModel|SamplewiseRsa|Crossnobis|RdmAnalysis|RsaAnalysis|SoftLda|CanonicalEffectMvpa|ManovaMvpa|NonnegativeCanonical|SignedCrossRunRayleigh' modules examples benchmarks docs tools
```

M0.04 owns the frozen numerical receipt. Its initial exact commands are kept
separate by platform so Scala.js linking remains bounded:

```sh
LC_ALL=C LANG=C Rscript tools/mvpa/generate_migration_parity.R --check
Rscript tools/r-parity/generate_mvpa_r_parity_fixtures.R
Rscript tools/r-parity/generate_one_shot_manova_fixtures.R
Rscript tools/r-parity/generate_constrained_canonical_fixtures.R
Rscript tools/r-parity/generate_signed_cross_run_rayleigh_fixtures.R
sbt "mvpaJVM/test"
sbt "mvpaJS/test"
sbt "mvpaFitJVM/test"
sbt "mvpaFitJS/test"
sbt "mvpaDatasetJVM/test" "mvpaSpatialJVM/test"
sbt "mvpaDatasetJS/test" "mvpaSpatialJS/test"
```

Consumer cutovers additionally run the headless workflow and the warning-clean
cross-platform compile gate:

```sh
sbt "workflowExamplesJVM/runMain scalafim.examples.workflows.runAtlasMvpaWorkflow"
sbt scalafimCompileAll
```

M0.06 freezes hardware, costs, workloads, and comparisons in the
[resource and comparative benchmark protocol](unified-mvpa-resource-comparative-protocol.md).
The fixed-fit target remains `n=1,000`, `p=100,000`, `q=256`, `r=16`, `h=16`
within 5 minutes and 8 GiB. The approved court also fixes complete nested
selection, first/steady ROI queries, conditional information, 2,000-reference
rank confirmation, family-complete voxel inference, process-cold I/O, and
Scala.js budgets. The rank row replaces the PRD's provisional 1,000-resample
target so it matches the M0.05 inferential protocol. No row may report a budget
met without a reproducible receipt counting preparation, selection, retained
outputs, source buffers, workers, covariance work, and backend scratch.

## Closure rule

A ledger row closes only when its destination exists, its preserved fixtures
and affected JVM/Scala.js gates pass, all named production/test/example/doc
callers have migrated, and a source scan finds neither the retired definition
nor a renamed wrapper delegating to it. Numerical parity, protocol correctness,
resource qualification, semantic conformance, and scientific calibration are
recorded independently; success in one category cannot close another.
