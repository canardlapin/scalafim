# MVPA zero-cruft migration record

Status: completed clean-break record; sole historical-name exemption

Mote epic: `bd-01M0SV0ZFCE9154M7S9BBR3ZQ9`

This file is intentionally exhaustive. It is the only file allowed to retain
the retired names when the epic closes. It is not current API documentation.

## Cutover decision

The refactor did not retain a compatibility layer. The following declarations
were the frozen removal inventory during the cutover and are now absent from
production code, tests, examples, build definitions, and current API
documentation:

| Declaration | Frozen definition |
|---|---|
| `RoiPayload` | exactly `Classification`, `OperatorRidge`, `Rdm`, `Rsa`, `SamplewiseRsa`, `FeatureModel` |
| `Response` | exactly `Categorical`, `Probabilistic`, `Continuous` |
| `FeatureSetKind` | exactly `Region`, `Searchlight` |
| `FoldPlan` | one declaration in `FoldPlan.scala`; no variants or new purposes |
| `RoiAnalysis` | one declaration in `MvpaEngine.scala`; no sibling universal analysis trait |
| `MvpaResult` | one declaration in `MvpaEngine.scala`; no new payload/result shell |

The deleted migration ledger recorded the exact declaration and call-site
inventory during the cutover. Final source removal is Mote
`bd-01M0SVK5BF37JZ558WEMDG9C00` (P6.4), and
the repository-wide absence proof is `bd-01M0SVK9B1A1JEM5T40DX4X1YH` (P6.7).
The canonical binding, compiler, validation, cross-fit, and pairing sources
are a zero-reference zone; they neither accept nor return a
compatibility-shaped fold value.

No `compat`, `compatibility`, `legacy`, or `adapter` namespace may be introduced
under `scalafim.fmri.mvpa`. A temporary migration bridge must be:

- package-private;
- used only on the implementation branch;
- named in the authoritative migration ledger;
- linked to a concrete Mote deletion ticket;
- deleted before P6 closes.

`tools/verify-mvpa-legacy-surface.sh --final` enforces the completed state. It
also rejects reintroduction of the deleted classifier registry, duplicate
relational entry points, dormant execution policy, and historical oracle or
generator names.

## Production declarations to remove

### Source and axis ontology

- `PatternMatrix`
- `PatternOperator`, `PatternOperatorOrigin`, `PatternOperatorProvenance`
- `PatternSource`, `DensePatternSource`, `OperatorPatternSource`,
  `InMemoryPatternSource`
- `SampleIndex` and count-only `SampleAxis`
- `FeatureIndex` where it denotes an unbound ordinal
- `RoiId`
- `ResponseContext`, `FoldedResponseContext`
- `ClassMembership` as an unbound sample container

### Feature-set and split ontology

- `FeatureSet`
- `FeatureSetKind`
- `FeatureSetPlan`
- `Fold`
- `FoldPlan`
- `FoldPartition`
- spatial and locus feature-set-plan builders and wrappers

### Per-region execution ontology

- `RoiAnalysis`, `FoldRequiredRoiAnalysis`
- `DenseRoiAnalysis`, `OperatorRoiAnalysis`
- `FoldRequiredDenseRoiAnalysis`, `FoldRequiredOperatorRoiAnalysis`
- `RoiContext`, `UnfoldedRoiContext`, `FoldedRoiContext`
- `RoiAnalysisResult`
- `RoiPayload`
- `RoiOutcome`
- `MvpaTask`
- `MvpaEngine`
- `MvpaStream`, `MvpaStreamControl`
- `MvpaResult`
- every public specialized scanner or parallel engine that bypasses the
  canonical compiler

### Parallel method result shells

The numerical kernels may survive, but these parallel top-level shells do not:

- `CanonicalEffectMvpaResult`, `CanonicalFeatureSetOutcome`,
  `CanonicalFeatureSetPayload`
- `NonnegativeCanonicalMvpaResult`, `NonnegativeCanonicalFeatureSetOutcome`,
  `NonnegativeCanonicalFeatureSetPayload`
- `ManovaMvpaResult`, `ManovaFeatureSetOutcome`, `ManovaFeatureSetPayload`
- signed cross-run Rayleigh MVPA result/outcome/payload types
- `SoftLdaCrossValidatedResult` when used as a parallel MVPA result rather than
  the typed result of its estimand
- one-shot MVPA task and engine façades
- cross-domain per-region analysis/task/engine types

### Universal containers

- `Response`
- `RoiPayload`
- `MetricVector` as a scientific result rather than a derived view
- any replacement central `AnalysisKind`, `Payload`, `Target`, or universal
  split enum

## Modules and packages to remove

- directory and artifact `modules/mvpa-fit` / `scalafim-fmri-mvpa-fit`
- directory and artifact `modules/mvpa-dataset` /
  `scalafim-fmri-mvpa-dataset`
- directory and artifact `modules/mvpa-spatial` /
  `scalafim-fmri-mvpa-spatial`
- packages `scalafim.fmri.mvpa.fit`, `scalafim.fmri.mvpa.dataset`, and
  `scalafim.fmri.mvpa.spatial`
- their JVM/Scala.js project aliases, aggregates, module READMEs, and published
  dependency edges

All retained code moves behind the single `scalafim.fmri.mvpa` vocabulary.

## Tests and fixtures to rewrite or remove

Every test file is individually classified in the migration ledger. The final
text court additionally requires no test references to the production names
above. In particular:

- per-region engine/source representation tests are deleted;
- searchlight tests build a frame and run an estimand;
- dense/operator tests become canonical evidence execution laws;
- fold tests become validation, cross-fit, or pairing design laws;
- fixture values are retained only in neutral records with explicit estimand
  receipts;
- no compile-only compatibility suite remains.

The fixture generators remain only if their generated values still anchor a
named estimand. They must not emit old result, feature-set, fold, or response
containers.

## Examples and current documentation to rewrite or remove

Checked examples:

- `examples/atlas-jvm/src/main/scala/scalafim/examples/atlas/AtlasToMvpaRegions.scala`
- `examples/workflows-jvm/src/main/scala/scalafim/examples/workflows/AtlasMvpaWorkflow.scala`

Checked current or formerly current documentation:

- root `README.md`
- `docs/module-relations.md`
- `modules/mvpa/README.md`
- the three deleted-module READMEs
- `docs/plans/mvpa-engine.md`
- `docs/plans/one-shot-mvpa.md`
- `docs/plans/cca-one-shot-mvpa.md`
- `docs/plans/signed-cross-run-rayleigh.md`
- `docs/plans/finite-indexed-spaces.md`
- `docs/plans/neuroarchive-dataset-bridge.md`
- `docs/plans/ntab.md`
- `docs/plans/response-representation-archive-phase-6.md`
- `docs/plans/scenario-parity-harness.md`
- `tools/r-parity/mvpa-fixtures.md`

Historical method and oracle rationale may be preserved only after removing
claims that the retired API is current. This file is the sole exemption from
the exact retired-name search at epic close; the migration ledger is deleted or
archived outside current documentation before the final court.

## Final court

Run:

```sh
tools/verify-mvpa-legacy-surface.sh --final
```

Success means:

1. all three retired split-module directories are absent;
2. the retired packages are absent;
3. production, tests, examples, build definitions, READMEs, and current docs
   have zero exact retired-name hits;
4. no compatibility/legacy/adapter namespace exists;
5. this historical record is the only search exclusion.

Passing the text court is necessary but not sufficient. P6 also requires the
two canonical vertical slices, public consumer compilation, JVM and Scala.js
tests, oracle freshness, and scientific/execution receipt laws.
