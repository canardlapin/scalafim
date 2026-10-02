# UMVPA M3.13 final legacy retirement

Status: post-removal and serialized-domain repair gates pass on JVM/Scala.js;
independent source review approves the repaired dataset boundary. Final immutable
commit review follows this source receipt.

Packet `bd-01M2BNGX1BYSP54J3P9EYK4N55`, isolated branch
`work/umvpa-finish-20261001`, base
`c0760cf1b64ea9ae6ca71879cc777e3ff81c14fc`. No push, merge or release publication.

## Disposition

Removed the remaining MVPA sample/response/fold and feature-set ontology,
`PatternSource`, generic engine/task/stream, universal ROI analysis/result
hierarchy, old dataset views and generic one-shot dataset/engine. Native
predictive, relational and canonical callers were already migrated in M1.12,
M2.09 and M3.01. No compatibility tree or renamed delegation is introduced.

`PatternMatrix` and `PatternOperator` remain useful numerical storage kernels.
Their row counts and ordinal indices convey no nominal scientific domain or
replay permission. Column selection takes explicit ordered feature indices.
Dense copies require `PatternCopyBudget`, check the cell ceiling and primitive
capacity before provider access, and apply a single feature column at a time
rather than constructing a feature-square identity. Row selections preserve
original storage indices. These are returned-copy ceilings, not source/provider,
backend workspace or process-memory guarantees.

`RunTrialReadout` retains validated response/readout composition, design-only
fit scope and caller-budgeted optional materialization. Actual scientific axes,
provider acquisition and expiration belong to `IdentifiedReadoutRelations`.
`OneShotMvpaError` retains method-specific temporal/canonical scientific refusal
ADTs; its name does not expose the deleted engine or dataset.

`DatasetObservationEvidence` constructs nominal observations and label columns
while preserving selected mapping, metadata, timepoint/estimate origins and
explicit synchronous/effectful read boundaries. It constructs no folds or
parallel pattern source. The opened-dataset adapter reads through its existing
resource/effect boundary.

## Fixture preservation

Original hard-label categorical and R reference tables now feed single-fit
kernels directly. Independent grouped partitions replace legacy fold-builder
comparisons. Operator-ridge normal-equation, hard/simplex, convergence and
adjoint fixtures remain. Native predictive/relational/canonical suites preserve
method behavior already accepted in their owning packets. Removed engine and
scanner comparisons tested retired orchestration; no performance conclusion
transfers from their deletion.

Storage regression tests cover copy ceilings before reads, primitive capacity,
column-wise admitted copies, foreign/missing/duplicate column selection and
original row identity. Dataset/readout native regression coverage and exact
counts are recorded after the post-removal gate.

Current module/example guides use the native APIs. Earlier engine, trial-readout,
beta-free and finite-index plans are marked historical and link to immutable
pre-retirement records. Earlier audit evidence retains its recorded base/status.
The M0 ledger records every family disposition without importing resource or
inference qualification from a source-removal check.

## Verification boundary

Final command (bounded owning targets, followed by the compile alias):

```sh
python3 tools/build/sbt-warm mvpaJVM/test mvpaJS/test mvpaFitJVM/test mvpaFitJS/test mvpaDatasetJVM/test mvpaDatasetJS/test mvpaSpatialJVM/test mvpaSpatialJS/test workflowExamplesJVM/test workflowExamplesJS/test atlasExamplesJVM/test scalafimCompileAll
```

`gate111-retirement-final.log` and its exit metadata report exit 0. The four
owning modules pass 329 + 45 + 106 + 20 = 500 tests per platform. Consumer
workflows pass 3 JVM/2 JS and atlas examples 5 JVM: **508 JVM + 502 JS = 1,010**.
`scalafimCompileAll` passes with no warning/error lines in the complete raw log.
Raw log SHA-256: `2c739f154f4f7553acae0a6783342cfad04775fa30980c29ebc7a888d0010589`.

`gate111-sources.json` freezes 175 owning/consumer Scala sources, including the
native replacement files; every entry matches the tested working tree. Manifest
SHA-256: `ed6aac11c7946b80975ed4f11409005dc354d3ce4182d2bd4182707473fbff79`.
`gate111-retirement-scan.json` records 17 physical deletions, 1,513 scanned
source/test/example/benchmark/tool files, positive controls, zero active retired
symbol matches and zero old MVPA sample/response references in owning/consumer
Scala. The two nested numerical fixture `Fold` records are unrelated to the
retired package type. The unchanged v1 PyMVPA failure-policy string is explicitly
historical fixture data. Its frozen JSON hash remains
`07ebd4de37ae675895352c4763643c96b72b929c80c1e6027c8e7aea41645ee0`;
the generator's Python syntax check passes. Its payload was not regenerated.

Dataset native tests cover selected/effectful reads, metadata/mapping/value
orientation, estimate origins, original labels, real native Alder classification,
isolated source/content/ordered-label identity changes and explicit read/shape
failures. Native trial-readout tests retain the independent coefficient oracle,
estimability, fit scope, explicit copy budget and named mismatch refusals.

Staged-tree whitespace validation found one trailing blank line in the new
readout file. After correction, `gate112-retirement-staged-readout.log` reruns
all 45 readout tests on each platform and `scalafimCompileAll`, exit 0 with no
warning/error lines. Raw log SHA-256: `d325360d8a189b55bb80a9ed53695b16df4672bd09e39e14210b62f066446e09`.
`gate112-sources.json` freezes the final 175 sources, SHA-256
`49023258d1de6f368bc39dc72e490cad81f4139e96f79c22b94dd9ad1f7847ad`. Only that whitespace-only readout source differs
from gate111; the other 174 sources are identical. Owning and consumer behavior
therefore retains the gate111 counts, with affected readout source reverified.

Earlier draft gate109 passed core/readout only. Gate110 failed compilation of
the added dataset workflow fixture and was corrected; it is not passing evidence.
The complete final gate111 supersedes those drafts. Evidence files are under
`/private/tmp/scalafim-umvpa-finish-evidence-20261001`.

Independent core review resolved the local-row-position ambiguity by using
`selectRowPositions(IndexedSeq[Int])`; storage IDs remain preserved separately.
Final source review finds no compatibility delegation or second scientific axis
system. Returned-copy ceilings remain distinct from whole-call memory evidence.

The structured pattern fitter remains experimental. Whole-brain allocation,
operational audit, inference calibration, group qualification, matched benchmark
and release packets retain their own acceptance gates.

## Serialized-domain repair after immutable review

Independent review blocked draft commit
`195f26ecc19ee6b03e8c82fea0bc0ac80f1149f2`: equal-shaped axes could decode
foreign labels onto unrelated dataset samples. The repair requires an explicit
`DatasetId` for derived rows and series, binds sample coordinates to domain,
ordered origins, ordinals and run identities, and binds neural coordinates to
domain, mapping kind and spatial shape. Label and numeric payload changes
preserve coordinates while changing their own evidence identity. This is a
caller-declared population contract, not proof of external acquisition identity.

Two shared regression tests exercise actual serialized `Column.decode` refusals
for foreign datasets and different sample origins, same-coordinate acceptance,
neural shape/domain distinctions and sample-axis preservation under feature
restriction. Independent read-only review approves the repaired three source
files.

`gate117-repaired-integration.log` exits 0: core 336, readout 45, dataset 108,
spatial 20 per platform, workflow 3 JVM/2 JS, atlas 5 JVM: **517 JVM + 511 JS
= 1,028**. Seven core tests per platform belong to the concurrently developed
M4.03 packet and do not qualify that packet's later revisions. This packet's
source-specific count is 510 JVM + 504 JS = 1,014. The complete raw log contains
no compiler warning/error lines; `scalafimCompileAll` passes.
Raw log SHA-256: `3e52fee8491af241ff827447d45be9bd244b4cc8df2c128703aeb9d119daf46d`.

`gate117-retirement-sources.json` freezes this packet's 175 sources, SHA-256
`b1e916a2f629a65f62e876313889911669395019b3ebf34d2c1b62c05aafd6f8`. Only the three dataset boundary/fixture files
differ from gate112; other 172 sources are byte-identical. The retirement scan
and immutable reference fixture hashes are unchanged. Gate114/115 failed
compilation of a concurrent M4.03 draft; gate116 was interrupted after its
identity writer entered a confirmed infinite loop. None is passing evidence.
The repaired dataset source passed the complete gate117.
