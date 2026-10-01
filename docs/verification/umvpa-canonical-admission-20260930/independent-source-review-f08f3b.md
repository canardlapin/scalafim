# Independent source review: PASS

Exact source: `f08f3b9b126a37172ca5720665061918beef6e86`, clean at inspection.
Repository: `/private/tmp/scalafim-umvpa-20260930-canonical-admission`.
Verdict: PASS for the bounded origins, exposure, and minimum truthful M1.05 source contract. This is not a compile/test/release or canonical Alder-admission verdict.

## Final delta

Compared with reviewed source f0166b5649fb567cda5a33e547d950e0cee1c862, only Observations.scala and Diagnostics.scala changed. All eight same-list dependent origins defaults are replaced by no-origin overloads that construct conservative source-bound unknown support and delegate to explicit-origin implementations. This preserves the existing call forms, nominal return axes, and centralized validation; there is no new callback or payload access. PlanDiff now has package-scoped construction, allowing Diagnostics.diff to instantiate it while rebindRequired remains derived from changes. Both source blockers in the preceding review are resolved. No further source blocker was found in this delta.

## Retained scientific review findings

- Complete canonical EvidenceIdentity framing includes node operations, parents, all provenance evidence variants, roots, recursively structured ValueIdentity, and origins independently. Native root and exposure association use it. Explicit Unknown origins cannot erase independently retained provenance. Regression source contains actual provenance-only Unknown-origin mutation, changed root/reference assertions, and foreign-reference refusal with zero operator calls.
- Both plan metadata and explanation continuations check PlanId plus a framed fingerprint of captured source and caller capabilities. This prevents the previously demonstrated silent skip when available capabilities change. Tests directly exercise metadata continuation context changes; direct explanation continuation coverage remains narrower, although it shares the checked source logic.
- Failed or thrown DerivedScore callbacks retain an event and deny untouched confirmation, including after DTO reconstruction. Payload attempts are likewise conservative. The current hasObservedScores/DependsOnObservedScores naming also covers uncertain exposure; it must not be interpreted as proof that a failed callback delivered a score.
- Observation and response records retain origins; reindexing updates output value binding without narrowing direct/preparation support. EvidenceSource equality is structural, and origin hashing includes provenance evidence. The typed executable axes remain intact.
- Native training-only requests are refused before callback; reference, assurance, and budget checks precede execution. Only Instrumented native assurance is admitted. Attempts and errors retain exposure records. A training-shaped child does not establish physical confinement of a parent operator.
- Diagnostics cap axis and plan pages, preserve axis error causes, distinguish caller/source capability absence, label explanation as capability preflight, and label complete/block content assertions as provider-reported. PlanDiff and RepairAdvice cannot accept public contradictory flags. Generic frame hashes do not prove ROI subset relations; changed frames require rebinding. Automatic ROI-shrink classification and exhaustive method-specific bind diagnosis remain outside this qualified scope.

## Acceptance and evidence limits

The source-level discriminating cases are Unknown-origin provenance mutation, changed-capability continuation reuse, and failed-score DTO reconstruction; fixes and regression source address those cases. The existing verification agent must provide exact-source JVM/JS compilation and relevant suite receipts. This reviewer ran no compiler, tests, benchmarks, or operator experiments and changed no repository source. Only this requested review artifact was written.

Canonical Alder dataset admission remains unqualified: default-pin publication/reservation and canonical consumer gates are separate. Provider declarations are not locally verified content proofs. Support metadata does not prove stochastic independence or physical reads. Immutable exposure records do not establish external freshness, prevent reuse of an old consistent snapshot, or authenticate externally supplied history. No Mote review state was written.

## SHA-256 manifest

`modules/mvpa-dataset/shared/src/main/scala/scalafim/fmri/mvpa/dataset/predictive/AlderExposureReads.scala`

`5664b50a2e0ed9c6e3263c096f13299e47c44976d7f60d28b426ac9452dfff98`

`modules/mvpa-dataset/shared/src/main/scala/scalafim/fmri/mvpa/dataset/predictive/AlderPredictiveAdmission.scala`

`c010fa95cca782caf38ade56b3da3c7f76d446e9804790a6dbf4aca66a726bc3`

`modules/mvpa-dataset/shared/src/test/scala/scalafim/fmri/mvpa/dataset/predictive/AlderExposureReadsSuite.scala`

`fed2e5b186e3675caf1596b6567e2e9877543e19ea5c30d26ab471f29276a0f0`

`modules/mvpa-fit/shared/src/main/scala/scalafim/fmri/mvpa/fit/IdentifiedReadoutRelations.scala`

`f3695ced9a9f1be74f7ccc5965746fab82e517ac009a2b549428ee198a4a5338`

`modules/mvpa-fit/shared/src/test/scala/scalafim/fmri/mvpa/fit/IdentifiedReadoutRelationsSuite.scala`

`fb01e962f24715d917a9028163326c74f27755c892ba699970947c971b55d879`

`modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/EvidenceOrigins.scala`

`32a9ca560f1d9162613483c3ffbe3f0b9cf6d3c256a002dedb8987171451923a`

`modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/Observations.scala`

`ad2bb2c1b1f38b542226579bdd139fe558ca5f400d88a811a5e1dbc158a75972`

`modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/analysis/Compilation.scala`

`c92a3ca74ef86441dda884725dc4e75413be1e940ba49660e2a7257399b3f83b`

`modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/analysis/Diagnostics.scala`

`c35318c2a5de00914d2dd1a0309b18b62cee17e05aa7e601fa38e6a4f36b6ca3`

`modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/analysis/EvidenceExposure.scala`

`254fc4f38ccb57bf9343c73222de5ca01a1513dcf7e7b13b5e3eeec2f72c03ae`

`modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/relation/Relations.scala`

`6fb65a4380074b806da627952b9568182a9c0565033d84a780b5f7df750e8ce8`

`modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/EvidenceOriginsSuite.scala`

`8d207c638fc047c1afa8499f7b4f3778e39e576e723f9743c93cf25ea260fac3`

`modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/analysis/DiagnosticsSuite.scala`

`ea4c5721a8be6f335247dd56cc7958c49d107b2297c6097dd786627e9e515873`

`modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/analysis/EvidenceExposureSuite.scala`

`dbf4b84ecb487e58913c0fe36b8a135d609499415c518d3a3378ed38a05c58c0`
