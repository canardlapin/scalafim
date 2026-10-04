# Independent source review: BLOCK

Source: `f0166b5649fb567cda5a33e547d950e0cee1c862`, clean at inspection.
Repository: `/private/tmp/scalafim-umvpa-20260930-canonical-admission`.
Review scope: EvidenceOrigins, Observations/MultiResponse identity and records, relation/readout support, EvidenceExposure, AlderExposureReads/native root identity, and minimum truthful M1.05 diagnostics acceptance. No repository edits, builds, tests, publication, or Mote state writes were performed by this reviewer. This report is the only newly written artifact.

## Blocking finding

`Diagnostics.scala:148` declares `final class PlanDiff private (...)`; `Diagnostics.diff` at line 319 constructs it outside the class or companion. There is no PlanDiff companion. The parent independently confirmed the source defect. Restore authorized construction through `private[analysis]` or a companion factory while keeping `rebindRequired` derived. Acceptance: the resulting exact source compiles on JVM and JS; public callers still cannot supply inconsistent rebind flags.

Additional compile concern, not a claimed compiler result: eight Observations/MultiResponse signatures default origins from source, valueIdentity, and samples in the same parameter list. Have the existing verifier check default-getter scope; overloads delegating to explicit-origins variants can preserve existing call sites if rejected. No compiler execution was performed here.

## Previous three scientific counterexamples: repaired in source

1. Unknown-origin provenance-only mutation: `EvidenceIdentity.writeFramed` independently frames source, every provenance node operation/parents/evidence, roots, structured value identity, and origins. Alder exposure association and native root identity now share this complete framer. The new native regression constructs identical values/IDs with different Domain provenance evidence under explicit Unknown origins, checks changed exposure/root identity, and checks foreign-reference rejection before either operator executes. A separate fixed-versus-joint preparation test checks both identities.
2. Capability-context continuation reuse: both metadata and explanation paths calculate a framed context fingerprint of captured source capabilities and caller availability, checking it together with PlanId. The changed-availability and same-PlanId changed-source tests exercise metadata tokens. The equivalent explanation path shares this admission logic, but the exact three-capability explanation counterexample is not yet directly tested; that is a coverage limitation, not an observed remaining source failure.
3. Failed score access: overlapping DerivedScore attempts now deny untouched confirmation regardless of callback outcome. Tests cover both Left and thrown failure plus DTO reconstruction. The classification is conservative: hasObservedScores/DependsOnObservedScores also represents possible exposure after failed attempts, not proof that a score was actually observed.

## Other reviewed boundaries

Record persistence and reindexed reconstruction retain origins and update output value binding while preserving upstream support. Source equality is structural. Origin framing includes provenance evidence. Native training-only requests remain refused before callback; native reads accept only Instrumented assurance, check actual metadata reference and budget before entering the callback, and preserve failed-attempt exposure. No executable nominal-axis erasure was found in these boundaries.

M1.05 has capped axis/plan pages, typed context-bound continuations, retained axis error causes, caller-versus-source capability facts, capability-preflight wording rather than a binding-success guarantee, provider-reported content identity names, and controlled/derived repair flags. Generic frame hashes do not classify ROI subset relations. Automatic ROI-shrink classification and complete method-specific bind diagnosis are not qualified by this review.

## Evidence limits

This is source review, not a JVM/JS test receipt. The constructor blocker prevents overall acceptance at this SHA. Canonical Alder dataset admission remains unclaimed; its default pin is separately blocked by a foreign reservation. Declared support and provider reports do not prove payload content, stochastic independence, or physical read confinement. Immutable exposure snapshots detect mismatched supplied snapshots/permits, not external-store freshness or reuse of an older consistent snapshot. Provenance reference metadata is not an authenticity certificate.

## SHA-256 source/test manifest

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

`63f8928c28bdf89c0c773c05d9318dabc0368dccf4ed64ea4daf478b0a304377`

`modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/analysis/Compilation.scala`

`c92a3ca74ef86441dda884725dc4e75413be1e940ba49660e2a7257399b3f83b`

`modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/analysis/Diagnostics.scala`

`269598ce88023ce5becd94431d4a97549529d375464657b9e61c99577bdfa82d`

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
