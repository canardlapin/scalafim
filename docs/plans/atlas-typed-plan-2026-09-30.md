# Typed atlas implementation plan

Tag: `atlas-typed-20260930`. Mote epic: `bd-01M3RY746DMGYQJE4SM14YQJPQ`.

The [atlas review](atlas-review-2026-09-30.md) identifies six reproduced defects and the computational gaps against neuroatlas. This plan keeps the exact `AtlasRealization` assignment and locus4s field model as the center. Visualization is excluded. Mote is authoritative for task status, dependencies, claims and acceptance; this document records the design and execution order.

| Order | Mote issue | Deliverable | Prerequisites |
| --- | --- | --- | --- |
| 1 | `bd-01M3RY7VTJYEA5G7AH06FZX5D9` | Canonical parcel identity independent of source integer labels; hemisphere-safe Glasser mapping | None |
| 2 | `bd-01M3RY7X4P9VQRD10BFRENN0FV` | Safe constructors and updates, consistent queries/metadata, duplicate-value rejection, selection derivation | 1 |
| 3 | `bd-01M3RY7YHQ6J71SRJAC8Y092CV` | Authoritative `Field[P,A]` results and checked keyed alignment | 1, 2 |
| 4 | `bd-01M3RY7ZZW811AAS8DWR9CVDTQ` | Continuous image expansion and identity-preserving metric persistence | 3 |
| 5 | `bd-01M3RY81BSEPB2F8Y43GK8XZF8` | Exact-grid composition, typed grouping/weighting, assignment selection and dilation adapters | 3 |
| 6 | `bd-01M3RY82QRX5J6G04V7S4Y4TVF` | Pinned FSL XML/Harvard–Oxford/Julich, HCPex, subcortical and MTL loading families | 1, 2 |
| 7 | `bd-01M3RY846W0RP9G6S5B2RCTQQV` | Standard surface acquisition, selection and reduction | 3 |
| 8 | `bd-01M3RY85HRGZA151F2Y73S6PPZ` | Thin connectivity, named batch and transport workflow adapters | 3 |

Steps 4–8 can proceed independently after their prerequisites. Step 6 must split into per-family issues before implementation so asset/parser evidence stays bounded. Spin inference and general soft atlas membership require separate scientific designs when pursued; neither is disguised as a routine R port.

All eight slices and their composition/loading children are complete as of
2026-10-03. The final native MDTB10 loading blocker is resolved by the hosted
image4s signed-INT8 codec revision. See the [epic completion receipt](../verification/atlas-epic-2026-10-03.md)
for exact pins, source-backed qualification, JVM/JS gates and remaining scope
boundaries. The final codec pin, regression and completion documentation are
recorded in a local commit; earlier ScalaFIM implementation changes remain
uncommitted. The upstream codec is published on a dedicated branch with a draft PR.

The execution notes below are historical. Their next-step and open-status
statements describe each slice at the time of its original handoff.

Step 5 is split into independently accepted children under the same tag:

| Child | Mote issue | Scope |
| --- | --- | --- |
| 5a | `bd-01M3VT7FX7GMNE0774VT0VYNQE` | Exact-grid two-parent composition |
| 5b | `bd-01M3VT7SD4EZX9MG356225G0C7` | Typed hemisphere and partial-network grouping, explicit weighting |
| 5c | `bd-01M3VT7XJPZ9HNG6FYQ86YQQ2Y` | Thin image-layer dilation adapter |

Composition uses the authoritative assignments, exact grid congruence and
identical declared template/coordinate spaces. Overlap and fully occluded
parcels have separate explicit policies. New IDs follow canonical parent order;
display order follows the parents. The namespace binds ordered canonical parent
keys, retained ancestry and policies. Canonical Glasser identity is independent
of numeric source encoding; conservative source-local parent keys remain local.
Network names are scoped by parent; matching text does not assert cross-atlas
equivalence. The result retains the first frame and spatial owner, with checked
partial parcel remaps to its fresh parcel owner. These remaps express
correspondence, not unchanged spatial fibers after overlap.

Both parent provenance records remain separately scoped, including original
metadata/display order, exact support evidence and assignment digest references.
Publication and metric admission validate ancestry and the nested evidence.
Digest references do not verify omitted assignment bytes. Evidence for 5a is
`/private/tmp/scalafim-atlas-compose-20261001/`. Selection already composes a
certified partial parcel map and records parent identity; it needs no second
implementation. Step 5 remains open while grouping/dilation are outstanding.

## First slice: identity contract

Source integer labels identify an encoding, not anatomy. Glasser volume and surface ID 1 can identify opposite hemispheres. The canonical key uses the complete HCP-MMP name (`L_V1_ROI`, `R_V1_ROI`) and checks any supplied hemisphere against it. A typed identity policy makes the choice inspectable:

- Source-local labels are the conservative default. Their identity includes the source/representation context and metadata, so unknown volume and surface encodings do not accidentally align.
- Explicitly shared region IDs declare that the numeric encoding is common across representations, as in Schaefer. They use the same current key format as the other policies.
- Glasser canonical labels use a versioned key namespace. Canonical domain order is by full label; source IDs and original display order remain annotations and converters. Renumbering/reordering source labels must not reorder aligned anatomical values.

The user confirmed there are no dependent users requiring legacy compatibility. Replace the numeric key contract outright; retain no compatibility mode or migration API. The fixed foreign-producer fixture is independently regenerated for the current keys. Different or unknown domains require an explicit checked locus4s bijection.

Acceptance includes opposite hemispheres, renumbered and reordered labels, malformed/missing/duplicate canonical keys, conflicting hemisphere metadata, exact publication source-label maps, legitimate Schaefer identity, independent foreign-producer bytes for the current format, and full atlas JVM/JS suites. Independent neuroatlas hemisphere fixtures anchor the anatomical mapping; no external download is needed for the synthetic regression court.

## Verification and handoff

Each slice requires relevant shared tests on JVM and Scala.js, meaningful boundary failures, compiled affected examples, and warning-clean compilation. R parity evidence is required where scientific semantics must match. Asset-backed execution and synthetic parser tests are reported separately. Preserve unrelated checkout edits and reserve exact paths in Mote before editing.

Evidence and local dashboard for this slice: `/private/tmp/scalafim-atlas-typed-20260930/`. The review evidence remains in `/private/tmp/scalafim-atlas-review-20260930/`.

## Fourth slice: expansion and metric persistence

Task `bd-01M3RY7ZZW811AAS8DWR9CVDTQ` extends the generic image parcellation
renderer with continuous values and an explicit background. Its stable scalar
sample space retains the exact grid and frame. Categorical rendering shares the
assignment scatter kernel. Atlas adds a thin precise-owner adapter; series
expansion continues through the existing `ParcelSeries.toDense` operation.

Metric V1 stores one Float64 scalar per canonical parcel, with a checked measure
schema, canonical ordered domain identity, complete originating atlas provenance,
metadata/display order, and compact support/assignment fingerprint references.
It reuses the existing atlas key codec to bind originating identity policy,
family/model/release, parcel variant and source encoding to the stored keys.
Restoration validates that origin independently and then requires the target's
exact canonical ordered parcel identity. Different volume/surface support,
source IDs and presentation order are permitted when anatomy is identical.
The origin remains evidence of the saved measure's computation, not evidence of
recomputation on the target support.

The JSON envelope hashes its exact UTF-8 payload string. A retained external
digest can detect replacement; a self-contained digest is not an authorship
signature. IEEE Float64 hexadecimal values preserve signed zero, infinities,
subnormals and finite extremes. NaNs use one canonical quiet-NaN encoding.
Shared computation has no filesystem dependency; JVM file writes create a new
UTF-8 file and refuse replacement. No alternate field algebra or legacy facade
is introduced.

Acceptance includes independent coordinate-keyed R expansion fixtures, actual
JVM/Scala.js producer documents consumed on both platforms, origin namespace
tampering, key order/count/schema/IEEE failures, live foreign owner rejection,
precise return annotations, snapshot behavior and unchanged categorical labels.
Run complete image and atlas suites on both platforms, affected examples and
consumers, and warning-clean compilation. Evidence is in
`/private/tmp/scalafim-atlas-metrics-20260930/`.

## First slice result

Canonical Glasser identity is implemented through `ParcelIdentity` and checked opaque `GlasserParcelKey`. Domain order is independent of Glasser source numbering; assignment, metadata and network fields follow that order, while display order and source integer labels remain intact. Surface membership checks the canonical key's hemisphere. The loader normalizes recognized source hemisphere aliases and rejects ambiguous names. All policies use one current key format; the former format is removed.

Validation: atlas JVM 127/127, atlas Scala.js 89/89, atlas examples 5/5, and repository-wide `scalafimCompileAll` without compiler warnings. The atlas-to-MVPA workflow passed 2/2 in isolation on the merged checkout (`atlas-workflow-retry.log`). One earlier rerun failed during sbt cached build-definition loading before tests; the retry passed without cache deletion or source changes. The independent R key-alignment fixture passed, and the fixed publication fixture was regenerated independently with Python `struct`/`hashlib`.

The broader workflow project is not fully green: `ProfileHrfConditionWorkflowSuite` returned `BudgetExceeded` for all 12 voxels. That fitting example does not use atlas code and was left unchanged. An attempted full R reference loader test process aborted; the successful R evidence covers key alignment, not downloaded asset or loader qualification.

Evidence: `/private/tmp/scalafim-atlas-typed-20260930/`, including complete raw logs and exit metadata, the Python-produced byte fixture, and SHA-256 hashes for 53 verification inputs. Work is uncommitted. At this slice's handoff, the next Mote slice was constructor/API invariant repair (`bd-01M3RY7X4P9VQRD10BFRENN0FV`); the remaining seven roadmap children were open.

The atlas suites and full compilation were run on the pre-merge build at `7989608d32ed8283ed11f5ff3df54c7c7761a23e`. Another workstream merged the checkout to `9c656bc79254be9142a8b605d0d4e404b410bd1b` during verification. All 53 atlas verification inputs remained unchanged, and the direct dependency module changes were limited to a transform test log fixture. The isolated atlas-to-MVPA workflow passed on that merged checkout. The full compilation result is evidence for the pre-merge build, not a rerun of the expanded build.

## Second slice: API invariants

Task `bd-01M3RY7X4P9VQRD10BFRENN0FV` is implemented and verified. Replace
phantom-tagged case classes with sealed representation-specific references and
checked classified spaces. Metadata updates cannot change representation or
spaces. Store checked `RegionLabel` and `RegionAttributes` directly; raw string
entry points share one validation path. Admit detached parcel tables through
`ParcelValues.from`, rejecting duplicates, unknown/missing IDs and metadata
mismatches, with deterministic display ordering.

Queries default to the atlas native coordinate space. Selection composes a
certified partial parcel map with the parent assignment and retains the exact
spatial owner. Record parent parcel/support identities and assignment digests,
plus canonical kept/dropped keys; preserve the release identity and reject
empty selection through a typed error. Single-side surface operations accept
`CorticalHemisphere` exclusively. Update actual JVM loaders, examples and
MVPA consumers; retain no phantom constructor/copy compatibility layer.

Acceptance: counterexample regressions and compile-time rejection tests,
complete atlas suites on JVM and JS, affected MVPA suites on both platforms,
atlas examples and the atlas-to-MVPA workflow. Full logs and source hashes live
in `/private/tmp/scalafim-atlas-invariants-20260930/`. No publication is included.

## Second slice result

Sealed representation-specific references and checked classified spaces replace
the forgeable phantom records. `withDetails` preserves the concrete reference
type, representation and spaces. Realization traits are also sealed so external
implementations cannot bypass admission. Region metadata stores checked labels
and attributes; detached parcel values reject duplicate, unknown, missing and
inconsistent records before indexing. Queries use native coordinates by default.
Selection composes a certified partial parcel map, preserves the exact spatial
owner, rejects empty output, and records parent identities/assignment digests
and canonical kept/dropped keys. Single-side surface operations require
`CorticalHemisphere`. Surface admission and publication preserve separate
template and coordinate spaces. Obsolete constructors, copy surfaces and phantom
aliases have no compatibility adapters.

Independent read-only review identified two additional holes: externally
implementable realizations and a surface template/coordinate declaration
mismatch. Both reproduced as failing tests before repair, then passed in the
complete suites. The follow-up review found both closed at source level;
execution evidence comes from the parent verification runs.

| Check | Result | Raw log |
| --- | --- | --- |
| Atlas JVM | 138/138 | `atlas-jvm-final.log` |
| Atlas Scala.js | 100/100 | `atlas-js-and-compile-final.log` |
| MVPA spatial JVM | 16/16 | `consumer-jvm-isolated.log` |
| MVPA spatial Scala.js | 15/15 | `atlas-js-and-compile-final.log` |
| Atlas examples | 5/5 | `consumer-jvm-isolated.log` |
| Atlas-to-MVPA workflow | 2/2 | `consumer-jvm-isolated.log` |
| `scalafimCompileAll` | Passed without compiler warnings | `atlas-js-and-compile-final.log` |

The combined final JVM command exited 1 after the atlas suite passed, because
the subsequent consumer suite exposed an existing fixture that expected a fresh
grid to share exact ownership and an unchanged timing receipt exceeded its
30-second timeout. The fixture now compares the supplied grid owner and also
asserts rejection of an independent owner. Both consumer checks passed in the
fresh process; no timing threshold changed. Two fresh attempts failed during
cached dependency build-definition loading before compilation. Exact pinned
sources were cloned into this session's isolated staging cache; shared staging
was untouched by that repair.

Evidence includes full raw logs and exit metadata, a review receipt, exact
dependency pins, and SHA-256 hashes for 67 relevant verification inputs in
`/private/tmp/scalafim-atlas-invariants-20260930/`. The final build ran at
`43616621a23c7c7147fd32b22a8fecbb85a3f03e`; all recorded inputs remained unchanged
after the consumer fixture correction. Only README/plan prose changed afterward.
The full repository test suite and broader workflow suites were not rerun.
Changes are uncommitted. The next slice is authoritative `Field[P,A]` results
and checked keyed alignment (`bd-01M3RY7YHQ6J71SRJAC8Y092CV`).

## Third slice: authoritative parcel fields

Task `bd-01M3RY7YHQ6J71SRJAC8Y092CV` is implemented and verified. Remove `ParcelValues` and
`ParcelData`; use locus4s `Field[P,A]` as the only authoritative scalar value
container. Derive `ParcelRecord` display rows from realization metadata and
selection order. Return precise realization frame/spatial/parcel types from
series reduction. No compatibility facade or second field algebra is introduced.

Detached imports reject duplicates before any dropping or indexing. Canonical
namespaced keys and checked anatomical Glasser keys have explicit unknown/missing
policies. Numeric source IDs require the actual source realization's complete
ID-to-key mapping: equal canonical domain identity does not prove equal numeric
encodings. Explicit field transport uses locus4s persistent ordered alignment.

A typed `ParcelReducer` replaces function-reference dispatch. Scalar, series and
portable support-field reduction share one policy kernel for masks, skipped,
propagated or rejected NaNs, and independently empty support. Custom callbacks
receive fresh arrays; outputs are fully evaluated and callback failures typed.

Acceptance includes independent R-generated key alignment cases, foreign and
reordered-domain failures, positive precise-return annotations, static foreign
owner rejection, scalar/one-timepoint policy parity, all-missing and empty-support
separation, callback array retention and input snapshot behavior. Both platforms,
affected consumers/examples and full compilation are required. Evidence is in
`/private/tmp/scalafim-atlas-fields-20260930/`; no publication is included.

## Third slice result

`ParcelFields` provides checked keyed admission and metadata-derived display rows
on authoritative locus4s `Field[P,A]` values. Source ID imports resolve through
the declared source realization's actual mapping, covering opposite Glasser
hemisphere encodings and arbitrary renumbering. Explicit persistent alignment
rejects equal-sized foreign or reordered identities. `ParcelValues`, `ParcelData`
and function-reference reducer dispatch are removed, with no compatibility layer.

Scalar and series results retain exact realization ownership. One policy kernel
handles masks, skipped/propagated/rejected NaNs, independently empty support, and
typed callback failures. Custom arrays are independent per parcel/sample; results
snapshot inputs and callbacks run only during reduction. Portable support-field
reduction also works on admitted surface realizations. The existing example row
presentation stays intact and derives from authoritative metadata.

| Check | Result | Raw log |
| --- | --- | --- |
| Atlas JVM | 154/154 | `atlas-jvm-final.log` |
| Atlas Scala.js | 116/116 | `atlas-js-consumer-final.log` |
| MVPA spatial JVM | 16/16 | `consumer-jvm-final.log` |
| MVPA spatial Scala.js | 15/15 | `atlas-js-consumer-final.log` |
| Atlas examples | 5/5 | `consumer-jvm-final.log` |
| Atlas-to-MVPA workflow | 2/2 | `consumer-jvm-final.log` |
| `scalafimCompileAll` | Passed without compiler warnings | `compile-all-final.log` |
| Independent R alignment | Six cases and unqualified cross-convention ID rejection passed | `r-alignment.log` |

The full JVM suite executed the available cached TemplateFlow checks, including
the numerical inverse. The R fixture pins neuroatlas `d65ff97` and actual
`R/parcel_data.R`, generator, TSV and portable Scala fixture SHA-256 hashes. It
qualifies synthetic alignment semantics only; no atlas assets were acquired.
Read-only source review found no consequential hole and ran no tests. Fray's
consumer scan confirmed no external dependency on the removed tables; the actual
consumer/example acceptance comes from the parent test runs.

The first focused test compile rejected widened realization aliases. Singleton
aliases fixed those call sites without casts or weaker result types; the focused
retry passed 68/68. Final acceptance includes an additional independent NaN-policy
expectation test. All final commands exited zero. Full compilation waited for a
shared Ivy cache lock and completed without disturbing another session.

Evidence is `/private/tmp/scalafim-atlas-fields-20260930/`: `acceptance.json`,
raw logs/exit metadata, the read-only review receipt, 14 clean pinned dependency
clones, 91 verified unchanged inputs, and Fray snapshot
`manifest:87aed53ac15fff372f417f4e8be4e9c73a772a42d0242bc947a6ffda919b2c68`.
Final checks ran at `79b8b7d5e7e9b52890b395bdd37674e1f068937c`; the head and
recorded inputs remained unchanged. Only plan prose changed afterward. Changes
are uncommitted. Full repository tests and unrelated workflow suites were not
rerun. Next: typed continuous image expansion and metric persistence,
`bd-01M3RY7ZZW811AAS8DWR9CVDTQ`.

## Fourth slice result

Task `bd-01M3RY7ZZW811AAS8DWR9CVDTQ` is implemented and verified. Generic
`VolumeParcellation.renderContinuous` and thin `AtlasExpand.volume` retain the
precise scalar sample owner over the original grid. Categorical rendering shares
the same assignment scatter without changing labels. Background is explicit.

`ParcelMetricJson` stores checked scalar Float64 measure schemas on canonical
ordered parcel domains. Decoding validates the original namespace and evidence
before creating a strict target-owned locus4s field. A restored metric can be
re-saved while preserving original provenance, metadata, presentation order and
support/assignment references. Checked JVM UTF-8 IO refuses replacement.
Generic field math remains in locus4s; no legacy value container returns.

Source review found an unsorted-ID/display-order confusion, accepted known
space-kind contradictions, and accepted impossible background declarations.
The three space/background counterexamples reproduced together as false
rejection flags before repair. Corrected admission and regression tests pass
on both platforms. The review also prompted the preserving re-save overload.
The later parent changes bound volume-size multiplication to Int capacity and
add a compile-time foreign-owner regression; the review receipt distinguishes
those changes from the exact independently reviewed hashes.

| Check | Result | Raw log |
| --- | --- | --- |
| Image JVM | 385/385 | `image-atlas-jvm-final.log` |
| Image Scala.js | 356/356 | `image-atlas-js-final.log` |
| Atlas JVM | 167/167 | `image-atlas-jvm-final.log` |
| Atlas Scala.js | 128/128 | `image-atlas-js-final.log` |
| MVPA spatial JVM / JS | 16/16 and 15/15 | `consumer-jvm-final.log`, `image-atlas-js-final.log` |
| Atlas examples / atlas-to-MVPA workflow | 6/6 and 2/2 | `consumer-jvm-final.log` |
| `scalafimCompileAll` | Passed without compiler warnings | `compile-all-final.log` |
| Independent R expansion | Three coordinate-keyed cases passed | `r-expansion.log` |
| Actual JVM/JS metric interchange | Both producer documents consumed on both platforms; Python UTF-8 SHA-256 check passed | `consumer-update-refresh.log`, `js-producer.log`, final atlas logs |

The initial example failed because its cached transitive classpath omitted the
new uPickle jars, including in a fresh sbt process. Explicit
`atlasExamplesJVM/update` refreshed that resolution; the example and final
consumer checks passed with no class-loader configuration change. The full JVM
atlas suite also ran the available cached TemplateFlow numerical checks.

Evidence is `/private/tmp/scalafim-atlas-metrics-20260930/`: `acceptance.json`,
raw logs and actual exit metadata, source-review and interop receipts, 14 clean
pinned dependency clones, and hashes for 226 relevant verification inputs. All
recorded computational inputs remained unchanged; README/plan prose changed
afterward. Checks started at `adaac06a095a8cdfd0b297b58caff879c4bee032`.
Another workstream advanced the checkout to
`8e6eb4d19360788c1aa0f71b84b46b35694e383d` during final compilation, adding
transform qualification tests, fixtures and documentation without production
or build changes. The complete commit delta is recorded separately.

Metric V1 covers scalar Float64 measures. Fingerprint references identify
omitted assets but do not verify their bytes, and digests do not authenticate
authorship. The R fixture records the reference commit and actual source hashes
for synthetic finite/missing expansion; it does not qualify acquired assets or
match neuroatlas's infinity-rejection policy. Full repository tests and unrelated
workflow suites were not rerun. Changes remain uncommitted. Next is step 5,
`bd-01M3RY81BSEPB2F8Y43GK8XZF8`: exact-grid composition before grouping and
dilation adapters.

## Fifth slice, composition result

Child `bd-01M3VT7FX7GMNE0774VT0VYNQE` implements `AtlasCompose.volume` from
authoritative assignments. The result retains the first frame/spatial owner and
returns checked parent-to-child partial remaps. Explicit policies govern overlap
and fully occluded parcels. Source-ID collisions and large IDs use fresh output
IDs; canonical Glasser ancestry survives renumbering and display changes.
Retained parent keys prevent an occlusion-induced namespace collision. Network
names stay scoped by parent, with original annotations retained in provenance.

Both complete parent records, exact small grid records, canonical correspondence
and assignment digest references survive publication and metric persistence.
Nested parents reject impossible hard-label declarations and foreign supports;
selection history must account for the resulting inventory and order. The three
nested-origin counterexamples reproduced as accepted before repair. A canonical
metric/display-order test also failed before the validator was corrected to use
canonical metadata. Read-only source review found no remaining consequential
blocker in its stated bounds; all executable evidence comes from parent runs.

| Check | Result | Raw log |
| --- | --- | --- |
| Atlas JVM / Scala.js | 180/180 and 141/141 | `atlas-jvm-final.log`, `atlas-js-consumer-final.log` |
| MVPA spatial JVM / Scala.js | 16/16 and 15/15 | `consumer-compile-final.log`, `atlas-js-consumer-final.log` |
| Atlas examples / atlas-to-MVPA workflow | 6/6 and 2/2 | `consumer-compile-final.log` |
| `scalafimCompileAll` | Passed without compiler warnings | `consumer-compile-final.log` |
| Actual neuroatlas merge | Three coordinate-keyed ancestry cases passed | `r-composition-01.log` |

Evidence is `/private/tmp/scalafim-atlas-compose-20261001/`, including actual exit
metadata, `acceptance.json`, reproduction logs, source review hashes, 14 clean
pinned dependency clones and 232 unchanged verification inputs. Checks ran at
`debdf34b821e6bf34103dc596dc815f55509b541`; this plan's result prose changed
afterward. During handoff, another workstream advanced HEAD through `dadf5973`
to `966502b7`, changing documentation and transform guide tests without production
or build changes. The delta is recorded separately; all 232 atlas verification
inputs remained unchanged. The JVM suite executed the available cached TemplateFlow checks.
The R oracle pins neuroatlas `d65ff97` and actual source/generator/fixture hashes.
It qualifies synthetic second-parent precedence and parcel ancestry. R retains
occluded inventory; Scala explicitly rejects or drops it. No acquired atlas asset
or other overlap-policy parity is claimed. Assignment digests identify omitted
payloads without independently verifying their bytes or overlap execution.
Full repository tests and unrelated workflow suites were not rerun. Changes are
uncommitted. Next: grouping child `bd-01M3VT7SD4EZX9MG356225G0C7`; dilation
child `bd-01M3VT7XJPZ9HNG6FYQ86YQQ2Y` also remains open.
