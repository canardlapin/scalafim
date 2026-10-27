# Remaining Phase D — 2026-10-27

Base: `47d643d5709c9d2fa877ca3e9d13489928b6de6b` (merged PR #69).
Branch: `simplify/phase-d-20261027`.

Outcome: finish the remaining Phase D decisions and remove the group/dataset
coupling without changing participant values, ordering, scientific fixtures,
dataset admission/errors or published identity strings. Scope is subject ID
ownership and dependency boundaries; PHRF remains pending its study, and no
numerical algorithm or provider pin changes.

## Current ownership and decisions

The domain migration was already delivered in `0f3a57624`: no Scala source
references DomainFactory or unsafeRestore, no local locus-data sources or build
project remain. Connectivity, latent, threshold and dataset call native locus4s
registry restoration. The U6 ownership ledger records the transferred laws.
Historical plan/receipt references and the external provider classpath label
"locus-data" do not represent a surviving ScalaFIM facade.

Pinned neuroim4s `4f75c2bf141449d032f9fe3c558ef13a22a4d59f` and dataset had the
same subject grammar: String.trim and nonempty. Dataset now provides a transparent
type alias and delegates validation to neuroim4s, retaining make/apply/value,
DatasetError.InvalidLabel fields and the throwing constructor message. Group and
MVPA/group use the shared provider type, so group no longer depends on dataset.
Other dataset ID types remain separate. No unchecked cast or new generic ID
implementation is introduced. The existing joined participant key and MVPA
invalid-label diagnostic are retained.

Retain pipeline as an optional graph/execution boundary with actual fit workflow
contracts; fit uses it only in Test scope. Retain the already-reduced mvpa-spatial
MVPA/threshold integration: moving it into either lower owner would impose the
other's dependency. It has one production policy and test-scoped imaging adapters.
No candidate strong-FWER/FDR refusal or scientific admission is relaxed.

## Acceptance

Focused identity cases cover literal normalized Unicode/composite labels, all
33 ASCII trim characters, empty-label error fields/message, bidirectional shared
type assignment, ordering and normalized duplicates. Full owning/consumer suites
must execute on JVM and Scala.js. Final all-module/examples compile and resolved
group/mvpa-group classpaths must prove no dataset production dependency.

All checks completed successfully. The full owning/boundary and consumer gates
executed **2,387 passes**, zero failures and zero skips:

| Module | JVM | Scala.js |
|---|---:|---:|
| dataset | 95 | 72 |
| group | 107 | 106 |
| mvpa-group | 18 | 18 |
| pipeline | 38 | 38 |
| mvpa-spatial | 12 | 12 |
| fit | 874 | 816 |
| fmri-workflow | 82 | 63 |
| dataset-zarr | 15 | 7 |
| workflow examples | 8 | 6 |

The earlier focused 12 passes overlap these full suites and are not added.
`scalafimCompileAll` and `examplesCompile` pass on both platforms. The architecture
gate checked 559 production files and 24 compile closures with zero errors.
Six resolved production [classpath witnesses](classpaths.json) establish no
dataset dependency in group or mvpa-group on either platform, and no pipeline
production dependency in fit. Test inventory: 85 supported targets in 37 batches;
scenario manifest: 38 entries. No Scala source warnings; inherited upstream sbt
unused-setting messages remain.

The independent read-only review gave qualified approval with no consequential
type/API findings. This establishes source compatibility for the inspected API;
no binary compatibility, new native or new scientific acceptance is claimed.
All 15 modified existing test files retain exact assertions and numeric inputs;
only imports and checked subject construction differ. New tests add identity
contracts. No fixture, tolerance, admission or provider pin is changed.

[Checks](checks.json), [source manifest](source-manifest.json),
[test preservation](test-preservation.json) and [review](review.json) bind the
actual local source and logs. Raw logs and metadata are in `/private/tmp/` at
the paths and SHA-256s recorded in checks.json; they are local evidence, not a
claim of durable external custody. The owning sbt server was shut down after the
completed gates.
Local regression, hosted CI, publication, native and scientific acceptance are
separate claims. Nothing has been pushed or merged for this phase.

The staged whitespace gate found extra EOF blank lines in the two new test files.
They were removed; both affected owning modules were rerun on JVM and Scala.js,
with 380 passes and zero failures/skips (overlapping the table above). Production
code and assertions stayed unchanged. `gated-source-manifest.json` preserves the
pre-format hashes; `source-manifest.json` binds the final source.
