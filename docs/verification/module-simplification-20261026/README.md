# Remaining module simplification — 2026-10-26

## D: MVPA integration consolidation

Base: `c98148fab2dbcb168b99019ec01cff86f56eb1e5`. The 27 source/test files from
mvpa-fit and mvpa-dataset moved byte-for-byte into mvpa-fmri. Their packages,
scientific fixtures, persisted identities and assertion bodies are unchanged.
The unused mvpa-pipeline interpreter and its dedicated tests are removed.
Source-bearing modules: 34 → 32. Current build targets, bounded CI, README,
module map and workflow guide use mvpaFmriJVM/mvpaFmriJS.

The manifest [d-moves.json](d-moves.json) binds both old and new paths to exact
SHA-256s. All were checked against Git base bytes and independently reviewed.
The merged project retains both compile/test closures, including Multivar test
fixtures and group/artifact test edges. No source consumer of the deleted
interpreter remains; the maintained PLS Neuro source/build inventory has no old
project target references.

Verification: owning suites 55 JVM/55 Scala.js; workflow consumers 8 JVM/6 JS,
zero failures/skips. scalafimCompileAll and examplesCompile pass on both
platforms. Resolved boundary gate: 24 compile closures, zero errors. Inventory:
85 targets/37 bounded batches, eight inventory unit tests; 38 scenario manifest
entries and executable first-level guide validation pass. No Scala source
warnings; upstream sbt unused-setting messages remain. Full raw logs and exit
metadata are identified by [d-checks.json](d-checks.json); temporary paths are
not durable external custody.

Independent source/build/move review found no blockers. These are local
qualification results, not hosted CI, native display acceptance or publication.
Other Phase D decisions and the unfinished PHRF study remain outside this
user-authorized merge/delete slice. Stages A, C and E follow in the same goal.

## A: remaining hygiene

Removed the unused private boxed Complex implementation, Library.hrfSet alias,
Residualize.residualizeByName and its private name lookup, and unused research
Fs.moveAtomic/CpuMeter.childSeconds helpers. FitPlanExecutor now calls the
existing package-private legacyDatasetReader; return type, reader admission and
error mapping are identical. No numerical kernel, atomic write, CPU accounting,
public failure policy or type visibility was weakened.

The current owner audit finds no @deprecated definitions or PrimitiveBuffers
in ScalaFIM; earlier image/spatial findings do not authorize deleting APIs now
owned by extracted libraries. Original unused dependencies/edges and stale build
checks were already removed. [a-public-audit.json](a-public-audit.json) records
93 lexical singleton candidates: 6 removed, 80 retained public scientific or
compile-contract surfaces, 7 deferred to the separately authorized typed bridge
refactor. Absence of a lexical caller is not proof of a dispensable public API.
The selected removals were independently searched across source, tests, docs and
the maintained PLS Neuro consumer. Unknown third-party binary compatibility is
not claimed for the two removed public convenience methods; their underlying
composition APIs remain.

Full affected suites pass: JVM HRF412/design588/fit869/research544; Scala.js
HRF412/design588/fit811/research254: 4,478 passes, four existing JVM skips.
No Scala source warnings or new failures. Independent review found no blockers.
Raw log and actual exit metadata: [a-checks.json](a-checks.json). No new scientific
or performance acceptance is inferred from these regression checks.
