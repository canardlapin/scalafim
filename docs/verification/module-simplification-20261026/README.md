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
