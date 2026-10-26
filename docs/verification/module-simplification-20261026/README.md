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

## C: external evidence custody and archived plans

The manifest [c-custody.json](c-custody.json) records 1,554 original files
(540,746,338 source bytes) from commit `a6abc2fb92389a584380754d73283e1a41adf386`.
They are stored in `scalafim-evidence-a6abc2fb9.tar` at
`/Volumes/My Passport for Mac/codex-work/module-simplification-20261026/`.
The Journaled HFS+ volume UUID is `51B72C66-64BB-3C5F-8853-2219CA2FFA9F`.
Archive SHA-256: `9a2029a993296cc3db235ccfbdbabbde2f5a8ab4d48e63d782246d2c4af4c95a`.

Every member matched its Git blob before packaging. A full restore verified
1,554 hashes and independent byte comparisons before removal. The archive,
source hashes and physical volume identity were rechecked immediately before
removal. See [c-restore-check.json](c-restore-check.json). No Git history was
rewritten: these bytes also remain in the source commit. This removes 1,532
bulk evidence/source snapshots from the current tree; 48 executable inputs
remain. Historical Markdown receipts and generators are retained. Their links
to externalized raw files require a restore; they are not claims that those
raw files remain in a fresh checkout.

Twenty-two completed/superseded plans moved to [the plan archive](../../plans/archive/README.md).
Old URLs retain forwarding pages and section anchors. Links were rebased,
with zero newly incorrect targets; 14 pre-existing missing historical targets
remain recorded in [c-plan-link-check.json](c-plan-link-check.json). Proposed
and active plans remain in place. Original plan bytes are also in the tar.

Restore into a **new directory**, because the current tree contains plan
forwarders that intentionally differ from the originals:

```sh
python3 tools/verification/evidence_archive.py \
  --manifest docs/verification/module-simplification-20261026/c-custody.json \
  --archive '/Volumes/My Passport for Mac/codex-work/module-simplification-20261026/scalafim-evidence-a6abc2fb9.tar' \
  --restore-root /private/tmp/scalafim-original-evidence
```

Without `--restore-root`, the tool verifies archive size/hash, exact membership
and every member hash without writing. Restore refuses different existing
content and unsafe paths. Copy only the desired raw evidence into a working
checkout afterward; do not replace the archived-plan forwarding pages.
The external directory contains the manifest, verification tool/tests, receipts
and D/A full execution logs with their exit metadata under `current-run-logs/`.

Local cleanup removed seven untracked, target-only obsolete module directories:
linalg-breeze, linalg, graph, atlas-workflows, locus-kernel, mvpa-pipeline and
locus-laws. Each had no tracked files and only target descendants; each full
tree (including hidden files, modes and symlinks) was copied to the same
external volume under `local-targets/`, fingerprinted before/after, and checked
for activity before removal. [c-local-targets.json](c-local-targets.json) records
all source/destination fingerprints. Other worktrees and their metadata remain:
missing temporary paths alone do not establish safe custody of detached commits,
and the offline EcoBrain volume's registration must remain recoverable.


The template collector completed successfully: **36 stale templates removed,
18 referenced templates retained**. Thirty-eight exact full-tree mirrors are
stored externally (two mirrors belong to templates retained because external
base links reference them). Both current and legacy pin markers from retained
bases, including readable external symlink targets, are protected. Broken or
unreadable references fail closed. The first apply refused before deletion;
that receipt is retained. A separate live child process proved open-file
visibility before the successful apply. Every deletion rechecked custody,
volume UUID, pin references and recursive activity under the base lock.

[c-template-gc.json](c-template-gc.json) records all removals/retentions and
postconditions; [c-template-custody.json](c-template-custody.json) binds every
mirror. The restore helper and instructions also live beside the external
copies. The collector's 78 executed unit tests passed; one environment-specific
live-observation test skipped in its sandbox, with a successful separate-child
positive control in the actual apply context. Eight archive restoration tests
passed. The source checkout's historical Git objects and other worktrees were
retained; this operation does not claim physical disk savings from logical sizes.
