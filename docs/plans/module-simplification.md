# ScalaFIM module simplification

Current status (2026-10-26): the authorized MVPA integration slice and remaining
A, C and E work are complete locally. See the completion checkpoint and
[verification receipts](../verification/module-simplification-20261026/README.md).

Status: accepted with decisions (2026-10-19). Companion to `ecosystem-unification.md`, which
moves imaging, surface, atlas and MVPA code into provider libraries. This
plan covers what remains in ScalaFIM: boundaries that do not pay for
themselves, legacy and dead code, build residue, and repository weight.

## 1. Review findings

**Granularity.** After the eco epic, 34 of today's 51 modules remain. Most
boundaries are justified: each keeps a dependency category away from
consumers that do not need it, and every module is cross-built for JVM and
Scala.js, which multiplies the cost of an unneeded dependency. Five boundaries
do not pay for themselves:

| Boundary | Problem |
|---|---|
| `model` / `fit` | Same dependency closure; every `model` consumer also uses `fit`. |
| `hrf-laws` | 409 lines with a single consumer, itself an unpublished test court. |
| `fit-estimates` | 332 lines with no consumers; `fmri-workflow` already has its dependencies. |
| `mvpa-fit` / `mvpa-dataset` | `mvpa-fit`'s only consumer is `mvpa-dataset`. |
| `mvpa-pipeline` | 37 lines with no consumers; its purpose (keeping `pipeline` out of mvpa4s core) is already met. |

One boundary is missing: `design` carries Intaglio graphics in its main
classpath for four plotting files (423 lines) that only its own tests use,
so every analysis module from `model` to `mvpa-*` resolves the Intaglio build.

**Wrong or redundant edges.** `mvpa-spatial → atlas` (no import uses it),
`mvpa-spatial → mvpa-dataset % test` (only reaches transitive libraries),
`mvpa-artifacts → estimates-io` (it uses `estimates` and `archive.io`), and
redundant edges in `phrf-comparison`, `fmri-workflow` and `mvpa-spatial`.
`hrf` declares cats, spire and JTransforms and `design` declares cats and
spire; neither imports them.

**Dead and legacy code** (verified by repository-wide reference search):

- `image/SeparableVolumeFilters.scala` (448 lines), orphaned since the
  registration engine was extracted.
- `hrf/compat/RNames.scala` (198 lines), an R-name facade nothing uses.
- 34 `@deprecated` members in `image` and `spatial`, called only by tests
  that pin the deprecations.
- About 150 never-called public definitions across design, image,
  connectivity, model, fit, spatial and others (list in the audit notes).
- Duplicate `legacyReader` in `fit`, and the typed/legacy `FitConfig` and
  `ContrastCore` bridge pairs (43 and 25 occurrences).

**Build residue.** `imageAlgebraBoundaryCheck` scans every source file on each
`image` compile for names of types that no longer exist (the compiler already
rejects them); `FrameErasureGate` runs twice per build; `compileAll`/`testAll`
duplicate the `scalafim*` aliases; Multivar `ir`/`inference` project refs are
unused; `tools/build/mill-spike` is a finished experiment.

**Repository weight.** `docs/` holds 4,617 tracked files; `docs/verification`
is about 600 MB, mostly 1,061 committed archives (`.tar.gz`, `.partNN`,
`.jsonl.gz`) and 30 source snapshots that no code loads. `docs/plans` mixes
about 20 completed or superseded plans with live ones. Locally there are 7
target-only module directories (~125 MB), 37 prunable worktrees, and 10 GB of
sbt-warm templates with no garbage collection.

**Keep, despite appearances.** `interop-archived-response` reads persisted
LNA and Zarr archives, so old files depend on it. `provider-contract` is the
plan's provider-composition contract (do not delete it).
`response-laws` and `first-level-laws` are intended law kits. `FrameErasureGate`
enforces a live typing rule. `MissingDataPolicy.Propagate` is a persisted
identity string. The JMH benchmark projects are run by CI.

## 2. Principles

1. A module exists only if some consumer needs it without another module's
   extra dependencies: fitting, storage, graphics, or platform IO. Equal
   closures or a single consumer mean the boundary should go.
2. Graphics never sit on the analysis path; plotting is a separate module.
3. JVM/JS differences live in platform directories of one cross project;
   separate modules only for single-platform artifacts.
4. Move a module to its own provider only for a demonstrated outside consumer.
5. Law kits are separate modules only when shared or implementer-facing.
6. Merges are build-only: packages, persisted strings and schema identifiers
   stay byte-identical. Package moves and API changes go in separate commits.

## 3. Target structure (after eco)

| Module | Change |
|---|---|
| `hrf`, `ar`, `response`, `response-laws`, `latent` | Unchanged (dead dependencies removed). |
| `design` | Graphics removed. |
| `design-view` | New: the four plotting files, `design` + Intaglio. |
| `fit` | Absorbs `model`. |
| `fmri-workflow` | Absorbs `fit-estimates`. |
| `mvpa-fmri` | Merges `mvpa-fit` and `mvpa-dataset` (packages unchanged). |
| `first-level-laws` | Absorbs `hrf-laws`. |
| `dataset`, `dataset-zarr`, `archive`, `archive-lna`, `archive-zarr`, `estimates`, `estimates-io`, `interop-archived-response`, `group`, `mvpa-group`, `mvpa-artifacts`, `mvpa-spatial`, `pipeline`, `motion`, `threshold`, `connectivity`, `surface-view-connectivity`, `phrf-comparison`, `scenario-testkit` | Kept; each boundary keeps storage, graphics or IO off a consumer that does not need it. Edges corrected as above. |
| `mvpa-pipeline` | Deleted. |
| `provider-contract` | Renamed from `provider-spike`. |
| examples | One `examples/workflows` cross project instead of separate JVM and JS projects. |

Current implementation (2026-10-26): Phase B is landed. The requested D merge
and interpreter deletion are locally qualified: **32 source-bearing modules**
remain, down from 34. Packages and persisted identities stay unchanged. See
[stage evidence](../verification/module-simplification-20261026/README.md).

## 4. Plan

Every step gates the touched modules on JVM and JS, keeps persisted
identifiers byte-identical, and commits with explicit pathspecs.

### Phase A — hygiene (independent of eco, low risk)

1. Remove unused dependencies (`hrf`: cats, spire, JTransforms; `design`:
   cats, spire), unused Multivar refs, and the dead or wrong edges
   (`mvpa-spatial → atlas`, `mvpa-spatial → mvpa-dataset % test`,
   `mvpa-artifacts → estimates-io`, redundant `phrf-comparison`,
   `fmri-workflow` and `mvpa-spatial` edges).
2. Delete `SeparableVolumeFilters.scala`, `hrf/compat/RNames.scala`, the 34
   `@deprecated` members with their pinning tests, and the duplicate
   `legacyReader`. Doing this before the image extraction shrinks what moves.
3. Sweep never-called public definitions module by module; the compiler on
   both platforms is the final check.
4. Build: drop the name blacklist and removed-file list from
   `imageAlgebraBoundaryCheck`, keep a single `FrameErasureGate` call, drop
   `compileAll`/`testAll`, delete `tools/build/mill-spike`.

### Phase B — merges (independent of eco)

1. `hrf-laws` → `first-level-laws`.
2. `fit-estimates` → `fmri-workflow`.
3. `model` → `fit`. Measure coverage first: `fit` has a stricter coverage gate
   than `model`; add tests rather than lower it.
4. Split `design-view` out of `design`. plsneuro's model-studio graphics add
   one dependency in the same window.
5. Rename `provider-spike` → `provider-contract`; merge the workflow examples
   into one cross project. The current implementation is pending qualification.

### Phase C — repository weight (needs decisions, §5)

1. Move committed archives, logs, `.jsonl.gz` and source snapshots out of
   `docs/verification` to release assets or external storage; keep markdown
   receipts, small manifests and the fixture generators that tests cite.
2. Move completed plans to `docs/plans/archive/`; delete superseded ones.
3. Local cleanup: target-only directories, prunable worktrees (after
   checking owners), and an sbt-warm template garbage collector.

### Phase D — after eco tasks

| Step | Waits for |
|---|---|
| `mvpa-fit` + `mvpa-dataset` → `mvpa-fmri`; delete `mvpa-pipeline` | U4 integrations |
| Replace `DomainFactory.unsafeRestore` in connectivity, latent, threshold and dataset; delete `locus-data` | U1 domain work |
| `group` uses the shared subject id instead of depending on `dataset` | I1, if the id grammar matches exactly |
| Decide whether `pipeline` stays; re-size `mvpa-spatial` | U6 |
| Revisit `phrf-comparison` (retire or keep) | the study being completed |

### Phase E — separate refactor

Collapse the typed/legacy `FitConfig` and `ContrastCore` bridges to the typed
form. This touches fit identity hashing, so it needs its own plan with
identity fixtures before any change.

## Implementation progress (2026-10-19)

- A1: unused HRF/design dependencies, unused Multivar project refs, wrong
  MVPA edges, and redundant workflow/comparison edges removed. MVPA spatial
  tests now declare their predictive provider directly; MVPA artifact codecs
  declare `ujson` directly.
- A2, bounded portion: the unreferenced separable-filter implementation and
  R-name HRF facade removed. Deprecated APIs, `PrimitiveBuffers`, duplicate
  reader handling, and the unused-public-definition audit remain.
- A4: obsolete removed-type/file scans, the duplicate frame-erasure scan,
  duplicate all-build aliases, and the Mill experiment removed. The live
  frame-erasure and provider-state checks remain at the image boundary.
- B1/B2: `hrf-laws` absorbed by `first-level-laws`; `fit-estimates` absorbed by
  `fmri-workflow`. Packages stay unchanged. Compiler policy required only two
  unused imports and one unused pattern variable to be removed from moved
  law files. The build now declares 49 modules instead of 51.
- Remaining B steps retain their original gates and coordination requirements.

The [verification receipt](../verification/module-simplification-20261019/README.md)
records the checks, original/final move hashes, adverse attempts, and recovery
of the unpublished mesh4s prerequisite. These steps do not complete either
simplification phase or an ecosystem milestone.

## 5. Decisions (2026-10-19)

1. Delete `mvpa-pipeline`: **approved**.
2. `docs/verification` archives: **move them out** to release assets or
   external storage, going forward. History is not rewritten unless decided
   separately, since a rewrite affects every clone and open branch.
3. `phrf-comparison`: **stays in ScalaFIM** as an unpublished research module
   until the study is resolved; no separate repository. Revisit at completion.
4. `hrf` and `ar` become providers only for a non-experimental outside
   consumer: **agreed**. plsneuro already uses them; it is experimental, so it
   does not yet justify extraction, but it is the first consumer to consult
   when the question returns.

## Remaining simplification execution — 2026-10-26

Outcome: complete the user-authorized D integration merge/delete slice, then
remaining A, C and E as one goal. Base: main c98148fab; Phase B Root PR68 and
PLS Neuro PR3 are merged. This is landing evidence, not new native acceptance.

D scope: merge mvpa-fit and mvpa-dataset into mvpa-fmri, delete the unused
mvpa-pipeline interpreter and its tests, update current build/CI/docs. Package,
source bytes, numerical fixtures and persisted identities stay unchanged.
The other D decisions and the unfinished PHRF study are outside this slice.

A scope: re-audit deprecated/dead definitions against current ownership and
known external consumers, then remove only demonstrably obsolete code. Extraction
may already have removed earlier findings; do not resurrect old source lists.

C scope: externalize historical bulk evidence with per-file hashes, physical
volume identity, verified restoration and retained small receipts/generators;
archive completed plans without breaking live references. No Git history rewrite.
Local cleanup requires fresh ownership and regenerability/custody checks.

E scope: first freeze fit/contrast identity fixtures, then replace typed/legacy
bridges with typed contracts while preserving admitted scientific behavior,
wire identities and fail-closed input validation. Detail the API migration before
editing identity-sensitive implementations.

Acceptance: affected full JVM/JS suites, actual consumer/law checks, test-inventory
and scenario validation, and final independent review. Raw logs retain actual
exit status and failed attempts. Exact source moves and stage evidence live in
`docs/verification/module-simplification-20261026/`. No publication/merge or new
scientific performance claim is inferred from local checks.

### Completion checkpoint

The requested MVPA merge/delete slice is committed as `fe6fe70bb`; remaining
Phase A hygiene is committed as `a6abc2fb9`. Phase C now has verified external
custody for 1,554 historical files and 38 full template trees, 22 archived plans
with forwarding pages, seven retired target-only module directories, and 36
retired stale templates. Phase C is recorded in `1ba80c172`. Phase E is complete in this branch: typed
fit preparation and contrast composition, unchanged literal identities, full
JVM/Scala.js owning and consumer checks, and final repository/examples compile.
The requested D slice plus A, C and E are complete locally; publication is separate. See [the stage receipts](../verification/module-simplification-20261026/README.md).
