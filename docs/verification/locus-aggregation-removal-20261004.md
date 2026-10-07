# locus-data Aggregation removal readiness receipt (2026-10-04)

Branch `review/c8-locus-aggregation-removal-20261004`, built from preserved
snapshot `fce38df8` (`wip/locus-aggregation-removal-20261004`, cut from main
`53097f3f`) and merged with main `3ddc8693` (merge `b3c28143`). Origin: agent
codex-next-five, mote `bd-01KZ6BCRARE78N6HS59NN893TH` ("Remove duplicate
ScalaFIM aggregation and Cats-only locus dependency"), still open. Not landed;
awaiting independent review.

## Rationale

`scalafim.locus.Aggregation.foldMapBy` was a second copy of the one-pass
parcel reduction loop. After locus4s became the owner of generic finite-domain
algebra, `locus4s.data.Aggregation` provides the same reduction over
`PartialMap`/`PartialSurjection` and representation-neutral `Field`. The mote's
acceptance criteria: one aggregation loop owned by locus4s-data, no production
ScalaFIM consumer requiring `IndexedField` for aggregation, dependency
declarations that reflect actual consumers, and focused JVM and Scala.js tests
of the integration seam that do not duplicate upstream laws.

The snapshot deleted the loop and its suite and added the seam suite, but left
the `cats-kernel` declaration in `build.sbt` because another agent held a
reservation on that file. That declaration was the only unfinished item.

## Replacement

`locus4s.data.Aggregation.foldMapByChecked` at the pinned locus4s revision
`58c9739be51345ad9adc4bc9c9e7335023254ec9`
(`modules/locus4s-data/shared/src/main/scala/locus4s/data/Aggregation.scala`),
together with `locus4s.PartialMap.fromOptionalTargetOrdinals`
(`modules/locus4s-core/.../PartialMap.scala`). Both exist at the pin; no
locus4s bump is needed. To aggregate over a ScalaFIM parcellation `p`, build
`PartialMap.fromOptionalTargetOrdinals(p.ambient, p.parcels,
p.assignmentOrdinals)` and pass it to
`Aggregation.foldMapByChecked(mapping, field)(empty)(contribution)(combine)`.
`ProviderAggregationSuite` exercises exactly this path.

`ProviderAggregationSuite` (shared, 2 tests) fixes the seam behavior the old
loop had: background points are skipped, sources are visited in increasing
ordinal order, a field from a different runtime owner is rejected, and an
empty parcel domain produces an empty result without reading any source. The
provider takes explicit `empty`/`combine` arguments rather than a Cats
`CommutativeMonoid`, so the hierarchy-fusion law in the deleted suite is now
an upstream locus4s concern.

## API-break analysis

`scalafim.locus.Aggregation` was public in the `scalafim-locus-data` artifact,
so removing it is source- and binary-incompatible for any caller. No caller
was found:

- `git grep` of the whole repository (all modules, examples, docs, build):
  no Scala reference to `scalafim.locus.Aggregation` or `foldMapBy`. The
  remaining mentions are hash entries in historical verification manifests,
  the historical plan `docs/plans/finite-indexed-spaces.md`, and unrelated
  enums (`AcfAggregation`, `SurfaceSampleAggregation`).
- Downstream consumers that pin scalafim, `~/code/scala/eidolon`,
  `~/code/scala/PLSNeuro` and `~/code/scala/plsneuro`: no reference to
  `scalafim.locus`, `foldMapBy`, `locusData`/`locus-data` or `cats-kernel` in
  Scala, sbt or Markdown sources. eidolon's only `cats-kernel` mention is a
  note about its own `core-domain` module.

Downstream projects pin scalafim source revisions, and none uses the removed
object, so this change does not add a deprecation cycle. The NEWS entry
records the removal and the replacement.

## Build change

`locusData` no longer declares `"org.typelevel" %%% "cats-kernel" % "2.12.0"`.
After the change no Scala source under `modules/locus-data` mentions `cats`,
and `show locusDataJVM/libraryDependencies` lists only `scala3-library` and
`munit`. Cats elsewhere in the build is unchanged.

No other module relied on locus-data's transitive `cats-kernel`:

- Main sources: `scalafimCompileAll` compiles every module's main sources on
  both platforms, including those that import Cats (`image`, `atlas`, `latent`,
  `dataset`, `fit` and others).
- Test sources: `scalafimCompileAll` does not compile tests, so test-source
  Cats imports were checked by `git grep "cats\."` over
  `modules/*/{shared,jvm,js}/src/test`. The modules with such imports are
  `archive`, `archive-zarr`, `dataset`, `fit`, `image`,
  `interop-archived-response`, `latent`, `mvpa-dataset`,
  `mvpa-foundation-spike`, `response`, `response-laws` and `surface`. Each one
  either declares `cats-core` (and `cats-effect`) itself (`archive`,
  `archive-zarr`, `image`, `response`, and `mvpa-foundation-spike` in `Test`),
  or reaches it through an internal `dependsOn` on one of those declaring
  modules (`response`, `image` or `archive`). None reaches Cats only through
  `locus-data`. `cats-core` itself depends on `cats-kernel`. The test sources
  of every module on that list were compiled on JVM and JS after the change
  (see Gates).

## Documentation

- `modules/locus-data/README.md` (from the snapshot, with one wording fix):
  the Cats Kernel dependency and `foldMapBy` are gone from the module summary,
  and a new paragraph documents the provider adapter path.
- `README.md` and `docs/module-relations.md`: the locus-data blurbs no longer
  claim locus-data owns aggregation, and they point to `locus4s.data.Aggregation`.
- `NEWS.md`: an Unreleased entry, matching the one drafted on
  `wip/core-triage-docs-20261004`, and also noting the dependency removal.

## Gates

All gates ran through `tools/build/sbt-warm` in this worktree, with at least
59% memory free before each batch.

| Gate | JVM | JS |
| --- | --- | --- |
| `locusData/test` | 24/24 | 24/24 |
| `image/test` | 385/385 | 355/355 |
| `latent/test` | 44/44 | 44/44 |
| `surface/test` | 185/185 | 150/150 |
| `spatial/test` | 227/227 | 202/202 |
| `atlas/test` | 116/116 | 79/79 |
| `dataset/test` | 76/76 | 62/62 |
| `connectivity/test` | 64/64 | 64/64 |
| `mvpaFoundationAdmission/test` | 18/18 | 18/18 |
| `mvpaSpatial/test` | 15/16 (1 pre-existing failure) | 14/15 (1 pre-existing failure) |
| `scalafimCompileAll` | exit 0, 0 warnings | |
| `examplesCompile` | exit 0, 0 warnings | |

These are the direct `dependsOn(locusData)` projects in `build.sbt`.
`scalafimCompileAll` compiles main sources only. The test sources of the
remaining Cats-importing modules (`archive`, `archiveZarr`, `fit`,
`archivedResponseInterop`, `mvpaDataset`, `response`, `responseLaws`) were
compiled separately with `Test/compile` on JVM and JS: all exit 0, with no
compiler warnings.

### Review follow-up (after Opus review of `4196a208`)

- `ParcellationSuite` now tests `Parcellation.fromSurjection` directly
  (identities, assignments, total support, fibers as preimages). The deleted
  `AggregationSuite` had been its only test. A second new test checks fusion:
  aggregating through `fromSurjection` over parcel aggregates equals provider
  aggregation over `coarsen`.
- `ProviderAggregationSuite` uses significant-indentation style with named
  contribution functions and wrapped lines.
- `docs/plans/finite-indexed-spaces.md` carries superseded notes at §5.7, the
  dependency paragraph, the `AtlasReduce` row and the `locus-data` scope list.
- Re-run gates: `locusDataJVM/test` 26/26, `locusDataJS/test` 26/26,
  `scalafimCompileAll` exit 0 with no compiler warnings (the one `[warn]` line
  is an sbt GC notice).

**Pre-existing failure:** `SpatialFeatureSetPlansSuite` "volume label maps
become regional feature plans with linear voxel ordering" fails at
`SpatialFeatureSetPlansSuite.scala:98` (`SamplingAlignment.exact(...)`). It
fails identically on unmodified main `3ddc8693`, on both JVM (15/16) and JS
(14/15), in a detached baseline worktree. It concerns volume sampling
alignment and does not involve locus-data aggregation. It is not caused by
this change.

`git diff --check` is clean.

### Post-merge re-gate (main `b8e83fc4`, merge `07e239c1`)

`git merge main` from `f5940066` completed automatically with no conflicts.
Origin and C8 both changed `build.sbt`, `README.md`,
`docs/module-relations.md` and `docs/plans/finite-indexed-spaces.md`, but in
disjoint hunks:

- `build.sbt`: origin's pin bumps (alder, reframe4s, multivar) and new edges
  are kept. The `locusData` block still has no `cats-kernel`.
- `README.md`: origin's new `mvpa-artifacts` module blurb and
  `workflowExamples` commands sit alongside C8's locus-data wording.
- `docs/module-relations.md`: origin's `mvpa-artifacts` and `mvpa-dataset`
  rows and its new section sit alongside C8's locus-data rows and
  boundary text.
- `docs/plans/finite-indexed-spaces.md`: origin added a "historical design
  record" header, and C8's superseded notes remain in the body.

Origin did not change `modules/locus-data`. The one new locus-data dependency
edge is `mvpa`, which now `dependsOn(response, locusData, ...)`. `mvpa` does
not reference Cats in main or test sources. The set of modules whose test
sources import Cats is the same as before the merge.

Gates, at least 35% memory free before each batch. The direct
`dependsOn(locusData)` list was re-derived from the merged `build.sbt`:

| Gate | JVM | JS |
| --- | --- | --- |
| `locusData/test` | 26/26 | 26/26 |
| `image/test` | 385/385 | 355/355 |
| `latent/test` | 44/44 | 44/44 |
| `surface/test` | 280/280 | 225 (223 passed, 2 skipped) |
| `spatial/test` | 227/227 | 202/202 |
| `atlas/test` | 116/116 (see note) | 79/79 |
| `dataset/test` | 76/76 | 62/62 |
| `mvpa/test` (new dependent) | 401/401 | 401/401 |
| `connectivity/test` | 64/64 | 64/64 |
| `mvpaFoundationAdmission/test` | 18/18 | 18/18 |
| `mvpaSpatial/test` | 20/20 | 20/20 |
| `scalafimCompileAll` | exit 0, no compiler warnings | |
| `examplesCompile` | exit 0, no compiler warnings | |

The pre-merge `SpatialFeatureSetPlansSuite` failure no longer reproduces:
`mvpaSpatial` passes 20/20 on both platforms after the merge.

**Atlas note:** the first `atlasJVM/test` run went through the same resident
sbt server that had already run `image`, `latent`, `surface` and `spatial`
tests (in-process tests, 3 GB server heap). In that run, origin's new
`MniTemplateBridgeFilesSuite` test "a qualified numerical inverse gives the
forward map and an executable reverse route" failed with
`java.lang.OutOfMemoryError: Java heap space` inside
`reframe4s.field.NumericalInversion` (115/116). In a fresh server, the same
suite passed 8/8 on unmodified main `b8e83fc4`. The full `atlasJVM/test`
passed 116/116 on this branch, also in a fresh server. The failure depends on
how much heap the shared server had left; it is not caused by this change.
Running that suite as the first command in a server avoids it.
