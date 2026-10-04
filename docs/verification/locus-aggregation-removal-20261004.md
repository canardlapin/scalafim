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
`munit`. Other modules that use Cats (`image`, `atlas`, `latent`, `dataset`,
`fit` and others) all compile, so none relied on locus-data's transitive
`cats-kernel`; Cats elsewhere in the build is unchanged.

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

These are the direct `dependsOn(locusData)` projects in `build.sbt`. Modules
that depend on locus-data only transitively are covered by `scalafimCompileAll`.

**Pre-existing failure:** `SpatialFeatureSetPlansSuite` "volume label maps
become regional feature plans with linear voxel ordering" fails at
`SpatialFeatureSetPlansSuite.scala:98` (`SamplingAlignment.exact(...)`). It
fails identically on unmodified main `3ddc8693`, on both JVM (15/16) and JS
(14/15), in a detached baseline worktree. It concerns volume sampling
alignment and does not involve locus-data aggregation. It is not caused by
this change.

`git diff --check` is clean.
