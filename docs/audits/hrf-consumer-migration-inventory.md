# HRF Mat/Vec external-consumer inventory

Scope: ScalaFIM HRF `Mat`/`Vec` callers that would be affected by the staged S0--S5 plan in `docs/plans/hrf-matvec-contract-v4.md`. This is a read-only inventory of the consumer checkouts on 2026-10-04; it is not a provider-adoption or build qualification.

## Provider surface and migration boundary

The current provider, not yet S1, exposes `scalafim.fmri.hrf.linalg.Mat` and `Vec` as public final case classes, with public `data` arrays and public `unsafe` constructors. `Mat` also has `fromRows`, `rows`, `cols`, and element access; `Vec` has `toArray` and element access. The HRF package wildcard currently exports both types (`modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/package.scala:8-15`).

S0 changes tests only. S1 is planned to make `unsafe` and `data` `private[scalafim]`, adding read-only copy accessors. S4 changes the HRF core results to Gale `DMat`/`DVec`; S5 deletes the old types and wildcard export. The plan also replaces array-reference case-class equality with an explicit numeric value policy (`docs/plans/hrf-matvec-contract-v4.md:96-137,143-150,168-212`). None of those restrictions exists in the current provider source.

`Mat`/`Vec` remain case classes today, so `copy`/equality are potential public representation escape paths even where callers do not invoke them. The searched consumer files do not call `copy`, compare a `Mat`/`Vec` for equality, or serialize either type. All identified construction uses are listed below.

## Eidolon

- Checkout HEAD: `83c2a3a1864ce805d6d4c7c83a4ecee796314449`.
- Dirty input: 193 porcelain entries, including tracked documentation edits and untracked `.mote` operations. This is not commit-only provenance.
- Provider pin: `project/BuildVersions.scala:15` selects ScalaFIM Git revision `8e38b9a72932cf645abd123175f819f0ebee63f8`; `build.sbt:7-18` creates `hrfJVM` and `hrfJS` project references from that pin.

Direct affected sites:

- `modules/eidolon-scalafim-modules/shared/src/main/scala/eidolon/scalafim/modules/HrfEvaluationModule.scala:7-8,188-215` imports `hrf.*` and `hrf.linalg.Mat`, then makes `Mat` the type argument for its artifact and view adapters. It only reads `rows` and `cols`. S4/S5 replacement: import `gale.linalg.DMat`, parameterize those adapters with `DMat`, and retain the shape-only logic.
- `modules/eidolon-scalafim-workers/src/main/scala/eidolon/scalafim/workers/HrfEvaluationKernel.scala:7-18` declares its interpreter output and `Either` result as `Mat`, returned by `Evaluate.doubles`. S4/S5 replacement: change both to the migrated public result type (`DMat`) after the provider changes `Evaluate`.
- `modules/eidolon-scalafim-workers/src/test/scala/eidolon/scalafim/workers/HrfEvaluationWorkerSuite.scala:52-53` reads `result.data` for finite/nonzero assertions. S1 replacement: use the provider's read-only copy/iteration API; S4/S5 replacement: use the corresponding `DMat` accessor/iteration surface.

No Eidolon `Mat.unsafe`, `Vec.unsafe`, `Mat.fromRows`, direct array mutation, equality, or codec use was found. `HrfEvaluationModule.scala:7` relies on `hrf.*`, but it uses named HRF APIs rather than the wildcard-exported matrix names; removing the export should not make that import ambiguous.

## PLS Neuro

- Checkout HEAD: `8919882867a6b5c9d3bdd9d284e5cf30b1a51800`.
- Dirty input: four porcelain entries: modified `AGENTS.md` and untracked `.agents/`, `.claude/`, and `notes/`. This is not commit-only provenance.
- The active composite build requires `-Dplsneuro.providers=...` and reads its live ScalaFIM URI/revision from that external properties file (`project/Providers.scala:7-30`). No active provider contract is checked into this checkout, so a current resolved ScalaFIM revision cannot be established from source alone. Archived output receipts reference ScalaFIM `fd992a0c85001eb50ba2e78d29c497693ca494dc` with candidate patches; those are historical evidence, not an active pin.

Production code imports HRF descriptors, basis roles, response functionals, and sampling APIs, but has no direct `hrf.linalg.Mat`/`Vec`, `.unsafe`, or public HRF-array access. Its production HRF callers therefore need no S1 edit from this inventory.

Test-only direct uses requiring migration:

- `modules/glm-review/src/test/scala/glm/review/ContrastDraftProbe.scala:7,38,95` constructs synthetic design matrices with `Mat.unsafe`. S1 replacement: `Mat.fromRows(Vector.tabulate(rows)(r => Vector.tabulate(cols)(c => values(r * cols + c))))`, which copies the fixture values; S4/S5: `DMat.tabulate(rows, cols)((r, c) => values(r * cols + c))`.
- `modules/glm-review-javafx/src/test/scala/glm/review/fx/StandaloneReviewProbe.scala:15,36` and `StandaloneContrastBuilderProbe.scala:9,75` use `Mat.unsafe` for review fixtures. Same S1/S4 replacement.
- `modules/adapters/src/test/scala/plsneuro/adapters/FirstLevelProbe.scala:8,41`, `MultirunFirstLevelProbe.scala:10,38`, `TaskSignFirstLevelProbe.scala:7,50`, and `FactorialInferenceCalibrationProbe.scala:12,107` use `Mat.fromRows` for nuisance fixtures. `fromRows` remains callable through S1 but must become `DMat` construction once S4/S5 changes downstream first-level signatures.
- `modules/adapters/src/test/scala/plsneuro/adapters/ResponseDeclarationProbe.scala:12,265` uses `Vec.unsafe(Array(Double.NaN, Double.NaN))` to make an intentionally invalid HRF. S1 replacement: `Vec(Seq(Double.NaN, Double.NaN))`, an existing copying constructor. S4 replacement after the callback result migrates: `DVec(Double.NaN, Double.NaN)`. Generic vectors may contain non-finite values; the HRF validation boundary remains responsible for rejecting the callback result. No application-specific test API is needed.

The `EventSubsetProbe.scala:33` `.data` use is on an event value, not `hrf.linalg.Mat`/`Vec`. Other `.data` hits in PLS Neuro are unrelated matrix/image/domain objects and are outside this inventory.

## Consequence

Eidolon has one production migration seam, localized to HRF evaluation result transport. PLS Neuro needs fixture migration through existing copying constructors, but no production `Mat`/`Vec` migration identified here. The actual provider revision and compatibility result must be re-established from an explicit PLS Neuro provider contract before any consumer adoption claim.
