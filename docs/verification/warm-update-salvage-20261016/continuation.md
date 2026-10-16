# Recovery checkpoint, 2026-10-16

The selective recovery branch is based on canonical main `ac2ec8b706fc`. The original handoff and repaired Mote store are ancestors. PR [#62](https://github.com/canardlapin/scalafim/pull/62) is the temporary integration route. Final PLSNeuro adoption must pin an exact canonical main commit. ScalaFIM has no PLSNeuro source or build dependency.

Provider source is `f87fecf7ade27054229aa55bbd24d499f519dc8a`. Recovered first-level, image, atlas and surface contracts are recorded in `migration-progress.json`. Surface revision 9 retains complete geometry-family asset evidence; old September and canonical revision 8 writers are frozen independently. Paired cameras work through CPU, native JavaFX and Three composition. Final review fixes preserve the historical capability identifier and avoid colour work on camera-only updates. The six affected suites pass: 272/272 shared JVM/JS, 70/70 raster, 63 JavaFX and 57 Three.js. Full aggregate compile passed at final source `f87fecf7a`; required hosted gates are still separate and pending.

Provider behavior is not yet landed on main and PLSNeuro is not repinned or qualified. Native affine interpolation and dense-colour refinement remain withheld.

PLSNeuro diagnostic checkout: `/private/tmp/plsneuro-main-adoption-20261016`, branch `adoption/scalafim-main-20261016`, production checkpoint `76f0f745b13e3a66dac22bab9e3011dd33580678`. Diagnostic providers: `/private/tmp/plsneuro-main-providers-20261016`. Adapter test compilation and narrowed contrast-reprojection probe pass; desktop test fixture migration continues. Saved legacy geometry uses explicit first-axis-fastest-to-canonical translation and frozen asymmetric artifacts. The historical reachable-basis assertions are preserved but withheld; the application has no production caller of that API. See `consumer-spatial-migration.md` and the consumer's migration report.

Use canonical repository Mote stores for writes. The recovery branch carries a read-only portable snapshot. Active issues: ScalaFIM `bd-01M52AQ9JQS51XFMB17X4BDQJX`; PLSNeuro `bd-01M52AWZR7FJW4QX66CSFJ0SZT`. Release this task's leases when finishing. Preserve the September branch and October source snapshot.


2026-10-16 16:39 UTC: explicit event-role recovery is committed at 5d2a86db6598a9358a65189e674607e5d3939000 (7 JVM + 7 JS tests, independent review). This changes the source after c553 and requires new exact-tip hosted checks. Consumer fresh check at 0eb0b828 passed; headless did not. Continue the consumer repairs and explicit authenticated legacy-result import review; never weaken world-space admission or silently restore the old SPMG1 default.
