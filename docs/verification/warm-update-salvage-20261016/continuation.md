# Recovery checkpoint, 2026-10-16

The selective recovery branch is based on canonical main `ac2ec8b706fc`. The original handoff and repaired Mote store are ancestors. PR [#62](https://github.com/canardlapin/scalafim/pull/62) is the temporary integration route; final PLSNeuro adoption must pin an exact canonical main commit. ScalaFIM has no PLSNeuro dependency.

Provider behavior is not yet landed on main; PLSNeuro is not repinned or qualified. `migration-progress.json` records committed behavior and checks. Native worker colour preparation, compatible layer transactions and the independent review correction are implemented. Scene persistence compatibility and remaining scientific consumer contracts still need resolution. Native affine interpolation and performance qualification remain withheld.

PLSNeuro diagnostic checkout: `/private/tmp/plsneuro-main-adoption-20261016`, branch `adoption/scalafim-main-20261016`, base `2e0aac8e9030`. Diagnostic providers: `/private/tmp/plsneuro-main-providers-20261016`; not admitted dependencies. Last build against `229a65f3e` reported 72 adapter errors before spatial migration, with desktop not reached. Geometry migration is in progress, preserving legacy identities and explicitly translating first-axis-fastest saved voxel ordinals into canonical last-axis-fastest buffers. See `consumer-spatial-migration.md`.

Use canonical repository Mote stores for writes. The recovery branch carries a read-only portable snapshot. Active issues: ScalaFIM `bd-01M52AQ9JQS51XFMB17X4BDQJX`; PLSNeuro `bd-01M52AWZR7FJW4QX66CSFJ0SZT`. Release this task's leases when finishing. Preserve the September branch and October source snapshot until analysis and consumer checks are complete.
