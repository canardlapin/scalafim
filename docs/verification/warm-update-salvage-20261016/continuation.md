# Recovery checkpoint, 2026-10-16

The selective recovery branch is based on main `ac2ec8b706fc`, not a merge of the September branch. The original handoff and repaired Mote store are ancestors. `migration-progress.json` records committed behavior and bounded checks; `checks/` preserves successful and failed raw build attempts.

This is a development checkpoint. Provider behavior has not landed on main, and PLS Neuro has not been repinned or qualified. Preserve the September branch and October source snapshot until the remaining analysis and consumer checks are complete.

Continue by integrating sampled response-support policy with main's event scaling, orthogonalization, and derived-row provenance. Then recover typed study spatial support and reviewed timing metadata. Resolve surface-update transaction and scene-format contracts without removing native-renderer qualification gates.

PLS Neuro's diagnostic checkout is `/private/tmp/plsneuro-main-adoption-20261016`, branch `adoption/scalafim-main-20261016`, based on `2e0aac8e9030`. Its diagnostic provider tree is `/private/tmp/plsneuro-main-providers-20261016`; it has not been admitted to the provider contract. The last diagnostic build reported 110 application errors and had not reached desktop compilation. Source migration must explicitly account for canonical voxel ordering, persisted feature identities, and current world/grid ownership; see the README.

Use the canonical ScalaFIM root Mote store for writes. This branch carries a read-only portable snapshot, not an independent writable authority. Active issues: provider `bd-01M52AQ9JQS51XFMB17X4BDQJX`, consumer `bd-01M52AWZR7FJW4QX66CSFJ0SZT` in the PLS Neuro store. Release this task's reservations and claims when finishing or handing off; do not close the historical retained-atlas task as implementation-complete.
