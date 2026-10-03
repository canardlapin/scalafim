# Scoped-world fixture migration, 2026-09-30

The FSL and fit follow-up is committed locally as
`0aa969a2b30b13c62afb8ef9a2b9eaecef1f995b`. Its complete qualification is in
`../stp-fsl-chain-20260930`: 947 fit/transform JVM and JS tests, warning-clean
full compile, 35 scenario entries and 168 native fixture hashes. The exact local
commit receipt is preserved here in `fsl-fit-local-commit.json`.

Fray card 89 also identified an archive assertion that depended on the old global
unknown frame. `LnaManifestCodec.parseRun` persists dimensions only; its file
space label does not establish a precise coordinate-world identity. Two opens
must have distinct unresolved frames until the caller supplies shared evidence.

The `LnaDatasetSuite` path-resolution test now checks that both absolute and
relative paths yield the expected dimensions, timepoints and selected data. Raw
exact grid congruence must fail; strict congruence must then pass after explicit
caller admission into one declared world. Its original scalar budget is retained.
Production algorithms, archive formats and APIs are unchanged.

All archivedResponseInterop tests passed: JVM 142 and JS 104, with no warnings
or errors. The raw receipt and actual exit status are pinned in `qualification.json`.
The archive path case
is JVM filesystem IO; JS checks cover the module's portable contracts, not a
fictional JS filesystem execution. Source identity and the exact original test
are preserved here. The already-passed full compile applies because production
and build sources are unchanged.

The separate `SpatialFeatureSetPlansSuite` assertion has the same fixture issue,
but its path is reserved by `atlas-invariants-20260930` for AtlasRef migration.
The precise request and ownership handoff are on Fray card 94. That file is not
included in this local archive packet until its writer supplies verified evidence
or releases it. No global unknown-world identity will be restored.
