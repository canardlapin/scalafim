# UMVPA M2 aggregate acceptance

Source base: `47b5a851f970d27bb952668713fdaa3de3e7c847`, isolated branch
`work/umvpa-finish-20261001`. The final commit identity is attached in Mote;
this document does not attempt to contain its own commit hash.
Mote gate: `bd-01M2BND19FB3K3WTFAC6PFYYKX`.

## Aggregate review and repair

All nine M2 leaf packets are closed with substantive owner evidence. The
independent aggregate review checked nominal relation/query/metric boundaries,
origin and estimability claims, directed/adjoint/signed arithmetic, reuse
identity, operator-native readout and observation-mean consumers, and actual
scoped resource expiry. The initial frozen review accepted those numerical
and execution results but refused aggregate closure because obsolete spatial
helpers and an old-engine benchmark remained.

The repair physically deletes `SpatialFeatureSetPlans`, `LocusFeatureSetPlans`,
`SpatialFeatureDomain`/`SpatialFeaturePlan`, their implementation-only tests,
and `SearchlightPerformanceReceiptSuite`. No production consumer justified
retaining these helpers. The migration ledger and spatial README now identify
the native measurement-frame route. Generic source/fold/engine definitions
still needed by M3 retain their explicit M3.13 owner.

The retained PyMVPA fixture now constructs real complete-affine physical
volume neighborhoods and compares them with both frozen reference memberships
and independent direct physical-distance arithmetic. The index-space
counterfactual differs. Real surface geodesic neighborhoods match the frozen
independent Dijkstra references and differ from chord neighborhoods. Both
volume and surface memberships execute actual typed measurements and numeric
local contrasts. A real task failure at center 13 leaves center 23 successful.
No historical old-engine performance receipt transfers to the new route.

The independent reviewer approved this repaired source seam, conditional on
final compilation and frozen-source evidence. Gates83/84 separately passed
20 spatial tests on each platform. Gate84's subsequent aggregate compile
failed in then-unfinished M3.09 source; it was not counted as a successful
compile. The final gate91 supersedes that failed compilation.

The expanded module/example Scala removal scan has no remaining references
to the removed relational or spatial symbols (ripgrep exit 1, zero matches).
The exact pattern and result are in `m2-expanded-removal-scan.json`.

## Final qualification

Final combined verification: gate90 passed 324 mvpa + 45 mvpa-fit + 107
mvpa-dataset + 20 mvpa-spatial JVM tests, plus 3 workflow and 5 atlas example
tests (504 total). Gate91 passed the same four Scala.js module counts, plus
2 workflow example tests (498 total). Combined: 1,002 tests.
`scalafimCompileAll` passed both platforms without warnings. Full raw logs
contain no warning/error lines. The 181 source/fixture entries in
`gate90-sources.json` remained unchanged across both gates. `git diff --check`
passed.

Raw log SHA-256:

- gate90: `eea466e8660f97e813458dbe13de0760d1712bf446492e46aa59868194d5d3f2`
- gate91: `27e3f972ba4bf867b380b2532657f8dd751ccbfa9b6072cb27ab8ad60b7fd653`

Evidence directory: `/private/tmp/scalafim-umvpa-finish-evidence-20261001`.

This accepts the M2 operator-native relational replacement only. It does not
establish calibrated inference, measured process peak memory, replacement
performance targets, completed M3–M5, landing, push, merge or release.
