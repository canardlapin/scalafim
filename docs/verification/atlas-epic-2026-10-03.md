# Typed atlas epic completion — 2026-10-03

Epic `bd-01M3RY746DMGYQJE4SM14YQJPQ` is complete. All eight direct children
and their composition/loading children are closed in Mote on bounded functional
acceptance. The full-suite timing limitations below remain explicitly unqualified. Earlier slices and
independent R evidence are recorded in the [implementation plan](../plans/atlas-typed-plan-2026-09-30.md)
and [October 2 verification](atlas-backlog-2026-10-02.md).

This receipt qualifies the full working-tree snapshot identified below. The
local commit containing it records the image4s pin, INT8 regression, Atlas README,
implementation plan and completion evidence. Earlier typed-atlas implementation
and unrelated working-tree changes remain uncommitted; this commit alone does
not contain the complete epic implementation.

## Final repair

ScalaFIM now pins hosted image4s `2c0638fb0767058ccd59b37f93754e95fc27df28`. Signed INT8
NIfTI datatype 256 works through raw, stored/scaled scalar, native-label,
label/scalar output and incremental output paths. UInt8 remains unsigned.
A new signed-label writer regression failed against the recovered candidate
before repair. Deterministic cases cover both byte orders, signed boundaries,
scaling, overflow and refusal of nonidentity scaling for native labels.
ScalaFIM also checks native signed-byte storage and scaled volume reads.

The provider change is published on `codex/nifti-int8-atlas` in
[image4s draft PR #15](https://github.com/canardlapin/image4s/pull/15).
The formatter follow-up includes three existing geometry formatting failures
reported by CI. It changes formatting only. Neither repository's default branch
was merged or published by this task. Verification preceded the local ScalaFIM
commit described above; unrelated changes were preserved.

## Verification

| Gate | JVM | Scala.js |
| --- | ---: | ---: |
| image4s core, implementation commit | 96 | 92 |
| image4s geometry, final commit | 39 | 36 |
| image4s NIfTI, final commit | 58 | 39 |
| ScalaFIM image, final pin | 389 | 359 |
| ScalaFIM atlas, final pin (JVM: 43 focused suites) | 209 | 155 |
| Atlas workflows, final pin | 3 | 3 |
| MVPA spatial functional suites, final pin | 15 | 15 |

All listed tests passed. The focused Atlas JVM gate includes every shared suite
and every loader suite; it explicitly excludes `MniTemplateBridgeFilesSuite`.
The complete Atlas JVM gate is not green: its timing failure is retained below.
Atlas examples passed 6 tests and the atlas-to-MVPA
workflow passed 2. `scalafimCompileAll` passed for both platforms.
The image4s formatting checks and final hosted PR checks passed separately.
Core implementation bytes are unchanged by the formatter follow-up.

All source-backed atlas checks were explicitly enabled. Original native
CIT168 (16 parcels), HCP thalamic (14), MDTB10 (10) and HCP ROI (4) files
loaded on their original grids with verified digests. FSL, Jülich visual,
HCPex 1mm/2mm, Olsen MTL/hippocampus and CBIG Schaefer source checks also ran.
No source conversion, resampling or derived CIT168 hemisphere split was used.
Header-coordinate limitations remain explicit in provenance; loading does not
establish additional template-space equivalences.

Independent R alignment, expansion and composition fixtures retain their
recorded generator, TSV and portable Scala hashes. Their current suite
assertions passed. This run checked fixture integrity; it did not regenerate
or broaden the earlier R scientific evidence.

The first Atlas run at the implementation commit passed 216 of 217 tests.
The existing native TemplateFlow inverse reported numerical errors within
its thresholds but took 643 seconds against its unchanged 600-second test timeout. A second full run at the final pin was stopped after the same inverse again
exceeded its deadline. GC accounted for about 20 seconds of the observed run;
raising the heap did not resolve the delay. Final feature acceptance therefore
uses 42 explicitly named Atlas JVM suites plus the metric producer suite checked
separately, and the complete Atlas JS suite. This covers every shared computation
and loader/source suite. No timeout, tolerance
or assertion was changed. Full-suite timing remains a limitation, not a pass.

The later MVPA spatial JVM batch also failed its unchanged 30-second performance
receipt deadline at 70 seconds, after all 15 functional adapter tests passed.
The functional suites subsequently passed as a separate gate. The record
preserves the failed batch exit codes; successful earlier module commands are
identified separately. No performance qualification is claimed. A pre-existing
linops4s sbt build-definition warning is recorded separately from source compiler
warnings. Full repository tests and unrelated scientific qualification campaigns
were not run.

## Reproducibility

The isolated consumer snapshot started at ScalaFIM `62312f5da78302e67dc9b1ee05e63823236c9053`
plus the existing working-tree changes. Final shared HEAD was
`62312f5da78302e67dc9b1ee05e63823236c9053`. All 1965 recorded source, build and
fixture hashes match the tested snapshot and shared checkout. Final dependency
resolution used the canonical hosted HTTPS URI and immutable SHA without a
local image4s override. The remote branch SHA was independently checked.

[Acceptance receipt](atlas-epic-2026-10-03/acceptance.json),
[input hashes](atlas-epic-2026-10-03/consumer-input-hashes.json),
[native source inventory](atlas-epic-2026-10-03/native-source-inventory.json),
[provider patch](atlas-epic-2026-10-03/image4s-int8.patch), and full command logs
with exit metadata are retained in the evidence directory. Source inspection
and local execution are author validation; the draft PR is not an independent
review or a default-branch landing.

Visualization, spin inference and general probabilistic membership remain
outside this epic's stated scope.
