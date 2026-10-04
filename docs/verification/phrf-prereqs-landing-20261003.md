# PHRF prerequisite landing, 2026-10-03

Step 1 of the PHRF reconciliation plan: a bounded, selective landing of the
reviewed profile-HRF prerequisites from the isolated integration line onto
canonical `main`. PHRF itself (`modules/phrf-comparison`, `tools/phrf-comparison`,
`RidgeLss`, `TrialHeldOutPrediction`, the PHRF spike suite) is **not** part of
this landing; it is step 2.

- Branch: `land/phrf-prereqs-20261003`, cut from `main` at `62312f5d`.
- Integration source: `8d5b0e51` (clone at
  `/private/tmp/scalafim-execution-20260929/integration`), which already carries
  the upstream fixes for these files.
- Merge base of `main` and the integration line: `8d0dd730`.
- Per-file provenance: [`phrf-prereqs-landing-20261003/manifest.json`](phrf-prereqs-landing-20261003/manifest.json).
  It records every landed file's path, source commit, SHA-256, disposition
  (`taken` or `merged`), the integration commits that touched it and the review
  receipts that cover those commits.

## What is in

52 source/test files plus one `build.sbt` line.

**Taken byte-for-byte from `8d5b0e51` (50 files).** For each one, the `main`
blob equals the merge-base blob, so `main` has no content change to preserve.
Three of them (`TrialBanded.scala`, `TrialBandedSuite.scala`,
`ConditionMilestoneSuite.scala`) are listed by `git log 8d0dd730..main` because
of a side history merged into `main` (`b2612ab4`, `cff24b3e`), but their `main`
blobs are identical to the merge-base blobs. The manifest records both facts.

- model: `ProfileHrfPlan` and its suite.
- fit (shared): the profile executor and readout stack: `ProfileHrfFit`,
  `ProfileHrfTrialOutputs`, `ProfileTrialReadout`, `TrialMlObjective`,
  `ObservedFamilyCertification`, `BlockExecutor`, `CompactCondition`,
  `ConditionProfileFit`, `CriterionJet`, `OutputRequest`, `TrialBanded`,
  `TrialBandedMlBackend`, `TrialConditionalSolve`, `TrialConstrainedLogDetJet`,
  and their suites.
- fit (JVM): `ParallelBlockExecutor`, `ProfileHrfFitParallel`, and their suites.
- ar: `WhiteningPlan` (run-aware transpose) and `WhiteningTransposeSuite`.
- design: `KernelBasisDesign` and its suite.
- hrf: `GaussianFamily` (direct-lag evaluation), its suite and
  `GaussianPrecisionFixtures`.
- first-level-laws: `ConditionMilestoneSuite`, `TrialBandedSuite`,
  `ConditionAdmissionDiagnosticsSuite`, `ObservedFamilyAdmissionSuite`,
  `ConditionStationarityRoundoffSuite`, the C0 literal fixture, platform trace
  and qualification suites, and the JVM/JS `ConditionC0QualificationPlatform`
  shims.
- benchmarks: `ConditionProfileBenchmark`, `TrialBandedBenchmark`.

**Three-way merged (2 files).** `main` changed both after the merge base
(`e6eb79d0`, `cff24b3e`); each was merged with
`git merge-file main 8d0dd730 8d5b0e51`.

- `CompactConditionRuntimeSuite.scala`: clean merge. The result is identical to
  the integration blob, because the integration line already carries `main`'s
  decoder work-budget fixture alignment.
- `ConditionProfileFitSuite.scala`: one conflict, resolved as the union. The
  integration side's admission and nuisance fixtures and budget-relative `jets`
  bound are kept, and so is `main`'s additional
  `exactEvaluations <= maxExactEvaluations` assertion. No assertion was removed
  or weakened.

**`build.sbt`.** Only `galeRevision` moves from `18d24dbb` to `da38f8c4`, the
hosted Gale pin that `944b6fe3` was qualified against. The bump is additive: it
adds `BandedLogDetJet`. None of the integration line's `phrfComparison` project
definition or aggregate entries are included.

## Kept as `main` has them

- `ShapeDecoder.scala`: `main`'s blob (`d65e2339`) is the reviewed `8840dbfc`
  version and newer than the integration line's. The integration version is
  **not** landed.
- `ShapeDecoderSuite.scala` and `WorkReceipt.scala`: identical on both sides.

## Excluded, and why

| Excluded | Reason |
| --- | --- |
| `modules/phrf-comparison`, `tools/phrf-comparison`, `RidgeLss`, `TrialHeldOutPrediction`, `PhrfSpikeSuite` | PHRF proper; lands in step 2 |
| #74 fit-export cluster | Separate review and landing decision |
| Bounded residual traces and precision pooling (#17–19) | Independent of PHRF prerequisites; separate landing |
| HDF5 and other estimate providers | Provider/packaging gates remain open (macOS ARM/JDK25 evidence only) |
| Core-NIfTI and the image4s pin | Separate IO landing; pin change has its own blast radius |
| Atlas and Metal/JavaFX work | Unrelated to PHRF |
| Python oracle tool | Belongs to the PHRF comparison tooling (step 2) |

## Review coverage

Thirty-two integration commits in `8d0dd730..8d5b0e51` touch the landed files.
The audit named 30. The other two, `1d2bdf5d` and `498021e3`, touch only
`TrialBandedBenchmark.scala`, and the P1 record reviews both: the benchmark
constructor repair `cb1e71d1` is integrated as `1d2bdf5d`, and the benchmark
adapter `498021e3` is accepted with Stage 1.

| Integration commits | Reviewed basis | Receipts |
| --- | --- | --- |
| `4244446e` `fe4ee895` `3d718771` `113d1fb3` `ae719307` `ad439bd6` `4eccd371` `66d62361` `771672cb` `c42be910` | PHRF-12 condition integration: author source `b1df154d` (`ad439bd6`) and both-admission regressions `42bb5c03` (`c42be910`) | P1 README; `initial-profile-evidence.json.gz`; `condition-admission-open-receipts.json.gz` |
| `8c39a2f7` | Model-only `ProfileHrfPlan` reviewed at `4f47a4a6` | P1 README |
| `d855972f` `057dc067` | Executor windows reviewed at `74f4adcf`/`fed64de7`; lifecycle repaired at `d1879b4d`; final state accepted at `7bd14ec9` | P1 README; dispositions; `trial-executor-final-integrated.json.gz`; `profile-hrf-plan-executor-2026-09-29.md` |
| `d9a4005b` | Interleaved lowering `4134c23c` | `trial-laws-interleaved-final.json.gz`; `trial-laws-supported-domain.json.gz` |
| `5b2405b8` `63df576f` | Banded work counters `5b9c7b8e`/`b3ac2fbd` | `bounded-banded-work-final.json.gz` |
| `1d2bdf5d` | Benchmark constructor `cb1e71d1` | `trial-benchmark-attempted-work-2026-09-30.md`; `temporal-hosted-pin-final.json.gz` |
| `bf304201` `5cb42af9` `498021e3` | Stage 1 conditional solve `bf05dde6`+`72ed9bc6`; adapter `498021e3` | `trial-conditional-stage1-final.json.gz` |
| `d797a0dd` | Run-aware AR transpose | `whitening-transpose-2026-09-29.md`; `ar-adjoint-final.json.gz`; `accepted-prerequisite-local-commits.json` |
| `041514a6` | Accepted diagnostic prerequisites, recorded unchanged | `accepted-prerequisite-local-commits.json`; `trial-conditional-stage1-final.json.gz` |
| `9235c3c7` | AcceptBoundedExecutorLifecycleAnd22FieldAccounting | `profile-hrf-plan-executor-2026-09-29.md`; `trial-executor-final-integrated.json.gz`; `review-history-20260930-0530.json.gz` |
| `43db2e83` | AcceptBoundedPublicReadoutAndScopedStorage (`d7a9e900`) | `profile-trial-readout-stage2-2026-09-30.md`; `trial-public-executor-qualified.json.gz` |
| `d71e5ca9` | AcceptBoundedTypedCriterionAdapter (`0017dafd`) | `trial-ml-objective-2026-09-30.md`; `trial-criterion-qualified.json.gz` |
| `944b6fe3` | AcceptBoundedHostedNativeMlCoreAndHelper (`3d75dea0`, Gale `da38f8c4`) | `trial-banded-ml-backend-2026-09-30.md`; `trial-constrained-logdet-2026-09-30.md`; `hosted-ml-core-qualified.json.gz`; `trial-ml-helper-local-qualified.json.gz` |
| `ee83b1eb` | Public-declaration repair `a7521ffb` (parent: the rejected `e6543fb6`). The independent review verdict is "ACCEPT LOCAL for a7521ffb23b97f75856f5070cd24bfdd600bd51f". The integrated blob hashes match the reviewed source. The repair is also accepted with the `0edf75c2` disposition. | `hdf-public-c0-integration-qualified.json.gz`, payload `evidence/profile-public-provenance-independent-review.md` (sha256 `c24ed1b4…`); `profile-public-executor-readout.md` |
| `0edf75c2` | C0 default controls accepted; C0 scientific status is HOLD | `condition-c0-qualification.md`; C0 oracle/frozen200 archives; `c0-prefresh-179-independent-review.md` |
| `a6dbd3b1` | AcceptBoundedNativeMlExecutorIntegration (`8523c8c7`) | `profile-trial-ml-executor-2026-09-30.md`; `native-ml-executor-integrated-qualified.json.gz` |
| `a9fecbb4` | AcceptBoundedLiteralColumnPlatformTrace | `condition-c0-platform-trace-2026-09-30.md`; `c0-literal-platform-trace-qualified-v2.json.gz` |
| `c9554354` | AcceptDirectGaussianPrecisionRepair (qualified at `fcb0addb`) | `condition-stationarity-roundoff-20260930.md` and its directory; `gaussian-direct-lag-qualified-v2.json.gz` |
| `327eda76` | AcceptBoundedPublicMlOutput | `profile-ml-public-output-2026-09-30.md`; `profile-ml-public-output-qualified-v2.json.gz` |

"Dispositions" is `p1-execution-2026-09-29/current-review-dispositions.json`.
"P1 README" is `p1-execution-2026-09-29/README.md`.

### Archived receipts

The receipts this landing relies on are copied byte-for-byte from `8d5b0e51` to
the same `docs/verification/` paths they have on the integration line, so their
citations resolve on `main`. There are 107 files, about 10.3 MB. The manifest
lists each one with its SHA-256.

The copy is partial in one place. The P1 directory brings only the README, the
review dispositions, the accepted-prerequisite record, `SHA256SUMS`, and the
archives cited above. Its HDF5, calibration, Core-NIfTI, pooling and Fray
archives belong to excluded work and are not landed. Their links in the P1
README therefore do not resolve on `main`, and `SHA256SUMS` should be checked
with `shasum -a 256 -c --ignore-missing`. Several receipts also cite absolute
`/private/tmp/scalafim-execution-20260929/...` paths. Those were never
repository paths; their content is embedded in the archived `.json.gz` payloads.

## Scientific scope unchanged

This landing adds no scientific admission. The P1 record's open gates still
apply: C0 scientific qualification is on HOLD (the JVM SNR 0.5 cell admits
185/200 against an unchanged 190/200 gate), there is no fresh or 100000-voxel qualification,
calibration inference is Unresolved, and full PHRF tickets are not closed.

### Archived evidence numbers come from the integration-line decoder

The archived C0, condition-admission, stationarity and Gaussian-precision
numbers were all produced with the integration line's `ShapeDecoder.scala`
(SHA-256 `397f6826ee94bf761984cc592e0e1a397e31be68120546499852896218df8dcb`,
from `8d5b0e51`). This landing keeps `main`'s reviewed decoder from `8840dbfc`
instead (SHA-256 `982f724a37d62cc37b3f775b80a3ec9f0ae283f4f7c757482c690277d9315554`).

The landed suites pass on `main`'s decoder, as the gates below show. The archived
admission counts, timings, cohort outcomes and decoder work figures do **not**
transfer, however. They describe the integration decoder and must not be cited
as `main` measurements. Any scientific qualification on `main` needs fresh runs.

### Review model provenance

The archived integration-line reviews (P1 README, dispositions, per-feature
receipts) do not record which model performed them. The one review of this
landing that is known to have been run by Opus is the independent landing review
of `3ceba77c`. It verified the provenance of all 50 taken files by script, an
exact match between the commit and the manifest, byte identity of all 107
archived receipts, review coverage of all 32 commits, the decoder integration
and the `WhiteningPlan` transpose. Its verdict was CHANGES-REQUIRED, with one
defect (F1 below), and the follow-up commit addresses it.

### GaussianFamily behaviour change

`GaussianFamily.scala` (`c9554354`) evaluates each lag directly. Two behaviours
change as a result:

- the previous underflow flush to zero is removed;
- the uniform-grid recurrence fast path is gone.

The change was reviewed as AcceptDirectGaussianPrecisionRepair. The landing
review ran `hrfLawsJVM/test` and got 82/82. `hrfLaws` is now in this landing's
gate list on both platforms (see Gates).

## Gates

All gates ran on the landing worktree through `tools/build/sbt-warm`, one batch
at a time. Before each batch, `memory_pressure` had to report at least 30% free.

| Gate | Result |
| --- | --- |
| `scalafimCompileAll` | exit 0 (1415.9 s); no compiler warnings |
| `Test/compile` for model, ar, design, hrf, fit, firstLevelLaws (JVM) | all exit 0, warning-free |
| `Test/compile` for the same modules (JS) | all exit 0, warning-free |
| `fitBenchJVM/compile` | exit 0 |
| JVM tests | model 33/33, ar 152/152, design 269/269, hrf 259/259, fit 551/551, firstLevelLaws 80/82 |
| JS tests | model 33/33, ar 150/150, design 269/269, hrf 259/259, fit 498/498, firstLevelLaws 80/82 |

Totals: JVM 1344/1346 and JS 1289/1291 passed. Every landed suite passes on
both platforms.

The two failures are the same on both platforms:
`MissingResponseGeneratedLawsSuite` "propagated missing responses …" and
"voxel-specific row omission …". The failing seed is
`sp9DAl0FY1eBBKPdxp1XHmPtWKKKG-fp4dJAL7wpvdF=`. In each case, estimated-GLS
whole-volume and chunked fits disagree by one to three ulp in the estimated AR
coefficient, for example `0.7355886724316139` against `0.7355886724316136`. The
comparison is exact, so it fails on structural provenance.

**This failure is pre-existing on `main`; this landing did not cause it.**

- A pristine detached worktree at `main` `62312f5d` was run with
  `firstLevelLawsJVM/testOnly scalafim.fmri.laws.MissingResponseGeneratedLawsSuite`.
  It fails both properties with the same seed and the same AR values.
- The suite is not in this landing.
- It exercises `FitPlanExecutor` GLS paths that this landing does not modify. In
  `ar`, the landing only adds `WhiteningTransform.transposeMatrix`.
- The Gale bump is additive (a new `BandedCholesky.logDetJet` method plus `BandedLogDetJet`).
- The JS failure has the same signature, but it was not separately reproduced on
  pristine `main`.
- A likely origin is `main`'s `45173e2c` ("stream pooled GLS state"). That is not
  verified here.
- **Known pre-existing failure:** a separate fix is in progress outside this
  landing, tracked by the PHRF reconciliation coordinator. This landing does not
  change the suite.

## Follow-up to the landing review (F1, F2)

**F1: the condition workflow example no longer compiled.** The landed
`ConditionProfileFit.scala` makes `ConditionProfilePolicy.admission:
ObservedFamilyAdmission` a required field. `scalafimCompileAll` does not build
examples, so this broke only `workflowExamplesJVM/compile`, at
`ProfileHrfConditionWorkflow.scala:85` ([E171] missing argument for
`admission`).

The example now builds its admission the same way the merged
`ConditionProfileFitSuite` does:

- it calls `ObservedFamilyCertification.admitForCondition(plan, structure, expanded, term, frame, precision, None, Some(nuisance), points, ObservedFamilyRequirements(1e-2, 1e8, 1e-6))`;
- `nuisance` is the plan's non-task (baseline) columns;
- the certification points are `(4, log 1.2)` and `(6, log 2.0)`.

Its decode budget changes from `DecodeBudget(2, 2, 2, 6)` to `main`'s reviewed
`(coarseStride 2, maxNewtonSteps 6, maxJets 8, maxExactEvaluations 2)`. Its
suite is unchanged. The smoke run accepts 11 of 12 voxels; voxel 7 is
`BudgetExceeded`, and the suite requires at least 10.

**F2: `ConditionC0QualificationSuite` hit munit's default 30 s timeout under
host load.** The landing review measured 42 s and 87 s for "bounded DEV paired
study…" and "unavailable-audit control…" at a load average of about 52.

The suite now sets `munitTimeout = 10 min`. Measured times:

| Run | DEV paired study | zero-admission control | unavailable-audit control |
| --- | --- | --- | --- |
| JVM, landing gate | 14.2 s | 11.5 s | 10.8 s |
| JVM, F2 gate | 10.1 s | 11.5 s | 16.4 s |
| JS, landing gate | 29.9 s | 27.4 s | 26.9 s |
| JS, F2 gate | 24.8 s | 23.2 s | 21.3 s |
| Loaded host (review) | 42 s | — | 87 s |

Ten minutes is about seven times the worst loaded measurement. No assertion
changed. This file's bytes therefore now differ from `8d5b0e51`; the manifest's
`postLandingAmendments` records it.

### Follow-up gates

| Gate | Result |
| --- | --- |
| `examplesCompile` | exit 0 |
| `workflowExamplesJVM/test` | 3/3 (includes `ProfileHrfConditionWorkflowSuite`) |
| `workflowExamplesJVM/runMain scalafim.examples.workflows.runAtlasMvpaWorkflow` (CLAUDE.md smoke) | exit 0 |
| `workflowExamplesJVM/runMain scalafim.examples.workflows.runProfileHrfConditionWorkflow` | exit 0; 11/12 voxels Accepted |
| `firstLevelLawsJVM/testOnly …ConditionC0QualificationSuite` | 8/8 |
| `firstLevelLawsJS/testOnly …ConditionC0QualificationSuite` | 8/8 |
| `hrfLawsJVM/test` | 82/82 |
| `hrfLawsJS/test` | 82/82 |
