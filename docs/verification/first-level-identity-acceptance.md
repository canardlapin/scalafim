# First-level scientific-identity acceptance ledger

This ledger preserves the retained wording of epic
`bd-01KZ3ZCDD2RMYQ3WVY606FNKY1` and phases 0–4. It maps each
criterion to current source and executable evidence. `not_run` means the
mapping is current but the command has not yet run against the final clean
candidate; historical task closure is not counted as fresh evidence. The
candidate-bound release report supplies final execution status.

## Evidence catalog

### E01: Public F-contrast exporter and fixture

- Owning modules: `fit`
- Public API/source: `tools/scenarios/export_fit_public_f_contrast_fixture.py`
- Suites and exact tests/scenarios: `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/PublicFContrastScenarioSuite.scala` (`fit.public-f-contrast.v1`)
- External fixtures/receipts: `docs/scenarios/fixtures/fit.public-f-contrast.v1.nilearn.json`
- Commands:
  - `python3 -S tools/scenarios/validate_manifest.py docs/scenarios/manifest.json`
  - `sbt fitJVM/test`
  - `sbt fitJS/test`

### E02: Focused portable CI lane

- Owning modules: `hrf`, `hrf-laws`, `design`, `model`, `fit`, `scenario-testkit`
- Public API/source: `.github/workflows/first-level.yml`, `tools/ci/first-level-gate.sh`
- Suites and exact tests/scenarios: repository-level structural check
- External fixtures/receipts: none claimed
- Commands:
  - `bash tools/ci/first-level-gate.sh`

### E03: Scenario manifest contract

- Owning modules: `scenario-testkit`
- Public API/source: `docs/scenarios/manifest.json`, `tools/scenarios/validate_manifest.py`, `tools/scenarios/test_validate_manifest.py`
- Suites and exact tests/scenarios: repository-level structural check
- External fixtures/receipts: none claimed
- Commands:
  - `python3 -S tools/scenarios/test_validate_manifest.py`
  - `python3 -S tools/scenarios/validate_manifest.py docs/scenarios/manifest.json`

### E04: Single ScenarioResult authority

- Owning modules: `scenario-testkit`
- Public API/source: `modules/scenario-testkit/shared/src/main/scala/scalafim/scenarios/ScenarioCore.scala`, `modules/scenario-testkit/shared/src/test/scala/scalafim/scenarios/ScenarioCoreSuite.scala`
- Suites and exact tests/scenarios: `modules/scenario-testkit/shared/src/test/scala/scalafim/scenarios/ScenarioCoreSuite.scala` (`clean results are the only default CI pass`; `failed observations and blocking caveats cannot be policy-approved`)
- External fixtures/receipts: none claimed
- Commands:
  - `sbt scenarioTestkitJVM/test`
  - `sbt scenarioTestkitJS/test`

### E05: Immutable compiled-design authority

- Owning modules: `design`, `model`, `fit`
- Public API/source: `modules/design/shared/src/main/scala/scalafim/fmri/design/DesignSchema.scala`, `modules/model/shared/src/main/scala/scalafim/fmri/model/DesignBlock.scala`, `modules/model/shared/src/main/scala/scalafim/fmri/model/FmriModel.scala`, `modules/fit/shared/src/main/scala/scalafim/fmri/fit/FitPlanExecutor.scala`
- Suites and exact tests/scenarios: `modules/design/shared/src/test/scala/scalafim/fmri/design/DesignSchemaOwnershipSuite.scala` (`compiled schema owns its input and exported matrix storage`; `relabeling and combining schemas cannot reopen matrix ownership`; `runwise slices retain the matrix bound to their coefficient axis`); `modules/fit/shared/src/test/scala/scalafim/fmri/fit/DesignMatrixOwnershipSuite.scala` (`a compiled hypothesis and ordinary or chunked fits share an immutable design`; `LSS uses the canonical design after event and baseline arrays are mutated`; `model construction rejects a matrix changed without rebuilding its schema`)
- External fixtures/receipts: none claimed
- Commands:
  - `sbt designJVM/test modelJVM/test fitJVM/test`
  - `sbt designJS/test modelJS/test fitJS/test`

### E06: Structural column origins and deterministic fingerprint

- Owning modules: `design`
- Public API/source: `modules/design/shared/src/main/scala/scalafim/fmri/design/DesignSchema.scala`, `modules/design/shared/src/main/scala/scalafim/fmri/design/DesignColmap.scala`
- Suites and exact tests/scenarios: `modules/design/shared/src/test/scala/scalafim/fmri/design/DesignSchemaSuite.scala` (`built event models retain structural cell and basis provenance`; `built baseline models retain run-scoped structural origins`; `fingerprints are deterministic and respond to semantic, row, and matrix changes`); `modules/design/shared/src/test/scala/scalafim/fmri/design/DesignColmapSuite.scala` (`designColmap(event) includes HRF basis metadata and covariate modulation`; `designColmap(baseline) includes drift/intercept roles and run ids`)
- External fixtures/receipts: none claimed
- Commands:
  - `sbt designJVM/test`
  - `sbt designJS/test`

### E07: Model, FitPlan, and fit-result coefficient axis

- Owning modules: `model`, `fit`
- Public API/source: `modules/model/shared/src/main/scala/scalafim/fmri/model/FmriModel.scala`, `modules/model/shared/src/main/scala/scalafim/fmri/model/FitPlan.scala`, `modules/fit/shared/src/main/scala/scalafim/fmri/fit/FmriFitResult.scala`
- Suites and exact tests/scenarios: `modules/model/shared/src/test/scala/scalafim/fmri/model/ModelSuite.scala` (`model and fit plan retain the canonical structural coefficient axis`; `compatibility label changes do not change structural identity`); `modules/fit/shared/src/test/scala/scalafim/fmri/fit/FitBlockSuite.scala` (`dense block merge preserves and validates the structural coefficient axis`)
- External fixtures/receipts: none claimed
- Commands:
  - `sbt modelJVM/test fitJVM/test`
  - `sbt modelJS/test fitJS/test`

### E08: Fingerprint-bound structural T/F hypotheses and estimability

- Owning modules: `fit`
- Public API/source: `modules/fit/shared/src/main/scala/scalafim/fmri/fit/StructuralHypotheses.scala`, `modules/fit/shared/src/main/scala/scalafim/fmri/fit/StructuralHypothesisDsl.scala`
- Suites and exact tests/scenarios: `modules/fit/shared/src/test/scala/scalafim/fmri/fit/StructuralHypothesisSuite.scala` (`a compiled hypothesis rejects a result from a different design fingerprint`; `rank-deficient designs reject non-estimable T contrasts and reduce F numerator rank`)
- External fixtures/receipts: none claimed
- Commands:
  - `sbt fitJVM/test`
  - `sbt fitJS/test`

### E09: Basis identities, omnibus tests, and response functionals

- Owning modules: `hrf`, `fit`
- Public API/source: `modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/Basis.scala`, `modules/fit/shared/src/main/scala/scalafim/fmri/fit/StructuralHypotheses.scala`
- Suites and exact tests/scenarios: `modules/fit/shared/src/test/scala/scalafim/fmri/fit/StructuralHypothesisSuite.scala` (`response-level functionals expand over semantic basis elements`); `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/StaticTypeContractsSuite.scala` (`basis coefficients remain owned by the basis that constructed them`)
- External fixtures/receipts: none claimed
- Commands:
  - `sbt hrfJVM/test fitJVM/test firstLevelLawsJVM/test`
  - `sbt hrfJS/test fitJS/test firstLevelLawsJS/test`

### E10: Phase-1 public scenario set

- Owning modules: `fit`
- Public API/source: `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/StructuralFirstLevelScenarioSuite.scala`
- Suites and exact tests/scenarios: `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/StructuralFirstLevelScenarioSuite.scala` (`fit.structural-s07-2x2.v1`; `fit.structural-s10-response-functional.v1`; `fit.structural-s16-rank-deficient.v1`)
- External fixtures/receipts: `docs/scenarios/fixtures/design.structural-2x2.v1.r.json`
- Commands:
  - `sbt fitJVM/testOnly scalafim.fmri.fit.scenarios.StructuralFirstLevelScenarioSuite`
  - `sbt fitJS/testOnly scalafim.fmri.fit.scenarios.StructuralFirstLevelScenarioSuite`

### E11: Compatibility surfaces retain structural authority

- Owning modules: `design`, `fit`
- Public API/source: `modules/design/shared/src/main/scala/scalafim/fmri/design/DesignSchema.scala`, `modules/fit/shared/src/main/scala/scalafim/fmri/fit/MatrixAdapters.scala`, `modules/fit/shared/src/main/scala/scalafim/fmri/fit/ResultArtifacts.scala`
- Suites and exact tests/scenarios: `modules/design/shared/src/test/scala/scalafim/fmri/design/DesignSchemaOwnershipSuite.scala` (`compiled schema owns its input and exported matrix storage`)
- External fixtures/receipts: none claimed
- Commands:
  - `bash tools/ci/first-level-gate.sh`

### E12: Declared factor levels and empty-cell policy

- Owning modules: `design`, `model`
- Public API/source: `modules/design/shared/src/main/scala/scalafim/fmri/design/FactorLevels.scala`, `modules/design/shared/src/main/scala/scalafim/fmri/design/DesignSchema.scala`
- Suites and exact tests/scenarios: `modules/model/shared/src/test/scala/scalafim/fmri/model/ModelBuilderSuite.scala` (`buildModel propagates declared factor levels through the public model path`)
- External fixtures/receipts: none claimed
- Commands:
  - `sbt designJVM/test modelJVM/test`
  - `sbt designJS/test modelJS/test`

### E13: Missing, centering, and ordered orthogonalization receipts

- Owning modules: `design`
- Public API/source: `modules/design/shared/src/main/scala/scalafim/fmri/design/DesignSchema.scala`, `modules/design/shared/src/main/scala/scalafim/fmri/design/ModulatorPolicies.scala`
- Suites and exact tests/scenarios: `modules/design/shared/src/test/scala/scalafim/fmri/design/scenarios/ModulatorPolicyScenarioSuite.scala` (`design.modulator-policies.v1`)
- External fixtures/receipts: `docs/scenarios/fixtures/design.modulator-policies.v1.r.json`
- Commands:
  - `sbt designJVM/testOnly scalafim.fmri.design.scenarios.ModulatorPolicyScenarioSuite`
  - `sbt designJS/testOnly scalafim.fmri.design.scenarios.ModulatorPolicyScenarioSuite`

### E14: Structurally assigned heterogeneous HRFs

- Owning modules: `design`, `model`, `fit`
- Public API/source: `modules/design/shared/src/main/scala/scalafim/fmri/design/HrfAssignments.scala`, `modules/model/shared/src/main/scala/scalafim/fmri/model/FmriModelBuilder.scala`
- Suites and exact tests/scenarios: `modules/design/shared/src/test/scala/scalafim/fmri/design/scenarios/HeterogeneousHrfScenarioSuite.scala` (`design.heterogeneous-hrf.v1`); `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/StructuralFirstLevelScenarioSuite.scala` (`fit.structural-s09-heterogeneous-hrf.v1`)
- External fixtures/receipts: `docs/scenarios/fixtures/design.heterogeneous-hrf.v1.r.json`
- Commands:
  - `sbt designJVM/test fitJVM/test`
  - `sbt designJS/test fitJS/test`

### E15: Validated fixed and estimated WLS preparation

- Owning modules: `model`, `fit`
- Public API/source: `modules/model/shared/src/main/scala/scalafim/fmri/model/FitConfig.scala`, `modules/fit/shared/src/main/scala/scalafim/fmri/fit/ResponsePreparation.scala`
- Suites and exact tests/scenarios: `modules/fit/shared/src/test/scala/scalafim/fmri/fit/ResponsePreparationSuite.scala` (`fixed weights equal OLS on square-root weighted design and response`; `estimated DVARS weighting is executable and records its source and normalization`); `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/StructuralFirstLevelScenarioSuite.scala` (`fit.structural-s18-wls.v1`)
- External fixtures/receipts: `docs/scenarios/fixtures/fit.structural-s18-wls.v1.r.json`
- Commands:
  - `sbt fitJVM/test`
  - `sbt fitJS/test`

### E16: Rank, alias, condition, residual-df, and estimability diagnostics

- Owning modules: `design`, `fit`
- Public API/source: `modules/fit/shared/src/main/scala/scalafim/fmri/fit/RankDiagnostics.scala`, `modules/fit/shared/src/main/scala/scalafim/fmri/fit/StructuralHypotheses.scala`
- Suites and exact tests/scenarios: `modules/design/shared/src/test/scala/scalafim/fmri/design/DesignSchemaSuite.scala` (`validated schemas compute scale-aware structural rank previews`); `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/StructuralFirstLevelScenarioSuite.scala` (`fit.structural-s16-rank-deficient.v1`)
- External fixtures/receipts: none claimed
- Commands:
  - `sbt designJVM/test fitJVM/test`
  - `sbt designJS/test fitJS/test`

### E17: Shared, runwise, and fixed-effects coefficient scope

- Owning modules: `model`, `fit`
- Public API/source: `modules/model/shared/src/main/scala/scalafim/fmri/model/CoefficientScope.scala`, `modules/fit/shared/src/main/scala/scalafim/fmri/fit/FixedEffects.scala`, `modules/fit/shared/src/main/scala/scalafim/fmri/fit/FirstLevelFixedEffectsEstimates.scala`
- Suites and exact tests/scenarios: `modules/model/shared/src/test/scala/scalafim/fmri/model/ModelSuite.scala` (`FitPlan exposes separate-runs-then-fixed-effects as an executable scope`); `modules/fit/shared/src/test/scala/scalafim/fmri/fit/FirstLevelFixedEffectsEstimatesSuite.scala` (`preparation reads no responses and blocked pooling matches independent mixed-TR R contrasts`; `explicit retained run coefficients match independent R values without changing pooled products`)
- External fixtures/receipts: `docs/scenarios/fixtures/fit.mixed-tr-fixed-effects.v1.r.json`
- Commands:
  - `sbt modelJVM/test fitJVM/test`
  - `sbt modelJS/test fitJS/test`

### E18: Phase-2 public scenario set

- Owning modules: `design`, `fit`
- Public API/source: `docs/scenarios/manifest.json`
- Suites and exact tests/scenarios: `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/StructuralFirstLevelScenarioSuite.scala` (`fit.structural-s06-factor-modulator.v1`; `fit.structural-s08-3x3-trends.v1`; `fit.structural-s09-heterogeneous-hrf.v1`; `fit.structural-s18-wls.v1`); `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/MixedTrFixedEffectsScenarioSuite.scala` (`fit.mixed-tr-fixed-effects.v1`)
- External fixtures/receipts: `docs/scenarios/fixtures/fit.structural-s18-wls.v1.r.json`, `docs/scenarios/fixtures/fit.mixed-tr-fixed-effects.v1.r.json`
- Commands:
  - `bash tools/ci/first-level-gate.sh`

### E19: Multiphase trial provenance

- Owning modules: `design`
- Public API/source: `modules/design/shared/src/main/scala/scalafim/fmri/design/event/EventPhase.scala`, `modules/design/shared/src/main/scala/scalafim/fmri/design/DesignSchema.scala`
- Suites and exact tests/scenarios: `modules/design/shared/src/test/scala/scalafim/fmri/design/scenarios/MultiphaseProvenanceScenarioSuite.scala` (`design.multiphase-provenance.v1`)
- External fixtures/receipts: none claimed
- Commands:
  - `sbt designJVM/test`
  - `sbt designJS/test`

### E20: Executable 50-column DMS model, fits, and semantic hypotheses

- Owning modules: `design`, `model`, `fit`
- Public API/source: `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/DelayedMatchToSampleDslScenarioSuite.scala`, `docs/first-level-analysis.md`
- Suites and exact tests/scenarios: `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/DelayedMatchToSampleDslScenarioSuite.scala` (`fit.dms-multiphase-dsl.v1`)
- External fixtures/receipts: `docs/scenarios/fixtures/fit.dms-multiphase-dsl.corrected-spmg.v1.r.json`
- Commands:
  - `python3 -S tools/docs/check_first_level_docs.py --check`
  - `sbt fitJVM/testOnly scalafim.fmri.fit.scenarios.DelayedMatchToSampleDslScenarioSuite`
  - `sbt fitJS/testOnly scalafim.fmri.fit.scenarios.DelayedMatchToSampleDslScenarioSuite`

### E21: Mixed sustained/transient public workflow

- Owning modules: `fit`
- Public API/source: `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/MixedBlockNuisanceScenarioSuite.scala`
- Suites and exact tests/scenarios: `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/MixedBlockNuisanceScenarioSuite.scala` (`fit.mixed-block-transient.v1`)
- External fixtures/receipts: none claimed
- Commands:
  - `sbt fitJVM/testOnly scalafim.fmri.fit.scenarios.MixedBlockNuisanceScenarioSuite`
  - `sbt fitJS/testOnly scalafim.fmri.fit.scenarios.MixedBlockNuisanceScenarioSuite`

### E22: Realistic nuisance and independent corrected-SPMG inference

- Owning modules: `fit`
- Public API/source: `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/MixedBlockNuisanceScenarioSuite.scala`
- Suites and exact tests/scenarios: `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/MixedBlockNuisanceScenarioSuite.scala` (`fit.realistic-nuisance.v1`)
- External fixtures/receipts: `docs/scenarios/fixtures/fit.realistic-nuisance.corrected-spmg.v1.r.json`
- Commands:
  - `sbt fitJVM/testOnly scalafim.fmri.fit.scenarios.MixedBlockNuisanceScenarioSuite`
  - `sbt fitJS/testOnly scalafim.fmri.fit.scenarios.MixedBlockNuisanceScenarioSuite`

### E23: Run/censor-boundary AR and GLS

- Owning modules: `ar`, `fit`
- Public API/source: `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/ArCensorBoundaryGlsScenarioSuite.scala`
- Suites and exact tests/scenarios: `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/ArCensorBoundaryGlsScenarioSuite.scala` (`fit.ar-censor-boundary-gls.v1`)
- External fixtures/receipts: `docs/scenarios/fixtures/fit.ar-censor-boundary-gls.v1.r.json`
- Commands:
  - `sbt fitJVM/testOnly scalafim.fmri.fit.scenarios.ArCensorBoundaryGlsScenarioSuite`
  - `sbt fitJS/testOnly scalafim.fmri.fit.scenarios.ArCensorBoundaryGlsScenarioSuite`

### E24: Deterministic generated-law inputs and shrinking

- Owning modules: `first-level-laws`
- Public API/source: `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/FirstLevelGenerators.scala`, `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/ArLawGenerators.scala`, `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/MissingResponseGenerators.scala`, `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/LawRunProfile.scala`
- Suites and exact tests/scenarios: `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/DesignGeneratedLawsSuite.scala` (`generated explicit and mixed acquisition grids preserve run-local and global axes`; `generated factor, missingness, provenance, and policy audits remain internally consistent`); `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/HrfGeneratedLawsSuite.scala` (`generated impulse and epoch responses are additive, homogeneous, and translation equivariant`; `basis permutations transport coefficients without changing the reconstructed response`); `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/FitGeneratedLawsSuite.scala` (`generated full-rank multiresponse systems recover their planted coefficients`; `rank-deficient systems compare fitted geometry and estimable functions, never arbitrary betas`); `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/ArGeneratedLawsSuite.scala` (`fixed-order AR estimation is invariant to scale, signs, and response-column permutation`); `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/MissingResponseGeneratedLawsSuite.scala` (`propagated missing responses equal explicit finite-voxel fits across selection, chunking, and engines`)
- External fixtures/receipts: none claimed
- Commands:
  - `sbt firstLevelLawsJVM/test`
  - `sbt firstLevelLawsJS/test`

### E25: Metamorphic first-level invariants

- Owning modules: `hrf`, `design`, `fit`, `first-level-laws`
- Public API/source: `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws`, `modules/fit/shared/src/test/scala/scalafim/fmri/fit/PreparedContrastGeometrySuite.scala`
- Suites and exact tests/scenarios: `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/DesignGeneratedLawsSuite.scala` (`within-run event-row permutation preserves structural origins and the compiled numerical design`; `term permutation is column-equivariant when aligned by structural origin`); `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/HrfGeneratedLawsSuite.scala` (`generated pulse partitions agree and numerical quadrature converges under refinement`; `basis permutations transport coefficients without changing the reconstructed response`); `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/FitGeneratedLawsSuite.scala` (`column scaling transports coefficients and preserves fitted geometry`; `public fixed WLS preparation is the declared square-root-weight transformation`; `multiresponse fitting is invariant to response-column chunking`; `known AR(1) GLS equals independently whitened OLS and resets at censor gaps`); `modules/fit/shared/src/test/scala/scalafim/fmri/fit/PreparedContrastGeometrySuite.scala` (`rescaling a one-row contrast preserves the effect matrix`; `invertible nuisance basis changes preserve effect and residual geometry`); `modules/fit/shared/src/test/scala/scalafim/fmri/fit/FitBlockSuite.scala` (`dense OLS block results merge to the same result as a full voxel block`)
- External fixtures/receipts: none claimed
- Commands:
  - `bash tools/ci/first-level-gate.sh`

### E26: Versioned external receipts and declared reference locks

- Owning modules: `design`, `fit`
- Public API/source: `docs/scenarios/manifest.json`, `tools/r-parity/reference-lock.json`, `tools/r-parity/mixed-tr-reference-lock.json`, `tools/r-parity/receipt_tools.py`
- Suites and exact tests/scenarios: repository-level structural check
- External fixtures/receipts: `docs/scenarios/fixtures/design.structural-2x2.v1.r.json`, `docs/scenarios/fixtures/fit.dms-multiphase-dsl.corrected-spmg.v1.r.json`, `docs/scenarios/fixtures/fit.realistic-nuisance.corrected-spmg.v1.r.json`
- Commands:
  - `python3 -S tools/r-parity/test_receipt_tools.py`
  - `python3 -S tools/scenarios/validate_manifest.py docs/scenarios/manifest.json`

### E27: Separated PR, scheduled, regeneration, performance, and release lanes

- Owning modules: `repository`
- Public API/source: `.github/workflows/first-level.yml`, `.github/workflows/first-level-laws-calibration.yml`, `.github/workflows/scenario-receipts.yml`, `.github/workflows/first-level-performance.yml`, `tools/ci/first-level-release.sh`
- Suites and exact tests/scenarios: repository-level structural check
- External fixtures/receipts: none claimed
- Commands:
  - `bash -n tools/ci/first-level-release.sh`
  - `python3 -S tools/ci/test_collect_first_level_run_evidence.py`
  - `python3 -S tools/ci/test_collect_first_level_branch_evidence.py`

### E28: Compiler, formatting, coverage, compatibility, and performance policy

- Owning modules: `repository`
- Public API/source: `build.sbt`, `docs/release-assurance.md`, `tools/ci/first-level-coverage.sh`, `tools/ci/first-level-benchmark.sh`
- Suites and exact tests/scenarios: repository-level structural check
- External fixtures/receipts: none claimed
- Commands:
  - `sbt scalafimCompileAll`
  - `bash tools/ci/first-level-coverage.sh`
  - `bash tools/ci/first-level-benchmark.sh`

### E29: Executable public DMS documentation

- Owning modules: `fit`, `docs`
- Public API/source: `docs/first-level-analysis.md`, `tools/docs/check_first_level_docs.py`, `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/DelayedMatchToSampleDslScenarioSuite.scala`
- Suites and exact tests/scenarios: `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/DelayedMatchToSampleDslScenarioSuite.scala` (`fit.dms-multiphase-dsl.v1`)
- External fixtures/receipts: `docs/scenarios/fixtures/fit.dms-multiphase-dsl.corrected-spmg.v1.r.json`
- Commands:
  - `python3 -S tools/docs/check_first_level_docs.py --check`
  - `sbt fitJVM/testOnly scalafim.fmri.fit.scenarios.DelayedMatchToSampleDslScenarioSuite`
  - `sbt fitJS/testOnly scalafim.fmri.fit.scenarios.DelayedMatchToSampleDslScenarioSuite`

### E30: Candidate-bound full release court

- Owning modules: `repository`
- Public API/source: `tools/ci/first-level-release.sh`, `tools/ci/finalize_first_level_release.py`, `tools/ci/full-repository-tests.sh`
- Suites and exact tests/scenarios: repository-level structural check
- External fixtures/receipts: none claimed
- Commands:
  - `bash tools/ci/first-level-release.sh`
  - `python3 -S tools/ci/finalize_first_level_release.py --check <external-report-path>`

### E31: Candidate-bound HRF, AR, planning, reuse, multiresponse, and chunk benchmarks

- Owning modules: `hrf-bench`, `fit-bench`
- Public API/source: `benchmarks/hrf-jvm/src/main/scala/scalafim/fmri/hrf/BasisResponseBenchmark.scala`, `benchmarks/hrf-jvm/src/main/scala/scalafim/fmri/hrf/RegressorConvolutionBenchmark.scala`, `benchmarks/fit-jvm/src/main/scala/scalafim/fmri/ar/ArEstimationBenchmark.scala`, `benchmarks/fit-jvm/src/main/scala/scalafim/fmri/fit/FirstLevelFitBenchmark.scala`, `tools/benchmark/first-level-budgets.json`, `tools/benchmark/finalize_first_level_receipt.py`
- Suites and exact tests/scenarios: repository-level structural check
- External fixtures/receipts: none claimed
- Commands:
  - `bash tools/ci/first-level-benchmark.sh`
  - `python3 -S tools/benchmark/finalize_first_level_receipt.py --check <external-receipt-path>`

### E32: First-level estimates and fit-estimates bridge

- Owning modules: `estimates`, `fit-estimates`, `group`
- Public API/source: `modules/estimates/shared/src/main/scala/scalafim/estimates/EstimateSet.scala`, `modules/estimates/shared/src/main/scala/scalafim/estimates/EstimandCatalog.scala`, `modules/fit-estimates/shared/src/main/scala/scalafim/fmri/fit/estimates/FitEstimateProducer.scala`, `modules/fit-estimates/shared/src/main/scala/scalafim/fmri/fit/estimates/FitGroupAdapter.scala`
- Suites and exact tests/scenarios: `modules/fit-estimates/shared/src/test/scala/scalafim/fmri/group/scenarios/GroupFirstLevelBridgeScenarioSuite.scala` (`group.first-level-bridge.v1`); `modules/fit-estimates/shared/src/test/scala/scalafim/fmri/group/scenarios/FirstLevelToGroupKnownEffectScenarioSuite.scala` (`group.first-level-to-group-known-effect.v1`)
- External fixtures/receipts: none claimed
- Commands:
  - `sbt fitEstimatesJVM/test`
  - `sbt fitEstimatesJS/test`

## Global acceptance criteria

| ID | Retained criterion | Evidence | Current status | Source candidate |
| --- | --- | --- | --- | --- |
| G1 | Every realized design column has one stable structural origin containing the scientifically relevant term, cell, modulator, basis, role, and run scope. | [E06](#e06) | `not_run` | `—` |
| G2 | Matrix, row layout, structural column schema, audit, and deterministic design fingerprint remain inseparable through the model boundary. | [E05](#e05), [E07](#e07) | `not_run` | `—` |
| G3 | A compiled hypothesis is bound to the intended design fingerprint and structural column identities and fails before evaluation on a different design. | [E08](#e08) | `not_run` | `—` |
| G4 | Coefficient hypotheses, basis omnibus tests, and linear response functionals are explicit and unambiguous. | [E08](#e08), [E09](#e09) | `not_run` | `—` |
| G5 | Factor levels, empty cells, missing modulators, centering, weighting, rank, censoring, and run coefficient scope have inspectable policies and receipts. | [E12](#e12), [E13](#e13), [E15](#e15), [E16](#e16), [E17](#e17), [E23](#e23) | `not_run` | `—` |
| G6 | The public DMS workflow constructs and tests the expected 50 task columns without manual table explosion, column offsets, name parsing, or numeric contrast bookkeeping. | [E20](#e20), [E29](#e29) | `not_run` | `—` |
| G7 | Each accepted capability has JVM and Scala.js tests, suitable algebraic or metamorphic checks, and independent numerical evidence where parity is claimed. | [E02](#e02), [E10](#e10), [E18](#e18), [E20](#e20), [E22](#e22), [E23](#e23), [E25](#e25), [E26](#e26) | `not_run` | `—` |
| G8 | Scenario manifests, fixture receipts, external generators, CI gates, documentation, and performance claims agree with the implemented public surface. | [E03](#e03), [E26](#e26), [E27](#e27), [E29](#e29), [E31](#e31), [E32](#e32) | `not_run` | `—` |
| G9 | Full focused module gates and ultimately compileAll/testAll are warning-clean before epic closure. | [E02](#e02), [E28](#e28), [E30](#e30) | `not_run` | `—` |

## Phase 0 acceptance criteria

| ID | Retained criterion | Evidence | Current status | Source candidate |
| --- | --- | --- | --- | --- |
| P0.1 | The external fixture exporter is current, deterministic, and passes its own check mode without changing numerical truth accidentally. | [E01](#e01), [E26](#e26) | `not_run` | `—` |
| P0.2 | Pull requests run hrf, hrf-laws, design, model, and fit on JVM and Scala.js plus manifest and fixture freshness checks. | [E02](#e02) | `not_run` | `—` |
| P0.3 | Active scenarios have validated IDs, suite paths, platform claims, allowed statuses, reference strategies, and acceptance commands. | [E03](#e03) | `not_run` | `—` |
| P0.4 | ScenarioResult remains the single truth value; undeclared caveats cannot pass CI. | [E04](#e04) | `not_run` | `—` |
| P0.5 | The shared harness has one authority with module-specific numeric adapters and no production dependency cycle. | [E04](#e04) | `not_run` | `—` |
| P0.6 | The phase closes only after its child tasks and the focused public-consumer scenario paths pass. | [E02](#e02), [E03](#e03) | `not_run` | `—` |

## Phase 1 acceptance criteria

| ID | Retained criterion | Evidence | Current status | Source candidate |
| --- | --- | --- | --- | --- |
| P1.1 | Matrix, row layout, structural column schema, design audit, and fingerprint form one validated compiled-design authority. | [E05](#e05) | `not_run` | `—` |
| P1.2 | Each event, baseline, drift, intercept, sampled, and nuisance column has a stable structural origin; labels are rendered from that origin. | [E06](#e06) | `not_run` | `—` |
| P1.3 | Name/suffix parsing is removed from authoritative semantic paths and retained only in explicit compatibility adapters where required. | [E06](#e06), [E11](#e11) | `not_run` | `—` |
| P1.4 | Model and fit values preserve the design schema and fingerprint without duplicating parallel column truth. | [E05](#e05), [E07](#e07) | `not_run` | `—` |
| P1.5 | Compiled T/F hypotheses bind to structural columns and the design fingerprint and carry estimability evidence. | [E08](#e08) | `not_run` | `—` |
| P1.6 | Basis elements have stable identities and roles, and coefficient tests are distinct from response-level linear functionals. | [E09](#e09) | `not_run` | `—` |
| P1.7 | S07, S10, and S16 are active public-path scenarios on JVM and Scala.js. | [E10](#e10) | `not_run` | `—` |
| P1.8 | Existing formula, design export, fit, LSS, GLS, robust, and result-manifest behavior remains source-compatible where practical. | [E11](#e11), [E30](#e30), [E32](#e32) | `not_run` | `—` |

## Phase 2 acceptance criteria

| ID | Retained criterion | Evidence | Current status | Source candidate |
| --- | --- | --- | --- | --- |
| P2.1 | Declared factor level sets and empty-cell policies are stable across runs and subjects and are recorded in the design audit. | [E12](#e12) | `not_run` | `—` |
| P2.2 | Missing continuous modulators, centering scope, and any supported orthogonalization are selected explicitly and produce receipts. | [E13](#e13) | `not_run` | `—` |
| P2.3 | Heterogeneous HRF basis widths can be assigned structurally by cell or phase without manual offsets. | [E14](#e14) | `not_run` | `—` |
| P2.4 | Fixed and estimated volume weights are either applied with validated WLS semantics or rejected before execution; no supported configuration remains merely deferred. | [E15](#e15) | `not_run` | `—` |
| P2.5 | Rank diagnostics identify numerical rank, pivoting/alias information, condition evidence, residual degrees of freedom, and hypothesis estimability. | [E16](#e16) | `not_run` | `—` |
| P2.6 | Shared-across-run, run-specific, and separate-run/fixed-effects coefficient meanings are inspectable model semantics. | [E17](#e17) | `not_run` | `—` |
| P2.7 | S06, S08, S09, S13 extension, and S18 pass through public APIs on JVM and Scala.js. | [E18](#e18) | `not_run` | `—` |

## Phase 3 acceptance criteria

| ID | Retained criterion | Evidence | Current status | Source candidate |
| --- | --- | --- | --- | --- |
| P3.1 | One conceptual trial can lower to multiple phases while retaining parent trial identity, phase identity, factors, modulators, run identity, and original-row provenance. | [E19](#e19), [E20](#e20) | `not_run` | `—` |
| P3.2 | The DMS scenario constructs exactly 50 task columns with the declared structural origins and no manual table explosion, column offsets, or name parsing. | [E20](#e20), [E29](#e29) | `not_run` | `—` |
| P3.3 | DMS hypotheses cover sample response difference, delay load trend, probe mismatch, interaction, RT slope, basis omnibus, FIR omnibus, and a deliberately non-estimable case. | [E20](#e20) | `not_run` | `—` |
| P3.4 | A mixed sustained/transient scenario covers blocks, cues, errors or feedback, variable epochs, and phase-specific HRFs. | [E21](#e21) | `not_run` | `—` |
| P3.5 | A realistic nuisance scenario exercises motion-derived terms, CompCor-like components, spikes, constants, and near collinearity. | [E22](#e22) | `not_run` | `—` |
| P3.6 | A public AR scenario proves that whitening never crosses runs or censor gaps. | [E23](#e23) | `not_run` | `—` |
| P3.7 | All scenarios return one clean ScenarioResult and use only published/public module surfaces on both platforms. | [E02](#e02), [E03](#e03), [E04](#e04), [E20](#e20), [E21](#e21), [E22](#e22), [E23](#e23) | `not_run` | `—` |

## Phase 4 acceptance criteria

| ID | Retained criterion | Evidence | Current status | Source candidate |
| --- | --- | --- | --- | --- |
| P4.1 | Deterministic generators and shrinking cover acquisition grids, events, factors, modulators, bases, permutations, censor segments, rank stress, and multiresponse data. | [E24](#e24) | `not_run` | `—` |
| P4.2 | Metamorphic checks cover event/term/column/basis permutation, amplitude superposition, epoch partition, basis transport, column scaling, WLS/GLS transformations, multiresponse, and chunking. | [E25](#e25) | `not_run` | `—` |
| P4.3 | External fixtures have versioned receipts, hashes, producer commands, package revisions, conventions, and documented accepted differences. | [E26](#e26) | `not_run` | `—` |
| P4.4 | PR, nightly, and release-candidate lanes are separated; live R/Python regeneration does not make ordinary portable tests environment-dependent. | [E02](#e02), [E27](#e27) | `not_run` | `—` |
| P4.5 | Compiler, formatting, coverage, compatibility, and performance gates are adopted according to the pre-release support promise and documented honestly. | [E28](#e28), [E31](#e31) | `not_run` | `—` |
| P4.6 | Documentation examples compile and the DMS workflow is executable documentation. | [E20](#e20), [E29](#e29) | `not_run` | `—` |
| P4.7 | compileAll and testAll pass, fixture checks are fresh, benchmark claims have source/runtime receipts, and no unexplained oracle disagreement remains. | [E26](#e26), [E30](#e30), [E31](#e31) | `not_run` | `—` |
