# ScalaFIM Scenario Parity Harness

This plan adapts the useful parts of the sibling Python `fmrimod`
scenario/parity system to ScalaFIM. The goal is not to run Python from sbt or
copy fmrimod's API shape. The goal is to make realistic fMRI workflows
executable as typed, cross-compiled Scala tests with named numerical outputs,
declared tolerances, explicit caveats, and regenerable evidence.

`fmrimod` has four durable ideas worth borrowing:

1. A scenario is a deterministic workflow, not just a unit test.
2. Candidate and reference pipelines emit named numeric arrays.
3. Comparison is done through per-output tolerances and caveats, then rendered
   into a receipt.
4. A manifest records which workflows are public-seam proof, lower-level
   canaries, or known algorithm-divergence cases.

ScalaFIM should emulate those ideas with Scala 3 values, MUnit suites, and
generated fixtures that run on both JVM and Scala.js.

## Design Position

Scenario tests must respect the same module rules as production code:

- Shared JVM/Scala.js tests are the default. A scenario that validates shared
  numerical behavior belongs under `modules/<module>/shared/src/test`.
- Python, R, Nilearn, and external R packages are fixture generators only. They
  may live under `tools/scenarios/`, but they must not be required by
  `sbt testAll`.
- Public-seam scenarios are preferred over private-kernel canaries once the
  public Scala API exists. A private-kernel canary must name the public API gap
  that will replace it.
- Scenario output is typed and named. Do not compare anonymous tuples or
  position-only arrays when the model has column, term, contrast, subject, run,
  or voxel identities.
- Caveats are first-class values, not comments. A scenario with a known
  divergence can pass only as `pass_with_caveats`.

The harness should be small and dependency-light. It should use `DoubleVector`,
`DoubleMatrix`, primitive arrays, and MUnit assertions. It should not pull JSON,
Python, or filesystem assumptions into shared tests.

## Oderskyan Core

The harness should feel like Scala, not like a miniature Python test runner
translated into Scala syntax. Keep the core as a small algebra of immutable
values plus total functions:

- A scenario is a value: inputs, candidate pipeline, reference pipeline,
  tolerances, expected caveats, and metadata.
- Running a scenario is a function from that value to a `ScenarioResult`.
- A result always has one terminal verdict. Logs, receipts, and assertion
  messages explain the verdict; they do not define it.
- Exhaustive `enum` matches own closed choices. Do not encode status, tier,
  caveat policy, platform, or reference type as free strings in the executable
  core.
- Smart constructors validate invariants once: non-empty ids, finite
  tolerances, unique output names, declared caveat ids, and shape agreement
  where the shape is known before comparison.
- Invalid states should be impossible or explicit. Missing outputs, duplicate
  arrays, undeclared caveats, non-finite values, stale fixtures, and failed
  tolerances must all lower to `ScenarioStatus.Fail`, not to ad hoc warnings.
- Syntax can be pleasant, but it should be syntax over values. Add extension
  methods and small DSL helpers only after the algebra is clear.

The intended shape is:

```scala
val scenario =
  ScenarioCase(
    id = ScenarioId("fit_public_f_confound_drift"),
    tier = ScenarioTier.WorkflowParity,
    candidate = scalafimPipeline,
    reference = referencePipeline,
    tolerances = tolerances,
    expectedCaveats = Vector.empty
  )

val result = ScenarioHarness.run(scenario)
assert(result.ciPass, result.renderSummary)
```

That is the whole contract: compose a typed case, run it, receive one typed
result, and let the test assert the result's CI truth value.

## Harness Shape

Start with test-scope helpers local to the first module that needs them. Promote
to a small shared testkit only after two or more modules duplicate the same
types.

Initial home:

```text
modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/
  ScenarioHarness.scala
  FitScenarioFixtures.scala
  PublicFContrastScenarioSuite.scala
```

Likely later homes:

```text
modules/group/shared/src/test/scala/scalafim/fmri/group/scenarios/
modules/ar/shared/src/test/scala/scalafim/fmri/ar/scenarios/
tools/scenarios/
docs/scenarios/
```

The minimal harness types should look like this conceptually:

```scala
enum ScenarioStatus:
  case Pass, PassWithCaveats, Fail

  def ciPass: Boolean =
    this match
      case Pass | PassWithCaveats => true
      case Fail                   => false

enum ScenarioTier:
  case NumericCanary, WorkflowParity, AlgorithmDivergence, CrossLevelWorkflow, FlagshipWorkflow

opaque type ScenarioId = String

object ScenarioId:
  def apply(value: String): ScenarioId =
    val trimmed = value.trim
    require(trimmed.nonEmpty, "scenario id must be non-empty")
    trimmed

final case class ScenarioTolerance(
    rtol: Double = 1e-6,
    atol: Double = 1e-9,
    minPearson: Double = 0.999,
    minSpearman: Double = 0.999,
    checkAllClose: Boolean = true,
    maxMae: Option[Double] = None,
    maxAbs: Option[Double] = None
)

final case class ScenarioArray(name: String, shape: Vector[Int], values: Vector[Double])
final case class ScenarioOutput(arrays: Map[String, ScenarioArray])
final case class ScenarioDelta(name: String, maxAbs: Double, mae: Double, pearson: Double, passes: Boolean)
final case class ScenarioCaveat(id: String, quantity: String, reason: String, expected: String)
final case class ScenarioResult(
    id: ScenarioId,
    tier: ScenarioTier,
    status: ScenarioStatus,
    deltas: Vector[ScenarioDelta],
    caveats: Vector[ScenarioCaveat],
    failures: Vector[String]
):
  def ciPass: Boolean = status.ciPass
  def renderSummary: String =
    s"$id: $status (${deltas.count(_.passes)}/${deltas.length} deltas passed, ${failures.length} failures)"
```

The first implementation can be simpler than the sketch: only allclose,
max-absolute-error, and shape checks are necessary for the first OLS scenarios.
Pearson/Spearman gates become useful once AR or large-voxel parity introduces
small algorithmic differences.

Even the first implementation must keep the terminal-verdict contract. A test
suite should not scatter many unrelated assertions and then infer success from
"no exception happened". It should build a `ScenarioResult` and assert
`result.ciPass`; any lower-level assertion helpers should feed the result's
`deltas`, `caveats`, and `failures`.

`PassWithCaveats` is green only under a strict policy:

- every numerical gate that remains active passes;
- every bypassed or relaxed gate is tied to a declared caveat;
- every declared caveat appears in the scenario metadata and receipt;
- no undeclared caveat or unexpected output mismatch is present.

Otherwise the result is `Fail`.

## Fixture Policy

Use three reference strategies, in this order:

1. **Mathematical oracle.** Hand-computable or closed-form expected values.
   Good for OLS, one-sample group t, FDR, and matrix invariants.
2. **Generated Scala fixture.** A Python/R/fmrimod/Nilearn script writes a
   Scala object under `shared/src/test/scala/.../fixtures`, matching the
   existing `RParityFixtures.scala` pattern. This keeps tests portable.
3. **JVM-only receipt renderer.** A tool can regenerate JSON/Markdown receipts
   for human audit, but receipt generation is separate from the cross-platform
   test pass.

Do not put large JSON resource files in the first cut. Scala.js resource
loading and filesystem semantics are extra moving parts. Generated Scala
objects are boring, reviewable, and already match the repository's fixture
style.

Every generated fixture file must include:

- generator command,
- source repository or package,
- date or source commit when available,
- declared tolerance rationale,
- whether the reference is R, fmrimod, Nilearn, or mathematical.

## Scenario Tiers

Use a small classification so we know what a scenario proves.

| Tier | Meaning | Example |
| --- | --- | --- |
| `numeric_canary` | Private kernel or small numerical identity; useful but not public workflow proof. | OLS coefficients from a fixed design matrix. |
| `workflow_parity` | Public Scala workflow agrees with an external or mathematical reference. | Dataset + model + fit + t/F contrast. |
| `algorithm_divergence` | Same scientific target, known algorithmic differences, relaxed gates and caveats. | AR(1) prewhitening against Nilearn-style pooling. |
| `cross_level_workflow` | Data moves across module boundaries. | First-level contrasts into group one-sample t. |
| `flagship_workflow` | End-to-end narrative scenario used as release evidence. | Simulated subjects, first-level contrast, group inference, receipt. |

The manifest should eventually record these fields:

```json
{
  "schema_version": "scalafim-scenario-manifest/v1",
  "scenarios": [
    {
      "id": "fit_public_f_confound_drift",
      "tier": "workflow_parity",
      "module": "fit",
      "suite": "scalafim.fmri.fit.scenarios.PublicFContrastScenarioSuite",
      "public_seam": true,
      "reference": "fmrimod:tier_a_f_confound_drift_public",
      "allowed_statuses": ["pass"],
      "receipt": "docs/scenarios/fit_public_f_confound_drift.json",
      "caveats": []
    }
  ]
}
```

The manifest should not precede the first real scenarios. Add it after there
are at least two executable cases, so its fields reflect actual use.

## Scenario Catalog

The catalog should be broad enough to guide years of coverage, but it must land
in small slices. Each row below is a candidate scenario with a stable id,
owning module, and reference strategy. A row becomes active only when its public
Scala seam exists or the row explicitly declares itself as a private canary
with a replacement target.

### Wave 1: Dense First-Level And Group Core

These scenarios should land first because they cover the current
`design -> model -> fit -> group` spine and can run entirely from mathematical
or in-memory Scala fixtures.

| Scenario id | Tier | Owner | Reference | Workflow risk protected |
| --- | --- | --- | --- | --- |
| `fit_public_f_confound_drift` | `workflow_parity` | `fit` | mathematical first, fmrimod/Nilearn fixture second | Task regressors, nuisance columns, drift, t contrast, and F contrast stay aligned through the public model/fit seam. |
| `fit_nuisance_adjusted_ols` | `numeric_canary` -> `workflow_parity` | `fit` | mathematical | Nuisance projection does not absorb the task effect or silently retain duplicate/constant nuisance columns. |
| `fit_semantic_contrast_reordered_columns` | `workflow_parity` | `fit` | mathematical | Name-keyed contrasts survive design-column order changes and categorical level ordering. |
| `fit_lss_trialwise_recovery` | `workflow_parity` | `fit` | mathematical | LSS chooses trial columns by metadata and recovers injected per-trial amplitudes. |
| `fit_runwise_ols_pooling` | `workflow_parity` | `fit` | mathematical | Runwise fits preserve run metadata and combine estimates without swapping voxels or terms. |
| `design_hrf_factorial_2x2` | `workflow_parity` | `design` | fmridesign/fmrimod fixture | Crossed factors produce stable design metadata and generated omnibus contrasts. |
| `design_parametric_modulator_centering` | `workflow_parity` | `design` | fmridesign/fmrimod fixture | Parametric modulators are centered/scaled as intended and keep term provenance. |
| `design_baseline_runwise_drift` | `workflow_parity` | `design` | fmridesign fixture | Runwise intercept and drift columns are block-local and rank-stable. |
| `ar_yule_walker_whitening` | `numeric_canary` | `ar` | mathematical | Estimated AR plans reduce lag-one residual autocorrelation and keep segment/run boundaries. |
| `group_one_sample_t` | `workflow_parity` | `group` | mathematical | Group intercept-only OLS matches the standard one-sample t-test. |
| `group_two_sample_t` | `workflow_parity` | `group` | mathematical | Two-sample design terms and named group contrasts reproduce pooled two-sample tests. |
| `group_first_level_bridge` | `cross_level_workflow` | `group` | mathematical | `TContrastResult` estimates and standard errors become group effects/variances without losing subject/sample ids. |

### Wave 2: Realistic First-Level Stress Tests

These borrow directly from fmrimod's Tier A/Tier B workflow ideas. Most should
use generated fmrimod/Nilearn fixtures after a first mathematical or internal
oracle proves the Scala path is coherent.

| Scenario id | Tier | Owner | Reference | Workflow risk protected |
| --- | --- | --- | --- | --- |
| `first_level_to_group_known_effect` | `cross_level_workflow` | `group` | mathematical | Simulated subject effects survive first-level fitting, contrast evaluation, bridge conversion, and group inference. |
| `fit_censored_multirun_concat` | `workflow_parity` | `fit` | fmrimod/Nilearn fixture | Censor masks row-delete design and response data consistently and report correct residual df. |
| `design_mixed_tr_multirun` | `workflow_parity` | `design` | fmrimod/Nilearn fixture | Heterogeneous per-run TRs use correct sampling grids, run-local baselines, and cross-run condition columns. |
| `fit_mixed_tr_cross_run_contrast` | `workflow_parity` | `fit` | fmrimod/Nilearn fixture | Cross-run contrasts evaluate on the concatenated design rather than per-run isolated fits. |
| `fit_realistic_confounds_motion_omnibus` | `workflow_parity` | `fit` | fmrimod/Nilearn fixture | fMRIPrep-style motion/confound columns feed task and motion omnibus contrasts without manual index bookkeeping. |
| `fit_fir_basis_recovery` | `workflow_parity` | `fit` | fmrimod/Nilearn fixture | FIR basis columns recover block/epoch effects and preserve basis metadata. |
| `fit_block_epoch_durations` | `workflow_parity` | `fit` | fmrimod/Nilearn fixture | Nonzero event durations are convolved and contrasted correctly. |
| `fit_factorial_3way_rank_diagnostics` | `workflow_parity` | `fit` | fmrimod fixture | Factorial expansion remains inspectable and rank diagnostics catch non-estimable hypotheses. |
| `fit_parametric_interaction` | `workflow_parity` | `fit` | fmrimod fixture | Parametric-by-condition effects are recoverable and named. |
| `ar1_prewhitening_divergence` | `algorithm_divergence` | `ar` + `fit` | fmrimod/Nilearn fixture | AR(1) prewhitening agrees on effect ranking while documenting estimation-policy differences. |
| `fit_robust_outlier_downweighting` | `algorithm_divergence` | future `fit` or `robust` | mathematical/fmrimod fixture | Outlier frames or voxels receive lower influence without changing the ordinary OLS path. |
| `fit_reduced_rank_sketch_recovery` | `algorithm_divergence` | future `fit` | fmrimod fixture | Low-rank/sketched engines stay close to dense OLS and report approximation caveats. |

### Wave 3: Dataset, BIDS, And IO Boundaries

These scenarios should keep IO adapters outside shared tests unless the data is
tiny and platform-specific by design.

| Scenario id | Tier | Owner | Reference | Workflow risk protected |
| --- | --- | --- | --- | --- |
| `dataset_inmemory_selection_orientation` | `numeric_canary` | `dataset` | mathematical | Timepoints-by-voxels orientation and voxel/time selections remain canonical. |
| `dataset_latent_archive_roundtrip` | `workflow_parity` | `dataset` + `archive` + `latent` | mathematical | Latent/archive-backed datasets decode selections with stable shape and metadata. |
| `bids_entities_manifest_query` | `workflow_parity` | `bids` | BIDS spec fixture | BIDS entities, URIs, and manifest queries round-trip real naming patterns. |
| `bids_fmriprep_confound_selection` | `workflow_parity` | `bids` | hand fixture / fmriprep TSV slice | Confound strategy picks expected motion/CompCor/FD columns and preserves missing-column diagnostics. |
| `bids_to_model_ministudy` | `cross_level_workflow` | `bids` + `dataset` + `model` | generated mini BIDS fixture | A small BIDS-like study can be described, selected, converted to a dataset, and modeled without hidden IO assumptions. |
| `image_jvm_nifti_tiny_roundtrip` | `numeric_canary` | `image/jvm` | synthetic NIfTI fixture | JVM image IO preserves affine, shape, and data ordering; excluded from JS. |
| `archive_jvm_hdf5_tiny_roundtrip` | `numeric_canary` | `archive/jvm` | synthetic archive fixture | JVM HDF5 store preserves manifests, payloads, and checksums. |

### Wave 4: Spatial, Atlas, Threshold, Motion, And MVPA

These broaden the catalog beyond the GLM path. They should still use the same
scenario manifest and receipt rules, even when the owner module is not `fit`.

| Scenario id | Tier | Owner | Reference | Workflow risk protected |
| --- | --- | --- | --- | --- |
| `image_affine_resample_identity` | `workflow_parity` | `image` | mathematical | Identity and affine resampling preserve voxel values and world/voxel coordinate semantics. |
| `image_masked_stat_map_roundtrip` | `numeric_canary` | `image` | mathematical | Masked vector/map conversions preserve spatial indices. |
| `spatial_affine_route_lowering` | `workflow_parity` | `spatial` + `image` | mathematical | Graph morphism paths lower to executable image morphisms without reversing source/target semantics. |
| `atlas_parcel_reduction_overlap` | `workflow_parity` | `atlas` | neuroatlas fixture | Parcel lookup, overlap, adjacency, and mean/sum reduction match known atlas fixtures. |
| `surface_parcel_vertex_reduction` | `workflow_parity` | `surface` + `atlas` | neuroatlas/neurosurf fixture | Surface labels reduce vertex fields with hemisphere and label metadata intact. |
| `threshold_fdr_bh_by_maps` | `numeric_canary` | `threshold` + `group` | R `p.adjust` fixture | BH/BY correction over maps matches reference q-values. |
| `threshold_cluster_maxT_toy_field` | `workflow_parity` | `threshold` | mathematical/toy permutation fixture | Cluster and maxT-style correction preserve mask geometry and family-wise thresholds. |
| `motion_fd_dvars_censor_hints` | `workflow_parity` | `motion` | volregger fixture | FD/DVARS and censor hints match known rigid-motion traces. |
| `motion_apply_identity_and_shift` | `numeric_canary` | `motion` + `image` | mathematical | Applying identity and simple rigid shifts over 4D runs preserves expected voxels and padding policy. |
| `mvpa_roi_classification_recovery` | `workflow_parity` | `mvpa` | rMVPA fixture / mathematical | ROI classifier recovers a known class signal with stable fold semantics. |
| `mvpa_searchlight_smoke` | `cross_level_workflow` | `mvpa-spatial` + `mvpa` | mathematical | Spatial feature-set adapters feed the same MVPA engine as ROI plans. |
| `mvpa_rdm_rsa_alignment` | `workflow_parity` | `mvpa` | rMVPA fixture | RDM and RSA scoring align by labels, not row position. |
| `latent_boldzip_encode_decode` | `numeric_canary` | `latent` | mathematical | Basis/loadings payloads reconstruct expected response blocks. |
| `latent_transport_selection` | `workflow_parity` | `latent` + `dataset` | mathematical | Latent selections decode the same response samples as explicit payloads. |

### Wave 5: Flagship Workflows

Flagship scenarios should come after the lower-level scenarios they compose.
They are release evidence, not early development scaffolding.

| Scenario id | Tier | Owner | Reference | Workflow risk protected |
| --- | --- | --- | --- | --- |
| `flagship_single_subject_public_glm` | `flagship_workflow` | `model` + `fit` | fmrimod/Nilearn fixture | A realistic single-subject dataset goes through model, fit, semantic contrast, diagnostics, and receipt. |
| `flagship_subjects_to_group` | `flagship_workflow` | `fit` + `group` | mathematical + fmrimod fixture | Multiple subjects with injected effects reach group inference with signal/null separation. |
| `flagship_bids_ministudy_to_group` | `flagship_workflow` | `bids` + `dataset` + `fit` + `group` | generated mini BIDS fixture | BIDS-style discovery, dataset construction, first-level fitting, and group inference compose end to end. |
| `flagship_spatial_group_threshold` | `flagship_workflow` | `group` + `image` + `threshold` | mathematical/toy field | Group statistic maps can be gathered into image space and thresholded with geometry intact. |
| `flagship_atlas_mvpa_report` | `flagship_workflow` | `atlas` + `mvpa-spatial` + `mvpa` | rMVPA/neuroatlas fixtures | Atlas-defined feature sets feed MVPA/RSA and produce inspectable parcel-level outputs. |

## Priority Scenario Backlog

### 1. Public F-Contrast With Confounds And Drift

Source inspiration:
`/Users/bbuchsbaum/code/pycode/fmrimod/benchmarks/parity/tier_a_f_confound_drift/public_workflow.py`

ScalaFIM target:
`modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/PublicFContrastScenarioSuite.scala`

Workflow:

1. Build a deterministic event table with conditions `A` and `B`.
2. Build deterministic nuisance regressors similar to motion/confound columns.
3. Build an in-memory `FmriDataset`.
4. Build a `FitPlan` through `FmriModelBuilder`, using the public model seam.
5. Fit through `FitPlanExecutor`.
6. Evaluate an `A_minus_B` t contrast and a task omnibus F contrast.
7. Compare named outputs: design matrix, beta/effect, t statistic, F statistic,
   residual degrees of freedom, and rank.

Reference in the first cut should be a mathematical oracle computed from the
same realized design using the existing `linalg`/OLS primitives. The second cut
can replace or supplement the expected arrays with a generated fmrimod/Nilearn
Scala fixture.

Acceptance:

```sh
sbt fitJVM/test
sbt fitJS/test
```

The scenario must fail if a contrast silently binds to the wrong column after a
design-column order change.

### 2. Group One-Sample T From Per-Subject Effects

Source inspiration:
`/Users/bbuchsbaum/code/pycode/fmrimod/benchmarks/parity/tier_a_group_level_t/workflow.py`

ScalaFIM target:
`modules/group/shared/src/test/scala/scalafim/fmri/group/scenarios/GroupOneSampleScenarioSuite.scala`

Workflow:

1. Generate deterministic subject-by-voxel effects with a known mean shift.
2. Construct `GroupData.single`, `GroupDesign.intercept`, and `GroupModel`.
3. Fit with `GroupEngine.fit`.
4. Compare effect mean, standard error, t statistic, residual df, and p-value
   to a mathematical one-sample t oracle.

Acceptance:

```sh
sbt groupJVM/test
sbt groupJS/test
```

This becomes the bridge target for later first-level-to-group scenarios.

### 3. First-Level Contrast Into Group Workflow

ScalaFIM target:
`modules/group/shared/src/test/scala/scalafim/fmri/group/scenarios/FirstLevelToGroupScenarioSuite.scala`

Workflow:

1. Simulate several subjects with known condition effects.
2. Fit each subject with the public `model -> fit -> contrast` path.
3. Convert `TContrastResult` values with the existing first-level bridge.
4. Run group intercept-only OLS.
5. Assert that signal samples recover the known between-subject mean and null
   samples remain near zero.

This is the first `cross_level_workflow` tier. It should wait until scenario 1
and scenario 2 are stable, because it composes both.

### 4. Censored Multi-Run Or Row-Subset OLS

Source inspiration:
`fmrimod/benchmarks/parity/tier_a_censored_concat/workflow.py`

ScalaFIM target:
`fit` after the censoring API is explicit.

If ScalaFIM does not yet have a first-class censor mask in `FmriDataset` or
`FitPlan`, start with an honest row-subset OLS canary and record the public API
gap. Do not pretend row deletion is a full public censoring workflow until the
dataset/model seam carries censor intent.

### 5. Mixed-TR Multi-Run

Source inspiration:
`fmrimod/benchmarks/parity/tier_a_mixed_tr_multirun/workflow.py`

ScalaFIM target:
`design` and `fit`, because mixed TR primarily tests sampling-frame and design
realization before it tests fitting.

This should be a second-wave scenario. It is valuable, but it should not block
the simpler public F-contrast proof.

### 6. AR(1) Prewhitening With Declared Divergence

Source inspiration:
`fmrimod/benchmarks/parity/tier_a_ar1_prewhitening/workflow.py`

ScalaFIM target:
`ar` plus `fit`, after AR integration in the fit path is stable.

This is an `algorithm_divergence` scenario. It should use relaxed correlation
or MAE gates and an explicit caveat explaining the AR estimation policy.

## Implementation Phases

### Phase 0: Plan And Boundaries

Status: this document.

Acceptance:

- README links to this plan.
- The plan names initial scenario homes, tiers, and commands.
- No new runtime dependency is introduced.

### Phase 1: Minimal Module-Local Harnesses And Two Mathematical Scenarios

Deliverables:

- `ScenarioHarness.scala` in `fit` test scope with array comparison helpers.
- A small `GroupScenarioHarness.scala` or inline helper under `group` test
  scope. Keep the duplication tiny rather than adding a testkit module too
  early.
- `PublicFContrastScenarioSuite.scala` under `fit`.
- `GroupOneSampleScenarioSuite.scala` under `group`.

Constraints:

- Helpers must compile on JVM and Scala.js.
- No filesystem reads.
- No Python/R invocation.
- Scenario values may be hard-coded or generated deterministically in Scala.

Acceptance:

```sh
sbt fitJVM/test
sbt fitJS/test
sbt groupJVM/test
sbt groupJS/test
```

### Phase 2: fmrimod/Nilearn Fixture Exporter

Deliverables:

- `tools/scenarios/export_fmrimod_fit_scenarios.py`
- Generated Scala fixture object for the public F-contrast scenario.
- Generator instructions in the fixture header.

The exporter should run the fmrimod workflow or import its `load_inputs()` and
reference pipeline, then emit a compact Scala object:

```scala
object FmrimodFitScenarioFixtures:
  val publicFContrastDesign: MatrixFixture = ...
  val publicFContrastExpected: OutputFixture = ...
```

Acceptance:

```sh
python tools/scenarios/export_fmrimod_fit_scenarios.py --check
sbt fitJVM/test
sbt fitJS/test
```

The `--check` mode must fail if regenerated fixture content differs from the
checked-in file.

### Phase 3: Manifest And Receipts

Deliverables:

- `docs/scenarios/manifest.json`
- `docs/scenarios/fit_public_f_confound_drift.json`
- `docs/scenarios/group_one_sample_t.json`
- A JVM-only receipt renderer or script that writes JSON/Markdown from scenario
  results.

Receipts are audit artifacts. They should include status, tolerances, named
deltas, caveats, source references, and the exact command used to regenerate
them. They should not be required to run the cross-platform MUnit suite.

Acceptance:

```sh
sbt fitJVM/test
sbt groupJVM/test
```

and a documented receipt regeneration command.

### Phase 4: Wave 2 Stress Scenarios

Deliverables:

- First-level-to-group workflow.
- Mixed-TR design/fitting scenario.
- Censoring scenario once the public API supports it.
- AR(1) divergence scenario with caveat.
- Realistic-confounds motion omnibus scenario.
- FIR, block-duration, and factorial/parametric stress scenarios as generated
  fmrimod/Nilearn fixtures.

Acceptance:

```sh
sbt fitJVM/test
sbt fitJS/test
sbt designJVM/test
sbt designJS/test
sbt arJVM/test
sbt arJS/test
sbt groupJVM/test
sbt groupJS/test
```

with any slow or optional receipt generation kept outside the default test
command.

### Phase 5: Dataset, BIDS, Spatial, And Analysis Catalog Expansion

Deliverables:

- Dataset/BIDS/archive scenarios from Wave 3.
- Image/spatial/atlas/threshold/motion/MVPA/latent scenarios from Wave 4.
- Manifest rows for every active scenario, including JVM-only platform tags
  where appropriate.

Constraints:

- Shared scenarios still run on both JVM and Scala.js.
- JVM-only scenarios must be explicit in the manifest and excluded from JS
  acceptance commands.
- No scenario should force a higher module dependency into a lower module just
  to share fixture helpers.

Acceptance:

```sh
sbt testAll
sbt examplesTest
```

plus focused JVM-only checks for IO scenarios as they are added.

### Phase 6: Flagship Receipts

Deliverables:

- Flagship workflows from Wave 5.
- JSON/Markdown receipts under `docs/scenarios/`.
- A manifest gate that fails when an active scenario has no matching test suite
  or stale receipt metadata.

Acceptance:

```sh
sbt testAll
```

and a documented command to regenerate all receipts.

## Phase 1 Active Cases

Phase 1 keeps the harness test-scoped and proves the shape on shared JVM/Scala.js
tests before introducing any common production testkit.

| Scenario id | Suite | Reference | Terminal truth |
| --- | --- | --- | --- |
| `fit.public-f-contrast.v1` | `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/PublicFContrastScenarioSuite.scala` | mathematical/direct OLS oracle over the intended design matrix | `ScenarioResult.status` and `ScenarioResult.ciPass` |
| `fit.semantic-contrast-reordered-columns.v1` | `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/SemanticContrastReorderedColumnsScenarioSuite.scala` | paired public fits with reversed design-column order plus direct OLS oracle | `ScenarioResult.status` and `ScenarioResult.ciPass` |
| `fit.lss-trialwise-recovery.v1` | `modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/LssTrialwiseRecoveryScenarioSuite.scala` | public builder/executor result against direct metadata-selected LSS oracle | `ScenarioResult.status` and `ScenarioResult.ciPass` |
| `group.one-sample-analytic.v1` | `modules/group/shared/src/test/scala/scalafim/fmri/group/scenarios/GroupOneSampleScenarioSuite.scala` | analytic one-sample t oracle per sample | `ScenarioResult.status` and `ScenarioResult.ciPass` |
| `group.two-sample-analytic.v1` | `modules/group/shared/src/test/scala/scalafim/fmri/group/scenarios/GroupTwoSampleScenarioSuite.scala` | analytic pooled two-sample t oracle per sample plus named group contrast check | `ScenarioResult.status` and `ScenarioResult.ciPass` |
| `group.first-level-bridge.v1` | `modules/group/shared/src/test/scala/scalafim/fmri/group/scenarios/GroupFirstLevelBridgeScenarioSuite.scala` | first-level `TContrastResult` bridge into fixed-effects group inference against analytic inverse-variance oracle | `ScenarioResult.status` and `ScenarioResult.ciPass` |
| `group.first-level-to-group-known-effect.v1` | `modules/group/shared/src/test/scala/scalafim/fmri/group/scenarios/FirstLevelToGroupKnownEffectScenarioSuite.scala` | public first-level fits recover injected task effects, bridge to group data, and fixed-effects inference matches analytic inverse-variance oracle | `ScenarioResult.status` and `ScenarioResult.ciPass` |

The fit harness currently lives in:

```text
modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/ScenarioHarness.scala
```

The group harness currently lives in:

```text
modules/group/shared/src/test/scala/scalafim/fmri/group/scenarios/ScenarioHarness.scala
```

Keep the fit and group test harnesses separate until a third module needs the
same algebra. Promote only after duplication proves the common shape is worth a
shared testkit.

The active scenario registry lives in:

```text
docs/scenarios/manifest.json
```

The first external reference fixture is generated from the fmrimod/Nilearn
parity path:

```text
tools/scenarios/export_fit_public_f_contrast_fixture.py
docs/scenarios/fixtures/fit.public-f-contrast.v1.nilearn.json
modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/PublicFContrastNilearnFixture.scala
```

Phase 1 acceptance:

```sh
python tools/scenarios/export_fit_public_f_contrast_fixture.py --check
sbt fitJVM/test
sbt fitJS/test
sbt groupJVM/test
sbt groupJS/test
```

The next slice should start the Wave 2 stress cases with
`fit_censored_multirun_concat`.

## Definition Of Done For A Scenario

A scenario is complete only when all of the following hold:

- It runs on both JVM and Scala.js unless it intentionally tests platform IO.
- It exercises the highest public seam that currently exists.
- It returns a `ScenarioResult` with one terminal `ScenarioStatus`.
- CI or MUnit consumes exactly one truth value, `result.ciPass`.
- Every compared quantity has a stable name and documented tolerance.
- Any divergence is represented as a caveat value and linked to an owner or
  follow-up.
- `PassWithCaveats` is allowed only when every caveat is declared in the
  scenario metadata and permitted by the manifest/receipt policy.
- The scenario has a concise comment explaining what workflow risk it protects.
- The README, manifest, or plan points to the scenario if it is part of release
  evidence.
- The relevant command has passed on both platforms.

## Non-Goals

- Running Nilearn, Python, R, or rpy2 from sbt.
- A new public `scalafim-scenario` runtime module before the test surface proves
  it needs one.
- Large fixture files that make Scala.js tests depend on filesystem behavior.
- Marketing-style "golden path" documentation before the executable scenarios
  exist.
- Treating private OLS matrix parity as evidence that the public modeling API is
  coherent.

## Near-Term Execution Order

1. Implement the fit-scope harness and public F-contrast mathematical scenario.
2. Implement the group one-sample t scenario.
3. Add a fmrimod/Nilearn exporter for `fit_public_f_contrast`.
4. Build `first_level_to_group_known_effect` from the green fit and group
   pieces.
5. Expand into Wave 2 stress scenarios, starting with
   `fit_censored_multirun_concat`: mixed TR, censoring, realistic
   confounds, FIR/block durations, factorial/parametric designs, and AR
   divergence.
6. Add Wave 3 and Wave 4 module-family scenarios as their public seams become
   stable.
7. Promote only composed, receipt-backed workflows into Wave 5 flagship rows.
