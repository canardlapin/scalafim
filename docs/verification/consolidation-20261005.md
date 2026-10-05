# ScalaFIM consolidation, 2026-10-05

Mote: `bd-01M46MVJCFV7JXK7JW0DT44E1B`. Initial main `92033261fd82eb003b556c2b4a7af2c87551996d`; fetched origin/main `568ce1082b7f3878e0c6ff4607124aac71b9b63b`. Final code candidate `e40e36157dd308617a029f5247c2251d37f1b0ad`. Gate results and landing status are completed below after terminal verification.

## Scope and preservation

Prepare canonical main from work with current landing qualification. Retain historical, experimental, rejected and dependency-blocked work with explicit dispositions. This does not finish every retained research project or qualify every alternate branch. No original branch or source directory was deleted. Remote publication is outside this local consolidation.

Initial inventory: 146 branches, 44 worktree registrations. Twelve valid worktrees retained dirty source. Four stale registrations had missing Gitfiles and mostly absent base files, but 61 residual source files remained. Those files were verified and preserved before metadata-only pruning; their source directories remain intact.

All 1,036 dirty source paths across the twelve valid worktrees were archived, SHA-256 checked, then reproduced exactly in `refs/preserved/consolidation-20261005/*`. Original indexes and status remained unchanged. The four residual overlays are in `refs/preserved/orphan-worktrees-20261005/*`; their base commits and original index patches remain retained. These refs are local preservation evidence, not landing candidates. Manifests/index patches also live under `.git/preserved/consolidation-20261005/`.

## Integrated work

| Work | Exact source / review boundary | Consolidation |
|---|---|---|
| Endpoint-anchored B-splines | Completed issue `bd-01M43X88PVTG0A28RYC3X1TEJS`; ten-path qualified source manifest matched byte-for-byte | Commit `a25e19b3`; generator/reference trailing whitespace normalized by `ffafdca7`; pinned R oracle regenerated identical source and fixtures |
| Portable kernel/profile provenance v2 | Reviewed `c114cc3b`, including round-3 structural encoder/field guards; existing qualifications are author-run and independently inspected | Conflict-free merge `4a959dd3`; one required law-test wrapping correction `e40e3615` |
| Bounded robust and learned reduced-rank preparation | Independently reviewed code `752c323e`, qualified full fit JVM/JS and compile gates, receipt `36151a67` | Conflict-free queue merge `240373e0` |
| Bounded JavaFX affine recovery | Independently reviewed code `3bb30729`, source-bound native diagnostic receipts; production returns `SamplerUnqualified` | Same queue merge; current unit/compile checks listed below; native diagnostic executions were not repeated |
| Exact seven-state Cascade34 realization | Code, suite, 80-digit dense-exponential fixtures and generator exactly match `8b274c04` source manifest; current family blob is identical | Bounded source transplant `175db880`; no overlap consumer or provider-pin experiment included |

Existing provenance receipts describe v1/v2 source compatibility and remaining custom-family/trial-axis identity limits. This landing preserves those limits. Cascade34 remains an impulse/point-observation operator, with no likelihood backend, box forcing, observation averaging, inference or performance qualification. Its original archived receipt also describes an older overlap-provider experiment; that experiment is retained on its original branch and is not a claim about this consolidation.

## Retained work

Every initial branch has an exact SHA and disposition in [branches.json](consolidation-20261005/branches.json). Detailed merge-base/path/blob and patch-equivalence comparisons are retained in the compressed branch analysis. Ancestry, patch equivalence and historical source presence have distinct meanings; none substitutes for current scientific or integration acceptance.

| Disposition | Initial branches |
|---|---|
| contained-by-retained | 16 |
| dependency-blocked | 1 |
| evidence-retained | 3 |
| experiment-retained | 3 |
| integrated-ancestor | 84 |
| integration-deferred | 4 |
| merge-only-history | 1 |
| patch-equivalent | 4 |
| research-retained | 2 |
| selectively-integrated | 3 |
| snapshot-retained | 3 |
| source-represented | 3 |
| superseded | 3 |
| superseded-snapshot | 1 |
| unqualified-for-current-main | 15 |

Named deferred boundaries: typed-atlas snapshot C1 needs current workflow/spatial/examples integration; GIFTI C6 must reconcile current declaration/placement contracts; voxelwise RRG C3 is a mixed-era partial port and must be re-ported from the qualified calibration branch with legacy result-export policy adjudicated. Numerical-ownership profile changes require an unpublished Gale override; the S0 benchmark does not supply S4/S5 admission. The bootstrap research branch reports all three candidates Decline. Historical native/Metal/raster experiments remain separate product-admission work.

The following distinct lines remain unqualified for a blanket merge into current main. They are retained and explicitly accounted for; no new completion claim is made:

- `codex/atlas-domain-realizations` (`143e6ae7f038`)
- `codex/neuropublish-clear-compat-20260824` (`d89076763f23`)
- `codex/selected-evidence-20260921` (`f7b3a5ee0842`)
- `dataset/exact-companions-20260924` (`5d95334d2957`)
- `estimates/response-action-evidence-v1` (`aef31a29c238`)
- `fit/estimate-description-policy-20260914` (`51035bd324d4`)
- `integration/main-catchup-20260923` (`206243b37adf`)
- `integration/mvpa-on-exact-spatial` (`81a0408d91f3`)
- `linked-roots/scalafim-20260921` (`38cf957df56f`)
- `neuropublish/lifecycle-dispose` (`ac545e651f5c`)
- `surface/group-fslr-qualification-20260923` (`d741dff3e508`)
- `surface/strip-raster-qualified-20260912` (`452acac8e0e8`)
- `worktree-agent-a3ebf3564cf9328f3` (`0282f0aa3d1b`)
- `worktree-agent-a6daa1f0970060d9c` (`8be020efd3d4`)
- `worktree-agent-af1cb90d240e6706e` (`e1d8f3ce88f3`)

## Verification

Core gate `ffafdca7`: Java 17.0.20.1, sbt 1.11.7, Scala 3.7.4, both platforms. All twelve commands exited zero; no warnings/errors in the complete raw log. HRF 292/platform, HRF laws 82/platform, design 447 JVM/446 JS, model 53/platform, fit 649 JVM/594 JS, `scalafimCompileAll`, `examplesCompile`.

Candidate `175db880`: HRF 301/platform and HRF laws 82/platform passed. The next required formatter gate failed on one C2 test file. `e40e3615` changes only wrapping of two test expressions; numerical, semantic, timeout and admission assertions are unchanged. Failed receipts are retained. Previously passed HRF/core checks remain source-bound; both final law suites pass 84/platform. The subsequent PHRF JVM run had 470 passes, three missing-GLMsingle-package failures and one declared opt-in bias skip. The failures were traced to the incomplete retained interpreter, which was replaced for this task by a hash-lock installation and pinned GLMsingle source `1ab54a65`, selected explicitly on a fresh task-owned server. No foreign environment or source was changed. The final consumer/build rerun follows below.

| Final target | JVM passed | JS passed |
|---|---:|---:|
| hrf (with Cascade34) | 301 | 301 |
| hrfLaws | 82 | 82 |
| firstLevelLaws | 84 | 84 |
| phrfComparison | 473 | 242 |
| fitEstimates | 16 | 12 |
| group | 75 | 74 |
| fmriWorkflow | 23 | 19 |
| mvpaFit | 45 | 45 |
| mvpaDataset | 108 | 108 |
| mvpaSpatial | 20 | 20 |
| datasetZarr | 15 | 7 |
| surfaceViewJavafx | 35 | JVM-only |

Final `e40e3615` rerun: all 19 commands exited zero in 842.5 s. The complete log has no compiler warnings/errors. `scalafimCompileAll` and `examplesCompile` passed. PHRF JVM has exactly one declared opt-in `RhoBiasHeavySuite` skip; custody interop was mandatory (`PHRF_REQUIRE_INTEROP=1`), and all real GLMsingle checks passed. No numerical/semantic assertion, tolerance, decode budget, admission criterion or scientific limit was weakened.

The runtime reconstruction installed the declared runtime dependencies with the checked-in SHA-256 lock and installed GLMsingle from exact upstream `1ab54a65`. The environment freeze and lock/source hashes are retained. The test-only local discovery symlink was removed after execution. Original foreign environments remain unchanged.

Full logs, terminal metadata, source hashes and preserved-state summaries accompany this receipt. Previous all-module reconciliation evidence is [reconcile-origin-main-20261004.md](reconcile-origin-main-20261004.md); unchanged unrelated modules were not all rerun here. No hosted Linux/CI, native runtime repetition, optional bias/timing, scientific epic completion, release or performance qualification follows from these local gates.

## Landing

Canonical main is advanced locally by a guarded `--ff-only --no-autostash` landing from clean `a25e19b3`. The receipt commit changes documentation/evidence only above tested code `e40e36157dd308617a029f5247c2251d37f1b0ad`. Final local main contains current origin/main `568ce1082b7f3878e0c6ff4607124aac71b9b63b` and is 112 commits ahead, zero behind after adding this receipt. No remote push was performed. Only this task's two temporary worktrees/branches and sbt servers are closed; all 146 original branch names and remaining original worktrees are retained. Four stale registrations were pruned after source preservation; original source directories remain intact. See `gates.json`, `tested-source.json` and `preservation.json` for exact code/log/source custody hashes.
