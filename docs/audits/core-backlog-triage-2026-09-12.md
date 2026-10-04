# ScalaFIM core backlog triage — 2026-09-12

Reviewed all 107 non-closed issues in the initial inventory. Closed or retired 32; 75 remain: P1: 27, P2: 11, P3: 37. All resulting titles, statuses, priorities and triage tags were checked against the live tracker. The administrative triage issue is excluded from these counts.

## Priority policy

- P1: core scientific correctness, high-impact allocation and assurance, plus the active PHRF release chain and its required upstream work.
- P2: bounded core maintenance, correctness and performance follow-ups.
- P3: lower-priority core work, optional extensions and deferred development-package integrations. This Mote version supports only P0–P3; deferred tags distinguish parked work from ordinary P3 work.
- P0: reserved for an evidenced emergency; no reviewed issue requires P0.

Every reviewed issue has `triaged-core-2026-09-12`, a disposition category and a decision note. Original bodies, historical evidence and other tags remain. `needs-revalidation` means the branch, source or measurements must be refreshed before treating the reported condition as present. Retirement is not a claim that an upstream capability was implemented or published.

## Active PHRF

The following remain P1. PHRF-06 remains doing; its claim and reservations were not changed. Optional finite-state, experimental and follow-on PHRF work remains separate from the required release chain.

- `bd-01M24MQABKEBQXW89VZ8XWBB8H` — EPIC: Unified ProfileHrf condition and trial fitting with bounded compiled backends
- `bd-01M24MRY1DA0KPP37J7H8G619D` — PHRF-06: Implement banded SPD factors, solves and logdet upstream in Gale and pin
- `bd-01M24MS4WEQN14GH3P56K1257J` — PHRF-07: Implement the TrialBanded backend on shared Gram blocks
- `bd-01M24MSJQZH70BTAFYTXD3MPZZ` — PHRF-11: Add trial amplitudes, ML determinant and queries to the readout contract
- `bd-01M24MSZ4Z6BFWRV5R4S67VDN0` — PHRF-14: Qualify numerical approximation and scientific calibration
- `bd-01M24MT58X8W2Z0KJ25JJG2SGP` — PHRF-15: Measure throughput, memory and backend crossovers
- `bd-01M24MTB363KSHN1KDRSAM2KNH` — PHRF-16: Release unified ProfileHrf
- `bd-01M26A285YAQ3H8TGS26BK9T7J` — PHRF-29: Add `ProfileHrfPlan` and attach the trial backend to the executor

## Core work to pull next

Keep scientific and numeric defects ahead of broad migrations: truthful MVPA run grouping (reproduce on current APIs), persisted estimate/covariance integrity, GLS and uncertainty calibration, GIFTI transform selection, B-spline intercept policy, and fitting/allocation hot paths. PHRF scientific qualification and performance measurement remain part of its active delivery scope.

## Evidence and limits

Source and history checks included the current build pins and module ownership, removal of embedded linalg/BIDS/multivar/frame modules, Canvas/Three lifecycle fixes, provider-backed persisted estimate readback, current GIFTI transform selection, fitting fallback-status materialization and B-spline basis construction. Commit ancestry was checked for `8ea489e`, `76e4b77`, `8b5878d`, `5495ea3`, `316a157`, `8d21918` and `b6a3af2`. The DCT candidate `81f6c16` is not an ancestor of current HEAD and was not treated as landed.

Current main has no mesh4s dependency; its topology benchmark is deferred integration evidence. Several MVPA issues name identified-evidence APIs absent from current main; retain their scientific/performance intent with explicit revalidation. Do not import historical allocation numbers as measurements of current code.

No tests were newly run for this metadata-only triage. Recent SPMG work in this same checkout had passed both-platform focused tests and the full compile gate; that evidence does not certify unrelated old candidate branches, native rendering, or provider publication. Existing code edits were preserved.

Removed one obsolete blocking edge from persisted estimate work to the broad Eidolon/resource-descriptor migration: the issue explicitly states unrelated global scheduler/resource redesign is not a prerequisite. The bounded-preparation dependency remains. Other dependency edges, issue bodies and active PHRF ownership were preserved. Stale doing statuses without live ownership were returned to open, not declared complete.

## Dispositions

| Category | Count |
|---|---:|
| active-phrf | 8 |
| completed-verified | 10 |
| core-assurance | 5 |
| core-correctness | 17 |
| core-maintenance | 1 |
| core-performance | 7 |
| deferred-feature | 16 |
| deferred-integration | 16 |
| deferred-phrf | 1 |
| optional-phrf | 4 |
| retired-obsolete | 7 |
| retired-upstream | 15 |

## Issue-by-issue record

The adjacent JSON contains machine-readable before/after titles, priorities, statuses, tags and rationale.

### bd-01KXYHVBTCJ05W4EX3GY80DS1X — [DEFERRED] RFE: align scalafim workflow scope with eidolon ownership

P0 → P3; open → open; `deferred-integration`.

Eidolon boundary/migration proposal; retain the recorded ownership decision, but defer broad module retirement and orchestration integration until a concrete core requirement needs it.

### bd-01KXYJBP89WFPNKAZ03MQQMY5M — [DEFERRED] RFE: freeze and retire scalafim-pipeline workflow authority

P0 → P3; open → open; `deferred-integration`.

Eidolon boundary/migration proposal; retain the recorded ownership decision, but defer broad module retirement and orchestration integration until a concrete core requirement needs it.

### bd-01KXYJC5CK664MZZ9AH8PC0M3M — [DEFERRED] RFE: split fmri-workflow scientific specifications from app orchestration

P0 → P3; open → open; `deferred-integration`.

Eidolon boundary/migration proposal; retain the recorded ownership decision, but defer broad module retirement and orchestration integration until a concrete core requirement needs it.

### bd-01KZ3ZCDD2RMYQ3WVY606FNKY1 — EPIC: preserve first-level scientific identity from design compilation through inference

P0 → P1; open → open; `core-assurance`.

Retain first-level scientific identity, independent numerical evidence, executable examples, and measured allocation/performance gates as core priorities. Historical branch/CI claims need refresh; do not mistake current compilation for full release certification.

### bd-01KZ6BA6BKHKFY32E0A6XMYQQD — [DEFERRED] EPIC: Remove scalafim-locus-data and adopt locus4s directly

P0 → P3; open → open; `deferred-integration`.

locus-data still exists in build.sbt. This is an unfinished ownership/deletion migration, not an established defect in current computations. Keep the chain intact and defer; do not claim removal completed.

### bd-01KZ6BCTQG47202PW9EM5886GT — [DEFERRED] Rewire consumers and delete the scalafim-locus-data artifact

P0 → P3; open → open; `deferred-integration`.

locus-data still exists in build.sbt. This is an unfinished ownership/deletion migration, not an established defect in current computations. Keep the chain intact and defer; do not claim removal completed.

### bd-01KZ6BCXA6JB4Z7G8AMST1KKZ5 — [DEFERRED] Run the complete ScalaFIM locus-data removal migration court

P0 → P3; open → open; `deferred-integration`.

locus-data still exists in build.sbt. This is an unfinished ownership/deletion migration, not an established defect in current computations. Keep the chain intact and defer; do not claim removal completed.

### bd-01KZ9E1S24ZJJFS3HQEJ5G29F3 — Validate ScalaFIM against Gale 0.1 M1 candidate

P0 → P3; doing → closed; `retired-obsolete`.

Retire the July Gale 0.1 M1 exact-candidate exercise: current build pins Gale 83cac90a, and both-platform compile gates passed. This does not certify the old candidate. Current required banded-SPD adoption is tracked separately by active PHRF-06.

### bd-01M0Z5MJ46C70JQ9NNN4GPMJQQ — Predictive MVPA cannot bind a truthful run generalization axis

P0 → P1; open → open; `core-correctness`.

Truthful run grouping and validation-axis identity is core MVPA correctness. Keep high priority; reported behavior is a typed rejection, so P1 rather than an emergency P0. Verify against current identified-evidence implementation before choosing a repair. Current-main check: the identified-evidence APIs named by this issue are absent from modules; first reproduce the concern against current MVPA APIs or identify the candidate branch. Historical evidence is not a current-main reproduction.

### bd-01KX2CPJYP0DP751T841N00V6E — Epic: Oderskyan fmrireg parity architecture

P1 → P3; open → open; `deferred-feature`.

Broad historical parity umbrella includes optional plugin and estimator expansion. Keep existing children and completed history; prioritize concrete correctness/performance children independently rather than treating the entire expansion as urgent.

### bd-01KX6G842E07NN3XZSQF2Z5HH3 — [DEFERRED] Epic: out-of-core BIDS-to-group fMRI workflow

P1 → P3; open → open; `deferred-integration`.

Broad resumable BIDS-to-group orchestration/acceptance predates the newer ownership split. Defer application execution/resume integration; reusable bounded fit, persisted estimates, and group computation remain separately tracked.

### bd-01KX6G9B1ZT10A3214DPA096RZ — Workflow first level: make fit preparation serializable and out-of-core

P1 → P2; open → open; `core-performance`.

Bounded first-level/group block execution is relevant core scalability. Retain; distinguish existing selected-block execution from still-unfinished durable serialization or manifest-wide execution.

### bd-01KX6G9B8R86MRBZ9S8K8F5G7V — Workflow output: add transactional result sinks and a durable first-level catalog

P1 → P1; open → open; `core-correctness`.

Keep estimate persistence/readback integrity high priority. 8d21918 landed a qualified checkpoint, but its own completion note explicitly leaves stable schema/golden bundles, covariance, durability, and broader integration open. Do not close the broad item.

### bd-01KX6G9BFJRERNFRWR17WT0YGN — Workflow group level: execute manifest-backed spatial blocks

P1 → P2; open → open; `core-performance`.

Bounded first-level/group block execution is relevant core scalability. Retain; distinguish existing selected-block execution from still-unfinished durable serialization or manifest-wide execution.

### bd-01KX6G9BP68XVCN41XZTG73GVC — [DEFERRED] Workflow orchestration: lower study plans to resumable execution policies

P1 → P3; open → open; `deferred-integration`.

Broad resumable BIDS-to-group orchestration/acceptance predates the newer ownership split. Defer application execution/resume integration; reusable bounded fit, persisted estimates, and group computation remain separately tracked.

### bd-01KX6G9BWW51R09NEBJNBMWRMS — [DEFERRED] Workflow acceptance: prove fMRIPrep-to-group parity, bounded memory, and resume

P1 → P3; open → open; `deferred-integration`.

Broad resumable BIDS-to-group orchestration/acceptance predates the newer ownership split. Defer application execution/resume integration; reusable bounded fit, persisted estimates, and group computation remain separately tracked.

### bd-01KXSQ2Z32R1066VVQK4ZAR9C3 — Frame adapter: Parquet source, sink, and truthful pushdown

P1 → P3; open → closed; `retired-upstream`.

Retire from ScalaFIM scheduling: generic Frame/Parquet/DuckDB/Gale/spill implementation belongs to extracted frame4s (b6a3af2 is an ancestor; embedded Frame module is absent). This is scope retirement, not a claim upstream work is implemented. Reopen only a narrow ScalaFIM consumer defect with concrete evidence.

### bd-01KXSQ62V1G2XSSFT4DEX1HX7V — [DEFERRED] ntab 0: rebase the neuroimaging table plan onto independent Frame

P1 → P3; open → open; `deferred-feature`.

ntab/group-ntab is a proposed new feature layer with stale incubated-Frame references. Keep its scientific intent, but defer implementation and rebase onto standalone frame4s only when a current use case justifies it.

### bd-01KXSQ6M2NRFKS762S5A3FRM8N — [DEFERRED] ntab 1: feature references, exact support, and batched resolution over Frame rows

P1 → P3; open → open; `deferred-feature`.

ntab/group-ntab is a proposed new feature layer with stale incubated-Frame references. Keep its scientific intent, but defer implementation and rebase onto standalone frame4s only when a current use case justifies it.

### bd-01KXSQ75FHWMGQG0B78R93HQ3W — [DEFERRED] ntab 2: exact-support grouped reductions, contributor lineage, and drill-down

P1 → P3; open → open; `deferred-feature`.

ntab/group-ntab is a proposed new feature layer with stale incubated-Frame references. Keep its scientific intent, but defer implementation and rebase onto standalone frame4s only when a current use case justifies it.

### bd-01KXSQ84B3KAQXRR8XN0YDDG4C — [DEFERRED] ntab 3: cross-platform Frame integration, workflow evidence, docs, and dependency audit

P1 → P3; open → open; `deferred-feature`.

ntab/group-ntab is a proposed new feature layer with stale incubated-Frame references. Keep its scientific intent, but defer implementation and rebase onto standalone frame4s only when a current use case justifies it.

### bd-01KXSQ8Q65XK2THWC1MY74Y4NX — [DEFERRED] Epic: ntab — exact-support neuroimaging feature relations built on Frame

P1 → P3; open → open; `deferred-feature`.

ntab/group-ntab is a proposed new feature layer with stale incubated-Frame references. Keep its scientific intent, but defer implementation and rebase onto standalone frame4s only when a current use case justifies it.

### bd-01KXYJCH0R2W45SAPT38PCHFJJ — [DEFERRED] RFE: replace workflow artifact locations with reusable resource descriptors

P1 → P3; open → open; `deferred-integration`.

Eidolon boundary/migration proposal; retain the recorded ownership decision, but defer broad module retirement and orchestration integration until a concrete core requirement needs it.

### bd-01KXYJCTH1E42ET2N0NQ574FSW — [DEFERRED] Docs: record scalafim-eidolon module boundary and migration map

P1 → P3; open → open; `deferred-integration`.

Eidolon boundary/migration proposal; retain the recorded ownership decision, but defer broad module retirement and orchestration integration until a concrete core requirement needs it.

### bd-01KXYJK0XSA3RNH9ANMVC96EPE — Epic: migrate Scalafim linear algebra to Gale and retire internal linalg

P1 → P3; open → closed; `retired-obsolete`.

Retire the old monolithic Gale migration/purge gate: 8ea489e is on HEAD, modules/linalg and linalg-breeze are absent, and production/build scans find no scalafim.linalg or Breeze references. Remaining specialized HRF/motion duplicate review stays open in migration-5; this retirement does not claim every historical global gate was rerun.

### bd-01KXYJNQ1TKES0D4GPVP42F8HW — Scalafim Gale migration 3: fit and factorization consumers

P1 → P1; open → closed; `completed-verified`.

Core factorization/spectral consumer migration landed (8ea489e ancestor). Removed generic modules/imports are absent and current default-pin both-platform compile is warning-clean. Domain adapters and their scientific policies remain; residual duplicate audit stays separate.

### bd-01KXYJPAXGS2C588VJSXPSJ2J0 — Scalafim Gale migration 4: spectral and multivariate solvers

P1 → P1; open → closed; `completed-verified`.

Core factorization/spectral consumer migration landed (8ea489e ancestor). Removed generic modules/imports are absent and current default-pin both-platform compile is warning-clean. Domain adapters and their scientific policies remain; residual duplicate audit stays separate.

### bd-01KXYJQYW4V7WV9FC1Q0JTNVB8 — Scalafim Gale migration 6: purge internal linalg and close global gates

P1 → P3; open → closed; `retired-obsolete`.

Retire the old monolithic Gale migration/purge gate: 8ea489e is on HEAD, modules/linalg and linalg-breeze are absent, and production/build scans find no scalafim.linalg or Breeze references. Remaining specialized HRF/motion duplicate review stays open in migration-5; this retirement does not claim every historical global gate was rerun.

### bd-01KY4TXJQZ72S37EXCWTEFT5S4 — Make aligned structured multiblock a real joint fit

P1 → P3; open → closed; `retired-upstream`.

Retire from ScalaFIM scheduling: these tasks implement standalone multivar model lifecycle, estimators, IR, or documentation. modules/multivar is absent and build.sbt consumes the independent multivar project; docs/module-relations.md assigns it this authority. No upstream completion or ticket transfer is claimed. A concrete ScalaFIM consumer failure warrants its own bounded issue.

### bd-01KY4TXM5VB7R9FAQFDE7T6QN0 — Unify fitted-model lifecycle and runtime evidence emission

P1 → P3; open → closed; `retired-upstream`.

Retire from ScalaFIM scheduling: these tasks implement standalone multivar model lifecycle, estimators, IR, or documentation. modules/multivar is absent and build.sbt consumes the independent multivar project; docs/module-relations.md assigns it this authority. No upstream completion or ticket transfer is claimed. A concrete ScalaFIM consumer failure warrants its own bounded issue.

### bd-01KY4TXMG0YWBMNDVKPBSKS4XZ — Reconcile multivar documentation and contract API bindings with runtime

P1 → P3; open → closed; `retired-upstream`.

Retire from ScalaFIM scheduling: these tasks implement standalone multivar model lifecycle, estimators, IR, or documentation. modules/multivar is absent and build.sbt consumes the independent multivar project; docs/module-relations.md assigns it this authority. No upstream completion or ticket transfer is claimed. A concrete ScalaFIM consumer failure warrants its own bounded issue.

### bd-01KY4VDG22ED2C3WCFJ3GG1NPC — EPIC: one lawful multivar lifecycle from model program to IR

P1 → P3; open → closed; `retired-upstream`.

Retire from ScalaFIM scheduling: these tasks implement standalone multivar model lifecycle, estimators, IR, or documentation. modules/multivar is absent and build.sbt consumes the independent multivar project; docs/module-relations.md assigns it this authority. No upstream completion or ticket transfer is claimed. A concrete ScalaFIM consumer failure warrants its own bounded issue.

### bd-01KY4VH1XBMV2Q00BD1HJQT6QV — Emit and validate universal ModelRun IR from runtime fits

P1 → P3; open → closed; `retired-upstream`.

Retire from ScalaFIM scheduling: these tasks implement standalone multivar model lifecycle, estimators, IR, or documentation. modules/multivar is absent and build.sbt consumes the independent multivar project; docs/module-relations.md assigns it this authority. No upstream completion or ticket transfer is claimed. A concrete ScalaFIM consumer failure warrants its own bounded issue.

### bd-01KY4VH2PYYX3J46Q95KD92FCF — Release-gate the unified multivar lifecycle and remove legacy construction paths

P1 → P3; open → closed; `retired-upstream`.

Retire from ScalaFIM scheduling: these tasks implement standalone multivar model lifecycle, estimators, IR, or documentation. modules/multivar is absent and build.sbt consumes the independent multivar project; docs/module-relations.md assigns it this authority. No upstream completion or ticket transfer is claimed. A concrete ScalaFIM consumer failure warrants its own bounded issue.

### bd-01KYHNQ4T0S9S6A8ZER7N7QHDY — EPIC: extract standalone bids4s and migrate consumers

P1 → P3; open → closed; `retired-obsolete`.

ScalaFIM BIDS extraction/cutover is already on HEAD (76e4b77 ancestor), modules/bids is absent, and build.sbt uses exact standalone bids4s pins. Retire the old combined extraction/publication work item here; any remaining Maven/Eidolon release work belongs upstream and is not claimed complete.

### bd-01KYHNQYQR9N5TFWNFG6DWBTBJ — Release bids4s and remove embedded ScalaFIM module

P1 → P3; open → closed; `retired-obsolete`.

ScalaFIM BIDS extraction/cutover is already on HEAD (76e4b77 ancestor), modules/bids is absent, and build.sbt uses exact standalone bids4s pins. Retire the old combined extraction/publication work item here; any remaining Maven/Eidolon release work belongs upstream and is not claimed complete.

### bd-01KZ6BCP86MHP8HBHRWRDRTKN3 — [DEFERRED] Remove scalafim.locus facade vocabulary and MapEvidence

P1 → P3; open → open; `deferred-integration`.

locus-data still exists in build.sbt. This is an unfinished ownership/deletion migration, not an established defect in current computations. Keep the chain intact and defer; do not claim removal completed.

### bd-01KZ6BCRARE78N6HS59NN893TH — [DEFERRED] Remove duplicate ScalaFIM aggregation and Cats-only locus dependency

P1 → P3; open → open; `deferred-integration`.

locus-data still exists in build.sbt. This is an unfinished ownership/deletion migration, not an established defect in current computations. Keep the chain intact and defer; do not claim removal completed.

### bd-01M0NS3CM5KMPK7684W2WJ64BG — Add CanvasViewerController.dispose() that releases caches and ImageBitmaps

P1 → P1; review → closed; `completed-verified`.

Forward-port commits 8b5878d and 5495ea3 are ancestors of HEAD. Current Canvas runtime/controller dispose releases caches, ThreeJsRuntime.dispose calls loseContext, and typed ThreeJsRuntimeOptions carries clearColor. Existing issue notes record focused JS and consumer sentinel evidence. Close the stale review item; no new browser run or downstream pin adoption is claimed.

### bd-01M0NS3EFBY640V7D9C08B31V8 — ThreeJsRuntime.dispose() should force WebGL context loss

P1 → P1; review → closed; `completed-verified`.

Forward-port commits 8b5878d and 5495ea3 are ancestors of HEAD. Current Canvas runtime/controller dispose releases caches, ThreeJsRuntime.dispose calls loseContext, and typed ThreeJsRuntimeOptions carries clearColor. Existing issue notes record focused JS and consumer sentinel evidence. Close the stale review item; no new browser run or downstream pin adoption is claimed.

### bd-01M0T1CH3YJHZJ3YJY2E9CJYA5 — [DEFERRED] Optimize mesh4s cortical-scale topology construction

P1 → P3; open → open; `deferred-integration`.

The reported allocations are for a mesh4s/locus4s integration candidate. Current build.sbt has no mesh4s dependency and current surface module owns TriangleMesh/MeshTopology. Preserve historical benchmark evidence, but defer this provider-specific optimization to P3 until that integration is adopted. It does not establish a current ScalaFIM performance defect; qualify current topology independently before importing its targets.

### bd-01M0TGTH208WXGQ2CZJZM7RWRQ — Accelerate SurfaceWorldLink.nearestVertex with an owner-safe spatial index

P1 → P2; open → open; `core-performance`.

Retain bounded surface nearest-vertex acceleration with the exact world-space/owner-safe oracle. This is a measured scalability follow-up, below fitting and active PHRF hot paths.

### bd-01M0TN1MFACB485QP4ERG83BZ8 — Forward-port Neuropublish viewer lifecycle and clear colour prerequisites

P1 → P1; review → closed; `completed-verified`.

Forward-port commits 8b5878d and 5495ea3 are ancestors of HEAD. Current Canvas runtime/controller dispose releases caches, ThreeJsRuntime.dispose calls loseContext, and typed ThreeJsRuntimeOptions carries clearColor. Existing issue notes record focused JS and consumer sentinel evidence. Close the stale review item; no new browser run or downstream pin adoption is claimed.

### bd-01M0Z68QE7FFW1RDT8FPX2S4NT — Design a concise zero-cruft MVPA facade over identified evidence

P1 → P3; open → open; `deferred-feature`.

MVPA facade ergonomics is useful but secondary to correctness and measured allocation. Keep one ontology; defer API redesign until the core issues are resolved. Current-main check: the identified-evidence APIs named by this issue are absent from modules; first reproduce the concern against current MVPA APIs or identify the candidate branch. Historical evidence is not a current-main reproduction.

### bd-01M0Z7JBET61VHWHKSZRJ8HZ7A — Profile and reduce MVPA public result and identity allocation

P1 → P1; open → open; `core-performance`.

Retain P1 for qualification of core MVPA allocation through public APIs, preserving identity and estimability. The historical identified-evidence APIs are absent from current main; re-establish the baseline before implementing. Fitting fallback-status materialization is a separate current defect tracked by its own issue.

### bd-01M1WVPJ9MMPFDXAY5196E73D7 — Avoid full fallback status-vector allocation for single-voxel access

P1 → P1; open → open; `core-performance`.

High-priority core allocation work with concrete evidence. Profile and repair through public APIs without weakening identity, covariance, or estimability. Current source still contains full fallback-status materialization in point consumers; keep numerical equivalence and allocation budgets separate.

### bd-01M1Z1KN91XKADQ4Z80PBB781Q — Fit review: expose bounded identified residual traces for OLS and AR GLS

P1 → P1; open → open; `core-correctness`.

High-priority scientific validity: bounded fit diagnostics, run/whitening/covariance semantics, first-level uncertainty, calibration, or threshold p-value/cutoff agreement. Application origin does not make this optional integration; preserve native ownership and independent oracles.

### bd-01M1Z2050HGVC8FAZM07TM2K9Q — Qualify run-specific AR GLS estimates and covariance without changing coefficient scope

P1 → P1; open → open; `core-correctness`.

High-priority scientific validity: bounded fit diagnostics, run/whitening/covariance semantics, first-level uncertainty, calibration, or threshold p-value/cutoff agreement. Application origin does not make this optional integration; preserve native ownership and independent oracles.

### bd-01M20TKX7VYFT3BHCM7QAMA6NG — Admit calibrated group inference for unequal and estimated subject variances

P1 → P1; open → open; `core-correctness`.

High-priority scientific validity: bounded fit diagnostics, run/whitening/covariance semantics, first-level uncertainty, calibration, or threshold p-value/cutoff agreement. Application origin does not make this optional integration; preserve native ownership and independent oracles.

### bd-01M210WJ4BWVCXEMTARHDR2AC7 — Preserve first-level uncertainty provenance and degrees of freedom in group inputs

P1 → P1; open → open; `core-correctness`.

High-priority scientific validity: bounded fit diagnostics, run/whitening/covariance semantics, first-level uncertainty, calibration, or threshold p-value/cutoff agreement. Application origin does not make this optional integration; preserve native ownership and independent oracles.

### bd-01M21273A1SGJHQZP1W1X24SFZ — Support cutoff-period DCT drift in native first-level design and fitting

P1 → P1; doing → open; `core-correctness`.

DCT cutoff drift remains a core modeling capability. The recorded candidate commit 81f6c16 is not an ancestor of HEAD and current baseline source has no cutoff/DctBasis path. Reset stale doing status to open, retain exact provider admission and numerical requirements.

### bd-01M21BNZR9ZBRAYY9JD5WCQ8KX — Qualify joint effect-and-variance Gaussian bootstrap for group contrasts

P1 → P2; open → open; `core-assurance`.

Keep the joint effect/variance bootstrap as bounded research, not an already admitted method. First-level SE calibration and uncertainty provenance take priority; failed/deferred admission is a legitimate result.

### bd-01M21H6P1908Q5PGSZYWQZHWRN — Adopt image4s incremental NIfTI output for selected result maps

P1 → P1; open → closed; `completed-verified`.

The old canonical-provider/selected-output reconciliation blocker is resolved: 316a157 pins image4s ec56b348, Nifti.openScalarWriter delegates to the provider, and 8d21918 lands selected fits with independent persisted group readback. Existing continuation/commit receipts record pinned gates. Broader estimate-set schema/durability work remains open in the parent.

### bd-01M21VA8DYMNJS9PZ8076SBWWJ — Qualify first-level estimate and standard-error calibration under known truth

P1 → P1; open → open; `core-correctness`.

High-priority scientific validity: bounded fit diagnostics, run/whitening/covariance semantics, first-level uncertainty, calibration, or threshold p-value/cutoff agreement. Application origin does not make this optional integration; preserve native ownership and independent oracles.

### bd-01M23Q9Q92SBD5PHZZJYVY9EC3 — Reconcile selected-estimate work with the existing canonical image architecture

P1 → P1; open → closed; `completed-verified`.

The old canonical-provider/selected-output reconciliation blocker is resolved: 316a157 pins image4s ec56b348, Nifti.openScalarWriter delegates to the provider, and 8d21918 lands selected fits with independent persisted group readback. Existing continuation/commit receipts record pinned gates. Broader estimate-set schema/durability work remains open in the parent.

### bd-01M23ZX1KQB38EPD7W3PM56SE9 — Audit and qualify voxelwise threshold inference; repair max-null cutoff disagreement

P1 → P1; open → open; `core-correctness`.

High-priority scientific validity: bounded fit diagnostics, run/whitening/covariance semantics, first-level uncertainty, calibration, or threshold p-value/cutoff agreement. Application origin does not make this optional integration; preserve native ownership and independent oracles.

### bd-01M241Y5FPND8TPHTJQVHJWW7K — Remove cross-face texture contamination in JavaFX cortical surface display

P1 → P2; doing → open; `core-correctness`.

Retain renderer color/identity correctness; numerical/native candidate evidence is not main adoption. Old doing state has no live owner. Revalidate exact recovery source before integrating, without changing scientific interpolation semantics.

### bd-01M249KVV594XXRVFC0670RMAT — Restore a coherent Intaglio pin for the current surface-view APIs

P1 → P1; open → closed; `completed-verified`.

The stale Intaglio 2c8fd210 pin diagnosis is superseded: current build pins 596b398a and ordinary scalafimCompileAll (including surface/JavaFX projects) just passed without overrides or warnings. Close the pin-coherence blocker only; renderer and native adoption issues stay open.

### bd-01M24EQFARZKYMZ71BJP91C9XS — Reduce native draw cost after adaptive surface UV changes

P1 → P2; doing → open; `core-performance`.

Retain measured native draw/UV allocation work. Evidence belongs to an older isolated recovery candidate, not current main; reset inactive doing state and revalidate before implementation.

### bd-01M24KKCA32BDHHCSS3VRYFPPS — Integrate PR 6 and JavaFX recovery on provider-owned main

P1 → P3; doing → open; `deferred-integration`.

Historical PR6/provider/JavaFX merge bundle is not proven landed as a whole. Keep open but defer broad recovery integration; current core compiles. Revalidate live PR, exact provider pins and retained recovery changes before reuse. Do not recycle expired authorization failures as current blockers.

### bd-01M24MQABKEBQXW89VZ8XWBB8H — EPIC: Unified ProfileHrf condition and trial fitting with bounded compiled backends

P1 → P1; open → open; `active-phrf`.

User explicitly identifies PHRF as active priority. Preserve its required condition/trial implementation chain, upstream Gale banded-SPD prerequisite, readout, calibration and performance qualification. No claims, reservations, bodies or dependency edges on this active path are changed.

### bd-01M24MRY1DA0KPP37J7H8G619D — PHRF-06: Implement banded SPD factors, solves and logdet upstream in Gale and pin

P1 → P1; doing → doing; `active-phrf`.

User explicitly identifies PHRF as active priority. Preserve its required condition/trial implementation chain, upstream Gale banded-SPD prerequisite, readout, calibration and performance qualification. No claims, reservations, bodies or dependency edges on this active path are changed.

### bd-01M24MS4WEQN14GH3P56K1257J — PHRF-07: Implement the TrialBanded backend on shared Gram blocks

P1 → P1; open → open; `active-phrf`.

User explicitly identifies PHRF as active priority. Preserve its required condition/trial implementation chain, upstream Gale banded-SPD prerequisite, readout, calibration and performance qualification. No claims, reservations, bodies or dependency edges on this active path are changed.

### bd-01M24MSDQ0A8NK9YWT0SB4FCRX — PHRF-10: Deferred: finite-state innovation likelihood backend

P1 → P3; open → open; `deferred-phrf`.

PHRF-10 is explicitly deferred and opens only if the PHRF-07 dense-overlap checkpoint misses its budget. Keep that decision gate; do not schedule speculative finite-state backend work ahead of required PHRF delivery.

### bd-01M24MSJQZH70BTAFYTXD3MPZZ — PHRF-11: Add trial amplitudes, ML determinant and queries to the readout contract

P1 → P1; open → open; `active-phrf`.

User explicitly identifies PHRF as active priority. Preserve its required condition/trial implementation chain, upstream Gale banded-SPD prerequisite, readout, calibration and performance qualification. No claims, reservations, bodies or dependency edges on this active path are changed.

### bd-01M24MSZ4Z6BFWRV5R4S67VDN0 — PHRF-14: Qualify numerical approximation and scientific calibration

P1 → P1; open → open; `active-phrf`.

User explicitly identifies PHRF as active priority. Preserve its required condition/trial implementation chain, upstream Gale banded-SPD prerequisite, readout, calibration and performance qualification. No claims, reservations, bodies or dependency edges on this active path are changed.

### bd-01M24MT58X8W2Z0KJ25JJG2SGP — PHRF-15: Measure throughput, memory and backend crossovers

P1 → P1; open → open; `active-phrf`.

User explicitly identifies PHRF as active priority. Preserve its required condition/trial implementation chain, upstream Gale banded-SPD prerequisite, readout, calibration and performance qualification. No claims, reservations, bodies or dependency edges on this active path are changed.

### bd-01M24MTB363KSHN1KDRSAM2KNH — PHRF-16: Release unified ProfileHrf

P1 → P1; open → open; `active-phrf`.

User explicitly identifies PHRF as active priority. Preserve its required condition/trial implementation chain, upstream Gale banded-SPD prerequisite, readout, calibration and performance qualification. No claims, reservations, bodies or dependency edges on this active path are changed.

### bd-01M25Q0N1MG005XYTYE8HBS2ZB — PHRF-23: Optional: frozen readout fields for matrix-free MVPA

P1 → P3; open → open; `optional-phrf`.

Explicitly optional PHRF extension. Keep visible under the active product, but below the required TrialBanded/readout/calibration/performance/release chain; existing optional status and dependencies are preserved.

### bd-01M25Q0P2ANA5VBKFZR7ZXNW8J — PHRF-24: Optional: bounded stimulus-feature amplitude structure

P1 → P3; open → open; `optional-phrf`.

Explicitly optional PHRF extension. Keep visible under the active product, but below the required TrialBanded/readout/calibration/performance/release chain; existing optional status and dependencies are preserved.

### bd-01M25RZGYEC3SSXCYBF84MWG22 — Support paired lateral and medial surface cameras with per-slot render and pick correspondence

P1 → P3; doing → open; `deferred-feature`.

Paired per-slot cameras are a useful viewer capability, but not the current core numerical/performance priority. Consumer backport qualification is not native adoption; reset inactive doing state and retain provenance.

### bd-01M26A285YAQ3H8TGS26BK9T7J — PHRF-29: Add `ProfileHrfPlan` and attach the trial backend to the executor

P1 → P1; open → open; `active-phrf`.

User explicitly identifies PHRF as active priority. Preserve its required condition/trial implementation chain, upstream Gale banded-SPD prerequisite, readout, calibration and performance qualification. No claims, reservations, bodies or dependency edges on this active path are changed.

### bd-01M26A2AWJJ0RW7D2MHBA7RPEY — PHRF-30: Optional: exact seven-state Cascade34 realization

P1 → P3; open → open; `optional-phrf`.

Explicitly optional PHRF extension. Keep visible under the active product, but below the required TrialBanded/readout/calibration/performance/release chain; existing optional status and dependencies are preserved.

### bd-01M26A2D7WSNMK0JRE1JKC12TK — PHRF-31: Optional: voxelwise AR through lag-Gram expansion

P1 → P3; open → open; `optional-phrf`.

Explicitly optional PHRF extension. Keep visible under the active product, but below the required TrialBanded/readout/calibration/performance/release chain; existing optional status and dependencies are preserved.

### bd-01KX2CQZGWBQ3KKX7BWE28X3T2 — Extend group and meta-analysis interpreters

P2 → P3; open → open; `deferred-feature`.

PM/mKH repair exists; remaining REML/robust/multicontrast estimator expansion is feature work. Prioritize calibration/uncertainty of supported methods before broadening the estimator catalog.

### bd-01KX2CQZHGH43S93RK1KCY7ES1 — Add typed result bundles and export adapters

P2 → P2; open → open; `core-correctness`.

Keep typed result/export semantics but flag overlap with the newer estimates/estimates-io/fit-estimates architecture. Review remaining format coverage and contracts before adding a second result hierarchy.

### bd-01KX50HA4TGKKWR1CC2G8HRY2D — Design compressed RRG under voxelwise AR whitening

P2 → P3; open → open; `deferred-feature`.

Compressed voxelwise-AR reduced-rank GLS is a new scientific capability with a typed unsupported boundary. Keep deferred until geometry and covariance have an independent oracle; do not treat the refusal as a current numerical bug.

### bd-01KXSQ2Z2BW2CRQFR0X8V59B84 — Frame adapter: Gale numerical views with explicit lifetime and allocation receipts

P2 → P3; open → closed; `retired-upstream`.

Retire from ScalaFIM scheduling: generic Frame/Parquet/DuckDB/Gale/spill implementation belongs to extracted frame4s (b6a3af2 is an ancestor; embedded Frame module is absent). This is scope retirement, not a claim upstream work is implemented. Reopen only a narrow ScalaFIM consumer defect with concrete evidence.

### bd-01KXSQ2Z2M5XVAJ2CVWMAGK2Z7 — Frame adapter: DuckDB engine with Arrow handoff and semantic conformance

P2 → P3; open → closed; `retired-upstream`.

Retire from ScalaFIM scheduling: generic Frame/Parquet/DuckDB/Gale/spill implementation belongs to extracted frame4s (b6a3af2 is an ancestor; embedded Frame module is absent). This is scope retirement, not a claim upstream work is implemented. Reopen only a narrow ScalaFIM consumer defect with concrete evidence.

### bd-01KXSQ44YSMQSPDBXGCVMKFHGV — Frame adapters acceptance: conformance matrix, receipts, examples, and release boundaries

P2 → P3; open → closed; `retired-upstream`.

Retire from ScalaFIM scheduling: generic Frame/Parquet/DuckDB/Gale/spill implementation belongs to extracted frame4s (b6a3af2 is an ancestor; embedded Frame module is absent). This is scope retirement, not a claim upstream work is implemented. Reopen only a narrow ScalaFIM consumer defect with concrete evidence.

### bd-01KXSQ4RBY8ZP2E2FE12XWHSSJ — Epic: Frame adapters — Parquet, production engines, Gale, and advanced execution without core coupling

P2 → P3; open → closed; `retired-upstream`.

Retire from ScalaFIM scheduling: generic Frame/Parquet/DuckDB/Gale/spill implementation belongs to extracted frame4s (b6a3af2 is an ancestor; embedded Frame module is absent). This is scope retirement, not a claim upstream work is implemented. Reopen only a narrow ScalaFIM consumer defect with concrete evidence.

### bd-01KXSQ7N14B9KX18067V4B68SH — [DEFERRED] group-ntab: inferential bridge from Frame metadata and exact-support ntab features

P2 → P3; open → open; `deferred-feature`.

ntab/group-ntab is a proposed new feature layer with stale incubated-Frame references. Keep its scientific intent, but defer implementation and rebase onto standalone frame4s only when a current use case justifies it.

### bd-01KXYJPWYV08VFWRMCYYDPZCAV — Scalafim Gale migration 5: remove related matrix and solver duplicates

P2 → P2; open → open; `core-maintenance`.

Narrow remaining duplicate-math audit is still relevant: HRF-local Mat/Vec and design QR adapter files remain. Generic solvers belong in Gale, but deletion/renaming alone is not a performance result. Retire old broad migration parents, retain this bounded review.

### bd-01KY4TXMTY9M15ZME5AVDSWQWR — Add task-oriented builders over the multivar proof core

P2 → P3; open → closed; `retired-upstream`.

Retire from ScalaFIM scheduling: these tasks implement standalone multivar model lifecycle, estimators, IR, or documentation. modules/multivar is absent and build.sbt consumes the independent multivar project; docs/module-relations.md assigns it this authority. No upstream completion or ticket transfer is claimed. A concrete ScalaFIM consumer failure warrants its own bounded issue.

### bd-01KY4TXNRWM0N4RX65D6JB8KAX — Expose dense backend admission and realized materialization

P2 → P3; open → closed; `retired-upstream`.

Retire from ScalaFIM scheduling: these tasks implement standalone multivar model lifecycle, estimators, IR, or documentation. modules/multivar is absent and build.sbt consumes the independent multivar project; docs/module-relations.md assigns it this authority. No upstream completion or ticket transfer is claimed. A concrete ScalaFIM consumer failure warrants its own bounded issue.

### bd-01KY4VH29QS6SNNKKBB0FMBYKS — Adopt the unified lifecycle across every public multivar estimator

P2 → P3; open → closed; `retired-upstream`.

Retire from ScalaFIM scheduling: these tasks implement standalone multivar model lifecycle, estimators, IR, or documentation. modules/multivar is absent and build.sbt consumes the independent multivar project; docs/module-relations.md assigns it this authority. No upstream completion or ticket transfer is claimed. A concrete ScalaFIM consumer failure warrants its own bounded issue.

### bd-01KZ3ZEGV87J05YM4GJE371QA1 — Phase 4: close the computational assurance and release court

P2 → P1; open → open; `core-assurance`.

Retain first-level scientific identity, independent numerical evidence, executable examples, and measured allocation/performance gates as core priorities. Historical branch/CI claims need refresh; do not mistake current compilation for full release certification.

### bd-01KZ3ZPJ89W7W86084JQRDV7JD — P4.4: establish performance, executable-documentation, and final release court

P2 → P1; doing → open; `core-assurance`.

Retain first-level scientific identity, independent numerical evidence, executable examples, and measured allocation/performance gates as core priorities. Historical branch/CI claims need refresh; do not mistake current compilation for full release certification.

### bd-01M0NS3ERVZDN9GXY9W7K5X873 — Typed reorder and colorizer-replacement actions for image-view

P2 → P3; open → open; `deferred-feature`.

Layer reorder/colorizer actions are viewer API expansion; defer below current core numerical and allocation defects.

### bd-01M0NS3F26089W9CSFJS03BM2N — CanvasScrollCoordinator needs cancel() for host unmount

P2 → P2; open → open; `core-correctness`.

Unmount cancellation/resource lifetime is a bounded existing-runtime defect; retain independently of application integration or viewer feature expansion.

### bd-01M0QP2PZP4H2W07PEMX3X3NB7 — GIFTI: support NIFTI_TYPE_FLOAT64 data arrays

P2 → P2; open → open; `core-correctness`.

GIFTI Float64 is a bounded format-support gap with explicit rejection; retain at P2, below silent geometry/statistical correctness risks.

### bd-01M0QP352SC1GXD98YB2TDC8HG — GIFTI: choose the coordinate transform by its spaces, not transforms.headOption

P2 → P1; open → open; `core-correctness`.

Current GiftiSurfaceCodec still selects transforms.headOption. Coordinate-space misidentification can silently misplace scientific data, so promote to P1 with explicit space/transform provenance and independent geometry fixtures.

### bd-01M0SYJVY1ZAQ49KCRGBDZ2ADW — Make the surface-view clear colour a typed runtime option

P2 → P1; review → closed; `completed-verified`.

Forward-port commits 8b5878d and 5495ea3 are ancestors of HEAD. Current Canvas runtime/controller dispose releases caches, ThreeJsRuntime.dispose calls loseContext, and typed ThreeJsRuntimeOptions carries clearColor. Existing issue notes record focused JS and consumer sentinel evidence. Close the stale review item; no new browser run or downstream pin adoption is claimed.

### bd-01M0V23MZJBCM1ZQKWT00RDM1N — [DEFERRED] Replace ScalaFIM motion core with reframe4s-motion authority

P2 → P3; blocked → blocked; `deferred-integration`.

Respect the explicit 2026-08-24 user deferral: reframe4s-motion replacement resumes only on renewed product direction. Current task is core stabilization, not authority migration; retain blocked status and preserve existing fMRI QC functionality.

### bd-01M0Z696YYJC1QRNQRF8SF2CVV — Decide ownership of observation-RDM model comparison

P2 → P3; open → open; `deferred-feature`.

Observation-RDM query ownership/API expansion is not an established defect; defer until core MVPA semantics and allocation are qualified. Current-main check: the identified-evidence APIs named by this issue are absent from modules; first reproduce the concern against current MVPA APIs or identify the candidate branch. Historical evidence is not a current-main reproduction.

### bd-01M0Z69N65WFHE7DCPVZXG4J9P — Certify native searchlight neighborhood semantics against PyMVPA

P2 → P1; open → open; `core-assurance`.

Promote native searchlight support/radius/boundary qualification: exact support identity directly determines the scientific estimand. Use independent coordinate fixtures and measured setup/kernel cost, without a compatibility layer. Current-main check: the identified-evidence APIs named by this issue are absent from modules; first reproduce the concern against current MVPA APIs or identify the candidate branch. Historical evidence is not a current-main reproduction.

### bd-01M0Z8QAXS7H3MP6PES44FCTW8 — Remove dataset selection deprecations exposed by the MVPA gate

P2 → P2; open → closed; `completed-verified`.

Current DataSelection uses indices and DatasetAcquisitionDomain no longer contains the cited .at/pointOption calls; the integrated default-pin compile gate passed warning-clean. Close the obsolete deprecation finding.

### bd-01M1W6JR7PBT8FYM7DT808V6F4 — Preserve explicit temporal sampling metadata in NIfTI IO

P2 → P1; open → open; `core-correctness`.

Temporal sampling metadata affects scientific meaning, so retain high priority. The implementation has since moved to provider-owned NIfTI options/native headers: revalidate TR/origin/units round trips before changing code or claiming the old lightweight-header diagnosis still applies.

### bd-01M1X52T0SJK92JXQAW9N17X4V — Align reference raster lighting capability metadata with implemented shading

P2 → P2; open → open; `core-correctness`.

Keep the bounded capability-versus-pixel truthfulness audit. Renderer admission metadata should match supported shading; lower priority than statistical and coordinate correctness.

### bd-01M208TXSYF5Y5GZQ2DY4GZP8S — Reduce allocation in selected precision pooling without changing the estimator

P2 → P1; open → open; `core-performance`.

High-priority core allocation work with concrete evidence. Profile and repair through public APIs without weakening identity, covariance, or estimability. Current source still contains full fallback-status materialization in point consumers; keep numerical equivalence and allocation budgets separate.

### bd-01M24KM6PN5JX95MEWW8WXEVXR — Qualify the Alder dependency in an isolated ScalaFIM checkout

P2 → P3; open → closed; `retired-obsolete`.

The cited Alder dependency does not exist in current build.sbt or module Scala sources. Default-pin isolated and integrated full compile gates passed without Alder sibling discovery. Retire these stale-branch consumer blockers; no claim that standalone Alder itself was repaired.

### bd-01M28PRPACQDQP8E1K6R7R74PZ — Consumers load Alder, whose build discovers ../gale, ../resample4s and ../linop4s from the working directory

P2 → P3; open → closed; `retired-obsolete`.

The cited Alder dependency does not exist in current build.sbt or module Scala sources. Default-pin isolated and integrated full compile gates passed without Alder sibling discovery. Retire these stale-branch consumer blockers; no claim that standalone Alder itself was repaired.

### bd-01M28WR74HX73YMKTVJ1DJCH4M — B-spline basis drops its intercept spline, so it cannot represent a flat response

P2 → P1; open → open; `core-correctness`.

Promote the B-spline basis/estimand contract review. Current bsplineBasis drops the first column; constant-response and noninteger-knot assumptions need an independent oracle and explicit policy. Do not blindly change causal/normalization conventions merely to match another library.

### bd-01KX2CQZFRZHXS40YWA9PDX0DV — [DEFERRED] Add optional edge plugin catalog

P3 → P3; open → open; `deferred-feature`.

Optional plugin discovery, namespace cleanup, or browser-demo packaging is outside the current core correctness/performance focus. Keep for later with a concrete trigger; no completion claim.

### bd-01KXSQ3R06HBQB7MRDWN6BEEN0 — Frame execution: spill, segmented backends, async pulls, and evidence-driven physical planning

P3 → P3; open → closed; `retired-upstream`.

Retire from ScalaFIM scheduling: generic Frame/Parquet/DuckDB/Gale/spill implementation belongs to extracted frame4s (b6a3af2 is an ancestor; embedded Frame module is absent). This is scope retirement, not a claim upstream work is implemented. Reopen only a narrow ScalaFIM consumer defect with concrete evidence.

### bd-01KYX0F70J38699FPVBYKCGZDP — [DEFERRED] Decide the fate of the Vec/Mat re-export from package object hrf

P3 → P3; open → open; `deferred-feature`.

Optional plugin discovery, namespace cleanup, or browser-demo packaging is outside the current core correctness/performance focus. Keep for later with a concrete trigger; no completion claim.

### bd-01M0NS3FNR5RCYEP49GNGN6GWN — [DEFERRED] Pin Three.js and fix browser page import paths

P3 → P3; open → open; `deferred-feature`.

Optional plugin discovery, namespace cleanup, or browser-demo packaging is outside the current core correctness/performance focus. Keep for later with a concrete trigger; no completion claim.
