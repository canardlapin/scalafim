# ProfileHrf work-item index

Updated 2026-09-10. Canonical epic: `bd-01M24MQABKEBQXW89VZ8XWBB8H`. Scientific architecture and numerical/resource contracts: [profile-hrf.md](profile-hrf.md).

One expanded epic preserves PHRF-01..16 and adds PHRF-17..24. PHRF-01..22 are required; PHRF-23/24 are optional post-v1 work. PHRF-21 is the early condition-only qualification milestone. Dependencies are blocking unless stated otherwise. All implementation tickets are open; the act of revising this plan does not implement them.

| Ticket | Mote ID | Depends on | Scope |
| --- | --- | --- | --- |
| PHRF-01: Freeze unified condition/trial estimands and performance cohorts | `bd-01M24MRG6N43785BB75ZCZYK2J` | Ready | Required |
| PHRF-02: Add constrained scalar-family and second-order realization contracts | `bd-01M24MRJ6BWQ2M6BSAR03CWQZF` | PHRF-01 | Required |
| PHRF-03: Admit Gaussian and LWU derivatives and explicit half-cosine subfamilies | `bd-01M24MRN5PT063KQG00N9TMQ5D` | PHRF-02 | Required |
| PHRF-04: Implement continuous Cascade34 and its exact seven-state realization | `bd-01M24MRR3DPF01JA51ACZZ46A2` | PHRF-02 | Required |
| PHRF-05: Lower common HRF drives into condition and trial amplitude axes | `bd-01M24MRV17VNXFCB44QXVV5RZS` | PHRF-01 | Required |
| PHRF-06: Qualify Gale banded factors and admit exact provider revision | `bd-01M24MRY1DA0KPP37J7H8G619D` | PHRF-01 | Required |
| PHRF-07: Implement ridge-and-release TrialBanded backend | `bd-01M24MS4WEQN14GH3P56K1257J` | PHRF-03, PHRF-05, PHRF-06, PHRF-08, PHRF-20 | Required |
| PHRF-08: Implement common profile reduction, criteria and independent dense laws | `bd-01M24MS0YSMYZ7DGB130Q12K79` | PHRF-02, PHRF-05, PHRF-17 | Required |
| PHRF-09: Build one bounded router and constrained decoder for all backends | `bd-01M24MSAQZ2MF2KGQEG1K33E6C` | PHRF-08, PHRF-20 | Required |
| PHRF-10: Compile exact Cascade34 innovation likelihood and observed derivatives | `bd-01M24MSDQ0A8NK9YWT0SB4FCRX` | PHRF-04, PHRF-05, PHRF-08, PHRF-20 | Required |
| PHRF-11: Add bounded trial amplitudes and queries to the shared readout contract | `bd-01M24MSJQZH70BTAFYTXD3MPZZ` | PHRF-07, PHRF-09, PHRF-10, PHRF-22 | Required |
| PHRF-12: Integrate one shape-aware plan and bounded condition/trial executor | `bd-01M24MSQS8HFQBPY2TAVW1FJGM` | PHRF-05, PHRF-08, PHRF-19 | Required |
| PHRF-13: Share bounded pilot pooling and freeze all response-derived preparation | `bd-01M24MSVV1BRRGPFRW6A27DCX9` | PHRF-09, PHRF-18 | Required |
| PHRF-14: Qualify numerical approximation and scientific calibration | `bd-01M24MSZ4Z6BFWRV5R4S67VDN0` | PHRF-03, PHRF-11, PHRF-12, PHRF-13, PHRF-21 | Required |
| PHRF-15: Measure ProfileHrf throughput, memory and backend crossovers | `bd-01M24MT58X8W2Z0KJ25JJG2SGP` | PHRF-11, PHRF-12, PHRF-13, PHRF-14, PHRF-21 | Required |
| PHRF-16: Release unified ProfileHrf with compact conditions and qualified trial backends | `bd-01M24MTB363KSHN1KDRSAM2KNH` | PHRF-03, PHRF-04, PHRF-12, PHRF-14, PHRF-15, PHRF-21, PHRF-22 | Required |
| PHRF-17: Repair coefficient-penalty transport and qualify normalization invariance | `bd-01M25Q0ETQHAQ084S6S5MVPXD8` | PHRF-01 | Required |
| PHRF-18: Compile bounded observable condition families with rank and projector receipts | `bd-01M25Q0FVB7MPEQ3PQCCJR6WDZ` | PHRF-03, PHRF-05, PHRF-08, PHRF-20 | Required |
| PHRF-19: Implement compact condition scoring and continuous amplitude readout | `bd-01M25Q0GVNRDS6Q8PXN7SFC84X` | PHRF-18, PHRF-09 | Required |
| PHRF-20: Establish early condition/trial benchmarks and strict work accounting | `bd-01M25Q0HX1P7AB9HPBRWRZPH1N` | PHRF-01 | Required |
| PHRF-21: Qualify the first condition-only ProfileHrf milestone independently | `bd-01M25Q0JY8GA7CJRKZF2934PJ0` | PHRF-12, PHRF-13, PHRF-19, PHRF-20, PHRF-22 | Required |
| PHRF-22: Define signed condition/trial query outputs with direct compact sinks | `bd-01M25Q0M14Q0A7TZX387W9CSST` | PHRF-19, PHRF-12 | Required |
| PHRF-23: Optional: qualify frozen readout fields for matrix-free MVPA | `bd-01M25Q0N1MG005XYTYE8HBS2ZB` | PHRF-16 | Optional |
| PHRF-24: Optional: admit bounded stimulus-feature amplitude structure | `bd-01M25Q0P2ANA5VBKFZR7ZXNW8J` | PHRF-16, PHRF-22 | Optional |

The parent epic is blocked by PHRF-16. The graph has 64 blocking links including this release edge. Condition milestone PHRF-21 has no dependency on PHRF-04/06/07/10/11/14/15/16. Optional work depends on core release, never the reverse.

## Early execution

PHRF-01 is the sole ready implementation ticket. Then PHRF-17 (penalty repair), PHRF-20 (benchmark harness), family contracts, drive contracts and provider inspection can proceed through the declared graph. The compact condition path reaches PHRF-21 before full trial release. Backend implementation includes its own measurements; PHRF-15 consolidates final evidence.

## PHRF-01: Freeze unified condition/trial estimands and performance cohorts

`bd-01M24MRG6N43785BB75ZCZYK2J`

Scope: freeze one ProfileHrf scientific model with explicit amplitude structures and measurable accuracy/resource contracts before implementing backends.
Acceptance:

- ConditionMeans is the exact alpha=0, u=0 case. ConditionCenteredTrials accepts finite positive alpha=1/lambda; never use huge lambda, epsilon switching, or trial-sized objects to implement condition-only fitting.
- One shape group per voxel shared by signed condition/trial amplitudes; structural coefficient/run/pulse identities and full-rank free means. Keep within-voxel sharing distinct from regional borrowing.
- Choose PenalizedProfile or full constrained TrialRandomEffectsML explicitly; freeze sigma, alpha and observation geometry in the declared amplitude convention. At alpha=0 full covariance is identity and the criteria coincide. Penalized energy is not residual sum of squares. Preserve the projected-determinant counterexample.
- Define normalization/amplitude/penalty units jointly, including shape-dependent scale derivatives; effective drive-conditional response interpretation is distinct from vascular evidence, data identification and numerical acceptance.
- Freeze C0 and B0 inputs, local/edge cohorts, seeds, signal/noise regimes, family domains, rank/conditioning tolerances and actual CPU/OS/JDK/provider reference host. C0: T=600,C=3,F=6,V=100000,d<=3,K<=96 (C<=8 capacity); <=30 s proposed compute goal. B0 and all previous trial targets stay unchanged, including <=120 s and <=256 MiB. Both retain <=8 prepared references and <=2 evaluated references per voxel.
- State exact, empirical and certified evidence separately. Supplied demos with 60 centers, K=69, sampled peak normalizations or local optimizer references are research evidence, not admitted production kernels.
Performance: publish complete operation/memory accounting and measurable empty receipt schema with PHRF-20; no default family/rank/domain is chosen merely to meet speed. Rank or routing budget failure must refuse rather than relax accuracy.

## PHRF-02: Add constrained scalar-family and second-order realization contracts

`bd-01M24MRJ6BWQ2M6BSAR03CWQZF`

Owner: hrf shared. Acceptance:

- Typed validated shape parameters, coordinate charts, scalar values, first/mixed-second derivatives and support/smoothness/normalization metadata.
- Optional exact-state realization recipe distinguishes exact, approximate and unavailable; scientific family identity never follows backend choice.
- Deriv(time) and LwuBasis are not mistaken for normalized second-order shape derivatives; chain-rule and normalization tests independently verify the new contract.
- Exactness, finite support, state order, observation semantics and readout eligibility are explicit capabilities. Invalid parameters fail at construction.
- JVM+JS suites and hrf laws pass. No fit/dataset dependency or generic private matrix solver added to hrf.
Performance: value/jet evaluation uses allocation-controlled primitive workspaces; d<=3 has at most ten value/derivative components, with no fitted per-trial basis expansion.

Unified boundary: use one ParametricHrfFamily/shape-family contract for ConditionMeans and ConditionCenteredTrials. Keep it distinct from ResponseBasis and the existing HrfFamily descriptor name. Family constraints and optional realization/partial-linear recipes contain no events, priors or compiled fit matrices. Value/gradient/Hessian layouts are reusable; backends own primitive workspaces. Couple amplitude units and penalty conversions through PHRF-17/08 laws.

## PHRF-03: Admit Gaussian and LWU derivatives and explicit half-cosine subfamilies

`bd-01M24MRN5PT063KQG00N9TMQ5D`

Owner: hrf. Acceptance:

- Preserve causal public Gaussian and two-Gaussian LWU definitions; one shared rho, correct parameter-to-peak/FWHM summaries and one fixed smooth normalization rule.
- First/mixed-second shape derivatives agree with independent analytic/AD and finite-difference step sweeps, including pulse convolution and normalization factors.
- R parity fixtures distinguish raw-family agreement from intentional ScalaFIM causality/integration differences. Treat span separately from support; tail errors bound values and derivatives.
- Half-cosine retains its six-parameter identity. Explicit 2-3 parameter tied/fixed charts and cells are admitted only where required smoothness holds; moving-join counterexamples return NonsmoothCell instead of silently smoothing.
- JVM+JS tests cover bounds, rho=0, near-canceling area, normalization maxima, integration and rejected cells.
Performance: no dense trial-time design per voxel; support/derivative approximation error and preparation cost are recorded per family.

Shared admission: both condition and trial backends use exactly the same normalized scalar families and parameter charts. Fit-specific compact U is not a scientific ResponseBasis. Existing LwuBasis normalizePrimary behavior cannot supply naive parameter ratios or normalized second-order jets.

## PHRF-04: Implement continuous Cascade34 and its exact seven-state realization

`bd-01M24MRR3DPF01JA51ACZZ46A2`

Owner: hrf shared. Acceptance:

- Implement g3(t,kp)-rho*g4(t,ku), causal unbounded support, kp>ku>0 and rho>=0; unit-area positive/negative components, signed integral 1-rho, no division by 1-rho. Rho is area ratio.
- Validate parameter chart, preserve rate ordering, report actual peak/width/undershoot and tail-rate nonidentification at rho=0. Document coupled latency/width.
- Implement exact A_n(dt)=exp(-k dt) sum_{j=0}^{n-1}(k dt)^j J_n^j/j!, common injection b=(kp,0,0,ku,0,0,0), and output c=(0,0,1,0,0,0,-rho). Seven states, three fitted shape parameters.
- Independent closed-form Erlang, generic expm, semigroup, area, causality, stable bounds and first/mixed-second parameter derivative tests pass on JVM+JS.
- Analytic primitive supports ordinary BoxHeight/BoxMass convolution without claiming finite-state random-pulse elimination. Add HrfKind/HrfParams and exhaustive dispatch support.
Performance: fixed seven-state realization; exact scalar/finite-polynomial transitions, no time-stepping or per-evaluation generic matrix exponential. Tail truncation is not needed for exact state execution.

Condition mode can compile this exact family through aggregated deterministic drives without a trial covariance engine. Fast state admission remains an independent complete-drive capability; never force CompactCondition to use seven-state trial filtering.

## PHRF-05: Lower common HRF drives into condition and trial amplitude axes

`bd-01M24MRV17VNXFCB44QXVV5RZS`

Owner: design shared; reuse EventSchedule/EventPhase, structural condition/phase/modulator/run identities, RowLayout and existing axis types.
Acceptance:

- HrfDrivePlan retains unconvolved complete drives, family and one within-voxel shape-sharing identity, with explicit condition-aggregation and trial lowering. Numerical compact U, candidates, stencils and likelihoods belong in fit.
- ConditionMeans builds condition channels directly: summing trial drive contributions before convolution agrees with X(theta)M in independent oracles, without allocating T*N or a trial covariance. Arbitrary admitted amplitude channels have explicit estimands; only one-hot trial membership implies arithmetic condition means.
- Trial mode retains TrialId, one-hot ConditionId/run scope, active counts, output ordering after onset sorting, repeated phases sharing one coefficient, known drive gains, empty/singleton/coincident cases and full pulse semantics.
- Forward/adjoint and shape-derivative products match direct designs for on/off-grid impulses, BoxHeight/BoxMass, repeated drives and run resets. A censored observation skips the update but does not reset physiological HRF state or discard event inputs.
- Shared mask/noise/weight geometry has a stable fingerprint. State admission validates the complete random drive; no independent onset/offset shocks for one box amplitude, no unsupported observation averaging.
- JVM+JS identity/permutation/scenario contracts; fixed DesignSchema/HrfAssignment semantics stay intact.
Performance: aggregate condition drives and impulse covariance counts once, preserve streaming event intersections, no dense trial-time design per voxel; condition runtime has no N-sized workspace.

## PHRF-06: Qualify Gale banded factors and admit exact provider revision

`bd-01M24MRY1DA0KPP37J7H8G619D`

This is a ScalaFIM consumer/admission gate for native Gale capability, not permission to implement private generic factors here.
Acceptance:

- Confirm pinned Gale capability; open/link native Gale work if reusable SPD banded factorization/solve/logdet/conditioning or multi-RHS support is absent. No foreign-store dependency edge.
- Provider supplies independently tested banded factors, reusable solves and rank-aware small QR/Cholesky contracts on JVM+JS. Preserve packed storage and transpose/ownership semantics.
- Pin and qualify the exact provider artifact/revision against affected ScalaFIM consumers; record provider evidence separately from downstream and full-build evidence.
- Reject/diagnose non-SPD or unreliable systems; do not silently add regularization to free means or densify large bands.
Performance: O(N*b^2) preparation, O(N*b) factor storage and O(N*b) per-RHS solve for admitted bands, with allocation/cost measurements. Native banded performance must not be claimed from storage-only APIs.

Scope boundary: this gate concerns banded trial capabilities only. Condition compression uses admitted portable Gale QR/SVD/rank facilities and must not wait for this issue. Verify build.sbt's current exact pin rather than relying on an old provider HEAD claim.

## PHRF-07: Implement ridge-and-release TrialBanded backend

`bd-01M24MS4WEQN14GH3P56K1257J`

Owner: fit/profile. Acceptance:

- Prepare X'X+lambda I and small [F,XM] release with Gale; preserve signed unpenalized condition means and exact within-condition zero-sum deviations.
- Direct dense parity for solves, energy, score jets and ML determinant; include unequal/singleton conditions, coincidences, nuisance aliasing, permutations/signs, lambda extremes and run boundaries.
- Build derivative bands and low-rank corrections without dense N*N construction; Gaussian/LWU tails require explicit original-family value/derivative error accounting.
- Emit rank and conditioning diagnostics; no hidden mean shrinkage, iterative escape or family substitution. JVM+JS and scenario tests pass.
Performance: no per-voxel factors; prepared solves retain banded scaling and exact work counts. Measure sparse/dense bandwidth crossover and all filtering/derivative costs against B0.

Common-engine requirements: consume PHRF-08 jets/criterion, PHRF-09 decoder and PHRF-22 query contract; do not duplicate prior signs, normalization/ML logic or diagnoses. Preserve factor/cache ownership and measure complete attempted-voxel work using PHRF-20 as the implementation lands.

## PHRF-08: Implement common profile reduction, criteria and independent dense laws

`bd-01M24MS0YSMYZ7DGB130Q12K79`

Owner: model declarations and fit/profile shared; this is the DRY boundary, not a common dense storage layout.
Acceptance:

- One amplitude-structure ADT has exact ConditionMeans and positive-alpha ConditionCenteredTrials. ProfileHrfPlan is a sibling of fixed FitPlan initially; no fabricated fixed design or parallel condition/trial public engine.
- ProfileJet contains energy/value, gradient, symmetric observed Hessian, reference/chart, rank and approximation receipt. Criterion assembly adds sigma scaling/full logdet once; shared decoder adds the prior once. Backend-specific response encodings/workspaces avoid forcing time-domain blocks onto compact condition fitting.
- Implement/test E=s-b'G^-1 b, E_p=s_p-2b_p'w+w'G_p w and E_pq=s_pq-2b_pq'w+w'G_pq w-2r_p'G^-1 r_q, w=G^-1 b,r_p=b_p-G_p w. Temporal-filter s(theta) changes with shape; omitting its derivatives must fail a regression.
- Independent augmented/direct contrast solves verify ridge-and-release, exact alpha=0 condition QR, free-mean rank failures, arbitrary RHS readout solves and energy versus actual residual energy. Include unequal/singleton/coincident conditions, signs and permutations.
- Full K=I+alpha XPX' determinant and first/mixed-second derivatives match an independent dense covariance oracle. Keep nuisance-contrast/full-data ML distinct; at alpha=0 logdet is zero. Prefer a stable positive small determinant expression when subtractive I-U'RU is poorly conditioned.
- Normalization conversions preserve predictions, units, penalty energy, covariance and jets when alpha/lambda are transported; fixed-numeric-penalty comparisons explicitly change the model. Use constant-rank cells and fixed preparation or explicitly differentiated scale laws.
- Portable JVM+JS conditioning-aware tolerances and finite-difference step sweeps/independent AD; no expected-information substitution.
Performance: share reduction identities/criterion, not mandatory dense G/Gp/Gpq or generic per-scalar interfaces. Dispatch once per block; dense oracles are test/research only.

## PHRF-09: Build one bounded router and constrained decoder for all backends

`bd-01M24MSAQZ2MF2KGQEG1K33E6C`

Owner: fit/profile shared.
Acceptance:

- One decoder consumes criterion jets from CompactCondition, TrialBanded and FiniteState. Enumerate feasible stationary points and boundaries for d<=3, checking indefinite curvature, singular faces, ties, nonlinear constraints and chart/rank admission; add priors with correct signs.
- Default bank <=8 references per observation geometry and <=2 evaluated references per voxel, counting value-only routing, stencil-node and finalist score work. A scan of 60 or 8 centers is not free routing. Use frozen pilot/region routing or a qualified bounded compact-summary router; otherwise refuse.
- Compile stencils/derivative coefficients into local jets; do not inherit the demo's 19 runtime node evaluations per refined reference. Count all contraction ranks and operations. ValueOnly/ValueAndGradient/ValueGradientHessian requests are explicit, with no hidden expensive order escalation.
- Compare score values in a consistent criterion/chart, preserve data-only versus prior curvature and weak/prior-dominated/ambiguous/boundary/budget statuses.
- Original-model shape approximation requires score/remainder plus identification; amplitude residual alone is insufficient. Compare the complete frozen attempted cohort against sufficiently searched dense references; no post-hoc accepted-case cherry-picking.
- JVM+JS deterministic routing and strict work counters; no recentering, line search, iterative shape optimization or expensive fallback.
Performance: retain hard reference caps and accuracy goals. A wider bank/search mode requires an explicit separately measured target revision; failures cannot silently increase compute.

## PHRF-10: Compile exact Cascade34 innovation likelihood and observed derivatives

`bd-01M24MSDQ0A8NK9YWT0SB4FCRX`

Owner: fit/profile. Acceptance:

- Shared covariance recursion uses common branch trial shocks, exact interval transitions, event-before-observation timing, declared run-start state and resets.
- On/off-grid and coincident independent impulses match dense X*X'/lambda covariance and W'W inverse; per-event injection within each observation interval uses A(t-e)*b.
- Compile first/mixed-second derivatives of transitions, input/output maps, gains and innovation variances. Filter jets recover actual local observed score/Hessian and full constrained ML determinant derivatives.
- Release condition/nuisance means with stable small solves and rank receipts. At C=3,F=2 largest profile solve is 5*5, regardless of N.
- Reject unsupported random pulses/multiphase correlation and observation averaging rather than invent independent shocks. JVM+JS independent oracle tests pass.
Performance: exactly seven states (ten derivative components for d=3), no trial-sized derivative solves or voxelwise covariance recursion/factorization. Meet finite-state N-scaling target; prepared coefficient memory counted per reference.

Unified runtime: emit the same ProfileJet and use common criterion/decoder/receipts, but retain seven-state fused block loops and compilation-only covariance derivatives. Instrument with PHRF-20 before full integration. Censor gaps propagate HRF states and event inputs; observation-noise policy is separate. Compact condition compression is not required and cannot be assumed sufficient for positive trial variance.

## PHRF-11: Add bounded trial amplitudes and queries to the shared readout contract

`bd-01M24MSJQZH70BTAFYTXD3MPZZ`

Owner: fit/profile; trial implementations of the common output contract introduced by PHRF-19/22.
Acceptance:

- Recover signed trial amplitudes and arithmetic means at prepared shapes against independent original penalized equations for TrialBanded and FiniteState; preserve all trial identities and rank/estimability diagnostics.
- At decoded shapes use directional first-order solve then one fixed second-order residual correction including nuisance coefficients; show local cubic error under admitted conditioning/smoothness. Count the reference solve and scoring derivative solves too.
- Certify original normal equations or include complete interpolation/tail error; eta<1 bounds and empirical/certified scope are explicit. At most one additional reference inverse application for residual certification.
- Extend signed queries directly via Q M beta + alpha (X Q')'q, without mandatory full trial-amplitude output. Verify independent direct-query parity and per-query absolute/scaled error, including cancellations. Count any remaining O(N) scratch/certification visits honestly.
- Separate forward/adjoint tests precede fused backward filters. Shared conditional readout APIs distinguish adaptive estimates from response-independent LSS TrialReadout and conditional transpose from adaptive derivative.
- JVM+JS laws for both backends, query output, nuisance recovery, streaming and continuous/readout refusal.
Performance: no N*d*V derivative fields or retained amplitude blocks; one correction maximum; no per-voxel covariance preparation. Final-shape readout occurs only after selection unless internal trial scoring mathematically requires coefficients.

## PHRF-12: Integrate one shape-aware plan and bounded condition/trial executor

`bd-01M24MSQS8HFQBPY2TAVW1FJGM`

Owners: model and fit; existing storage adapters remain outer composition.
Acceptance:

- One ProfileHrfPlan describes fixed regressors plus adaptive family/drives, ConditionMeans or ConditionCenteredTrials, criterion, output request, frozen preparation and work budgets. No fake fixed FmriModel design; retain current fixed FitPlan/strategy APIs until a deliberate sum/adapter dispatch is qualified.
- One preparation barrier and block sink lifecycle support backend-owned response encodings. Implement first with CompactCondition; trial backends attach through the same contracts later without a second executor.
- Shared geometry is separate from response-derived priors/scales/routing. Default deterministic pilot then full block pass; no mandatory K*V compact response matrix in Prepared. Optional compact spool is explicit, bounded and precision-scoped.
- Reuse axes and transactional sink/catalog work bd-01KX6G9B8R86MRBZ9S8K8F5G7V. Flush payloads, stream receipts, handle cancellation/sink failure, and preserve block-size/worker/order parity. In-memory or supplied sink qualifies core execution; durable publication requires existing catalog work.
- Output signed structural condition/trial/query axes, shape parameters/summaries, effective-response interpretation, units, uncertainty availability and separate preparation/identification/approximation receipts. Ordinary OLS t/F/df is unavailable for adaptive penalized output.
- Missingness/weights/whitening are one shared geometry in v1. Illegal combinations fail during admission. JVM+JS portable execution and actual-platform IO tests.
Performance: <=256 MiB engine goal includes compilation, banks, all worker buffers/conversions and compact-spool caches. No Vector of all finished payloads and no whole-brain trial derivative arrays. <=256 voxel blocks per worker, <=8 JVM workers.

## PHRF-13: Share bounded pilot pooling and freeze all response-derived preparation

`bd-01M24MSVV1BRRGPFRW6A27DCX9`

Owners: fit input contracts and fmri-workflow orchestration; no atlas dependency in fit.
Acceptance:

- One ShapeEvidencePool accepts common score/gradient/Hessian summaries at identical references/charts. Preserve score/count/weights and per-voxel full-ML determinant contribution. Sign reversal preserves shape evidence, while signed output remains local.
- Compact condition scatter S=sum w*z*z'/sigma^2 obeys the trace identity; it is an optional representation. Default bounded pilot can pool ten jet components instead of mandatory K*K outer products over all voxels.
- Pilot <=2048 voxels, <=2 preparation passes for C0/B0, deterministic selection and accumulation; freeze scales/alpha, regional centers/nonzero spreads and routing before full streaming. Cap groups and all retained state; account for data reads, reference search and covariance preparation.
- Distinguish voxel shape sharing, pooling prior, anatomical adjacency and spatial noise. Spatial dependence/composite weighting cannot justify independent-voxel confidence. Compare weaker/differently partitioned priors and retain prior-dominance diagnostics.
- Freeze training folds through preparation, features, query meaning and supervised label coding. Held-out target labels cannot define condition-centered amplitude shrinkage for prediction.
- JVM+JS sign, trace-vs-jet, chunk-order, pilot determinism and preparation-boundary tests; uncertainty remains conditional unless end-to-end calibration supports it.
Performance: default work scales with bounded pilot and local jets. Full-cohort scatter/pooling is optional and needs explicit compact spool and separate measured cost; no dense spatial covariance or repeated whole-brain smoothing.

## PHRF-14: Qualify numerical approximation and scientific calibration

`bd-01M24MSZ4Z6BFWRV5R4S67VDN0`

Owner: shared test/law/scenario fixtures with independent research oracles.
Acceptance:

- Numerical court covers each admitted family/backend and both criteria, independent direct design/AD/high-precision/step-sweep references, original residuals and nonlinear shape oracle.
- Frozen matrix spans null/signed signals, zero means with nonzero trial variation, sparse/dense schedules, rho=0, weak shape information, nuisance aliasing, long tails, mask/run effects, prior/lambda sensitivity and model mismatch.
- Achieve epic local decoder/amplitude targets on preregistered well-identified cohort while explicitly classifying analytical null/alias cases. Report all attempted cases and failure distribution.
- Known-truth simulations measure shape/amplitude bias and RMSE, identification false positives, held-out prediction and spatial-pooling effects. Any claimed 95% interval needs >=500 independent replicates per claimed regime, coverage assessment with binomial uncertainty, and conditional versus end-to-end scope. Otherwise report uncertainty unavailable/conditional only.
- Reuse calibration work bd-01M21VA8DYMNJS9PZ8076SBWWJ and provenance bd-01M210WJ4BWVCXEMTARHDR2AC7 without treating ordinary GLS/OLS results as ProfileHrf calibration.
- JVM+JS scenarios obey ScenarioResult policy; no caveat silently treated as Pass.
Performance: record oracle-only optimization separately; production acceptance cannot use oracle fallback or tolerance relaxation to meet speed goals.

Unified scientific court: include condition-only compact family/projector/rank admission, alpha=0 cross-backend specialization and condition/trial/query-specific accuracy. Reuse PHRF-21's condition evidence, then qualify the integrated trial engines; no duplicated independent decoder or pooling policy.
Add paired known-truth regimes holding neural organization fixed while varying HRF fields, spatial noise/mixing and prior strength/partitions; then hold vascular response fixed while varying neural temporal integration. Record effective-response versus vascular interpretation and stimulus undercoverage. Geometry/topology claims remain downstream; no dense cross-vertex covariance work is added to the voxel engine.
The supplied noise-free condition demo and local optimizer trial demos are not calibration cohorts. Feature/readout-field follow-ons have their own courts and are not hidden core prerequisites.

## PHRF-15: Measure ProfileHrf throughput, memory and backend crossovers

`bd-01M24MT58X8W2Z0KJ25JJG2SGP`

Owner: benchmark harness and receipts. Acceptance:

- Execute every hard resource contract and performance goal in the epic on frozen B0/sparse/dense/scaling fixtures, both JVM and JS scopes.
- Publish raw repeated timings, allocations/GC, peak engine memory and process RSS, hardware/runtime/provider revisions, block/concurrency/reference settings, output bytes and accuracy/admission counts.
- Compare same scientific family and criterion: fixed-shape baseline, general observed-Hessian backend and finite-state observed-Hessian backend. Include preparation, score, release, readout, certification and IO as separate totals.
- No JVM build/startup hidden in warm timings or shared preparation omitted from cold/end-to-end reports. Failed targets remain explicit; any revision records previous target, measured result and accepted scientific/resource tradeoff.
- Allocation or operation instrumentation proves zero voxelwise large factorization/covariance preparation and no hidden unbounded fallback.
Performance goals and benchmark contract (targets, not achieved results):
- B0 representative case: T=600 observations, N=300 trials, C=3 conditions, 6 full-rank fixed regressors, V=100,000 voxels, d=3, r=7 for Cascade34; Float64 computation and Float32 trial output (120 MB decimal). Stream generated/input responses and outputs. Freeze sparse and dense event schedules, lambda, noise transform, family domains, reference cells and seeds before optimization. Also run N=1,200 and V=10,000/20,000/100,000 scaling cases.
- Hard bounded-execution contract: bank <=8 prepared references per geometry; <=2 references evaluated per voxel; one second-order amplitude residual correction; at most one additional reference solve for residual certification. Count all score/readout/certification work. No per-voxel N-by-N or T-by-T factorization, no per-voxel covariance-gain preparation, no unbounded nonlinear fallback. State scoring has no trial-sized derivative solves. One shared observation geometry in v1; unsupported geometry fails explicitly.
- Engine live-memory goal <=256 MiB for B0, including prepared reference bank and all active worker buffers, with block size <=256 voxels per worker and <=8 workers. Exclude VM/runtime baseline and files on disk, not buffers or retained result payloads. Report whole-process peak RSS separately. Increasing V from 10,000 to 100,000 at fixed concurrency/block size should add <=max(32 MiB,10% of baseline engine live memory). No N*d*V derivative field, no retained V-sized collection of amplitude blocks; small scalar receipts are allowed.
- Steady-state compute scaling goal: doubling V from 10,000 to 20,000 costs <=2.3x at fixed geometry, threads and backend, with IO reported separately. Finite-state score-only time at fixed T,C,F,d,r and reference count should change <=20% when N grows 300->1,200 after preparation; report event preparation and terminal O(N) readout separately.
- Relative runtime goals: complete one-reference profile+readout <=12x the same backend's exact fixed-shape fit+trial readout; two-reference <=24x. On the frozen dense-overlap case, finite-state observed-Hessian score+readout target >=3x faster than the general observed-Hessian backend, comparing the SAME Cascade34 kernel, observation geometry, criterion and admitted approximation budget. Report sparse-case crossover; do not replace a family or relax accuracy to win.
- Absolute engineering target: B0 complete two-reference Cascade34 profile+readout+certification <=120 seconds on the declared CPU-only JVM baseline (up to 8 workers, >=32 GiB host RAM), excluding input acquisition and disk persistence; report preparation and complete IO-inclusive elapsed time separately. Record exact CPU, OS, JDK, heap, compiler/provider revisions and power/thread settings. Freeze the actual reference host in PHRF-01 before measurement. This is a proposed target, not a hardware-independent promise.
- JVM and Scala.js both run correctness and relative scaling courts; JS uses one worker and at least V=10,000, with its absolute throughput reported separately. Timing: no build/startup in steady-state compute; warm up until stable, record >=5 measured runs, median/p95, allocations/GC, preparation and IO. Cold preparation is separately reported.
- Accuracy co-gate: benchmark the complete attempted cohort, not accepted cases selected afterwards. On the preregistered local well-identified cohort target >=95% admitted, with p95 decoded-vs-nonlinear-oracle peak latency <=0.02 s and FWHM <=0.05 s, p95 relative trial amplitude L2 error <=0.1%; enforce declared original-system residual/score budgets. Undefined summaries and deliberate ambiguity cases are assessed separately, never silently removed. These are approximation-agreement targets, not claims of real-fMRI precision.
- An unmet target stays visible as failed/unmet. Revise targets only through an explicit evidence-backed decision recording old/new values and tradeoff; do not close performance qualification on asymptotic arguments or a smoke run.

Condition performance qualification: consume the early PHRF-20 harness and PHRF-21 evidence. C0 uses T=600,C=3,F=6,V=100000,d<=3,K<=96 (C<=8 capacity), <=8 bank references, <=2 evaluated references and <=256 MiB engine memory. Proposed CPU compute goal <=30 s on the same declared B0 host; post-projection routing/decoding/readout target <= initial compact-projection time. Measure K=32/64/96 only where accuracy admits, T=600/1000,C=3/8,V=10000/20000/100000. Rank caps cannot force inaccurate compression.
Report cold compilation/pilot separately and end-to-end; no T*C*nodes dictionary retained in production compilation, no runtime center/stencil bank scan. Condition execution has no N-sized solver/workspace. Compare a K-column projection and exact fixed-shape condition fit separately, not as equivalent baselines. Optional queries/features/readout fields cannot silently change C0/B0 workload or replace full-trial B0 output to win the original target.

## PHRF-16: Release unified ProfileHrf with compact conditions and qualified trial backends

`bd-01M24MTB363KSHN1KDRSAM2KNH`

Acceptance:

- Required work PHRF-01 through PHRF-22 closes with exact source/provider, numerical/scientific and performance evidence. PHRF-21 is the earlier condition milestone; full release additionally requires TrialBanded and exact Cascade34/FiniteState. Optional PHRF-23/24 do not block v1 or the epic.
- One public scientific model/criterion, one decoder/pooling/result policy and one sink lifecycle serve both amplitude structures. Inspect source organization for duplicated condition/trial scientific policy and for abstractions that force dense storage or virtual dispatch into inner loops.
- Runnable condition Gaussian/LWU and trial Gaussian/LWU/Cascade34 examples demonstrate signed amplitudes, shape summaries, effective-response interpretation, queries, diagnostics, streaming and units. Half-cosine states admitted subfamilies/cells or rejection explicitly.
- Inspect a real downstream consumer and exported axes; adaptive output cannot masquerade as LSS/global linear readout or OLS t/F/df. Protect trial-target labels and all training preparation boundaries. Document measured uncertainty scope and unsupported drives/geometry.
- All affected JVM+JS suites/laws/scenarios pass on exact pins. Run scalafimCompileAll and complete aggregate test coverage in bounded JVM/JS module batches per AGENTS.md; never run the memory-heavy all-JS aggregate in one long-lived sbt. Distinguish unrelated gate failures; pending is not passing.
- PHRF-15 records every performance goal as achieved or explicitly dispositioned by an evidence-backed decision; no silent change to C0/B0 targets or accuracy. Dense Python probes are research evidence only.
Performance: preserve <=2 references, shared preparation, one trial correction, no large voxelwise factors/covariance, and bounded outputs. No production-ready claim without measured complete-workload evidence.

## PHRF-17: Repair coefficient-penalty transport and qualify normalization invariance

`bd-01M25Q0ETQHAQ084S6S5MVPXD8`

Owner: hrf shared BasisTransform and focused downstream consumers/tests.
Current source finding: Diagonal divides basis values by s, multiplies coefficients by s, but transportPenalty uses S*Q*S; BasisGeometrySuite currently mirrors that rule. For a coefficient quadratic form the invariant transport is S^-T*Q*S^-1. Covariance continues to use forward congruence.
Acceptance:

- Reproduce scalar beta=Q=1,s=2 (wrong transported penalty 16 instead of 1) and a non-diagonal SPD quadratic form using reconstructed signal and objective invariance; do not merely assert the new implementation formula.
- Repair diagonal penalty transport/docs and audit actual callers; preserve covariance and linear-functional transport. Validate finite/nonzero scales for inverse-required operations, dimensions, identity/permutation and inverse round trips with typed errors.
- Couple h*=s(theta)h, a*=a/s, lambda*=s^2 lambda (alpha*=alpha/s^2); qualify derivatives when conversion varies with shape. Distinguish a fixed prior in a chosen amplitude convention from invariant coordinate conversion.
- Optional feature extension also transports Gamma*=s^2 Gamma. Cross-family same-number lambda is not equal shrinkage by default.
- Focused hrf JVM+JS and affected downstream tests; retain no scientific defaults chosen from numeric convenience.
Performance: coordinate transport occurs during preparation; this repair adds no voxelwise solver or loop.

## PHRF-18: Compile bounded observable condition families with rank and projector receipts

`bd-01M25Q0FVB7MPEQ3PQCCJR6WDZ`

Owner: fit/profile CompactCondition compiler; Gale owns generic QR/SVD/rank kernels.
Acceptance:

- Lower ConditionMeans directly from aggregated condition drives. After shared W0 and thin nuisance QR, compile A(theta)=(I-QfQf')W0 B(theta) approximately U D(theta), with orthonormal U and explicit observed-family/geometry fingerprint. No required trial dimension or trial covariance.
- Stream design/derivative candidate blocks through bounded compilation; no retained T*(C*nodes) dictionary. Cache family/domain/normalization, all temporal preparation, rank/error policy and exact source/provider identity.
- Bound K and preparation node count/workspace/time. C0 admits K<=96,C<=8; a failed approximation/rank margin returns CompilationBudgetExceeded, never silent tolerance relaxation. Test K independently of shape dimension; rank-69 supplied demo is not evidence for rank 32.
- Validate held-out family points and derivative/projector errors with singular-value/rank margins under actual whitening/nuisance geometry. Low dictionary residual energy alone does not certify the likelihood subspace. Track empirical versus uniform evidence and outside-U signal error.
- Compile D and its local jets/continuous readout representation with <=8 references; runtime 19-node stencil scoring is excluded. Keep scientific coefficient axes distinct from compact coordinates.
- Precompile small nuisance-recovery factors; retain Qf'W0 y separately when requested. Noise-scale-from-complement is only admitted with adequate df, exact/qualified family coverage and declared whitening.
- JVM+JS parity against independent direct time-domain condition QR, signs, rank loss, constraints, pulses/runs, unseen shape points and error refusals. Measure compilation with PHRF-20.
Performance: shared bounded compilation and O(T*K*V) projection; compact bank/workspaces counted within <=256 MiB, no per-voxel family convolution.

## PHRF-19: Implement compact condition scoring and continuous amplitude readout

`bd-01M25Q0GVNRDS6Q8PXN7SFC84X`

Owner: fit/profile CompactCondition runtime, using shared PHRF-08/09 contracts.
Acceptance:

- Compute compact z, residualized energy and requested small nuisance statistics once per active response block. Score <=2 references through compiled jets; no subsequent raw time-series evaluation, family convolution or runtime stencil-node scan.
- Same full energy/criterion and constrained decoder as trial paths. Read signed condition amplitudes from qualified D(thetaHat) with a bounded K*C small QR/solve; exact at admitted prepared shapes, explicit approximation at continuous shapes.
- Recover fixed/nuisance coefficients with retained Qf'W0 y and compiled factors. Preserve structural amplitude identities and units. Repeated M beta output, if explicitly requested, is labeled condition-derived, not identified trial estimates.
- Independent direct condition QR tests for energy, amplitudes, gradient/Hessian, nuisance recovery, sign invariance, boundary/rank/weak-data status and normalization. Exact alpha=0 agrees with unified reference; compare continuous predictions/residuals and summary errors over frozen cohorts.
- JVM+JS allocation-controlled block kernels and counters; emitted receipts distinguish compact, local Taylor and readout errors.
Performance: C0 <=30 s proposed compute target and <=256 MiB, no N-sized workspace/factor, no full K*V mandatory state. Post-projection routing/decoding/readout target <= the measured initial compact projection; report exact K and all small solves.

## PHRF-20: Establish early condition/trial benchmarks and strict work accounting

`bd-01M25Q0HX1P7AB9HPBRWRZPH1N`

Owner: benchmark harness/receipts; implement before performance-critical backends.
Acceptance:

- Freeze measurable C0/B0 schemas from PHRF-01 with original trial targets preserved and proposed condition <=30 s target. Include attempted/accepted counts, family/error budgets, hardware/runtime/providers, rank/bandwidth/state dimensions and query/output sizes.
- Build reusable warmup/repeated measurement, allocation/GC, peak-engine-memory and process-RSS harness with separate geometry compilation, pilot, projection/filtering, routing/jets, decoding, readout, certification and IO phases.
- Count every value-only center and stencil-node score, full jet, large factorization, covariance preparation, reference inverse application, response read/pass, output byte and retained workspace. Fixed caps are checked independently of wall time.
- Add bounded measurement checkpoints to PHRF-18/19, PHRF-07/10/11 and PHRF-21; do not wait for PHRF-15 to discover regressions. The harness may start with trusted primitive/fixture kernels, but cannot call them production HRF throughput.
- Compare canonical fixed-shape fitting, K-column projection, compact profile and trial backends with honest denominators. Rank32/64/96 and N scaling are conditional on accuracy, not alternative easier scientific models.
- JVM/JS harness portability and deterministic work-count contracts; >=5 measured runs after stable warmup, median/p95 and raw receipts. Empty/pending benchmark cells stay empty/pending.
Performance: monitor <=256 MiB across compilation and execution, <=256 voxels/worker, <=8 JVM workers and <=2 evaluated references. No build/startup folded into warm compute; no preparation/IO costs omitted from total reporting.

## PHRF-21: Qualify the first condition-only ProfileHrf milestone independently

`bd-01M25Q0JY8GA7CJRKZF2934PJ0`

Release milestone for ConditionMeans within the existing unified epic; does not require banded trial factors or finite-state completion.
Acceptance:

- Qualified Gaussian/LWU family domains, compact compiler/runtime, shared decoder/pooling/executor and signed condition/query outputs work end-to-end. Half-cosine is explicitly admitted or rejected by chart/cell.
- Independent direct time-domain reference verifies compact energy/readout, exact alpha=0 law, original-family approximation, rank/conditioning and bounded routing over the complete frozen C0 cohort. Apply the declared >=95% local admission and p95 peak/FWHM/amplitude budgets; null/ambiguous regimes retain statuses.
- Known-truth condition simulations assess shape/amplitude recovery, weak/prior-dominated cases, neural-timing confounding, spatial pooling and held-out predictions. Conditional uncertainty is labeled; claimed 95% intervals require >=500 independent replicates per regime plus binomial coverage uncertainty.
- Measure <=30 s C0 compute goal, <=256 MiB engine goal, projection/shape-stage overhead and V/rank/C/T scaling with PHRF-20. No trial-sized workspace or hidden runtime dictionary/stencil scan. Accuracy cannot be traded away to meet rank/time caps.
- Runnable typed example and real exported structural coefficient/query axes; fixed/nuisance recovery and deterministic chunk/worker output. Affected JVM+JS tests, laws and condition integration gates on exact provider pins; distinguish unrelated failures.
- This milestone is condition-only evidence. Trial calibration/performance remains open for PHRF-14/15/16 and the parent epic.
Performance: early complete-workload evidence, not microbenchmark extrapolation; every unmet target is explicit and requires recorded disposition before qualification.

## PHRF-22: Define signed condition/trial query outputs with direct compact sinks

`bd-01M25Q0M14Q0A7TZX387W9CSST`

Owner: shared fit/profile output contract and bounded sink; first implementation uses ConditionMeans.
Acceptance:

- Typed output requests distinguish condition amplitudes, trial amplitudes and scientifically named signed queries with stable axes, units, coverage and provenance. Reuse existing sink/axis concepts; queries are not HRF score-jet fingerprints or automatically novel-stimulus predictions.
- Condition queries compile directly against beta; trial-query law is Q M beta + alpha (X Q')'q and reduces exactly at alpha=0. Independent dense oracles verify the law before PHRF-11 supplies both production trial implementations.
- No mandatory full N*V trial output. Bound J, query representation/bank size and active block buffers. Include query norm/cancellation, original-system error and output precision in per-query absolute/scaled accuracy tests; amplitude-vector relative error alone is insufficient.
- Carry effective-response interpretation, shape/scale/preparation receipt, reliability availability and split/stimulus coverage. Fixed conditional readout is distinct from adaptive estimator and from adaptive Jacobian; do not expand the LSS-only method enum as a shortcut.
- Portable JVM+JS condition/query/sink parity, axis permutations, sign reversal, cancellation, refusal and sink failure. Trial backend adoption is required by PHRF-11 and full release; this issue does not wait for trial implementation.
Performance: compile selected queries once, return only requested payloads, and measure any residual trial-sized scratch/certification work honestly. Core C0/B0 benchmarks remain unchanged output workloads.

## PHRF-23: Optional: qualify frozen readout fields for matrix-free MVPA

`bd-01M25Q0N1MG005XYTYE8HBS2ZB`

Optional post-v1 extension; not a release prerequisite.
Acceptance:

- Add a field-aware mvpa-fit adapter for sum_k L_k Y diag(w_k), with reference-routing masks and up to ten second-order Taylor terms per d=3 reference. Preserve conditional forward/transpose semantics and distinguish from the adaptive fit Jacobian.
- Compare forward/transpose and dot-product laws against explicitly materialized polynomial beta tables; demonstrate local readout approximation order against exact same-family fits. Do not equate a Taylor polynomial with higher-order residual-corrected readout without evidence.
- Operators preserve compact/banded/state structure; no ten dense N*T operators or N*d*V derivative fields. Frozen shape weights, target-independent or fold-scoped amplitude coding and preparation provenance are mandatory; held-out target labels cannot define shrinkage.
- Benchmark total response rereads, queries/reuse, reference groups and storage against materialized/cached beta blocks. Choose operator mode only where measured time/memory improves at the same accuracy.
- JVM+JS mvpa-fit and actual consumer laws; prior/core readout contracts remain intact.
Performance: explicit per-query and reuse budgets; no assumption that avoiding beta output is always faster.

## PHRF-24: Optional: admit bounded stimulus-feature amplitude structure

`bd-01M25Q0P2ANA5VBKFZR7ZXNW8J`

Optional post-v1 extension; direct queries do not depend on it.
Acceptance:

- Define a=M beta+Phi w+u with M'Phi=M'u=0 and penalties lambda||u||^2+w'Gamma w, Gamma SPD. Preserve ridge covariance and release [F,XM,XPhi] with blockdiag(0,0,Gamma).
- Verify against independent augmented least-squares/contrast-basis solves and test lambda Phi'u=Gamma w; u is generally not feature-orthogonal. Document penalty-dependent allocation rather than uniquely identified stimulus/residual components.
- Center/scale/learn features only inside training folds; admit bounded feature count/rank, conditioning, memory and larger small-system cost before execution. Carry feature identity/coverage and compare held-out predictions.
- Normalize amplitudes and both penalties consistently; query readout adds Q Phi w. Penalized fitting is admitted first; any marginal-likelihood interpretation requires its own probability model and determinant terms.
- Continuous deterministic stimulus streams and correlated repeated random forcing are separately admitted drives; do not invent independent trial noise at every frame.
- JVM+JS numerical/scientific and workload-specific performance evidence. Core C0/B0 targets and defaults stay intact.
Performance: optional bounded linear dimension only; no extra nonlinear shape degrees, no per-voxel hyperparameter optimization, no unbounded feature matrices or geometry solver.

## Provenance

Specification: docs/plans/profile-hrf.md (SHA256 d0d3d44b533dd5d01022968a2dba20bb5807252026e1fb1ebdf4b69677148168). Work-item index: docs/plans/profile-hrf-work-items.md. Shared unification probe: tools/validation/profile_hrf_unification.py (SHA256 55ca9b653e911430d8d8268248e61dac1e8be2ec3ada507f71fd1d9a124d2e28); receipt: docs/plans/profile_hrf_unification_checks.json. Existing research probes remain docs/plans/profile_hrf_context_checks.py and tools/validation/profile_hrf_review.py. Supplied condition/trial/innovation/alignment artifacts were inspected and rerun; exact source hashes and limitations are in the specification. These are planning/research artifacts, not production Scala implementation, broad calibration or throughput evidence.
