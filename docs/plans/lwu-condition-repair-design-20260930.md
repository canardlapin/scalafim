# PHRF-21 LWU repair design — 2026-09-30

## Decision and scope

Proceed first with a bounded tangent-direction correction in `ShapeDecoder`. Independently design a stable paired energy comparison for the same frozen-basis objective; do not replace it with an arbitrary ULP allowance. Keep boundary refusal until constrained inference has its own qualified contract. These are three separate scientific and numerical decisions.

This child (`bd-01M3T8ZESDS379YXB0BW5H6H01`) delivers design and read-only evidence. Parent `bd-01M25Q0JY8GA7CJRKZF2934PJ0` remains unmet. No production solver, chart, budget, threshold, prior, weak-shape policy, admission or inference changed. No new random responses, fresh qualification stream, 100k job, publication or full milestone claim. The separate C0 warm child remains with its existing owner.

Checkout base: `9db335906db0c6b41253048e21c90ad6b10395ba` (prior diagnostic evidence). Production numerical source: `327eda76792bcb0e542cb9e9c6f0d1351f45b1ea`. Historical DEV execution identity: `lwu-dev-87237a3c3163ccca571ec6ef33ae53c2f63c6d1272d58035de08e5b6f226ad5a`. Artifact directory: [lwu-condition-repair-design-20260930](../verification/lwu-condition-repair-design-20260930/). Raw inputs remain in [prior diagnostics](../verification/condition-lwu-diagnostics-20260930/).

## Original family and observation protocol

The causal unnormalised LWU family is

`h(t) = exp(-((t-tau)/sigma)^2/2) - rho * exp(-((t-tau-2*sigma)/(1.6*sigma))^2/2)`.

The represented chart is `(tau, log(sigma), rho)` with bounds `[3,8]`, `[log(.8),log(3)]`, `[0,.8]`. Tau is the positive component's centre and sigma its Gaussian width; neither is automatically the observable peak latency or FWHM of the combined curve. Rho is a component coefficient, not the observed trough-to-peak ratio. Nonnegative rho is a physical model restriction; `.8` is the configured chart cap. Altering either to rescue cohort counts changes the estimand and is outside this repair.

The saved protocol has 600 observations at `.5,1.5,...,599.5`, microtime step `.1`, horizon `32`, 300 Java-Random event onsets with seed `20260910`, and A/B/C conditions after sorting. AR(1) whitening has phi `.3` and first-row multiplier `sqrt(1-phi^2)`. Nuisance columns are intercept, centred/scaled linear time, squared time minus `1/3`, and three cosines. Independent QR nuisance elimination and SVD linear regression use these same quantities. No response regeneration occurs.

The direct reference initially used an incorrect zero first acquisition. `direct-reference-v1.json` and `invalid-v1.json` retain that error and explicitly withdraw its conclusions. Valid v2/v3 use half-TR acquisition. Java and Python onset schedules match all 300 binary64 values; direct event-loop and independent event-hat controls anchor the acquisition operator. Do not reuse v1 claims.

## Results and their limits

All four historical 95/100 acceptance gates remain failed:

| Platform | SNR 1 | SNR .5 |
| --- | ---: | ---: |
| JVM | 91 | 81 |
| JS | 89 | 82 |

Across 400 historical records there are 57 refusals: 41 Boundary, 14 candidate-attempt caps, two CurvatureNotPositive. Exact rational algebra on the exported binary64 jets classifies 39 Boundary records as stationary under the existing `1e-9` free-Newton criterion, two Boundary records as nonstationary, all 14 capped terminals as nonstationary, and both curvature refusals as nonstationary. This is recorded-jet algebra, not independent objective or Hessian qualification. The helper now includes production's full-Hessian SPD requirement when there are no free coordinates; classification is unchanged.

A finite independent original-family search over 225 nodes and five starts per refused input found 42 boundary and 15 interior best solutions. All 41 original Boundary refusals still have boundary best solutions; 13 of 14 caps and both curvature refusals have interior best solutions, with one cap still on rho `.8`. All found free corrections are below `1e-7` (maximum below `3.26e-8`), a diagnostic check distinct from production's `1e-9`. Finite multistart search is not proof of global optimality or a prediction of admission. None of the saved compact terminal points meet that direct-reference criterion: compression changes the objective, so this is not an additional production solver defect by itself.

Direct-reference controls (v3): whitening discrepancy zero; independent joint-regression SSE discrepancy at most `1.82e-12`; complex-step versus Richardson objective-gradient discrepancy at most `1.35e-8`; independent event-drive discrepancy zero; acquisition event-loop discrepancy at most `4.77e-14`. These controls establish the bounded reference's protocol and local derivative consistency, not inference coverage or universal compression error bounds.

## Confirmed clipping defect and minimal correction

`ShapeDecoder.newtonDirection` uses a gradient-defined active set and tests the raw free correction norm correctly. `decode` subsequently scales the entire Newton direction to the box and replaces `Direction` with `Stationary` if the clipped norm is tiny. Coupling can make one free Newton component point outward from an occupied bound even when its gradient points inward. Common clipping then returns alpha zero and falsely certifies convergence.

Both JVM and JS saved SNR `.5`, voxel 50 demonstrate this. At rho zero, `g_rho` is about `-14.32`, the full Hessian is positive definite, and the raw Newton direction is approximately `(-.557514,-.301623,-.256455)`. The rho component makes alpha zero. Work is exactly `[157,1,0,0,0,0,0]`: no candidate is attempted. Feasible positive rho has negative derivative, so the point cannot be a constrained optimum. This false internal convergence still returns Boundary refusal; it does not prove improper Accepted admission.

The independent direct objective also decreases with rho increased by `.001` (about `.01399` SSE), and finds an SSE improvement about `5.72255` at another rho-zero point. Thus fixing the false stop does not imply this input becomes interior or admissible.

For a positive-definite free Hessian, raw direction `p=-H_FF^-1 g_F` satisfies `g^T p<0`. Remove components pointing outward on occupied bounds using the existing bound convention to obtain tangent direction `p_T`. Every removed free component has `g_i p_i>=0`, hence `g^T p_T <= g^T p < 0`. This justifies tangent projection before common scaling. It is not a proof for an indefinite Hessian or arbitrary component clipping away from an occupied bound.

Implementation contract:

1. Preserve the existing free mask, curvature check and raw free correction stationarity criterion.
2. Project outward components at occupied bounds, then use the existing bounded candidate line search.
3. Never convert a tiny projected, clipped or unrepresentable displacement into Stationary when the raw correction was nonstationary. Represent a stalled nonstationary termination explicitly in the internal outcome; preserve refusal and coherent terminal evidence. Do not falsely report unspent budget exhaustion or introduce an unbounded loop.
4. Preserve the six Newton, eight jet, two exact-evaluation and four per-iteration candidate caps, terminal gradient/Hessian consistency, recovery provenance, prior contribution and all admission gates. Count actual calls; no hidden reference calls inside production.

Discriminating exact regression: `E(x,y)=x^2+xy+y^2-x/2-2*y`, box `[0,1] x [0,4]`, four corner nodes. The unique best bank point is `(0,0)`, gradient `(-1/2,-2)`, Hessian `[[2,1],[1,2]]`, raw direction `(-1/3,7/6)`. Current clipping gives alpha zero. Tangent `(0,7/6)` has energy `-35/36`; constrained optimum `(0,1)` has energy `-1` and gradient `(1/2,0)`. Extend with independent `z^2` for a three-coordinate control. Add mirrored upper-bound, genuine KKT-boundary, interior, small-positive-alpha, unrepresentable displacement, indefinite curvature, recovery and budget controls.

## Candidate caps: distinguish three numerical targets

All 14 capped terminals have predicted local quadratic improvements below one ULP of their absolute energy (about `.00894` to `.52112` ULP). Prediction alone does not establish a true energy sign.

A read-only delegate traced seven JVM caps, the JVM curvature refusal and voxel 50. All nine reproduce historical status, coordinates and seven work counters exactly. It returns frozen objective values unchanged; design copies and logging introduce no extra objective evaluations. It is not a performance measurement and does not establish JS callback equivalence.

For each of the seven caps, the first full proposal passes the existing stationarity criterion but is rejected because its printed absolute energy increases by 1–5 ULP. The following independent calculations deliberately target different quantities:

| Calculation | Target | Finding |
| --- | --- | --- |
| Exact Fraction LS | Captured already rounded binary64 designs and z | Three proposals decrease; four slightly increase |
| Decimal 60/90 digits | Smooth LWU kernel with captured binary64 lags, Phi, Rhat, z and exact binary64 1.6; recompute projection/design/LS | All seven decrease |
| Direct original-family QR/SVD | Original observation model without compact basis | Separate compression/reference check; not a substitute production comparator |

Smooth fixed-basis decreases range from `2.66877e-15` to `1.34700e-14`. Sixty- and ninety-digit differences agree within `1e-50`; recorded versus recomputed current designs differ below `1e-12`, amplitudes below `2e-12`. Independently finite-differenced smooth candidate gradients, translated with recorded candidate Hessians, give corrections about `1.47e-16` to `2.47e-15`. The Hessians themselves are not independently qualified. The reference's rho-only mask is valid for these seven because tau/log-sigma are strictly interior; it is not a generic decoder replay.

The sign discrepancy originates before the final absolute-energy subtraction, including rounded designs. Both production jet and energy callbacks use `coefficientJetInto`; an alleged evalInto/jetInto split is not supported here. Comparing explained variance alone, compensated final summation, or exact LS on the exported designs cannot by itself restore the smooth target's sign. Neither a fixed ULP tolerance nor automatic acceptance of stationary proposals is justified.

A future numerical contract should calculate paired `Delta E` for the SAME response, whitening, nuisance elimination, frozen basis/operators, normalization, chart and prior. Remove the constant response-energy term before cancellation, stabilize kernel/coefficient/design differences, include solve and dot-product errors, and return certified Decrease / Equal / Increase / Unresolved. Unresolved preserves refusal under existing caps; it must not force all saved caps to pass. Define the mathematical target and error model before implementation. A generic numerical primitive belongs in Gale; fit may own scientific policy and adapters.

For derivation, if `r0=z-D0*beta0` and `d_fit=D1*beta1-D0*beta0`, then `Delta E=-2*r0^T*d_fit+||d_fit||^2`. A potentially more stable exact LS identity uses `deltaD=D1-D0`, `q=deltaD*beta0`, `c=deltaD^T*r0-D1^T*q` and

`Delta E = -2*r0^T*q + q^T*q - c^T*(D1^T*D1)^-1*c`.

This identity uses exact current LS orthogonality; finite solve residuals must be included. It is a candidate derivation, not validated production arithmetic. Computing deltaD by subtracting already rounded designs may already lose the relevant signal. Any prior difference must be included consistently. Qualification must include genuinely increasing stationary proposals, zero/near-zero changes, rank/conditioning cases, and independent high-precision Hessian/gradient controls; the seven decreases alone cannot validate a general comparator.

## Boundary qualification

Boundary is a constraint/inference issue separate from solver convergence and weak-shape SD limits. Retain refusal. Full inverse-Hessian SDs (`sqrt(2*diag(H^-1))`) describe unconstrained local curvature and do not establish boundary coverage. Expanding rho bounds or relabelling these inputs Weak does not resolve this.

Boundary asymptotics depend on a constrained Gaussian projection and can produce nonstandard likelihood-ratio limits; even simple mixture prescriptions require model-specific assumptions. See [Self and Liang (1987)](https://www.stat.cmu.edu/~brian/763-2015/week06/papers/self-liang-1987.pdf). Applying that theory here requires checking identifiability, noise/AR treatment, fixed compression geometry, nuisance parameters, priors and regularity. This design does not prove those assumptions.

A later policy must distinguish a qualified constrained estimate from an interior estimate, expose active constraints and uncertainty method, and calibrate intervals/tests under lower and upper rho boundaries plus near-boundary alternatives, poor identifiability and pooling/prior variants. PHRF-14 or the owning inference work must supply that evidence. Any new admission contract needs its own predeclared gates; historical constrained convergence is not permission to change the current 95/100 milestone definition.

## Implementation and qualification sequence

First implement only the reviewed tangent-direction/stationarity correction with exact quadratic regressions and honest stalled termination. Replay the saved cases and the complete unchanged DEV cohorts on JVM and JS, retaining all refusal counts and coherent counters. Preserve the Gaussian, exact-ML, prior, weak-shape and geometry regressions; do not evaluate only the failed inputs.

Then select and review the paired energy arithmetic/error contract, upstreaming generic primitives to Gale where needed. Qualify it against the seven smooth oracle pairs and discriminating genuine-increase controls on both platforms. JS callback traces, independent Hessian qualification and general error bounds remain outstanding. Keep the boundary policy separate.

Run affected decoder, compact-objective and numerical suites, HRF/law suites, first-level law cohorts and downstream workflows on both JVM and JS in bounded sbt batches. Compile changed modules warning-clean on both platforms. Freeze the new numerical source and only then use a custodian-approved predeclared fresh stream for admission evidence. The parent and full C0/100k runs stay blocked by unmet gates; bounded warm work does not establish full qualification.

## Evidence identity, reproduction and checks

The artifact directory contains final `direct-reference-v3`, `saved-kkt-v2`, `trace-analysis-v2`, `smooth-reference-v2`, Java schedule and JVM trace-v3 raw logs/JSON/exit metadata. Intermediate and invalid attempts remain distinguishable. `payload-sha256.json` binds all payloads; `review.txt` binds the independent review to the report hash and payload index hash. Source snapshots retain intermediate Python versions where required. Earlier JVM trace v1/v2 are intermediate exports; the final Java source and v3 export are the reproducible reference pair.

Direct reference uses Python 3.14.7 / NumPy 2.4.3 / SciPy 1.17.1. Fraction and Decimal analyses use Python standard library. Java onset control used Java 22; saved-input trace used Java 25.0.1 against frozen existing compiled Scala artifacts. The 54 present classpath entries contain 11,695 files, all rehashed successfully against the manifest after tracing; one absent no-main-source class directory is recorded. Compiled artifacts remain local under `/private/tmp/scalafim-lwu-repair-design-20260930/frozen-classpath`; they are not vendored into Git. The archived source identities, manifest, exact inputs and captured operators support reconstruction, but do not claim a fresh clean-build replay of those binaries. Java Unsafe runtime warnings remain in raw receipts.

From checkout root, standard-library final analyses can be rerun with fresh output filenames:

```sh
python3 docs/verification/lwu-condition-repair-design-20260930/verify_saved_kkt.py --out /tmp/lwu-kkt-new.json
python3 docs/verification/lwu-condition-repair-design-20260930/analyze_saved_trace.py docs/verification/lwu-condition-repair-design-20260930/jvm-saved-trace-v3.log docs/verification/lwu-condition-repair-design-20260930/trace-inputs/cases.json --out /tmp/lwu-trace-new.json
python3 docs/verification/lwu-condition-repair-design-20260930/smooth_compact_reference.py docs/verification/lwu-condition-repair-design-20260930/jvm-saved-trace-v3.log --out /tmp/lwu-smooth-new.json
```

The direct script exposes its CLI through `--help`; source/runtime invocation and exact exit status are retained in its log metadata. Java trace metadata retains the full command and frozen classpath. This phase ran reference calculations and checked historical JVM/JS inputs; it did not run sbt or implement a production feature. Both-platform implementation testing is an explicit next gate, not claimed here.
