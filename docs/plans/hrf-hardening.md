# HRF module hardening

Status: **proposed** — review complete, scope not yet approved.
Date: 2026-07-31.

A review of `modules/hrf` against the "tractatus of hemodynamic response", plus
the empirical findings that came out of it. Everything numeric below was
measured, not inferred; the R comparisons were run against the installed
`fmrihrf` at `~/code/fmrihrf`.

---

## 1. What the module already gets right

Worth stating plainly, because it constrains how much should change.

- **`Seconds` / `NonNegativeSeconds` / `PositiveSeconds`** are opaque types with
  smart constructors and a `TimeError` ADT. The refinement lattice is real.
- **`HrfDescriptor`** (`HrfFamily` / `HrfParams` / `DerivativePolicy` /
  `PenaltyPolicy` / `components`) is a symbolic provenance layer that the R
  packages do not have. It is what lets `Deriv` dispatch to analytic SPMG
  derivatives and `Penalty` dispatch per family. This is the module's best idea.
- **Error ADTs with `def message`** throughout (`HrfSpecError`, `RegressorError`,
  `SampledProfileError`, `SamplingFrameError`, `BasisCountError`).
- **Two-tier constructors** (`validated` returning `Either`, `apply` throwing)
  match the house contract in `AGENTS.md`.
- **`StrictlyIncreasingTimes` / `WeightedProfile` / `SampledCurve`** — validated
  records that keep instances always-valid.
- **The §10.5 module boundary already exists**: `modules/design` owns conditions,
  factors, contrasts, formulas, baselines; `hrf` owns kernels and rendering.
- **The `Regressor` convolution path is a faithful port of R**, warts included
  (see §3.3). Conv peak on a 3-onset SPMG1 design: scalafim `1.73179`,
  R `1.73179`.

## 2. Blast radius (measured)

This determines what is cheap to change and what is not.

| Surface | External consumers | Cost to change |
|---|---|---|
| `Hrf` as `(Seconds => Vec)` | **1 test line** (`design/.../HrfGeneratorsSuite.scala:26`). Nobody outside `hrf` implements `Hrf`. | Nearly free |
| `Regressor` / `HrfAssignment` | **1 file** (`design/.../event/EventTerm.scala`) | Cheap |
| `Evaluate`, `Penalty`, `Deriv`, `Reconstruction`, `Toeplitz`, `Registry`, `LwuBasis`, `RegressorSet`, `Design`, `compat.RNames`, `HrfSpec`, `HrfDescriptor`, `ScalarHrf`, `BasisCount` | **zero** | Free |
| `linalg.Mat` | ~60 files, 65 `Mat.unsafe` sites, public `.data` field, re-exposed in `design`/`model`/`fit`/`group` **public APIs** | **Expensive — do not touch in this pass** |
| `SamplingFrame` + `Seconds` | `dataset`, `dataset-zarr`, `fit`, `interop` import *nothing else* from `hrf` | Splittable later |

The practical consequence: **the kernel algebra can be rebuilt almost freely;
`Mat` must stay exactly where it is.**

## 3. Findings

### 3.1 Duration quadrature diverges from R (live parity bug)

R's `evaluate.HRF` and `block_hrf` integrate the box with **trapezoid weights**
(`.block_offsets_weights` in `fmrihrf/R/utils-internal.R`), so the result
converges as `precision → 0`. scalafim sums the microtime samples **unweighted**,
so the result scales as `1/precision` and diverges.

`gen_hrf(HRF_SPMG1, width = 4, precision = p)` vs `Hrfs.SPMG1.block(4.0, p)`:

| precision | R | scalafim |
|---|---|---|
| 1.00 | 6.06364 | 7.20558 |
| 0.50 | 6.12922 | 13.4004 |
| 0.10 | 6.15001 | 62.6421 |

Same for the `Evaluate` path (`evaluate(HRF_SPMG1, g, duration=4, precision=p)`):
R converges to `5.935`; scalafim gives `7.11 / 13.08 / 19.02 / 60.58` for
`p = 1.0 / 0.5 / 0.33 / 0.05`.

This reaches the design module: `design/.../hrf/HrfGenerators.scala` calls
`base.block(width, precision, halfLife, summate, normalize)`.

`summate = false` is also wrong: R divides by `sum(weights)` (= duration), giving
the **duration-averaged** box — exactly the tractatus §4.4 `BoxMass`. scalafim
takes a **max** over offsets, and silently ignores the flag entirely when
`nbasis > 1` (`Evaluate.scala:39`, `Decorators.scala:90`).

### 3.2 Several stock kernels are not causal

`maxabs h(t)` at negative lag:

| kernel | t=-10 | t=-5 | t=-1 | t=-0.1 |
|---|---|---|---|---|
| spmg1, gamma, fir, bspline | 0 | 0 | 0 | 0 |
| gaussian | 2.5e-15 | 5.4e-08 | 4.4e-04 | 1.9e-03 |
| invLogit | 1.1e-07 | 1.7e-05 | 9.1e-04 | 2.2e-03 |
| mexhat | 4.0e-13 | 4.0e-06 | 1.2e-02 | 4.0e-02 |
| lwu | 3.6e-07 | 5.5e-05 | 1.6e-02 | 4.4e-02 |
| **fourier** | **1.000** | **0.966** | **0.966** | **1.000** |
| **sine** | **1.000** | **0.966** | **0.966** | **0.131** |
| **daguerre** | **31.85** | **7.998** | **1.735** | **1.064** |

`Regressor` masks negative lags itself, so ordinary design matrices are safe.
But `HrfCombinators.block` samples `hrf(t - offset)` unguarded, so blocking a
non-causal basis reads pre-onset values:

```
fourier(0)              = 0.0000, 1.0000, 0.0000, 1.0000
fourier.block(4.0)(0)   = -4.2473, 7.3565, -6.1298, 3.5391
```

Causality is currently a property each of ~17 constructors is trusted to
implement, and 7 of them don't. §13.2 puts it in one wrapper.

### 3.3 `EvalMethod` is three different answers, and the tests can't see it

On a 3-onset SPMG1 regressor with non-grid-aligned onsets (10, 23.5, 44.2),
peak 1.73:

```
conv-vs-fft  = 4.5e-14      (same algorithm)
conv-vs-loop = 0.210628     (12% of peak)
```

**R has the identical discrepancy — `0.210628` to six digits.** So this is
inherited, not introduced; `Conv` bins onsets to the microtime grid and `Loop`
evaluates exactly. It is a plan/tolerance question (§14.6), not a bug to silently
"fix" — fixing it would break R parity.

The reason no test catches it: the only cross-method equality test
(`HrfTypedRefactorSuite`, "per-event regressor evaluation dispatch is
exhaustive across methods") uses the **per-event** path, which dispatches all
three methods to `evalLoop`. It asserts bit-exact equality and passes vacuously.

### 3.4 `span` is advisory in one path and enforced in another

`Evaluate` does not truncate at `span`; `Regressor` does. For a
`gaussian(mean=6, sd=2, span=10)` at onset 0:

```
grid      0.0      5.0      10.0     12.0     20.0
Evaluate  0.00222  0.17603  0.02700  0.00222  0.00000
Loop      0.00222  0.17603  0.02700  0.00000  0.00000
```

R also does not truncate at the HRF level, so **enforcing a horizon inside
`at()` would deviate from R.** It belongs in a `CompactSupport` capability used
by the compilation plan, not in the evaluator.

### 3.5 Basis-space bookkeeping is untyped

- `withCoefficients(coeffs: Array[Double])` checks only `length == nbasis`.
  SPMG3 coefficients apply cleanly to a 3-column B-spline basis. §7.12, §13.5.
- `normalize()` rescales the basis and **returns no transform**, so coefficients
  and contrasts cannot be transported. §8.33.
- `normalize()` preserves `PenaltyPolicy.Roughness` unchanged across the
  rescaling — verified. Per §8.43 that is now a different model.
- `bspline(nBasis = 2, degree = 3)` silently returns **4** columns;
  `tent(nBasis = 1)` returns 2. Pinned by `BsplineParitySuite` as deliberate R
  parity, but silent (§8.32).

### 3.6 `Evaluate(normalize = true)` normalizes against the query grid

Same HRF, two grids:

```
grid1 = 0..29          value at t=2 -> 0.20568
grid2 = 0..29 minus [3,12]   value at t=2 -> 1.00000
```

Asking for different time points changes the scaling of the answer.

### 3.7 Smaller items

- `Design.regressorDesign` builds one regressor over the concatenated run axis,
  so an HRF tail crosses run boundaries. An event at t=98 s in a 100 s run puts a
  **peak of 1.75 into run 2** while leaving 0.04 in run 1. It has **zero
  consumers** — the real path (`EventTerm.scala:148`) correctly loops blocks with
  a per-block grid. Fix or delete; do not leave it as a public footgun.
- `Regressor` silently drops zero-amplitude events (3 in → 2 retained), which
  breaks per-event index correspondence for LSA/LSS.
- `Hrf.params: Map[String, Any]` and `HrfParams.Legacy` survive alongside the
  typed `HrfParams`. Zero external consumers.
- `Deriv.numeric` takes a central difference straddling `t = 0` for causal
  kernels.
- `package object hrf` exports `Mat`/`Vec` (creating the ambiguity surface the
  blast-radius table describes) but **not** the `Regressor.evaluate` extension,
  so `reg.evaluate(...)` fails from a `scalafim.fmri.hrf.*` import.
- `Hrfs.weighted` throws `IllegalArgumentException` with no `Either` twin.
- `LwuSuite` pins `LwuNormalize.Area` as a **no-op** — a test asserting a bug.
- No ScalaCheck / property / law infrastructure anywhere in the repo
  (`munit` 1.2.1 only). `modules/response-laws` is a runtime conformance
  validator, not algebraic law testing.
- The canonical HRF shapes are **not pinned to R at all**. `HrfSuite` only bounds
  the SPMG1 peak to `[4.0, 7.0]`. The single numeric R fixture in the module is
  `BsplineParitySuite`.

## 4. Verdict on the tractatus

Not all of it earns its keep here. Sorting by value:

**Adopt — these fix measured defects:**
- §2 lag vs. absolute time as distinct types. `Regressor` already computes
  `Seconds(g.value - onset.value)` and feeds it to `hrf(...)`; nothing in the
  types says that argument is a displacement.
- §3/§13.2 one causality wrapper (fixes 3.2).
- §4.4 `BoxHeight` / `BoxMass` as distinct constructors (fixes 3.1's
  `summate` overload).
- §6.2/§6.3 integration as a `Primitive` capability with exact primitives where
  available (fixes 3.1's divergence and removes `precision` from the semantics).
- §14 laws + §14.6 plan equivalence (fixes 3.3's blind spot).
- §7/§8 tagged basis coordinates and `BasisTransform` (fixes 3.5).

**Adopt in weakened form:**
- §13.1 `RealModule[V]` — the module is `Double`-only and `Vec`-valued
  throughout; a full module abstraction buys little. A concrete `Vec` algebra is
  enough.
- §11 observation functionals — `SamplingFrame` already carries `blockLens`,
  `tr`, `startTime`. Generalizing to arbitrary `O_n` is speculative; keep point
  sampling and leave the seam.

**Decline:**
- §3.2 the full `Hom(A,B)` kernel category. §3.5 says so itself.
- §12.3 Volterra / `NonlinearResponseOperator` — no consumer, no fixture.
- §16's five-way module split. `hrf` is 3 200 lines; splitting it into
  core/bases/numeric/gale/laws would be more boundary than content. One `hrf`
  plus one `hrf-laws` (mirroring the existing `response` / `response-laws` pair)
  is the right granularity.

## 5. Proposed phases

Each phase must compile and pass on **both** JVM and JS before the next starts.

### Phase 0 — Safety net (prerequisite for everything else)
There is currently no randomized or golden-corpus backstop, and several existing
tests use exact `assertEquals` on `Double`, including bit-exact equality across
an FFT path. Before changing any numerics:

1. An R fixture generator (`tools/r-parity/generate_fmrihrf_fixtures.R`) +
   generated Scala fixtures, mirroring the existing
   `design/.../fixtures/RParityFixtures.scala` pattern.
2. Pin SPMG1/2/3, gamma, gaussian, mexhat, invLogit, lwu, fourier, sine,
   daguerre, fir, tent, bspline curve values to R.
3. Pin the `Regressor` conv path to R (it already matches — lock it in).
4. Add `munit-scalacheck`.
5. Convert the audit probes into a characterization suite that documents each
   known divergence with the R value in the assertion message.

### Phase 1 — Lag, causality, support
- `opaque type Lag` distinct from `Seconds`; `Hrf.at(lag: Lag)`.
- Single causality wrapper; `evaluateNonNegative` for implementors.
- `CompactSupport` capability carrying `horizon`, consumed by the plan, **not**
  applied inside `at` (preserves R parity per 3.4).
- Decision required — see §6.

### Phase 2 — Pulse and primitive
- `enum Pulse: Impulse | BoxHeight(Span) | BoxMass(Span)`.
- `Primitive[K]` capability: exact incomplete-gamma for gamma/SPMG, exact
  piecewise-polynomial for FIR/tent/bspline, `Primitive.byQuadrature` (trapezoid,
  matching R) otherwise.
- Retain an explicitly named `Quadrature.LegacyUnweightedSum` so
  `Regressor.evaluate` keeps bit-parity with R's `regressor()`. Name the wart.
- Fixes 3.1. `precision` becomes a tolerance, not a scale factor.

### Phase 3 — Basis geometry
- `BasisCoefficients[S]` tagged by a path-dependent basis space.
- `BasisTransform` returned by `normalize` / rescaling ops; penalty transported
  or invalidated.
- Grid-independent normalization (normalize against the kernel's own support).
- Make `nbasis` inflation explicit rather than silent.

### Phase 4 — Plans and laws
- `hrf-laws` module: causality, empty-drive, event additivity, kernel additivity,
  homogeneity, permutation invariance, translation equivariance, box-primitive
  law, reconstruction-commutes-with-rendering.
- `PlanEquivalenceLaws` with a **stated** tolerance, documenting the
  conv/loop onset-quantization gap of 3.3 rather than hiding it.

### Phase 5 — Boundary hygiene
- Stop exporting `Vec`/`Mat` from `package object hrf`; export the
  `Regressor.evaluate` extension.
- Fix or delete `Design.regressorDesign`.
- Drop `HrfParams.Legacy` / `Hrf.params: Map[String, Any]`.
- `Either` twin for `Hrfs.weighted`.
- Optional: split `SamplingFrame` + `Seconds` into a timing module, deleting the
  `dataset → hrf` edge.

## 6. Decisions (settled 2026-07-31)

**Scope: the full arc, Phases 0–5.**

**Parity stance: correctness wins. Where R is wrong, scalafim diverges and the
bug is filed upstream** at `github.com/bbuchsbaum/fmrihrf`. No legacy
bug-compatibility mode.

This makes the causality picture two separate things, which the follow-up
measurements separated:

- **scalafim porting bugs.** R's `hrf_fourier` masks out-of-support values
  (`basis[!in_support, ] <- 0`) and correctly returns 0 at every negative lag.
  scalafim's port **dropped that mask**, which is where the `fourier`/`sine`/
  `daguerre` values in §3.2 come from. Straight bug; fix, no deviation.
- **Inherited R bugs.** `HRF_GAUSSIAN`, `hrf_mexhat`, `hrf_inv_logit`, and
  `hrf_lwu` are non-causal in R too — scalafim reproduces R's values to every
  digit measured. Fix in scalafim, file upstream, record the deviation in the
  parity fixtures.

Likewise for duration: **both** paths get convergent quadrature. `Evaluate` and
`block` move toward R's `evaluate.HRF`; `Regressor` deliberately leaves R's
`regressor()` convention behind, since that convention is the upstream bug being
reported.

### Fixture policy

Because several R values are being deliberately left behind, the corpus records
R output as *reference data* and the suites assert:

- **exact match to R** wherever the behavior is agreed;
- **the corrected value** wherever it is not, with R's number and the upstream
  issue in the assertion message.

No test is weakened to accommodate a divergence, and no divergence is silent.

## 7. Progress

**All phases landed. Verified on both platforms: `hrf` 171, `hrf-laws` 77,
`design` 146 — 394 tests green on JVM and again on JS.**

Phases 3–5 added:

- **Phase 3.** `ResponseBasis` with a path-dependent `Space`, so coefficients
  cannot cross between same-width bases; `BasisCoefficients[S]` as the dual;
  `BasisTransform` returned by `normalizeWithTransform`, carrying
  `transportCoefficients` and `transportPenalty`. `Evaluate`'s normalization now
  scales the kernel over its own support instead of the caller's query grid.
- **Phase 4.** `modules/hrf-laws`, mirroring the `response`/`response-laws`
  pair, wired into the aggregate and both command aliases. Laws return
  `Vector[LawFailure]` rather than asserting, so any module can run them against
  its own kernels.
- **Phase 5.** `Design.regressorDesign` renders per block on run-local clocks;
  `LwuNormalize.Area` implemented; `Deriv` steps one-sided at the causal
  boundary and at a compact horizon; `Hrfs.weightedValidated` added; the
  `Regressor` extension methods are exported from the package object.

Two findings came out of writing the laws themselves:

- `quadratureConvergence` cannot be a fixed tolerance. The trapezoid rule is
  `O(h²)` for a smooth kernel but only `O(h)` for one with a jump — FIR bins, a
  boxcar edge, a Fourier basis at its horizon — so a single bound is either
  vacuous or spuriously red. The law is stated as *refinement*: each successive
  halving must move the answer less than the previous one did.
- `planEquivalence` is stated with an explicit tolerance and a companion test
  showing the conv/loop gap shrinks as precision refines, rather than an
  equality that would have to be fudged.

Deliberately not done, and why:

- **Removing the `Vec`/`Mat` re-export from `package object hrf`.** It would
  require explicit imports across ~12 files in `design`/`model`/`fit`/`group`
  for no behavioural gain, and `Mat` stays the boundary type this pass.

## 8. Follow-up pass (2026-07-31)

The four remaining items were filed as beads and are now closed. Verified on
both platforms: `hrf` 202, `hrf-laws` 77, `design` 146 — 425 tests green on JVM
and again on JS.

### 8.1 Zero-amplitude events no longer vanish (was p1)

`Regressor.fromEvents` filtered `amplitude == 0.0`, so three events in became
two out and every subsequent index shifted — precisely what LSA/LSS cannot
tolerate, since they address trials by position. The filter is gone; a
zero-amplitude event now occupies its index and contributes nothing, and
`evalLoop` skips it so the saved convolution is still saved.

Looking for tests of that invariant turned up a **second, live instance of the
same bug**: `Regressor.evaluate` windows events to `[gridStart - span, gridEnd]`
via `keepIdx`, then `evalLoop` looked up per-event HRFs by the *compacted*
index. Any windowed-out onset re-bound every surviving event to the wrong
kernel. Reproduced at full amplitude (max diff 1.0) before the fix; the original
event indices are now threaded through explicitly.

### 8.2 `Lag` is a type (was p2)

`opaque type Lag`, distinct from `Seconds`. `Hrf.apply`/`at`/`eval`/`evalScalar`,
every shape function in `HrfFunctions`, `Evaluate`, `Deriv`, `LwuBasis`,
`Toeplitz`, `PulseResponse` and `Regressor` now speak it.

`Seconds - Seconds` deliberately still yields `Seconds`, contrary to the
original sketch: `Seconds` also names widths and horizons (`span`, `duration`,
`precision`), and an algebra where `a - b` is a `Lag` but `a + b` is a width is
incoherent — it would also have rippled outside the module. Instead the
conversion is confined to one named boundary, `Lag.between(onset, at)`, which is
called in exactly one place: `Regressor.evalLoop`. `Hrfs.SPMG1(onset)` no longer
compiles, and a compile-time test pins that.

Blast radius was as predicted: one call site outside `modules/hrf`
(`design/.../HrfGeneratorsSuite.scala:26`).

### 8.3 Exact primitives (was p2)

Measured payoff on the epoch path (`Evaluate` over a 600-point grid, µs/op):

| width | precision | Exact | Trapezoid | speedup |
|---|---|---|---|---|
| 2 s | 0.33 | 320 | 570 | 1.8× |
| 2 s | 0.05 | 322 | 2459 | 7.6× |
| 12 s | 0.33 | 311 | 2295 | 7.4× |
| 12 s | 0.05 | 313 | 14270 | **45.6×** |

The exact column is flat at ~315 µs across every width and precision, which is
the point: the trapezoid costs `O(width / precision)` kernel evaluations per
sample, the primitive costs two antiderivative evaluations.


`IntegrationPolicy` on `HrfDescriptor`, dispatched like `DerivativePolicy` and
`PenaltyPolicy`, with `Primitive.definiteIntegral` computing
`∫_{l-d}^{l} h = H(l) - H(l-d)`:

- **gamma** — regularized lower incomplete gamma;
- **Gaussian** — normal CDF (`erf` via `P(½, x²)`, so one series serves both);
- **SPMG1** — `A₁γ(P₁+1, x) - Cγ(P₂+1, x)`;
- **SPMG temporal / dispersion derivatives** — their primitives are the
  canonical kernel and the temporal derivative respectively, since the code's
  `spmg1Deriv` and `spmg1SecondDeriv` really are successive derivatives;
- **SPMG2/SPMG3** — stacked from the components;
- **boxcar, FIR, tent, B-spline** — piecewise polynomial, integrated with
  Gauss–Legendre of order `⌈(p+1)/2⌉` on each piece, which is exact.

Everything else keeps the trapezoid. `HrfDescriptor.derived` **clears** the
policy, so a lagged, blocked or normalized kernel cannot inherit a primitive
that no longer describes its shape — tested directly.

This removes the `O(h)` floor on the discontinuous kernels and makes `precision`
irrelevant on those paths. `block` with an infinite half-life now routes through
the same primitive; a finite half-life stays on quadrature, because
`∫h(t-u)2^{-u/T}du` is not a box response.

**The R parity anchor is preserved, not traded away.** Integration is an
explicit `Integration.Exact | Trapezoid` choice. `Exact` is the default;
`Trapezoid` reproduces R's `.block_offsets_weights` and the duration corpus is
still asserted against it bit-for-bit at 1e-9 across all four precisions. A new
test asserts the complementary fact — that R's sequence
`6.06364 → 6.12922 → 6.15001` is converging on the closed form.

### 8.4 Convolution paths measured

`benchmarks/hrf-jvm` (JMH, 1 fork, 3×1 s warmup, 5×1 s measurement) covers
`Conv`/`FFT`/`Loop` across scan count, basis count and precision, plus
exact-vs-trapezoid on the epoch path. Scan counts are TR = 2 s runs; events
every 12 s at a non-grid offset.

**`Conv` wins in every configuration measured** (µs/op):

| nScans | nbasis | precision | Conv | FFT | Loop |
|---|---|---|---|---|---|
| 300 | 1 | 0.33 | **42** | 148 | 168 |
| 300 | 3 | 0.1 | **217** | 1746 | 429 |
| 1200 | 1 | 0.1 | **294** | 2953 | 1389 |
| 1200 | 3 | 0.1 | **550** | 8464 | 2472 |
| 4800 | 1 | 0.33 | **2925** | 5628 | 19278 |
| 4800 | 3 | 0.1 | **3530** | 40959 | 23550 |

So the intuition that "FFT should win for long runs" is **wrong here**, and the
reason is structural rather than a constant factor: `evalConv` skips zero
entries of the neural drive, and an impulse design's drive is overwhelmingly
zero — a few hundred non-zero bins out of ~96 000. The direct path is therefore
effectively `O(events × kernel)` while the FFT is `O(N log N)` on the whole grid
no matter how empty it is.

`DenseDriveBenchmark` confirms the mechanism by filling the drive in: the gap
closes monotonically with duty cycle but does not reverse.

| nScans | duty cycle | Conv | FFT |
|---|---|---|---|
| 1200 | 0 | **253** | 2918 |
| 1200 | 0.25 | **688** | 2909 |
| 1200 | 0.9 | **1821** | 3977 |
| 4800 | 0.9 | **8459** | 14551 |

**Conclusion: `Conv` stays the default and no automatic selector is warranted.**
A crossover that does not occur in any realistic fMRI regime is not worth a
heuristic. `Loop` remains the choice when exactness at non-grid onsets matters,
and is already forced for per-event assignments.

Two changes came out of the measurement:

- **`Fft` rewritten on split primitive arrays — a real 2.1–3.5× speedup.**
  `Complex` is a `final case class`, so `Array[Complex]` was an array of
  *references*: every butterfly allocated three objects and every access was a
  pointer chase. That is what made an `O(n log n)` algorithm lose to an
  `O(n·m)` loop by up to 35×. It also violated this repo's own performance
  discipline. The transform now runs in place on `(re, im): Array[Double]` with
  twiddles by recurrence, and `FftSuite` pins it against direct convolution.
  `Complex` is now unused.
- **Removing the `Conv`/`FFT` per-column copies made no measurable difference**
  (0.93–1.12×, i.e. noise). `hrfFineColumns` is now column-major so no
  per-column array is allocated, and the `.take(nFine)` copies are gone; the
  code is simpler and allocates less, but it is not faster and is not claimed
  to be.

`RegressorMethodAccuracySuite` pins the accuracy side, and corrects a claim
worth recording: the `Conv`/`Loop` gap is **not** a convergence error and is not
monotone in `precision`. It is the residual from snapping onsets to the
microtime grid, so it disappears whenever the onsets are representable at that
step and returns when they are not:

| precision | 1.0 | 0.5 | 0.33 | 0.25 | 0.1 | 0.05 | 0.01 |
|---|---|---|---|---|---|---|---|
| Conv vs Loop | 0.340 | 0.101 | 0.211 | 0.101 | 0 | 0 | 0 |

(onsets 10, 23.5, 44.2; 23.5 and 44.2 are exact multiples of 0.1/0.05/0.01 but
not of 0.33/0.25.) The envelope over onset phase *is* `O(dt)` and does shrink
monotonically. `Loop` is exact for impulse designs at any precision.

### Phase 0–2 detail

**157 tests green on JVM *and* JS at that point; the `design` module's 146
tests unaffected.**

- **Phase 0.** `tools/r-parity/generate_fmrihrf_r_parity_fixtures.R` +
  generated `HrfRParityFixtures.scala` (15 kernels over `seq(-2, 32, 0.5)`,
  9 duration cases, 5 regressor cases). `munit-scalacheck` added.
  New suites: `HrfRParitySuite`, `HrfCausalitySuite`, `DurationQuadratureSuite`.
- **Phase 1a.** `Support` enum; causality and support enforced once in
  `Hrf.apply` with implementors supplying `evaluateInSupport`; support declared
  per kernel and propagated through `lag`/`block`/`normalize`/`bindBasis`/
  `withCoefficients`/`gen`.
- **Phase 2.** `Pulse` (`Impulse`/`BoxHeight`/`BoxMass`), `Quadrature`,
  `PulseResponse`; the neural drive rebuilt as a *measure* (mass per microtime
  bin) so impulse and box are one computation.

The corpus paid for itself immediately, catching four divergences that no
existing test could see. Their resolution, in every case decided by which side
is actually correct rather than by which is R:

| divergence | verdict |
|---|---|
| `fourier`, `sine` non-zero at negative lag **and past span** | scalafim bug — the port dropped R's `in_support` mask. **Fixed.** |
| duration amplitude scaling as `1/precision` | scalafim bug in the `Evaluate`/`block` path; also an R bug in `regressor()`. **Fixed; now matches R's `evaluate.HRF` exactly.** |
| `daguerre` values differ | **R bug** — R normalizes against the caller's `t` grid. scalafim keeps its grid-independent construction. |
| `bspline` regressor differs by 1% | **R bug** — `HRF_BSPLINE` derives interior knots from quantiles of the supplied time vector, so it is not a function of `t`. scalafim keeps fixed knots. |

The last two are now pinned by positive grid-independence tests rather than by
R's numbers, so the property that justifies each divergence is itself under
test.

Deferred within these phases, both non-blocking — **both landed in §8**:
- the opaque `Lag` type distinct from `Seconds` (the type-level half of §2 —
  the *runtime* half, causality, is done);
- exact analytic primitives (incomplete gamma; piecewise polynomial) as an
  accuracy and speed optimization over the trapezoid rule, which already
  converges.

### Upstream issues to file

1. `evaluate.HRF` integrates the box with trapezoid weights (convergent), while
   `evaluate.Reg` / `regressor()` sums unweighted (divergent, ~`1/precision`).
   Identical `duration`/`precision` arguments give amplitudes differing by an
   order of magnitude depending on which object is evaluated.
2. `hrf_fourier` / `hrf_sine` error on a scalar time input —
   `basis[!in_support, ] <- 0` fails with "incorrect number of subscripts on
   matrix" when `basis` is a vector.
3. `daguerre_basis` rescales each column by the maximum over the caller's `t`
   vector, so the kernel's values depend on the query grid.
4. `HRF_BSPLINE` takes its interior knots from quantiles of the supplied time
   vector (`splines::bs` with no `knots =`), so it is not a function of `t`;
   `hrf_bspline()` and `hrf_bspline_generator()` disagree, and R's own
   `method = "loop"` and `method = "conv"` disagree for this basis.
5. `HRF_GAUSSIAN`, `hrf_mexhat`, `hrf_inv_logit`, `hrf_lwu` return nonzero
   response at negative lag, while `SPMG1`, `gamma`, `fir`, `bspline`, and
   `fourier` are causal. Inconsistent, and unphysical for a hemodynamic kernel.

Drafted in full (with reproductions) at
`<scratchpad>/fmrihrf-issues.md`; not yet filed — see below.
