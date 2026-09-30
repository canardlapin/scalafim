# Group bootstrap timing probe (2026-09-30)

Mote: `bd-01M21BNZR9ZBRAYY9JD5WCQ8KX`. Scope: the mandatory timing probe of research declaration v2 §8 only.
**No pilot was run, no bootstrap was implemented, and no calibration claim is made.** The probe times the existing
native Paule-Mandel (PM) τ² estimate plus modified Knapp-Hartung (mKH) studentized group fit; it replaces the
declaration's unmeasured assumption of 15 µs per fit.

## Machine and toolchain

- `sysctl hw.model hw.ncpu`: `Mac15,11`, 14 (Apple M3 Max). OpenJDK 22 (build 22+36-2370); Node v26.7.0.
- Scala 3.7.4, sbt 1.11.7, via `run-sbt.py` (`sbt --batch -J-Xmx3g -J-XX:ActiveProcessorCount=4`).
- Tests are not forked (`Test / fork := false`), so the JVM figures come from the sbt JVM itself, with its
  compiler and GC state. All figures are single-threaded.
- Base commit `9504d696`; worktree `/private/tmp/scalafim-bootstrap-probe-20260930`.

## Method

Probe: `modules/group/shared/src/test/scala/scalafim/fmri/group/GroupBootstrapTimingProbe.scala`, with platform
gates `GroupBootstrapTimingProbeGate` (JVM: `-Dscalafim.group.timingProbe=true`; JS:
`SCALAFIM_GROUP_TIMING_PROBE=true` in the environment that starts sbt). Without the switch the single test is
reported as skipped (munit `assume`), so the default `groupJVM/test` and `groupJS/test` never run it.

- **Cells (24):** n ∈ {8, 20, 80} × design {I: intercept only; G: intercept + quarter-group indicator + smooth
  covariate z_i = (i-.5)/n - .5} × variances {flat σ² = .52; spread σ_i² = .04·25^((i-1)/(n-1))} × τ² ∈ {0, .2},
  as in declaration §5. Contrast: intercept (I) or group coefficient (G).
- **Data:** model J under the null, y_i = u_i + e_i, v_i = σ_i² χ²_8/8 (ν = 8), a pool of 512 pre-drawn
  replicates per cell (outside the timed region), cycled. The fraction of pool replicates with τ̂² = 0 is reported
  because boundary fits skip the PM iteration.
- **Paths timed per cell:**
  - `public-1col`: everything one replicate costs through the public API: wrap y and v as n×1 matrices,
    `GroupData.withVariances`, `GroupModel.mixedEffects(PM, mKH)`, `GroupEngine.fit` (this re-runs the design's
    pivoted-QR preparation every call), `GroupFit.term(...)` (statistic and t(n-p) p-value).
  - `prepared-1col`: the package-internal floor: design prepared once, then `GroupGlm.meta` with PM + mKH per
    replicate and T = β̂/SE. This is what a bootstrap loop inside `modules/group` could call.
  - `public-499col`: one public fit of an n×499 response (one column per bootstrap replicate, B = 499), which
    prepares the design once per call; reported per column.
  - `draws-only`: the B-plug per-replicate draw work without a fit: n Gaussian u*, n Gaussian e*, n χ²_8 v*
    (8 squared Gaussians each), with `scala.util.Random`.
- **Timing:** `System.nanoTime` around batches. JVM: 2000 warm-up fits, then 20000 timed fits per cell and path
  in 400 batches of 50 (public-499col: 5 warm-up calls, then 41 timed calls of 499 columns = 20459 fits). Median
  and p90 are over per-batch mean ns/fit. JS (rough): 1000 warm-up, 5000 timed (100 batches; 11 calls for
  public-499col), Scala.js fastLink output under Node. The JVM sweep was run twice to show run-to-run spread.

## Per-cell results (µs per fit; JVM run 1 median / p90, run 2 medians, JS medians)

| cell | τ̂²=0 frac | public-1col med / p90 | prepared-1col med / p90 | public-499col med / p90 | draws-only med / p90 | rep 2 med (pub-1 / prep / 499) | JS med (pub-1 / prep / 499 / draws) |
|---|---|---|---|---|---|---|---|
| n8-DI-Vflat-T0 | 0.426 | 19.6 / 26.5 | 1.9 / 2.1 | 1.7 / 1.9 | 2.0 / 2.7 | 27.4 / 2.1 / 2.5 | 20.5 / 3.8 / 4.8 / 2.7 |
| n8-DI-Vflat-T2 | 0.266 | 13.2 / 15.0 | 1.7 / 1.8 | 2.3 / 2.3 | 1.9 / 1.9 | 19.7 / 1.7 / 2.4 | 21.5 / 4.3 / 5.3 / 2.7 |
| n8-DI-Vspread-T0 | 0.422 | 5.5 / 11.8 | 1.5 / 1.6 | 1.2 / 1.3 | 1.9 / 1.9 | 11.7 / 1.5 / 1.3 | 18.3 / 3.5 / 4.3 / 2.7 |
| n8-DI-Vspread-T2 | 0.092 | 4.3 / 4.7 | 1.8 / 1.9 | 1.8 / 1.9 | 1.9 / 1.9 | 9.3 / 2.1 / 2.0 | 19.1 / 5.2 / 6.0 / 2.8 |
| n8-DG-Vflat-T0 | 0.478 | 8.1 / 9.8 | 2.0 / 2.1 | 2.0 / 2.0 | 1.9 / 1.9 | 14.5 / 2.9 / 2.7 | 27.9 / 8.7 / 8.9 / 2.7 |
| n8-DG-Vflat-T2 | 0.363 | 6.6 / 7.0 | 2.3 / 2.5 | 2.4 / 2.4 | 1.9 / 1.9 | 9.9 / 2.7 / 2.5 | 28.4 / 10.1 / 10.2 / 2.7 |
| n8-DG-Vspread-T0 | 0.498 | 5.1 / 5.4 | 2.0 / 2.1 | 2.0 / 2.0 | 1.9 / 1.9 | 6.7 / 2.3 / 2.2 | 26.9 / 8.5 / 8.8 / 2.6 |
| n8-DG-Vspread-T2 | 0.162 | 6.2 / 6.5 | 3.0 / 3.2 | 3.0 / 3.1 | 1.9 / 1.9 | 6.8 / 3.0 / 3.2 | 30.8 / 13.0 / 13.3 / 2.7 |
| n20-DI-Vflat-T0 | 0.324 | 6.5 / 6.7 | 1.6 / 1.7 | 2.0 / 2.1 | 4.7 / 4.8 | 7.2 / 1.6 / 2.2 | 34.5 / 7.1 / 8.5 / 6.6 |
| n20-DI-Vflat-T2 | 0.090 | 6.9 / 7.1 | 2.1 / 2.2 | 2.3 / 2.4 | 4.7 / 4.8 | 7.1 / 2.7 / 2.6 | 37.0 / 8.8 / 10.4 / 6.6 |
| n20-DI-Vspread-T0 | 0.334 | 5.8 / 5.9 | 1.6 / 1.8 | 1.9 / 1.9 | 4.7 / 4.8 | 6.6 / 1.7 / 2.2 | 34.9 / 7.0 / 8.7 / 6.6 |
| n20-DI-Vspread-T2 | 0.012 | 6.7 / 6.8 | 2.6 / 2.8 | 3.0 / 3.1 | 4.7 / 4.8 | 7.7 / 2.7 / 3.0 | 39.2 / 10.9 / 12.6 / 6.6 |
| n20-DG-Vflat-T0 | 0.361 | 9.3 / 13.1 | 3.3 / 3.6 | 3.4 / 3.5 | 4.7 / 4.8 | 10.1 / 3.4 / 3.6 | 45.3 / 13.7 / 14.3 / 6.7 |
| n20-DG-Vflat-T2 | 0.143 | 10.1 / 10.4 | 4.2 / 4.5 | 4.5 / 4.6 | 4.7 / 4.8 | 11.7 / 4.2 / 4.6 | 49.9 / 16.7 / 17.6 / 6.7 |
| n20-DG-Vspread-T0 | 0.363 | 10.0 / 10.4 | 3.4 / 3.7 | 3.6 / 3.7 | 4.7 / 4.8 | 10.8 / 3.5 / 3.8 | 45.4 / 13.7 / 14.9 / 6.7 |
| n20-DG-Vspread-T2 | 0.012 | 12.1 / 12.4 | 5.5 / 5.7 | 5.7 / 5.8 | 4.7 / 4.8 | 13.0 / 5.6 / 5.7 | 53.5 / 21.2 / 22.5 / 6.6 |
| n80-DI-Vflat-T0 | 0.123 | 30.3 / 36.6 | 5.3 / 5.6 | 6.3 / 6.6 | 18.9 / 19.2 | 23.2 / 5.6 / 7.0 | 122.8 / 21.7 / 30.3 / 26.6 |
| n80-DI-Vflat-T2 | 0.002 | 21.8 / 23.1 | 6.6 / 7.0 | 7.5 / 8.2 | 18.9 / 19.2 | 23.8 / 6.9 / 7.8 | 125.7 / 26.0 / 35.1 / 26.5 |
| n80-DI-Vspread-T0 | 0.078 | 25.6 / 26.3 | 5.7 / 6.0 | 7.3 / 7.4 | 18.9 / 21.2 | 27.2 / 6.0 / 7.9 | 124.6 / 22.9 / 42.4 / 26.3 |
| n80-DI-Vspread-T2 | 0.000 | 23.1 / 27.9 | 7.9 / 8.0 | 9.0 / 9.2 | 18.9 / 19.2 | 29.2 / 8.4 / 9.9 | 134.4 / 32.0 / 41.2 / 26.4 |
| n80-DG-Vflat-T0 | 0.127 | 33.2 / 34.0 | 11.5 / 12.0 | 11.8 / 11.9 | 18.9 / 19.1 | 34.6 / 11.9 / 11.6 | 147.8 / 37.3 / 47.2 / 26.6 |
| n80-DG-Vflat-T2 | 0.004 | 39.9 / 40.6 | 13.9 / 14.2 | 13.9 / 14.2 | 18.8 / 19.1 | 41.8 / 14.3 / 14.4 | 157.7 / 44.9 / 54.0 / 26.5 |
| n80-DG-Vspread-T0 | 0.090 | 34.0 / 35.0 | 12.1 / 12.9 | 12.6 / 12.7 | 18.9 / 19.2 | 35.6 / 12.4 / 12.3 | 163.1 / 39.8 / 51.6 / 27.3 |
| n80-DG-Vspread-T2 | 0.000 | 42.6 / 43.9 | 16.7 / 17.0 | 16.6 / 16.9 | 18.9 / 19.1 | 40.0 / 17.3 / 17.9 | 167.3 / 55.2 / 65.9 / 27.8 |

Means over the 24 cells of the per-cell medians (µs/fit), JVM run 1 (run 2 in parentheses):

| path | all cells | n=8 | n=20 | n=80 | JS all cells |
|---|---|---|---|---|---|
| public-1col | 16.1 (18.2) | 8.6 | 8.4 | 31.3 | 69.9 |
| prepared-1col | 5.0 (5.3) | 2.0 | 3.1 | 10.0 | 18.2 |
| public-499col | 5.3 (5.6) | 2.1 | 3.3 | 10.6 | 22.5 |
| draws-only | 8.5 (8.6) | 1.9 | 4.7 | 18.9 | 12.0 |

## Findings

1. The PM + mKH fit itself costs 1.2–17 µs (JVM median), mean ≈ 5.3 µs across the declared cells when the design
   is prepared once (batched columns or the prepared kernel; the two agree within ≈ 10%). Cost grows with n and p
   and with τ² > 0 (fewer boundary fits, more PM iterations). The declaration's 15 µs is therefore about 3×
   pessimistic for the fit alone, but only for an implementation that does not re-prepare the design per draw.
2. Re-entering the public single-column API per replicate costs ≈ 3× more (mean 16–18 µs, 43 µs at n=80 G),
   because every `GroupEngine.fit` call redoes the design QR preparation and the model/data validation.
3. Draw generation is not negligible: with `scala.util.Random` and χ²_8 as a sum of 8 squared normals it costs
   ≈ 0.24 µs per subject, which exceeds the fit at n = 20 and n = 80 (19 µs at n=80). It scales with ν under this
   method (ν = 40 would be ≈ 5× more); a gamma sampler (e.g. Marsaglia-Tsang) and a non-synchronized SplitMix64
   stream, as the declaration's seeding already specifies, would remove most of it. This draw cost was not measured
   for SplitMix64 here.
4. JS (fastLink, rough) is ≈ 3.5–4× slower than the JVM for the fit and ≈ 1.4× for draws.

## Recomputed compute estimate (declaration §8 counts, single core)

Fit counts from §8, with the three candidates included: pilot null 117 × 2000 × 499 × 3 = 350.3 M; pilot power
90 × 2000 × 499 × 3 = 269.5 M; confirmation null 12 × 20000 × 999 × 3 = 719.3 M; confirmation power
4 × 20000 × 999 × 3 = 239.8 M; **bootstrap total 1578.8 M fits**. The Monte Carlo sign-flip count is not itemised in
§8; its "≤ 2.2 h at 15 µs" bound corresponds to ≤ 528 M fits, which is used here (sign-flip draws hold v fixed, so
only the fit is priced).

| per-fit unit (JVM, cell mean) | bootstrap | sign-flip | total |
|---|---|---|---|
| declared 15 µs (reference) | 6.6 h | 2.2 h | 8.8 h |
| prepared-once fit only (5.3 µs) | 2.3 h | 0.8 h | 3.1 h |
| **prepared-once fit + B-plug draws (13.8 µs)** | **6.1 h** | **0.8 h** | **6.9 h** |
| public 1-column API per draw + draws (24.6 µs) | 10.8 h | 2.4 h | 13.2 h |
| JS, prepared-once fit + draws (34.5 µs) | 15.1 h | 3.3 h | 18.4 h |

Run 2 gives 7.0 h for the bolded line. **Recommended planning figure: ≈ 7 core-hours on the JVM** (bootstrap fits
with B-plug-style draws, design prepared once per study, plus sign-flip), i.e. about 2 h wall-clock at the 4-core
cap of the shared runner, or under 1 h on 14 cores if studies are run in parallel. A per-draw public-API
implementation roughly doubles this (≈ 13 h). The declaration's 8.8 h is thus the right order of magnitude but for
the wrong reason: the fit is cheaper than assumed and the draws, not priced in §8, take up the difference.

The estimate still excludes (as §8 does) baselines, restricted null fits (once per study, ≈ 10⁶ fits, negligible),
the B-EB hyperparameter fit and posterior χ² draws (comparable to the B-plug draws), first-level AR generation for
stress cells, R parity runs and the separate JS runs.

## Caveats

- **Not the full per-study probe of §8.** §8 asks for "3 cells × 100 studies measuring the full per-study cost".
  That requires the bootstrap implementation, which is not authorized. This probe prices its two dominant parts,
  the refit and the draws, separately; their sum ignores p-value bookkeeping and allocation of draw matrices.
- **JIT and in-process measurement.** The JVM figures are in sbt's own JVM after 2000 warm-up fits per cell and
  path. The first cells and the `public-1col` path show the most run-to-run spread (e.g. n8-DI-Vflat-T0: 19.6 vs
  27.4 µs), consistent with JIT and GC interference; the prepared and batched paths agree within ≈ 10% between runs.
- **Allocation.** Every fit allocates (Gale matrices, `Either` chains, result case classes); allocation rate was
  not measured and GC pauses are inside the batch timings (visible in some p90 values).
- **Draw generation.** The bootstrap refit adds draw generation, measured here with `scala.util.Random`
  (synchronized `java.util.Random`) and a naive χ² sampler at ν = 8; the planned SplitMix64 streams and ν ∈ {∞, 40}
  change it in opposite directions.
- **ν and τ̂² boundary.** Fit cost depends on the share of boundary fits (τ̂² = 0 skips PM iterations); the pool
  uses ν = 8 only. Bootstrap draws from τ̂0² rather than from the true τ², so their boundary share differs.
- **JS** figures use fastLink (not fullLink) output and smaller counts; treat them as rough.

## Reproduction and logs

```
python3 /private/tmp/scalafim-execution-20260929/run-sbt.py <worktree> <log> -Dscalafim.group.timingProbe=true \
  "groupJVM/testOnly scalafim.fmri.group.GroupBootstrapTimingProbe"
SCALAFIM_GROUP_TIMING_PROBE=true python3 /private/tmp/scalafim-execution-20260929/run-sbt.py <worktree> <log> \
  "groupJS/testOnly scalafim.fmri.group.GroupBootstrapTimingProbe"
```

Logs under `/private/tmp/scalafim-execution-20260929/logs/`:
`bootstrap-probe-jvm-timing.log` (run 1), `bootstrap-probe-jvm-timing-rep2b.log` (run 2),
`bootstrap-probe-js-timing.log` (JS), `bootstrap-probe-default-tests-compileall.log` (default `groupJVM/test`:
76 total, 75 passed, 1 skipped; default `groupJS/test`: 75 total, 74 passed, 1 skipped; `scalafimCompileAll`
succeeded with no warnings). `bootstrap-probe-jvm-timing-rep2.log` is a superseded run that hit munit's 30 s default
timeout after printing all rows; the probe now sets its own timeout. Run 1 and the JS run predate that one-line
timeout change, which does not touch the measured code.
