# PHRF-25 spike results: kernel-basis condition fit

Date: 2026-09-10. Ticket: PHRF-25 in
[profile-hrf-work-items-v2.md](profile-hrf-work-items-v2.md). Code:
`modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/profile/spike/`
(test scope only; five files; not a public API). Run with

```sh
sbt -Dscalafim.image4s.build=<image4s checkout> \
  "firstLevelLawsJVM/testOnly scalafim.fmri.laws.profile.spike.ProfileHrfSpikeSuite" \
  "firstLevelLawsJS/testOnly scalafim.fmri.laws.profile.spike.ProfileHrfSpikeSuite"
```

The image4s override is needed only while the working tree carries
uncommitted `modules/image` edits ahead of the pin; the spike itself does not
touch image4s.

## What was built

- Causal Gaussian family in the chart `(tau, log sigma)` with analytic first
  and mixed second parameter derivatives, verified against central finite
  differences (max abs discrepancy below 1e-6 for all six jet components).
- Kernel basis `Phi` from a Gale SVD of the family and its derivatives sampled
  on a 26 x 21 shape grid over `tau in [3, 8] s`, `sigma in [0.8, 3] s`,
  fine step 0.1 s, horizon 24 s. Held-out error curve on 300 random shapes.
- One shared pass through the existing `ar` whitening (pure AR(1), phi = 0.3)
  and Gale pivoted QR: nuisance basis `qF` (6 columns), then the rank-revealing
  `U R` of the whitened, nuisance-projected expanded design
  `A_tilde = [S_1 Phi' ... S_C Phi']`, un-permuted so `D(theta) = R (I kron c(theta))`.
- Per-voxel post-solve in compact coordinates: exhaustive node scan, jet at the
  best node from a precomputed node jet bank, safeguarded Newton steps verified
  by exact compact re-evaluation, `K x C` amplitude readout, conditional
  curvature status.
- Dense time-domain oracle using the exact kernel (no basis): 101 x 21 shape
  grid, whitened and projected, then a seven-level compass search with exact
  evaluations (about 52 exact time-domain evaluations per voxel).
- Synthetic cohorts: `T = 600` at TR 1 s, `C = 3`, 100 events per condition
  at 0.1 s onset resolution, six nuisance columns, AR(1) noise, truth drawn
  from the interior of the domain, signed amplitudes in `[0.5, 2]`.

## Measurements (JVM, Apple Silicon laptop, single thread, warm JIT)

Kernel basis rank `m` by held-out relative error:

| Tolerance | value | first derivative | second derivative |
| --- | ---: | ---: | ---: |
| 1e-3 | 19 | 22 | 26 |
| 1e-4 | 24 | 26 | 31 |
| 1e-5 | 27 | 30 | 35 |

The main runs use `m = 27` (value tolerance 1e-5), giving `K = C m = 81` at
full rank. Prepared-shape agreement between the compact energy and the direct
time-domain fit at the true shape: max relative energy discrepancy 2.1e-7,
max relative amplitude discrepancy 2.2e-6.

### Decoder variants against the dense oracle

200 voxels per SNR, admitted voxels only (status `Accepted`: positive
curvature, interior, conditional sd(tau) <= 0.5 s). Gates: admitted >= 95%,
p95 latency <= 0.02 s, p95 FWHM <= 0.05 s, p95 relative amplitude L2 <= 1e-3.
"hier" scans a coarse sub-grid then the fine neighbourhood of the coarse best
(72 of 225 nodes for 15x15, 87 of 529 for 23x23). "full" refinement jets carry
the observed Hessian (six components); "frozenH" jets carry value and first
derivatives only and reuse the node's exact Hessian; "GN" uses the
Gauss-Newton curvature.

| Variant | SNR 1.0: p95 lat / FWHM / amp | SNR 0.5: p95 lat / FWHM / amp | Gates |
| --- | --- | --- | --- |
| 8x8 full x2 | 0.0043 s / 0.0081 s / 1.03e-3 | 0.0059 s / 0.0071 s / 1.32e-3 | amplitude unmet |
| 8x8 full x3 | 0.0002 / 0.0010 / 9.5e-5 | 0.0002 / 0.0011 / 1.1e-4 | met |
| 12x12 full x2 | 0.0005 / 0.0012 / 1.2e-4 | 0.0009 / 0.0016 / 1.8e-4 | met |
| 12x12 node+GN x3 | 0.0003 / 0.0019 / 1.8e-4 | 0.0026 / 0.0133 / 1.5e-3 | unmet at 0.5 |
| 12x12 frozenH x3 | 0.0038 / 0.0071 / 8.9e-4 | 0.0038 / 0.0068 / 1.0e-3 | unmet at 0.5 |
| **15x15 hier full x2** (default) | 0.0002 / 0.0011 / 9.8e-5 | 0.0002 / 0.0012 / 1.2e-4 | met |
| 15x15 hier frozenH x3 | 0.0008 / 0.0020 / 2.4e-4 | 0.0013 / 0.0032 / 4.7e-4 | met |
| 23x23 hier full x2 | 0.0002 / 0.0010 / 9.3e-5 | 0.0002 / 0.0011 / 1.1e-4 | met |
| 23x23 hier frozenH x2 | 0.0007 / 0.0013 / 1.6e-4 | 0.0015 / 0.0031 / 3.9e-4 | met |

Admission: 200/200 at SNR 1.0, 197 to 199/200 at SNR 0.5, 116 to 135/200 at
SNR 0.25 (no variant meets the admission gate at SNR 0.25; the admitted
voxels still meet the accuracy gates for the full-Newton variants). Recovery
against the generating truth (not a gate): median latency error 0.06 s at
SNR 1.0, 0.17 s at SNR 0.5, 0.24 s at SNR 0.25.

### Basis tolerance sweep (default variant)

| Basis tolerance | m | K | SNR | p95 latency | p95 rel. amplitude | ms/voxel |
| --- | ---: | ---: | --- | ---: | ---: | ---: |
| 1e-3 | 19 | 57 | 1.0 | 0.0003 s | 1.2e-4 | 0.045 |
| 1e-3 | 19 | 57 | 0.5 | 0.0005 s | 2.2e-4 | 0.047 |
| 1e-4 | 24 | 72 | 1.0 | 0.0002 s | 9.9e-5 | 0.056 |
| 1e-4 | 24 | 72 | 0.5 | 0.0002 s | 1.2e-4 | 0.057 |
| 1e-5 | 27 | 81 | 1.0 | 0.0002 s | 9.8e-5 | 0.060 |

A 1e-3 kernel tolerance still meets the 1e-3 amplitude gate with a factor
of four to spare, and cuts the post-solve by a quarter.

### Benchmark at `V = 10,000` (K = 81), per voxel

| Phase | ms/voxel |
| --- | ---: |
| Whitening through `ar` | 0.003 |
| Projection, per-voxel loop / 4-wide blocked | 0.017 / 0.016 |
| Post-solve, 15x15 hier full x2 (default) | 0.060 |
| of which node scan (72 scores) | 0.009 |
| of which jets (1 node jet from the bank + 1 full continuous jet) | 0.045 |
| of which final exact evaluation (1) | 0.006 |
| Post-solve, 8x8 full x3 (first version's choice) | 0.093 |
| Post-solve, 23x23 hier frozenH x2 (fastest gate-meeting) | 0.043 |

Extrapolated to `V = 100,000`, single thread on JVM: 7.9 s for whitening,
projection and post-solve with the default, 6.1 s with the fastest variant,
and about 6 s with the default on a 1e-4 basis. The first version of the
spike measured 13 s; the gains came from the node jet bank, using the
refinement jet as the step's verification (exact evaluations per voxel 3 to
1), the exp recurrence in the kernel evaluator, and the hierarchical scan.

Negative results, kept in the code: Gauss-Newton and frozen-Hessian half-jets
cost half a full jet but converge linearly, so they need one or two more
steps and end up no cheaper (frozenH with a 23x23 bank is the exception, at
thinner accuracy margins); the 4-wide blocked projection gains under 10%
because the projection is not the bottleneck.

### Scala.js (Node)

All six tests pass on Node with bit-identical accuracy numbers.

| Phase (per voxel) | JS | JVM | Ratio |
| --- | ---: | ---: | ---: |
| Projection, per-voxel loop | 0.062 ms | 0.017 ms | 3.7x |
| Projection, 4-wide blocked | 0.037 ms | 0.016 ms | 2.3x |
| Post-solve, default (15x15 hier full x2) | 0.213 ms | 0.060 ms | 3.5x |
| of which jets | 0.159 ms | 0.045 ms | 3.5x |
| Post-solve, default on a 1e-3 basis (m = 19) | 0.146 ms | 0.045 ms | 3.2x |

Unlike the JVM, the 4-wide blocked projection is worth 1.7x on JS. Extrapolated
to `V = 100,000` on JS: 26 s single-threaded for whitening, blocked projection
and post-solve with the default (down from 48 s in the first version), about
19 s on a 1e-3 basis.

## Findings

1. **Kernel-basis lowering works and is exact within the basis.** Energy and
   amplitudes agree with the direct time-domain fit to 1e-7 and 1e-6 at a
   prepared shape, continuous readout needs no separate interpolation
   representation, and `Phi` is a data-independent object that can be wrapped
   as a `ResponseBasis`. Go.
2. **Node scan plus safeguarded Newton replaces routing.** Every admitted
   voxel took Newton steps; no fallback path fired at SNR >= 0.5. Two full
   Newton steps from an 8x8 grid leave amplitude agreement at 1.0e-3 to
   1.3e-3, just outside the gate. A denser bank scanned hierarchically (15x15,
   72 scores) with two full steps meets every gate with an order of magnitude
   to spare. The first jet always sits at a node and comes from a precomputed
   bank, so it is nearly free; the refinement jet doubles as the step's exact
   verification. Go, with budgets of about 72 to 90 node scores, 2 jets and
   1 exact evaluation per voxel.
3. **The rank is higher than estimated.** `m = 27` at 1e-5 over this domain;
   `K = 81` at `C = 3`. At `C = 8` this would be `K = 216`, above the plan's
   `K <= 96` cap. Either the cap moves, the domain narrows, or the tolerance
   relaxes to 1e-4 (`m = 24`) or 1e-3 (`m = 19`, `K = 152` at `C = 8`). This
   is a decision for PHRF-01, backed by these numbers.
4. **Post-solve is about 4x the projection, not 1x.** The review's estimate
   assumed `m = 16` and cheap jets. The single continuous full jet is 75% of
   the post-solve: it evaluates the family on 240 fine samples, projects six
   components through `Phi` (6 x m x 240 MACs) and assembles six design blocks
   (6 x K x C x m MACs); both scale with `m`. Remaining levers for PHRF-19, in
   order: choose the basis tolerance from the amplitude gate (1e-3 gives
   `m = 19` and a 25% cheaper post-solve at unchanged accuracy); JVM
   parallelism over voxel blocks (8 workers); a coarser fine step where the
   basis certificate allows. Half-jets and blocked projection are measured
   dead ends. At 7.9 s single-threaded for 100k voxels the 30-second C0 goal
   holds on JVM before parallelism.
5. **Admission at SNR 0.25 is 61%.** Not a decoder failure: the unadmitted
   voxels are weakly identified by the conditional sd(tau) criterion, and the
   admitted ones still meet the accuracy gates. The admission rule and its
   calibration belong to PHRF-14.
6. **Retention path.** The spike retained `(U, R, z, e)` from a rank-revealing
   QR and composed it with the existing `ar` whitening; it did not use the
   working tree's `BasisExpandedFitProduct`, which is under concurrent edit.
   Composing with that product is still open for PHRF-19.

## Go/no-go

- Kernel-basis lowering: **go**.
- Bounded grid scan replacing the condition router: **go**, with backend
  budgets of about 72 to 90 hierarchical node scores, 2 jets and 1 exact
  evaluation per voxel.
- Open for PHRF-01: `K` cap versus domain and tolerance (a 1e-3 kernel tolerance is defensible for a 1e-3 amplitude gate); jet budget of 2.
- Open for PHRF-19: jet cost reduction; `BasisExpandedFitProduct` composition.
