# ProfileHrf frozen cohorts and reference host (PHRF-01)

Frozen 2026-09-10. Companion to [profile-hrf-work-items-v2.md](profile-hrf-work-items-v2.md)
(decisions D1 to D3 on PHRF-01) and [profile-hrf-spike-results.md](profile-hrf-spike-results.md).
Changing any value below is a recorded decision with old and new values.

## Reference host

| Item | Value |
| --- | --- |
| CPU | Apple M3 Max, 14 physical cores |
| Memory | 36 GiB |
| OS | macOS 14.3 |
| JDK | openjdk version "22" 2024-03-19 |
| Node (Scala.js tests) | v26.7.0 |
| sbt | 1.11.7, JVM options `-Xmx6g ` |
| Gale revision | 83cac90a678d |
| Threads for absolute targets | 1 (single-thread figures); parallel figures report worker count separately |
| Power | mains, no thermal throttling observed; report if a run is throttled |

## Families and charts

| Family | Chart | Domain | Horizon | Kernel tolerance (default) |
| --- | --- | --- | --- | --- |
| Gaussian | (tau, log sigma) | tau in [3, 8] s, sigma in [0.8, 3] s | 24 s | 1e-3 held-out relative value error (m = 19, K = 57 at C = 3) |
| LWU | (tau, log sigma, rho) | tau in [3, 8] s, sigma in [0.8, 3] s, rho in [0, 0.8] | 32 s | to be measured by PHRF-21 with the same rule |
| Half-cosine | not admitted in v1 | | | rejected by chart (moving joins); revisit with derivative-free cells |
| Cascade34 | (log kp, logit(ku/kp), rho) | PHRF-04 | | PHRF-04 |

Kernel basis compilation grid: 26 x 21 shape nodes (Gaussian), fine step
0.1 s, derivatives included, 300 held-out points, seed 11.

## Condition cohort C0

| Item | Value |
| --- | --- |
| T | 600 samples at TR 1.0 s, one run |
| Events | 300, 100 per condition, onsets uniform on a 0.1 s grid, seed 20260910 |
| C | 3 (capacity statement: C <= 8 with K <= 96 on a narrowed domain; C = 8 on the full Gaussian domain is refused) |
| Nuisance | 6 columns: intercept, linear and quadratic trend, three low-frequency cosines |
| Noise | AR(1), phi = 0.3, shared whitening; SNR levels 1.0, 0.5, 0.25 (signal sd / noise sd per timepoint) |
| Truth | tau uniform on [3.5, 7.5], log sigma uniform on [log 1.0, log 2.5], amplitudes signed uniform on [0.5, 2] |
| V | 100,000 for absolute targets; 10,000 for JS and scaling; accuracy cohorts 200 voxels per SNR, seeds 101, 102, 103 |
| Decoder | 15 x 15 node bank scanned hierarchically (coarse stride 2), 2 full Newton steps, budgets from D2 |
| Admission (provisional, D3) | positive observed curvature, interior, conditional sd(tau) <= 0.5 s |
| Absolute compute target | <= 30 s single-thread for whitening, projection and post-solve; parallel and IO reported separately |
| Memory | <= 256 MiB engine live memory |
| Accuracy gates | >= 95 % admitted at SNR >= 0.5; p95 latency <= 0.02 s, FWHM <= 0.05 s, relative amplitude L2 <= 1e-3 against the dense oracle |

## Trial cohort B0 (unchanged from revision 1)

T = 600, N = 300, C = 3, six fixed columns, V = 100,000, d = 3, r = 7 for
Cascade34; N = 1,200 stress case; <= 120 s two-reference profile plus readout
plus certification; <= 256 MiB; one amplitude correction; trial budgets as
in revision 1.

## Rank and conditioning tolerances

| Item | Value |
| --- | --- |
| QR rank tolerance | 1e-10 (pivoted, Gale) |
| Observed-family certification | smallest singular value of the projected direct design >= 1e-3; projector error reported per held-out shape |
| Compact Gram | Cholesky pivot > 0 or the cell is refused |
| Float32 output audit | per-query absolute tolerance, default 1e-6 for amplitudes |
