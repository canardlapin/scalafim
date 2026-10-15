# PHRF explicit reference bank and storage gate — 2026-10-08

This packet tests reference placement and retained storage with the previous
canonical-like domain, 96-second support, rank-8 representation, accuracy
threshold and work limits held fixed. It is a fixed-shape diagnostic, not
production admission or a calibrated physiological prior.

The implementation separates explicit reference points from the regular-grid
decoder objective. `TrialReferencePoints` validates immutable points and provides
coordinate-only nearest-reference routing. `TrialReferenceBank` supplies readout
and full jets; `TrialBandedObjective` remains the grid-backed decoder subtype.
Existing grid callers and storage defaults are unchanged.

`ReconstructSecondBands` retains full coefficient, cross, inverse-cross and
release jets, plus value and first-derivative normal bands. Six second-derivative
bands per 3D reference are discarded after construction and reconstructed, in the
same summation order, into one reusable, on-demand band per curvature worker. Readout-only workers
retain no reconstruction buffer. The worker estimate still charges its full
capacity. Corrected readout uses
no reconstructed bands. Terminal curvature adds six basis-pair contractions,
with no extra inverse or factorisation. Receipts report both reconstructed bands
and scalar weight-times-Gram additions; worker sums and ML deltas preserve them.

## Frozen development and confirmation

The parent domain protocol is
`29fc8e066b28d3d15cf1879868b7096555e059e318d93d868f849cdc2c586aa3`.
This protocol is
`3d68e7e4fe106f877608c09468c3cfd62b97b35ec578347bdf5ab92adc88eda1`.
A seed-disjointness regression caught overlapping response seed ranges before
measurement. The corrected protocol records that pre-measurement change.

Development uses the prior N=300 cohort, now explicitly development evidence.
Three eight-point tensor layouts use cell centers, axis 0 fastest. Selection
maximizes primary pass count, then minimizes p95 relative error, then uses the
listed candidate order. Boundary outcomes do not enter selection.

| Development bank | Primary pass count | Primary p95 | Boundary pass count |
|---|---:|---:|---:|
| 4×2×1 | 237/256 | 0.00109527 | 18/27 |
| **4×1×2** | **256/256** | **0.000450441** | **22/27** |
| 8×1×1 | 239/256 | 0.00119490 | 17/27 |

Axes are log positive rate, logit undershoot/positive rate ratio, and undershoot
area ratio. `selected.json` freezes the selected coordinates and hashes the full
development receipt. Confirmation validates those hashes before evaluation and
uses disjoint shape and response seeds. No routing depends on response, residual,
or oracle results. Each candidate uses one reference, three inverse applications,
one residual correction, and zero actual-shape factors. Exact prepared and full
original-design QR comparisons are separate diagnostic work.

Fresh confirmation results:

| N | Cohort / noise ratio | Corrected passes | p95 relative error | Exact prepared passes |
|---|---|---:|---:|---:|
| 30 | Fresh / 0.1 | 64/64 | 0.000425315 | 64/64 |
| 300 | Fresh / 0.1 | **255/256 (99.609%)** | **0.000597729** | 256/256 |
| 30 | Boundary / 0.1 | 24/27 | 0.00127568 | 27/27 |
| 300 | Boundary / 0.1 | 22/27 | 0.00171830 | 27/27 |
| 30 | Paired / 0 | 32/32 | 0.000439313 | 32/32 |
| 300 | Paired / 0 | 64/64 | 0.000543605 | 64/64 |
| 30 | Paired / 1 | 32/32 | 0.000494648 | 32/32 |
| 300 | Paired / 1 | 64/64 | 0.000512140 | 64/64 |

All 566 exact prepared comparisons pass the original-observation amplitude
threshold. Both conditional scenarios pass; both production scenarios remain
`Fail` because the declared blocking caveats still apply. The new confirmation
cohort is not reused to retune the bank. One fresh and eight boundary failures
across the two geometries remain explicit. The prior N=300 bank achieved
232/256 (90.625%) on its earlier cohort; this is improvement across distinct
cohorts, not a paired confirmation estimate of the difference.

## Resource scope

The compact option removes actual retained arrays; the former value-only storage
estimate is not used to claim completion. The N=1200 stress path constructs one
bank directly from blocked preparation, without constructing the legacy decoder
bank alongside it. `stress.json` contains listed-array estimates and actual JVM
reachable-graph sizes, measured by an isolated Java agent with identity
deduplication and shallow `Instrumentation.getObjectSize` measurements.

Three graphs are measured: bank plus eight curvature workers, bank plus eight
public readout workers, and their conservative combined union. All eight curvature
workers are warmed and own distinct encoded-response buffers before measurement.
The initial eager-buffer measurement is retained in `stress-eager-scratch.json`;
it prompted this allocation-only optimization, without changing placement or
readout arithmetic. Graph traversal
includes reachable basis, source, physical axes, response buffers, conditional
scratch and object overhead where present. Inaccessible fields cause failure.
The roots and exclusions are recorded in each result. These are retained graph
measurements, not total process RSS, JS heap sizing, or peak live memory.

At N=1200, rank 8 and bandwidth 240:

| Resource | Bytes | MiB |
|---|---:|---:|
| Full reference, previous storage | 29,498,968 | 28.13 |
| Compact full-jet reference | 15,617,368 | 14.89 |
| Preparation + compact shared bank, listed arrays | 216,061,544 | 206.05 |
| Listed bank + eight worker capacities | 237,022,632 | 226.04 |
| Actual bank + eight warmed curvature workers | 238,466,864 | 227.42 |
| Actual bank + eight public readout workers | 221,557,056 | 211.29 |
| **Actual combined graph: eight of each worker** | **243,153,840** | **231.89** |

The combined retained graph leaves 24.11 MiB below 256 MiB. Its 3D terminal
reference-node jet succeeds, with six reconstructed bands, 62,467,200 scalar
weight-times-Gram additions, ten unchanged banded response solves and zero
factorisations. The eight warmed buffers retain 18,508,800 bytes; the bank itself
retains zero reconstruction scratch. One diagnostic terminal call took 0.122 s;
the complete standalone stress task took 7.24 s. These single JVM observations
are not controlled throughput claims. Integral byte counts in `stress.json` use
ujson's lossless decimal-string encoding.

Construction still builds full derivative bands for one reference before
compaction. That temporary alone is 10×1200×241×8 = 23,136,000 bytes; compact copying
adds 4×1200×241×8 = 9,254,400 bytes until the original reference dies. Construction
also uses factor/canonical copies, cross/release arrays, matrix builders and
compiler/lowering scratch. These arrays are not all simultaneously live with the
finished eight-worker execution graph, but their peak has not been admitted.
Continuous-shape full jets also still construct temporary full references; the
measured terminal jets above are at prepared reference nodes. Concurrent transient
storage is not covered by the retained graph. The ML determinant helper additionally
materializes derivative matrices; its
transient memory remains outside this penalized-profile resource gate.

## Validation

Full fit suites pass: **712 JVM / 654 Scala.js**. The four affected law suites
pass **12 tests on each platform**, including three-dimensional Hessian parity.
Repository-wide `scalafimCompileAll` passes both platforms with no compiler
warnings. The existing multiple-main discovery message is recorded separately.
The initial fixed receipt-field-count assertion was updated from 24 to 26 after
adding the reconstruction counters; all final gates pass. The final allocation
change was revalidated on both platforms and remeasured after warming all workers.

## Reproduction and limits

From this worktree, use `SBT_WARM_HEAP=5g python3 tools/build/sbt-warm` for the
commands recorded in `validation.json` and compressed logs. The JVM driver is
`firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.TrialReferencePlacementMain
<packet-directory> development|confirmation|stress`. Development refuses to
replace an existing selection. Reproduce into a new directory containing the
frozen protocol; confirmation requires its matching development and selection.

For the isolated stress process, export `firstLevelLawsJVM/Test/fullClasspath` to
a log and pass that log to `python3 <packet-directory>/run-stress.py <log>`. The
script builds the included test-only agent in a temporary directory and uses a
2 GiB JVM with four active processors. `python3 -S <packet-directory>/verify.py`
checks source/artifact hashes, selection, routing, all outcomes, work accounting,
coverage arithmetic and storage arithmetic.

No production original-equation certificate is enabled. The new bank is available through explicit public freeze/readout APIs; the
PreparedProfileHrf decoder path still uses its existing regular-grid bank. Next,
integrate a single shared <=8-reference policy without retaining an additional
decoder bank or exceeding the candidate/terminal work allowance. Decoder accuracy
under that legal budget, scientific/domain calibration, peak-memory admission and
B0 end-to-end throughput remain separate requirements. Boundary and high-noise
failures remain in the record; they are not trimmed or converted to successful
production scenarios. No full timing sweep is run in this packet.
