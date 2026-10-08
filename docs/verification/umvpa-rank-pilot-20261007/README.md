# Rank metric, oracle, and expanded-pilot evidence

The accepted raw/closed metric split and independent rank oracles are complete. All 61 fresh pilot cells completed, but their statistical diagnostics remain unfavorable: higher-rank null tests are conservative and standard-root power is low. No confirmation stream was consumed and no inferential claim was admitted.

## What was frozen

- Source and metric clarification: `38f98dab`.
- Expanded pilot manifest: `e5ba1827`, committed before generation/execution, with 175 source locks.
- Eight full-B sizing fixtures: `89e1a46a`, committed before generation/execution.
- [Metric clarification](../../plans/unified-mvpa-rank-metric-bindings-v1.md): raw H2/H3/H4 pointwise calibration, closed R0 FWER, and closed detectable-rank/power decisions. Only the new .20 root has the standard-power target.
- The original three n80/p6/q4/intercept null pilots remain unchanged (closed counts 8/200, 6/200, 2/200). Their missing raw values are not invented or regenerated.

## Statistical findings

The expansion retains **12,200 datasets**, all evaluated, with zero failures or missing records. Each used B199: 2,427,800 nonidentity draws and 9,711,200 compact fits. Every record retains raw counts, raw/closed probabilities, both decisions, seeds and population truth.

Of 29 fresh primary null cells, **9** have a descriptive CP90 interval wholly below the frozen lower calibration target .035; **0** lie wholly above .065. Raw-stage recording therefore does not by itself resolve the earlier conservatism warning.

All **8/8** standard-power cells have a one-sided CP95 upper bound below .80. Their observed closed power ranges from 3.0% to 24.0%.

| Standard-root cell | Closed detections | Descriptive CP90 |
| --- | ---: | --- |
| rank.R3.n80.p4.q6.intercept.alternative | 6/200 | 1.3%–5.8% |
| rank.R3.n80.p4.q6.three-column.alternative | 9/200 | 2.4%–7.7% |
| rank.R3.n80.p6.q4.intercept.alternative | 6/200 | 1.3%–5.8% |
| rank.R3.n80.p6.q4.three-column.alternative | 10/200 | 2.7%–8.3% |
| rank.R3.n160.p4.q6.intercept.alternative | 33/200 | 12.3%–21.4% |
| rank.R3.n160.p4.q6.three-column.alternative | 35/200 | 13.2%–22.5% |
| rank.R3.n160.p6.q4.intercept.alternative | 39/200 | 15.0%–24.7% |
| rank.R3.n160.p6.q4.three-column.alternative | 48/200 | 19.1%–29.5% |

These are pilot diagnostics, not replacements for the 10,000-null/5,000-alternative confirmation criteria. All cells remain in the receipt; neither populations nor thresholds were changed after observing the results. Full primary and secondary counts/intervals are in [metric-intervals.tsv](metric-intervals.tsv), with machine-readable warnings in [pilot-warnings.json](pilot-warnings.json).

## Independent and engineering checks

- Base R QR/SVD agrees with production roots and every tail statistic for unequal dimensions in both directions and all 720 actions of a complete finite group. Exact exceedances, plus-one probabilities, closure and detectable rank agree; nuisance projectors and the production nuisance-adjusted binding agree at combined tolerance 1e-10.
- Six new oracle tests pass on JVM and Scala.js. Full MVPA gates: **464 passed plus one opt-in skip on each platform**. Python controls: **16 passed**.
- All exact pilot input bytes, headers, SHA-derived seeds, named resampling seeds and complete truth vectors are checked. Inputs are retained in per-cell archives.
- The first oracle build exceeded the JVM method-size limit; compact fixture serialization fixed it without changing the numeric expectations. That failure log is retained. A GC warning during Scala.js linking under the 2 GiB heap is also retained; the full owning gates passed.

## Resource evidence

The pilot campaign took 15.55 minutes including input generation, archives, builds and serial coordination. Recorded dataset evaluations sum to 178.62 seconds. Maximum sampled resident sbt JVM RSS was 2.649 GiB.

| n | p/q | Nuisance | Pilot datasets | Mean evaluation (s) | Sampled JVM RSS (GiB) |
| ---: | --- | --- | ---: | ---: | ---: |
| 80 | 4/6 | intercept | 1600 | 0.01357 | 2.518 |
| 80 | 4/6 | three-column | 1600 | 0.00933 | 2.586 |
| 80 | 6/4 | intercept | 1000 | 0.01145 | 2.318 |
| 80 | 6/4 | three-column | 1600 | 0.02042 | 2.442 |
| 160 | 4/6 | intercept | 1600 | 0.01524 | 2.627 |
| 160 | 4/6 | three-column | 1600 | 0.01785 | 2.496 |
| 160 | 6/4 | intercept | 1600 | 0.01425 | 2.637 |
| 160 | 6/4 | three-column | 1600 | 0.01383 | 2.649 |

The eight fresh B1999 fixtures completed 15,992 draws and 63,968 compact fits. Their sampled JVM peak was 2.607 GiB. These are one-observation sizing probes per shape, not a rate study.

The rank-only confirmation inventory remains **480,000 datasets / 959,520,000 nonidentity draws**. Scaling pilot mean evaluation costs by B gives approximately **19.87 hours**; the p95-cost illustration is **48.23 hours**. Multiplying each single full-B fixture by its 60,000-dataset shape inventory gives **15.26 hours**. These estimates are planning evidence, not an upper bound or resource admission. See [the full resource plan](rank-confirmation-resource-plan.json).

RSS covers the resident sbt JVM after its socket identifies the PID; cold-start memory before that point, thin clients and R generators are outside the measurement. Build state is included. The observed Mac14,12 / macOS15 / Node24 host differs from the frozen reference benchmark profile. No standalone J-RANK/JS-RANK performance gate is claimed.

## Next admission work

Review conservative partial-null calibration and low standard-root power before spending the full confirmation budget. Any changed scientific method or population needs its own predeclared evidence; this packet changes neither. M4.07/M4.09 stay open, and M4.10 reference performance and the broader voxel/component/group gates remain pending. The [campaign readiness ledger](campaign-readiness.md) identifies the remaining definitions and dependencies.

The final `execution.json` locks source and artifact bytes. `pilot-manifest.json` and `full-b-sizing-manifest.json` remain the pre-execution authorities; `results/` and `full-b-sizing/` retain every generated input and observed output.
