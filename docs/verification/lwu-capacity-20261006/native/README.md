# Native Linux final qualification — 2026-10-06

[GitHub run 37530987004](https://github.com/canardlapin/scalafim/actions/runs/37530987004)
completed successfully at exact source `19f535758f3b1345d8667c055642ec464ab7a7c2`.
Both jobs used default published Gale
`b56a9dd0b8ad479621a3594880f90c9add8c2824`; no source overlay or sibling override.

| Native gate | JVM passed | Scala.js passed |
| --- | ---: | ---: |
| Kernel/budget controls | 29 | 29 |
| Unchanged compact cohort + stationarity control | 4 | 4 |
| Complete frozen LWU accuracy test | 1 | 1 |
| Full fit court | 686 | 629 |
| Total | 720 | 663 |

**1,383 test executions passed, zero failures/errors/skips.** The unchanged
compact cohort admitted 24/24 JVM and 23/24 JS, passing its original minimum 22/24,
accuracy and resource assertions. This is the declared native qualification
scope for the previously failing Linux JS cohort.

## LWU scientific accuracy

The verification-only exact-one inherited selector retained the original 100
samples at each SNR, seeds 111/112, oracle 26 x 11 x 9, 60-minute suite timeout and all
original scientific assertions. No throughput or entire first-level spike ran.
The caller explicitly admitted 3 million array cells and 128 billion spectral work units; global defaults
and other compiler caps were preserved.

| Runtime | SNR | Admission | Reported p95 latency | Reported p95 FWHM | Reported p95 relative amplitude |
| --- | ---: | ---: | ---: | ---: | ---: |
| JVM | 1.0 | 93% | 0.0000s | 0.0100s | 3.19e-4 |
| JVM | 0.5 | 82% | 0.0100s | 0.0100s | 4.50e-4 |
| JS | 1.0 | 92% | 0.0000s | 0.0100s | 3.19e-4 |
| JS | 0.5 | 82% | 0.0100s | 0.0100s | 4.50e-4 |

Latency<=0.02s, FWHM<=0.05s and relative amplitude<=0.001 passed at both SNRs on
both platforms. Metrics above are rounded log reports; assertions use full
precision. **Admission below 95% remains the original reported unmet target for
rho-weak voxels.** These accuracy passes do not establish a 95% admission result.
The executed decode budget was 6 Newton steps, 8 jets, 2 exact evaluations.

## Observed runtime and evidence

Actual host fingerprints: Ubuntu 24.04.5 x64, kernel 6.17.0-1022-azure,
Temurin 17.0.20.1+1, Node 24.21.0, V8 13.6.233.17-node.53, glibc 2.39-0ubuntu8.9.
The setup requested 17.0.20+1; the observed patch binary above is recorded
separately. This qualifies the declared JDK 17 baseline without claiming the
requested historical binary was supplied exactly.

`receipt.json` binds the run, source hashes, scope, every stage and admission
caveats. `native-results.tar.gz` retains both original platform artifacts and
raw job logs, including host fingerprints, complete source manifest, exact-one
selector identity, exit codes and scientific results. Verify without network
or sbt:

```sh
python3 -S verify.py
```

No general timing or performance claim is made. Root handled commit/publication
and dispatch; this agent only monitored, retrieved and packaged this evidence.
