# Historical LWU refusal diagnosis — 2026-09-30

The frozen historical LWU admission shortfall is dominated by fits on the rho chart boundary. The prior blanket attribution to "rho-weak voxels" is unsupported by the recorded conditional-SD checks. All four fixed 95/100 admission gates remain unmet. This packet diagnoses historical development evidence; it does not qualify PHRF-21.

| Platform | SNR | Accepted / 100 | Boundary | CandidateAttemptCap | CurvatureNotPositive |
|---|---:|---:|---:|---:|---:|
| JVM | 1.0 | 91 | 5 | 4 | 0 |
| JVM | 0.5 | 81 | 15 | 3 | 1 |
| JS | 1.0 | 89 | 5 | 6 | 0 |
| JS | 0.5 | 82 | 16 | 1 | 1 |

## Source and unchanged policy

Base commit: `327eda76792bcb0e542cb9e9c6f0d1351f45b1ea`. Diagnostic execution source: `lwu-dev-87237a3c3163ccca571ec6ef33ae53c2f63c6d1272d58035de08e5b6f226ad5a`. [source-manifest-v2.json](condition-lwu-diagnostics-20260930/source-manifest-v2.json) hashes 1,440 source/build/script files present before execution; later evidence archives are covered by the packet index. The only compiled changes are the two shared test sources. Production numerical source is unchanged. The original `compile`, `cohort`, `budgetFor` and `accuracy` helper bodies are unchanged except trailing whitespace. The source corrects stale budget comments/output and removes the unsupported refusal attribution.

Original fixture: 600 rows, 100 synthetic voxels per SNR, seeds 111 at SNR 1 and 112 at SNR 0.5. It uses the existing LWU fixture, whitening, preparation, bank and runtime with no prior, noise variance 1, and unnormalised HRF. Original chart bounds are tau [3,8], log(sigma) [log(.8),log(3)], rho [0,.8]. Budget remains six Newton steps, eight jets, two exact evaluations, and four candidate attempts per iteration. Stationarity is a free-coordinate Newton correction no greater than 1e-9; weak-SD limits remain (.5,1,1). Boundary statuses and refusal precedence remain unchanged.

The standalone diagnostic registers the original MUnit fixture but does not run its oracle, Gaussian accuracy or throughput tests. It emits 200 indexed records per platform and explicitly verifies the original aggregate admissions: JVM 91/81, JS 89/82. These are historical DEV responses, not new holdouts. Aggregate reproduction does not establish byte identity of JVM and JS responses or equality of every original per-voxel status.

## Evidence and interpretation

Every record retains original status, energy, amplitudes, Hessians, conditional SDs and all seven decoder work deltas. Exact hexadecimal raw and whitened input literals are retained for every refusal. All fits include a noncryptographic whitened-word fingerprint; file SHA-256 provides packet integrity. The extra terminal jet runs after fitting and after the decoder counter snapshot, outside the production budget. Its availability never changes admission or refits a voxel.

[The read-only analyzer](condition-lwu-diagnostics-20260930/analyze_lwu_records.py) checks all records, identities, dimensions, work caps, hexadecimal round trips and input fingerprints. It independently computes principal minors, cofactor inverses, conditional SDs and bound-active Newton corrections with 60-digit Decimal arithmetic. This is an independent algebra check of recorded terminal derivatives; it is not an independent direct-objective derivative oracle. Relative 2e-10 and absolute 2e-12 coherence checks cover roundoff between terminal recomputation and returned fit values; admission still uses the original 1e-9 correction threshold.

No finite shape-SD limit exceedances were found. All recorded Boundary refusals are at a rho chart limit. Boundary status alone does not establish a converged constrained optimum: SNR .5 voxel 50 has post-fit correction 0.5575143097494156 on JVM and 0.5575143097493597 on JS. The detailed analyzer outputs preserve nonstationary and overlapping boundary/curvature/budget findings rather than equating status counts with disjoint scientific causes. CandidateAttemptCap refusals still miss the fixed stationarity gate. A positive extra terminal Hessian cannot erase a budget or earlier curvature refusal.

[Analytic and synthetic controls](condition-lwu-diagnostics-20260930/analyzer-controls.json) validate a coupled 3x3 SPD inverse, reject an indefinite matrix, preserve wholly unavailable returned curvature and reject mixed unavailable/stale curvature. Synthetic missing-value controls are separate from cohort evidence. The Scala smoke test likewise preserves wholly unavailable returned Hessians/SDs; it compares both returned Hessians when available.

## Verification and retained attempts

- Final JVM: focused MUnit test passed, standalone historical 200-record execution exited zero; [raw](condition-lwu-diagnostics-20260930/lwu-diagnostic-v3-jvm.log), [metadata](condition-lwu-diagnostics-20260930/lwu-diagnostic-v3-jvm.log.meta.json), [analysis](condition-lwu-diagnostics-20260930/jvm-analysis-v2.json).
- Scala.js: focused MUnit test passed in the retained v3 attempt; subsequent sbt `Test/run` argument parsing failed before cohort execution. The selected entry was then linked successfully and run directly with Node and explicit historical-mode arguments; [link raw](condition-lwu-diagnostics-20260930/lwu-diagnostic-v4-js-link.log), [Node raw](condition-lwu-diagnostics-20260930/lwu-diagnostic-v4-js-node.log), [Node metadata](condition-lwu-diagnostics-20260930/lwu-diagnostic-v4-js-node.log.meta.json), [analysis](condition-lwu-diagnostics-20260930/js-analysis-v2.json).
- sbt 1.11.7 ran on Homebrew Java 25.0.1; Scala 3.7.4; Node v26.7.0. Shell `java -version` resolves another JDK and is not the sbt runtime. All sbt jobs used the shared serial runner, 3 GiB heap and four active processors.
- Build source pins Gale `da38f8c429294657d30ec29f06eae3fab636d428`; its loaded cache HEAD was checked. A separate transitive Gale build at `099832ff15c8a4a8fcf3398c7b779fb4bbc12434` is also visible in sbt's load log. These observations are source/load provenance, not a new claim of sole-provider qualification.
- The initial sandbox boot-lock failure, intermediate passing JVM v2 run, Scala.js argument failure and original parent requalification receipt are retained. The Scala.js v3 warning about multiple main classes was a launcher warning, not a compiler warning; explicit main selection is used for the final link. Both changed shared test sources compiled on JVM and JS. No full-project test or compile-all result is claimed.

To reproduce the algebra audit from this directory:

```sh
python3 condition-lwu-diagnostics-20260930/analyze_lwu_records.py condition-lwu-diagnostics-20260930/lwu-diagnostic-v3-jvm.log --source lwu-dev-87237a3c3163ccca571ec6ef33ae53c2f63c6d1272d58035de08e5b6f226ad5a --out /tmp/lwu-jvm-new.json
python3 condition-lwu-diagnostics-20260930/analyze_lwu_records.py condition-lwu-diagnostics-20260930/lwu-diagnostic-v4-js-node.log --source lwu-dev-87237a3c3163ccca571ec6ef33ae53c2f63c6d1272d58035de08e5b6f226ad5a --out /tmp/lwu-js-new.json
python3 condition-lwu-diagnostics-20260930/check_analyzer_controls.py condition-lwu-diagnostics-20260930/lwu-diagnostic-v3-jvm.log
```

Output paths must be new. Exact launcher commands and exit statuses are in each metadata file. [The hash index](condition-lwu-diagnostics-20260930/SHA256SUMS) covers archived payloads, excluding itself.

## Qualification boundary and handoff

Bounded child `bd-01M3SVJZTKKVMV8774NC2NNPKE` delivers diagnosis and test-label repair. Parent `bd-01M25Q0JY8GA7CJRKZF2934PJ0` remains open. The independent review is archived with its exact source and evidence hashes.

The next scientific decision is whether to propose a concrete repair for the fixed-policy refused inputs or revise the supported family/chart claim with independent justification. Boundary changes, solver changes and budget changes would require owner review and a new freeze before qualification. Examined DEV inputs cannot become fresh evidence. This packet changes no production solver, thresholds, budgets, normalization, fresh streams, 100k admission, calibration controller, release or publication state.
