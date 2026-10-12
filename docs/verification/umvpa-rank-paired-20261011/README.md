# Paired rank runner and failed preflight — 2026-10-11

The three-method exploratory runner is implemented, but **the pilot was not launched**. The required retained-input JVM/Scala.js parity gate failed. All 12,800 proposed pilot streams remain unopened; no assignments manifest, input generation, pilot-spend marker, new calibration estimate, confirmation or rank admission was produced.

The final retained replay evaluates the same eight archived resource fixtures through historical coefficient permutation, current score-orthogonal permutation and the current Gaussian reference: 24 records on each platform, 48 total, all evaluated with verified actual RNG receipts and 199 draws (796 compact fits per method/dataset). Inputs are byte-identical to the previous packet. Historical production remains unchanged; its added recorder and current sbt-warm are retained in the incremental bundle and patch.

`preflight-audit-final.json` is an independently rerunnable audit of those records, not launch authority. Run:

```sh
python3 tools/mvpa-inference/paired/audit_preflight.py \
  docs/verification/umvpa-rank-paired-20261011/preflight-5 /tmp/rank-preflight-audit.json
```

## Failed portability gate

Four of eight historical-permutation fixtures and three of eight repaired-permutation fixtures differ in their exceedance counts between JVM and JS. Every difference occurs with the three-column nuisance design. The largest absolute raw p-value difference is 0.03 (six of 199 exceedances, denominator 200). The actual seed receipts agree, and the maximum scaled difference in observed roots/statistics is only 1.2740689443226704e-15. All eight Gaussian-reference fixtures have identical counts. All methods' raw and closed rejection decisions agree on these eight fixtures.

Agreement of the binary decisions is insufficient to pass the frozen exact-count gate. The root of the permutation discrepancy has not been isolated; basis/transform construction is a diagnostic lead, not a demonstrated explanation. Neither historical production nor Multivar was modified to make the gate pass. A prospective repair/review and a new retained-input preflight are required before any freeze or launch. The historical method must remain identifiable if platform-specific comparator behavior is eventually accepted by a revised protocol.

## Resource evidence and failed engineering attempts

The final replay's four commands exit zero with no resource refusals. Measured peak resident JVM memory is 3.02–3.503 GiB; each numerical worker has an effective 3 GiB heap and four active processors. Commands take 57.62–62.05 seconds including startup. The cap remains 4 GiB sampled owned-JVM RSS; this is not an instantaneous memory guarantee.

Attempts 1–4 remain archived. Attempt 1 exposed the interval between socket shutdown and process exit. Attempt 2 counted sbt's short-lived `--rt-ext-dir` lookup as a numerical worker replacement. Attempt 3 exposed a process-exit race in diagnostic `ps`. Attempt 4 identified the helper's exact command. The monitor now records the helper and counts its RSS toward the cap, while requiring one numerical worker lifetime, the effective worker flags, and no other provider server. All attempts replay only the same eight already-observed fixtures.

The final source strengthens aggregate startup RSS reporting and requires an effective-flags receipt. Its Python tests pass; no additional pilot or preflight data was collected after the scientific gate failed. These runner changes do not qualify the deferred full campaign's runtime.

## Implemented prospective execution contract

`tools/mvpa-inference/paired` contains inventory/seed/source validation, resource preflight, freeze, external-R input generation, serialized two-provider execution, descriptive paired analysis and regression tests. The inventory remains the prior 64 cells × 200 datasets × three methods, B=199. It includes both p/q orientations, both nuisance designs, seven populations at n=80/160, and standard-R3 n=320/640. No cell has been dropped.

A passing preflight is required before freezing source, exact resolved classpath bytes and all stream assignments in a committed manifest. Launch checks the passing preflight again, verifies source/runtime hashes and committed assignments, then writes a one-use spend marker. Historical and current numerical JVMs never overlap; each of 16 geometry groups uses one lifetime per provider. The aggregate hard cap is four hours, with a 900-second provider/cell cap, one worker, four CPUs and a 3 GiB worker heap. Any resource, provenance or evaluation failure stops execution with failed/missing slots retained. There is no automatic redraw or retry. Identical archived input bytes reach all three methods; historical and repaired permutations share the declared transform stream, and Gaussian hypotheses have separate child streams.

The runner's full-campaign execution and analysis remain unvalidated by a real campaign because this gate stopped launch. Parsing the two R scripts and passing engineering tests are not evidence that the complete study will satisfy its wall-time or statistical targets.

## Validation and provenance

- Current engineering suites: 10 passed and one opt-in skip on each platform before fixture replay.
- Historical engineering suites: six passed and one opt-in skip on each platform.
- Final opt-in replay: historical eight records/platform, current sixteen/platform; all four test commands pass. Scientific parity audit fails as described above.
- Nine Python protocol/resource/freeze-guard tests pass. Both R scripts parse.
- Historical recorder HEAD: `8dc005d3dfc65d60fc437743dd360142e8f47b5f`; historical Multivar: `ab811e257dd67f77e8c3b70cb1ea600f274429a3`.
- Current final-replay HEAD: `4dfdc8b9f23a8ecd420ac77f85a1c3964a5a937f`; current Multivar: `edb05de01401ec0b3aea4dc1190dd3100e70ee51`.
- `historical-paired.bundle` requires `6dd0d0310b28e5cafdd024758e940ce9b2c771ee`, already retained by `../umvpa-rank-integration-20261008/historical-replay.bundle`.

M4.09 remains open. This packet supplies the runner and the negative preflight evidence; it does not supply the deferred paired Monte Carlo study or confirmatory qualification.
