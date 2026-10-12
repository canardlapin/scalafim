# PHRF S10 real-engine harness rehearsal — 2026-10-12

The engineering rehearsal completed on the frozen public harness inputs. The
original ten-dataset alpha diagnostic **did not qualify readiness**: six PHRF
trial ML fits refused. Every dataset and method slot is retained. No pilot was
launched, no scientific admission was issued, and no whitelist was released.

## Results

| Check | Retained result |
| --- | --- |
| Seven cells, two datasets, frozen arm inventory | 60 contribution records, including typed failures |
| Real GLMsingle | Four successful fits, each with measured child CPU |
| Marker-written interruption and resume | One earlier completed job rerun; first contributions reconstructed |
| Real owner reconstruction | `Valid(0350adc8c8d24269,2,60,Vector())` |
| Real process/child CPU | 357.611992 seconds for the final rehearsal |
| Forced real CPU stop | One real arm attempt before `CpuCeilingReached`; unraised restart dispatched nothing; authorized test restart produced eight contributions |
| Original alpha check | All 40 slots retained; 34 alpha choices, six typed refusals; nonconstant endpoints, incomplete qualification |
| Actual launch authority | None; `pilot_ready: false` |

The seven-cell rehearsal retains `TrialMlFailure` for T-TS-fast dataset 0 and
T-TX-fast dataset 1. The ten-dataset diagnostic retains PHRF failures for
persistent datasets 1, 5, 6 and no-deviation datasets 1, 3, 7. All 20 rLSS choices
succeeded. The audit deliberately returns exit 1 after writing these results;
a partial set of successful alpha choices cannot pass completeness.

The outstanding diagnosis is mote **bd-01M4Q7PSY7RE6K5ZQBE3K0GBHV**. Parent S10
readiness **bd-01M3TNWW63XC71C3EF2PQME3P8** remains open. Do not drop or redraw the
retained failures, tune the scientific policy, or interpret the successful
engineering checks as a pilot authorization.

## Boundary correction found by real data

NumPy's `arange` truth grid multiplies by 0.1; the Scala score grid divides by
10. Exact equality rejected otherwise valid condition truth, with an observed
maximum difference of 3.552713678800501e-15 seconds. `PilotScoreTruth` now accepts
absolute binary64 roundoff up to 1e-12 seconds around the same canonical grid.
The shared regression embeds the actual 321 values from C-TX-.5 dataset 0 and
still refuses changed, shifted and nonfinite grids.

The package-internal two-dataset assembler retains all inventory, input
fingerprint and truth checks. Public pilot aggregation still requires at least
15 datasets, with a regression confirming the rehearsal cannot construct a
`PilotCorpus`. The verification record uses a test-only schema and marker;
it is not a production pilot aggregate or whitelist. Coverage is explicitly
Tau for this rehearsal only. The owner key was generated in memory for the
test; no owner custody key was loaded or stored.

## Reproduction and provenance

Use [the rehearsal runbook](../../../tools/phrf-comparison/rehearsal/README.md)
from the repository root. This machine used Python 3.12.13 in two separate
virtual environments. The preexisting lockfile header names Python 3.12.10;
this rehearsal therefore does not claim an exact pilot-runtime identity.
Package versions and install logs are retained in `environments.json` and
`logs/`; dependency pins were installed as follows:

```sh
/path/to/generator-python -m pip install -r tools/phrf-comparison/generator/requirements.lock
/path/to/glmsingle-python -m pip install --require-hashes -r tools/phrf-comparison/glmsingle/requirements.lock
# Check out GLMsingle at 1ab54a65edd3ea41a6133d4b4ecb78a9c7296684 first:
/path/to/glmsingle-python -m pip install --no-deps /path/to/pinned-GLMsingle
```

`source-lock.json` binds the sources used by the final real run. Alpha/CPU
executables started before the scoring-grid correction; their fitting paths do
not call that truth-grid scorer. Those runs are not represented as an exact
post-correction pilot binary. The source dependency pins remain those in the
recorded `build.sbt` for this slice.

`rehearsal-inputs.json` and `generator-manifests.json` bind every one of the 22
NPZs to its generator code, entry script, cell specification, public seed and
SHA-256. The roughly 257 MB of raw inputs remain at
`/private/tmp/phrf-real-inputs-v0-20261012`; they are not committed. Recreate
with the committed generator and verify every digest before fitting. The
final real output is `/private/tmp/phrf-real-rehearsal-output-20261012-3`,
CPU output is `/private/tmp/phrf-real-cpu-guard-20261012-3`, and alpha records
are `/private/tmp/phrf-alpha-zero-20261012.jsonl`. These are machine-local
retained paths, not portable publication links.

## Retained attempts

No failed attempt is used to replace a dataset or suppress a refusal:

1. Initial input preparation used generator defaults (200 scored voxels and
   100 pool voxels). It was never fitted. Preparation was corrected to the
   frozen v0 specification: 40 scored voxels; 4,000 pool voxels in T-TX-fast
   and T-TX-jit; no pool in T-TS-fast.
2. First full engine attempt interrupted the first job, leaving no earlier
   committed job to rerun. The required rerun assertion failed. The hook now
   interrupts the second cell, after one job has committed.
3. A compile attempt rejected a discarded `Files.writeString` value under
   `-Werror`; its explicit `Unit` result was corrected.
4. The first tiny CPU threshold reached the soft dataset stop. Equal soft/hard
   0.02-second thresholds then stopped before any arm work; that receipt is
   retained but rejected. The accepted exercise uses 0.5 seconds and explicitly
   requires real work before the stop. Production ceilings were unchanged.
5. The second full engine run reached scoring and refused real condition
   truth because of exact grid equality. Its full engine events and failure
   log are retained. The final run prevalidated all fourteen truth records,
   applied the boundary correction, and reconstructed successfully.
6. The completed ten-dataset alpha run and independent audit both return
   nonzero for the retained ML refusals. Neither is a clean readiness gate.

`real-receipt.json`, `real-engine-events.jsonl`, `cpu-guard-receipt.json`,
`alpha.jsonl`, `alpha-summary.json`, `phrf-can-timings.jsonl`, `audit.json` and
compressed logs contain the underlying observations. The two PHRF-can probes
are timing-only harness measurements, not trial score arms.
