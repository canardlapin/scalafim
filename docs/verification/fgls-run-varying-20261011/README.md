# Prospective run-varying AR(2) study

This work implements an **experimental** estimator for Mote
`bd-01M4K6KEPYEMP9XHFT2J0WCYEE`. **Confirmation failed the original margins
on both platforms. The estimator is not scientifically admitted**, and the
Mote remains open. It is not exposed as a qualified FitPlan policy. The
previous pooled estimator's run-varying scope limit remains unchanged.

## Method and frozen protocol

The corrected, one-pass AR(2) estimates for each run are shrunk toward their
full-run-length-weighted mean within the same voxel. Two fixed retention
values, 0.75 and 0.50, are declared in
[`protocol.json`](../../../tools/scenarios/fgls-run-varying/protocol.json).
There is no spatial pooling, response-dependent weight selection, clipping,
or retry. Censored observations use continuous stationary covariance.

Kenward–Roger inference uses per-run covariance derivatives and propagates
the working parameter covariance through the shrinkage Jacobian as
`A W Aᵀ`. This is a first-order working approximation to the uncertainty of
the corrected Yule–Walker estimator. It does not remove shrinkage bias, and
the fixture agreement below is not a coverage or calibration claim.

The protocol retains every original corrected-GLS margin and requires all
five cells, including run heterogeneity, to pass. The new random domains are
disjoint from the prior studies. Pilot outcomes serve runtime/parity checks;
512 development replicates per cell select one candidate under the declared
rule. The selected candidate and exact source are committed before the
single 2,048-replicate confirmation on each platform. Missing or duplicate
trial slots, refusals, source changes, or differing platform decisions
prevent admission. The runner retains failed runs and a single-use
confirmation receipt.

## Engineering evidence before random outcomes

- JVM and Scala.js: 13 tests each in `RunArShrinkageSuite` and
  `VoxelwiseArFglsSuite`, including retention endpoints, cross-run covariance,
  and an independent dense R finite-difference fixture for beta, covariance,
  contrast-specific scale, and denominator degrees of freedom.
- Four Python tests check unchanged margins, candidate eligibility, the
  declared tie break, and missing/duplicate/wrong-profile trial rejection.
- The ordinary JVM study smoke suite passes; this does not consume the new
  development or confirmation domains.

## Pilot outcome

The retained pilot has 120 records on each platform (8 datasets × 5 cells ×
3 engines), no refusals or missing/duplicate slots, unchanged source bytes,
and identical t/F/coverage decisions. The largest scaled numeric difference
is 2.45e-12. These are engineering checks, not admission evidence.

The shared FitPlan replay harness was then corrected to avoid duplicate
baseline intercepts and formatted before development. The shrinkage method,
candidate settings, random domains, and acceptance margins did not change.

## Reproduction

Commit source first, then run from the repository root:

```sh
python3 tools/scenarios/fgls-run-varying/run_study.py --profile pilot --output docs/verification/fgls-run-varying-20261011/pilot
python3 tools/scenarios/fgls-run-varying/run_study.py --profile development --platforms JVM --output docs/verification/fgls-run-varying-20261011/development
python3 tools/scenarios/fgls-run-varying/freeze_candidate.py --development docs/verification/fgls-run-varying-20261011/development --output docs/verification/fgls-run-varying-20261011/freeze.json
```

Commit `freeze.json` before running confirmation with its candidate and the
`--freeze` path. A failed scientific confirmation keeps the Mote open; it
does not authorize another seed or adjusted margins.

## Development decision, before confirmation

Development completed 7,680 records on JVM (512 datasets × 5 cells × 3
engines), with no refusals, complete unique trial slots, and unchanged
committed source `d8b4743628d9d00ca5f515ca8bc558edbfce3054`.

The declared selection rule chooses **retention 0.75**: it passes 46 of 55
criteria across the five cells, compared with 45 for retention 0.50. The
retained summaries include every failure. These development results do not
qualify the estimator; in particular, the short censored homogeneous cell
misses null-t, null-F, coverage, and effect-bias criteria at this development
sample size. The run-heterogeneous cell passes AR recovery but misses null-F.
No margin, candidate setting, or cell has changed in response.

[`freeze.json`](freeze.json) binds the selected engine to the exact protocol,
source, development records, and independent analysis. It is committed before
the one-use confirmation run on the separate domain, with both platforms:

```sh
python3 tools/scenarios/fgls-run-varying/run_study.py --profile confirmation --candidate run-shrink-075-kr --freeze docs/verification/fgls-run-varying-20261011/freeze.json --output docs/verification/fgls-run-varying-20261011/confirmation
```

Retained `.log.gz` and `.jsonl.gz` files decompress to the original bytes;
each phase's `archives.json` binds both compressed and uncompressed hashes.

## Confirmation verdict: not admitted

The single-use confirmation completed **20,480 unique records on each
platform** (2,048 datasets × 5 cells × 2 engines), with no fit refusals or
missing/duplicate trials. Both eight-test execution gates passed. The frozen
source remained byte-identical throughout; subsequent Git merges contained
only documentation, retained receipts, and Mote operations.

Both independent R analyses return scientific failure. Every decision and
every per-cell criterion agrees between JVM and Scala.js; the largest scaled
numeric difference is 3.70e-12. This is a scientific admission failure, not
an execution or cross-platform discrepancy.

| Candidate cell | Criteria passed | Failed criterion |
| --- | ---: | --- |
| Short, censored, homogeneous | 11/11 | None |
| Long, censored, homogeneous | 10/11 | Standardized effect bias |
| Short, complete, homogeneous | 11/11 | None |
| Short, censored, voxel-heterogeneous | 11/11 | None |
| Short, censored, run-heterogeneous | 10/11 | Coverage |

In the run-heterogeneous cell, AR RMSE is **0.09931** (limit 0.10) and mean
absolute whiteness is **0.08505** (limit 0.10). Coverage is 1,901/2,048
(92.82%), with a 90% exact Clopper–Pearson interval of **[91.813%, 93.739%]**.
The lower endpoint is below the binding 92% margin, so this cell fails.

The long homogeneous cell's standardized effect bias is **0.17679** for the
candidate and **0.17555** for the known-coefficient control. Each mean alone
already exceeds the 0.10 bound, before its confidence allowance. The shared
control failure calls for investigation of the simulation/protocol and
metric before attributing this bias to shrinkage.

All adverse evidence is retained in `confirmation/` and summarized in
[`verdict.json`](verdict.json). No margin, cell, candidate setting, or seed
was changed after confirmation. `confirmation-spend.json` remains committed;
the stream is spent. Further work must address the coverage failure and the
known-control bias under a separately declared protocol. This result does
not authorize a qualified run-varying FitPlan policy or a broader inference
claim.
