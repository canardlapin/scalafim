# Prospective run-varying AR(2) study

This work implements an **experimental** estimator for Mote
`bd-01M4K6KEPYEMP9XHFT2J0WCYEE`. It has no scientific admission yet and is not
exposed as a qualified FitPlan policy. The previous pooled estimator's failure
on run-varying coefficients remains a binding scope limit.

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
