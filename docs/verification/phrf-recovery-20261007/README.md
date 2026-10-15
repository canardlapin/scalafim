# PHRF recovery experiment — 2026-10-07

**The matched model is recoverable; the current fast search is incomplete.**
An independent nine-start reference recovers all 16 noiseless B0 signals.
The original eight-node decoder recovers none, even with substantially more
work. A small opt-in center probe recovers all 16. This is a useful repair,
but **PHRF-33 and the epic remain open and not admitted**: one strong-signal
refinement failure and a worse local minimum remain, alongside certification
and workload-preparation blockers.

## Controlled experiment

The experiment uses the first 16 frozen B0 voxels: T=600, N=300, C=3,
six nuisance columns, Cascade34, rank 10, lambda=1 and AR(1)=0.3.
`BlockedPartial(96)` preserves the original chart, training grid and tolerance.
Signal amplitudes and noise draws are fixed; only noise scale changes. The
ratio below is noise SD divided by the unwhitened task signal RMS. Ratio 2
reproduces the original B0 responses, including the intercept. Truth generates
and audits the data; it is never a search start.

The offline reference independently implements the Cascade coefficient jet,
nuisance projection and dense penalized conditional solve. L-BFGS-B uses nine
interior starts (center plus the eight quarter/three-quarter points), normalized
chart coordinates and energy, at most 300 iterations/1,200 function calls per
start, and no generating-coordinate seed. Final augmented QR separately checks
amplitudes, rank and energy. Analytic gradients and two finite-difference steps
audit stationarity and curvature. All nine start results remain in
`reference.json`; multistart is not a proof of global optimality.

| Noise ratio | Reference interior, stationary, positive curvature | Reference boundary | Expanded bank-only accepted | Center probe accepted |
| --- | ---: | ---: | ---: | ---: |
| 0 (noiseless) | 16/16 | 0/16 | 0/16 | 16/16 |
| 0.1 | 16/16 | 0/16 | 0/16 | 15/16 |
| 0.5 | 15/16 | 1/16 | 0/16 | 15/16 |
| 1 | 10/16 | 6/16 | 0/16 | 8/16 |
| 2 (original B0 noise) | 3/16 | 13/16 | 0/16 | 2/16 |

Both last columns use the same eight-node bank, 16 Newton steps, 20 jets,
40 exact evaluations, 12 attempts per step and stationarity tolerance 1e-6.
Only initialization differs. The original default budget (2 steps, 2 jets,
6 exact evaluations, 4 attempts, tolerance 1e-9) also accepts zero at every
noise level; those baseline records are in the archived reference input.
These explicit diagnostic budgets do not amend the frozen B0 gate.

The reference noiseless shape error is at most 1.038e-8 chart widths. Across all
80 cells, its conditional amplitudes agree with independent QR within 5.774e-14.
Scala banded energies agree with the reference at all 80 returned shapes in
portable fixture tests. Among center-probe outputs at ratios 0, 0.1 and 0.5,
maximum shape difference from the reference is 4.303e-7 chart widths and maximum
absolute energy difference is 1.041e-13 after division by T times signal RMS
squared. `comparison.json` preserves per-level errors and all refusal counts.

Two counterexamples prevent a general recovery claim:

* Ratio 0.1, voxel 10 stops with `CurvatureNotPositive`, energy 1.8835732663;
  the reference finds a positive-curvature interior solution, energy
  1.7900486410. This is a search failure even on strong data.
* Ratio 1, voxel 3 is locally `Accepted`, energy 3.9292657532; the reference
  finds a better boundary solution, energy 3.9263841234. Coordinates differ by
  0.3364 chart widths. Local terminal admission does not establish selection
  of the best solution. The eight accepted outputs in that row are therefore
  not eight agreements with the reference.

Increasing noise makes the reference itself prefer boundaries in 13/16 original
B0 cases. Those results cannot all be repaired by suppressing decoder refusals.
This fixture has condition-constant trial amplitudes, not trial random effects;
it diagnoses the present penalized criterion on a matched prepared basis. It
does not establish original-family accuracy, random-effects calibration,
coverage or general identifiability. Those require the planned scientific
cohorts and an explicit treatment of weak/boundary fits.

## Retained repair and rejected alternatives

`DecodeBudget(initialization = DecodeInitialization.ChartCenterProbe)` tries
one full jet at the chart center after selecting a bank node. It charges the
existing jet and candidate counters and keeps the probe only if finite and
strictly better under the data-plus-prior objective. No quota remains means no
probe. Failed or worse probes preserve the accepted state. Existing refinement,
terminal curvature, boundary and refusal rules then apply. The default remains
`BankNode`; no default numerical budget or tolerance changes.

The budget identity becomes `decode-budget/v2`, including the initialization
field for both policies. Consequently default provenance/cache identities also
change, although default numerical behavior does not. Golden identity tests
were updated; prior identities must not be treated as the new schema.

The retained eight-factor preparation reports an engine allocation estimate of
19,737,000 bytes. A 125-node diagnostic bank estimated 209,362,176 bytes and
still failed one strong-signal case. A 27-node bank did not improve recovery
enough either. Nine interior probes and curvature-guarded backtracking were
also tried: neither fixed the remaining strong-signal case, and the latter
added budget exhaustion. Neither policy ships. Their exact experimental sources
are archived in `rejected-variants-sources.zip`, with the exploratory receipts
and logs. Intermediate receipts have the schema/configuration active at their
capture; `center-final.json` and `bank-expanded-control.json` are the controlled
final-source comparison.

The final receipts retain all 80 attempts, decoder counters, conditional/readout
work and execution times. For example, the noiseless center cell uses 103 jets,
87 candidate attempts and 71 Newton steps for 16 successful readouts. These are
single-worker, 16-voxel diagnostic runs with a recording sink. Timings exclude
setup and do not measure the full Float32 output workload, controlled warmup,
peak memory or qualifying throughput. The shared public-readout suite separately
consumes the actual Float32 outputs and verifies all 16 noiseless off-node fits.

## Next bounded work

1. Repair refinement/global selection against the two counterexamples above,
   retaining the independent oracle, work caps and refusal semantics. A generic
   bounded optimizer belongs in Gale; do not add a private optimizer family in
   ScalaFIM. Admission should require agreement with the best reference result
   as well as a locally coherent terminal jet.
2. Specify and test weak/boundary behavior on the intended statistical cohorts.
   Do not interpret a boundary-preferred criterion as a successful shape estimate
   merely to raise the output count.
3. Complete bounded N=1,200 design lowering and PHRF-11 original-equation evidence.
   The preceding checkpoint still refuses 7.2 million lowering cells above the
   current cap, and original-family certification still refuses before reads.
4. Once correctness and preparation are admitted, run the full B0 and stress
   performance cells with fixed-shape denominators and complete memory/phase
   receipts. Make the PHRF-33 activation/engineering decision on those results.
   PHRF-14/15/16 remain open; finite-state implementation stays deferred.

## Validation and reproduction

Final checks: `fitJVM/test` **689/689**, `fitJS/test` **632/632**;
`TrialRecoverySuite`, `DecodedTrialCheckpointSuite` and `ConditionProfileFitSuite`
**15/15 on each platform**. New decoder tests cover corner escape, exhausted
quota, failed/worse probes, prior ranking and provenance. Native ML public
readout coherence is also exercised. The first full JVM run found the missing
default provenance field; the final schema/goldens fix passed the full rerun.
No new Scala compiler warnings; sbt retains its multiple-main discovery warning.

Base commit `ff4dd39f` on `work/phrf-checkpoint-20261007`; final source and artifact
hashes are in `manifest.json`. Hosted Gale remains
`d03eb99bde389ce9bce21b8e0fc59bec9ac9b4aa`, without a local override. Host/settings
are the same as the preceding packet: Mac14,12, 10 CPUs, 32 GiB, macOS 15.1.1,
Java 21.0.12.1, Node 24.21.0. Wrapper heap setting was 5g; `.jvmopts` also supplies
`-Xmx6g`. Concurrent machine work was not excluded. This is not the historical
M3 Max performance host. Reference: Python 3.12.13, NumPy 2.4.3, SciPy 1.17.1,
four outer workers and one BLAS thread each; 29.3 seconds for the 80-cell solve.

```sh
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.TrialRecoveryExport target/phrf-recovery-input 16'
python3 tools/verification/phrf-recovery/reference.py \
  target/phrf-recovery-input /tmp/phrf-reference.json --workers 4 \
  --scala-fixture /tmp/TrialRecoveryOracle.scala
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.TrialRecoveryCandidate /tmp/phrf-center.json 16 2 center 1e-6' \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.TrialRecoveryCandidate /tmp/phrf-bank.json 16 2 bank 1e-6'
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm fitJVM/test fitJS/test
SBT_WARM_HEAP=5g python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/testOnly scalafim.fmri.laws.profile.TrialRecoverySuite scalafim.fmri.laws.profile.DecodedTrialCheckpointSuite scalafim.fmri.laws.profile.ConditionProfileFitSuite' \
  'firstLevelLawsJS/testOnly scalafim.fmri.laws.profile.TrialRecoverySuite scalafim.fmri.laws.profile.DecodedTrialCheckpointSuite scalafim.fmri.laws.profile.ConditionProfileFitSuite'
python3 -S docs/verification/phrf-recovery-20261007/verify.py
```

Use a Python environment with the recorded NumPy/SciPy versions for reference
generation; Scala tests consume the committed fixture without Python. Its
generator was checked against all 80 committed values. `reference-input.zip`
retains the exact binary matrices and metadata used by the captured reference,
whose input hashes are checked by the packet verifier. Re-exported progress
strings contain process identities and are not byte-stable; compare numerical
data and decisions, not those strings or timings.

The preceding checkpoint packet is historical evidence at `ff4dd39f`. Its
source-hash verifier deliberately does not pass against the changed decoder.
Reproduce that packet on its original revision; its manifest was not rewritten.
Neither packet verifier can promote diagnostic evidence to PHRF qualification.
