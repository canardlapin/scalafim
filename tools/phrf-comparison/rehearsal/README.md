# S10 real-engine harness rehearsal

This uses the public harness root `7a3c91d50b44e2f1`. It never loads a pilot
root, owner custody key or launch authorization. The throwaway test owner is
generated in memory. The two-dataset assembler is package-internal, and
ordinary pilot aggregation still requires at least 15 datasets.

Prepare the existing pinned generator and GLMsingle environments described
by `../generator/requirements.lock` and `../glmsingle/requirements.lock`.
Install GLMsingle source commit `1ab54a65edd3ea41a6133d4b4ecb78a9c7296684`
without dependency resolution after installing the hash-locked environment.
The verification packet records the installation commands and versions.

Generate into a new directory with the generator Python:

```sh
/path/to/generator-python tools/phrf-comparison/rehearsal/generate.py /tmp/new-phrf-harness
```

All seven frozen v0 cells have 40 scored voxels and two datasets. T-TX-fast
and T-TX-jit retain their 4,000-voxel pools. T-TX-fast additionally has datasets
2–9 for the original ten-dataset no-deviation check. No cell is dropped and
no failed dataset is redrawn.

Run from the ScalaFIM repository root; all output paths must be new:

```sh
python3 tools/build/sbt-warm \
  'phrfComparisonJVM/Test/runMain scalafim.phrfcmp.exec.RealEngineRehearsal /tmp/new-phrf-harness /tmp/new-phrf-real /path/to/glmsingle-python'
python3 tools/build/sbt-warm \
  'phrfComparisonJVM/Test/runMain scalafim.phrfcmp.exec.RealCpuGuardRehearsal /tmp/new-phrf-harness /tmp/new-phrf-guard /path/to/glmsingle-python'
python3 tools/build/sbt-warm \
  'phrfComparisonJVM/Test/runMain scalafim.phrfcmp.exec.AlphaZeroRehearsal /tmp/new-phrf-harness /tmp/new-phrf-alpha.jsonl'
python3 tools/phrf-comparison/rehearsal/audit.py /tmp/new-phrf-harness \
  /tmp/new-phrf-real /tmp/new-phrf-guard /tmp/new-phrf-alpha.jsonl /tmp/new-phrf-audit.json
```

The real rehearsal runs the frozen arm inventory, interrupts a marker commit
after an earlier job has completed, resumes, and asks the real owner verifier
to reconstruct the corpus from sealed contributions. Its custom verification
record is explicitly a harness record, not a pilot whitelist. Tau-only
coverage is a rehearsal choice and does not choose the actual study policy.

The CPU exercise measures actual process CPU at a 0.5-second test ceiling,
requires real arm work before the stop, rejects an unraised restart and uses
the ephemeral test owner's output-bound authorization to resume. This does
not change the production 60-core-hour ceiling or authorize an actual pilot.

The alpha diagnostic runs the original `y0 = y - signal + signal_condition_mean`
transformation for all ten datasets, keeping truth outside each fitter's
`FitInputs`. It retains all refusals, endpoint counts and the two PHRF-can
timing probes. A missing alpha choice makes its completeness gate fail; a
partial set of favorable choices cannot satisfy that gate. The audit writes
its report even when that readiness condition fails, and returns nonzero.

The [retained packet](../../../docs/verification/phrf-real-rehearsal-20261012/README.md)
records successful engineering checks and outstanding scientific refusals.
