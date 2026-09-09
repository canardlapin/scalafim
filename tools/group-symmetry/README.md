# Group symmetry qualification

The ordinary portable regressions are in `GroupSignFlipSuite`. Run:

```sh
sbt 'all groupJVM/test groupJS/test fmriWorkflowJVM/test fmriWorkflowJS/test'
```

The extended JVM tools are outside the ordinary test source roots. Load them
explicitly from the repository root:

```sh
sbt 'set groupJVM / Test / unmanagedSourceDirectories += file("tools/group-symmetry")' \
  'groupJVM/Test/runMain scalafim.fmri.group.GroupSymmetryCalibration 10000 100 20 999'
```

Arguments are exact studies, independent Monte Carlo plan batches, studies per
plan batch, and Monte Carlo draws. A small executable smoke uses `20 2 5 19`;
it is not calibration evidence. Keep only the CSV header/count lines when
extracting data from sbt's logging. The retained qualification runs use direct
JVM execution; exact commands and classpaths are in the compressed receipt.

`--confirm` selects the prespecified form of the subsequently identified largest
Monte Carlo deviation (n80, df8, t3, dominant precision, tau2=1), with **different**
outcome and action seed roots. The confirmation run uses `10000 500 40 1999
--confirm`, giving 20,000 fresh studies. This is an additional diagnostic, not a
replacement for unfavorable grid cells. The complete grid remains in the receipt.

The [protocol](protocol.md) records the assumptions, target, calibration grid,
refusal boundaries and declared gate. `reference.py` independently enumerates
exact signs using rational arithmetic and a 60-digit decimal score; its result
is checked in the shared regression suite. `summarize.py` uses SciPy to distinguish
independent-study binomial intervals from intervals clustered by shared sign
plan. `plot-calibration.py` uses Matplotlib to render selected null/power results:

```sh
python3 tools/group-symmetry/reference.py
python3 tools/group-symmetry/summarize.py calibration.csv summary.json
python3 tools/group-symmetry/plot-calibration.py summary.json
```

Monte Carlo p-values are identity-corrected. The reported simulation intervals
describe precision of the rejection-rate estimates, not subject confidence
intervals. Coverage here means acceptance of the true center when these tests
are inverted; interval endpoints and connectedness are not implemented.

For completed-map performance, load the same tool directory and run
`GroupSymmetryBenchmark 20 200000 999 equal` or
`GroupSymmetryBenchmark 80 20000 999 precision`. Each invocation performs three
warmups and five measured fits. Use three fresh, sequential JVM forks per shape
and weighting; measure plan construction separately. Allocation includes result
construction and is not RSS. No performance claim is inferred from smoke runs.

The [native report](../../docs/verification/group-symmetry-2026-09-08.md) links
the compressed evidence. It retains all grid and confirmation counts, source
and runtime hashes, commands, test logs, completed benchmark forks and actual
plot reviews. `review/calibration-v1.png` is the inspected rejected plot;
`calibration.png` is the corrected, inspected export. This is a scientific QA
figure, not an application UI acceptance specimen.
