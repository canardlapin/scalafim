# Analytic FIR estimate conformance

The bounded fixture uses nine one-second scans acquired at 0.5 through 8.5 seconds,
an impulse at 6 seconds, and `Hrfs.fir(nBasis = 2, span = 4.s)`. It supplies literal
two-voxel responses and independent group-mean OLS expectations. The compiled design
is asserted to have rows `(0,0,1)` through row 5, `(1,0,1)` at rows 6 and 7, and
`(0,1,1)` at row 8, with numerical rank three.

The producer assertion covers effects `(2,5)` and `(-3,0)`, residual variances `1`
and `4`, marginal standard errors from the independently derived variances, and
residual degrees of freedom six. The JVM Core-NIfTI assertion verifies physical value
and validity files, then opens a new store and reads reversed FIR and sample axes.

Verification ran at detached candidate base `cf0c6ad4858aa11489366017b37275918a45ef60`:

```text
python3 /private/tmp/scalafim-execution-20260929/run-sbt.py \
  /private/tmp/scalafim-execution-20260929/fir-analytic-conformance \
  fir-analytic-producer-v5 \
  'fitEstimatesJVM/testOnly scalafim.fmri.fit.estimates.FirEstimateProducerSuite scalafim.fmri.fit.estimates.FitEstimateReadbackSuite' \
  'fitEstimatesJS/testOnly scalafim.fmri.fit.estimates.FirEstimateProducerSuite'
```

It exited 0. JVM reported 6 passing tests with no failures or errors; Scala.js reported
the new shared FIR suite passing 1 test with no failures or errors. The full raw log
and metadata are `/private/tmp/scalafim-execution-20260929/logs/fir-analytic-producer-v5`
and `/private/tmp/scalafim-execution-20260929/logs/fir-analytic-producer-v5.meta.json`.

## Separate producer and relocated IO-only reader

Continuation added persisted standard-error assertions with the literal reversed
variance vector `(14/3, 7/6, 8/3, 2/3)`, as well as literal FIR response coordinates,
binding weights, run pooling, selected scans and estimated residual-df origin.
The updated focused JVM 6/6 and Scala.js 1/1 checks passed at the existing candidate;
the two process entry points compiled successfully. The complete build log has no
compiler warnings. Actual commands and terminal exit 0 are retained in
`/private/tmp/scalafim-execution-20260929/logs/fir-analytic-resume-v1.log` and its
`.meta.json` sidecar.

`FirAnalyticProducerProbe` publishes the actual native fit through Core-NIfTI and
exits 0. The controller then renames the complete artifact tree, verifies the
original path is absent and that every payload hash is unchanged, and launches
`FirAnalyticReadbackProbe` in a distinct JVM using only the estimates-io test
classpath. The consumer imports no fitter or producer fixture. Its class inventory
contains no fit, model, design or dataset classes; runtime absence checks also
cover the fitter, publication producer and analytic producer fixture.

The consumer independently asserts literal dataset/unit/catalog identities,
observations, FIR intervals, bindings, product axes/precision/pooling, scans,
residual df and uncertainty origin, then checks all twelve scalar cells and
validity with reversed bins and samples. Expected effects are `(0,5,-3,2)`, SE² is
`(14/3,7/6,8/3,2/3)`, and residual variance is `(4,1,4,1)`. The measured zero remains
valid. Binding column IDs are checked for consistency/count rather than literal
compiled IDs; the affine is not separately asserted by this bounded case.

The producer and reader both exited 0 with their pass markers. Classpaths were
copied to a private immutable snapshot before execution, with before/after source
and snapshot hashes. The relocated payload and runtime snapshot remained unchanged.
Launch order, full commands, raw logs, exact source/classpath hashes and relocation
evidence are in
`/private/tmp/scalafim-execution-20260929/fir-analytic-relocation-resume/receipt.json`;
the controller is `run_probe.py` beside that receipt. The independent review first
accepted the exact analytic expectations and probe sources; terminal lifecycle
acceptance is recorded separately in the execution-root continuation review.

This does not qualify negative-origin FIR, effective/reference/per-voxel degrees of
freedom, joint hypotheses or the complete physical fixture family. The bounded
separate-process reader probe is now verified; the parent Core qualification remains
open.
