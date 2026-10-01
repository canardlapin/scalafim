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
  fir-analytic-producer-v4 \
  'fitEstimatesJVM/testOnly scalafim.fmri.fit.estimates.FirEstimateProducerSuite scalafim.fmri.fit.estimates.FitEstimateReadbackSuite' \
  'fitEstimatesJS/testOnly scalafim.fmri.fit.estimates.FirEstimateProducerSuite'
```

It exited 0. JVM reported 6 passing tests with no failures or errors; Scala.js reported
the new shared FIR suite passing 1 test with no failures or errors. The full raw log
and metadata are `/private/tmp/scalafim-execution-20260929/logs/fir-analytic-producer-v4`
and `/private/tmp/scalafim-execution-20260929/logs/fir-analytic-producer-v4.meta.json`.

This does not qualify negative-origin FIR, effective/reference/per-voxel degrees of
freedom, joint hypotheses, the complete physical fixture family, or a separate-process
reader probe.
