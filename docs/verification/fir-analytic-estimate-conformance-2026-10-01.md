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

## Local integration verification, 2026-10-03

The two reviewed FIR commits were selectively integrated on canonical ScalaFIM
main `263e6a917c4cea402b7cdb58de96f22994b742ab`, retaining every existing main
readback test. Main does not expose the candidate's `storedDatatype` metadata
field, so the physical storage assertion reads the actual NIfTI header and
requires datatype code 64 (FLOAT64). The analytic fixture and both process
probes retain their reviewed source hashes.

At integrated source revision `adcccf0f6e519138a6d3679958dc3963825aebf4`,
JDK 21 checks passed `fitEstimatesJVM/test` (16), `fitEstimatesJS/test` (12),
`estimatesIoJVM/test` (10), and `estimatesIoJS/test` (4). The terminal build
exit was zero, with no compiler warnings in that gate. Full raw output and
metadata are `/private/tmp/scalafim-merge-20261003/fir-gates-v3.log` and its
`.meta.json` sidecar; earlier failed attempts are retained separately.

The integrated producer exited zero before the artifact was renamed. The
distinct IO-only consumer exited zero after relocation and checked the literal
metadata and twelve scalar cells. The original path was absent, all eleven
payload hashes were unchanged, and the frozen classpath inventory excluded
fit/model/design/dataset classes. JAR inventory is taken from the frozen copy.
The actual classpath closure, launcher, lifecycle and source hashes are in
`/private/tmp/scalafim-merge-20261003/fir-relocation/receipt.json` and its
`classpath-manifest.json`; their SHA256 values are respectively
`fbc01493e5d94fa3a6a376c83f56421c1e12fcad34c6fb7448dc978a1b20921b` and `b1edf568a3394c90436b4b1eded5e9ba5b6ffba0b71bcb0fccd9a8a72be4490b`.

This integration retains the bounded scope and open parent qualification
described above. No scientific admission policy or dependency pin was changed.
