# Group-model audit probes

These are public-API audit probes for the 8 September 2026 group-engine review.
They are intentionally outside ordinary test source roots. The current engine
fails ten of the sixteen added checks on each platform; a failing exit is the
recorded audit finding, not a completed acceptance gate. See
[the report](../../docs/verification/group-audit-2026-09-08.md).

`shared/` contains the MUnit audit suite and literal numeric fixtures. `jvm/`
contains the pointwise null-calibration experiment and the resource benchmark.
The latter accepts `participants terms samples OLS|FE|DL` and uses two warmups,
three measured fits, and public GroupData/GroupModel/GroupEngine calls. It measures
calling-thread allocation and CPU, process CPU, and wall time separately. Use a
fresh JVM for each shape/method; peak RSS can be collected with `/usr/bin/time -l`.
The existing experiment is exploratory: warmup/JIT, GC and host contention need
better control before performance admission.

## Run the public-API tests

Use an isolated checkout with explicitly selected provider dependencies. Do not
change another session's checkout or its build settings. From that checkout:

```sh
cp tools/group-audit/shared/*.scala modules/group/shared/src/test/scala/scalafim/fmri/group/
sbt 'groupJVM/testOnly scalafim.fmri.group.GroupAuditSuite'
sbt 'groupJS/testOnly scalafim.fmri.group.GroupAuditSuite'
```

The recorded audit copied these sources into the isolated module's test
roots and ran both complete suites. The receipt records the commands and source
hashes. Ordinary baseline tests had already passed before injecting the probes.

## Run the calibration and benchmark

```sh
mkdir -p modules/group/jvm/src/test/scala/scalafim/fmri/group
cp tools/group-audit/jvm/*.scala modules/group/jvm/src/test/scala/scalafim/fmri/group/
sbt \
  'set LocalProject("groupJVM") / Test / fork := true' \
  'set LocalProject("groupJVM") / Test / javaOptions ++= Seq("-Xmx3g", "-XX:ActiveProcessorCount=6")' \
  'groupJVM/Test/runMain scalafim.fmri.group.GroupCalibration'

sbt \
  'set LocalProject("groupJVM") / Test / fork := true' \
  'set LocalProject("groupJVM") / Test / javaOptions ++= Seq("-Xmx3g", "-XX:ActiveProcessorCount=6")' \
  'groupJVM/Test/runMain scalafim.fmri.group.GroupBenchmark 100 4 200000 DL'
```

The calibration output fields are `method,n,tau2,simulations,rejections,nonfinite`.
Although the local Scala variable is named `tau`, it is the *between-participant
variance*: the generated observation SD is `sqrt(firstLevelVariance + tau)`.
The experiment tests the zero-mean null with known first-level variances, not
an estimated-SE, spatial-FWER or repeated-measures pipeline.

Benchmark fields are `method,n,p,samples,replicate,setupSeconds,fitSeconds,
allocatedBytes,contrastSeconds,fdrSeconds,gcMilliseconds,checksum,
threadCpuSeconds,processCpuSeconds`. Allocation covers the calling thread during
fit, whereas peak RSS covers the entire process. Both effect and variance input
matrices are resident even for OLS. Input generation and validation happen before
the measured fit. Use JDK 21+ for the benchmark's `Thread.threadId()` and the
supported `com.sun.management` counters; this audit ran on JDK 25.

## Reproduce the independent references

The literal `fixtures/x.csv`, `y.csv`, and `v.csv` are the authoritative shared
inputs. In a temporary directory containing those files, run `reference.R`
with R and metafor to regenerate `reference.csv`, then `scaling-reference.R` to
check unit invariance independently using R QR. The archived R session records
the exact package environment. `generate-fixtures.py` records the deterministic
fixture construction; NumPy 2.4.3 generated the archived CSV/literal inputs.
`emit-reference.py` emits the Scala reference table from R's CSV output.

Do not change expected results to make an admitted defect pass. When a native
fix is implemented, promote its small regression to the normal JVM/JS suites;
retain the historical audit receipt and update a new qualification record.
