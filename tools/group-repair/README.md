# Group-model repair qualification

Ordinary regression tests live in `modules/group/shared/src/test/scala/scalafim/fmri/group/GroupRepairSuite.scala` and run on JVM and JavaScript. They retain the original audit counterexamples and R fixtures, then exercise normalized label collisions, uncertainty bounds, design reparameterization, partial failures, and PM/mKH inference.

Run:

```sh
sbt 'all groupJVM/test groupJS/test fmriWorkflowJVM/test fmriWorkflowJS/test'
```

`pm-reference.R` independently generates PM/DL z and modified Knapp–Hartung references using `metafor::rma.uni`. Supply the audited input directory and an output directory:

```sh
mkdir -p /tmp/group-reference
LC_ALL=C LANG=C Rscript --vanilla tools/group-repair/pm-reference.R \
  tools/group-audit/fixtures /tmp/group-reference
```

The checked references use R 4.5.1 and metafor 5.0.1, with `control(tol=1e-12, maxiter=1000)`. The PM root tolerance must be tightened: its ordinary R default is too loose for coefficient-level regression comparisons. The Scala solver stops at `abs(Q-df) <= 1e-9 * df`, reports failed convergence, and retains the zero-heterogeneity boundary.

For extended evidence, compile `jvm/GroupRepairCalibration.scala` and `jvm/GroupRepairBenchmark.scala` against the JVM group test classpath (or temporarily include them in that test source set). Neither is part of ordinary CI: calibration creates 1,080,000 independent null data sets; benchmarking intentionally allocates dense whole-map inputs. Run the compiled main classes with:

```sh
java -Xmx3g -XX:ActiveProcessorCount=6 -cp "$GROUP_TEST_CP" \
  scalafim.fmri.group.GroupRepairCalibration 20000
java -Xmx3g -XX:ActiveProcessorCount=6 -cp "$GROUP_TEST_CP" \
  scalafim.fmri.group.GroupRepairBenchmark 100 4 200000 FE
```

`GroupRepairBenchmark` accepts `n p samples OLS|FE|DL`, generates identical seeded Gaussian effects and continuously varying variances for each implementation, warms ten full fits, then measures five. Run three independent forks per implementation/configuration in randomized order. Compile the **same benchmark source** against each implementation; these source-compatible constructor calls are not binary-compatible across the added inference parameter. Record baseline and candidate runtime hashes, not just the benchmark source hash. Every fitted coefficient and SE is checked outside the timing interval; checksums summarize completed outputs.

`BENCH` fields are: mode, n, p, samples, repetition, setup seconds, fit wall seconds, calling-thread allocation bytes, contrast seconds, FDR seconds, GC milliseconds through FDR, consumed value, calling-thread CPU seconds, process CPU seconds, coefficient sum, SE sum. `/usr/bin/time -l` supplies whole-process peak RSS; it is separate from per-fit allocation and includes inputs, warmup and JVM overhead.

Calibration compares OLS, DL/z, DL/mKH and PM/mKH on the **same data**. The grid is n=8/20/80, p=1/3, known variances or independent chi-square variance estimates with df=8/40, and true tau²=0/.2/1. The p=3 case includes an imbalanced group indicator and a continuous covariate. These are pointwise Gaussian-null results, not power, heavy-tail robustness, repeated-subject validity, or spatial error-control certification. Both mKH candidates still show inflation in some cases; no automatic small-sample default is admitted.

See [the qualification report](../../docs/verification/group-repair-2026-09-08.md) and its compressed receipt for exact sources, commands, test logs, complete calibration counts, and all completed benchmark forks. The original audit remains in `tools/group-audit` and its original report is unchanged.
