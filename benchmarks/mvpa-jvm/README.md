# MVPA JVM benchmarks

This project measures two completed public execution boundaries:

- `PredictiveExecutionBenchmark` prepares a fixed identified dataset,
  validation schedule, measurement frame, estimand, and execution strategy in
  trial setup. The timed method is one end-to-end `Mvpa.run` using the portable
  operator-ridge path.
- `RelationalQueryBenchmark` prepares dense and matrix-free representations of
  the same identified relation evidence. Trial setup proves that both calls to
  the sole public `RelationalFit.query` route return the same RDM. The timed
  methods compare explicit local materialization with matrix-free projection
  for that one scientific estimand.

Compile the JMH project with:

```console
sbt -Dsbt.supershell=false mvpaBenchJVM/Jmh/compile
```

A bounded smoke run that keeps setup outside measurement is:

```console
sbt -Dsbt.supershell=false \
  "mvpaBenchJVM/Jmh/run -wi 0 -i 1 -f 1 -t 1 -r 200ms -p samples=24 -p features=16 .*PredictiveExecutionBenchmark.*"
sbt -Dsbt.supershell=false \
  "mvpaBenchJVM/Jmh/run -wi 0 -i 1 -f 1 -t 1 -r 200ms -p effects=12 -p neuralCoordinates=64 -p partitions=4 .*RelationalQueryBenchmark.*"
```

Append `-prof gc` to either JMH command to inspect allocation for that exact
fixture, JVM, and invocation. JMH timing and allocation numbers are diagnostic,
not cross-machine pass/fail thresholds and not claims about every MVPA route.
Deterministic work scaling is enforced separately by
`MvpaWorkAccountingSuite` on both the JVM and Scala.js; Scala.js carries no
timing claim.

## Bounded local evidence

On 2026-08-25, a Darwin 23.3.0 arm64 host ran JMH 1.37 on the JDK 25.0.1 VM
reported by the fork. The evidence command used one 300 ms warmup, three 300 ms
measurements, one fork, one thread, and `-prof gc` with the parameter values
shown above.

| Timed boundary | Average time | Allocated per operation |
| --- | ---: | ---: |
| operator-ridge `Mvpa.run`, 24 samples x 16 features | 0.364 ms/op | 838,777.676 B/op |
| relational explicit materialization, 12 effects x 64 neural x 4 partitions | 0.145 ms/op | 273,933.409 B/op |
| relational matrix-free projection, same estimand and shape | 0.148 ms/op | 203,849.162 B/op |

For this exact relational fixture, matrix-free projection allocated 25.6% fewer
bytes per operation. The short timing samples do not establish a speed
advantage: the two relational time estimates are effectively indistinguishable
at this evidence level. Re-run the documented command on the target deployment
JVM before making a broader performance claim.

## External-toolkit conformance benchmarks

`MvpaConformanceBenchmark.scala` adds six fixed-shape public-call courts that
share checksums with the pinned Python rsatoolbox, original MATLAB RSA Toolbox,
and PyMVPA runners:

- correlation observation RDM, 96 samples by 64 features;
- identity-noise crossvalidated RDM, 8 partitions by 8 effects by 64 features;
- Pearson and intercepted-OLS queries over the same 28 RDM pairs;
- leave-one-run-out standardized nearest centroid, 8 runs by 4 classes by 32
  features;
- the same predictive analysis over 16 declared supports of width 8.

The recorded court uses three 500 ms warmups, seven 500 ms measurements, one
fork, one thread, and the GC profiler:

```console
sbt -Dsbt.supershell=false \
  "mvpaBenchJVM/Jmh/run -wi 3 -i 7 -w 500ms -r 500ms -f 1 -t 1 -prof gc -rf json -rff /tmp/scalafim-mvpa-conformance-jmh.json .*ConformanceBenchmark.*"
```

The 2026-08-26 host receipt is reduced in
`docs/audits/mvpa-conformance/performance.csv`. Its main ScalaFIM medians and
JMH allocation estimates were:

| Public call | Median | JVM allocated/op |
| --- | ---: | ---: |
| correlation observation RDM | 3.655 ms | 5,155,431 B |
| identity crossvalidated RDM | 0.354 ms | 713,005 B |
| Pearson RDM query | 0.008 ms | 23,328 B |
| intercepted OLS query | 0.021 ms | 59,048 B |
| standardized-centroid LORO | 2.431 ms | 4,992,056 B |
| 16-support predictive frame | 31.916 ms | 62,708,150 B |

The relational results show that the numerical core is not uniformly slow.
The observation and predictive public boundaries do, however, allocate enough
to warrant profiling. The predictive timings also retain the known scientific
receipt defect: the numeric schedule is leave-one-run-out, but the current
source cannot truthfully bind `run` as its generalization axis.
