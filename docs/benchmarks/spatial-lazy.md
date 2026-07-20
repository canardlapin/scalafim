# Spatial lazy benchmark receipt

This receipt accompanies the spatial lazy pull-through acceptance gate. It is a
diagnostic snapshot, not a portable performance promise: wall time and allocated
bytes vary with the JVM, JIT state, operating system, and machine. The stable
gates are the work counts and IO reduction.

## Reproduce

```text
sbt "spatialJVM/testOnly scalafim.spatial.SpatialLazyPerformanceSuite"
```

The suite constructs 500 equivalent two-affine views over a 128 × 1 × 1,
20-observation NIfTI root. The terminal demand selects eight target rows and
five observations. It records elapsed time with `System.nanoTime`, current-thread
allocated bytes with `com.sun.management.ThreadMXBean`, NIfTI bytes read, cache
entries, runtime work counts, and a checksum that keeps the numerical workload
observable.

## 2026-07-20 local receipt

Environment: OpenJDK 25.0.1, sbt 1.10.5, Scala 3.4.2. This was a focused test
run without a dedicated benchmark fork or JIT warm-up.

```text
spatial-lazy-benchmark-v1
chain.iterations=500
chain.elapsed_nanos=86617167
chain.allocated_bytes=19939800
first.elapsed_nanos=2212125
first.allocated_bytes=167240
repeat.iterations=50
repeat.elapsed_nanos=3919250
repeat.allocated_bytes=189224
source.bytes_read=320
source.full_bytes=20480
cache.plan_entries=1
cache.result_entries=1
runtime.compilations=1
runtime.executions=1
runtime.result_cache_hits=50
checksum=532330.0
```

The durable result is that the chained description performs no eager source
work, the first terminal demand compiles and executes once, 50 repeats reuse the
materialized result, and the source reads 320 of 20,480 possible bytes (1.5625%).
The timing and allocation numbers are retained to make regressions observable,
but are not CI thresholds.

## Numerical oracle

`SpatialLazyAcceptanceSuite` freezes an independent R 4.5.1 `stats::approx`
fixture for the route target → dense pullback → affine pullback → root sample.
For root values `[0, 8, 0, 4, 0, 12]`, the direct root-first result is
`[4, 8, 0]`; sequentially resampling after each transform produces
`[4, 4, 3]`. The fixture therefore detects direction/convention mistakes and
the scientifically important double-interpolation regression.
