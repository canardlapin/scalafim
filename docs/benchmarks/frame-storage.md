# Frame storage benchmark receipt

This is an implementation receipt for the semantic storage core, not a
production-engine benchmark or a zero-copy claim.

Command:

```sh
sbt 'frameJVM/Test/runMain scalafim.frame.StorageBench'
```

Environment recorded on 2026-07-22:

- Scala 3.7.4
- sbt 1.10.5
- OpenJDK 25.0.1
- macOS on the repository development machine

The bounded benchmark constructs one million `Int32` values with every tenth
row null, traverses validity and values, creates and closes 10,000 retained
128-row slices, and checks owner/view accounting.

```text
rows=1000000 checksum=450000000000
build_ms=19.036
validity_and_value_scan_ms=67.654
retained_slice_10000_ms=37.083
before_close=BufferSnapshot(2,2,0)
after_close=BufferSnapshot(0,0,2)
```

The checksum fixes the traversal workload. The tracker receipt demonstrates
that the nullable primitive array uses two physical owners (validity and values),
temporary slices release their retained views, and closing the root releases
both owners. The timings are a single smoke measurement without warmup,
fork control, confidence intervals, or comparison to Arrow/Polars. They are
useful for catching order-of-magnitude regressions only; stable performance
claims require a dedicated benchmark harness and backend comparisons.

## Cross-platform relational smoke receipt

`RelationalBench` constructs 100,000 rows with nullable floating values and
repeated UTF-8 group keys. It measures reference-interpreter scan, normalized
filter/project, grouped count/mean, a ten-key inner join, and direct UTF-8
traversal. It also checks that the nine physical buffers owned by the two input
tables are all released.

Commands:

```sh
sbt 'frameJVM / Test / runMain scalafim.frame.RelationalBench'
sbt \
  'set frameJS / Test / scalaJSUseTestModuleInitializer := false' \
  'set frameJS / Test / scalaJSUseMainModuleInitializer := true' \
  'frameJS / Test / run'
```

JVM receipt:

```text
rows=100000 scan_rows=100000 project_rows=50000 aggregate_rows=10 join_rows=100000
normalization_rules=PushFilterThroughProject
utf8_checksum=200000
scan_ms=18.639
filter_project_ms=234.982
group_aggregate_ms=189.817
inner_join_ms=254.241
utf8_scan_ms=9.007
before_close=BufferSnapshot(9,9,0)
after_close=BufferSnapshot(0,0,9)
```

Scala.js/Node receipt:

```text
rows=100000 scan_rows=100000 project_rows=50000 aggregate_rows=10 join_rows=100000
normalization_rules=PushFilterThroughProject
utf8_checksum=200000
scan_ms=3.551
filter_project_ms=133.360
group_aggregate_ms=99.707
inner_join_ms=990.260
utf8_scan_ms=19.512
before_close=BufferSnapshot(9,9,0)
after_close=BufferSnapshot(0,0,9)
```

These numbers characterize the bounded semantic reference path on one machine.
They do not compare JVM and JavaScript runtime quality, predict production
throughput, or justify choosing the reference interpreter over a vectorized
backend. The row counts, normalization receipt, checksum, and ownership
snapshots are the durable regression signals.
