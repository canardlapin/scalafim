# HRF S0 remote staging

The user authorized `buc-gw01` over SSH on 2026-10-04 UTC.
Isolated directory:
`/Users/bbuchsbaum/benchmarks/scalafim-hrf-20261004-codex`.
No system toolchain, existing checkout, VM or other process is modified.

Live host: Darwin arm64, 10 physical cores, 32 GiB RAM, AC power. Initial
one-minute load was 4.52, later 4.05; both fail the v4 limit of 2.0.
`pmset -g therm` reported no recorded thermal/performance warning.
The completed S0 baseline passes the amended recorded checks; see
`s0-review.json`. No S4 non-inferiority admission is claimed.

An isolated Temurin 21.0.12.1+1 is installed under `toolchain/`, with archive
SHA-256 `3623232f33a9c3baadf304480b2535f9a3cba8a58d42ecbb438ba267315d9998`
verified after transfer. Its provenance JSON and checksum file accompany it.
Remote commands set `LC_ALL=C LANG=C`; the inherited `C.UTF-8` locale is not
available to the host's Perl `shasum` command.

The planned baseline covers all declared parameter combinations in
`RegressorConvolutionBenchmark`, `DenseDriveBenchmark` and
`BasisResponseBenchmark`, the three classes named by contract v4. The additional
`EpochIntegrationBenchmark` is not silently substituted for BasisResponse.
Use five independent forks per combination, one thread, three one-second
warmup iterations, five one-second measured iterations, and JMH's GC profiler.
This is S0 variance characterization, not the paired S4 admission study. S4
sample size and baseline/candidate schedule must be frozen separately before
any S4 outcomes are read.

The portable package must contain the exact compiled classes/resources and
runtime jars from `hrfBenchJVM/Jmh/fullClasspath`, a manifest of their hashes,
the source revision and source hashes, and a host-validity receipt. It needs
neither sbt nor source-dependency downloads on the measurement host.

The original rule required one-minute load <=2.0. Before any timings, the user
authorized a prospective amendment to <=5.0 for 10-core buc-gw01 only; see
`buc-gw01-amendment.json`. That threshold applies at start and end. During each measured job, sample
other Java/Node CPU at 1 Hz, exclude only the launched job's process group, and
record power/thermal status. Check load again at the end. Retain invalid runs
and their raw logs without using them as baseline evidence. No automatic
retries, altered margins or relaxed host criteria are authorized by staging.

The completed S0 run is `s0-baseline-20261004-02`; see `hrf-s0-job.json`.
A previous amended-threshold attempt refused load 5.2949 without launching JMH.
All other criteria and the 1.05 margin are unchanged.
