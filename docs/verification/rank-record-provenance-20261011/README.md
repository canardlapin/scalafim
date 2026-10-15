# Rank record provenance slice

Current schema-2 rank records retain their raw and closed stage metrics. New
records additionally bind `source_commit`, the SHA-256 of the exact source-lock
file, and `case_input_sha256`. Workers verify locked source text before and after
execution; the pilot runner independently compares each evaluated record with
its retained input bytes and source lock.

`provenance_version=1` records actual receipt seeds and the reference algorithm.
The adapter validates completed draws, exhausted stopping, the alternative,
family dimensions, input evidence identity and method-specific seed derivation.
Gaussian interlacing uses one hypothesis child per stage; permutation uses the
method root. A declared root or successful fit alone does not establish this.

Opt-in workers now require `SCALAFIM_CALIBRATION_SOURCE_COMMIT`,
`SCALAFIM_CALIBRATION_SOURCE_LOCK` (a JSON object of relative source paths to
SHA-256), and `SCALAFIM_CALIBRATION_SOURCE_ROOT` (defaults to the working directory).
The existing pilot runner supplies these. The readiness source closure includes
the new validator. Unbound historical records remain preserved; the current
validator refuses to treat them as bound calibration evidence.

This is the authorized provenance slice of UMVPA-M4.09. It runs no calibration
campaign, admits no scientific procedure, and does not close the full Mote.

Validation on 2026-10-11: `CalibrationProtocolSuite` and
`RankRecordProvenanceSuite` pass on both JVM and Scala.js (15 tests per platform),
including the production Gaussian and permutation adapters, altered seeds,
method mismatch, incomplete draws, changed source text and changed input bytes.
All MVPA test sources compile on both platforms. The independent Python protocol
and readiness tests pass (25 tests). The only new dependency is test-scoped ujson
4.1.0 for source-lock parsing; it adds no production MVPA dependency.
